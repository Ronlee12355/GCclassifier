#' @title Gastric cancer molecular subtypes prediction
#' @description Predict the molecular subtypes of gastric cancer samples,
#' based on log2 scaled gene expression profile (GEP).
#'
#' @param Expr a dataframe or matrix with log2 scaled gene expression profile data,
#' with samples in column and genes in rows, gene names should not be empty.
#' @param method subtyping classification model, should be either "ACRG", "EMP" or "TCGA". By default is "EMP".
#' @param idType a string which indicates which type of gene ids used in the rownames of GEP,
#' should be one of "SYMBOL","ENSEMBL","ENTREZID" and "REFSEQ". By default is "SYMBOL".
#' @param minPosterior minimal posterior probability to classify a sample in TCGA subtyping system. By default is 0.5.
#' @param maxp the maxp parameter used in \code{\link[impute]{impute.knn}} function,
#' it is optional.
#' @param verbose a single logical value specifying to display detailed messages (when verbose=TRUE)
#' or not (when verbose=FALSE).
#'
#' @return a dataframe with sample names and predicted molecular subtype labels. If the method
#' is one of "EMP" and "TCGA", predicted probabilities of each subtype (with corresponding
#' subtype column names) will be included.
#'
#' @importFrom impute impute.knn
#' @importFrom stats predict
#' @importFrom dplyr filter group_by summarize_all case_when mutate left_join pull
#' @importFrom magrittr %>%
#' @importFrom AnnotationDbi mapIds
#' @import randomForest
#' @import org.Hs.eg.db
#' @export
#'
#' @references Oh, S.C., Sohn, B.H., Cheong, JH. et al. Clinical and genomic landscape of gastric
#' cancer with a mesenchymal phenotype. Nat Commun 9, 1777 (2018).
#' https://doi.org/10.1038/s41467-018-04179-8.
#' @references Cristescu, R., Lee, J., Nebozhyn, M. et al. Molecular analysis of gastric cancer
#' identifies subtypes associated with distinct clinical outcomes. Nat Med 21, 449–456 (2015).
#' https://doi.org/10.1038/nm.3850.
#' @references The Cancer Genome Atlas Research Network. Comprehensive molecular characterization of
#' gastric adenocarcinoma. Nature 513, 202–209 (2014).
#' https://doi.org/10.1038/nature13480
#'
#' @examples
#' data("GSE62254")
#' result <- get_molecular_subtype(Expr = GSE62254, method = "ACRG", idType = "SYMBOL")


get_molecular_subtype <- function(Expr = NULL,
                                  method = "EMP",
                                  idType = "SYMBOL",
                                  minPosterior = 0.5,
                                  maxp = NULL,
                                  verbose = TRUE
) {
  ## 1. Check input
  if (isTRUE(verbose)) {
    message('Checking input dataset and parameters......')
  }
  if (!is.matrix(Expr) && !is.data.frame(Expr)) {
    stop('Only gene expression profile in dataframe or matrix format is accepted.')
  }
  if (is.null(rownames(Expr)) || is.null(colnames(Expr))) {
    stop('Rownames and colnames are madatory in gene expression profile.')
  }
  if (sum(apply(Expr, 2, is.numeric)) != ncol(Expr)) {
    stop('Only numeric values in gene expression profile is accepted.')
  }
  if (any(Expr < 0, na.rm = T)) {
    stop('Gene expression profile cannot contain any negative value(s).')
  }
  if (any(is.na(Expr))) {
    stop('Gene expression profile cannot contain any NA value(s).')
  }
  if (sum(c("SYMBOL", "ENSEMBL", "ENTREZID", "REFSEQ") %in% colnames(Expr)) != 0) {
    stop(
      'Sample names in expression profile should not contain "SYMBOL", "ENSEMBL", "ENTREZID" and "REFSEQ".'
    )
  }
  if (is.matrix(Expr)) {
    message('Transforming the input gene expression profile into dataframe format.')
    Expr <- as.data.frame(Expr)
  }
  Expr <- Expr[apply(Expr, 1, function(x) {
    mad(x) > 0
  }), ]

  ## 2. Process parameters
  if (is.null(method) ||
      !(method %in% c("ACRG", "EMP", "TCGA")) || length(method) != 1) {
    stop('method should be one of "ACRG", "EMP" and "TCGA".')
  }
  if (is.null(idType) ||
      length(idType) != 1 ||
      !(idType %in% c("SYMBOL", "ENSEMBL", "ENTREZID", "REFSEQ"))) {
    stop('idType should be one of "SYMBOL", "ENSEMBL", "ENTREZID", "REFSEQ".')
  }

  ## 3. Convert gene id to gene symbol
  if (idType != 'SYMBOL') {
    if (isTRUE(verbose)) {
      message('Converting gene ids to gene symbols......')
    }
    Expr[[idType]] <- suppressMessages(
      AnnotationDbi::mapIds(
        org.Hs.eg.db,
        keys = rownames(Expr),
        keytype = idType,
        column = 'SYMBOL'
      )
    )
    Expr <-
      Expr %>% filter(!is.na(get(idType))) %>% group_by(get(idType)) %>%
      summarize_all(max) %>% as.data.frame()
    rownames(Expr) <- NULL
    rownames(Expr) <- Expr[[idType]]
    Expr <- Expr[, sapply(Expr, is.numeric)]
  }

  if (isTRUE(toLog2(df = Expr))) {
    if (isTRUE(verbose)) {
      message('Performing log2 transformation......')
    }
    Expr <- log2(Expr + 1)
  }
  ## 4. Impute gene expression
  if (method == 'EMP') {
    gene_in_model <- gc.mp$required.gene %in% rownames(Expr)
    if(sum(gene_in_model) < length(gc.mp$required.gene)*0.5){
      pct <- 1 - (sum(gene_in_model)/length(gc.mp$required.gene))
      stop(paste0(round(pct, digits = 3)*100, '% required genes were missing, please check your expression profile.'))
    }
    if (sum(gene_in_model) < length(gc.mp$required.gene)) {
      gene_not_in_expr <- gc.mp$required.gene[!gene_in_model]
      tmpExpr <- data.frame(matrix(
        NA,
        nrow = length(gene_not_in_expr),
        ncol = ncol(Expr)
      ))
      rownames(tmpExpr) <- gene_not_in_expr
      colnames(tmpExpr) <- colnames(Expr)
      Expr_impute <- rbind(Expr, tmpExpr)
      if (is.null(maxp)) {
        maxp <- nrow(Expr_impute)
      }
      if (isTRUE(verbose)) {
        message('Imputing missing data......')
      }
      Expr_impute <- suppressMessages(impute::impute.knn(
        as.matrix(Expr_impute),
        maxp = maxp,
        rowmax = 1
      )$data)
      Expr_impute <-
        as.data.frame(Expr_impute[gc.mp$required.gene, ])
    } else{
      Expr_impute <- Expr
    }

  } else if(method == 'ACRG'){
    gene_in_model <- gc.acrg.emt$required.gene %in% rownames(Expr)
    if(sum(gene_in_model) < length(gc.acrg.emt$required.gene)*0.5){
      pct <- 1 - (sum(gene_in_model)/length(gc.acrg.emt$required.gene))
      stop(paste0(round(pct, digits = 3)*100, '% required genes were missing, please check your expression profile.'))
    }
    if (sum(gene_in_model) < length(gc.acrg.emt$required.gene)) {
      gene_not_in_expr <- gc.acrg.emt$required.gene[!gene_in_model]
      tmpExpr <- data.frame(matrix(
        NA,
        nrow = length(gene_not_in_expr),
        ncol = ncol(Expr)
      ))
      rownames(tmpExpr) <- gene_not_in_expr
      colnames(tmpExpr) <- colnames(Expr)
      Expr_impute <- rbind(Expr, tmpExpr)
      if (is.null(maxp)) {
        maxp <- nrow(Expr_impute)
      }
      if (isTRUE(verbose)) {
        message('Imputing missing data......')
      }
      Expr_impute <- suppressMessages(impute::impute.knn(
        as.matrix(Expr_impute),
        maxp = maxp,
        rowmax = 1
      )$data)
      Expr_impute <-
        as.data.frame(Expr_impute[gc.acrg.emt$required.gene, ])
    } else{
      Expr_impute <- Expr
    }

  } else if(method == 'TCGA'){
    gene_in_model <- gc.tcga$required.gene %in% rownames(Expr)
    if(sum(gene_in_model) < length(gc.tcga$required.gene)*0.5){
      pct <- 1 - (sum(gene_in_model)/length(gc.tcga$required.gene))
      stop(paste0(round(pct, digits = 3)*100, '% required genes were missing, please check your expression profile.'))
    }
    if (sum(gene_in_model) < length(gc.tcga$required.gene)) {
      gene_not_in_expr <- gc.tcga$required.gene[!gene_in_model]
      tmpExpr <- data.frame(matrix(
        NA,
        nrow = length(gene_not_in_expr),
        ncol = ncol(Expr)
      ))
      rownames(tmpExpr) <- gene_not_in_expr
      colnames(tmpExpr) <- colnames(Expr)
      Expr_impute <- rbind(Expr, tmpExpr)
      if (is.null(maxp)) {
        maxp <- nrow(Expr_impute)
      }
      if (isTRUE(verbose)) {
        message('Imputing missing data......')
      }
      Expr_impute <- suppressMessages(impute::impute.knn(
        as.matrix(Expr_impute),
        maxp = maxp,
        rowmax = 1
      )$data)
      Expr_impute <-
        as.data.frame(Expr_impute[gc.tcga$required.gene, ])
    } else{
      Expr_impute <- Expr
    }
  }

  ## 5. Make prediction
  if (isTRUE(verbose)) {
    message('Making molecular subtype prediction......')
  }
  res <- data.frame("sample" = colnames(Expr_impute))
  if (method == "EMP") {
    pred.prob <- predict(gc.mp$MP.EP,
                         Expr_impute %>% t() %>% scale() %>% as.data.frame(),
                         type = 'prob')

    res$subtype <- ifelse(pred.prob[, 2] >= gc.mp$MP.EP.youden.index, 'MP', 'EP')
    res$EP <- pred.prob[, 1] %>% round(digits = 2) %>% value2label()
    res$MP <- pred.prob[, 2] %>% round(digits = 2) %>% value2label()

  } else if(method == "ACRG"){
    res$MSI.EMT <- predict(gc.acrg.emt$MSI.EMT,
                           Expr_impute %>% t() %>% scale() %>% as.data.frame())

    tmp <-
      res[res$MSI.EMT %in% c('MSS/TP53+', 'MSS/TP53-'), 'sample', drop = F]

    tmp$TP53.score <- Expr_impute[rownames(gc.acrg.emt$TP53$importance), tmp$sample] %>% t() %>%
      scale() %>% as.data.frame() %>% dplyr::mutate(score=(MDM2+CDKN1A)/2) %>% dplyr::pull(score)

    tmp$TP53 <- ifelse(tmp$TP53.score >= gc.acrg.emt$ACRG.youden.index ,
                       'MSS/TP53+',
                       'MSS/TP53-')

    res <- res %>% left_join(tmp[, c('sample', 'TP53')], by = 'sample')

    res <- res %>% dplyr::mutate(subtype = dplyr::case_when(
      is.na(TP53) ~ as.character(MSI.EMT),!is.na(TP53) ~ as.character(TP53)
    ))
    res <- res[, c('sample', 'subtype')]
  } else if(method == 'TCGA'){
    res$subtype <- predict(
      gc.tcga$TCGA,
      Expr_impute[gc.tcga$required.gene,] %>% t() %>% scale() %>% as.data.frame()
    )
    res.tmp <- as.data.frame(predict(
      gc.tcga$TCGA,
      Expr_impute[gc.tcga$required.gene, ] %>% t() %>% scale() %>% as.data.frame(),
      type = "prob"
    ))
    res.tmp.label <- sapply(res.tmp, function(x) {
      round(x, digits = 2) %>% value2label()
    }) %>% as.data.frame()
    rownames(res) <- NULL
    res$subtype[which(apply(res.tmp, 1, max) < minPosterior)] <- NA
    res <- cbind(res, res.tmp.label)
  }
  return(res)
}
