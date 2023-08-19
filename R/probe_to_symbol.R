#' @title Aggregate gene expression profile(s) from probeset IDs to gene identifiers for microarray-based datasets
#' @description This function could help to convert the gene expression profiles from probeset level to
#' gene identifier (SYMBOL, ENSEMBL, ENTREZID) level.
#'
#' @param Expr a dataframe or matrix with log2 scaled gene expression profiles, with samples in columns,
#' and probes in rows, rownames corresponding to the "probe" column in annotation file.
#' @param annotation probeset ID annotation file, must have "probe" and "symbol" in colnames,
#' "probe" corresponding to rownames in Expr and "sample" corresponding to the
#' specified gene identifiers.
#' @param aggregateMethod the method to calculate gene expression values if multiple probes
#' map to the same symbol. The default is max.
#'
#' @importFrom magrittr %>%
#'
#' @return a dataframe with log2 scaled gene expression profiles in specified gene identifier level.
#' @export
#'
#' @examples
#' \dontrun{
#' expr <- data.frame('v1'=runif(5), 'v2'=runif(5))
#' rownames(expr) <- paste0('xx', 1:5)
#' anno <- data.frame('probe'=paste0('xx', 1:5), 'symbol'=c(letters[1:4], 'd'))
#' probe_to_symbol(Expr = expr, annotation = anno, aggregateMethod = 'max')
#' }

probe_to_symbol <-
  function(Expr,
           annotation,
           aggregateMethod = 'max')
  {
    if (!is.data.frame(Expr) & !is.matrix(Expr)) {
      stop('Only gene expression profiles in dataframe or matrix format is accepted.')
    }
    if (is.null(rownames(Expr)) | is.null(colnames(Expr))) {
      stop('Rownames and colnames are madatory in gene expression profiles.')
    }
    if (sum(apply(Expr, 2, is.numeric)) != ncol(Expr)) {
      stop('Only numeric values in gene expression profiles is accepted.')
    }
    if (any(Expr < 0, na.rm = T)) {
      stop('Gene expression profiles cannot contain any negative value(s).')
    }
    if (any(is.na(Expr))) {
      stop('Gene expression profiles cannot contain any NA value(s).')
    }
    if (sum(c('probe', 'symbol') %in% colnames(Expr)) != 0) {
      stop('Colnames in gene expression profiles shoud not contain `probe` and `symbol`.')
    }
    Expr <- as.data.frame(Expr)

    ## ===== annotation check =========
    if (!is.data.frame(annotation)) {
      stop('Only annotation in dataframe format is accepted.')
    }
    if (sum(c('probe', 'symbol') %in% colnames(annotation)) != 2) {
      stop(
        'Colnames in gene annotation file shoud contain `probe` and `symbol` for further analysis.'
      )
    }
    if (is.null(colnames(Expr))) {
      stop('Colnames are mandatory in gene annotation.')
    }
    annotation <- as.data.frame(annotation)[, c('probe', 'symbol')]

    if (is.null(aggregateMethod) |
        !(aggregateMethod %in% c('mean', 'median', 'max')) |
        length(aggregateMethod) != 1) {
      stop('aggregateMethod should be one of "mean", "median" and "max".')
    }

    if (length(intersect(rownames(Expr), annotation$probe)) <= 0) {
      stop('No overlapping probes in gene expression and annotation profiles.')
    }

    Expr$probe <- rownames(Expr)
    Expr <-
      Expr %>% dplyr::left_join(annotation, by = 'probe') %>% dplyr::filter(!is.na(symbol)) %>%
      dplyr::select(-c(probe)) %>% dplyr::group_by(symbol) %>% dplyr::summarise_all(aggregateMethod, na.rm = T) %>%
      as.data.frame()
    rownames(Expr) <- Expr$symbol
    Expr <- Expr %>% dplyr::select_if(is.numeric) %>% as.data.frame() %>% log2IfNeeded()
    return(Expr)
  }
