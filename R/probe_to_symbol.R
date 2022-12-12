#' @title Aggregate gene expression profile in probe level to symbol level
#' @description This function could help to convert the gene expression from probe level to
#' symbol (SYMBOL, ENSEMBL, ENTREZID) level.
#'
#' @param Expr a dataframe with log2 scaled gene expression profile, samples in columns,
#' probes in rows, rownames corresponding to the probe in annotation file.
#' @param annotation probe annotation file, must have "probe" and "symbol" in colnames,
#' "probe" corresponding to rownames in Expr and "sample" corresponding to the specified gene identifier.
#' @param aggregateMethod the method to calculate expression values if multiple probes map to the same symbol.
#' The default is max.
#'
#' @importFrom magrittr %>%
#'
#' @return a dataframe with log2 scaled gene expression profile in symbol level.
#' @export
#'
#' @examples
#' \dontrun{
#' expr <- data.frame('v1'=runif(5), 'v2'=runif(5))
#' rownames(expr) <- paste0('xx', 1:5)
#' anno <- data.frame('probe'=paste0('xx', 1:5), 'symbol'=c(letters[1:4], 'd'))
#' probe_to_symbol(Expr = expr, annotation = anno, aggregateMethod = 'max')
#' }

probe_to_symbol <- function(Expr, annotation, aggregateMethod = c('mean', 'median', 'max')){
  if(!is.data.frame(Expr)){
    stop('Only gene expression profile in dataframe format is accepted.')
  }
  if(is.null(rownames(Expr)) | is.null(colnames(Expr))){
    stop('Rownames and colnames are madatory in gene expression profile.')
  }
  if(sum(sapply(Expr, is.numeric)) != ncol(Expr)){
    stop('Only numeric values in gene expression profile is accepted.')
  }
  if(any(Expr < 0, na.rm = T)){
    stop('Gene expression profile cannot contain any negative value(s).')
  }
  if(any(is.na(Expr))){
    stop('Gene expression profile cannot contain any NA value(s).')
  }
  if(sum(c('probe', 'symbol') %in% colnames(Expr)) != 0){
    stop('Colnames in gene expression profile shoud not contain `probe` and `symbol`.')
  }
  Expr <- as.data.frame(Expr)

  ## ===== annotation check =========
  if(!is.data.frame(annotation)){
    stop('Only annotation in dataframe format is accepted.')
  }
  if(sum(c('probe', 'symbol') %in% colnames(annotation)) != 2){
    stop('Colnames in gene annotation file shoud contain `probe` and `symbol` for further analysis.')
  }
  if(is.null(colnames(Expr))){
    stop('Colnames are madatory in gene annotation.')
  }
  annotation <- as.data.frame(annotation)[,c('probe', 'symbol')]

  if(is.null(aggregateMethod) | !(aggregateMethod %in% c('mean', 'median', 'max')) | length(aggregateMethod) != 1){
    stop('aggregateMethod should be one of "mean", "median" and "max".')
  }

  if(length(intersect(rownames(Expr), annotation$probe)) <= 0){
    stop('No overlapping porbes in gene expression and annotation profiles.')
  }

  Expr$probe <- rownames(Expr)
  Expr <- Expr %>% dplyr::left_join(annotation, by='probe') %>% dplyr::filter(!is.na(symbol)) %>%
    dplyr::select(-c(probe)) %>% dplyr::group_by(symbol) %>% dplyr::summarise_all(aggregateMethod, na.rm = T) %>%
    as.data.frame()
  rownames(Expr) <- Expr$symbol
  Expr <- Expr %>% dplyr::select_if(is.numeric) %>% as.data.frame()
  return(Expr)
}
