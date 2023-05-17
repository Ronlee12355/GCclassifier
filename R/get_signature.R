#' Extract signature genes in symbol/entrez ids from a specific gastric cancer molecular subtype classifier
#'
#' @param method Molecular subtype for which features should be reported. This can be a string of 'EMP','TCGA' and 'ACRG' (default EMP)
#'
#' @return A data frame with signature genes
#' @export
#' @importFrom AnnotationDbi mapIds
#' @import org.Hs.eg.db
#'
#' @examples
#' get_signature(method = 'EMP')
get_signature <- function(method = 'EMP') {
  if (is.null(method)) {
    stop('Choose which molecular subtype signature genes should be reported')
  }
  if (!(method %in% c('EMP', ' TCGA', 'ACRG'))) {
    stop("The molecular subtype should be among 'EMP','TCGA' and 'ACRG'")
  }

  if (method == 'EMP') {
    ex <- gc.mp$required.gene
  } else if (method == 'TCGA') {
    ex <- gc.tcga$required.gene
  } else{
    ex <- gc.acrg.emt$required.gene
  }
  ex.id <- suppressMessages(
    AnnotationDbi::mapIds(
      org.Hs.eg.db,
      keys = ex,
      keytype = 'SYMBOL',
      column = 'ENTREZID'
    )
  )
  res <- data.frame('Symbol' = ex, 'Entrez' = ex.id, 'Method' = as.character(method))
  rownames(res) <- NULL
  return(res)
}
