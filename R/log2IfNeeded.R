#' Log2 scale gene expression if needed
#'
#' @param df a data frame/matrix of gene expression that will be log2 transformed (default: NULL)
#'
#' @return log2 scaled gene expression in data frame or matrix format
#' @export
#'
log2IfNeeded <- function(df = NULL) {
  if (is.null(df)) {
    stop("Data frame/matrix 'df' has not been specified")
  }
  qx <-
    as.numeric(quantile(df, c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm = T))
  LogC <- (qx[5] > 100) ||
    (qx[6] - qx[1] > 50 && qx[2] > 0) ||
    (qx[2] > 0 && qx[2] < 1 && qx[4] > 1 && qx[4] < 2)
  if (LogC) {
    message('Performing log2 transformation ......')
    df[which(df < 0)] <- NA
    res <- log2(df + 1)
    return(res)
  } else{
    message('log2 transformation not needed')
    res <- df
    return(res)
  }
}
