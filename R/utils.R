#' To check whether the GEP should perform log2 transformation
#'
#' @param df a dataframe of GEP
#'
#' @return a logical value (TRUE/FALSE)
toLog2 <- function(df){
  qx <- as.numeric(quantile(df, c(0., 0.25, 0.5, 0.75, 0.99, 1.0), na.rm=T))
  LogC <- (qx[5] > 100) ||
    (qx[6]-qx[1] > 50 && qx[2] > 0) ||
    (qx[2] > 0 && qx[2] < 1 && qx[4] > 1 && qx[4] < 2)
  if (LogC) {
    return(TRUE)
  }else{
    return(FALSE)
  }
}
