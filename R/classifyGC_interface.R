#' @title The shiny interface of gastric cancer molecular subtypes prediction
#' @description This function runs the shiny app for subtypes prediction of gastric cancer. The maximum upload file size is 30MB.
#' @export
#' @import shiny
#'
#' @examples
#' classifyGC_interface()
classifyGC_interface <- function() {
  appDir <- system.file('shiny', package = 'GCclassifier')

  if (appDir == '') {
    stop('Could not load shiny directory. Try re-install `GCclassifier`')
  }

  shiny::runApp(appDir, display.mode = 'normal')
}
