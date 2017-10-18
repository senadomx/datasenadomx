#' @title Explores the data in the package
#' @description Check the data
#' @export
data_explorer <- function() {
  appDir <- system.file("shinyapp", package = "datasenadomx")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `datasenadomx`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
