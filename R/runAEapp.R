#' Run Adverse Events App
#'
#' @param launch.browser Boolean logic to launch application in browser. Default to TRUE.
#' @return An R Shiny Application.
#' @export
#' @rawNamespace import(shiny, except=c(dataTableOutput, renderDataTable))
#' @import shinycssloaders
#' @import shinythemes
#' @importFrom shinyjs useShinyjs
#' @import tidyverse
#' @import rio
#' @import janitor
#' @import DT
#' @import skimr
#' @import ggplot2
#' @import ggpubr
#' @import ggnewscale
#' @import ggrepel
#' @import survival
#' @import survminer
#' @import lubridate
#'
runAEapp <- function(launch.browser = TRUE) {
  appDir <- system.file("app", package = "AdverseEvents")
  if (appDir == "") {
    stop("Could not find myapp. Try re-installing `mypackage`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal", launch.browser = launch.browser)
}


