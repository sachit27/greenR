#' Run Shiny App
#'
#' This function runs the included Shiny app. The app provides an interactive
#' interface to use the functions in this package. You can download OSM data,
#' calculate green indices, plot green index, and save green index data as a
#' JSON file or as a Leaflet map in an HTML file.
#'
#' @return No return value, called for side effects
#' @importFrom shiny runApp
#' @importFrom DT renderDataTable datatable
#' @examples
#' \dontrun{
#'   run_app()
#' }
#' @export
run_app <- function() {
  app_dir <- system.file("shiny", package = "greenR")
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Please ensure that the package has been installed.")
  }

  message("Starting the Shiny app for greenR. Please wait...")
  shiny::runApp(app_dir)
}
