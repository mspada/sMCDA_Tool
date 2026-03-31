#' Run the sMCDA Tool
#'
#' @param ... Named options to pass through `golem::with_golem_options()`.
#' @param port Port used by the Shiny application.
#' @param launch.browser Whether to open the app in a browser.
#'
#' @return Invisibly returns the running Shiny app.
#' @export
run_app <- function(..., port = getOption("shiny.port"), launch.browser = interactive()) {
  app_dir <- get_app_dir()
  legacy_app <- load_legacy_app(app_dir = app_dir)

  app <- golem::with_golem_options(
    app = shiny::shinyApp(
      ui = function(request) app_ui(request = request, legacy_app = legacy_app),
      server = function(input, output, session) {
        app_server(input = input, output = output, session = session, legacy_app = legacy_app)
      }
    ),
    golem_opts = list(...)
  )

  shiny::runApp(app, port = port, launch.browser = launch.browser)
}
