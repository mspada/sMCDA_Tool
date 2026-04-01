if (!exists("dbHeader", inherits = TRUE) ||
    !exists("dashboardPage", mode = "function", inherits = TRUE)) {
  source("global.R", local = TRUE)
}

source("legacy_definitions.R", local = TRUE)
shiny::shinyApp(ui, server)
