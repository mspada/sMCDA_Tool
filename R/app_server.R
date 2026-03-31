app_server <- function(input, output, session, legacy_app) {
  legacy_app$server(input, output, session)
}
