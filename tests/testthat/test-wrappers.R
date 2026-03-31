test_that("get_app_dir returns an existing app directory", {
  app_dir <- sMCDATool:::get_app_dir()

  expect_true(dir.exists(app_dir))
  expect_true(file.exists(file.path(app_dir, "global.R")))
  expect_true(file.exists(file.path(app_dir, "legacy_definitions.R")))
})

test_that("app_ui delegates to legacy ui", {
  legacy <- list(ui = "dummy-ui")

  expect_identical(
    sMCDATool:::app_ui(request = NULL, legacy_app = legacy),
    "dummy-ui"
  )
})

test_that("app_server delegates to legacy server", {
  state <- new.env(parent = emptyenv())
  state$called <- FALSE

  legacy_server <- function(input, output, session) {
    state$called <- TRUE
    state$input <- input
    state$output <- output
    state$session <- session
  }

  legacy <- list(server = legacy_server)
  sMCDATool:::app_server(input = "i", output = "o", session = "s", legacy_app = legacy)

  expect_true(state$called)
  expect_identical(state$input, "i")
  expect_identical(state$output, "o")
  expect_identical(state$session, "s")
})
