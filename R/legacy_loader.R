get_app_dir <- function() {
  app_dir <- system.file("app", package = "sMCDATool")

  if (app_dir == "") {
    local_app_dir <- file.path("inst", "app")
    if (dir.exists(local_app_dir)) {
      app_dir <- normalizePath(local_app_dir)
    }
  }

  if (app_dir == "") {
    stop("Could not find the app directory (installed package or local inst/app).", call. = FALSE)
  }

  app_dir
}

load_legacy_app <- function(app_dir = get_app_dir()) {
  www_dir <- file.path(app_dir, "www")
  paths <- shiny::resourcePaths()

  if (dir.exists(www_dir)) {
    if ("smcda-www" %in% names(paths)) {
      shiny::removeResourcePath("smcda-www")
    }
    shiny::addResourcePath("smcda-www", www_dir)
  }

  old_asset_prefix <- Sys.getenv("SMCDA_ASSET_PREFIX", unset = "")
  on.exit(Sys.setenv(SMCDA_ASSET_PREFIX = old_asset_prefix), add = TRUE)
  Sys.setenv(SMCDA_ASSET_PREFIX = "smcda-www")

  legacy_env <- new.env(parent = globalenv())
  sys.source(file.path(app_dir, "global.R"), envir = legacy_env)
  sys.source(file.path(app_dir, "legacy_definitions.R"), envir = legacy_env)

  if (!exists("ui", envir = legacy_env, inherits = FALSE) ||
      !exists("server", envir = legacy_env, inherits = FALSE)) {
    stop("Legacy app scripts did not create 'ui' and 'server' objects.", call. = FALSE)
  }

  list(
    ui = get("ui", envir = legacy_env, inherits = FALSE),
    server = get("server", envir = legacy_env, inherits = FALSE)
  )
}
