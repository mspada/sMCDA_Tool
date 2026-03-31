# Convenience bootstrap for local development:
# source("app.R")
# run_app()

local({
  r_minor <- paste0("R-", R.version$major, ".", sub("\\..*$", "", R.version$minor))
  lib_root <- file.path("renv", "library", r_minor)

  if (dir.exists(lib_root)) {
    platform_libs <- list.dirs(lib_root, recursive = FALSE, full.names = TRUE)
    if (length(platform_libs) > 0) {
      .libPaths(c(platform_libs, .libPaths()))
    }
  }
})

source("R/legacy_loader.R")
source("R/app_ui.R")
source("R/app_server.R")
source("R/run_app.R")

if (!exists("run_app", mode = "function")) {
  stop("Could not load run_app(). Check package setup and dependencies.", call. = FALSE)
}

message("sMCDATool loaded. Run `run_app()`.")
