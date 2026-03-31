# sMCDA Tool
sMCDA Tool is a Shiny tool for spatial Multi-Criteria Decision Analysis problems. The tool considers definite or uncertain inputs and can run two different types of Multi-Criteria Decision Analysis methods: i) weighted sum; ii) ELECTRE-TRI. In both cases, the tool can run a SMAA approach for uncertainty quantification. Results can be saved as images, csv, xlsx and ESRI shape files.

## Run in a golem environment

The project is now refactored around a golem-compatible structure:

- `R/run_app.R`: golem entrypoint
- `R/app_ui.R`: UI wrapper
- `R/app_server.R`: server wrapper
- `R/legacy_loader.R`: loader for the current legacy logic
- `inst/app/legacy_definitions.R`: legacy `ui`/`server` definitions

From the project root:

```r
install.packages("golem")
remotes::install_local(".")
sMCDATool::run_app()
```

Quick local run (without package install):

```r
source("R/legacy_loader.R")
source("R/app_ui.R")
source("R/app_server.R")
source("R/run_app.R")
run_app()
```

Legacy entrypoints remain available:

```r
shiny::runApp("inst/app")
source("app.r")
```
