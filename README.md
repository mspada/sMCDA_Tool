# sMCDA Tool

sMCDA Tool is a Shiny application for **spatial Multi-Criteria Decision Analysis (sMCDA)**.
It supports:

- **Weighted Sum**
- **ELECTRE-TRI (Outranking)**
- deterministic and uncertain inputs (SMAA-style uncertainty handling)
- export to images, CSV, XLSX, and ESRI shapefiles

## Project Layout

The app is now organized in a golem-compatible structure while preserving legacy logic.

- `R/run_app.R`: main entrypoint
- `R/app_ui.R`: UI wrapper
- `R/app_server.R`: server wrapper
- `R/legacy_loader.R`: loader for legacy app definitions
- `inst/app/legacy_definitions.R`: legacy `ui` and `server`
- `inst/app/global.R`: legacy global setup and utility functions

## Requirements

- **R 4.5.x** (the lockfile was generated with R `4.5.1`)
- internet connection for first restore/install
- enough disk space for the local `renv` library

## OS Setup

### Windows (10/11)

1. Install **R 4.5.x** from CRAN.
2. Install **Rtools45** (needed if source compilation is required).
3. Most CRAN binaries should avoid manual system setup for `sf`/`terra`.

### macOS (Intel and Apple Silicon)

1. Install **R 4.5.x**.
2. Install Command Line Tools:

```bash
xcode-select --install
```

3. If source compilation is needed (especially for geospatial packages):

```bash
brew install gdal geos proj udunits
```

### Linux (Debian/Ubuntu)

```bash
sudo apt update
sudo apt install -y \
  build-essential gfortran \
  libcurl4-openssl-dev libssl-dev libxml2-dev \
  libfontconfig1-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
  libharfbuzz-dev libfribidi-dev \
  libudunits2-dev libgdal-dev libgeos-dev libproj-dev
```

### Linux (Fedora/RHEL/CentOS Stream)

```bash
sudo dnf install -y \
  gcc gcc-c++ gcc-gfortran make \
  libcurl-devel openssl-devel libxml2-devel \
  fontconfig-devel freetype-devel libpng-devel libtiff-devel libjpeg-turbo-devel \
  harfbuzz-devel fribidi-devel \
  udunits2-devel gdal-devel geos-devel proj-devel
```

## Recommended Installation with `renv`

1. Clone the repository and open R/RStudio in the project root.
2. Activate and restore dependencies:

```r
source("renv/activate.R")
renv::restore(prompt = FALSE)
```

3. Verify environment consistency:

```r
renv::status()
```

If everything is correct, `renv::status()` should report no issues.

## Run the App

### Option A (recommended, simple bootstrap)

```r
source("app.R")
run_app()
```

### Option B (direct golem entrypoint)

```r
source("R/legacy_loader.R")
source("R/app_ui.R")
source("R/app_server.R")
source("R/run_app.R")
run_app()
```

### Option C (install local package)

```r
install.packages("remotes")
remotes::install_local(".")
sMCDATool::run_app()
```

### Option D (legacy app-dir run)

```r
shiny::runApp("inst/app")
```

## `renv` Workflow in This Project

### What `renv` controls

- `renv.lock` stores exact package versions.
- The project uses an isolated local library (not your global user library).
- Every collaborator should restore from `renv.lock` for reproducibility.

### Core commands

Restore from lockfile:

```r
renv::restore(prompt = FALSE)
```

Check lockfile/library consistency:

```r
renv::status()
```

Update dependencies and record them:

```r
renv::update()
renv::snapshot(prompt = FALSE)
```

Rebuild a problematic package:

```r
renv::rebuild("sf")
```

Clean unused packages:

```r
renv::clean()
```

### Team workflow

1. Run `renv::restore()` after each `git pull`.
2. Develop normally.
3. If you intentionally change dependency versions, run `renv::snapshot()`.
4. Commit `renv.lock` (and `renv/` files if they changed).

## Troubleshooting

### CRAN download/network errors

Typical symptom: errors reaching `cloud.r-project.org`.

- check proxy/firewall settings
- retry on a different network
- configure a company-approved CRAN mirror if required

### `sf` / `terra` / `units` installation errors

Usually caused by missing system libraries.
Install OS prerequisites above, then run:

```r
renv::restore(prompt = FALSE)
```

### Warning: "package was built under R version ..."

Usually non-blocking if the app starts correctly.
For full alignment, use the same R minor version as the team and re-run `renv::restore()`.

### `webshot` / PhantomJS

Map export uses `webshot`. If needed:

```r
webshot::install_phantomjs()
```

## Root Files (Why They Exist)

- `.gitignore`: **required** to avoid committing temporary/local files.
- `.Rbuildignore`: recommended for R package builds to exclude non-package artifacts.

## License

This project is licensed under **GNU GPL v2**.
See [LICENSE](LICENSE).
