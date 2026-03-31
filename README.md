# sMCDA Tool

sMCDA Tool e una Shiny app per problemi di **spatial Multi-Criteria Decision Analysis (sMCDA)**.
Supporta:

- metodo **Weighted Sum**
- metodo **ELECTRE-TRI (Outranking)**
- gestione di input certi/incerti con approccio SMAA
- export risultati in immagini, CSV, XLSX e shapefile ESRI

## Struttura del progetto

La codebase e stata portata in un setup compatibile con `{golem}` mantenendo la logica legacy.

- `R/run_app.R`: entrypoint principale
- `R/app_ui.R`: wrapper UI
- `R/app_server.R`: wrapper server
- `R/legacy_loader.R`: caricamento logica legacy
- `inst/app/legacy_definitions.R`: `ui` / `server` originali

## Requisiti comuni

- **R 4.5.x** (il lockfile e stato aggiornato con R `4.5.1`)
- connessione internet (prima installazione/restauro)
- spazio disco sufficiente per libreria `renv`

## Prerequisiti per OS

### Windows (10/11)

1. Installa **R 4.5.x** da CRAN.
2. Installa **Rtools45** (necessario se qualche pacchetto deve compilare da sorgente).
3. In genere i pacchetti CRAN binari evitano dipendenze di sistema manuali per `sf`/`terra`.

### macOS (Intel e Apple Silicon)

1. Installa **R 4.5.x**.
2. Installa Command Line Tools:

```bash
xcode-select --install
```

3. Se necessario per compilazioni da sorgente (soprattutto `sf`/`terra`), installa librerie geospaziali:

```bash
brew install gdal geos proj udunits
```

### Linux (Debian/Ubuntu)

Installa toolchain e librerie di sistema piu comuni per i pacchetti del progetto:

```bash
sudo apt update
sudo apt install -y \
  build-essential gfortran \
  libcurl4-openssl-dev libssl-dev libxml2-dev \
  libfontconfig1-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
  libharfbuzz-dev libfribidi-dev \
  libudunits2-dev libgdal-dev libgeos-dev libproj-dev
```

### Linux (Fedora/RHEL/CentOS stream)

```bash
sudo dnf install -y \
  gcc gcc-c++ gcc-gfortran make \
  libcurl-devel openssl-devel libxml2-devel \
  fontconfig-devel freetype-devel libpng-devel libtiff-devel libjpeg-turbo-devel \
  harfbuzz-devel fribidi-devel \
  udunits2-devel gdal-devel geos-devel proj-devel
```

## Installazione con `renv` (consigliata)

> Questa e la procedura raccomandata su tutti gli OS.

1. Clona il repository e entra nella cartella progetto.
2. Avvia R o RStudio **nella root del progetto**.
3. Attiva `renv` e ripristina l'ambiente:

```r
source("renv/activate.R")
renv::restore(prompt = FALSE)
```

4. Verifica che sia tutto coerente:

```r
renv::status()
```

Se lo stato e corretto, deve risultare senza problemi.

## Come avviare l'app

### Opzione A (consigliata, entrypoint golem)

```r
source("R/legacy_loader.R")
source("R/app_ui.R")
source("R/app_server.R")
source("R/run_app.R")
run_app()
```

### Opzione B (installazione package locale)

```r
install.packages("remotes")
remotes::install_local(".")
sMCDATool::run_app()
```

### Opzione C (legacy)

```r
shiny::runApp("inst/app")
```

## Guida pratica a `renv` in questo progetto

### Come funziona qui

- `.Rprofile` esegue `source("renv/activate.R")` all'apertura del progetto.
- `renv.lock` e la sorgente di verita delle versioni pacchetti.
- la libreria locale e isolata dal tuo ambiente globale R.

### Comandi fondamentali

Ripristino ambiente da lockfile:

```r
renv::restore(prompt = FALSE)
```

Controllo coerenza lockfile/libreria:

```r
renv::status()
```

Aggiornare dipendenze (quando vuoi cambiare versioni):

```r
renv::update()
renv::snapshot(prompt = FALSE)
```

Reinstallare un pacchetto problematico:

```r
renv::rebuild("sf")
```

Pulire pacchetti non piu usati:

```r
renv::clean()
```

### Workflow consigliato team

1. `renv::restore()` dopo `git pull`.
2. Sviluppo normale.
3. Se aggiorni dipendenze: `renv::snapshot()`.
4. Committa sempre almeno `renv.lock` (e file `renv/` se modificati).

## Troubleshooting

### Errore download CRAN / rete bloccata

Sintomo tipico: errori su `cloud.r-project.org`.

- verifica proxy/firewall aziendale
- riprova con connessione libera
- in casi aziendali configura mirror/proxy CRAN

### Errori su `sf`, `terra`, `units`

Quasi sempre mancano librerie di sistema geospaziali.
Installa i prerequisiti OS sopra e rilancia:

```r
renv::restore(prompt = FALSE)
```

### Warning "package was built under R version ..."

Di solito e solo un warning di compatibilita binaria. Se l'app parte, non e bloccante.
Per riallineare completamente, usa R della stessa minor release del team e riesegui `renv::restore()`.

### `webshot` / PhantomJS

L'app usa `webshot` per export mappe. Alla prima esecuzione puo essere richiesto setup di PhantomJS.
Se serve manualmente:

```r
webshot::install_phantomjs()
```

## Licenza

Questo progetto e distribuito sotto **GNU GPL v2**.
Vedi il file [LICENSE](LICENSE).
