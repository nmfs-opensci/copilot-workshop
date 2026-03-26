# NWFSC Survey Grid – SST Shiny App

An interactive R Shiny application that visualizes **Mean Sea Surface Temperature (SST)** on the NWFSC survey grid points along the US West Coast.

## Features

* Leaflet map centered on the WA/OR coast.
* Date picker populated with the valid date range from the NOAA ERDDAP server.
* Grid points coloured by SST using the **viridis** colour palette with a legend.
* Click any point to see its **Grid Cell ID**, coordinates, and SST value in a popup and sidebar panel.
* Data are fetched from ERDDAP only when the selected date changes (reactive).

## Data Sources

| Source | Details |
|--------|---------|
| **SST** | NOAA ERDDAP – dataset `ncdcOisst21Agg_LonPM180` via the `rerddap` package |
| **Grid** | `surveyjoin::nwfsc_grid` (filtered to `survey == "NWFSC.Combo"`) |

## Prerequisites

* R ≥ 4.1
* System libraries (Ubuntu/Debian): `libcurl4-openssl-dev`, `libssl-dev`, `libxml2-dev`, `libgdal-dev`, `libgeos-dev`, `libproj-dev`, `libudunits2-dev`

Install R packages:

```r
install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")
pak::pkg_install(c("shiny", "leaflet", "rerddap", "sf", "dplyr", "viridis"))
pak::pkg_install("DFO-NOAA-Pacific/surveyjoin")
```

## Running the App

```r
shiny::runApp("app.R")
```

Or from the terminal:

```bash
Rscript -e "shiny::runApp('app.R')"
```

The app will open in your browser. Use the **date picker** in the sidebar to choose a date; the map will update automatically with SST values for that day.
