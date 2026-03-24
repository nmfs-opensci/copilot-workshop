# copilot-workshop

This repository contains an R Shiny app that visualizes mean sea surface temperature
(SST) on the NWFSC survey grid using NOAA ERDDAP data.

## Requirements

- R (>= 4.0 recommended)
- Internet access to NOAA ERDDAP

Install the required packages:

```r
install.packages(c("shiny", "leaflet", "rerddap", "viridis"))
pak::pkg_install("DFO-NOAA-Pacific/surveyjoin")
```

## Run the app

From the repository root, run:

```r
shiny::runApp("app.R")
```

The app will start on port 3838 by default and display a leaflet map of the NWFSC
survey grid colored by mean SST for the selected date.
