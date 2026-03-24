# copilot-workshop

## NWFSC Survey Grid SST Shiny App

This repository contains a Shiny app that maps mean sea surface temperature (SST)
for the NWFSC survey grid points using NOAA ERDDAP data.

### Run the app

1. Install required R packages:
   - shiny
   - leaflet
   - rerddap
   - surveyjoin
   - viridis
   - dplyr
2. Start the app from the repository root:

```r
shiny::runApp("app.R")
```

3. Open your browser to <http://localhost:3838> if it does not open automatically.
