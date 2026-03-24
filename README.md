# copilot-workshop

## NWFSC Survey Grid SST Shiny App
This repository includes a single-file R Shiny application (`app.R`) that
visualizes mean sea surface temperature (SST) across NWFSC survey grid
polygons along the Washington and Oregon coasts. The app fetches SST data
from the NOAA ERDDAP server (`ncdcOisst21Agg_LonPM180`) and colors each grid
cell by its mean SST for the selected date (with a short lookback window if
data for the requested date are not yet available).

## Run the app
1. Install R and the required packages:
   ```r
   install.packages(c(
     "shiny", "leaflet", "rerddap", "surveyjoin", "sf", "dplyr", "viridis"
   ))
   ```

2. Run the application from the repository root:
   ```r
   shiny::runApp("app.R", host = "0.0.0.0", port = 3838)
   ```

3. Open the app in your browser:
   ```
   http://localhost:3838
   ```
