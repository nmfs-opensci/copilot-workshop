# copilot-workshop

## NWFSC Survey Point SST Shiny App
This repository includes a single-file R Shiny application (`app.R`) that
visualizes sea surface temperature (SST) at NWFSC survey locations along the
Washington and Oregon coasts. The app uses the survey point data from
`surveyjoin::nwfsc_grid()`, fetches SST data from the NOAA ERDDAP server
(`ncdcOisst21Agg_LonPM180`), and colors each survey point by the nearest SST
value for the selected date (with a short lookback window if data for the
requested date are not yet available).

## Run the app
1. Install R and the required packages:
   ```r
   install.packages(c(
     "shiny", "leaflet", "rerddap", "surveyjoin", "sf", "viridis"
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
