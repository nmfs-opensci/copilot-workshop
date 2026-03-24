library(shiny)
library(leaflet)
library(rerddap)
library(surveyjoin)
library(viridis)
library(dplyr)

erddap_url <- "https://coastwatch.pfeg.noaa.gov/erddap/"
dataset_id <- "ncdcOisst21Agg_LonPM180"

nwfsc_grid <- surveyjoin::nwfsc_grid |>
  filter(survey == "NWFSC.Combo") |>
  mutate(grid_id = row_number())

grid_bbox <- list(
  min_lon = min(nwfsc_grid$lon, na.rm = TRUE),
  max_lon = max(nwfsc_grid$lon, na.rm = TRUE),
  min_lat = min(nwfsc_grid$lat, na.rm = TRUE),
  max_lat = max(nwfsc_grid$lat, na.rm = TRUE)
)

get_available_dates <- function() {
  info_list <- rerddap::info(dataset_id, url = erddap_url)
  time_meta <- info_list$alldata$time
  range_row <- time_meta[time_meta$attribute_name == "actual_range", , drop = FALSE]
  if (nrow(range_row) == 0) {
    stop("Unable to determine dataset time range from ERDDAP metadata.")
  }
  values <- trimws(strsplit(range_row$value[1], ",")[[1]])
  numeric_range <- as.numeric(values)
  start_time <- as.POSIXct(numeric_range[1], origin = "1970-01-01", tz = "UTC")
  end_time <- as.POSIXct(numeric_range[2], origin = "1970-01-01", tz = "UTC")
  seq.Date(as.Date(start_time), as.Date(end_time), by = "day")
}

available_dates <- get_available_dates()
default_date <- max(available_dates, na.rm = TRUE)

ui <- fluidPage(
  titlePanel("NWFSC Survey Grid Mean SST"),
  sidebarLayout(
    sidebarPanel(
      dateInput(
        "date",
        "Select date",
        value = default_date,
        min = min(available_dates, na.rm = TRUE),
        max = max(available_dates, na.rm = TRUE),
        format = "yyyy-mm-dd"
      ),
      helpText("SST from NOAA ERDDAP dataset ncdcOisst21Agg_LonPM180.")
    ),
    mainPanel(
      leafletOutput("map", height = 700)
    )
  )
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -124.2, lat = 44.5, zoom = 5)
  })

  sst_points <- eventReactive(input$date, {
    req(input$date)
    selected_date <- as.Date(input$date)
    date_string <- format(selected_date, "%Y-%m-%d")
    sst_raw <- withProgress(message = "Fetching SST data...", {
      rerddap::griddap(
        dataset_id,
        url = erddap_url,
        time = c(date_string, date_string),
        latitude = c(grid_bbox$min_lat, grid_bbox$max_lat),
        longitude = c(grid_bbox$min_lon, grid_bbox$max_lon),
        fields = "sst"
      )
    })

    sst_data <- sst_raw$data
    sst_data <- sst_data[!is.na(sst_data$sst), , drop = FALSE]
    req(nrow(sst_data) > 0)

    sst_lats <- sort(unique(sst_data$latitude))
    sst_lons <- sort(unique(sst_data$longitude))
    nearest_value <- function(target, choices) {
      choices[which.min(abs(choices - target))]
    }

    sst_lookup <- sst_data |>
      select(latitude, longitude, sst) |>
      distinct()

    nwfsc_grid |>
      mutate(
        sst_lat = vapply(lat, nearest_value, numeric(1), choices = sst_lats),
        sst_lon = vapply(lon, nearest_value, numeric(1), choices = sst_lons)
      ) |>
      left_join(
        sst_lookup,
        by = c("sst_lat" = "latitude", "sst_lon" = "longitude")
      ) |>
      rename(mean_sst = sst)
  })

  observeEvent(sst_points(), {
    points <- sst_points()
    points <- points |>
      mutate(
        popup_text = sprintf(
          "Grid Cell ID: %s<br>Mean SST: %s °C",
          grid_id,
          ifelse(is.na(mean_sst), "NA", sprintf("%.2f", mean_sst))
        )
      )
    pal <- colorNumeric(viridis(256), domain = points$mean_sst, na.color = "#cccccc")

    leafletProxy("map", data = points) |>
      clearMarkers() |>
      clearControls() |>
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        fillColor = ~pal(mean_sst),
        color = ~pal(mean_sst),
        fillOpacity = 0.8,
        radius = 4,
        stroke = FALSE,
        popup = ~popup_text
      ) |>
      addLegend(
        "bottomright",
        pal = pal,
        values = ~mean_sst,
        title = "Mean SST (°C)",
        opacity = 1
      )
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
