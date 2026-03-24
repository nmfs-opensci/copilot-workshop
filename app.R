library(shiny)
library(leaflet)
library(rerddap)
library(surveyjoin)
library(sf)
library(viridis)

ERDDAP_DATASET_ID <- "ncdcOisst21Agg_LonPM180"
BBOX_BUFFER_DEGREES <- 0.5
MAX_LOOKBACK_DAYS <- 7

nwfsc_grid_raw <- surveyjoin::nwfsc_grid()

if (inherits(nwfsc_grid_raw, "sf")) {
  nwfsc_points <- nwfsc_grid_raw
  current_crs <- sf::st_crs(nwfsc_points)
  if (is.na(current_crs)) {
    sf::st_crs(nwfsc_points) <- 4326
  } else if (!isTRUE(current_crs$epsg == 4326)) {
    nwfsc_points <- sf::st_transform(nwfsc_points, 4326)
  }

  if (any(sf::st_geometry_type(nwfsc_points) != "POINT")) {
    nwfsc_points <- sf::st_centroid(nwfsc_points)
  }

  if (!("lon" %in% names(nwfsc_points)) || !("lat" %in% names(nwfsc_points))) {
    coords <- sf::st_coordinates(nwfsc_points)
    nwfsc_points$lon <- coords[, 1]
    nwfsc_points$lat <- coords[, 2]
  }

  lon_column <- "lon"
  lat_column <- "lat"
} else {
  blank_name <- names(nwfsc_grid_raw) == ""
  if (any(blank_name)) {
    names(nwfsc_grid_raw)[blank_name] <- "grid_id"
  }

  lon_candidates <- names(nwfsc_grid_raw)[grepl("lon", names(nwfsc_grid_raw), ignore.case = TRUE)]
  lat_candidates <- names(nwfsc_grid_raw)[grepl("lat", names(nwfsc_grid_raw), ignore.case = TRUE)]

  if (length(lon_candidates) == 0 || length(lat_candidates) == 0) {
    stop("nwfsc_grid data must include longitude and latitude columns.")
  }

  lon_column <- lon_candidates[1]
  lat_column <- lat_candidates[1]

  nwfsc_points <- sf::st_as_sf(
    nwfsc_grid_raw,
    coords = c(lon_column, lat_column),
    crs = 4326,
    remove = FALSE
  )
}

palette_base <- viridis(256)

id_candidates <- names(nwfsc_points)[
  grepl("grid", names(nwfsc_points), ignore.case = TRUE) |
    grepl("id$", names(nwfsc_points), ignore.case = TRUE)
]
if (length(id_candidates) == 0) {
  nwfsc_points$grid_id <- seq_len(nrow(nwfsc_points))
  id_column <- "grid_id"
} else {
  id_column <- id_candidates[1]
}

bbox <- sf::st_bbox(nwfsc_points)
buffer_degrees <- BBOX_BUFFER_DEGREES
bbox_expanded <- bbox
bbox_expanded[c("xmin", "ymin")] <- bbox[c("xmin", "ymin")] - buffer_degrees
bbox_expanded[c("xmax", "ymax")] <- bbox[c("xmax", "ymax")] + buffer_degrees

bbox_center <- c(
  lng = mean(c(bbox_expanded["xmin"], bbox_expanded["xmax"])),
  lat = mean(c(bbox_expanded["ymin"], bbox_expanded["ymax"]))
)

fetch_sst <- function(target_date) {
  candidate_dates <- rev(
    seq.Date(from = target_date - MAX_LOOKBACK_DAYS, to = target_date, by = "1 day")
  )
  for (candidate_date in candidate_dates) {
    result <- tryCatch(
      rerddap::griddap(
        ERDDAP_DATASET_ID,
        time = c(as.character(candidate_date), as.character(candidate_date)),
        longitude = c(bbox_expanded["xmin"], bbox_expanded["xmax"]),
        latitude = c(bbox_expanded["ymin"], bbox_expanded["ymax"]),
        fields = "sst"
      ),
      error = function(e) NULL
    )

    if (!is.null(result) && !is.null(result$data) && nrow(result$data) > 0) {
      return(
        list(
          data = result$data,
          requested_date = target_date,
          actual_date = as.Date(candidate_date)
        )
      )
    }
  }

  list(data = NULL, requested_date = target_date, actual_date = NA)
}

ui <- fluidPage(
  titlePanel("NWFSC Survey Point SST"),
  sidebarLayout(
    sidebarPanel(
      dateInput(
        "sst_date",
        "Select date (last 30 days)",
        value = Sys.Date() - 2,
        min = Sys.Date() - 30,
        max = Sys.Date()
      ),
      textOutput("status")
    ),
    mainPanel(
      leafletOutput("map", height = 700)
    )
  )
)

server <- function(input, output, session) {
  sst_result <- eventReactive(input$sst_date, {
    req(input$sst_date)
    withProgress(message = "Fetching SST data...", value = 0, {
      fetch_sst(as.Date(input$sst_date))
    })
  }, ignoreNULL = FALSE)

  point_sst <- reactive({
    result <- sst_result()

    point_data <- nwfsc_points
    point_data$sst <- NA_real_

    if (!is.null(result$data)) {
      sst_data <- result$data
      names(sst_data) <- tolower(names(sst_data))

      lon_matches <- names(sst_data)[grepl("lon", names(sst_data))]
      lat_matches <- names(sst_data)[grepl("lat", names(sst_data))]
      sst_matches <- names(sst_data)[grepl("sst", names(sst_data))]

      sst_lon_column <- if (length(lon_matches) > 0) lon_matches[1] else NA_character_
      sst_lat_column <- if (length(lat_matches) > 0) lat_matches[1] else NA_character_
      sst_column <- if (length(sst_matches) > 0) sst_matches[1] else NA_character_

      if (!is.na(sst_lon_column) && !is.na(sst_lat_column) && !is.na(sst_column)) {
        sst_points <- sf::st_as_sf(
          sst_data,
          coords = c(sst_lon_column, sst_lat_column),
          crs = 4326,
          remove = FALSE
        )

        nearest_index <- sf::st_nearest_feature(point_data, sst_points)
        point_data$sst <- sst_points[[sst_column]][nearest_index]
      }
    }

    point_data$popup_text <- sprintf(
      "<strong>Survey Point ID:</strong> %s<br/><strong>SST:</strong> %s °C<br/><strong>Lon:</strong> %.4f<br/><strong>Lat:</strong> %.4f",
      point_data[[id_column]],
      ifelse(
        is.na(point_data$sst),
        "No data",
        format(round(point_data$sst, 2), nsmall = 2)
      ),
      point_data[[lon_column]],
      point_data[[lat_column]]
    )

    point_data
  })

  output$status <- renderText({
    result <- sst_result()
    if (is.null(result$data)) {
      return("No SST data available for the selected date or the previous week.")
    }

    if (!is.na(result$actual_date) && result$actual_date != result$requested_date) {
      return(
        sprintf(
          "Data for %s not available yet. Showing %s instead.",
          format(result$requested_date, "%Y-%m-%d"),
          format(result$actual_date, "%Y-%m-%d")
        )
      )
    }

    sprintf("Showing data for %s.", format(result$actual_date, "%Y-%m-%d"))
  })

  output$map <- renderLeaflet({
    point_data <- point_sst()
    pal <- colorNumeric(palette_base, domain = point_data$sst, na.color = "transparent")

    leaflet(point_data) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = bbox_center["lng"], lat = bbox_center["lat"], zoom = 6) |>
      addCircleMarkers(
        radius = 5,
        stroke = TRUE,
        weight = 0.6,
        color = "#2c3e50",
        fillColor = ~pal(sst),
        fillOpacity = 0.8,
        popup = ~popup_text
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~sst,
        title = "SST (°C)"
      )
  })
}

shinyApp(ui, server)
