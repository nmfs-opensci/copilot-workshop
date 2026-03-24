library(shiny)
library(leaflet)
library(rerddap)
library(surveyjoin)
library(sf)
library(dplyr)
library(viridis)

ERDDAP_DATASET_ID <- "ncdcOisst21Agg_LonPM180"
BBOX_BUFFER_DEGREES <- 0.5
MAX_LOOKBACK_DAYS <- 7

nwfsc_grid_raw <- surveyjoin::nwfsc_grid()
if (inherits(nwfsc_grid_raw, "sf")) {
  nwfsc_grid <- nwfsc_grid_raw
} else {
  nwfsc_grid <- sf::st_as_sf(nwfsc_grid_raw)
}

if (is.na(sf::st_crs(nwfsc_grid))) {
  sf::st_crs(nwfsc_grid) <- 4326
}

nwfsc_grid <- sf::st_transform(nwfsc_grid, 4326)

palette_base <- viridis(256)

id_candidates <- names(nwfsc_grid)[
  grepl("grid", names(nwfsc_grid), ignore.case = TRUE) |
    grepl("id$", names(nwfsc_grid), ignore.case = TRUE)
]
if (length(id_candidates) == 0) {
  nwfsc_grid$grid_id <- seq_len(nrow(nwfsc_grid))
  id_column <- "grid_id"
} else {
  id_column <- id_candidates[1]
}

bbox <- sf::st_bbox(nwfsc_grid)
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
  titlePanel("NWFSC Survey Grid Mean SST"),
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

  grid_sst <- reactive({
    result <- sst_result()

    if (is.null(result$data)) {
      grid_data <- nwfsc_grid
      grid_data$mean_sst <- NA_real_
      grid_data$popup_text <- sprintf(
        "<strong>Grid Cell ID:</strong> %s<br/><strong>Mean SST:</strong> No data",
        grid_data[[id_column]]
      )
      return(grid_data)
    }

    sst_data <- result$data
    names(sst_data) <- tolower(names(sst_data))

    lon_matches <- names(sst_data)[grepl("lon", names(sst_data))]
    lat_matches <- names(sst_data)[grepl("lat", names(sst_data))]
    sst_matches <- names(sst_data)[grepl("sst", names(sst_data))]

    lon_column <- if (length(lon_matches) > 0) lon_matches[1] else NA_character_
    lat_column <- if (length(lat_matches) > 0) lat_matches[1] else NA_character_
    sst_column <- if (length(sst_matches) > 0) sst_matches[1] else NA_character_

    if (is.na(lon_column) || is.na(lat_column) || is.na(sst_column)) {
      grid_data <- nwfsc_grid
      grid_data$mean_sst <- NA_real_
      grid_data$popup_text <- sprintf(
        "<strong>Grid Cell ID:</strong> %s<br/><strong>Mean SST:</strong> No data",
        grid_data[[id_column]]
      )
      return(grid_data)
    }

    sst_points <- sf::st_as_sf(
      sst_data,
      coords = c(lon_column, lat_column),
      crs = 4326,
      remove = FALSE
    )

    joined <- sf::st_join(sst_points, nwfsc_grid[, id_column], join = sf::st_within)

    if (!(id_column %in% names(joined))) {
      grid_data <- nwfsc_grid
      grid_data$mean_sst <- NA_real_
      grid_data$popup_text <- sprintf(
        "<strong>Grid Cell ID:</strong> %s<br/><strong>Mean SST:</strong> No data",
        grid_data[[id_column]]
      )
      return(grid_data)
    }

    mean_sst <- joined |>
      sf::st_drop_geometry() |>
      group_by(.data[[id_column]]) |>
      summarize(mean_sst = mean(.data[[sst_column]], na.rm = TRUE), .groups = "drop")

    grid_data <- nwfsc_grid |>
      left_join(mean_sst, by = id_column)

    grid_data$popup_text <- sprintf(
      "<strong>Grid Cell ID:</strong> %s<br/><strong>Mean SST:</strong> %s °C",
      grid_data[[id_column]],
      ifelse(
        is.na(grid_data$mean_sst),
        "No data",
        format(round(grid_data$mean_sst, 2), nsmall = 2)
      )
    )

    grid_data
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
    grid_data <- grid_sst()
    pal <- colorNumeric(palette_base, domain = grid_data$mean_sst, na.color = "transparent")

    leaflet(grid_data) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = bbox_center["lng"], lat = bbox_center["lat"], zoom = 6) |>
      addPolygons(
        fillColor = ~pal(mean_sst),
        fillOpacity = 0.7,
        color = "#2c3e50",
        weight = 0.4,
        popup = ~popup_text
      ) |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = ~mean_sst,
        title = "Mean SST (°C)"
      )
  })
}

shinyApp(ui, server)
