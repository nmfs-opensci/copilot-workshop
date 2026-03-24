library(shiny)
library(leaflet)
library(rerddap)
library(surveyjoin)
library(viridis)

erddap_url <- "https://coastwatch.pfeg.noaa.gov/erddap/"
dataset_id <- "ncdcOisst21Agg_LonPM180"
CACHE_DIR <- file.path(path.expand("~"), ".cache")
CACHE_MAX_AGE_HOURS <- 24
NA_MARKER_COLOR <- "#000000"
VIRIDIS_PALETTE_SIZE <- 256

grid_points <- surveyjoin::nwfsc_grid
grid_points <- grid_points[grid_points$survey == "NWFSC.Combo", , drop = FALSE]
grid_points$grid_id <- seq_len(nrow(grid_points))

get_available_dates <- function() {
  info_list <- rerddap::info(dataset_id, url = erddap_url)
  time_meta <- info_list$alldata$time
  actual_range <- time_meta$value[time_meta$attribute_name == "actual_range"]
  units <- time_meta$value[time_meta$attribute_name == "units"]
  dimension <- time_meta$value[time_meta$row_type == "dimension"]

  if (length(actual_range) == 0 || length(units) == 0) {
    stop("Unable to read time metadata from ERDDAP.")
  }

  range_values <- as.numeric(strsplit(actual_range, ",")[[1]])
  origin <- sub(".*since ", "", units)
  time_range <- as.POSIXct(range_values, origin = origin, tz = "UTC")

  n_values <- suppressWarnings(as.integer(sub(".*nValues=([0-9]+).*", "\\1", dimension)))
  if (is.na(n_values) || n_values < 2) {
    time_values <- seq(time_range[1], time_range[2], by = "1 day")
  } else {
    time_values <- seq(time_range[1], time_range[2], length.out = n_values)
  }

  unique(as.Date(time_values))
}

cache_path <- file.path(CACHE_DIR, "nwfsc_sst_dates.rds")
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
cache_valid <- function(path, max_age_hours = CACHE_MAX_AGE_HOURS) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  file_info <- file.info(path)
  if (is.na(file_info$mtime)) {
    return(FALSE)
  }
  age_hours <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "hours"))
  age_hours <= max_age_hours
}
available_dates <- tryCatch(
  if (cache_valid(cache_path)) {
    readRDS(cache_path)
  } else {
    dates <- get_available_dates()
    saveRDS(dates, cache_path)
    dates
  },
  error = function(e) {
    message(
      "Failed to fetch ERDDAP metadata. Check internet connectivity and ERDDAP availability: ",
      conditionMessage(e)
    )
    seq(Sys.Date() - 30, Sys.Date(), by = "1 day")
  }
)

ui <- fluidPage(
  titlePanel("NWFSC Survey Grid Mean SST"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "date",
        "Select date",
        choices = format(available_dates, "%Y-%m-%d"),
        selected = format(max(available_dates), "%Y-%m-%d")
      ),
      textOutput("status")
    ),
    mainPanel(
      leafletOutput("map", height = 650)
    )
  )
)

nearest_indices <- function(grid, target) {
  if (length(grid) == 1) {
    return(rep(1L, length(target)))
  }
  idx <- findInterval(target, grid, all.inside = TRUE)
  left <- grid[idx]
  right <- grid[idx + 1L]
  idx + (abs(target - right) < abs(target - left))
}

server <- function(input, output, session) {
  error_message <- reactiveVal(NULL)

  sst_points <- eventReactive(input$date, {
    error_message(NULL)
    req(input$date)

    tryCatch({
      selected_date <- as.Date(input$date)
      lat_range <- range(grid_points$lat, na.rm = TRUE)
      lon_range <- range(grid_points$lon, na.rm = TRUE)

      sst_result <- rerddap::griddap(
        dataset_id,
        url = erddap_url,
        time = c(
          paste0(format(selected_date, "%Y-%m-%d"), "T00:00:00Z"),
          paste0(format(selected_date, "%Y-%m-%d"), "T23:59:59Z")
        ),
        latitude = lat_range,
        longitude = lon_range,
        fields = "sst"
      )

      sst_data <- sst_result$data
      sst_data <- sst_data[!is.na(sst_data$sst), , drop = FALSE]
      if (nrow(sst_data) == 0) {
        stop("No SST data returned for the selected date.")
      }

      sst_matrix <- xtabs(sst ~ latitude + longitude, data = sst_data)
      lat_values <- as.numeric(rownames(sst_matrix))
      lon_values <- as.numeric(colnames(sst_matrix))

      lat_index <- nearest_indices(lat_values, grid_points$lat)
      lon_index <- nearest_indices(lon_values, grid_points$lon)

      output_points <- grid_points
      output_points$sst <- sst_matrix[cbind(lat_index, lon_index)]
      output_points
    }, error = function(e) {
      error_message(conditionMessage(e))
      NULL
    })
  })

  output$status <- renderText({
    if (!is.null(error_message())) {
      paste("Error:", error_message())
    } else if (!is.null(input$date)) {
      paste("Showing SST for", input$date)
    } else {
      ""
    }
  })

  output$map <- renderLeaflet({
    data <- sst_points()
    req(data)
    validate(need(!all(is.na(data$sst)), "No SST values available for the selected date."))

    pal <- colorNumeric(
      viridis(VIRIDIS_PALETTE_SIZE),
      domain = data$sst,
      na.color = NA_MARKER_COLOR
    )

    leaflet(data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -124.5, lat = 45.5, zoom = 6) %>%
      addCircleMarkers(
        ~lon,
        ~lat,
        radius = 4,
        stroke = FALSE,
        fillOpacity = 0.8,
        color = ~pal(sst),
        popup = ~paste0(
          "Grid ID: ", grid_id,
          "<br>Mean SST: ", sprintf("%.2f", sst), " \u00b0C"
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~sst,
        title = "Mean SST (\u00b0C)",
        opacity = 1
      )
  })
}

shinyApp(ui, server)
