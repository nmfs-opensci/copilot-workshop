library(shiny)
library(leaflet)
library(rerddap)
library(surveyjoin)
library(viridis)
library(dplyr)

# ── Static data loaded once at startup ──────────────────────────────────────

# Load NWFSC.Combo grid points
grid_pts <- surveyjoin::nwfsc_grid |>
  dplyr::filter(survey == "NWFSC.Combo") |>
  dplyr::mutate(cell_id = dplyr::row_number()) |>
  dplyr::select(cell_id, lon, lat)

# Fetch available date range from ERDDAP metadata
erddap_url  <- "https://coastwatch.pfeg.noaa.gov/erddap/"
dataset_id  <- "ncdcOisst21Agg_LonPM180"

get_date_range <- function() {
  tryCatch({
    meta <- rerddap::info(dataset_id, url = erddap_url)
    time_row <- meta$alldata$time[
      meta$alldata$time$attribute_name == "actual_range", ]
    if (nrow(time_row) == 0) stop("No time range found")
    # actual_range is two doubles (seconds since 1970-01-01)
    vals <- as.numeric(strsplit(time_row$value, ",\\s*")[[1]])
    list(
      min = as.Date(as.POSIXct(vals[1], origin = "1970-01-01", tz = "UTC")),
      max = as.Date(as.POSIXct(vals[2], origin = "1970-01-01", tz = "UTC"))
    )
  }, error = function(e) {
    list(min = as.Date("1981-09-01"), max = Sys.Date() - 2)
  })
}

date_range <- get_date_range()

# ── UI ───────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  titlePanel("NWFSC Survey Grid - Sea Surface Temperature"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      dateInput(
        inputId = "selected_date",
        label   = "Select Date",
        value   = date_range$max - 1,
        min     = date_range$min,
        max     = date_range$max,
        format  = "yyyy-mm-dd"
      ),
      hr(),
      helpText("Click a grid point on the map to see its SST details."),
      hr(),
      verbatimTextOutput("click_info")
    ),

    mainPanel(
      width = 9,
      leafletOutput("map", height = "600px")
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Fetch SST from ERDDAP – only re-runs when the date changes
  sst_data <- eventReactive(input$selected_date, {
    date_str <- format(input$selected_date, "%Y-%m-%d")

    # Bounding box around the nwfsc_grid extent
    lon_min <- floor(min(grid_pts$lon))   - 0.5
    lon_max <- ceiling(max(grid_pts$lon)) + 0.5
    lat_min <- floor(min(grid_pts$lat))   - 0.5
    lat_max <- ceiling(max(grid_pts$lat)) + 0.5

    withProgress(message = "Fetching SST data…", value = 0.5, {
      raw <- tryCatch(
        rerddap::griddap(
          datasetx  = dataset_id,
          url       = erddap_url,
          time      = c(date_str, date_str),
          longitude = c(lon_min, lon_max),
          latitude  = c(lat_min, lat_max),
          fields    = "sst",
          fmt       = "csv"
        ),
        error = function(e) NULL
      )
    })

    if (is.null(raw)) return(NULL)

    sst_df <- raw$data |>
      dplyr::rename(lon = longitude, lat = latitude) |>
      dplyr::filter(!is.na(sst)) |>
      dplyr::select(lon, lat, sst)

    sst_df
  })

  # Spatially match each grid point to the nearest SST observation
  joined_data <- reactive({
    sst_df <- sst_data()
    if (is.null(sst_df) || nrow(sst_df) == 0) return(NULL)

    # Round to OISST grid resolution (0.25°) for fast join
    res <- 0.25
    sst_df <- sst_df |>
      dplyr::mutate(
        lon_r = round(lon / res) * res,
        lat_r = round(lat / res) * res
      )

    grid_pts |>
      dplyr::mutate(
        lon_r = round(lon / res) * res,
        lat_r = round(lat / res) * res
      ) |>
      dplyr::left_join(
        sst_df |> dplyr::select(lon_r, lat_r, sst),
        by = c("lon_r", "lat_r")
      ) |>
      dplyr::select(cell_id, lon, lat, sst) |>
      dplyr::filter(!is.na(sst))
  })

  # ── Base map (rendered once) ──────────────────────────────────────────────
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -124.5, lat = 46.0, zoom = 5)
  })

  # ── Update coloured points whenever joined_data changes ──────────────────
  observe({
    df <- joined_data()

    proxy <- leafletProxy("map")
    proxy |> clearGroup("sst_points") |> removeControl("legend")

    if (is.null(df) || nrow(df) == 0) return()

    pal <- colorNumeric(
      palette = viridis::viridis(256),
      domain  = df$sst,
      na.color = "transparent"
    )

    proxy |>
      addCircleMarkers(
        data        = df,
        lng         = ~lon,
        lat         = ~lat,
        radius      = 4,
        color       = ~pal(sst),
        fillColor   = ~pal(sst),
        fillOpacity = 0.85,
        stroke      = FALSE,
        group       = "sst_points",
        layerId     = ~cell_id,
        label       = ~paste0("Cell ", cell_id, " | SST: ", round(sst, 2), " °C")
      ) |>
      addLegend(
        position = "bottomright",
        pal      = pal,
        values   = df$sst,
        title    = "SST (°C)",
        layerId  = "legend"
      )
  })

  # ── Click popup ──────────────────────────────────────────────────────────
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    df    <- joined_data()
    if (is.null(df) || is.null(click$id)) return()

    row <- df[df$cell_id == as.integer(click$id), ]
    if (nrow(row) == 0) return()

    mean_sst <- round(row$sst, 2)

    output$click_info <- renderText({
      paste0(
        "Grid Cell ID : ", row$cell_id, "\n",
        "Longitude    : ", round(row$lon, 4), "\n",
        "Latitude     : ", round(row$lat, 4), "\n",
        "SST          : ", mean_sst, " °C\n",
        "Date         : ", format(input$selected_date, "%Y-%m-%d")
      )
    })

    leafletProxy("map") |>
      addPopups(
        lng     = click$lng,
        lat     = click$lat,
        popup   = paste0(
          "<b>Cell ID:</b> ", row$cell_id, "<br>",
          "<b>SST:</b> ", mean_sst, " °C<br>",
          "<b>Date:</b> ", format(input$selected_date, "%Y-%m-%d")
        )
      )
  })
}

# ── Launch ───────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
