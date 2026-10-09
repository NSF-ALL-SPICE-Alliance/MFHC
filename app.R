# ==============================================================================
# Maunalua Fishpond Heritage Center dashboard: Kānewai & Kalauhaʻihaʻi
# ==============================================================================
# Files this app needs next to app.R:
#   fishpond.duckdb               (table: sensor_data)
#   DJI_0321.JPG, DJI_0353.JPG    (drone images)
#   www/                          (logos and photos)
# Secrets: OPENAI_API_KEY (set in .Renviron locally and in Connect Cloud).
#
# Time convention: date_time_hst holds Hawaii wall-clock time stored without a
# time zone. Everything below treats it as a plain timestamp (R labels it UTC),
# so the app behaves the same on a laptop in Hawaii and on a server in UTC.
# ==============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(here)
library(magick)
library(plotly)
library(shinyWidgets)
library(shinycssloaders)
library(DBI)
library(duckdb)
library(ellmer)
library(shinychat)
library(mapgl)
library(later)
library(lubridate)
library(readr)

# --- Settings -----------------------------------------------------------------
openai_model   <- "gpt-4o"
hourly_plots   <- TRUE   # TRUE: time-series charts use hourly means (fast).
# FALSE: plot every 10-minute reading (slow).
chat_max_rows  <- 200    # cap on rows sent back to the model per query

# --- Database -----------------------------------------------------------------
conn <- dbConnect(duckdb(), dbdir = here("fishpond.duckdb"), read_only = TRUE)
onStop(\() dbDisconnect(conn, shutdown = TRUE))

date_range <- dbGetQuery(conn, "
  SELECT CAST(MIN(date_time_hst) AS DATE) AS min_date,
         CAST(MAX(date_time_hst) AS DATE) AS max_date
  FROM sensor_data
")

# Time series for the line charts, loaded once at startup.
time_expr <- if (hourly_plots) "date_trunc('hour', date_time_hst)" else "date_time_hst"
series <- dbGetQuery(conn, sprintf("
  SELECT site, site_specific, variable, %s AS ts, AVG(value) AS value
  FROM sensor_data
  GROUP BY ALL
  ORDER BY 1, 2, 3, 4
", time_expr)) |>
  rename(date_time_hst = ts)

# Temperature at every sensor of one site at one moment (for the pond map).
snapshot <- function(site, ts_string) {
  dbGetQuery(conn, "
    SELECT site_specific, AVG(value) AS value
    FROM sensor_data
    WHERE site = ?
      AND variable = 'temperature'
      AND date_time_hst = CAST(? AS TIMESTAMP)
    GROUP BY site_specific
  ", params = list(site, ts_string))
}

# Load tide data
tide_data <- readr::read_csv(
  here("cleaned_data/honolulu_tide_10min.csv"),
  show_col_types = FALSE
) |>
  mutate(
    date_time_hst = force_tz(as.POSIXct(date_time_hst), tzone = "Pacific/Honolulu")
  )

# --- Site configuration -------------------------------------------------------
# Everything that differs between the two sites lives here.
sites <- list(
  kanewai = list(
    db_name      = "Kanewai",
    label        = "Kānewai",
    raster       = as.raster(image_read("DJI_0321.JPG")),
    xmax         = 10,
    ymax         = 10,
    label_offset = 0.45,
    map_height   = "600px",
    sensors = data.frame(
      site_specific = c("Norfolk", "Auwai", "Rock", "Springledge"),
      x = c(1, 8.5, 9, 6),
      y = c(7.5, 3.45, 2, 2.5)
    ),
    series_exclude = character(0),
    colors = c(
      Norfolk = "#1E3A8A", Shade = "#B91C1C", Auwai = "#065F46",
      RockWall = "#6B21A8", Rock = "#EA580C", Springledge = "#7C2D12"
    ),
    temp_ylim_c = c(20, 32.5),
    ph_ylim     = c(6.5, 11),
    oxygen_ylim = c(0, 10)
  ),
  kalauhaihai = list(
    db_name      = "Kalauhaihai",
    label        = "Kalauhaʻihaʻi",
    raster       = as.raster(image_read("DJI_0353.JPG")),
    xmax         = 5472,
    ymax         = 3648,
    label_offset = 190,
    map_height   = "700px",
    sensors = data.frame(
      site_specific = c("Garage", "Makaha"),
      x = c(900, 2750),
      y = c(600, 1900)
    ),
    series_exclude = c("Auwai", "Coconut"),
    colors = c(Garage = "#1E3A8A", Makaha = "#B91C1C"),
    temp_ylim_c = NULL,
    ph_ylim     = NULL,
    oxygen_ylim = NULL
  )
)

# --- Helpers ------------------------------------------------------------------
c_to_f <- function(x) x * 9 / 5 + 32

time_choices <- sprintf("%02d:%02d", rep(0:23, each = 6), rep(seq(0, 50, 10), 24))

finish_plotly <- function(p) {
  p |>
    plotly::layout(hovermode = "x unified") |>
    plotly::config(
      displaylogo = FALSE,
      modeBarButtonsToAdd = c("hovercompare", "toggleSpikelines")
    )
}

# Line chart with labelled horizontal reference lines (used for pH and oxygen).
# `lines` is a list of list(y, label, colour, linetype).
threshold_plot <- function(df, title, ylab, colour, ylim = NULL, lines = list()) {
  p <- ggplot(df, aes(x = date_time_hst, y = value)) +
    geom_line(linewidth = 0.4, colour = colour) +
    labs(title = title, x = "Date and Time", y = ylab) +
    theme_minimal()
  
  for (l in lines) {
    p <- p + geom_hline(yintercept = l$y, linetype = l$linetype, colour = l$colour)
  }
  if (!is.null(ylim)) p <- p + coord_cartesian(ylim = ylim)
  
  annotations <- lapply(lines, function(l) {
    list(
      x = 0.01, xref = "paper", xanchor = "left",
      y = l$y, yref = "y", yanchor = "top",
      text = l$label, showarrow = FALSE,
      font = list(size = 12, color = "#333333")
    )
  })
  
  finish_plotly(ggplotly(p)) |> plotly::layout(annotations = annotations)
}

ph_lines <- list(
  list(y = 7, label = "pH level of freshwater", colour = "blue", linetype = "dashed")
)
oxygen_lines <- list(
  list(y = 5, label = "above 5 mg/L is generally considered healthy",
       colour = "darkgreen", linetype = "dashed"),
  list(y = 2, label = "below 2 mg/L is considered hypoxic (NOAA)",
       colour = "darkred", linetype = "solid")
)

# --- Plot help panel ----------------------------------------------------------
plotHelpUI <- function(title = "How to explore these charts") {
  tags$details(
    class = "plot-help",
    open = NA,
    tags$summary(HTML(paste0("💡 ", title))),
    HTML('
      <ul class="plot-help-list">
        <li><strong>Hover</strong> to see exact values, time, and sensor/site.</li>
        <li><strong>Zoom</strong>: click-and-drag a box over dates to zoom in.
            <em>Double-click</em> anywhere to reset.</li>
        <li><strong>Pan</strong>: after zooming, drag on the plot (or use the toolbar buttons).</li>
        <li><strong>Straight lines</strong> indicate time between data collection 📆.</li>
        <li><strong>Compare</strong>: turn on <em>Compare data on hover</em> in the toolbar to see all series at once.</li>
        <li><strong>Spike lines</strong>: toggle <em>Spikes</em> for vertical guide lines under the cursor.</li>
        <li><strong>Download</strong>: use the 📷 icon to save the figure as PNG.</li>
      </ul>
      <div class="plot-help-note">
        Tip: You can also drag along just the <em>x-axis</em> labels to zoom the time window precisely.
      </div>
    ')
  )
}

app_styles <- tags$style(HTML("
  .plot-help {
    background:#f6faf9; border:1px solid #d7ece7; border-radius:10px;
    padding:12px 16px; margin:10px 0 16px 0;
  }
  .plot-help > summary { cursor:pointer; font-weight:700; color:#0b5d73; margin-bottom:8px; }
  .plot-help-list { margin:8px 0 0 0; padding-left:18px; line-height:1.4; }
  .plot-help-list li { margin:6px 0; }
  .plot-help-note { margin-top:10px; font-size:0.92em; color:#356e7a; }
  .map-hint { color:#356e7a; margin:6px 0 10px 0; }
"))

# --- Site module --------------------------------------------------------------
siteUI <- function(id, cfg) {
  ns <- NS(id)
  nav_panel(
    cfg$label,
    layout_columns(
      col_widths = c(4, 5, 3),
      dateInput(
        ns("date"), "Date",
        value = as.Date("2022-07-10"),
        min = date_range$min_date, max = date_range$max_date
      ),
      sliderTextInput(
        ns("time"), "Time of day (HST)",
        choices = time_choices, selected = "08:00", grid = FALSE,
        width = "100%"
      ),
      input_switch(ns("fahrenheit"), "Show °F", value = FALSE)
    ),
    div(
      class = "map-hint",
      "👆 Click a sensor to see its temperature over time 📈. ",
      "⭐ indicates the pH and oxygen sensor."
    ),
    # ---- Map (left) + same-day graphs (right) ----
    layout_columns(
      col_widths = c(7, 5),
      withSpinner(plotOutput(ns("map"), click = ns("click"), height = cfg$map_height)),
      div(
        h5("Selected day – sensor values"),
        withSpinner(plotlyOutput(ns("day_temp"), height = "220px")),
        withSpinner(plotlyOutput(ns("day_ph"), height = "180px")),
        withSpinner(plotlyOutput(ns("day_oxygen"), height = "180px")),
        withSpinner(plotlyOutput(ns("day_tide"), height = "160px"))
      )
    ),
    plotHelpUI(paste("Explore", cfg$label, "Temperature, pH, Oxygen, and Tide")),
    uiOutput(ns("temp_ui")),
    plotlyOutput(ns("ph")),
    plotlyOutput(ns("oxygen")),
    plotlyOutput(ns("tide"), height = "400px")   # full tide graph
  )
}

siteServer <- function(id, cfg) {
  moduleServer(id, function(input, output, session) {
    site_series <- series |> filter(site == cfg$db_name)
    
    temp_unit <- reactive(if (isTRUE(input$fahrenheit)) "°F" else "°C")
    convert   <- function(x) if (isTRUE(input$fahrenheit)) c_to_f(x) else x
    
    # Data for the selected calendar day only
    day_data <- reactive({
      req(input$date)
      d <- as.Date(input$date)
      site_series |>
        filter(
          as.Date(date_time_hst) == d
        )
    })
    
    # Same day temp plot
    output$day_temp <- renderPlotly({
      df <- day_data() |>
        filter(
          variable == "temperature",
          site_specific != "General",
          !site_specific %in% cfg$series_exclude
        ) |>
        mutate(value = convert(value))
      
      if (nrow(df) == 0) {
        return(plotly_empty() |> layout(title = "No temperature data this day"))
      }
      
      p <- ggplot(df, aes(x = date_time_hst, y = value, colour = site_specific)) +
        geom_line(linewidth = 0.5) +
        scale_colour_manual(values = cfg$colors, guide = "none") +
        labs(title = "Temperature (selected day)", x = NULL, y = temp_unit()) +
        theme_minimal(base_size = 11)
      
      finish_plotly(ggplotly(p))
    })
    
    # Same day pH plot
    output$day_ph <- renderPlotly({
      df <- day_data() |> filter(variable == "pH")
      
      if (nrow(df) == 0) {
        return(plotly_empty() |> layout(title = "No pH data this day"))
      }
      
      p <- ggplot(df, aes(x = date_time_hst, y = value)) +
        geom_line(colour = "tomato4", linewidth = 0.5) +
        geom_hline(yintercept = 7, linetype = "dashed", colour = "blue") +
        labs(title = "pH (selected day)", x = NULL, y = "pH") +
        theme_minimal(base_size = 11)
      
      finish_plotly(ggplotly(p))
    })
    
    # Same day oxygen plot
    output$day_oxygen <- renderPlotly({
      df <- day_data() |> filter(variable == "oxygen")
      
      if (nrow(df) == 0) {
        return(plotly_empty() |> layout(title = "No oxygen data this day"))
      }
      
      p <- ggplot(df, aes(x = date_time_hst, y = value)) +
        geom_line(colour = "#0B5D73", linewidth = 0.5) +
        geom_hline(yintercept = 5, linetype = "dashed", colour = "darkgreen") +
        geom_hline(yintercept = 2, colour = "darkred") +
        labs(title = "Oxygen (selected day)", x = NULL, y = "mg/L") +
        theme_minimal(base_size = 11)
      
      finish_plotly(ggplotly(p))
    })
    
    # Same day tide plot
    output$day_tide <- renderPlotly({
      req(input$date)
      d <- as.Date(input$date)
      
      df <- tide_data |>
        filter(as.Date(date_time_hst) == d)
      
      if (nrow(df) == 0) {
        return(plotly_empty() |> layout(title = "No tide data this day"))
      }
      
      p <- ggplot(df, aes(x = date_time_hst, y = water_level_m)) +
        geom_line(colour = "#1B4F72", linewidth = 0.5) +
        labs(
          title = "Tide (selected day)",
          x = NULL,
          y = "m (MLLW)"
        ) +
        theme_minimal(base_size = 11)
      
      finish_plotly(ggplotly(p))
    })
    
    output$tide <- renderPlotly({
      # Match the overall date range of this site’s sensor data
      sensor_range <- range(site_series$date_time_hst, na.rm = TRUE)
      
      df <- tide_data |>
        filter(
          date_time_hst >= sensor_range[1],
          date_time_hst <= sensor_range[2]
        )
      
      p <- ggplot(df, aes(x = date_time_hst, y = water_level_m)) +
        geom_line(colour = "#1B4F72", linewidth = 0.35, alpha = 0.85) +
        labs(
          title = "Honolulu Tide (Station 1612340) – Water Level",
          subtitle = "Relative to MLLW • 10-minute intervals",
          x = "Date and Time (HST)",
          y = "Water Level (m)"
        ) +
        theme_minimal()
      
      finish_plotly(ggplotly(p))
    })
    
    # Selected moment as a plain "YYYY-MM-DD HH:MM:SS" string (HST wall clock)
    selected_ts <- reactive({
      req(input$date, input$time)
      paste0(format(input$date, "%Y-%m-%d"), " ", input$time, ":00")
    })
    
    
    # --- Pond map ---
    map_data <- reactive({
      snapshot(cfg$db_name, selected_ts()) |>
        right_join(cfg$sensors, by = "site_specific") |>
        mutate(value = convert(value))
    })
    
    output$map <- renderPlot({
      d <- map_data()
      subtitle <- if (all(is.na(d$value))) {
        paste("No temperature readings at", selected_ts(), "(try another date or time)")
      } else {
        paste("Water temperature at", selected_ts(), "HST")
      }
      
      ggplot(d, aes(x = x, y = y)) +
        annotation_raster(cfg$raster, xmin = 0, xmax = cfg$xmax, ymin = 0, ymax = cfg$ymax) +
        geom_point(
          aes(fill = value),
          shape = 21, size = 12, colour = "black", alpha = 0.8
        ) +
        geom_text(
          aes(y = y + cfg$label_offset, label = site_specific),
          colour = "white", size = 4.5, fontface = "bold"
        ) +
        scale_fill_viridis_c(
          name = paste0("Temperature (", temp_unit(), ")"),
          option = "C", na.value = "grey70"
        ) +
        coord_fixed(xlim = c(0, cfg$xmax), ylim = c(0, cfg$ymax), expand = FALSE) +
        labs(subtitle = subtitle) +
        theme_void() +
        theme(legend.position = "right", plot.subtitle = element_text(size = 13))
    }, res = 96)
    
    # --- Clicked sensor -> temperature time series ---
    clicked <- reactiveVal(NULL)
    observeEvent(input$click, {
      hit <- nearPoints(cfg$sensors, input$click, xvar = "x", yvar = "y",
                        threshold = 25, maxpoints = 1)
      if (nrow(hit) > 0) clicked(hit$site_specific)
    })
    
    output$temp_ui <- renderUI({
      if (is.null(clicked())) return(NULL)
      withSpinner(plotlyOutput(session$ns("temp_plot")))
    })
    
    output$temp_plot <- renderPlotly({
      sensor <- req(clicked())
      df <- site_series |>
        filter(variable == "temperature",
               site_specific != "General",
               !site_specific %in% cfg$series_exclude) |>
        mutate(value = convert(value))
      
      line_colour <- if (sensor %in% names(cfg$colors)) cfg$colors[[sensor]] else "#0B5D73"
      
      p <- ggplot(mapping = aes(x = date_time_hst, y = value)) +
        geom_line(
          data = df |> filter(site_specific != sensor),
          aes(group = site_specific),
          colour = "grey60", linewidth = 0.3, alpha = 0.4
        ) +
        geom_line(
          data = df |> filter(site_specific == sensor),
          colour = line_colour, linewidth = 0.6, alpha = 0.8
        ) +
        labs(
          title = paste(cfg$label, "Temperature:", sensor),
          x = "Date and Time",
          y = paste0("Temperature (", temp_unit(), ")")
        ) +
        theme_minimal()
      
      if (!is.null(cfg$temp_ylim_c)) {
        p <- p + coord_cartesian(ylim = convert(cfg$temp_ylim_c))
      }
      finish_plotly(ggplotly(p))
    })
    
    # --- pH and oxygen ---
    output$ph <- renderPlotly({
      threshold_plot(
        site_series |> filter(variable == "pH"),
        title = paste(cfg$label, "pH"), ylab = "pH Level",
        colour = "tomato4", ylim = cfg$ph_ylim, lines = ph_lines
      )
    })
    
    output$oxygen <- renderPlotly({
      threshold_plot(
        site_series |> filter(variable == "oxygen"),
        title = paste(cfg$label, "Oxygen"), ylab = "Oxygen (mg/L)",
        colour = "#0B5D73", ylim = cfg$oxygen_ylim, lines = oxygen_lines
      )
    })
  })
}

# --- Chatbot text -------------------------------------------------------------
system_prompt_str <- paste0(
  "You're an AI assistant analyzing fishpond sensor data stored in 'fishpond.duckdb'. ",
  "The 'sensor_data' table contains environmental measurements from two Hawaiian fishpond sites.\n\n",
  
  "**DATA STRUCTURE:**\n",
  "- **site**: Two values only: 'Kanewai' or 'Kalauhaihai' (always capitalize first letter in queries)\n",
  "- **site_specific**: Location within each site, or 'General' for pH/oxygen\n",
  "- **variable**: Three values only: 'temperature', 'pH', or 'oxygen'\n",
  "- **date_time_hst**: Timestamp in Hawaii Standard Time\n",
  "- **value**: Numeric sensor reading\n\n",
  
  "**IMPORTANT RULES:**\n",
  "1. **Site names**: Always query as 'Kanewai' or 'Kalauhaihai' (capital first letter, ignore diacritical marks like ʻ). ",
  "Users may type variations like 'kanewai', 'Kānewai', 'kane wai' - treat all as 'Kanewai'. ",
  "Same for 'Kalauhaihai' variations like 'kalauhaihaʻi', 'kalauha ihai', etc.\n",
  
  "2. **Variable names**: Translate user input flexibly:\n",
  "   - 'temp', 'temperature', 'water temp' → 'temperature'\n",
  "   - 'pH', 'ph', 'acidity' → 'pH'\n",
  "   - 'oxygen', 'DO', 'dissolved oxygen', 'o2' → 'oxygen'\n\n",
  
  "3. **Site-specific locations**:\n",
  "   - Temperature available at: Kanewai (Auwai, Shade, Norfolk, Springledge, Rock) and ",
  "Kalauhaihai (Auwai, Makaha, Coconut, Garage)\n",
  "   - pH and oxygen use site_specific = 'General' only\n",
  "   - Ignore capitalization: 'norfolk', 'Norfolk', 'NORFOLK' → 'Norfolk'\n\n",
  
  "4. **SQL Guidelines**:\n",
  "   - Use DuckDB-compatible SQL only; SELECT statements only\n",
  "   - Filter site using: WHERE site = 'Kanewai' (exact match after normalization)\n",
  "   - For date ranges use a half-open interval so the last day is included: ",
  "date_time_hst >= '2023-08-01' AND date_time_hst < '2023-09-01'\n",
  "   - Readings are every 10 minutes, so raw results are huge. The query tool returns at most ",
  chat_max_rows, " rows. Always aggregate (AVG, MIN, MAX, COUNT, date_trunc by day/month) ",
  "rather than selecting raw rows.\n\n",
  
  "5. **Response format**:\n",
  "   - Provide clear, concise answers with units (°C for temp, mg/L for oxygen)\n",
  "   - Show the SQL query used in a code block\n",
  "   - If a query returns no data, explain why (wrong site_specific for variable, date out of range, etc.)\n",
  "   - For ambiguous questions, ask for clarification\n\n",
  
  "**EXAMPLE QUERIES:**\n",
  "User: 'What was the average temp at kanewai norfolk in August 2023?'\n",
  "→ SELECT AVG(value) FROM sensor_data WHERE site = 'Kanewai' AND site_specific = 'Norfolk' ",
  "AND variable = 'temperature' AND date_time_hst >= '2023-08-01' AND date_time_hst < '2023-09-01'\n\n",
  
  "User: 'Show me oxygen levels at Kalauhaihai'\n",
  "→ SELECT date_trunc('month', date_time_hst) AS month, AVG(value) AS avg_oxygen, MIN(value) AS min_oxygen, ",
  "MAX(value) AS max_oxygen FROM sensor_data WHERE site = 'Kalauhaihai' AND site_specific = 'General' ",
  "AND variable = 'oxygen' GROUP BY 1 ORDER BY 1\n\n",
  
  "User: 'Were there hypoxic conditions at kanewai?'\n",
  "→ SELECT COUNT(*) FROM sensor_data WHERE site = 'Kanewai' AND site_specific = 'General' ",
  "AND variable = 'oxygen' AND value < 2.0\n\n",
  
  "Always answer questions using the actual data from the database. Be helpful and accurate!"
)

greeting <- paste0(
  "⚡ **AI Data Assistant** 🌐\n\n",
  "I'm your intelligent interface for exploring sensor data from **Kānewai Spring** and ",
  "**Kalauhaʻihaʻi Fishpond**. Ask me anything and I will translate your question into code ",
  "to generate the best answer possible.\n\n",
  "**Example questions:**\n\n",
  "📊 *\"What was the average water temperature at kanewai norfolk in August 2023?\"*\n\n",
  "📉 *\"When did Kanewai Norfolk record its highest temperature?\"*\n\n",
  "🔄 *\"What percentage of time were the conditions at the kalauhaihai site hypoxic?\"*\n\n",
  "💡 *\"What's the average temperature across all kanewai sites?\"*\n\n",
  "Just type naturally below. 🚀"
)

# Tool the model calls to read the database.
run_query <- function(query) {
  if (!grepl("^\\s*(select|with)\\b", query, ignore.case = TRUE, perl = TRUE)) {
    stop("Only SELECT statements are allowed.")
  }
  df <- dbGetQuery(conn, query)
  jsonlite::toJSON(
    list(
      total_rows = nrow(df),
      truncated  = nrow(df) > chat_max_rows,
      data       = head(df, chat_max_rows)
    ),
    auto_unbox = TRUE, dataframe = "rows"
  )
}

# --- Overview text ------------------------------------------------------------
overview_html <- HTML('
  <div style="padding-top: 30px; color: #333;">
    <h3><strong>🌺 Project Overview</strong></h3>
    <p>
      Aloha, Welcome to our pilot data dashboard!<br><br>
      This project is a collaboration with
      <a href="https://maunaluafishpond.org/" target="_blank">Maunalua Fishpond Heritage Center</a>.
      Since 2021, Chaminade students in Professor
      <a href="https://chaminade.edu/nsm/nsm-faculty/lupita-ruiz-jones/" target="_blank">Lupita Ruiz-Jones</a>’s
      environmental science classes have explored Kalauhaʻihaʻi and Kānewai to study the impact of the vital flow of water mauka to makai.
      This research focuses on tracking water temperature, oxygen levels, salinity, pH, and water level changes.
    </p>
    <p>
      At Kalauhaʻihaʻi, data collected before and after lava tube restoration helps assess the impact of increased freshwater input and connectivity to the ocean.
      Comparing this with Kānewai offers insights into how continuous freshwater flow shapes these ecosystems.
    </p>
    <p>
      This pilot dashboard features graphs of temperature, pH, and dissolved oxygen for both sites.
      We’ll keep updating it with data from 2021 to the present, adding the other variables we have been measuring—and we welcome your feedback to help us improve!
    </p>

    <h4><strong>Participants</strong></h4>
    <ul>
      <li><a href="https://lupita-ruiz-jones.squarespace.com/" target="_blank">Lupita Ruiz-Jones PhD</a>, Associate Professor of Environmental Science at Chaminade University</li>
      <li><a href="https://connorflynn.github.io/" target="_blank">Connor Flynn MS</a>, Data Scientist</li>
      <li>Hina Ioane, Environmental Science recent graduate</li>
      <li>Kate Dugger, Environmental Studies recent graduate</li>
      <li>Samantha Gibson, Environmental Studies recent graduate</li>
      <li>Anson Ekau, Data Science recent graduate</li>
      <li>Brandon Koskie, Data Science student</li>
      <li>Students in multiple courses taught by Professor Ruiz-Jones since 2021</li>
    </ul>

    <h4><strong>A few notes about the data</strong></h4>
    <ul>
      <li>Onset sensors and data loggers record every 10 min continuously when deployed.</li>
      <li>Sensors are calibrated following the manufacturer’s guidelines.</li>
      <li>We continue to collect data and will continue to add it to the dashboard.</li>
      <li>Student research interns have presented results from this project at the Hawaiʻi Conservation Conference 2023, 2024, and 2025.</li>
    </ul>

    <table class="table table-bordered" style="max-width: 480px;">
      <tr><th>Variable</th><th>Date we started to collect data</th></tr>
      <tr><td>Temperature</td><td>2021</td></tr>
      <tr><td>pH</td><td>2022</td></tr>
      <tr><td>Dissolved Oxygen</td><td>2022</td></tr>
      <tr><td>Conductivity (Salinity)</td><td>2022</td></tr>
      <tr><td>Water Level Change</td><td>2023</td></tr>
    </table>

    <h4><strong>Funding</strong></h4>
    <p>
      Student research interns have been supported through the following funding sources:
      NSF Alliance Supporting Pacific through Computation Excellence (All-SPICE) and
      NSF Louis Stokes Alliances for Minority Participation (LSAMP).
    </p>

    <div style="display: flex; gap: 20px; margin-top: 30px; flex-wrap: wrap;">
      <img src="mast_chris_instruction.png" height="150px" style="border-radius: 10px;">
      <img src="mast_lupita_instruction.png" height="150px" style="border-radius: 10px;">
      <img src="mast_anson.png" height="150px" style="border-radius: 10px;">
      <img src="mast_brandon.png" height="150px" style="border-radius: 10px;">
    </div>

    <div style="display: flex; justify-content: space-around; align-items: center; margin: 40px 0; flex-wrap: wrap; gap: 20px;">
      <img src="MFHClogo.png" height="60px">
      <img src="SPICELogo1.png" height="60px">
      <img src="cuh_logo.png" height="60px">
    </div>
  </div>
')

# --- UI -----------------------------------------------------------------------
ui <- page_sidebar(
  title = tags$img(src = "app_logo_updated.png", height = "110px", alt = "Fishpond dashboard"),
  window_title = "Maunalua Fishpond Dashboard",
  theme = bs_theme(version = 5, primary = "#0B5D73"),
  fillable = FALSE,
  sidebar = sidebar(
    width = 420,
    open = "desktop",
    chat_ui("chat", height = "75vh", fill = TRUE, messages = list(greeting))
  ),
  tags$head(app_styles),
  navset_tab(
    nav_panel(
      "Overview",
      h4("🌏 Kalauhaʻihaʻi & Kānewai from Above", class = "mt-3"),
      maplibreOutput("hawaii_map", height = "600px"),
      tags$small(class = "text-muted", "Imagery © Esri and its data providers"),
      overview_html
    ),
    siteUI("kanewai", sites$kanewai),
    siteUI("kalauhaihai", sites$kalauhaihai)
  )
)

# --- Server -------------------------------------------------------------------
server <- function(input, output, session) {
  
  siteServer("kanewai", sites$kanewai)
  siteServer("kalauhaihai", sites$kalauhaihai)
  
  # --- Overview map: satellite imagery with a fly-in to both sites ---
  output$hawaii_map <- renderMaplibre({
    maplibre(
      style  = carto_style("positron-no-labels"),
      center = c(-157.7319623, 21.2822266),
      zoom   = 7
    ) |>
      add_raster_source(
        id       = "esri_imagery",
        tiles    = "https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
        tileSize = 256,
        maxzoom  = 19
      ) |>
      add_raster_layer(
        id     = "esri_imagery_layer",
        source = "esri_imagery",
        raster_opacity = 1
      )
  })
  
  # Runs once, when the map first reports its position.
  observeEvent(input$hawaii_map_center, {
    # Fly to Kalauhaʻihaʻi (30 s) ...
    maplibre_proxy("hawaii_map", session = session) |>
      fly_to(
        center = c(-157.7319623, 21.2822266),
        zoom = 19.66, pitch = 60, bearing = 179.9, duration = 30000
      )
    # ... then on to Kānewai (10 s)
    later::later(function() {
      maplibre_proxy("hawaii_map", session = session) |>
        fly_to(
          center = c(-157.72686729221283, 21.2842080160462),
          zoom = 18, pitch = 60, bearing = 179.9, duration = 10000
        )
    }, delay = 31)
  }, ignoreInit = TRUE, once = TRUE)
  
  # --- Chatbot (one client per visitor session) ---
  chat <- tryCatch(
    {
      client <- chat_openai(model = openai_model, system_prompt = system_prompt_str)
      client$register_tool(tool(
        run_query,
        name = "query",
        description = paste(
          "Run a read-only DuckDB SELECT statement on the sensor_data table",
          "and return the results as JSON. Returns at most", chat_max_rows, "rows."
        ),
        arguments = list(
          query = type_string("A DuckDB SQL query; must be a SELECT statement.")
        )
      ))
      client
    },
    error = function(e) {
      message("Chat client could not be created: ", conditionMessage(e))
      NULL
    }
  )
  
  observeEvent(input$chat_user_input, {
    if (is.null(chat)) {
      chat_append("chat", "The AI assistant is unavailable right now (no API key is configured).")
      return()
    }
    # Depending on the shinychat version this is a string or a list; handle both.
    msg <- input$chat_user_input
    if (is.list(msg)) msg <- unlist(msg, use.names = FALSE)[[1]]
    # stream = "content" lets shinychat display the SQL tool calls and results.
    chat_append("chat", chat$stream_async(as.character(msg), stream = "content"))
  })
}

shinyApp(ui, server)
