library(shiny)
library(ggplot2)
library(ggforce)
library(dplyr)
library(readr)
library(here)
library(magick)
library(grid)
library(plotly)
library(shinyWidgets)
library(shinycssloaders)  # Loading spinner package
library(DBI)
library(duckdb)
library(ellmer)
library(shinychat)
library(promises)
library(mapgl)

openai_model <- "gpt-4o"

# --- NEW: Plot help UI + styles ------------------------------------------------
plotHelpUI <- function(id, title = "How to explore these charts") {
  htmltools::tags$details(
    class = "plot-help",
    open = NA,  # set to NA for open-by-default. Remove to keep collapsed.
    htmltools::tags$summary(htmltools::HTML(paste0("💡 ", title))),
    htmltools::HTML('
      <ul class="plot-help-list">
        <li><strong>Hover</strong> to see exact values, time, and sensor/site.</li>
        <li><strong>Zoom</strong>: click-and-drag a box over dates to zoom in.
            <em>Double-click</em> anywhere to reset.</li>
        <li><strong>Pan</strong>: after zooming, drag on the plot (or use the 🔎/🖐️ toolbar buttons).</li>
        <li><strong>Legend</strong>: click a legend item to hide/show a series.
            <em>Double-click</em> a legend item to isolate it; double-click again to restore all.</li>
        <li><strong>Compare</strong>: turn on <em>Compare data on hover</em> in the toolbar to see all series at once.</li>
        <li><strong>Spike lines</strong>: toggle <em>Spikes</em> for vertical guide lines under the cursor.</li>
        <li><strong>Download</strong>: use the 📷 icon to save the figure as PNG.</li>
        <li><strong>Select points</strong> (advanced): use <em>Box/Lasso Select</em>; clear with <em>Reset axes</em>.</li>
      </ul>
      <div class="plot-help-note">
        Tip: You can also drag along just the <em>x-axis</em> labels to zoom the time window precisely.
      </div>
    ')
  )
}

plotHelpStyles <- htmltools::tags$style(htmltools::HTML("
  .plot-help { 
    background:#f6faf9; border:1px solid #d7ece7; border-radius:10px; 
    padding:12px 16px; margin:10px 0 16px 0; 
  }
  .plot-help > summary {
    cursor: pointer; font-weight: 700; color:#0b5d73; margin-bottom:8px;
  }
  .plot-help-list { 
    margin: 8px 0 0 0; padding-left: 18px; line-height: 1.4;
  }
  .plot-help-list li { margin: 6px 0; }
  .plot-help-note {
    margin-top:10px; font-size:0.92em; color:#356e7a;
  }
"))

# --- DB + chatbot setup --------------------------------------------------------
conn <- dbConnect(duckdb(), dbdir = here("fishpond.duckdb"), read_only = TRUE)
onStop(\() dbDisconnect(conn))

system_prompt_str <- paste0(
  "You are an AI assistant analyzing fishpond sensor data in DuckDB. The 'sensor_data' dataset contains temperature and environmental readings from Kanewai fishpond. Answer only questions about the data using SQL compatible with DuckDB. There are 2 features in the database describing location, site and site_specific. Kanewai and Kalauhaihai are the two sites. site_specific has locations within each site including Norfolk, Garage, Shade, etc.. Ignore the users use of capitilization for site and site_specific and alway squery with a capital first letter like Kanewai."
)

greeting <- paste0(
  "👋 **Hi, I'm an AI Chatbot!** 🌊\n\n",
  "I can help you explore and analyze sensor data in this app from Kanewai Spring and Kalauhaihai Fishpond.\n\n",
  "**Ask me questions about:**\n\n",
  "✅ **Sensor readings**\n",
  "   *(e.g., \"What was the average water temperature for the Kanewai Norfolk site during August 2023?\")*\n\n",
  "✅ **Trends over time**\n",
  "   *(e.g., \"What was the date with the highest water temperature for the Kanewai Norfolk site?\")*\n\n",
  "✅ **Comparisons**\n",
  "   *(e.g., \"Were there ever hypoxic conditions at Kanewai?.\")*\n\n",
  "✅ **Summarized insights**\n",
  "   *(e.g., \"What is the average temperature across all sites for Kanewai for all recorded data?\")*\n\n",
  "To get started, type your question below! 🏝️"
)

dbListTables(conn)

# --- Images -------------------------------------------------------------------
kanewai_image <- image_read("kanewai_aerial.png")
kanewai_image_raster <- as.raster(kanewai_image)

kalauhaihai_image <- image_read("new_kalauhaihai_aerial.png")
kalauhaihai_info <- image_info(kalauhaihai_image)
img_width <- kalauhaihai_info$width
img_height <- kalauhaihai_info$height
kalauhaihai_image_raster <- as.raster(kalauhaihai_image)

# --- Sensor coordinates --------------------------------------------------------
kanewai_sensors <- data.frame(
  site_specific = c("Norfolk", "Shade", "Auwai", "RockWall", "Rock", "Springledge"),
  x = c(0.5, 4, 5.2, 6.4, 5.5, 5.75),
  y = c(0.5, 8.4, 9, 8.5, 6.5, 3.45),
  radius = 0.3
)

kalauhaihai_sensors <- data.frame(
  site_specific = c("Garage", "Makaha"),
  x = c(3.5, 6.5) * 5472 / 10,
  y = c(7, 2) * 3648 / 10,
  radius = 0.3 * 5472 / 10
)

# --- Data ---------------------------------------------------------------------
data <- read_csv(here("cleaned_data/master_data_pivot.csv"))

# --- UI -----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(plotHelpStyles),  # <--- NEW styles
  tags$div(
    style = "padding: 10px 0;",
    tags$img(src = "app_logo.png", height = "160px")
  ),
  sidebarLayout(
    sidebarPanel(
      div(
        style = "background-color: white; padding: 10px; border-radius: 8px;",
        chat_ui("chat", height = "600px", fill = TRUE)
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          h4("🌏 Kalauhaihai from Above"),
          maplibreOutput("hawaii_map", height = "600px"),
          HTML('
  <div style="padding-top: 30px; font-family: sans-serif; color: #333;">
    <h3><strong>🌺 Project Overview</strong></h3>
   <p>
  Aloha, Welcome to our pilot data dashboard!<br><br>
  This project is a collaboration with 
  <a href="https://maunaluafishpond.org/" target="_blank">Maunalua Fishpond Heritage Center</a>. 
  Since 2021, Chaminade students in Professor 
  <a href="https://lupita-ruiz-jones.squarespace.com/" target="_blank">Lupita Ruiz-Jones</a>’s environmental science classes have explored Kalauha’iha’i and Kānewai to study the impact of the vital flow of water mauka to makai. 
  This research focuses on tracking water temperature, oxygen levels, salinity, pH, and water level changes.
</p>

<p>
  At Kalauha’iha’i, data collected before and after lava tube restoration helps assess the impact of increased freshwater input and connectivity to the ocean. 
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
  <li>Student research interns have presented results from this project at the Hawai’i Conservation Conference 2023, 2024, and 2025.</li>
</ul>

<table border="1" cellpadding="5">
  <tr>
    <th>Variable</th>
    <th>Date we started to collect data</th>
  </tr>
  <tr>
    <td>Temperature</td>
    <td>2021</td>
  </tr>
  <tr>
    <td>pH</td>
    <td>2022</td>
  </tr>
  <tr>
    <td>Dissolved Oxygen</td>
    <td>2022</td>
  </tr>
  <tr>
    <td>Conductivity (Salinity)</td>
    <td>2022</td>
  </tr>
  <tr>
    <td>Water Level Change</td>
    <td>2023</td>
  </tr>
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

    <div style="display: flex; justify-content: space-around; align-items: center; margin-top: 40px;">
      <img src="MFHClogo.png" height="60px">
      <img src="SPICELogo1.png" height="60px">
      <img src="cuh_logo.png" height="60px">
    </div>
  </div>
')
        ),
        tabPanel(
          "Kanewai",
          fluidRow(
            column(
              12,
              switchInput("temp_unit_kanewai", value = FALSE, onLabel = "°F", offLabel = "°C"),
              airDatepickerInput(
                "date_time_hst_kanewai", "Select Date and Time:",
                minDate = min(data$date_time_hst),
                maxDate = max(data$date_time_hst),
                value = as.POSIXct("2022-07-10 08:00 am"),
                timepicker = TRUE
              ),
              withSpinner(plotOutput("kanewai_map", click = "kanewai_click", height = "600px")),
              
              # --- NEW help panel above the plots
              plotHelpUI("kanewai_help", "Explore Kanewai Temperature, pH, and Oxygen"),
              
              uiOutput("kanewai_sensor_plots"),
              plotlyOutput("kanewai_pH"),
              plotlyOutput("kanewai_oxygen")
            )
          )
        ),
        tabPanel(
          "Kalauhaihai",
          fluidRow(
            column(
              12,
              switchInput("temp_unit_kalauhaihai", value = FALSE, onLabel = "°F", offLabel = "°C"),
              airDatepickerInput(
                "date_time_hst_kalauhaihai", "Select Date and Time:",
                minDate = min(data$date_time_hst),
                maxDate = max(data$date_time_hst),
                value = as.POSIXct("2022-07-10 08:00 am"),
                timepicker = TRUE
              ),
              withSpinner(plotOutput("kalauhaihai_map", click = "kalauhaihai_click", height = "800px", width = "100%")),
              
              # --- NEW help panel above the plots
              plotHelpUI("kalauhaihai_help", "Explore Kalauhaihai Temperature, pH, and Oxygen"),
              
              uiOutput("kalauhaihai_sensor_plots"),
              plotlyOutput("kalauhaihai_pH"),
              plotlyOutput("kalauhaihai_oxygen")
            )
          )
        )
      )
    )
  )
)

# --- Server -------------------------------------------------------------------
server <- function(input, output, session) {
  
  ### Kanewai Logic ###
  kanewai_data <- reactive({
    data %>%
      filter(site == "Kanewai", date_time_hst == input$date_time_hst_kanewai) %>%
      right_join(kanewai_sensors, by = "site_specific")
  })
  
  kanewai_temp_data <- reactive({
    df <- kanewai_data()
    if (input$temp_unit_kanewai) {
      df <- df %>% mutate(value = value * 9/5 + 32)
    }
    df
  })
  
  kanewai_clicked_sensor <- reactiveVal(NULL)
  observeEvent(input$kanewai_click, {
    clicked <- nearPoints(kanewai_sensors, input$kanewai_click, xvar = "x", yvar = "y", maxpoints = 1)
    if (nrow(clicked) > 0) kanewai_clicked_sensor(clicked$site_specific)
  })
  
  output$kanewai_map <- renderPlot({
    pond_data <- kanewai_temp_data()
    
    ggplot() +
      annotation_raster(kanewai_image_raster, xmin = 0, xmax = 10, ymin = 0, ymax = 10) +
      geom_circle(
        data = pond_data, aes(x0 = x, y0 = y, r = radius, fill = value),
        color = "black", alpha = 0.7
      ) +
      geom_text(
        data = pond_data, aes(x = x, y = y + 0.4, label = site_specific),
        color = "white", size = 4, fontface = "bold"
      ) +
      scale_fill_viridis_c(
        name = if (input$temp_unit_kanewai) "Temperature (°F)" else "Temperature (°C)",
        option = "C"
      ) +
      coord_fixed() + theme_minimal() + theme(legend.position = "right") +
      labs(title = "Temperature Map (+ Click a sensor to see temperature over time 📈)", x = "", y = "")
  })
  
  output$kanewai_sensor_plots <- renderUI({
    if (is.null(kanewai_clicked_sensor())) return(NULL)
    withSpinner(plotlyOutput("kanewai_temperature_plot"))
  })
  
  observe({
    sensor_name <- kanewai_clicked_sensor()
    if (!is.null(sensor_name)) {
      output$kanewai_temperature_plot <- renderPlotly({
        sensor_data_combined <- data %>%
          filter(site == "Kanewai", variable == "temperature", site_specific != "general") %>%
          mutate(value = if (input$temp_unit_kanewai) value * 9/5 + 32 else value) %>%
          mutate(order = ifelse(site_specific == sensor_name, 2, 1)) %>%
          arrange(order, date_time_hst)
        
        line_plot <- ggplot(sensor_data_combined) +
          geom_line(
            data = sensor_data_combined %>% filter(site_specific != sensor_name),
            aes(x = date_time_hst, y = value, group = site_specific),
            color = "grey", size = 0.4, alpha = 0.3
          ) +
          geom_line(
            data = sensor_data_combined %>% filter(site_specific == sensor_name),
            aes(x = date_time_hst, y = value, color = site_specific),
            size = 0.8, alpha = 0.5
          ) +
          scale_color_manual(values = c(
            "Norfolk" = "#1E3A8A",
            "Shade" = "#B91C1C",
            "Auwai" = "#065F46",
            "RockWall" = "#6B21A8",
            "Rock" = "#EA580C",
            "Springledge" = "#7C2D12"
          )) +
          labs(
            title = paste("Kanewai Temperature:", sensor_name),
            x = "Date and Time",
            y = if (input$temp_unit_kanewai) "Temperature (°F)" else "Temperature (°C)"
          ) +
          theme_minimal()
        
        ggplotly(line_plot) %>%
          layout(hovermode = "x unified") %>%
          config(displaylogo = FALSE, modeBarButtonsToAdd = c("hovercompare","toggleSpikelines","toImage"))
      })
    }
  })
  
  output$kanewai_pH <- renderPlotly({
    p2 <- ggplot(
      data %>% filter(variable == "pH", site == "Kanewai"),
      aes(x = date_time_hst, y = value)
    ) +
      geom_line(size = 0.5, color = "#0B5D73") +
      labs(title = "Kanewai pH", x = "Date and Time", y = "pH Level") +
      theme_minimal()
    
    ggplotly(p2) %>%
      layout(hovermode = "x unified") %>%
      config(displaylogo = FALSE, modeBarButtonsToAdd = c("hovercompare","toggleSpikelines","toImage"))
  })
  
  output$kanewai_oxygen <- renderPlotly({
    p3 <- ggplot(
      data %>% filter(variable == "oxygen", site == "Kanewai"),
      aes(x = date_time_hst, y = value)
    ) +
      geom_line(size = 0.5, color = "#0B5D73") +
      labs(title = "Kanewai Oxygen", x = "Date and Time", y = "Oxygen (mg/L)") +
      theme_minimal()
    
    ggplotly(p3) %>%
      layout(hovermode = "x unified") %>%
      config(displaylogo = FALSE, modeBarButtonsToAdd = c("hovercompare","toggleSpikelines","toImage"))
  })
  
  ### Kalauhaihai Logic ###
  kalauhaihai_data <- reactive({
    data %>%
      filter(site == "Kalauhaihai", date_time_hst == input$date_time_hst_kalauhaihai) %>%
      right_join(kalauhaihai_sensors, by = "site_specific")
  })
  
  kalauhaihai_temp_data <- reactive({
    df <- kalauhaihai_data()
    if (input$temp_unit_kalauhaihai) {
      df <- df %>% mutate(value = value * 9/5 + 32)
    }
    df
  })
  
  kalauhaihai_clicked_sensor <- reactiveVal(NULL)
  observeEvent(input$kalauhaihai_click, {
    clicked <- nearPoints(kalauhaihai_sensors, input$kalauhaihai_click, xvar = "x", yvar = "y", maxpoints = 1)
    if (nrow(clicked) > 0) kalauhaihai_clicked_sensor(clicked$site_specific)
  })
  
  output$kalauhaihai_map <- renderPlot({
    pond_data <- kalauhaihai_temp_data()
    
    ggplot() +
      annotation_raster(kalauhaihai_image_raster, xmin = 0, xmax = 5472, ymin = 0, ymax = 3648) +
      geom_circle(
        data = pond_data, aes(x0 = x, y0 = y, r = radius, fill = value),
        color = "black", alpha = 0.7
      ) +
      geom_text(
        data = pond_data, aes(x = x, y = y + 100, label = site_specific),
        color = "white", size = 4, fontface = "bold"
      ) +
      scale_fill_viridis_c(
        name = if (input$temp_unit_kalauhaihai) "Temperature (°F)" else "Temperature (°C)",
        option = "C"
      ) +
      xlim(0, 5472) + ylim(0, 3648) +
      coord_fixed(ratio = 1, expand = FALSE) +
      theme_minimal() + theme(legend.position = "right") +
      labs(title = "Temperature Map (👆 Click a sensor to see temperature over time 📈)", x = "", y = "")
  })
  
  output$kalauhaihai_sensor_plots <- renderUI({
    if (is.null(kalauhaihai_clicked_sensor())) return(h3(""))
    withSpinner(plotlyOutput("kalauhaihai_temperature_plot"))
  })
  
  observe({
    sensor_name <- kalauhaihai_clicked_sensor()
    if (!is.null(sensor_name)) {
      output$kalauhaihai_temperature_plot <- renderPlotly({
        sensor_data_combined <- data %>%
          filter(site == "Kalauhaihai", variable == "temperature", !site_specific %in% c("general", "Auwai", "Coconut")) %>%
          mutate(value = if (input$temp_unit_kalauhaihai) value * 9/5 + 32 else value) %>%
          mutate(order = ifelse(site_specific == sensor_name, 2, 1)) %>%
          arrange(order, date_time_hst)
        
        line_plot <- ggplot(sensor_data_combined) +
          geom_line(
            data = sensor_data_combined %>% filter(site_specific != sensor_name),
            aes(x = date_time_hst, y = value, group = site_specific),
            color = "grey", size = 0.4, alpha = 0.3
          ) +
          geom_line(
            data = sensor_data_combined %>% filter(site_specific == sensor_name),
            aes(x = date_time_hst, y = value, color = site_specific),
            size = 0.8, alpha = 0.5
          ) +
          scale_color_manual(values = c("Garage" = "#1E3A8A", "Makaha" = "#B91C1C")) +
          labs(
            title = paste("Kalauhaihai Temperature:", sensor_name),
            x = "Date and Time",
            y = if (input$temp_unit_kalauhaihai) "Temperature (°F)" else "Temperature (°C)"
          ) +
          theme_minimal()
        
        ggplotly(line_plot) %>%
          layout(hovermode = "x unified") %>%
          config(displaylogo = FALSE, modeBarButtonsToAdd = c("hovercompare","toggleSpikelines","toImage"))
      })
    }
  })
  
  output$kalauhaihai_pH <- renderPlotly({
    p <- ggplot(
      data %>% filter(variable == "pH", site == "Kalauhaihai"),
      aes(x = date_time_hst, y = value)
    ) +
      geom_line(size = 0.5, color = "#0B5D73") +
      labs(title = "Kalauhaihai pH", x = "Date and Time", y = "pH Level") +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(hovermode = "x unified") %>%
      config(displaylogo = FALSE, modeBarButtonsToAdd = c("hovercompare","toggleSpikelines","toImage"))
  })
  
  output$kalauhaihai_oxygen <- renderPlotly({
    p <- ggplot(
      data %>% filter(variable == "oxygen", site == "Kalauhaihai"),
      aes(x = date_time_hst, y = value)
    ) +
      geom_line(size = 0.5, color = "#0B5D73") +
      labs(title = "Kalauhaihai Oxygen", x = "Date and Time", y = "Oxygen (mg/L)") +
      geom_hline(yintercept = 2, color = "#B91C1C", linetype = "dashed") +
      annotate("text", x = as.POSIXct("2023-02-20"), y = 2.5, label = "hypoxic conditions",
               color = "#B91C1C", hjust = 0.2, vjust = 0.5, size = 4) +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(hovermode = "x unified") %>%
      config(displaylogo = FALSE, modeBarButtonsToAdd = c("hovercompare","toggleSpikelines","toImage"))
  })
  
  # --- Maplibre overview ------------------------------------------------------
  output$hawaii_map <- renderMaplibre({
    maplibre(
      style = carto_style("voyager"),
      zoom = 0,
      center = c(0, 0),
      pitch = 0,
      bearing = 0
    )
  })
  
  observeEvent(input$hawaii_map_center, {
    maplibre_proxy("hawaii_map") |>
      fly_to(
        center = c(-157.7319623, 21.2822266),
        zoom = 19.66,
        pitch = 172,
        bearing = 179.9,
        duration = 30000
      )
  })
  
  # --- Chatbot ---------------------------------------------------------------
  append_output <- function(...) {
    txt <- paste0(...)
    shinychat::chat_append_message(
      "chat",
      list(role = "assistant", content = txt),
      chunk = TRUE,
      operation = "append",
      session = session
    )
  }
  
  update_dashboard <- function(query, title) {
    append_output("\n
sql\n", query, "\n
\n\n")
    tryCatch(
      { dbGetQuery(conn, query) },
      error = function(err) {
        append_output("> Error: ", conditionMessage(err), "\n\n")
        stop(err)
      }
    )
    if (!is.null(query)) current_query(query)
    if (!is.null(title)) current_title(title)
  }
  
  query <- function(query) {
    append_output("\n```sql\n", query, "\n```\n\n")
    df <- tryCatch(dbGetQuery(conn, query),
                   error = function(e) { append_output("> Error: ", conditionMessage(e), "\n\n"); stop(e) })
    tbl_html <- df_to_html(df, maxrows = 5)
    append_output(tbl_html, "\n\n")
    df |> jsonlite::toJSON(auto_unbox = TRUE)
  }
  
  chat <- chat_openai(model = openai_model, system_prompt = system_prompt_str)
  chat$register_tool(tool(
    update_dashboard,
    "Modifies the data presented in the data dashboard, based on the given SQL query, and also updates the title.",
    query = type_string("A DuckDB SQL query; must be a SELECT statement."),
    title = type_string("A title to display at the top of the data dashboard, summarizing the intent of the SQL query.")
  ))
  chat$register_tool(tool(
    query,
    "Perform a SQL query on the data, and return the results as JSON.",
    query = type_string("A DuckDB SQL query; must be a SELECT statement.")
  ))
  
  chat_append("chat", greeting)
  
  observeEvent(input$chat_user_input, {
    chat_append("chat", chat$stream_async(input$chat_user_input)) %...>% { }
  })
}

df_to_html <- function(df, maxrows = 5) {
  df_short <- if (nrow(df) > 10) head(df, maxrows) else df
  tbl_html <- capture.output(
    df_short |>
      xtable::xtable() |>
      print(type = "html", include.rownames = FALSE, html.table.attributes = NULL)
  ) |> paste(collapse = "\n")
  if (nrow(df_short) != nrow(df)) {
    rows_notice <- glue::glue("\n\n(Showing only the first {maxrows} rows out of {nrow(df)}.)\n")
  } else {
    rows_notice <- ""
  }
  paste0(tbl_html, "\n", rows_notice)
}

shinyApp(ui, server)
