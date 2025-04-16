knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(here)
library(shiny)
library(bslib)
library(bsicons)
library(plotly)
library(ggplot2)
library(ggforce)
library(magick)
library(grid)
library(shinyWidgets)
library(shinycssloaders)
library(DBI)
library(duckdb)
library(shinychat)
library(jsonlite)
library(xtable)
library(glue)

# Load master data
data <- read_csv(here("cleaned_data/master_data_pivot.csv"))

# Define shared sidebar content
sidebar_content <- list(
  selectInput("site", "Select Site:", choices = unique(data$site)),
  selectInput("site_specific", "Select Site Specific:", choices = NULL),
  selectInput("variable", "Select Variable:", choices = unique(data$variable)),
  conditionalPanel(
    condition = "input.variable == 'temperature'",
    radioButtons(
      inputId = "temp_unit",
      label = "Select Temperature Unit:",
      choices = c("Celsius" = "C", "Fahrenheit" = "F"),
      selected = "C"
    )
  ),
  HTML('<img src="MFHClogo.png" width="100%" height="auto">'),
  "Welcome to the Maunalua Fishpond Heritage Center dashboard. Explore data on pH, oxygen levels, temperature, and conductivity. Please note that the site is a work in progress, with ongoing updates to enhance functionality and data availability."
)

# Load Kanewai and Kalauhaihai images
kanewai_image <- image_read("kanewai_aerial.png")
kanewai_image_raster <- as.raster(kanewai_image)
kalauhaihai_image <- image_read("new_kalauhaihai_aerial.png")
kalauhaihai_image_raster <- as.raster(kalauhaihai_image)

# Define sensor coordinates
kanewai_sensors <- data.frame(
  site_specific = c("Norfolk", "Shade", "Auwai", "RockWall", "Rock", "Springledge"),
  x = c(0.5, 4, 5.2, 6.4, 5.5, 5.75),
  y = c(0.5, 8.4, 9, 8.5, 6.5, 3.45),
  radius = 0.3
)

kalauhaihai_sensors <- data.frame(
  site_specific = c("Garage", "Makaha"),
  x = c(3.5, 6.5),
  y = c(7, 2),
  radius = 0.3
)

# DuckDB setup
conn <- dbConnect(duckdb(), dbdir = here("fishpond.duckdb"), read_only = TRUE)
onStop(function() dbDisconnect(conn))

system_prompt_str <- paste0(
  "You are an AI assistant analyzing fishpond sensor data in DuckDB. The 'sensor_data' dataset contains temperature and environmental readings from Kanewai fishpond. Answer only questions about the data using SQL compatible with DuckDB."
)

greeting <- paste0(
  "\U0001F44B **Welcome to the Fishpond Sensor Chatbot!** \U0001F30A\n\n",
  "I can help you explore and analyze sensor data from Kanewai Fishpond.\n\n",
  "**Ask me questions about:**\n\n",
  "✅ **Sensor readings**\n   *(e.g., \"What was the water temperature at Norfolk on a specific date?\")*\n\n",
  "✅ **Trends over time**\n   *(e.g., \"Show me the highest temperature at a specific site\")*\n\n",
  "✅ **Comparisons**\n   *(e.g., \"Compare oxygen levels at RockWall and Shade.\")*\n\n",
  "✅ **Summarized insights**\n   *(e.g., \"What is the average temperature across all sensors?\")*\n\n",
  "To get started, type your question below! \U0001F3DD\uFE0F"
)

# Define UI
ui <- navbarPage(
  "Maunalua Fishpond Heritage Center Dashboard",
  theme = bs_theme(bootswatch = "darkly", primary = "#c5a668"),
  
  # Dashboard tab
  tabPanel("Dashboard",
           page_sidebar(
             sidebar = sidebar(sidebar_content),
             layout_columns(
               card(
                 plotlyOutput("linePlot")
               ),
               col_widths = c(12)
             )
           )
  ),
  
  # Kanewai tab
  tabPanel("Kanewai",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 chat_ui("chat", height = "400px", fill = TRUE),
                 airDatepickerInput(
                   "date_time_hst_kanewai",
                   "Select Date and Time:",
                   minDate = min(data$date_time_hst),
                   maxDate = max(data$date_time_hst),
                   value = min(data$date_time_hst),
                   timepicker = TRUE
                 ),
                 switchInput("temp_unit_kanewai", value = FALSE, onLabel = "°F", offLabel = "°C")
               ),
               mainPanel(
                 withSpinner(plotOutput("kanewai_map", click = "kanewai_click", height = "600px")),
                 uiOutput("kanewai_sensor_plots"),
                 plotlyOutput("kanewai_pH"),
                 plotlyOutput("kanewai_oxygen")
               )
             )
           )
  ),
  
  # Kalauhaihai tab
  tabPanel("Kalauhaihai",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 chat_ui("chat", height = "400px", fill = TRUE),
                 airDatepickerInput(
                   "date_time_hst_kalauhaihai",
                   "Select Date and Time:",
                   minDate = min(data$date_time_hst),
                   maxDate = max(data$date_time_hst),
                   value = min(data$date_time_hst),
                   timepicker = TRUE
                 ),
                 switchInput("temp_unit_kalauhaihai", value = FALSE, onLabel = "°F", offLabel = "°C")
               ),
               mainPanel(
                 withSpinner(plotOutput("kalauhaihai_map", click = "kalauhaihai_click", height = "600px")),
                 uiOutput("kalauhaihai_sensor_plots")
               )
             )
           )
  )
)


server <- function(input, output, session) {

  # --- MFHC Dashboard Server Logic ---

  validate_selection <- function(site_specific, variable) {
    valid_variables <- c("temperature", "oxygen", "pH")
    if (!variable %in% valid_variables) {
      stop("Error: The selected variable is not valid. Please select one of: temperature, oxygen, or pH.")
    }
    if (site_specific == "General" && variable == "temperature") {
      stop("Please select a specific site.")
    }
    if (site_specific != "General" && variable %in% c("pH", "oxygen")) {
      stop("Only temperature is available for specific sites. Select Temperature or General to view the data.")
    }
    return(TRUE)
  }

  observe({
    selected_site <- input$site
    updateSelectInput(session, "site_specific", choices = unique(data[data$site == selected_site, "site_specific"]))
  })

  filtered_data <- reactive({
    req(input$site, input$site_specific, input$variable)
    validate_selection(input$site_specific, input$variable)
    filtered <- data %>%
      filter(site == input$site,
             site_specific == input$site_specific,
             variable == input$variable)
    if (input$variable == "temperature" && input$temp_unit == "F") {
      filtered$value <- filtered$value * 9 / 5 + 32
    }
    filtered
  })

  output$linePlot <- renderPlotly({
    data <- filtered_data()
    unit <- ifelse(input$variable == "temperature" && input$temp_unit == "F", "°F", "°C")
    p <- ggplot(data, aes(x = date_time_hst, y = value)) +
      geom_line(color = "mediumaquamarine") +
      labs(
        title = paste("Line Graph of", input$variable, "at", input$site, "-", input$site_specific),
        x = "Date Time",
        y = paste(input$variable, "(", unit, ")")
      ) +
      theme_bw()
    ggplotly(p)
  })

  # --- Kanewai Tab ---

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
    if (nrow(clicked) > 0) {
      kanewai_clicked_sensor(clicked$site_specific)
    }
  })

  output$kanewai_map <- renderPlot({
    pond_data <- kanewai_temp_data()
    ggplot() +
      annotation_raster(kanewai_image_raster, xmin = 0, xmax = 10, ymin = 0, ymax = 10) +
      geom_circle(data = pond_data, aes(x0 = x, y0 = y, r = radius, fill = value),
                  color = "black", alpha = 0.7) +
      geom_text(data = pond_data, aes(x = x, y = y + 0.4, label = site_specific),
                color = "white", size = 4, fontface = "bold") +
      scale_fill_viridis_c(
        name = if (input$temp_unit_kanewai) "Temperature (°F)" else "Temperature (°C)",
        option = "C"
      ) +
      coord_fixed() + theme_minimal() + theme(legend.position = "right") +
      labs(title = "Temperature Map (Select a sensor for temp graph)",
           x = "Pond X-Coordinate", y = "Pond Y-Coordinate")
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
          geom_line(data = sensor_data_combined %>% filter(site_specific != sensor_name),
                    aes(x = date_time_hst, y = value, group = site_specific),
                    color = "grey", size = 0.4, alpha = 0.3) +
          geom_line(data = sensor_data_combined %>% filter(site_specific == sensor_name),
                    aes(x = date_time_hst, y = value, color = site_specific),
                    size = 0.8, alpha = 0.5) +
          scale_color_manual(values = c("Norfolk" = "blue", "Shade" = "red", 
                                        "Auwai" = "green", "RockWall" = "purple", 
                                        "Rock" = "orange", "Springledge" = "brown")) +
          labs(title = paste("Kanewai Temperature:", sensor_name),
               x = "Date and Time",
               y = if (input$temp_unit_kanewai) "Temperature (°F)" else "Temperature (°C)") +
          theme_minimal()

        ggplotly(line_plot)
      })
    }
  })

  output$kanewai_pH <- renderPlotly({
    p2 <- ggplot(data %>% filter(variable == "pH", site == "Kanewai"), aes(x = date_time_hst, y = value)) +
      geom_line(size = 0.5, color = "darkgreen") +
      labs(title = "Kanewai pH", x = "Date and Time", y = "pH Level") +
      theme_minimal()
    ggplotly(p2)
  })

  output$kanewai_oxygen <- renderPlotly({
    p3 <- ggplot(data %>% filter(variable == "oxygen", site == "Kanewai"), aes(x = date_time_hst, y = value)) +
      geom_line(size = 0.5, color = "darkblue") +
      labs(title = "Kanewai Oxygen", x = "Date and Time", y = "Oxygen (mg/L)") +
      theme_minimal()
    ggplotly(p3)
  })

  # --- Kalauhaihai Tab ---

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
    if (nrow(clicked) > 0) {
      kalauhaihai_clicked_sensor(clicked$site_specific)
    }
  })

  output$kalauhaihai_map <- renderPlot({
    pond_data <- kalauhaihai_temp_data()

    ggplot() +
      annotation_raster(kalauhaihai_image_raster, xmin = 0, xmax = 10, ymin = 0, ymax = 10) +
      geom_circle(data = pond_data, aes(x0 = x, y0 = y, r = radius, fill = value),
                  color = "black", alpha = 0.7) +
      geom_text(data = pond_data, aes(x = x, y = y + 0.4, label = site_specific),
                color = "white", size = 4, fontface = "bold") +
      scale_fill_viridis_c(
        name = if (input$temp_unit_kalauhaihai) "Temperature (°F)" else "Temperature (°C)",
        option = "C"
      ) +
      coord_fixed() + theme_minimal() + theme(legend.position = "right") +
      labs(title = "Temperature Map (Select sensor for temp graph)",
           x = "Pond X-Coordinate", y = "Pond Y-Coordinate")
  })

  output$kalauhaihai_sensor_plots <- renderUI({
    if (is.null(kalauhaihai_clicked_sensor())) {
      return(h3("Click on a sensor on the map to view its data."))
    }
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
          geom_line(data = sensor_data_combined %>% filter(site_specific != sensor_name),
                    aes(x = date_time_hst, y = value, group = site_specific),
                    color = "grey", size = 0.4, alpha = 0.3) +
          geom_line(data = sensor_data_combined %>% filter(site_specific == sensor_name),
                    aes(x = date_time_hst, y = value, color = site_specific),
                    size = 0.8, alpha = 0.5) +
          scale_color_manual(values = c("Garage" = "blue", "Makaha" = "red")) +
          labs(title = paste("Kalauhaihai Temperature:", sensor_name),
               x = "Date and Time",
               y = if (input$temp_unit_kalauhaihai) "Temperature (°F)" else "Temperature (°C)") +
          theme_minimal()

        ggplotly(line_plot)
      })
    }
  })
}



shinyApp(ui = ui, server = server)

