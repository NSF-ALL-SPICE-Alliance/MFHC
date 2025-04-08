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
library(shinycssloaders)

# Load Kanewai fishpond image
kanewai_image <- image_read("kanewai_aerial.png")
kanewai_image_raster <- as.raster(kanewai_image)

# Load Kalauhaihai fishpond image
kalauhaihai_image <- image_read("new_kalauhaihai_aerial.png")
kalauhaihai_image_raster <- as.raster(kalauhaihai_image)

# Define coordinates for Kanewai sensors
kanewai_sensors <- data.frame(
  site_specific = c("Norfolk", "Shade", "Auwai", "RockWall", "Rock", "Springledge"),
  x = c(0.5, 4, 5.2, 6.4, 5.5, 5.75),
  y = c(0.5, 8.4, 9, 8.5, 6.5, 3.45),
  radius = 0.3
)

# Define coordinates for Kalauhaihai sensors
kalauhaihai_sensors <- data.frame(
  site_specific = c("Garage", "Makaha"),
  x = c(3.5, 6.5),
  y = c(7, 2),
  radius = 0.3
)

# Read in data
data <- read_csv(here("cleaned_data/master_data_pivot.csv"))

# UI
ui <- fluidPage(
  titlePanel("Fishpond Sensor Data Visualization"),
  tabsetPanel(
    tabPanel("Kanewai",
             sidebarLayout(
               sidebarPanel(
                 airDatepickerInput("date_time_hst_kanewai", "Select Date and Time:",
                                    minDate = min(data$date_time_hst),
                                    maxDate = max(data$date_time_hst),
                                    value = as.POSIXct("2022-07-10 08:00 am"),
                                    timepicker = TRUE)
               ),
               mainPanel(
                 withSpinner(plotOutput("kanewai_map", click = "kanewai_click", height = "600px")),
                 uiOutput("kanewai_sensor_plots"),
                 plotlyOutput("kanewai_pH"),
                 plotlyOutput("kanewai_oxygen")
               )
             )
    ),
    tabPanel("Kalauhaihai",
             sidebarLayout(
               sidebarPanel(
                 airDatepickerInput("date_time_hst_kalauhaihai", "Select Date and Time:",
                                    minDate = min(data$date_time_hst),
                                    maxDate = max(data$date_time_hst),
                                    value = as.POSIXct("2022-07-10 08:00 am"),
                                    timepicker = TRUE)
               ),
               mainPanel(
                 withSpinner(plotOutput("kalauhaihai_map", click = "kalauhaihai_click", height = "600px")),
                 uiOutput("kalauhaihai_sensor_plots")
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  ### Kanewai Logic ###
  kanewai_data <- reactive({
    data %>%
      filter(site == "Kanewai", date_time_hst == input$date_time_hst_kanewai) %>%
      right_join(kanewai_sensors, by = "site_specific")
  })
  
  kanewai_clicked_sensor <- reactiveVal(NULL)
  observeEvent(input$kanewai_click, {
    clicked <- nearPoints(kanewai_sensors, input$kanewai_click, xvar = "x", yvar = "y", maxpoints = 1)
    if (nrow(clicked) > 0) {
      kanewai_clicked_sensor(clicked$site_specific)
    }
  })
  
  output$kanewai_map <- renderPlot({
    pond_data <- kanewai_data()
    
    ggplot() +
      annotation_raster(kanewai_image_raster, xmin = 0, xmax = 10, ymin = 0, ymax = 10) +
      geom_circle(data = pond_data, aes(x0 = x, y0 = y, r = radius, fill = value),
                  color = "black", alpha = 0.7) +
      geom_text(data = pond_data, aes(x = x, y = y + 0.4, label = site_specific),
                color = "white", size = 4, fontface = "bold") +
      scale_fill_viridis_c(name = "Temperature (°C)", option = "C") +
      coord_fixed() + theme_minimal() + theme(legend.position = "right") +
      labs(
        title = "Temperature Map (Select a sensor for temp graph)",
        x = "Pond X-Coordinate",  # Label for X-axis
        y = "Pond Y-Coordinate"   # Label for Y-axis
      )
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
          labs(title = paste("Kanewai Temperature:", sensor_name), x = "Date and Time", y = "Value") +
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
  
  ### Kalauhaihai Logic ###
  kalauhaihai_data <- reactive({
    data %>%
      filter(site == "Kalauhaihai", date_time_hst == input$date_time_hst_kalauhaihai) %>%
      right_join(kalauhaihai_sensors, by = "site_specific")
  })
  
  kalauhaihai_clicked_sensor <- reactiveVal(NULL)
  observeEvent(input$kalauhaihai_click, {
    clicked <- nearPoints(kalauhaihai_sensors, input$kalauhaihai_click, xvar = "x", yvar = "y", maxpoints = 1)
    if (nrow(clicked) > 0) {
      kalauhaihai_clicked_sensor(clicked$site_specific)
    }
  })
  
  output$kalauhaihai_map <- renderPlot({
    pond_data <- kalauhaihai_data()
    
    ggplot() +
      annotation_raster(kalauhaihai_image_raster, xmin = 0, xmax = 10, ymin = 0, ymax = 10) +
      geom_circle(data = pond_data, aes(x0 = x, y0 = y, r = radius, fill = value),
                  color = "black", alpha = 0.7) +
      geom_text(data = pond_data, aes(x = x, y = y + 0.4, label = site_specific),
                color = "white", size = 4, fontface = "bold") +
      scale_fill_viridis_c(name = "Temperature (°C)", option = "C") +
      coord_fixed() + theme_minimal() + theme(legend.position = "right") +
      labs(
        title = "Temperature Map (Select sensor for temp graph)",
        x = "Pond X-Coordinate",  # Label for X-axis
        y = "Pond Y-Coordinate"   # Label for Y-axis
      )
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
          labs(title = paste("Kalauhaihai Temperature:", sensor_name), x = "Date and Time", y = "Value") +
          theme_minimal()
        
        ggplotly(line_plot)
      })
    }
  })
}

shinyApp(ui, server)
