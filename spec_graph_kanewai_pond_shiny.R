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

# Load the original fishpond image
pond_image <- image_read("kanewai_aerial.png")
pond_image_raster <- as.raster(pond_image)  # Convert to raster format

# Define coordinates for each sensor location
sensor_data <- data.frame(
  site_specific = c("Norfolk", "Shade", "Auwai", "RockWall", "Rock", "Springledge"),
  x = c(0.5, 4, 5.2, 6.4, 5.5, 5.75),
  y = c(0.5, 8.4, 9, 8.5, 6.5, 3.45)
)

# Set the radius for each sensor circle
sensor_data$radius <- 0.3

# Read in variable data and merge with sensor data
data <- read_csv(here("cleaned_data/master_data_pivot.csv"))
data <- full_join(sensor_data, data, by = "site_specific") %>%
  filter(site == "Kanewai")  # Keep all variables for Kanewai

# Ensure no missing data for the selected variable
sum(is.na(data))

# Shiny app UI
ui <- fluidPage(
  titlePanel("Fishpond Sensor Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      airDatepickerInput(
        "date_time_hst",
        "Select Date and Time:",
        minDate = min(data$date_time_hst),
        maxDate = max(data$date_time_hst),
        value = min(data$date_time_hst),
        timepicker = TRUE
      )
    ),
    mainPanel(
      withSpinner(plotOutput("pondImagePlot", click = "map_click", width = "100%", height = "600px")), # Loading spinner
      uiOutput("sensorPlots"),
      plotlyOutput("kanewai_pH"),  # New pH graph
      plotlyOutput("kanewai_oxygen")  # New Oxygen graph
    )
  )
)

# Shiny app server logic
server <- function(input, output, session) {
  # Reactive data filtered by date
  filtered_data <- reactive({
    data %>%
      filter(date_time_hst == input$date_time_hst) %>%
      select(site_specific, value, variable, date_time_hst) %>%
      right_join(sensor_data, by = "site_specific")
  })
  
  # Highlight clicked sensor
  clicked_sensor <- reactiveVal(NULL)
  observeEvent(input$map_click, {
    clicked <- nearPoints(sensor_data, input$map_click, xvar = "x", yvar = "y", maxpoints = 1)
    if (nrow(clicked) > 0) {
      clicked_sensor(clicked$site_specific)
    }
  })
  
  # Render the pond map with sensors and labels above circles
  output$pondImagePlot <- renderPlot({
    pond_data <- filtered_data()
    
    ggplot() +
      annotation_raster(
        pond_image_raster, 
        xmin = 0, xmax = 10, ymin = 0, ymax = 10
      ) +
      geom_circle(
        data = pond_data,
        aes(x0 = x, y0 = y, r = radius, fill = value),
        color = "black",
        alpha = 0.7
      ) +
      scale_fill_viridis_c(name = "Temperature (°C)", option = "C") +
      coord_fixed() +
      labs(
        title = "Pond Sensor Locations Colored by Value",
        x = "Pond X-Coordinate",
        y = "Pond Y-Coordinate"
      ) +
      theme_minimal() +
      theme(legend.position = "right") +
      
      # Add sensor labels above the circle with white font
      geom_text(
        data = pond_data,
        aes(x = x, y = y + 0.40, label = site_specific),  # Adjust y-position for above the circle
        color = "white",  # White font
        size = 4,  # Adjust the size as needed
        fontface = "bold"  # Bold text for better visibility
      )
  })
  
  # Render dynamic sensor-specific plots with loading spinner
  output$sensorPlots <- renderUI({
    if (is.null(clicked_sensor())) {
      return(h3("Click on a sensor on the map to view its data."))
    }
    
    sensor_name <- clicked_sensor()
    withSpinner(plotlyOutput(paste0("plot_", sensor_name)))  # Loading spinner added
  })
  
  observe({
    sensor_name <- clicked_sensor()
    if (!is.null(sensor_name)) {
      output[[paste0("plot_", sensor_name)]] <- renderPlotly({
        sensor_data_combined <- data %>%
          mutate(order = ifelse(site_specific == sensor_name, 2, 1)) %>%
          arrange(order, date_time_hst)
        
        line_plot <- ggplot(sensor_data_combined) +
          geom_line(data = sensor_data_combined %>% filter(site_specific != sensor_name),
                    aes(x = date_time_hst, y = value, group = site_specific),
                    color = "grey", size = 0.4, alpha = 0.3) +  # Adjust transparency for non-selected sensors
          geom_line(data = sensor_data_combined %>% filter(site_specific == sensor_name),
                    aes(x = date_time_hst, y = value, color = site_specific),
                    size = 0.8, alpha = 0.5) +
          
          scale_color_manual(values = c("Norfolk" = "blue", "Shade" = "red", 
                                        "Auwai" = "green", "RockWall" = "purple", 
                                        "Rock" = "orange", "Springledge" = "brown")) +
          
          labs(title = paste("Kanewai Temperature:", sensor_name),
               x = "Date and Time", y = "Value") +
          theme_minimal()
        
        ggplotly(line_plot)
      })
    }
  })
  
  # Render the Kanewai pH graph
  output$kanewai_pH <- renderPlotly({
    kanewai_pH <- data %>%
      filter(variable == "pH")
    
    p2 <- ggplot(kanewai_pH, aes(x = date_time_hst, y = value)) +
      geom_line(size = 0.5, color = "darkgreen") +
      labs(title = "Kanewai pH",
           x = "Date and Time", y = "pH Level") +
      theme_minimal()
    
    ggplotly(p2)
  })
  
  # Render the Kanewai Oxygen graph
  output$kanewai_oxygen <- renderPlotly({
    kanewai_oxygen <- data %>%
      filter(variable == "oxygen")
    
    p3 <- ggplot(kanewai_oxygen, aes(x = date_time_hst, y = value)) +
      geom_line(size = 0.5, color = "darkblue") +
      labs(title = "Kanewai Oxygen",
           x = "Date and Time", y = "Oxygen (mg/L)") +
      theme_minimal()
    
    ggplotly(p3)
  })
}

shinyApp(ui, server)
