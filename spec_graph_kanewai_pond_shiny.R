library(shiny)
library(ggplot2)
library(ggforce)
library(dplyr)
library(readr)
library(here)
library(magick)
library(grid)

# Load the original fishpond image
pond_image <- image_read("kanewai_aerial.png")
pond_image_raster <- as.raster(pond_image)  # Convert to raster format

# Define coordinates for each sensor location (based on the new coordinate system)
sensor_data <- data.frame(
  site_specific = c("Norfolk", "Shade", "Auwai", "RockWall", "Rock", "Springledge"),
  x = c(0.5, 4, 5.2, 6.4, 5.5, 5.75),  # Adjusted X-coordinates within the range 0-10
  y = c(0.5, 8.4, 9, 8.5, 6.5, 3.45)  # Adjusted Y-coordinates within the range 0-10
)

# Set the radius for each sensor circle (adjust as needed)
sensor_data$radius <- 0.3

# Read in variable data and merge with sensor data
data <- read_csv(here("cleaned_data/master_data_pivot.csv"))
data <- full_join(sensor_data, data, by = "site_specific") %>%
  filter(variable == "temperature" & site == "Kanewai")

# Ensure no missing data for the selected variable
sum(is.na(data))

# Shiny app UI
ui <- fluidPage(
  titlePanel("Fishpond Sensor Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "date_time_hst",
        "Select Date and Time:",
        min = min(data$date_time_hst),
        max = max(data$date_time_hst),
        value = min(data$date_time_hst),
        timeFormat = "%Y-%m-%d %H:%M:%S",
        step = 600  # 10-minute increments
      ),
      selectInput(
        "variable",
        "Select Variable:",
        choices = unique(data$variable)
      )
    ),
    mainPanel(
      plotOutput("pondImagePlot", click = "map_click"),
      uiOutput("sensorPlots")
    )
  )
)

# Shiny app server logic
server <- function(input, output, session) {
  # Reactive data filtered by date and variable
  filtered_data <- reactive({
    data %>%
      filter(date_time_hst == input$date_time_hst, variable == input$variable) %>%
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
  
  # Render the pond map with sensors
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
      theme(legend.position = "right")
  })
  
  # Render dynamic sensor-specific plots
  output$sensorPlots <- renderUI({
    if (is.null(clicked_sensor())) {
      return(h3("Click on a sensor on the map to view its data."))
    }
    
    sensor_name <- clicked_sensor()
    plotOutput(paste0("plot_", sensor_name))
  })
  
  observe({
    sensor_name <- clicked_sensor()
    if (!is.null(sensor_name)) {
      output[[paste0("plot_", sensor_name)]] <- renderPlot({
        sensor_data <- data %>% filter(site_specific == sensor_name)
        
        ggplot(sensor_data, aes(x = date_time_hst, y = value)) +
          geom_line(color = "blue") +
          labs(
            title = paste("Data for Sensor:", sensor_name),
            x = "Date and Time",
            y = paste(input$variable, "(units)")
          ) +
          theme_minimal()
      })
    }
  })
}

shinyApp(ui, server)
