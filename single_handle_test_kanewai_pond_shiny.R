# Load required libraries
library(shiny)
library(ggplot2)
library(ggforce)
library(dplyr)
library(readr)
library(here)
library(magick)

# Load the original fishpond image
pond_image <- image_read("kanewai_aerial.png")

# Define sensor locations with coordinates
sensor_locations <- data.frame(
  site_specific = c("Norfolk", "Shade", "Auwai", "RockWall", "Rock", "Springledge"),
  x = c(0.5, 4, 5.2, 6.4, 5.5, 5.75),  # Adjusted X-coordinates within the range 0-10
  y = c(0.5, 8.4, 9, 8.5, 6.5, 3.45),  # Adjusted Y-coordinates within the range 0-10
  radius = 0.3
)

# Read in variable data and merge with sensor locations
sensor_df <- read_csv(here("cleaned_data/master_data_pivot.csv"))

# Merge sensor locations with temperature data
sensor_df <- full_join(sensor_locations, sensor_df, by = "site_specific")

# Ensure the date column is in POSIXct format
sensor_df$date_time_hst <- as.POSIXct(sensor_df$date_time_hst)

# Define the Shiny UI
ui <- fluidPage(
  titlePanel("Fishpond Sensor Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("site", "Select Site:", choices = unique(sensor_df$site)),
      selectInput("site_specific", "Select Sensor Location:", choices = unique(sensor_df$site_specific)),
      selectInput("variable", "Select Variable:", choices = unique(sensor_df$variable)),
      sliderInput(
        "date_time_hst", 
        "Select Date and Time:", 
        min = min(sensor_df$date_time_hst, na.rm = TRUE), 
        max = max(sensor_df$date_time_hst, na.rm = TRUE), 
        value = min(sensor_df$date_time_hst, na.rm = TRUE),  # Single date for single handle
        timeFormat = "%Y-%m-%d %H:%M:%S",
        step = 600,  # 10-minute step
        animate = animationOptions(interval = 1000, loop = TRUE)
      )
    ),
    mainPanel(
      plotOutput("pondPlot")
    )
  )
)

# Define the Shiny server logic
server <- function(input, output, session) {
  # Reactive filtered data based on input selections
  filtered_data <- reactive({
    sensor_df %>%
      filter(
        site == input$site,
        site_specific == input$site_specific,
        variable == input$variable,
        date_time_hst == input$date_time_hst  # Match the exact selected date and time
      )
  })
  
  # Render the fishpond plot with sensor data
  output$pondPlot <- renderPlot({
    pond_data <- filtered_data()
    
    if (nrow(pond_data) > 0) {
      ggplot() +
        annotation_raster(
          as.raster(pond_image), 
          xmin = 0, xmax = 10, ymin = 0, ymax = 10
        ) +
        geom_point(
          data = pond_data, 
          aes(x = x, y = y, color = value), 
          size = 3  # Adjust size as needed
        ) +
        scale_color_viridis_c(option = "C", name = paste(input$variable, "(units)")) +
        xlim(0, 10) +
        ylim(0, 10) +
        theme_void() +
        theme(legend.position = "right") +
        ggtitle(paste("Sensor Data for", input$site_specific, "(", input$variable, ")"))
    } else {
      ggplot() + 
        annotate("text", x = 5, y = 5, label = "No data available", size = 6) +
        theme_void()
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
