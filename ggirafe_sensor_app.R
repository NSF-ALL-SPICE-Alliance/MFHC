library(shiny)
library(ggplot2)
library(ggforce)
library(dplyr)
library(readr)
library(here)
library(magick)
library(grid)
library(ggiraph)

# Load the original fishpond image
pond_image <- image_read("kanewai_aerial.png")
pond_image_raster <- as.raster(pond_image)  # Convert to raster format

# Define coordinates for each sensor location (based on the new coordinate system)
sensor_data <- data.frame(
  site_specific = c("Norfolk", "Shade", "Auwai", "RockWall", "Rock", "Springledge"),
  x = c(0.5, 4, 5.2, 6.4, 5.5, 5.75),  # Adjusted X-coordinates within the range 0-10
  y = c(0.5, 8.4, 9, 8.5, 6.5, 3.45),  # Adjusted Y-coordinates within the range 0-10
  radius = 3  # Adjust point size for better visualization
)

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
      fluidRow(
        column(6,
               girafeOutput("pondImagePlot", width = "100%", height = "600px")
        ),
        column(6,
               girafeOutput("lineGraph", width = "100%", height = "600px")
        )
      )
    )
  )
)

# Shiny app server logic
server <- function(input, output, session) {
  # Reactive data filtered by date and variable
  filtered_data <- reactive({
    data %>%
      filter(variable == input$variable) %>%
      select(site_specific, value, variable, date_time_hst) %>%
      right_join(sensor_data, by = "site_specific")
  })
  
  # Highlight clicked sensor
  clicked_sensor <- reactiveVal(NULL)
  observeEvent(input$pondImagePlot_selected, {
    clicked <- input$pondImagePlot_selected
    if (!is.null(clicked)) {
      clicked_sensor(clicked$id)
    }
  })
  
  # Render the pond map with sensors
  output$pondImagePlot <- renderGirafe({
    pond_data <- filtered_data()
    
    plot <- ggplot() +
      annotation_raster(
        pond_image_raster, 
        xmin = 0, xmax = 10, ymin = 0, ymax = 10
      ) +
      geom_point_interactive(
        data = pond_data,
        aes(x = x, y = y, size = radius, color = value, tooltip = site_specific, data_id = site_specific),
        alpha = 0.7
      ) +
      scale_size_identity() +
      scale_color_viridis_c(name = "Temperature (°C)", option = "C") +
      coord_fixed() +
      labs(
        title = "Pond Sensor Locations Colored by Value",
        x = "Pond X-Coordinate",
        y = "Pond Y-Coordinate"
      ) +
      theme_minimal() +
      theme(legend.position = "right")
    
    girafe(ggobj = plot, width_svg = 10, height_svg = 6)
  })
  
  # Render line graph with all data and highlight selected sensor
  output$lineGraph <- renderGirafe({
    all_data <- data %>% filter(variable == input$variable)
    
    selected_sensor <- clicked_sensor()
    
    plot <- ggplot(all_data, aes(x = date_time_hst, y = value, group = site_specific, color = site_specific)) +
      geom_line_interactive(aes(alpha = ifelse(site_specific == selected_sensor, 1, 0.3),
                                tooltip = paste(site_specific, value)),
                            size = 1) +
      scale_alpha_identity() +
      labs(
        title = "Time Series Data for All Sensors",
        x = "Date and Time",
        y = paste(input$variable, "(units)")
      ) +
      theme_minimal()
    
    girafe(ggobj = plot, width_svg = 10, height_svg = 6)
  })
}

shinyApp(ui, server)


