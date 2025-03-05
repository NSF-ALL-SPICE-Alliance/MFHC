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

#testing

# Load the original fishpond image
pond_image <- image_read("new_kalauhaihai_aerial.png")
pond_image_raster <- as.raster(pond_image)  # Convert to raster format

# Define coordinates for each sensor location
sensor_data <- data.frame(
  site_specific = c("Garage", "Makaha"),
  x = c(3.5, 6.5),  # Adjusted X-coordinates
  y = c(7, 2)  # Adjusted Y-coordinates
)

# Set the radius for each sensor circle
sensor_data$radius <- 0.3

# Read in variable data
data <- read_csv(here("cleaned_data/master_data_pivot.csv"))
data <- full_join(sensor_data, data, by = "site_specific") %>%
  filter(variable == "temperature" & site == "Kalauhaihai")

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
        timepicker = TRUE
      ),
      selectInput(
        "variable",
        "Select Variable:",
        choices = unique(data$variable)
      )
    ),
    mainPanel(
      plotOutput("pondImagePlot", click = "map_click", width = "100%", height = "600px"),
      uiOutput("sensorPlots")
    )
  )
)

# Shiny app server logic
server <- function(input, output, session) {
  
  # Reactive data filtered by date and variable
  filtered_data <- reactive({
    data %>%
      filter(
        date_time_hst == input$date_time_hst,
        variable == input$variable,
        site_specific %in% c("Garage", "Makaha")
      ) %>%
      select(site_specific, value, variable, date_time_hst) %>%
      right_join(sensor_data, by = "site_specific")
  })
  
  # Highlight clicked sensor
  clicked_sensor <- reactiveVal(NULL)
  observeEvent(input$map_click, {
    clicked <- nearPoints(sensor_data, input$map_click, xvar = "x", yvar = "y", maxpoints = 1)
    if (nrow(clicked) > 0 && clicked$site_specific %in% c("Garage", "Makaha")) {
      clicked_sensor(clicked$site_specific)
    }
  })
  
  # Render the pond map with sensors
  output$pondImagePlot <- renderPlot({
    pond_data <- filtered_data()
    
    # Dynamically calculate aspect ratio
    image_dims <- dim(pond_image_raster)
    aspect_ratio <- image_dims[2] / image_dims[1]  # width / height
    
    ggplot() +
      annotation_raster(
        pond_image_raster, 
        xmin = 0, xmax = 15, ymin = 0, ymax = 15 / aspect_ratio
      ) +
      geom_circle(
        data = pond_data,
        aes(x0 = x, y0 = y, r = radius, fill = value),
        color = "black",
        alpha = 0.7
      ) +
      scale_fill_viridis_c(name = "Temperature (°C)", option = "C") +
      labs(
        title = "Pond Sensor Locations Colored by Value",
        x = "Pond X-Coordinate",
        y = "Pond Y-Coordinate"
      ) +
      theme_minimal() +
      theme(legend.position = "right")
  }, width = 600, height = 600) 
  
  # Render dynamic sensor-specific plots
  output$sensorPlots <- renderUI({
    if (is.null(clicked_sensor())) {
      return(h3("Click on a sensor on the map to view its data."))
    }
    
    sensor_name <- clicked_sensor()  # Get selected sensor
    
    output$plot_combined <- renderPlotly({
      sensor_data_combined <- data %>% 
        filter(site_specific %in% c("Garage", "Makaha"))
      
      # Reorder data so the selected sensor is plotted last (on top)
      sensor_data_combined <- sensor_data_combined %>%
        mutate(order = ifelse(site_specific == sensor_name, 2, 1)) %>%
        arrange(order, date_time_hst)
      
      # Line plot with transparency effect
      line_plot <- ggplot() +
        # Background: Non-selected sensor (grey, more transparent)
        geom_line(data = sensor_data_combined %>% filter(site_specific != sensor_name),
                  aes(x = date_time_hst, y = value, group = site_specific),
                  color = "grey", alpha = 0.8, size = 0.4) +  
        
        # Foreground: Selected sensor (colored, semi-transparent)
        geom_line(data = sensor_data_combined %>% filter(site_specific == sensor_name),
                  aes(x = date_time_hst, y = value, color = site_specific, alpha = 0.6),
                  size = 0.4) +  
        
        labs(
          title = "Temperature Data for Garage and Makaha Sensors",
          x = "Date and Time",
          y = paste(input$variable, "(units)")
        ) +
        scale_color_manual(values = c("Garage" = "blue", "Makaha" = "red")) +
        scale_alpha_identity() +  # Ensures transparency is applied
        theme_minimal()
      
      ggplotly(line_plot)
    })
    
    # Return the plot
    return(plotlyOutput("plot_combined"))
  })
}

shinyApp(ui, server)
