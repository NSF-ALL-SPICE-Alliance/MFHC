library(shiny)
library(ggplot2)
library(ggforce)
library(dplyr)
library(readr)
library(here)
library(magick)
library(grid)
library(plotly)

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
        min = min(data$date_time_hst, na.rm = TRUE),
        max = max(data$date_time_hst, na.rm = TRUE),
        value = min(data$date_time_hst, na.rm = TRUE),
        timeFormat = "%Y-%m-%d %H:%M:%S",
        step = 600
      ),
      selectInput(
        "variable",
        "Select Variable:",
        choices = unique(data$variable)
      )
    ),
    mainPanel(
      plotOutput("pondImagePlot", click = "map_click", width = "100%", height = "600px"),
      plotOutput("linePlot", hover = hoverOpts(id = "plot_hover"))
    )
  )
)

# Shiny app server logic
server <- function(input, output, session) {
  filtered_data <- reactive({
    req(input$date_time_hst, input$variable)
    data %>%
      filter(date_time_hst == input$date_time_hst, variable == input$variable) %>%
      right_join(sensor_data, by = "site_specific")
  })
  
  clicked_sensor <- reactiveVal(NULL)
  observeEvent(input$map_click, {
    clicked <- nearPoints(sensor_data, input$map_click, xvar = "x", yvar = "y", maxpoints = 1)
    if (nrow(clicked) > 0) {
      clicked_sensor(clicked$site_specific)
    }
  })
  
  hovered_transect <- reactiveVal(NULL)
  observe({
    req(input$plot_hover)
    hover <- input$plot_hover
    hover_point <- st_sfc(st_point(c(hover$x, hover$y)), crs = 4326)
    kaalawai_joined_sf <- st_transform(data, crs = 4326)
    nearest_index <- st_nearest_feature(hover_point, kaalawai_joined_sf)
    if (!is.na(nearest_index)) {
      hovered_transect(kaalawai_joined_sf$site_specific[nearest_index])
    }
  })
  
  output$pondImagePlot <- renderPlot({
    pond_data <- filtered_data()
    ggplot() +
      annotation_raster(
        pond_image_raster, xmin = 0, xmax = 10, ymin = 0, ymax = 10
      ) +
      geom_circle(
        data = pond_data,
        aes(x0 = x, y0 = y, r = radius, fill = value),
        color = "black", alpha = 0.7
      ) +
      scale_fill_viridis_c(name = "Temperature (°C)", option = "C") +
      coord_fixed() +
      labs(
        title = "Pond Sensor Locations Colored by Value",
        x = "Pond X-Coordinate",
        y = "Pond Y-Coordinate"
      ) +
      theme_minimal()
  })
  
  output$linePlot <- renderPlot({
    sensor_name <- hovered_transect()
    line_data <- data %>% arrange(date_time_hst)
    
    line_data <- line_data %>%
      mutate(highlight = ifelse(site_specific == sensor_name, "Highlighted", "Other"))
    
    ggplot(line_data, aes(x = date_time_hst, y = value, group = site_specific, color = highlight)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("Highlighted" = "steelblue", "Other" = "grey")) +
      labs(
        title = "Temperature Change Over Time",
        subtitle = if (!is.null(sensor_name)) paste("Hovered Sensor:", sensor_name) else "Hover over a sensor line to highlight",
        x = "Date and Time",
        y = "Temperature (°C)"
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)