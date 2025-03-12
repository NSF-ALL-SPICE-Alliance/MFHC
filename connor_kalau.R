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
image_path <- here("new_kalauhaihai_aerial.png")
pond_image <- image_read(image_path)
pond_image_raster <- as.raster(pond_image)

# Get image dimensions
image_info <- image_info(pond_image)
image_width <- image_info$width
image_height <- image_info$height

# Define coordinate system based on image size
xmin <- 0
xmax <- image_width
ymin <- 0
ymax <- image_height

# Define sensor locations with correct radius
sensor_data <- data.frame(
  site_specific = c("Garage", "Makaha"),
  x = c(1500, 4000),
  y = c(3000, 1000),
  radius = 200  # Keep correct radius
)

# Read in variable data
data <- read_csv(here("cleaned_data/master_data_pivot.csv")) %>%
  full_join(sensor_data, by = "site_specific") %>%
  filter(variable %in% c("temperature", "pH", "oxygen") & site == "Kalauhaihai")

# Ensure date_time_hst is in POSIXct format
data$date_time_hst <- as.POSIXct(data$date_time_hst, format = "%Y-%m-%d %H:%M:%S", tz = "Pacific/Honolulu")

# Define date range for x-axis limits
start_date <- as.POSIXct("2022-06-10 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "Pacific/Honolulu")
end_date <- as.POSIXct("2024-06-13 23:59:59", format = "%Y-%m-%d %H:%M:%S", tz = "Pacific/Honolulu")

# UI
ui <- fluidPage(
  titlePanel("Kalauhaihai Fishpond Sensor Data"),
  tags$head(
    tags$style(HTML("
      /* Make the date picker label larger */
      .large-input label {
        font-size: 48px !important;
        font-weight: bold !important;
      }

      /* Make the date picker input bigger */
      .large-input .air-datepicker-global-container {
        font-size: 24px !important;
      }

      /* Make the date input box bigger */
      .large-input .form-control {
        font-size: 24px !important;
        height: 60px !important;
        padding: 10px !important;
      }

      /* Make dropdown options larger */
      .large-input .datepicker--cell {
        font-size: 44px !important;
        padding: 24px !important;
      }

      /* Increase text size in calendar */
      .large-input .air-datepicker {
        font-size: 36px !important;
        width: 500px !important;
        height: auto !important;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "large-input",
          airDatepickerInput(
            "date_time_hst",
            "Select Date and Time:",
            minDate = as.character(min(data$date_time_hst)),
            maxDate = as.character(max(data$date_time_hst)),
            value = as.character(as.POSIXct("2022-06-12 01:30 PM", format = "%Y-%m-%d %I:%M %p", tz = "Pacific/Honolulu")),
            timepicker = TRUE,
            width = '1200px'
          )
      )
    ),
    mainPanel(
      withSpinner(plotOutput("pondImagePlot", width = "100%", height = "1200px")),
      plotlyOutput("Kalauhaihai_temp", height = "500px"),
      plotlyOutput("Kalauhaihai_pH", height = "500px"),
      plotlyOutput("Kalauhaihai_oxygen", height = "500px")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive filtered data
  filtered_data <- reactive({
    data %>%
      filter(date_time_hst == input$date_time_hst) %>%
      select(site_specific, value, variable, date_time_hst) %>%
      right_join(sensor_data, by = "site_specific")
  })
  
  # Render pond image with sensors
  output$pondImagePlot <- renderPlot({
    pond_data <- filtered_data()
    
    ggplot() +
      annotation_raster(
        pond_image_raster, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
      ) +
      geom_circle(
        data = pond_data,
        aes(x0 = x, y0 = y, r = radius, fill = value),
        color = "black",
        alpha = 0.7
      ) +
      scale_fill_viridis_c(name = "Temperature (°C)", option = "C") +
      theme_minimal() +
      coord_fixed() +
      theme(legend.position = "right") +
      geom_text(
        data = pond_data,
        aes(x = x, y = y + 200, label = site_specific),
        color = "white", size = 10, fontface = "bold"
      ) +
      theme(legend.position = "right",
            legend.text = element_text(size = 20),  # Increase legend text size
            legend.title = element_text(size = 22, face = "bold"),  # Increase legend title
            legend.key.size = unit(2, "cm"),  # Increase size of legend keys (color boxes)
            legend.spacing.x = unit(0.5, "cm"),  # Adjust horizontal spacing
            legend.spacing.y = unit(0.8, "cm")   # Adjust vertical spacing
      )
  }, width = 1600, height = 1066)
  
  # Render Temperature Graph
  output$Kalauhaihai_temp <- renderPlotly({
    Kalauhaihai_temp <- data %>% filter(variable == "temperature")
    
    p1 <- ggplot(Kalauhaihai_temp, aes(x = date_time_hst, y = value, color = site_specific)) +
      geom_line(size = 0.5) +
      labs(title = "Kalauhaihai Temperature", x = "Date and Time", y = "Temperature (°C)") +
      theme_minimal() +
      scale_x_datetime(limits = c(start_date, end_date)) +
      scale_color_viridis_d()
    
    ggplotly(p1) %>%
      layout(legend = list(x = 0, y = 1, orientation = "v"))
  })
  
  # Render pH Graph
  output$Kalauhaihai_pH <- renderPlotly({
    Kalauhaihai_pH <- data %>% filter(variable == "pH")
    
    p2 <- ggplot(Kalauhaihai_pH, aes(x = date_time_hst, y = value)) +
      geom_line(size = 0.5, color = "darkgreen") +
      labs(title = "Kalauhaihai pH", x = "Date and Time", y = "pH Level") +
      theme_minimal() +
      scale_x_datetime(limits = c(start_date, end_date))
    
    ggplotly(p2)
  })
  
  # Render Oxygen Graph
  output$Kalauhaihai_oxygen <- renderPlotly({
    Kalauhaihai_oxygen <- data %>% filter(variable == "oxygen")
    
    p3 <- ggplot(Kalauhaihai_oxygen, aes(x = date_time_hst, y = value)) +
      geom_line(size = 0.5, color = "darkblue") +
      labs(title = "Kalauhaihai Oxygen", x = "Date and Time", y = "Oxygen (mg/L)") +
      theme_minimal() +
      scale_x_datetime(limits = c(start_date, end_date))
    
    ggplotly(p3)
  })
}

shinyApp(ui, server)
