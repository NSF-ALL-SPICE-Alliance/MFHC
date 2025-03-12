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
data <- read_csv(here("cleaned_data/master_data_pivot.csv")) %>%
  full_join(sensor_data, by = "site_specific") %>%
  filter(site == "Kanewai")  # Keep all variables for Kanewai

# Ensure date_time_hst is in POSIXct format with explicit timezone
#data$date_time_hst <- as.POSIXct(data$date_time_hst, format = "%Y-%m-%d %H:%M:%S", tz = "Pacific/Honolulu")


# Ensure no missing data for the selected variable
#sum(is.na(data))

# Shiny app UI
# Shiny app UI
ui <- fluidPage(
  titlePanel(""),
  
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style(HTML("
      /* Make the date picker label larger */
      .large-input label {
        font-size: 48px !important;  /* Increase label size */
        font-weight: bold !important; /* Make it bold */
      }

      /* Make the date picker input bigger */
      .large-input .air-datepicker-global-container {
        font-size: 24px !important;  /* Larger text */
      }

      /* Make the date input box bigger */
      .large-input .form-control {
        font-size: 24px !important;  /* Bigger text */
        height: 60px !important;     /* Taller box */
        padding: 10px !important;
      }

      /* Make dropdown options larger */
      .large-input .datepicker--cell {
        font-size: 44px !important;
        padding: 24px !important;
        .large-input .air-datepicker {
    font-size: 36px !important;  /* Increase text size in calendar */
    width: 500px !important;     /* Make the calendar box bigger */
    height: auto !important;
}
      }
    "))
      ),
      
      # Apply the class to the wrapper div
      div(class = "large-input",
          airDatepickerInput(
            "date_time_hst",
            "Select Date and Time:",
            minDate = as.character(min(data$date_time_hst)),
            maxDate = as.character(max(data$date_time_hst)),
            value = as.character(as.POSIXct("2022-06-12 01:30 PM", format = "%Y-%m-%d %I:%M %p", tz = "Pacific/Honolulu")),
            timepicker = TRUE,
            width = '1200px',
            startView = as.character(as.POSIXct("2022-06-12 01:30 PM", format = "%Y-%m-%d %I:%M %p", tz = "Pacific/Honolulu"))
          )
      )
    ),
    
    mainPanel(
      withSpinner(plotOutput("pondImagePlot", click = "map_click", width = "100%", height = "1200px")), # Larger pond image
      plotlyOutput("kanewai_temp", height = "500px"),
      plotlyOutput("kanewai_pH", height = "500px"),
      plotlyOutput("kanewai_oxygen", height = "500px")
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
        title = "",
        x = "",
        y = ""
      ) +
      theme_minimal() +
      theme(legend.position = "right",
            legend.text = element_text(size = 20),  # Increase legend text size
            legend.title = element_text(size = 22, face = "bold"),  # Increase legend title
            legend.key.size = unit(2, "cm"),  # Increase size of legend keys (color boxes)
            legend.spacing.x = unit(0.5, "cm"),  # Adjust horizontal spacing
            legend.spacing.y = unit(0.8, "cm")   # Adjust vertical spacing
      ) +
      
      # Add sensor labels above the circle with white font
      geom_text(
        data = pond_data,
        aes(x = x, y = y + 0.40, label = site_specific),  # Adjust y-position for above the circle
        color = "white",  # White font
        size = 8,  # Adjust the size as needed
        fontface = "bold"  # Bold text for better visibility
      )
  })
  
  start_date <- as.POSIXct("2022-06-10 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "Pacific/Honolulu")
  end_date <- as.POSIXct("2024-06-13 23:59:59", format = "%Y-%m-%d %H:%M:%S", tz = "Pacific/Honolulu")
  
  
  # Render the Kanewai temp graph
  output$kanewai_temp <- renderPlotly({
    kanewai_temp <- data %>%
      filter(variable == "temperature")
    
    p1 <- ggplot(kanewai_temp, aes(x = date_time_hst, y = value, color = site_specific)) +
      geom_line(size = 0.5) +
      labs(title = "Kanewai temp",
           x = "Date and Time", y = "temp") +
      theme_minimal() +
      scale_x_datetime(limits = c(start_date, end_date)) +
      scale_color_viridis_d() 
    
    ggplotly(p1) %>%
      layout(legend = list(x = 0, y = 1, orientation = "v"))
  })
  
  
  # Render the Kanewai pH graph
  output$kanewai_pH <- renderPlotly({
    kanewai_pH <- data %>%
      filter(variable == "pH")
    
    p2 <- ggplot(kanewai_pH, aes(x = date_time_hst, y = value)) +
      geom_line(size = 0.5, color = "darkgreen") +
      labs(title = "Kanewai pH",
           x = "Date and Time", y = "pH Level") +
      theme_minimal() +
      scale_x_datetime(limits = c(start_date, end_date)) 
    
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
      theme_minimal() +
      scale_x_datetime(limits = c(start_date, end_date)) 
    
    ggplotly(p3)
  })
}

shinyApp(ui, server)