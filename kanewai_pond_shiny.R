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


# Define coordinates for each sensor location (based on the new coordinate system)
sensor_data <- data.frame(
  site_specific = c("Norfolk", "Shade", "Auwai", "RockWall", "Rock", "Springledge"),
  x = c(0.5, 4, 5.2, 6.4, 5.5, 5.75),  # Adjusted X-coordinates within the range 0-10
  y = c(0.5, 8.4, 9, 8.5, 6.5, 3.45)  # Adjusted Y-coordinates within the range 0-10
)

# Set the radius for each sensor circle (adjust as needed)
sensor_data$radius <- 0.3  # 

# Read in varibale Data
data <- read_csv(here("cleaned_data/master_data_pivot.csv"))

data <- full_join(sensor_data, data, by = "site_specific")

head(data)

data <- data %>% 
  filter(variable == "temperature" & site == "Kanewai")

sum(is.na(data))

# Shiny app

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
      )
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("linePlot")),
        column(6, plotOutput("pondImagePlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    data %>%
      filter(date_time_hst == input$date_time_hst) %>%
      select(site_specific, value) %>%
      right_join(sensor_data, by = "site_specific")
  })
  
  output$linePlot <- renderPlot({
    filtered_line_data <- data %>% filter(date_time_hst <= input$date_time_hst)
    
    ggplot(filtered_line_data, aes(x = date_time_hst, y = value, color = site_specific)) +
      geom_line() +
      facet_wrap(~site_specific, scales = "free_y") +
      labs(
        title = "Temperature Over Time by Sensor Location",
        x = "Time",
        y = "Temperature (°C)"
      ) +
      theme_minimal() +
      theme(
        strip.text = element_text(size = 12),
        legend.position = "none"
      )
  })
  
  output$pondImagePlot <- renderPlot({
    pond_data <- filtered_data()
    
    ggplot() +
      annotation_custom(
        rasterGrob(pond_image, width = unit(1, "npc"), height = unit(1, "npc")),
        xmin = 0, xmax = 10, ymin = 0, ymax = 10
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
      theme_minimal() +
      theme(legend.position = "right")
  })
}

shinyApp(ui, server)
