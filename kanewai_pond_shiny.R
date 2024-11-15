library(readr)  # Load the readr package for read_csv()
library(here)
library(dplyr)
library(shiny)
library(ggplot2)
library(magick)


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

# Shiny app

#doesnt work yet, conversation can be found here: https://chatgpt.com/share/672d68a1-6930-8000-844a-9202fb5a25c2
ui <- fluidPage(
  titlePanel("Fishpond Sensor Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("site", "Select Site:", choices = unique(data$site)),
      selectInput("site_specific", "Select Sensor Location:", choices = unique(data$site_specific)),
      selectInput("variable", "Select Variable:", choices = unique(data$variable)),
      sliderInput(
        "date_time_hst", 
        "Select Date and Time:", 
        min = min(data$date_time_hst), 
        max = max(data$date_time_hst), 
        value = range(data$date_time_hst), 
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

server <- function(input, output, session) {
  filtered_data <- reactive({
    data %>%
      filter(
        site == input$site,
        site_specific == input$site_specific,
        variable == input$variable,
        date_time_hst >= input$date_time_hst[1],
        date_time_hst <= input$date_time_hst[2]
      )
  })
  
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
          size = 3  # Equivalent to a radius of 0.3 on a continuous scale
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

shinyApp(ui, server)
