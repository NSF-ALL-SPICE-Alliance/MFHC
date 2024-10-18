library(tidyverse)
library(here)
library(shiny)
library(bslib)
library(bsicons)
library(plotly)

# Read your data
data <- read_csv(here("cleaned_data/master_data_pivot.csv"))

# Sidebar content
sidebar_content <- list(
  selectInput("site", "Select Site:", choices = unique(data$site)),
  selectInput("site_specific", "Select Site Specific:", choices = NULL),
  selectInput("variable", "Select Variable:", choices = unique(data$variable)),
  HTML('<img src="MFHClogo.png" width="100%" height="auto">'),
  "Welcome to the Maunalua Fishpond Heritage Center dashboard. Explore data on pH, oxygen levels, temperature, and conductivity. Please note that the site is a work in progress, with ongoing updates to enhance functionality and data availability."
)

# UI definition
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('/MFHC logo.png');
        background-size: cover;
        background-position: center;
        height: 100vh;
        display: flex;
        justify-content: center;
        align-items: center;
      }
      .landing {
        background: rgba(255, 255, 255, 0.8);
        padding: 20px;
        border-radius: 10px;
        text-align: center;
      }
      .landing a {
        font-size: 24px;
        text-decoration: none;
        color: #000;
      }
      .landing a:hover {
        color: #007BFF;
      }
    "))
  ),
  div(class = "landing",
      a(href = "#main", "Enter the App")
  ),
  uiOutput("main_ui")
)

# Server logic
server <- function(input, output, session) {
  
  output$main_ui <- renderUI({
    req(input$hash == "#main")
    fluidPage(
      h1("Main App"),
   # Added comma to separate elements in fluidPage
      
      observe({
        selected_site <- input$site
        updateSelectInput(session, "site_specific", choices = unique(data[data$site == selected_site, "site_specific"]))
      }),
      
      plotlyOutput("linePlot")
    )
  })
  
  output$linePlot <- renderPlotly({
    filtered_data <- data %>%
      filter(site == input$site,
             site_specific == input$site_specific,
             variable == input$variable)
    
    p <- ggplot(filtered_data, aes(x = date_time_hst, y = value)) +
      geom_line() +
      labs(title = paste("Line Graph of", input$variable, "at", input$site, "-", input$site_specific),
           x = "Date Time",
           y = input$variable)
    
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui, server)
