
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(here)
library(shiny)
library(bslib)
library(bsicons)



data <- read_csv(here("cleaned_data/master_data_pivot.csv"))



sidebar_content <- list(
  selectInput("site", "Select Site:", choices = unique(data$site)),
  selectInput("site_specific", "Select Site Specific:", choices = NULL),
  selectInput("variable", "Select Variable:", choices = unique(data$variable)),
  HTML('<img src="MFHClogo.png" width="100%" height="auto">'),
  "Welcome to the Maunalua Fishpond Heritage Center dashboard. Explore data on pH, oxygen levels, temperature, and conductivity. Please note that the site is a work in progress, with ongoing updates to enhance functionality and data availability."
)

show_hide <- actionButton(inputId = "button", label = "About")


ui <- page_sidebar(
  
  theme = bs_theme(bootswatch = "darkly",
                   primary = "#c5a668",),
  
  title = "Maunalua Fishpond Heritage Center Dashboard",
  sidebar = sidebar(
    sidebar_content),
  
  layout_columns(
    
  card(card_header("Output"),
       plotOutput("linePlot",
       dblclick = "linePlot_dblclick",
       brush = brushOpts(
         id = "linePlot_brush",
         resetOnNew = TRUE
       )
      )
  ),
  
  col_widths = c(12) 
 )
)

server <- function(input, output, session) {
  
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  # Update site_specific choices based on selected site
  observe({
    selected_site <- input$site
    updateSelectInput(session, "site_specific", choices = unique(data[data$site == selected_site, "site_specific"]))
  })
  
  observeEvent(input$linePlot_dblclick, {
    brush <- input$linePlot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  output$linePlot <- renderPlot({
    filtered_data <- data %>%
      filter(site == input$site,
             site_specific == input$site_specific,
             variable == input$variable)
    
    ggplot(filtered_data, aes(x = date_time_hst, y = value)) +
      geom_line() +
      labs(title = paste("Line Graph of", input$variable, "at", input$site, "-", input$site_specific),
           x = "Date Time",
           y = input$variable) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  })
}


shinyApp(ui = ui, server = server)
