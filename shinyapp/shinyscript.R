knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(here)
library(shiny)
library(bslib)
library(bsicons)
library(plotly)



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
    
    card(card_header(),
         plotlyOutput("linePlot")
                      
  # card(card_header("Output"),
  #      plotOutput("linePlot",
  #      dblclick = "linePlot_dblclick",
  #      brush = brushOpts(
  #        id = "linePlot_brush",
  #        resetOnNew = TRUE
       
      
  ),
  
  col_widths = c(12) 
 )
)

server <- function(input, output, session) {
  
  # Example function for validation
  validate_selection <- function(site_specific, variable) {
    # Define valid variables
    valid_variables <- c("temperature", "oxygen", "pH")
    
    # Check if the selected variable is valid for the given location
    if (!variable %in% valid_variables) {
      stop("Error: The selected variable is not valid. Please select one of: temperature, oxygen, or pH.")
    }
    
    # Custom validation for the 'general' site
    if (site_specific == "General" && variable == "temperature") {
      stop("Please select a specific site.")
    }
    
    # Proceed with your logic if validation passes
    if (site_specific != "General" && variable %in% c("pH", "oxygen")) {
      stop("Only temperature is available for specific sites. Select Temperature or General to view the data.")
    }
    
    return(paste("You selected site:", site_specific, "and variable:", variable))
  }
  
  # Update site_specific choices based on selected site
  observe({
    selected_site <- input$site
    updateSelectInput(session, "site_specific", choices = unique(data[data$site == selected_site, "site_specific"]))
  })
  
  # Render the plot with validation
  output$linePlot <- renderPlotly({
    # Perform validation before proceeding
    tryCatch({
      validate_selection(input$site_specific, input$variable)
      
      # Proceed to filter the data and create the plot
      filtered_data <- data %>%
        filter(site == input$site,
               site_specific == input$site_specific,
               variable == input$variable)
      
      p <- ggplot(filtered_data, aes(x = date_time_hst, y = value)) +
        geom_line(color = "mediumaquamarine") +
        labs(title = paste("Line Graph of", input$variable, "at", input$site, "-", input$site_specific),
             x = "Date Time",
             y = input$variable) +
        theme_bw()
      
      ggplotly(p)
    }, error = function(e) {
      # Display the custom error message as a Shiny notification
      showNotification(e$message, type = "error", duration = 10)
      NULL # Return NULL to prevent rendering
    })
  })
}

shinyApp(ui = ui, server = server)
