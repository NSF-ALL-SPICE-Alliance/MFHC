knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(here)
library(shiny)
library(bslib)
library(bsicons)
library(plotly)


#THIS IS THE CURRENT WORKING MFHC APP/DASHBOARD

data <- read_csv(here("cleaned_data/master_data_pivot.csv"))




sidebar_content <- list(
  selectInput("site", "Select Site:", choices = unique(data$site)),
  selectInput("site_specific", "Select Site Specific:", choices = NULL),
  selectInput("variable", "Select Variable:", choices = unique(data$variable)),
  conditionalPanel(
    condition = "input.variable == 'temperature'",
    radioButtons(
      inputId = "temp_unit",
      label = "Select Temperature Unit:",
      choices = c("Celsius" = "C", "Fahrenheit" = "F"),
      selected = "C"
    )
  ),
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
  # Function to validate selection
  validate_selection <- function(site_specific, variable) {
    valid_variables <- c("temperature", "oxygen", "pH")
    if (!variable %in% valid_variables) {
      stop("Error: The selected variable is not valid. Please select one of: temperature, oxygen, or pH.")
    }
    if (site_specific == "General" && variable == "temperature") {
      stop("Please select a specific site.")
    }
    if (site_specific != "General" && variable %in% c("pH", "oxygen")) {
      stop("Only temperature is available for specific sites. Select Temperature or General to view the data.")
    }
    return(TRUE)
  }
  
  # Update site_specific choices based on the selected site
  observe({
    selected_site <- input$site
    updateSelectInput(session, "site_specific", choices = unique(data[data$site == selected_site, "site_specific"]))
  })
  
  # Reactive function to filter and convert data
  filtered_data <- reactive({
    req(input$site, input$site_specific, input$variable)
    
    validate_selection(input$site_specific, input$variable)
    
    # Filter the data
    filtered <- data %>%
      filter(site == input$site,
             site_specific == input$site_specific,
             variable == input$variable)
    
    # Convert temperature if applicable
    if (input$variable == "temperature" && input$temp_unit == "F") {
      filtered$value <- filtered$value * 9 / 5 + 32
    }
    
    filtered
  })
  
  # Render the plot
  output$linePlot <- renderPlotly({
    data <- filtered_data()
    unit <- ifelse(input$variable == "temperature" && input$temp_unit == "F", "°F", "°C")
    
    # Plot using ggplot
    p <- ggplot(data, aes(x = date_time_hst, y = value)) +
      geom_line(color = "mediumaquamarine") +
      labs(
        title = paste("Line Graph of", input$variable, "at", input$site, "-", input$site_specific),
        x = "Date Time",
        y = paste(input$variable, "(", unit, ")")
      ) +
      theme_bw()
    
    ggplotly(p)
  })
}


shinyApp(ui = ui, server = server)