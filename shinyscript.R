library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
      .navbar-default {
        background-color: #007bc2;
        border-color: #007bc2;
      }
      .navbar-default .navbar-brand {
        color: white;
      }
      ")
    )
  ),
  titlePanel("Maunalua Fishpond Heritage Center Dashboard", windowTitle = "Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Select_Variable", label = "Select Variable", choices = NULL, selected = "pH"),
      selectInput("Select_Site", label = "Select Site", choices = NULL, selected = "Kanewai"),
      selectInput("Select_Site_Specific", label = "Select Specific Site", choices = NULL),
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  ),
  style = "background-color: #f0f0f0;"
)

# Define server logic
server <- function(input, output) {
  all_data_pivot <- readr::read_csv(here::here("cleaned_data/master_data_pivot.csv"))
  
  observe({
    unique_site <- unique(all_data_pivot$site)
    updateSelectInput(session, "Select_Site", choices = unique_site)
  })
  
  observe({
    unique_site_specific <- unique(all_data_pivot$site_specific)
    updateSelectInput(session, "Select_Site_Specific", choices = unique_site_specific)
  })
  
  observe({
    unique_variable <- unique(all_data_pivot$variable)
    updateSelectInput(session, "Select_Variable", choices = unique_variable)
  })
  
  filtered_data <- reactive({
    all_data_pivot_pivot_pivot %>% 
      filter(variable == input$Select_Variable,
             site == input$Select_Site,
             site_specific == input$Select_Site_Specific)
  })
  
  output$plot <- renderPlotly({
    a <- ggplot(filtered_data(), aes(date_time_hst, value)) +
      geom_line(color = "mediumaquamarine") +
      labs(x = "Date Time", y = paste(input$Select_Variable), title = paste(input$Select_Variable)) +
      theme_minimal()
    
    if (input$Select_Variable == "dissolved_oxygen_mg_l") {
      a <- a + geom_hline(yintercept = 2, linetype = "dashed", color = "red", linewidth = 0.5) +
        annotate(geom = "text", x = as.POSIXct("2023-02-18 21:10:00"), y = 2.5, label = "hypoxic", colour = "red")
    }
    
    ggplotly(a)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
