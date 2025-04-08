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
library(DBI)
library(duckdb)
library(ellmer)
library(shinychat)
library(promises)

openai_model <- "gpt-4o"

# Load the original fishpond image
pond_image <- image_read("kanewai_aerial.png")
pond_image_raster <- as.raster(pond_image)  # Convert to raster format

# Define coordinates for each sensor location
sensor_data <- data.frame(
  site_specific = c("Norfolk", "Shade", "Auwai", "RockWall", "Rock", "Springledge"),
  x = c(0.5, 4, 5.2, 6.4, 5.5, 5.75),
  y = c(0.5, 8.4, 9, 8.5, 6.5, 3.45),
  radius = rep(0.3, 6)
)

# Read in variable data and merge with sensor data
data <- read_csv(here("cleaned_data/master_data_pivot.csv"))

conn <- dbConnect(duckdb(), dbdir = here("fishpond.duckdb"), read_only = TRUE)
# Close database when app stops
onStop(\() dbDisconnect(conn))

system_prompt_str <- paste0(
  "You are an AI assistant analyzing fishpond sensor data in DuckDB. The 'sensor_data' dataset contains temperature and environmental readings from Kanewai fishpond. Answer only questions about the data using SQL compatible with DuckDB.")



# Chatbot greeting
greeting <- paste0(
  "👋 **Welcome to the Fishpond Sensor Chatbot!** 🌊\n\n",
  "I can help you explore and analyze sensor data from Kanewai Fishpond.\n\n",
  "**Ask me questions about:**\n\n",
  "✅ **Sensor readings**\n",
  "   *(e.g., \"What was the water temperature at Norfolk yesterday?\")*\n\n",
  "✅ **Trends over time**\n",
  "   *(e.g., \"Show me the temperature trends for the past week.\")*\n\n",
  "✅ **Comparisons**\n",
  "   *(e.g., \"Compare oxygen levels at RockWall and Shade.\")*\n\n",
  "✅ **Summarized insights**\n",
  "   *(e.g., \"What is the average temperature across all sensors?\")*\n\n",
  "To get started, type your question below! 🏝️"
)




dbListTables(conn)


# Shiny app UI
ui <- fluidPage(
  titlePanel("Fishpond Sensor Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      width = 4,  # Adjust sidebar width
      chat_ui("chat", height = "400px", fill = TRUE),  # Chatbot UI
      airDatepickerInput(
        "date_time_hst",
        "Select Date and Time:",
        minDate = min(data$date_time_hst),
        maxDate = max(data$date_time_hst),
        value = min(data$date_time_hst),
        timepicker = TRUE
      ),
      selectInput("variable", "Select Variable:", choices = unique(data$variable)),
      sliderInput("alpha_selected", "Transparency (Selected Sensor):", 
                  min = 0, max = 1, value = 0.6, step = 0.1),
      sliderInput("alpha_unselected", "Transparency (Unselected Sensors):", 
                  min = 0, max = 1, value = 0.3, step = 0.1)
    ),
    mainPanel(
      withSpinner(plotOutput("pondImagePlot", click = "map_click", width = "100%", height = "600px")),
      uiOutput("sensorPlots")
    )
  )
)


# Shiny app server logic
server <- function(input, output, session) {
  # Reactive data filtered by date and variable
  filtered_data <- reactive({
    dbGetQuery(conn, paste0(
      "SELECT * FROM sensor_data WHERE date_time_hst = '", input$date_time_hst, "' 
      AND variable = '", input$variable, "'"
    )) %>%
      right_join(sensor_data, by = "site_specific")
  })
  
  # Highlight clicked sensor
  clicked_sensor <- reactiveVal(NULL)
  observeEvent(input$map_click, {
    clicked <- nearPoints(sensor_data, input$map_click, xvar = "x", yvar = "y", maxpoints = 1)
    if (nrow(clicked) > 0) {
      clicked_sensor(clicked$site_specific)
    }
  })
  
  # Render the pond map with sensors
  output$pondImagePlot <- renderPlot({
    pond_data <- filtered_data()
    
    ggplot() +
      annotation_raster(pond_image_raster, xmin = 0, xmax = 10, ymin = 0, ymax = 10) +
      geom_circle(
        data = pond_data,
        aes(x0 = x, y0 = y, r = radius, fill = value),
        color = "black",
        alpha = 0.7
      ) +
      scale_fill_viridis_c(name = "Temperature (°C)", option = "C") +
      coord_fixed() +
      labs(title = "Pond Sensor Locations Colored by Value", x = "Pond X-Coordinate", y = "Pond Y-Coordinate") +
      theme_minimal() +
      theme(legend.position = "right") +
      geom_text(
        data = pond_data,
        aes(x = x, y = y + 0.40, label = site_specific),
        color = "white",
        size = 4,
        fontface = "bold"
      )
  })
  
  # Render dynamic sensor-specific plots
  output$sensorPlots <- renderUI({
    if (is.null(clicked_sensor())) {
      return(h3("Click on a sensor on the map to view its data."))
    }
    
    sensor_name <- clicked_sensor()
    withSpinner(plotlyOutput(paste0("plot_", sensor_name)))
  })
  
  observe({
    sensor_name <- clicked_sensor()
    if (!is.null(sensor_name)) {
      output[[paste0("plot_", sensor_name)]] <- renderPlotly({
        sensor_data_combined <- dbGetQuery(conn, paste0(
          "SELECT * FROM sensor_data WHERE site_specific = '", sensor_name, "'"
        ))
        
        line_plot <- ggplot(sensor_data_combined) +
          geom_line(aes(x = date_time_hst, y = value, color = site_specific), alpha = input$alpha_selected, size = 0.4) +
          scale_color_manual(values = c("Norfolk" = "blue", "Shade" = "red", 
                                        "Auwai" = "green", "RockWall" = "purple", 
                                        "Rock" = "orange", "Springledge" = "brown")) +
          labs(title = paste("Data for Sensor:", sensor_name), x = "Date and Time", y = paste(input$variable, "(units)")) +
          theme_minimal()
        
        ggplotly(line_plot)
      })
    }
  })
  
  # ✨ Sidebot ✨ -------------------------------------------------------------
  
  append_output <- function(...) {
    txt <- paste0(...)
    shinychat::chat_append_message(
      "chat",
      list(role = "assistant", content = txt),
      chunk = TRUE,
      operation = "append",
      session = session
    )
  }
  
  #' Modifies the data presented in the data dashboard, based on the given SQL
  #' query, and also updates the title.
  #' @param query A DuckDB SQL query; must be a SELECT statement.
  #' @param title A title to display at the top of the data dashboard,
  #'   summarizing the intent of the SQL query.
  update_dashboard <- function(query, title) {
    append_output("\n```sql\n", query, "\n```\n\n")
    
    tryCatch(
      {
        # Try it to see if it errors; if so, the LLM will see the error
        dbGetQuery(conn, query)
      },
      error = function(err) {
        append_output("> Error: ", conditionMessage(err), "\n\n")
        stop(err)
      }
    )
    
    if (!is.null(query)) {
      current_query(query)
    }
    if (!is.null(title)) {
      current_title(title)
    }
  }
  
  #' Perform a SQL query on the data, and return the results as JSON.
  #' @param query A DuckDB SQL query; must be a SELECT statement.
  #' @return The results of the query as a JSON string.
  query <- function(query) {
    # Do this before query, in case it errors
    append_output("\n```sql\n", query, "\n```\n\n")
    
    tryCatch(
      {
        df <- dbGetQuery(conn, query)
      },
      error = function(e) {
        append_output("> Error: ", conditionMessage(e), "\n\n")
        stop(e)
      }
    )
    
    tbl_html <- df_to_html(df, maxrows = 5)
    append_output(tbl_html, "\n\n")
    
    df |> jsonlite::toJSON(auto_unbox = TRUE)
  }
  
  # Preload the conversation with the system prompt. These are instructions for
  # the chat model, and must not be shown to the end user.
  chat <- chat_openai(model = openai_model, system_prompt = system_prompt_str)
  chat$register_tool(tool(
    update_dashboard,
    "Modifies the data presented in the data dashboard, based on the given SQL query, and also updates the title.",
    query = type_string("A DuckDB SQL query; must be a SELECT statement."),
    title = type_string("A title to display at the top of the data dashboard, summarizing the intent of the SQL query.")
  ))
  chat$register_tool(tool(
    query,
    "Perform a SQL query on the data, and return the results as JSON.",
    query = type_string("A DuckDB SQL query; must be a SELECT statement.")
  ))
  
  # Prepopulate the chat UI with a welcome message that appears to be from the
  # chat model (but is actually hard-coded). This is just for the user, not for
  # the chat model to see.
  chat_append("chat", greeting)
  
  # Handle user input
  observeEvent(input$chat_user_input, {
    # Add user message to the chat history
    chat_append("chat", chat$stream_async(input$chat_user_input)) %...>% {
      # print(chat)
    }
  })
}

df_to_html <- function(df, maxrows = 5) {
  df_short <- if (nrow(df) > 10) head(df, maxrows) else df
  
  tbl_html <- capture.output(
    df_short |>
      xtable::xtable() |>
      print(type = "html", include.rownames = FALSE, html.table.attributes = NULL)
  ) |> paste(collapse = "\n")
  
  if (nrow(df_short) != nrow(df)) {
    rows_notice <- glue::glue("\n\n(Showing only the first {maxrows} rows out of {nrow(df)}.)\n")
  } else {
    rows_notice <- ""
  }
  
  paste0(tbl_html, "\n", rows_notice)
}

shinyApp(ui, server)
