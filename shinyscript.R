library(shiny)
#runExample("01_hello")


ui <- fluidPage(
  titlePanel("Maunalua Fishpond Heritage Center Dashboard"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      h1("Place filter here"),
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "#007bc2", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
  })
  
}

shinyApp(ui = ui, server = server)

