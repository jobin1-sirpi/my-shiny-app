# Minimal Shiny app for testing deployment
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Hello Posit Connect"),
  mainPanel(
    h3("Simple Test App"),
    p("If you can see this, the app deployed successfully!"),
    verbatimTextOutput("serverInfo")
  )
)

# Define server
server <- function(input, output, session) {
  output$serverInfo <- renderPrint({
    # Print R version and session info
    paste("R version:", R.version.string)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
