library(shiny)

ui <- fluidPage(
  h2("Hello World"),
  p("If you can see this message, the app is working.")
)

server <- function(input, output) {}

shinyApp(ui, server)
