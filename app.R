# File: app.R
library(shiny)

# Define UI
ui <- fluidPage(
  # App title
  titlePanel("Hello, Posit Connect!"),
  
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      # Input: Slider for the number of observations
      sliderInput("obs", 
                  "Number of observations:", 
                  min = 1,
                  max = 1000, 
                  value = 500)
    ),
    
    # Main panel for outputs
    mainPanel(
      # Output: Histogram
      plotOutput("distPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Generate histogram of random normal distributions
  output$distPlot <- renderPlot({
    # Generate random numbers from a normal distribution
    x <- rnorm(input$obs)
    
    # Create a histogram
    hist(x, 
         breaks = 30, 
         col = 'skyblue', 
         main = "Histogram of Random Normal Distribution",
         xlab = "Value")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
