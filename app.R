# app.R - Main application file

# Load required packages
library(shiny)
library(ggplot2)
library(dplyr)

# Define UI
ui <- fluidPage(
  # Custom CSS
  includeCSS("www/styles.css"),
  
  # Application title
  titlePanel(
    div(
      img(src = "logo.png", height = 50),
      "Simple Data Explorer"
    )
  ),
  
  # Sidebar with input controls
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", 
                 "Choose a variable:",
                 choices = c("Sales", "Profit", "Customers"),
                 selected = "Sales"),
      
      radioButtons("plotType", 
                  "Plot type:",
                  choices = c("Bar" = "bar",
                             "Line" = "line",
                             "Point" = "point"),
                  selected = "bar"),
      
      sliderInput("bins",
                 "Number of bins:",
                 min = 5,
                 max = 50,
                 value = 20)
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("distPlot")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Data", dataTableOutput("dataTable"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Load data
  data <- reactive({
    read.csv("data/sample_data.csv")
  })
  
  # Create the plot based on user input
  output$distPlot <- renderPlot({
    # Get the data
    df <- data()
    
    # Select the variable based on user input
    x_var <- switch(input$variable,
                   "Sales" = df$sales,
                   "Profit" = df$profit,
                   "Customers" = df$customers)
    
    # Create plot based on type selected
    if (input$plotType == "bar") {
      ggplot(df, aes_string(x = "month", y = input$variable)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_minimal() +
        labs(title = paste(input$variable, "by Month"),
             x = "Month", y = input$variable)
    } else if (input$plotType == "line") {
      ggplot(df, aes_string(x = "month", y = input$variable, group = 1)) +
        geom_line(color = "steelblue", size = 1) +
        geom_point(color = "steelblue", size = 3) +
        theme_minimal() +
        labs(title = paste(input$variable, "by Month"),
             x = "Month", y = input$variable)
    } else {
      ggplot(df, aes_string(x = "month", y = input$variable)) +
        geom_point(color = "steelblue", size = 3) +
        theme_minimal() +
        labs(title = paste(input$variable, "by Month"),
             x = "Month", y = input$variable)
    }
  })
  
  # Create a summary of the data
  output$summary <- renderPrint({
    df <- data()
    summary(df[, c("sales", "profit", "customers")])
  })
  
  # Display the data table
  output$dataTable <- renderDataTable({
    data()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
