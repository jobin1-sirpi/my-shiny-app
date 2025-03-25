# app.R - Simplified Shiny app compatible with R 3.6.3

# Load required packages
library(shiny)
library(ggplot2)
library(dplyr)

# Generate sample data directly in app.R to avoid file dependency issues
generate_sample_data <- function(months = 24) {
  set.seed(123)
  
  # Generate months
  month_seq <- seq(as.Date("2023-01-01"), by = "month", length.out = months)
  month_labels <- format(month_seq, "%b %Y")
  
  # Generate sales with seasonal pattern
  base_sales <- 10000
  trend <- seq(0, months * 100, by = 100)
  seasonality <- sin(seq(0, 2*pi * (months/12), length.out = months)) * 2000
  sales <- base_sales + trend + seasonality + rnorm(months, 0, 500)
  
  # Generate profits (approximately 30% of sales with some variation)
  profit_margin <- runif(months, 0.25, 0.35)
  profit <- sales * profit_margin
  
  # Generate customers (roughly correlated with sales)
  base_customers <- 500
  customers <- base_customers + (sales / 100) * runif(months, 0.8, 1.2)
  
  # Create data frame
  data.frame(
    month = month_labels,
    date = month_seq,
    sales = round(sales, 2),
    profit = round(profit, 2),
    customers = round(customers, 0),
    stringsAsFactors = FALSE  # Explicit for R 3.6.3 compatibility
  )
}

# Generate sample data
sample_data <- generate_sample_data()

# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("Simple Data Explorer"),
  
  # Sidebar with input controls
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", 
                 "Choose a variable:",
                 choices = c("sales", "profit", "customers"),
                 selected = "sales"),
      
      radioButtons("plotType", 
                  "Plot type:",
                  choices = c("Bar" = "bar",
                             "Line" = "line",
                             "Point" = "point"),
                  selected = "bar")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("distPlot")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Data", tableOutput("dataTable"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Use the sample data
  data <- reactive({
    sample_data
  })
  
  # Create the plot based on user input
  output$distPlot <- renderPlot({
    # Get the data
    df <- data()
    
    # For R 3.6.3 compatibility, not using aes_string
    if (input$variable == "sales") {
      y_val <- df$sales
      y_lab <- "Sales"
    } else if (input$variable == "profit") {
      y_val <- df$profit
      y_lab <- "Profit"
    } else {
      y_val <- df$customers
      y_lab <- "Customers"
    }
    
    # Create plot based on type selected
    if (input$plotType == "bar") {
      ggplot(df, aes(x = month, y = y_val)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste(y_lab, "by Month"),
             x = "Month", y = y_lab)
    } else if (input$plotType == "line") {
      ggplot(df, aes(x = month, y = y_val, group = 1)) +
        geom_line(color = "steelblue", size = 1) +
        geom_point(color = "steelblue", size = 3) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste(y_lab, "by Month"),
             x = "Month", y = y_lab)
    } else {
      ggplot(df, aes(x = month, y = y_val)) +
        geom_point(color = "steelblue", size = 3) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste(y_lab, "by Month"),
             x = "Month", y = y_lab)
    }
  })
  
  # Create a summary of the data
  output$summary <- renderPrint({
    df <- data()
    summary(df[, c("sales", "profit", "customers")])
  })
  
  # Display the data table
  output$dataTable <- renderTable({
    head(data(), 10)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
