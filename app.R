# Simple Shiny app compatible with Posit Connect Cloud
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("New Zealand Earthquake Dashboard"),
  
  fluidRow(
    column(width = 6,
      div(
        style = "background-color: #0099cc; color: white; padding: 20px; border-radius: 5px; text-align: center;",
        h3("Hours since last earthquake"),
        h2("20")
      )
    ),
    column(width = 6,
      div(
        style = "background-color: #f0f0f0; padding: 20px; border-radius: 5px; text-align: center;",
        h3("Earthquakes in the last 24 hours"),
        h2("2")
      )
    )
  ),
  
  br(),
  
  fluidRow(
    column(width = 12,
      h3("Recent Earthquakes"),
      tableOutput("quake_table")
    )
  ),
  
  br(),
  
  fluidRow(
    column(width = 12,
      p("Note: This is a static demonstration. In a full application, this would display real-time data from the GeoNet API."),
      p("Created for Posit Connect Cloud demonstration.")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Create a sample dataset
  sample_quakes <- data.frame(
    Magnitude = c(2.7, 3.6, 2.6, 3.7, 3.5),
    When = c("2 days ago", "2 days ago", "2 days ago", "2 days ago", "3 days ago"),
    Time = c("12:14 PM", "05:01 AM", "02:40 AM", "02:34 AM", "03:35 PM"),
    Location = c("5 km east of Napier", "25 km west of Te Kaha", "15 km west of Masterton", 
                "25 km north-west of Tokomaru Bay", "20 km east of French Pass"),
    Depth = c(27, 46, 16, 49, 41),
    stringsAsFactors = FALSE
  )
  
  # Display the table
  output$quake_table <- renderTable({
    sample_quakes
  })
}

# Run the application
shinyApp(ui = ui, server = server)
