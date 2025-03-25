# New Zealand Earthquake Dashboard
# Compatible with R 3.6.3

# Load required packages
library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)
library(DT)
library(htmltools)

# Function to fetch earthquake data from GeoNet API
fetch_geonet_data <- function() {
  # URL for GeoNet API - fetches earthquakes for the past 7 days
  url <- "https://api.geonet.org.nz/quake?MMI=3"
  
  # Read and parse JSON data
  earthquake_data <- tryCatch({
    jsonlite::fromJSON(url)$features
  }, error = function(e) {
    # Return empty data frame if API call fails
    message("Error fetching data: ", e$message)
    return(data.frame())
  })
  
  if (length(earthquake_data) == 0 || nrow(earthquake_data) == 0) {
    return(data.frame())
  }
  
  # Extract the properties and geometry
  properties <- earthquake_data$properties
  geometry <- earthquake_data$geometry
  
  # Create a dataframe with the data we need
  earthquakes <- data.frame(
    time = as.POSIXct(properties$time, format = "%Y-%m-%dT%H:%M:%S.%OSZ", tz = "UTC"),
    magnitude = properties$magnitude,
    depth = properties$depth,
    locality = properties$locality,
    mmi = properties$mmi,
    quality = properties$quality,
    latitude = geometry$coordinates[,2],
    longitude = geometry$coordinates[,1],
    stringsAsFactors = FALSE
  )
  
  # Convert time to New Zealand time
  earthquakes$time <- with_tz(earthquakes$time, tzone = "Pacific/Auckland")
  
  # Sort by time (most recent first)
  earthquakes <- earthquakes[order(earthquakes$time, decreasing = TRUE),]
  
  return(earthquakes)
}

# Function to get magnitude color
get_magnitude_color <- function(magnitude) {
  if (magnitude < 1) return("#a4a4a4") # gray
  else if (magnitude < 2) return("#7a59a5") # purple
  else if (magnitude < 3) return("#c25a87") # pink
  else if (magnitude < 4) return("#ea5e58") # red-orange
  else if (magnitude < 5) return("#fa9031") # orange
  else return("#ffd324") # yellow
}

# UI definition
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .card {
        border-radius: 5px;
        padding: 20px;
        margin: 10px;
        color: white;
        text-align: center;
        height: 150px;
      }
      .blue-card {
        background-color: #0099cc;
      }
      .gray-card {
        background-color: #f0f0f0;
        color: #333;
      }
      .card h3 {
        margin-top: 0;
        font-size: 16px;
      }
      .card h2 {
        font-size: 48px;
        margin: 20px 0;
      }
      .magnitude-cell {
        font-weight: bold;
      }
      .map-container {
        border: 1px solid #ddd;
        border-radius: 5px;
      }
    "))
  ),
  titlePanel("Recent Earthquakes in Aotearoa New Zealand"),
  h4("Of Weak Intensity or Greater"),
  
  fluidRow(
    column(width = 3,
      div(class = "card blue-card",
        h3("Hours since last earthquake"),
        uiOutput("hours_since_last")
      )
    ),
    column(width = 3,
      div(class = "card gray-card",
        h3("Earthquakes in the last 24 hours"),
        uiOutput("quakes_24hrs")
      )
    ),
    column(width = 6,
      actionButton("refresh", "Refresh Data", icon = icon("refresh")),
      textOutput("last_updated")
    )
  ),
  
  fluidRow(
    column(width = 6,
      h3("Recent Earthquakes"),
      DTOutput("quake_table")
    ),
    column(width = 6,
      h3("100 Most Recent Earthquakes"),
      div(class = "map-container",
        leafletOutput("quake_map", height = 500)
      )
    )
  ),
  
  fluidRow(
    column(width = 12,
      p("Retrieved from the ", 
        a("GeoNet API", href = "https://api.geonet.org.nz/"),
        textOutput("fetch_time", inline = TRUE)
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Reactive for earthquake data
  quake_data <- reactiveVal(data.frame())
  fetch_time <- reactiveVal(Sys.time())
  
  # Initial data load
  observe({
    data <- fetch_geonet_data()
    quake_data(data)
    fetch_time(Sys.time())
  })
  
  # Refresh data when button is clicked
  observeEvent(input$refresh, {
    data <- fetch_geonet_data()
    quake_data(data)
    fetch_time(Sys.time())
  })
  
  # Hours since last earthquake
  output$hours_since_last <- renderUI({
    data <- quake_data()
    if (nrow(data) == 0) return(h2("N/A"))
    
    time_diff <- difftime(Sys.time(), data$time[1], units = "hours")
    h2(round(as.numeric(time_diff)))
  })
  
  # Earthquakes in last 24 hours
  output$quakes_24hrs <- renderUI({
    data <- quake_data()
    if (nrow(data) == 0) return(h2("N/A"))
    
    last_24hr <- Sys.time() - hours(24)
    count <- sum(data$time >= last_24hr)
    h2(count)
  })
  
  # Last updated time
  output$last_updated <- renderText({
    paste("Last updated:", format(fetch_time(), "%Y-%m-%d %H:%M:%S %Z"))
  })
  
  # Fetch time for API attribution
  output$fetch_time <- renderText({
    paste(" at", format(fetch_time(), "%Y/%m/%d %H:%M %Z"))
  })
  
  # Table of recent earthquakes
  output$quake_table <- renderDT({
    data <- quake_data()
    if (nrow(data) == 0) return(NULL)
    
    # Calculate days ago
    now <- Sys.time()
    data$days_ago <- as.numeric(difftime(now, data$time, units = "days"))
    data$days_text <- paste(round(data$days_ago), "days ago")
    data$time_formatted <- format(data$time, "%H:%M %p")
    
    # Subset and reorder columns for display
    display_data <- data[1:min(10, nrow(data)), c("magnitude", "days_text", "time_formatted", "locality", "depth")]
    colnames(display_data) <- c("Magnitude", "When", "Time", "Location", "Depth (km)")
    
    # Custom rendering of the table
    datatable(
      display_data, 
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE
      )
    ) %>% 
    formatRound("Magnitude", 1) %>%
    formatRound("Depth (km)", 1)
  })
  
  # Map of earthquakes
  output$quake_map <- renderLeaflet({
    data <- quake_data()
    if (nrow(data) == 0) {
      return(leaflet() %>% 
               addTiles() %>% 
               setView(174.7633, -41.2889, zoom = 5))
    }
    
    # Limit to 100 most recent quakes
    data <- data[1:min(100, nrow(data)),]
    
    # Create pop-up content
    popups <- paste0(
      "<strong>Magnitude: ", round(data$magnitude, 1), "</strong><br>",
      "Location: ", data$locality, "<br>",
      "Depth: ", round(data$depth, 1), " km<br>",
      "Time: ", format(data$time, "%Y-%m-%d %H:%M")
    )
    
    # Create the map
    leaflet(data) %>%
      addTiles() %>%
      setView(174.7633, -41.2889, zoom = 5) %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~pmax(4, magnitude * 2),
        color = "black",
        weight = 1,
        opacity = 1.0,
        fillColor = ~sapply(magnitude, get_magnitude_color),
        fillOpacity = 0.7,
        popup = popups
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("#a4a4a4", "#7a59a5", "#c25a87", "#ea5e58", "#fa9031", "#ffd324"),
        labels = c("<1", "1", "2", "3", "4", ">5"),
        title = "Magnitude",
        opacity = 1
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
