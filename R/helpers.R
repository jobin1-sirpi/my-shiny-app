# R/helpers.R - Helper functions

# Calculate month-over-month growth percentage
calculate_mom_growth <- function(values) {
  c(NA, diff(values) / lag(values)[-1])
}

# Calculate year-over-year growth percentage
calculate_yoy_growth <- function(values, period = 12) {
  growth <- numeric(length(values))
  for (i in (period+1):length(values)) {
    growth[i] <- (values[i] - values[i-period]) / values[i-period]
  }
  return(growth)
}

# Generate sample data for testing
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
    customers = round(customers, 0)
  )
}

# Save sample data
save_sample_data <- function() {
  data <- generate_sample_data()
  dir.create("data", showWarnings = FALSE)
  write.csv(data, "data/sample_data.csv", row.names = FALSE)
  return(data)
}
