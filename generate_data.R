# generate_data.R - Script to generate sample data
# Run this script once to create the sample data CSV file

# Source helpers containing the data generation function
source("R/helpers.R")

# Generate and save sample data
data <- save_sample_data()

# Display first few rows of the generated data
print(head(data))
cat("\nSample data has been saved to data/sample_data.csv\n")
