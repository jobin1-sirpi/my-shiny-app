# global.R - Global variables and functions
# This file runs once when the app starts

# Load required packages
library(shiny)
library(ggplot2)
library(dplyr)

# Define global constants
APP_NAME <- "Simple Data Explorer"
APP_VERSION <- "1.0.0"

# Define helper functions
format_currency <- function(x) {
  paste0("$", format(x, big.mark = ",", scientific = FALSE))
}

format_percent <- function(x) {
  paste0(round(x * 100, 1), "%")
}

# You can also source helper functions from external files
source("R/helpers.R")
