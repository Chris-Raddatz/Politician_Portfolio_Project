if (!require(httr)) install.packages("httr")
library(httr)

if (!require(dplyr)) install.packages("dyplr")
library(dplyr)

if (!require(tidyr)) install.packages("tidyr")
library(tidyr)

if (!require(jsonlite)) install.packages("jsonlite")
library(jsonlite)

capitolTradesurl <- "https://bff.capitoltrades.com/trades"

# Make a GET request to the API
response <- GET(capitolTradesurl)

# Check if the request was successful (status code 200)
if (http_status(response)$category == "Success") {
  # Parse the JSON response
  data <- fromJSON(content(response, "text"))
} else {
  # If the request was not successful, print the error message
  print(paste("Error:", http_status(response)$reason))
}

print(data$data)

simpleTradeData <- data$data %>%
  select(`_txId`,txDate, txType, asset, politician, price, size, reportingGap) %>%
  unnest(c(asset, politician))

print(simpleTradeData)
