library(tidyquant)
library(lubridate)
library(httr)
library(dplyr)
library(jsonlite)
library(tidyr)
library(purrr)

#Dedicated file to fetch and cleanse data from capitoltrades and other finance sources, and store data in ../Data

#fetches all available trades data from CapitolTrades.com
fetchAllCapitolTradesData <- function() {

  base_url <- "https://bff.capitoltrades.com/trades"
  page <- 1
  all_data <- list()
  total_pages <- 2 # Placeholder, will update dynamically
  
  # Loop through pages until the current page exceeds total pages
  while (page <= total_pages) {
    # Make API request
    url <- sprintf("%s?pageSize=96&page=%d", base_url, page)
    response <- GET(url)
    
    # Check if the request was successful
    if (status_code(response) == 200) {
      # Parse the JSON response
      data <- fromJSON(content(response, "text"))
      
      # Update total_pages based on the response
      total_pages <- data$meta$paging$totalPages
      
      # Extract the transactions data
      transactions <- data$data
      
      # Check if transactions is empty, indicating end of data
      if (length(transactions) == 0) {
        break # Exit loop if there are no transactions
      }
      
      # Add the current page's transactions to all_data
      all_data <- c(all_data, list(transactions))
      
      # Move to the next page
      page <- page + 1
      } else {
        cat("Failed to fetch data. HTTP status code:", status_code(response), "\n")
        break # Exit loop in case of an HTTP error
      }
  }
  
  all_transactions_df <- bind_rows(all_data) %>%
    mutate(committees = map_chr(committees, paste, collapse = ", ")) %>%
    unnest_longer(c(asset, politician, issuer)) %>%
    select(`_txId`, txDate, txType, asset, politician, committees, price, value, issuer, size, reportingGap)
  
  return(all_transactions_df)
}

#call function
allCapitolTradesData <- fetchAllCapitolTradesData()

#for consistency, write to disk and read back in with each run.  Or just read back in.
write.csv(allCapitolTradesData, "../Data/All_CapitolTrades_Data.csv", row.names = FALSE)
allCapitolTradesData <- read.csv("../Data/All_CapitolTrades_Data.csv")

# Function to update each ticker in the dataset
update_ticker <- function(ticker_with_suffix, changed_ticker_mapping) {
  # Remove the suffix and replace "/" with "-" to match the mapping keys
  ticker_corrected <- gsub("-", "/", substr(ticker_with_suffix, 1, nchar(ticker_with_suffix) - 3))
  
  # If this modified ticker is in the mapping, return the new value; otherwise, return the original without modification
  if(any(ticker_corrected %in% names(changed_ticker_mapping))) {
    return(changed_ticker_mapping[ticker_corrected])
  } else {
    # If not in mapping, assume the original ticker (without the last 3 characters) is still valid
    return(ticker_corrected)
  }
}

#scrubs the trades to include only US-issued stocks, and adjusts for changed tickers
scrubCapitolTradesData <- function(capitolTradesDatadf){
  #scrub for stocks only
  capitolTradesDatadf <- capitolTradesDatadf[capitolTradesDatadf$asset.assetType == "stock",]
  
  #scrub for only US issues
  capitolTradesDatadf <- capitolTradesDatadf[(capitolTradesDatadf$issuer.country == "us"),]
  
  #change txDate to date type
  capitolTradesDatadf$txDate <- as.Date(capitolTradesDatadf$txDate, format = "%Y-%m-%d")
  
  #scrub to reorder
  capitolTradesDatadf <- capitolTradesDatadf[order(capitolTradesDatadf$txDate, decreasing = FALSE),]

  #some tickers have changed
  changed_ticker_mapping <- c(ORCC = "OBDC", CWEN.A = "CWEN-A", HHC = "HHH", MTBC = "CCLD",
                    MLHR = "MLKN", RLGY = "HOUS", ENOB = "RENB", VIAC = "PARA", 
                    "ETWO-WS" = "ETWO", "LGF/A" = "LGF-A", "BRK/B"="BRK-B", "BF/A"="BF-A",
                    "BF/B" = "BF-B", "LGF/B" = "LGF-B", "LEN/B" = "LEN-B")

  capitolTradesDatadf$asset.assetTicker <- update_ticker(capitolTradesDatadf$asset.assetTicker, changed_ticker_mapping)
  return(capitolTradesDatadf)
}

scrubbedCapitolTradesData <- scrubCapitolTradesData(allCapitolTradesData)


#Gets all stock trades for the tickers contained in the CapitolTrades DataFrame that is passed in
#This can take some time to run
getStockPricesForTickers <- function(anyCapitolTradesdf){
  #get unique tickers to get data for
  tickers <- unique(anyCapitolTradesdf$asset.assetTicker)
  #get the earliest and latest date in the dataframe so we can get the entire month of stock returns to calculate return
  # Find the earliest and latest dates
  earliest_date <- min(anyCapitolTradesdf$txDate, na.rm = TRUE)
  latest_date <- max(anyCapitolTradesdf$txDate, na.rm = TRUE)
  
  # Extract the first day of the month of the earliest date
  first_day_of_first_month <- as.Date(format(earliest_date, "%Y-%m-01"))
  
  # Get the last day of the last month
  last_day_of_last_month <- ceiling_date(latest_date, "month") - days(1)
  
  from_date <- first_day_of_first_month
  to_date <- last_day_of_last_month
  
  stock_data <- tq_get(tickers,
                        get  = "stock.prices",
                        from = from_date,
                        to   = to_date  )
  return(stock_data)
}

stockPricesForTickers <- getStockPricesForTickers(scrubbedCapitolTradesData)
write.csv(stockPricesForTickers, file = "../Data/Stock_Prices_For_All_CapitolTrades_Tickers.csv")

removeTradesWithoutStockPrices <- function(anyCapitolTradesdf, anyStockPricesdf){
  #get unique tickers from both datasets
  unique_prices_tickers <- unique(anyStockPricesdf$symbol)
  unique_trades_tickers <- unique(anyCapitolTradesdf$asset.assetTicker)
  #find the difference between the two
  ticker_diff <- setdiff(unique_prices_tickers,unique_trades_tickers)
  #edit first dataset to filter the data 
  anyCapitolTradesdf <- anyCapitolTradesdf %>%
    filter(!(asset.assetTicker %in% ticker_diff))
  return(anyCapitolTradesdf)
}

finalScrubbedTradesData <- removeTradesWithoutStockPrices(scrubbedCapitolTradesData, stockPricesForTickers)
write.csv(finalScrubbedTradesData, file = "../Data/Scrubbed_All_CapitolTrades_Data.csv")
