if (!require(tidyquant)) install.packages("tidyquant")
if (!require(lubridate)) install.packages("lubridate")
if (!require(dplyr)) install.packages("dplyr")

library(tidyquant)
library(lubridate)
library(dplyr)

all_politician_trade_data <- read.csv("../Data/Scrubbed_All_CapitolTrades_Data.csv", header = TRUE) %>%
  mutate(txDate = as.Date(txDate))

all_stock_trade_data <- read.csv("../Data/Stock_Prices_For_All_CapitolTrades_Tickers.csv", header = TRUE) %>%
  mutate(date = as.Date(date))

calculate_net_positions <- function(trades_df, start_date = NULL, end_date = NULL) {
  trades_df <- trades_df %>%
    filter(if(!is.null(start_date)) txDate >= start_date else TRUE) %>%
    filter(if(!is.null(end_date)) txDate <= end_date else TRUE)
  
  net_positions <- trades_df %>%
    group_by(asset.assetTicker) %>%
    summarise(netPosition = sum(ifelse(txType %in% c("buy", "receive"), value, -value)))
  #receive is equal to a buy, exchange is equal to a sell from a portfolio perspective]
  
  return(net_positions)
}
#sample_net_position <- calculate_net_positions(politician_trade_data, start_date = "2021-03-01", end_date = "2021-03-31")

sum_net_positions <- function(netPosition.x, netPosition.y) {
  # Merge dataframes by assetTicker
  merged_df <- merge(netPosition.x, netPosition.y, by = "asset.assetTicker", all = TRUE, suffixes = c("_1", "_2"))
  
  # Replace NA values with 0
  merged_df[is.na(merged_df)] <- 0
  
  # Calculate sum of net positions
  merged_df$netPosition <- merged_df$netPosition_1 + merged_df$netPosition_2
  
  # Remove unnecessary columns
  merged_df <- merged_df[, c("asset.assetTicker", "netPosition")]
  
  return(merged_df)
}
#sample_sum_net_pos <- sum_net_positions(netPosition.x = sample_net_position, netPosition.y = sample_net_position)

calculate_portfolio_weights_from_net_positions <- function(net_positions_df) {
  
  # Calculate net positions
  net_positions <- net_positions_df %>%
    filter(netPosition > 0) %>%
    filter(!is.na(asset.assetTicker) & asset.assetTicker != "")
  
  # Calculate portfolio weights
  portfolio_weights <- net_positions %>%
    mutate(portfolioWeight = netPosition / sum(netPosition)) %>%
    select(symbol = asset.assetTicker, weight = portfolioWeight)
  
  return(portfolio_weights)
}
#sample_calculated_portfolio_weights <- calculate_portfolio_weights_from_net_positions(sample_sum_net_pos)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

industries = unique(all_politician_trade_data$issuer.sector)[1:11] 
# There is a null value for some reason showing up at end so I index slice it

allTickerMonthlyStockReturns <- all_stock_trade_data %>%
  group_by(symbol) %>%
  tq_transmute(adjusted, periodReturn, period = "monthly")

all_industry_portfolio = data.frame(date = as.Date(character()), portfolio_returns = numeric(), stringsAsFactors = FALSE)

for (industry in industries) {
  industry_name = industry
  print(industry_name)
  industry_df <- all_politician_trade_data %>%
    filter(issuer.sector == industry)
  starting_net_positions <- calculate_net_positions(industry_df, start_date = "2021-01-01", end_date = "2021-06-30")
  #Specify the date range for the analysis
  start_date <- as.Date("2021-07-01")
  end_date <- as.Date("2022-12-31")
  running_net_positions <- starting_net_positions
  monthly_dates <- seq(start_date, end_date, by = "month") %>%
    floor_date(unit = "month") %>%
    unique()
  #Create the empty results dataframe
  results <- data.frame(date = as.Date(character()), portfolio_returns = numeric(), stringsAsFactors = FALSE)
  
  for (i in 1:(length(monthly_dates)-1)) {
    #get the start and end dates for the current month
    month_start <- monthly_dates[i]
    month_end <- monthly_dates[i+1] - days(1)
    
    #filter the stock trades dataframe for the current month
    month_trades <- industry_df %>%
      filter(txDate >= month_start & txDate <= month_end)
    
    #update running net position for the current month
    running_net_positions <- sum_net_positions(running_net_positions, calculate_net_positions(month_trades))
    
    #create portfolio weights for the current month
    month_portfolio_weights <- calculate_portfolio_weights_from_net_positions(running_net_positions)
    
    #create the monthly returns for the current month
    #add tq_get function?
    #pre-create a results table with month_index, and closing dates, use it to handle this months+1
    month_returns <- allTickerMonthlyStockReturns %>%
      filter(date >= month_start & date <= month_end)
    
    month_portfolio_returns_df <- left_join(month_portfolio_weights, month_returns, by = "symbol") %>%
      mutate(portfolio_return = weight * monthly.returns)
    
    month_portfolio_return <- month_portfolio_returns_df %>%
      summarise(portfolio_return = sum(portfolio_return, na.rm = TRUE))
    
    results <- results %>%
      add_row(date = Mode(month_portfolio_returns_df$date), portfolio_returns = month_portfolio_return$portfolio_return)
  }
  if(industry == "industrials"){
    all_industry_portfolio = results
  }else{
    all_industry_portfolio = merge(all_industry_portfolio, results, by = "date")
  }
}

names(all_industry_portfolio)[2:12] = industries #Rename columns


#get the market results
snp_market_results <- tq_get("^GSPC", get="stock.prices", from = start_date , to = end_date) %>%
  tq_transmute(adjusted, periodReturn, period = "monthly")

#append market results to 
all_industry_portfolio <- merge(all_industry_portfolio, snp_market_results, by="date")

all_industry_portfolio <- all_industry_portfolio %>%
  rename(snp_market_returns = monthly.returns)

beta_model <- lm(portfolio_returns ~ snp_market_returns, data=results)
summary(beta_model)

chart.CumReturns(all_industry_portfolio, geometric = TRUE, legend.loc = "topright")

##Should try and figure out how to combine all of these into one dataframe, then you could plot them all. 
