if (!require(tidyquant)) install.packages("tidyquant")
if (!require(lubridate)) install.packages("lubridate")
if (!require(dplyr)) install.packages("dplyr")

library(tidyquant)
library(lubridate)
library(dplyr)
library(plotly)
library(xts)

all_politician_trade_data <- read.csv("Data/Scrubbed_All_CapitolTrades_Data.csv", header = TRUE) %>%
  mutate(txDate = as.Date(txDate))

#filter out any null or "" values of tickers if any
all_politician_trade_data <- all_politician_trade_data[!is.na(all_politician_trade_data$asset.assetTicker), ]
all_politician_trade_data <- all_politician_trade_data[all_politician_trade_data$asset.assetTicker != "", ]

all_stock_trade_data <- read.csv("Data/Stock_Prices_For_All_CapitolTrades_Tickers.csv", header = TRUE) %>%
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

#create a starting portfolio for the first 2 quarters of 2021
starting_net_positions <- calculate_net_positions(all_politician_trade_data, 
                                                  start_date = "2021-03-23", 
                                                  end_date = "2021-08-31")

#allTickerMonthlyStockReturns is used for all portfolio analysis
allTickerMonthlyStockReturns <- all_stock_trade_data %>%
  group_by(symbol) %>%
  tq_transmute(adjusted, periodReturn, period = "monthly")

#Specify the date range for the analysis
start_date <- as.Date("2021-09-01")
end_date <- as.Date("2024-02-29")
running_net_positions <- starting_net_positions

monthly_dates <- seq(start_date, end_date, by = "month") %>%
  floor_date(unit = "month") %>%
  unique()

#Create the empty results dataframe
results <- data.frame(date = as.Date(character()), portfolio_returns = numeric(), stringsAsFactors = FALSE)

#Mode date function for some reason
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Iterate over each month
for (i in 1:(length(monthly_dates)-1)) {
  #get the start and end dates for the current month
  month_start <- monthly_dates[i]
  month_end <- monthly_dates[i+1] - days(1)
  
  #filter the stock trades dataframe for the current month
  month_trades <- all_politician_trade_data %>%
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
    add_row(date = Mode(month_portfolio_returns_df$date[1]), portfolio_returns = month_portfolio_return$portfolio_return)
}

#get the market results
snp_market_results <- tq_get("^GSPC", get="stock.prices", from = start_date , to = end_date) %>%
  tq_transmute(adjusted, periodReturn, period = "monthly")

#append market results to 
results <- merge(results, snp_market_results, by="date")

results <- results %>%
  rename(snp_market_returns = monthly.returns)

beta_model <- lm(portfolio_returns ~ snp_market_returns, data=results)
summary(beta_model)

jpeg(file = "Visualizations/Overall Portfolio Returns.png")
chart.CumReturns(results, 
                 geometric = TRUE, 
                 legend.loc = "topright", 
                 #plot.engine = "plotly",
                 main = "Overall Cumulative Returns")
dev.off()

#convert to timeseries from dataset
results_portfolio_returns_xts <- xts(x = results$portfolio_returns, order.by = results$date)
results_snp_market_returns_xts <- xts(x = results$snp_market_returns, order.by = results$date)

#get cumulative returns
portfolio_cum_returns <- Return.cumulative(results_portfolio_returns_xts, geometric = TRUE)
paste("Cumulative returns for overall portfolio is:", round(portfolio_cum_returns*100,2),"%")

snp_market_cum_returns <- Return.cumulative(results_snp_market_returns_xts, geometric = TRUE)
paste("Cumulative returns for S&P market is:", round(snp_market_cum_returns*100,2),"%")

#generate drawdown table and chart
table.Drawdowns(results_portfolio_returns_xts,top=5,digits=3)
chart.Drawdown(results_portfolio_returns_xts, 
               main = "Drawdowns for Portfolio Returns", 
               plot.engine = "plotly")


chart.CumReturns(results, geometric = TRUE, legend.loc = "top", main = "Cumulative Returns of Politicians vs the S&P 500")
#dev.off()
