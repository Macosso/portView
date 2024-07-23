library(tidyverse)
library(readr)
library(readxl)
library(openxlsx)


account_statement <- paste0("data/",sort(list.files("data", "etoro-account-sta"), decreasing = TRUE)[1])



#activity <- read_excel(account_statement, sheet = "Account Activity")
#dividends <- read_excel(account_statement, sheet = "Dividends")




get_closed_positions <- function(account_statement){
  
  closed_positions <- read_excel(account_statement, sheet = "Closed Positions")
  
  closed_positions <- closed_positions |>
    rename("position_id" = "Position ID",
           "long_short" = "Long / Short",
           "open_date" = "Open Date",
           "close_date" = "Close Date",
           "spread_fees" = "Spread Fees (USD)",
           "market_spread" = "Market Spread (USD)",
           "profit_usd" = "Profit(USD)",
           "open_price" = "Open Rate",
           "close_price" = "Close Rate",
           "take_profit_price" = "Take profit rate",
           "stop_loss_price" = "Stop lose rate",
           "roll_over_fees_dividends" = "Overnight Fees and Dividends",
           "copy_from" = "Copied From") |>
    mutate(symbol = str_extract(string = Action, pattern = "(?<=\\().*(?=\\))"),
           symbol = word(symbol, -1, sep = "\\("),
           symbol = ifelse(Type == "Crypto", paste0(symbol, "-", "USD"), symbol),
           open_date = as.POSIXct(strptime(open_date , format = "%d/%m/%Y %H:%M:%S")),
           close_date = as.POSIXct(strptime(close_date, format = "%d/%m/%Y %H:%M:%S")))
  
  return(closed_positions)
}
  



get_open_positions <- function(account_statement){
  
  activity <- read_excel(account_statement, sheet = "Account Activity")
  
  activity <- activity |>
    filter(Type == "Open Position") |>
    rename("asset_type" = "Asset type",
           "position_id" = "Position ID",
           "realized_equity_change" = "Realized Equity Change",
           "realized_equity" = "Realized Equity") |> 
    mutate(symbol = word(Details, 1, sep = "/"),
           currency = word(Details, 2, sep = "/"),
           open_price = as.numeric(Amount)/as.numeric(Units),
           symbol = ifelse(asset_type == "Crypto", paste0(symbol, "-", currency), symbol),
           execution_date_time = as.POSIXct(strptime(Date, format = "%d/%m/%Y %H:%M:%S")),
           Date = as.Date(execution_date_time),
           time =  strftime(execution_date_time, format="%H:%M:%S"))
    
  
  return(activity)
}




get_deposits <- function(account_statement){
  
  activity <- read_excel(account_statement, sheet = "Account Activity")
  
  activity <- activity |>
    filter(Type == "Deposit") |>
    rename("asset_type" = "Asset type",
           "position_id" = "Position ID",
           "realized_equity_Change" = "Realized Equity Change",
           "realized_equity" = "Realized Equity") |> 
    mutate(execution_date_time = as.POSIXct(Date, format = "%d/%m/%Y %H:%M:%S"),
           Date = as.Date(execution_date_time),
           time =  strftime(execution_date_time, format="%H:%M:%S")) %>%
    select(-c(Units, asset_type,position_id, ))
  
  
  return(activity)
}


get_withdraw <- function(account_statement){
  
  activity <- read_excel(account_statement, sheet = "Account Activity")
  
  activity <- activity |>
    filter(grepl("Withdraw", Type)) |>
    rename("asset_type" = "Asset type",
           "position_id" = "Position ID",
           "realized_equity_Change" = "Realized Equity Change",
           "realized_equity" = "Realized Equity") |> 
    mutate(execution_date_time = as.POSIXct(Date, format = "%d/%m/%Y %H:%M:%S"),
           Date = as.Date(execution_date_time),
           time =  strftime(execution_date_time, format="%H:%M:%S")) %>%
    select(-c(Units, asset_type,position_id, ))
  
  
  return(activity)
}





get_dividends <- function(account_statement){
  
  dividends <- read_excel(account_statement, sheet = "Dividends")
  
  dividends <- dividends |>
    rename("position_id" = "Position ID",
           "net_dividend" = "Net Dividend Received (USD)",
           "dividend_tax_rate" = "Withholding Tax Rate (%)",
           "Date" = "Date of Payment",
           "dividend_tax_payment" = "Withholding Tax Amount (USD)",
           "instrument_name" = "Instrument Name") |> 
    mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
           dividend_tax_rate = as.numeric(gsub(" %", "", dividend_tax_rate))/100)
  
  
  return(dividends)
}


#
expand_dates <- function(df) {
  dta1 <- df %>%
    # Replace NA in 'close' with the current date
    mutate(close_date = as.Date(ifelse(is.na(close_date), Sys.Date(), close_date)),
           open_date = as.Date(ifelse(is.na(open_date), Date, open_date))) %>%
    # Expand each row to include a sequence of dates from open to close
    rowwise() %>%
    do({
      data.frame(date = seq.Date(from = .$open_date, to = .$close_date, by = "day"),
                 position_id = .$position_id)
    })%>%
    ungroup() %>%
    left_join(df, by = "position_id")
  
  return(dta1)
}


