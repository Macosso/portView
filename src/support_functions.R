library(tidyverse)
library(ggthemes)

get_prices_long <- function(x){
  df <- data.frame(date = index(x),coredata(x)) |>
    pivot_longer(!date, values_to = "price", names_to = "symbol")
  
  return(df)
}



forex <- na.locf(quantR::loadTokens(c("EURUSD=X", "GBPUSD=X", "HKD=X", "BTC-USD"), start = "2020-01-01"))
forex <- forex[,colnames(forex)[colnames(forex) != "BTC-USD"]]
forex <- data.frame(date = index(forex), coredata(forex))

merge_acc_prices <- function(open_closed, long_df_prices, forex_df){
  
  df <- open_closed |> mutate(date = as.Date(date),
                              symbol = gsub("-", ".", symbol)) |> 
                                left_join(long_df_prices |> mutate(date = as.Date(date)), by = c("date", "symbol")) |>
    left_join(forex_df, by = "date") |>
    group_by(position_id) |>
    mutate(open_price = case_when(currency == "EUR" ~ (Amount/EURUSD.X[which.min(date)])/Units,
                                  currency == "GBX" ~ (Amount/GBPUSD.X[which.min(date)])/Units,
                                  currency == "HKD" ~ (Amount/HKD.X[which.min(date)])/Units,
                                  .default =  Amount/Units),
           open_price = ifelse(symbol == "ASC.L", open_price*100, open_price)) |>
    group_by(position_id) |>
    arrange(date) |>
    mutate(returns = (price/lag(price)) - 1,
           cum_returns = (price/open_price) - 1,
           current_value = ifelse(symbol == "Cash", Amount, Amount*(1+cum_returns))) |>
    group_by(date) |>
    mutate(w = Amount/sum(Amount, na.rm = TRUE),
           current_w = current_value/sum(current_value, na.rm = TRUE))
  
  return(df)
}




get_port_weights <- function(merged_acc_prices, weight = "current_price"){
  
  if(weight == "current_price"){
    return(merged_acc_prices |>
      group_by(symbol, date) |>
      summarise(w = sum(current_w, na.rm = TRUE)) |>
      ungroup() |>
      pivot_wider(names_from = symbol, values_from = w) |>
      mutate(across(!date, ~ifelse(is.na(.x), 0, .x))))
  } else{
    return(merged_acc_prices |>
             group_by(symbol, date) |>
             summarise(w = sum(w, na.rm = TRUE)) |>
             ungroup() |>
             pivot_wider(names_from = symbol, values_from = w) |>
             mutate(across(!date, ~ifelse(is.na(.x), 0, .x))))
  }

}

plot_port_value <- function(merged_acc_prices){
  
  merged_acc_prices |>
    group_by(date) |>
    summarise(current_value = sum(current_value, na.rm = TRUE)) |>
    ungroup() |>
    ggplot(aes(x = date, y = current_value)) +
    geom_line()  + 
    scale_y_continuous(n.breaks =17, labels= unit_format(unit = "$")) +
    ggthemes::theme_hc()
  
}
