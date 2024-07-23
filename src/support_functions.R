library(tidyverse)

get_prices_long <- function(x){
  df <- data.frame(date = index(x),coredata(x)) |>
    pivot_longer(!date, values_to = "price", names_to = "symbol")
  
  return(df)
}



merge_acc_prices <- function(open_closed, long_df_prices){
  
  df <- open_closed |> left_join(long_df_prices, by = c("date", "symbol"))  |>
    group_by(date) |>
    mutate(w = Amount/sum(Amount, na.rm = TRUE),
           cash = Balance[which(execution_date_time == max(execution_date_time, na.rm = TRUE))]) |>
    group_by(position_id) |>
    arrange(date) |>
    mutate(returns = log(price) - log(lag(price)),
           cum_returns = log(price) - log(open_price))
  
  return(df)
}




get_port_weights <- function(merged_acc_prices){
  
  merged_acc_prices |>
    group_by(symbol, date) |>
    summarise(w = sum(w, na.rm = TRUE)) |>
    ungroup() |>
    pivot_wider(names_from = symbol, values_from = w) |>
    mutate(across(!date, ~ifelse(is.na(.x), 0, .x)))
}
