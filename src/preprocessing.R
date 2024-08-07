# Read the data
source("src/preprocessing_def.R")

#read datasets
closed_positions <- get_closed_positions(account_statement)
deposits <- get_deposits(account_statement)
dividends <- get_dividends(account_statement)
open_positions <- get_open_positions(account_statement)
withdraws <- get_withdraw(account_statement)


symbols_mapping <- open_positions |>
  select(symbol, asset_type, position_id) |>
  left_join(closed_positions |> select(position_id, ISIN), by = "position_id")



open_closed <- open_positions |>
  left_join(closed_positions |> select(-c(symbol, open_price, Amount, Units, Type)), by = "position_id") |>
  mutate(open_date = as.Date(open_date),
         close_date = as.Date(close_date)) |>
  expand_dates()


#expand_dates(df = open_closed)


"#f4b943"
#00ff00