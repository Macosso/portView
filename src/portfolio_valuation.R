library(dplyr)
library(tidyr)
library(readr)
library(quantmod)
library(xts)

source("src/etoro_client.R")

load_instrument_mapping <- function(mapping_file = "data/instrument_mapping.csv") {
  if (file.exists(mapping_file)) {
    message(sprintf("[portfolio] Loading instrument mapping from %s", mapping_file))
    return(readr::read_csv(mapping_file, show_col_types = FALSE))
  }
  
  message(sprintf("[portfolio] No mapping file found at %s. Returning empty mapping.", mapping_file))
  tibble(instrument_id = integer(), ticker = character())
}

save_instrument_mapping <- function(mapping_df, mapping_file = "data/instrument_mapping.csv") {
  readr::write_csv(mapping_df, mapping_file)
  message(sprintf("[portfolio] Saved instrument mapping to %s", mapping_file))
}

try_resolve_ticker_from_instrument_id <- function(instrument_id, known_mappings = NULL) {
  if (!is.null(known_mappings) && instrument_id %in% known_mappings$instrument_id) {
    ticker <- known_mappings$ticker[known_mappings$instrument_id == instrument_id]
    message(sprintf("[portfolio] Resolved instrument_id %s -> %s (from cache)", instrument_id, ticker))
    return(ticker)
  }
  
  message(sprintf("[portfolio] No ticker found for instrument_id %s. Manual mapping required.", instrument_id))
  return(NA_character_)
}

enrich_transactions_with_tickers <- function(transactions_df, mapping_df = NULL) {
  if (is.null(mapping_df)) {
    mapping_df <- load_instrument_mapping()
  }
  
  transactions_df <- transactions_df %>%
    left_join(mapping_df, by = "instrument_id")
  
  missing_count <- sum(is.na(transactions_df$ticker))
  if (missing_count > 0) {
    missing_ids <- unique(transactions_df$instrument_id[is.na(transactions_df$ticker)])
    warning(
      sprintf(
        "[portfolio] %s transactions missing ticker symbols for instrument_ids: %s. Update data/instrument_mapping.csv",
        missing_count,
        paste(missing_ids, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  transactions_df
}

fetch_historical_prices_quantmod <- function(tickers, start_date, end_date = Sys.Date(), forex_tickers = c("EURUSD=X", "GBPUSD=X", "HKD=X")) {
  all_tickers <- unique(c(tickers, forex_tickers))
  all_tickers <- all_tickers[!is.na(all_tickers)]
  
  if (length(all_tickers) == 0) {
    message("[portfolio] No tickers to fetch.")
    return(tibble(date = as.Date(character()), ticker = character(), close = numeric()))
  }
  
  message(sprintf("[portfolio] Fetching prices for %s tickers from %s to %s", length(all_tickers), start_date, end_date))
  
  price_list <- lapply(all_tickers, function(ticker) {
    message(sprintf("[portfolio] Fetching %s", ticker))
    tryCatch(
      {
        prices_xts <- quantmod::getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
        
        if (is.null(prices_xts) || nrow(prices_xts) == 0) {
          warning(sprintf("[portfolio] No data returned for %s", ticker), call. = FALSE)
          return(NULL)
        }
        
        close_col <- grep("Close", colnames(prices_xts), value = TRUE)
        if (length(close_col) == 0) {
          close_col <- colnames(prices_xts)[ncol(prices_xts)]
        } else {
          close_col <- close_col[1]
        }
        
        df <- data.frame(
          date = index(prices_xts),
          ticker = ticker,
          close = as.numeric(prices_xts[, close_col]),
          stringsAsFactors = FALSE
        )
        
        return(df)
      },
      error = function(err) {
        warning(sprintf("[portfolio] Failed to fetch %s: %s", ticker, err$message), call. = FALSE)
        return(NULL)
      }
    )
  })
  
  prices_df <- dplyr::bind_rows(price_list)
  message(sprintf("[portfolio] Fetched %s price records", nrow(prices_df)))
  
  prices_df
}

reconstruct_holdings_timeline <- function(transactions_df) {
  transactions_df <- transactions_df %>%
    filter(!is.na(ticker)) %>%
    mutate(
      open_date = as.Date(open_timestamp),
      close_date = as.Date(close_timestamp)
    )
  
  if (nrow(transactions_df) == 0) {
    message("[portfolio] No transactions with valid tickers to reconstruct holdings.")
    return(tibble(date = as.Date(character()), ticker = character(), units = numeric(), instrument_id = integer()))
  }
  
  min_date <- min(transactions_df$open_date, na.rm = TRUE)
  max_date <- Sys.Date()
  
  all_dates <- seq.Date(from = min_date, to = max_date, by = "day")
  
  holdings_list <- lapply(seq_len(nrow(transactions_df)), function(idx) {
    row <- transactions_df[idx, ]
    
    start_date <- row$open_date
    end_date <- if (is.na(row$close_date)) max_date else row$close_date
    
    if (is.na(start_date)) {
      return(NULL)
    }
    
    holding_dates <- seq.Date(from = start_date, to = end_date, by = "day")
    
    tibble(
      date = holding_dates,
      ticker = row$ticker,
      instrument_id = row$instrument_id,
      position_id = row$position_id,
      units = row$units,
      side = row$side
    )
  })
  
  holdings_df <- dplyr::bind_rows(holdings_list)
  
  holdings_df <- holdings_df %>%
    group_by(date, ticker, instrument_id) %>%
    summarise(
      units = sum(units * ifelse(side == "buy", 1, -1), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(units != 0)
  
  message(sprintf("[portfolio] Reconstructed holdings for %s unique dates", length(unique(holdings_df$date))))
  
  holdings_df
}

calculate_portfolio_value <- function(holdings_df, prices_df) {
  prices_df <- prices_df %>%
    filter(!grepl("=X$", ticker))
  
  portfolio_values <- holdings_df %>%
    inner_join(prices_df, by = c("date", "ticker")) %>%
    mutate(
      position_value = units * close
    ) %>%
    group_by(date) %>%
    summarise(
      total_value = sum(position_value, na.rm = TRUE),
      n_positions = n(),
      .groups = "drop"
    ) %>%
    arrange(date)
  
  message(sprintf("[portfolio] Calculated portfolio value for %s dates", nrow(portfolio_values)))
  
  portfolio_values
}

run_portfolio_valuation <- function(data_dir = "data", start_date = "2020-01-01") {
  transactions_path <- file.path(data_dir, "transactions.csv")
  mapping_path <- file.path(data_dir, "instrument_mapping.csv")
  
  if (!file.exists(transactions_path)) {
    stop(sprintf("Transactions file not found: %s. Run ingestion first.", transactions_path), call. = FALSE)
  }
  
  message("[portfolio] Loading transactions.")
  transactions_df <- readr::read_csv(transactions_path, show_col_types = FALSE)
  
  mapping_df <- load_instrument_mapping(mapping_path)
  
  transactions_df <- enrich_transactions_with_tickers(transactions_df, mapping_df)
  
  tickers <- unique(transactions_df$ticker)
  tickers <- tickers[!is.na(tickers)]
  
  if (length(tickers) == 0) {
    stop(
      sprintf(
        "No tickers available. Create %s with columns: instrument_id, ticker",
        mapping_path
      ),
      call. = FALSE
    )
  }
  
  min_transaction_date <- min(as.Date(transactions_df$open_timestamp), na.rm = TRUE)
  fetch_start <- min(as.Date(start_date), min_transaction_date)
  
  prices_df <- fetch_historical_prices_quantmod(tickers, start_date = fetch_start)
  
  prices_path <- file.path(data_dir, "historical_prices.csv")
  readr::write_csv(prices_df, prices_path)
  message(sprintf("[portfolio] Wrote historical prices to %s", prices_path))
  
  holdings_df <- reconstruct_holdings_timeline(transactions_df)
  
  holdings_path <- file.path(data_dir, "holdings_timeline.csv")
  readr::write_csv(holdings_df, holdings_path)
  message(sprintf("[portfolio] Wrote holdings timeline to %s", holdings_path))
  
  portfolio_values <- calculate_portfolio_value(holdings_df, prices_df)
  
  portfolio_path <- file.path(data_dir, "portfolio_value.csv")
  readr::write_csv(portfolio_values, portfolio_path)
  message(sprintf("[portfolio] Wrote portfolio value to %s", portfolio_path))
  
  list(
    transactions = transactions_df,
    prices = prices_df,
    holdings = holdings_df,
    portfolio_value = portfolio_values
  )
}

if (sys.nframe() == 0) {
  run_portfolio_valuation()
}
