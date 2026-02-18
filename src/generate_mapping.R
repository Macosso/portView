library(readr)
library(dplyr)

generate_instrument_mapping_template <- function(transactions_path = "data/transactions.csv", output_path = "data/instrument_mapping.csv") {
  if (!file.exists(transactions_path)) {
    stop(sprintf("Transactions file not found: %s. Run ingestion first.", transactions_path), call. = FALSE)
  }
  
  if (file.exists(output_path)) {
    message(sprintf("[mapping] %s already exists. Delete it first if you want to regenerate.", output_path))
    return(invisible(NULL))
  }
  
  transactions <- readr::read_csv(transactions_path, show_col_types = FALSE)
  
  unique_instruments <- transactions %>%
    select(instrument_id) %>%
    distinct() %>%
    filter(!is.na(instrument_id)) %>%
    arrange(instrument_id) %>%
    mutate(ticker = NA_character_)
  
  readr::write_csv(unique_instruments, output_path)
  
  message(sprintf("[mapping] Created %s with %s instrument IDs.", output_path, nrow(unique_instruments)))
  message("[mapping] Edit the file and fill in the 'ticker' column with Yahoo Finance symbols (e.g., AAPL, GOOGL, BTC-USD).")
  message("[mapping] Then run: Rscript src/portfolio_valuation.R")
  
  invisible(unique_instruments)
}

if (sys.nframe() == 0) {
  generate_instrument_mapping_template()
}
