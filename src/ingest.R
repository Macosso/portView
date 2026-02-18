library(dplyr)
library(readr)
library(tibble)
library(purrr)

source("src/etoro_client.R")

coalesce_null <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    return(y)
  }
  x
}

safe_number <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_real_)
  }
  if (is.list(x)) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
  }
  suppressWarnings(as.numeric(x[[1]]))
}

safe_integer <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_integer_)
  }
  if (is.list(x)) {
    x <- unlist(x, recursive = TRUE, use.names = FALSE)
  }
  suppressWarnings(as.integer(x[[1]]))
}

safe_time <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(as.POSIXct(NA))
  }

  x_chr <- as.character(x)
  x_chr[!nzchar(x_chr)] <- NA_character_
  suppressWarnings(as.POSIXct(x_chr, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
}

is_route_not_found_error <- function(err) {
  grepl("RouteNotFound|status\\s+404", err$message, ignore.case = TRUE)
}

normalize_transactions <- function(trades_payload) {
  if (is.null(trades_payload) || length(trades_payload) == 0) {
    return(tibble(
      position_id = integer(),
      order_id = integer(),
      instrument_id = integer(),
      side = character(),
      units = numeric(),
      open_rate = numeric(),
      close_rate = numeric(),
      open_timestamp = as.POSIXct(character()),
      close_timestamp = as.POSIXct(character()),
      fees = numeric(),
      net_profit = numeric(),
      leverage = numeric()
    ))
  }

  purrr::map_dfr(trades_payload, function(row) {
    tibble(
      position_id = safe_integer(row$positionId),
      order_id = safe_integer(row$orderId),
      instrument_id = safe_integer(row$instrumentId),
      side = dplyr::case_when(
        isTRUE(row$isBuy) ~ "buy",
        identical(row$isBuy, FALSE) ~ "sell",
        TRUE ~ NA_character_
      ),
      units = safe_number(row$units),
      open_rate = safe_number(row$openRate),
      close_rate = safe_number(row$closeRate),
      open_timestamp = safe_time(row$openTimestamp),
      close_timestamp = safe_time(row$closeTimestamp),
      fees = safe_number(row$fees),
      net_profit = safe_number(row$netProfit),
      leverage = safe_number(row$leverage)
    )
  })
}

normalize_positions <- function(portfolio_payload) {
  positions <- coalesce_null(portfolio_payload$clientPortfolio$positions, list())

  if (length(positions) == 0) {
    return(tibble(
      position_id = integer(),
      instrument_id = integer(),
      side = character(),
      units = numeric(),
      open_rate = numeric(),
      open_timestamp = as.POSIXct(character()),
      amount = numeric(),
      leverage = numeric(),
      total_fees = numeric()
    ))
  }

  purrr::map_dfr(positions, function(row) {
    tibble(
      position_id = safe_integer(row$positionId),
      instrument_id = safe_integer(row$instrumentId),
      side = dplyr::case_when(
        isTRUE(row$isBuy) ~ "buy",
        identical(row$isBuy, FALSE) ~ "sell",
        TRUE ~ NA_character_
      ),
      units = safe_number(row$units),
      open_rate = safe_number(row$openRate),
      open_timestamp = safe_time(row$openDateTime),
      amount = safe_number(row$amount),
      leverage = safe_number(row$leverage),
      total_fees = safe_number(row$totalFees)
    )
  })
}

as_rate_table <- function(rates_payload) {
  if (is.null(rates_payload)) {
    return(tibble())
  }

  if (is.data.frame(rates_payload)) {
    return(tibble::as_tibble(rates_payload))
  }

  if (is.list(rates_payload)) {
    if (length(rates_payload) == 0) {
      return(tibble())
    }

    if (is.null(names(rates_payload))) {
      rows <- purrr::map(rates_payload, function(item) {
        if (is.list(item)) {
          tibble::as_tibble(item)
        } else {
          tibble::tibble(value = item)
        }
      })
      return(dplyr::bind_rows(rows))
    }

    return(tibble::as_tibble(rates_payload))
  }

  tibble::tibble(value = rates_payload)
}

first_existing_col <- function(df, candidates) {
  matched <- intersect(candidates, colnames(df))
  if (length(matched) == 0) {
    return(NULL)
  }
  matched[[1]]
}

normalize_rates <- function(rates_payload) {
  rates_tbl <- as_rate_table(rates_payload)

  if (nrow(rates_tbl) == 0) {
    return(tibble(
      instrument_id = integer(),
      timestamp = as.POSIXct(character()),
      bid = numeric(),
      ask = numeric(),
      mid = numeric()
    ))
  }

  instrument_col <- first_existing_col(rates_tbl, c("instrumentId", "instrument_id"))
  timestamp_col <- first_existing_col(rates_tbl, c("timestamp", "updatedAt", "time", "rateTimestamp"))
  bid_col <- first_existing_col(rates_tbl, c("bid", "bidRate"))
  ask_col <- first_existing_col(rates_tbl, c("ask", "askRate"))
  mid_col <- first_existing_col(rates_tbl, c("mid", "midRate", "rate"))

  canonical <- tibble(
    instrument_id = if (!is.null(instrument_col)) as.integer(rates_tbl[[instrument_col]]) else NA_integer_,
    timestamp = if (!is.null(timestamp_col)) safe_time(rates_tbl[[timestamp_col]]) else as.POSIXct(NA),
    bid = if (!is.null(bid_col)) as.numeric(rates_tbl[[bid_col]]) else NA_real_,
    ask = if (!is.null(ask_col)) as.numeric(rates_tbl[[ask_col]]) else NA_real_,
    mid = if (!is.null(mid_col)) as.numeric(rates_tbl[[mid_col]]) else NA_real_
  )

  used_cols <- unique(Filter(Negate(is.null), c(instrument_col, timestamp_col, bid_col, ask_col, mid_col)))

  extra_cols <- rates_tbl[, setdiff(colnames(rates_tbl), used_cols), drop = FALSE]
  dplyr::bind_cols(canonical, extra_cols)
}

ensure_data_dir <- function(data_dir = "data") {
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
}

cache_path <- function(data_dir, dataset_name) {
  file.path(data_dir, paste0(dataset_name, ".csv"))
}

write_cache <- function(df, data_dir, dataset_name) {
  path <- cache_path(data_dir, dataset_name)
  readr::write_csv(df, path)
  message(sprintf("[ingest] wrote %s rows to %s", nrow(df), path))
  path
}

load_cached_or_stop <- function(data_dir, dataset_name, reason) {
  path <- cache_path(data_dir, dataset_name)

  if (file.exists(path)) {
    warning(sprintf("[ingest] %s. Falling back to cached %s", reason, path), call. = FALSE)
    return(readr::read_csv(path, show_col_types = FALSE))
  }

  stop(
    sprintf(
      "%s and no cached file found at %s. Run ingestion again when API/network is available.",
      reason,
      path
    ),
    call. = FALSE
  )
}

run_ingestion <- function(min_date, page_size = 200, data_dir = "data") {
  if (missing(min_date) || is.null(min_date) || !nzchar(as.character(min_date))) {
    stop("run_ingestion() requires min_date (YYYY-MM-DD).", call. = FALSE)
  }

  ensure_data_dir(data_dir)

  message("[ingest] Starting transactions ingestion.")
  transactions_df <- tryCatch(
    {
      raw_transactions <- handle_pagination_page_based(min_date = min_date, page_size = page_size)
      normalized <- normalize_transactions(raw_transactions)
      write_cache(normalized, data_dir, "transactions")
      normalized
    },
    error = function(err) {
      load_cached_or_stop(data_dir, "transactions", paste0("Transactions fetch failed: ", err$message))
    }
  )

  message("[ingest] Starting positions ingestion.")
  positions_df <- tryCatch(
    {
      raw_positions <- fetch_positions()
      normalized <- normalize_positions(raw_positions)
      write_cache(normalized, data_dir, "positions")
      normalized
    },
    error = function(err) {
      load_cached_or_stop(data_dir, "positions", paste0("Positions fetch failed: ", err$message))
    }
  )

  message("[ingest] Starting rates ingestion.")
  rates_df <- tryCatch(
    {
      raw_rates <- fetch_market_rates()
      normalized <- normalize_rates(raw_rates)
      write_cache(normalized, data_dir, "rates")
      normalized
    },
    error = function(err) {
      if (is_route_not_found_error(err)) {
        warning(
          paste0(
            "[ingest] Rates endpoint unavailable (404 RouteNotFound). ",
            "Writing empty rates cache and continuing."
          ),
          call. = FALSE
        )
        empty_rates <- tibble(
          instrument_id = integer(),
          timestamp = as.POSIXct(character()),
          bid = numeric(),
          ask = numeric(),
          mid = numeric()
        )
        write_cache(empty_rates, data_dir, "rates")
        return(empty_rates)
      }

      load_cached_or_stop(data_dir, "rates", paste0("Rates fetch failed: ", err$message))
    }
  )

  list(
    transactions = transactions_df,
    positions = positions_df,
    rates = rates_df
  )
}

if (sys.nframe() == 0) {
  arg_min_date <- commandArgs(trailingOnly = TRUE)
  min_date <- if (length(arg_min_date) >= 1) arg_min_date[[1]] else Sys.getenv("ETORO_MIN_DATE", unset = "")

  if (!nzchar(min_date)) {
    stop(
      "No min_date supplied. Provide an argument (e.g. Rscript src/ingest.R 2024-01-01) or set ETORO_MIN_DATE.",
      call. = FALSE
    )
  }

  run_ingestion(min_date = min_date)
}
