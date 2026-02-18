library(httr)
library(jsonlite)

etoro_base_url <- "https://public-api.etoro.com/api/v1"

get_etoro_keys <- function() {
  shared_key <- Sys.getenv("ETORO_KEY", unset = "")
  api_key <- Sys.getenv("ETORO_API_KEY", unset = "")
  user_key <- Sys.getenv("ETORO_USER_KEY", unset = "")

  api_key <- if (nzchar(api_key)) api_key else shared_key
  user_key <- if (nzchar(user_key)) user_key else shared_key

  if (!nzchar(api_key) || !nzchar(user_key)) {
    stop(
      "Missing eToro credentials. Set ETORO_API_KEY + ETORO_USER_KEY, or set ETORO_KEY as a shared fallback.",
      call. = FALSE
    )
  }

  list(api_key = api_key, user_key = user_key)
}

generate_request_id <- function() {
  hex_chars <- c(0:9, letters[1:6])
  uuid_raw <- paste0(sample(hex_chars, 32, replace = TRUE), collapse = "")

  substr(uuid_raw, 13, 13) <- "4"
  substr(uuid_raw, 17, 17) <- sample(c("8", "9", "a", "b"), 1)

  paste0(
    substr(uuid_raw, 1, 8), "-",
    substr(uuid_raw, 9, 12), "-",
    substr(uuid_raw, 13, 16), "-",
    substr(uuid_raw, 17, 20), "-",
    substr(uuid_raw, 21, 32)
  )
}

build_request_headers <- function() {
  keys <- get_etoro_keys()

  c(
    "x-api-key" = keys$api_key,
    "x-user-key" = keys$user_key,
    "x-request-id" = generate_request_id()
  )
}

parse_json_response <- function(response) {
  response_text <- httr::content(response, as = "text", encoding = "UTF-8")
  jsonlite::fromJSON(response_text, simplifyDataFrame = FALSE)
}

perform_get_request <- function(endpoint, query = list(), retries = 4, backoff_base = 0.5, timeout_sec = 30) {
  url <- paste0(etoro_base_url, endpoint)
  query <- query[!vapply(query, is.null, logical(1))]

  for (attempt in seq_len(retries)) {
    page_info <- if (!is.null(query$page)) paste0(" page=", query$page) else ""
    message(sprintf("[eToro] GET %s%s attempt=%s/%s", endpoint, page_info, attempt, retries))

    response <- tryCatch(
      {
        httr::GET(
          url = url,
          httr::add_headers(.headers = build_request_headers()),
          query = query,
          httr::timeout(timeout_sec)
        )
      },
      error = function(err) err
    )

    if (inherits(response, "error")) {
      message(sprintf("[eToro] GET %s failed before response: %s", endpoint, response$message))

      if (attempt < retries) {
        wait_seconds <- backoff_base * (2 ^ (attempt - 1))
        message(sprintf("[eToro] Retrying %s after %.1fs", endpoint, wait_seconds))
        Sys.sleep(wait_seconds)
        next
      }

      stop(sprintf("Request failed for %s: %s", endpoint, response$message), call. = FALSE)
    }

    status_code <- httr::status_code(response)
    message(sprintf("[eToro] GET %s status=%s", endpoint, status_code))

    if (status_code >= 200 && status_code < 300) {
      return(parse_json_response(response))
    }

    retryable_status <- status_code %in% c(408, 429) || status_code >= 500

    if (retryable_status && attempt < retries) {
      wait_seconds <- backoff_base * (2 ^ (attempt - 1))
      message(sprintf("[eToro] Retryable status on %s; backing off %.1fs", endpoint, wait_seconds))
      Sys.sleep(wait_seconds)
      next
    }

    body_text <- tryCatch(httr::content(response, as = "text", encoding = "UTF-8"), error = function(e) "")
    stop(sprintf("Request failed for %s with status %s. Response: %s", endpoint, status_code, body_text), call. = FALSE)
  }

  stop(sprintf("Request failed for %s after %s attempts.", endpoint, retries), call. = FALSE)
}

fetch_positions <- function() {
  perform_get_request("/trading/info/portfolio")
}

fetch_transactions <- function(min_date, page_size = 200, page = 1) {
  if (missing(min_date) || is.null(min_date) || !nzchar(as.character(min_date))) {
    stop("min_date is required for fetch_transactions().", call. = FALSE)
  }

  perform_get_request(
    endpoint = "/trading/info/trade/history",
    query = list(
      minDate = as.character(min_date),
      page = as.integer(page),
      pageSize = as.integer(page_size)
    )
  )
}

fetch_market_rates <- function(...) {
  extra_args <- list(...)
  if (length(extra_args) > 0) {
    message("[eToro] Additional arguments supplied to fetch_market_rates() are ignored to avoid undocumented query params.")
  }

  perform_get_request("/instruments/rates")
}

resolve_instrument_id <- function(ticker) {
  if (missing(ticker) || is.null(ticker) || !nzchar(as.character(ticker))) {
    stop("ticker is required for resolve_instrument_id().", call. = FALSE)
  }

  perform_get_request(
    endpoint = "/market-data/search",
    query = list(internalSymbolFull = as.character(ticker))
  )
}

normalize_trade_page <- function(page_payload) {
  if (is.null(page_payload)) {
    return(list())
  }

  if (is.data.frame(page_payload)) {
    if (nrow(page_payload) == 0) {
      return(list())
    }

    rows <- vector("list", nrow(page_payload))
    for (idx in seq_len(nrow(page_payload))) {
      rows[[idx]] <- as.list(page_payload[idx, , drop = FALSE])
    }
    return(rows)
  }

  if (is.list(page_payload)) {
    if (length(page_payload) == 0) {
      return(list())
    }

    if (is.null(names(page_payload))) {
      return(page_payload)
    }

    return(list(page_payload))
  }

  list(page_payload)
}

handle_pagination_page_based <- function(min_date, page_size = 200, max_pages = 1000) {
  all_trades <- list()

  for (current_page in seq_len(max_pages)) {
    page_trades <- fetch_transactions(
      min_date = min_date,
      page_size = page_size,
      page = current_page
    )

    page_rows <- normalize_trade_page(page_trades)
    page_count <- length(page_rows)
    message(sprintf("[eToro] trade history page=%s rows=%s", current_page, page_count))

    if (page_count == 0) {
      message("[eToro] trade history pagination exhausted (empty page).")
      break
    }

    all_trades <- c(all_trades, page_rows)

    if (page_count < page_size) {
      message("[eToro] trade history pagination exhausted (rows < page_size).")
      break
    }
  }

  all_trades
}
