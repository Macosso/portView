# portView

Phase 1 data ingestion is implemented for the eToro Public API and can run as a standalone script or be sourced inside Shiny.

## What is included

- `src/etoro_client.R`
	- Header/auth builder using environment variables
	- eToro endpoint wrappers for portfolio, trade history, market rates, and instrument lookup
	- Page-based pagination handler for trade history
	- Retry/backoff + request logging
- `src/ingest.R`
	- Normalizes API responses into canonical tables
	- Writes cache files to `data/`
	- Falls back to cached CSVs if API/network fails

## Environment variables (required)

Set these in `.Renviron` (recommended) or your shell environment:

- `ETORO_API_KEY`
- `ETORO_USER_KEY`

If your eToro portal issues a single key, you can set:

- `ETORO_KEY`

and it will be used as a fallback for both API and user keys.

Optional:

- `ETORO_MIN_DATE` (used by `src/ingest.R` when no CLI argument is passed)

## Run ingestion

From project root:

`Rscript src/ingest.R 2024-01-01`

or set `ETORO_MIN_DATE` and run:

`Rscript src/ingest.R`

You can also source and run in R/Shiny:

`source("src/ingest.R")`

`run_ingestion(min_date = "2024-01-01")`

## Output files

Generated in `data/`:

- `transactions.csv`
- `positions.csv`
- `rates.csv` (current rates endpoint)

## Offline fallback behavior

- If an API call fails, ingestion logs a warning and loads the corresponding cached CSV.
- If cache is missing for that dataset, ingestion stops with an actionable error.
- If `GET /instruments/rates` returns `404 RouteNotFound`, ingestion logs a warning, writes an empty `data/rates.csv`, and continues.

## Portfolio valuation (historical prices + value over time)

eToro API does not expose historical price data, so we use quantmod (Yahoo Finance) to fetch prices.

### Setup instrument mapping

1. After running ingestion, generate a mapping template:

   `Rscript src/generate_mapping.R`

   This creates `data/instrument_mapping.csv` with all your instrument IDs and empty ticker columns.

2. Edit `data/instrument_mapping.csv` and fill in ticker symbols (Yahoo Finance format):

   ```csv
   instrument_id,ticker
   1001,AAPL
   1002,GOOGL
   100000,BTC-USD
   ```

   Reference: `data/instrument_mapping.example.csv`

### Run portfolio valuation

From project root:

`Rscript src/portfolio_valuation.R`

Or in R/Shiny:

`source("src/portfolio_valuation.R")`

`run_portfolio_valuation(start_date = "2020-01-01")`

### Valuation outputs

Generated in `data/`:

- `instrument_mapping.csv` (you create this manually)
- `historical_prices.csv` (fetched from Yahoo Finance via quantmod)
- `holdings_timeline.csv` (daily holdings reconstructed from transactions)
- `portfolio_value.csv` (daily total portfolio value)

## Notes

- Secrets are not hardcoded in source.
- Market rates ingestion uses only current rates (`GET /instruments/rates`).
- Historical candles/OHLC are intentionally not implemented without official endpoint details.
- Portfolio valuation requires manual ticker mapping because eToro API returns numeric `instrumentId` only.
