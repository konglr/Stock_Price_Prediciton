# Alpha Vantage API Test Script
# Get stock daily K-line data

library(httr)
library(jsonlite)
library(xts)
library(quantmod)

# Read API Key from .Renviron
API_KEY <- Sys.getenv("ALPHA_VANTAGE_API_KEY")

if (API_KEY == "") {
  stop("ALPHA_VANTAGE_API_KEY not found. Please set it in .Renviron file")
}

# Alpha Vantage API base URL
BASE_URL <- "https://www.alphavantage.co/query"

# Function to get daily data
get_daily_data <- function(symbol, outputsize = "compact") {
  params <- list(
    "function" = "TIME_SERIES_DAILY",
    symbol = symbol,
    outputsize = outputsize,
    apikey = API_KEY
  )
  
  cat("Fetching daily data for", symbol, "...\n")
  
  response <- GET(BASE_URL, query = params)
  
  if (status_code(response) != 200) {
    stop("API request failed:", status_code(response))
  }
  
  content <- content(response, "parsed", encoding = "UTF-8")
  
  if (!is.null(content$`Error Message`)) {
    stop("API Error:", content$`Error Message`)
  }
  
  if (!is.null(content$Note)) {
    warning("API Rate Limit:", content$Note)
    return(NULL)
  }
  
  if (is.null(content$`Time Series (Daily)`)) {
    stop("No time series data found")
  }
  
  daily_data <- content$`Time Series (Daily)`
  
  dates <- names(daily_data)
  df <- data.frame(
    Date = as.Date(dates),
    Open = as.numeric(sapply(daily_data, `[[`, "1. open")),
    High = as.numeric(sapply(daily_data, `[[`, "2. high")),
    Low = as.numeric(sapply(daily_data, `[[`, "3. low")),
    Close = as.numeric(sapply(daily_data, `[[`, "4. close")),
    Volume = as.numeric(sapply(daily_data, `[[`, "5. volume"))
  )
  
  df <- df[order(df$Date), ]
  
  xts_data <- xts(df[, c("Open", "High", "Low", "Close", "Volume")], 
                  order.by = df$Date)
  
  return(xts_data)
}

# ============ TESTS ============

cat("\n=== Test 1: IBM Stock ===\n")
tryCatch({
  ibm_data <- get_daily_data("IBM")
  cat("Success!\n")
  cat("Dimensions:", dim(ibm_data), "\n")
  cat("\nLast 5 days:\n")
  print(tail(ibm_data, 5))
  
}, error = function(e) {
  cat("Error:", e$message, "\n")
})

cat("\n=== Test 2: Apple Stock ===\n")
tryCatch({
  aapl_data <- get_daily_data("AAPL")
  cat("Success!\n")
  cat("Dimensions:", dim(aapl_data), "\n")
  cat("\nLast 5 days:\n")
  print(tail(aapl_data, 5))
  
}, error = function(e) {
  cat("Error:", e$message, "\n")
})

cat("\n=== API Rate Limit Info ===\n")
cat("Free API limits:\n")
cat("- 5 requests per minute\n")
cat("- 500 requests per day\n")
cat("- Use 'compact' for last 100 days\n")
cat("- Use 'full' for all historical data\n")

# Test 3: Try outputsize = "full" (should be premium)
cat("\n=== Test 3: outputsize = 'full' (Premium) ===\n")
tryCatch({
  params <- list(
    "function" = "TIME_SERIES_DAILY",
    symbol = "IBM",
    outputsize = "full",
    apikey = API_KEY
  )

  response <- GET(BASE_URL, query = params)
  content <- content(response, "parsed", encoding = "UTF-8")

  cat("Raw API response:\n")
  print(content[c("Error Message", "Note", "Information", "Meta Data")])

}, error = function(e) {
  cat("Error:", e$message, "\n")
})

cat("\nTest complete!\n")
