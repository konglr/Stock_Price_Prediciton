# Load necessary libraries
library(httr)
library(jsonlite)

# 1. Get API Key from environment variables
aliyun_api_key <- Sys.getenv("ALIYUNCS_API_KEY")

if (is.null(aliyun_api_key) || aliyun_api_key == "") {
  # Attempt to load from .Renviron if not found
  if(file.exists(".Renviron")) {
    readRenviron(".Renviron")
    aliyun_api_key <- Sys.getenv("ALIYUNCS_API_KEY")
  }
}

if (aliyun_api_key == "") {
  stop("ALIYUNCS_API_KEY not found. Please set it in your .Renviron file.")
}

# 2. List of models to test
models_to_test <- c(
  "qwen3.5-plus", 
  "qwen3-max-2026-01-23",
  "qwen3-coder-next",
  "qwen3-coder-plus",
  "glm-4.7",
  "kimi-k2-5" # Adjusted from kimi-k2.5, this was the only correct adjustment
)

# Base URL for the API
base_url <- "https://coding.dashscope.aliyuncs.com/v1"

# 3. Loop through each model and test it
for (model_name in models_to_test) {
  
  cat(paste("\n\n------------------- Testing Model:", model_name, "-------------------\n"))
  
  # Prepare the request payload
  payload <- list(
    model = model_name,
    messages = list(
      list(role = "system", content = "You are a helpful assistant."),
      list(role = "user", content = "你好，请介绍你自己。")
    ),
    stream = FALSE
  )
  
  # Make the POST request
  cat("Sending request...\n")
  response <- POST(
    url = file.path(base_url, "chat/completions"),
    add_headers(
      `Authorization` = paste("Bearer", aliyun_api_key),
      `Content-Type` = "application/json"
    ),
    body = toJSON(payload, auto_unbox = TRUE),
    encode = "json",
    timeout(15) # Add a timeout to prevent long waits
  )
  
  # Process and print the response
  cat("Response received. Status code:", status_code(response), "\n")
  
  if (status_code(response) == 200) {
    # Request was successful
    parsed_content <- content(response, "parsed", encoding = "UTF-8")
    
    # Extract and print the assistant's message
    if (!is.null(parsed_content$choices) && length(parsed_content$choices) > 0) {
      message_content <- parsed_content$choices$message$content
      cat("--- Model Response ---\n")
      cat(message_content, "\n")
    } else {
      cat("--- Response received, but no message content found. ---\n")
      print(str(parsed_content))
    }
    
  } else {
    # Request failed
    cat("--- Error Response ---\n")
    error_content <- content(response, "text", encoding = "UTF-8")
    cat(error_content, "\n")
  }
  
  # Add a small delay to avoid hitting rate limits
  Sys.sleep(2)
}

cat("\n\n------------------- All tests completed. -------------------\n")
