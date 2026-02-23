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

# 2. Define the API endpoint for listing deployable models
# As per the provided documentation.
api_url <- "https://dashscope.aliyuncs.com/api/v1/deployments/models?page_no=1&page_size=100"

cat("Attempting to list models from:", api_url, "
")

# 3. Make the GET request
tryCatch({
  response <- GET(
    url = api_url,
    add_headers(
      `Authorization` = paste("Bearer", aliyun_api_key)
    ),
    timeout(10)
  )
  
  cat("Response received. Status code:", status_code(response), "
")
  
  if (status_code(response) == 200) {
    # Request was successful
    parsed_content <- content(response, "parsed", encoding = "UTF-8")
    
    # OpenAI-compatible APIs usually return a list with a "data" element
    # which contains the list of models.
    if (!is.null(parsed_content$data)) {
      # Extract just the model IDs
      model_ids <- sapply(parsed_content$data, function(m) m$id)
      cat("
--- Available Models ---
")
      print(sort(model_ids))
    } else {
      cat("
--- Full Response (unexpected format) ---
")
      print(str(parsed_content))
    }
    
  } else {
    # Request failed
    cat("
--- Error Response ---
")
    error_content <- content(response, "text", encoding = "UTF-8")
    cat(error_content, "
")
  }
  
}, error = function(e) {
  cat("
--- Request Error ---
")
  cat("An error occurred during the request:", e$message, "
")
})
