# Load necessary libraries
library(httr)
library(jsonlite)

# 1. Get API Key from environment variables
# Make sure you have ALIYUNCS_API_KEY set in your .Renviron file
aliyun_api_key <- Sys.getenv("ALIYUNCS_API_KEY")

if (aliyun_api_key == "") {
  stop("ALIYUNCS_API_KEY not found. Please set it in your .Renviron file.")
}

# 2. API endpoint information
base_url <- "https://coding.dashscope.aliyuncs.com/v1"
model_name <- "qwen-plus" # Using a standard qwen model, you can change it to others like "qwen-max"

# 3. Prepare the request payload for chat completion
# This is a sample payload, you can adjust it
payload <- list(
  model = model_name,
  messages = list(
    list(role = "system", content = "You are a helpful assistant."),
    list(role = "user", content = "Hello! Can you introduce yourself?")
  ),
  stream = FALSE # Set to TRUE if you want to test streaming
)

# 4. Make the POST request to the API
cat("Sending request to Aliyun DashScope API...\n")
response <- POST(
  url = file.path(base_url, "chat/completions"),
  add_headers(
    `Authorization` = paste("Bearer", aliyun_api_key),
    `Content-Type` = "application/json"
  ),
  body = toJSON(payload, auto_unbox = TRUE),
  encode = "json"
)

# 5. Process and print the response
cat("Response received. Status code:", status_code(response), "\n")

if (status_code(response) == 200) {
  # Request was successful
  content <- content(response, "text", encoding = "UTF-8")
  parsed_content <- fromJSON(content)
  
  cat("\n--- Full Response ---\n")
  print(str(parsed_content))
  
  cat("\n--- Message Content ---\n")
  # Extract and print the assistant's message
  if (!is.null(parsed_content$choices) && length(parsed_content$choices) > 0) {
    message_content <- parsed_content$choices[[1]]$message$content
    cat(message_content, "\n")
  } else {
    cat("Could not find a message in the response.\n")
  }
  
} else {
  # Request failed
  cat("\n--- Error Response ---\n")
  content <- content(response, "text", encoding = "UTF-8")
  cat(content, "\n")
}
