# Skill: Aliyun DashScope (qwen) Models API

This document outlines how to connect to Aliyun's DashScope services, which provide access to Tongyi Qianwen (qwen) models and other third-party models.

## API Keys

An API key must be available as an environment variable named `ALIYUNCS_API_KEY`. You can set this in your `.Renviron` file.

```r
# In .Renviron file
ALIYUNCS_API_KEY="your_actual_api_key"
```

```r
# In R code
apiKey <- Sys.getenv("ALIYUNCS_API_KEY")
```

## Base URLs

DashScope provides different base URLs depending on the API protocol you intend to use. **Do not mix these with the generic DashScope Base URL.**

*   **OpenAI Compatible Protocol:**
    ```
    https://coding.dashscope.aliyuncs.com/v1
    ```

*   **Anthropic Compatible Protocol:**
    ```
    https://coding.dashscope.aliyuncs.com/apps/anthropic
    ```

## Verified Models (as of 2026-03-02)

The following model IDs have been successfully tested with the `https://coding.dashscope.aliyuncs.com/v1` endpoint.

### Tongyi Qianwen (qwen) Series
*   `qwen3.5-plus`
*   `qwen3-max-2026-01-23`
*   `qwen3-coder-next`
*   `qwen3-coder-plus`

### Third-Party Models
*   `glm-4.7`
*   `kimi-k2-5` (Note: the working ID is `kimi-k2-5`) 

## R Implementation Example (OpenAI Protocol)

The following example shows how to make a chat completion request to the `qwen3.5-plus` model.

```r
library(httr)
library(jsonlite)

# Get API Key from environment
aliyun_api_key <- Sys.getenv("ALIYUNCS_API_KEY")

# API endpoint information
base_url <- "https://coding.dashscope.aliyuncs.com/v1"
model_name <- "qwen3.5-plus"

# Prepare the request payload
payload <- list(
  model = model_name,
  messages = list(
    list(role = "system", content = "You are a helpful assistant."),
    list(role = "user", content = "Hello! Tell me about yourself.")
  )
)

# Make the POST request
response <- POST(
  url = file.path(base_url, "chat/completions"),
  add_headers(
    `Authorization` = paste("Bearer", aliyun_api_key),
    `Content-Type` = "application/json"
  ),
  body = toJSON(payload, auto_unbox = TRUE)
)

# Process the response
if (status_code(response) == 200) {
  # content() with "parsed" will automatically use jsonlite to parse the JSON
  parsed_content <- content(response, "parsed")
  
  # The result is a list containing data.frames.
  # We access the message content like this:
  message_content <- parsed_content$choices$message$content
  cat(message_content)
  
} else {
  cat("Error:", status_code(response), "
")
  print(content(response, "text"))
}
```
