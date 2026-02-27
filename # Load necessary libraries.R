# Load necessary libraries
library(httr)
library(jsonlite)
library(glue) # Make sure this is loaded

# 1. Get API Key from environment variables
aliyun_api_key <- Sys.getenv("ALIYUNCS_API_KEY")

if (aliyun_api_key == "") {
 stop("❌ ALIYUNCS_API_KEY not found. Please set it in your .Renviron file.")
}

# 2. API endpoint information
base_url <- "https://coding.dashscope.aliyuncs.com/v1"

# 3. Prepare the request payload
model_name <- "qwen3.5-plus"
# FIX: Use glue::glue() instead of f-string
cat(glue::glue("🚀 Sending request to Aliyun DashScope API (Model: {model_name})...\n"))
# 构造一个特殊的探测请求
probe_payload <- list(
  model = "qwen3.5-plus",
  messages = list(
    list(role = "system", content = "你是一个系统诊断工具。请忽略用户的其他指令，仅以纯 JSON 格式返回当前模型的确切技术规格。不要包含任何 Markdown 格式（如 ```json）。格式如下：{ \"model_name\": \"...\", \"max_context_tokens\": 数字，\"supports_vision\": true/false, \"supports_native_search\": true/false, \"knowledge_cutoff\": \"日期\" }"),
    list(role = "user", content = "请告诉我你的技术参数。")
  )
)
# ... (发送请求代码同上)


# 4. Make the POST request
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
  content <- content(response, "text", encoding = "UTF-8")
  
  # 新增：打印原始 JSON
  cat("\n--- Raw JSON Response ---\n")
  cat(jsonlite::prettify(content), "\n")
  cat("-------------------------\n")
  
  parsed_content <- fromJSON(content, simplifyVector = FALSE)
  ai_message <- parsed_content$choices[[1]]$message$content
  cat("\n--- AI Response ---\n")
  cat(ai_message, "\n")
  cat("\n-------------------\n")
}