# 调试 Gemini API - 打印详细错误信息
library(httr2)
library(jsonlite)

# Load .Renviron
readRenviron(".Renviron")
apiKey <- Sys.getenv("GEMINI_API_KEY")

cat("API Key 存在:", nchar(apiKey) > 0, "\n")
cat("API Key 前10位:", substr(apiKey, 1, 10), "\n\n")

# 测试不同模型
models <- c(
  "gemini-2.0-flash",
  "gemini-2.0-flash-exp",
  "gemini-1.5-flash",
  "gemini-3.1-flash-lite-preview",
  "gemini-2.5-flash"
)

for (model_id in models) {
  cat("\n=== 测试模型:", model_id, "===\n")
  
  api_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_id, ":generateContent")
  
  req_body <- list(
    contents = list(
      list(
        parts = list(list(text = "你好"))
      )
    )
  )
  
  tryCatch({
    resp <- request(api_url) %>%
      req_url_query(key = apiKey) %>%
      req_method("POST") %>%
      req_body_json(req_body) %>%
      req_perform()
    
    result <- resp_body_json(resp)
    cat("✓ 成功! 响应:", substr(result$candidates[[1]]$content$parts[[1]]$text, 1, 100), "\n")
  }, error = function(e) {
    # 打印详细错误
    if (!is.null(e$response)) {
      status <- httr2::resp_status(e$response)
      body <- httr2::resp_body_string(e$response)
      cat("HTTP 状态:", status, "\n")
      cat("错误响应:", substr(body, 1, 500), "\n")
    } else {
      cat("错误:", e$message, "\n")
    }
  })
  
  Sys.sleep(1)  # 避免太快
}