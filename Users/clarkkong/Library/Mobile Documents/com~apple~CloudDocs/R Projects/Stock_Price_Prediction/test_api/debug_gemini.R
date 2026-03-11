# 测试 Gemini API 调用
library(httr2)

cat("=== 测试 Gemini API ===\n")

api_key <- Sys.getenv("GEMINI_API_KEY")
cat("API Key 存在:", api_key != "", "\n")

if (api_key != "") {
  model_id <- "gemini-3.1-flash-lite-preview"
  api_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_id, ":generateContent")
  
  req_body <- list(
    contents = list(list(parts = list(list(text = "你好，请用一句话介绍自己")))),
    generationConfig = list(
      temperature = 0.7,
      maxOutputTokens = 100
    )
  )
  
  tryCatch({
    resp <- httr2::request(api_url) |>
      httr2::req_url_query(key = api_key) |>
      httr2::req_method("POST") |>
      httr2::req_body_json(req_body) |>
      httr2::req_perform()
    
    result <- httr2::resp_body_json(resp)
    cat("响应成功!\n")
    cat("内容:", result$candidates[[1]]$content$parts[[1]]$text, "\n")
  }, error = function(e) {
    cat("错误:", e$message, "\n")
  })
} else {
  cat("API Key 为空!\n")
}

# 测试带联网搜索
cat("\n=== 测试 Gemini 联网搜索 ===\n")

if (api_key != "") {
  req_body <- list(
    contents = list(list(parts = list(list(text = "特斯拉股票今天走势如何")))),
    generationConfig = list(
      temperature = 0.7,
      maxOutputTokens = 500
    ),
    tools = list(list(google_search = setNames(list(), character(0))))
  )
  
  tryCatch({
    resp <- httr2::request(api_url) |>
      httr2::req_url_query(key = api_key) |>
      httr2::req_method("POST") |>
      httr2::req_body_json(req_body) |>
      httr2::req_perform()
    
    result <- httr2::resp_body_json(resp)
    cat("联网搜索响应成功!\n")
    cat("内容:", substr(result$candidates[[1]]$content$parts[[1]]$text, 1, 500), "\n")
    
    # 检查 grounding
    if (!is.null(result$candidates[[1]]$groundingMetadata)) {
      cat("Grounding 信息:\n")
      print(result$candidates[[1]]$groundingMetadata)
    }
  }, error = function(e) {
    cat("联网搜索错误:", e$message, "\n")
  })
}