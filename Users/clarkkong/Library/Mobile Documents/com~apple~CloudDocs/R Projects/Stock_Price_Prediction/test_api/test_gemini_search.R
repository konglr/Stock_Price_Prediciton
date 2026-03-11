library(httr2)
library(jsonlite)

# 读取 .Renviron
readRenviron("../.Renviron")

api_key <- Sys.getenv("GEMINI_API_KEY")
cat("API Key 长度:", nchar(api_key), "\n\n")

# 测试联网搜索
search_request <- list(
  contents = list(
    list(role = "user", parts = list(list(text = "今天苹果公司AAPL股票的最新价格是多少？")))
  ),
  tools = list(list(google_search = setNames(list(), character(0)))),
  generationConfig = list(temperature = 0.5)
)

cat("=== 测试 Gemini 联网搜索 ===\n")
tryCatch({
  resp <- request("https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent") |>
    req_url_query(key = api_key) |>
    req_method("POST") |>
    req_body_json(search_request) |>
    req_perform()
  
  result <- resp_body_json(resp)
  cat("响应状态: 成功\n\n")
  cat("AI 回复:\n")
  cat(result$candidates[[1]]$content$parts[[1]]$text, "\n")
  
  # 检查 grounding
  if (!is.null(result$candidates[[1]]$groundingMetadata)) {
    cat("\n=== 联网来源 ===\n")
    meta <- result$candidates[[1]]$groundingMetadata
    if (!is.null(meta$groundingAttributions)) {
      for (attr in meta$groundingAttributions) {
        cat("来源:", attr$web$title, "\n")
        cat("链接:", attr$web$uri, "\n")
      }
    }
  } else {
    cat("\n注意: 无 groundingMetadata，联网搜索可能未触发\n")
  }
}, error = function(e) {
  cat("错误:", e$message, "\n")
  if (!is.null(e$response)) {
    cat("HTTP 状态:", resp_status(e$response), "\n")
    cat("详情:", substr(resp_body_string(e$response), 1, 500), "\n")
  }
})