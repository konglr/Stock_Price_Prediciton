library(httr2)
library(jsonlite)

# Load .Renviron
readRenviron(".Renviron")
apiKey <- Sys.getenv("GEMINI_API_KEY")

# Setup
model_id <- "gemini-2.0-flash" # Use a known working ID
api_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_id, ":generateContent")

system_prompt <- "你是一位拥有20年经验的资深美股投资专家。任务：基于用户提供的历史交易数据以及最新的市场信息，进行多维度的技术和量价分析。
    注意：请直接返回一个合法的 JSON 字符串，不要包含任何 Markdown 格式。JSON 必须包含以下字段：
    - news：股票相关核心新闻动态
    - financial: 财务信息、行业对比、估值评估总结
    - trend: 简短描述当前走势趋势
    - prediction_5d: 预测未来 5 个交易日的估计收盘价数组
    - reasoning: 详细的投资逻辑分析
    - support_level: 主要支撑位置价格
    - resistance_level: 主要阻力位置价格
    - trade_advice: { action: '买入价格/盈利价格/止顺价格', buy_price, take_profit, stop_loss }。"

user_query <- "股票代码: AAPL\n最近半年涨幅较大，估值偏高，请分析。"

req_body <- list(
  contents = list(
    list(
      role = "user",
      parts = list(list(text = user_query))
    )
  ), 
  systemInstruction = list(parts = list(list(text = system_prompt))), 
  tools = list(
    list(google_search = setNames(list(), character(0)))
  )
)

cat("Calling Gemini...\n")
tryCatch({
  resp <- request(api_url) %>%
    req_url_query(key = apiKey) %>% 
    req_method("POST") %>%
    req_body_json(req_body) %>%
    req_perform()
  
  result <- resp_body_json(resp)
  cat("RAW Response received.\n")
  
  if (!is.null(result$candidates[[1]]$content$parts[[1]]$text)) {
    raw_text <- result$candidates[[1]]$content$parts[[1]]$text
    cat("\n--- RAW TEXT START ---\n")
    cat(raw_text)
    cat("\n--- RAW TEXT END ---\n")
  } else {
    cat("No text content found.\n")
    print(result$candidates[[1]])
  }
}, error = function(e) {
  cat("Error:", e$message, "\n")
  if (!is.null(e$response)) {
    print(resp_body_string(e$response))
  }
})
