# MiniMax API 测试脚本
library(httr2)

# 加载 .Renviron 文件中的环境变量
readRenviron(".Renviron")

# 获取 API Key
apiKey <- Sys.getenv("MINIMAX_API_KEY")

# 检查 API Key 是否存在
if (apiKey == "") {
  stop("请在 .Renviron 文件中设置 MINIMAX_API_KEY")
}

cat("API Key 已加载:", substr(apiKey, 1, 10), "...\n")

# 测试请求
system_prompt <- "你是一位有用的AI助手。请用一句话介绍你自己。你的版本是哪一个？ 你可以做哪些事情？"

api_url <- "https://api.minimax.chat/v1/text/chatcompletion_v2"

cat("正在调用 MiniMax API...\n")

tryCatch({
  resp <- request(api_url) %>%
    req_method("POST") %>%
    req_headers(
      "Authorization" = paste0("Bearer ", apiKey),
      "Content-Type" = "application/json"
    ) %>%
    req_body_json(list(
      model = "MiniMax-M2.1",
      messages = list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = "你好")
      ),
      temperature = 0.7,
      max_tokens = 256
    )) %>%
    req_retry(max_tries = 3, backoff = ~ 1 * 2^(.x - 1)) %>%
    req_perform()
  
  # 打印响应状态
  cat("响应状态:", resp$status_code, "\n")
  
  # 解析 JSON
  result <- resp_body_json(resp)
  
  # 打印完整响应（用于调试）
  print(str(result))
  
  # 提取内容
  if (!is.null(result$choices) && length(result$choices) > 0) {
    content <- result$choices[[1]]$message$content
    cat("\n=== AI 回复 ===\n")
    cat(content, "\n")
  } else {
    cat("未获取到有效回复\n")
    print(result)
  }
  
}, error = function(e) {
  cat("错误:", conditionMessage(e), "\n")
})
