# 测试 ask_ai_question_type 函数
# 诊断 AI 返回空响应的问题

library(httr2)
library(jsonlite)

# 加载环境变量
readRenviron(".Renviron")

# API Keys
minimax_apiKey <- Sys.getenv("MINIMAX_API_KEY")
aliyun_apiKey <- Sys.getenv("ALIYUNCS_API_KEY")

# 测试问题
test_question <- "帮我分析中国银行的股票走势"

# 构建提示词
query <- paste0("你是一个股票代码识别助手，专门返回 Yahoo Finance 格式的股票代码。",
                "\n\n用户问题是：\"", test_question, "\"",
                "\n\n请判断用户问题中是否提到了上市公司？",
                "\n\nYahoo Finance 股票代码格式：",
                "\n1. 美股：直接返回代码（如 GOOGL、AAPL、TSLA、NVDA）",
                "\n2. A 股（上海交易所）：6 位数字 + .SS（如 601988.SS 中国银行、600519.SS 贵州茅台）",
                "\n3. A 股（深圳交易所）：6 位数字 + .SZ（如 000001.SZ 平安银行、002594.SZ 比亚迪）",
                "\n4. 港股：4 位数字 + .HK（如 3988.HK 中国银行、0700.HK 腾讯、9988.HK 阿里巴巴）",
                "\n\n转换规则：",
                "\n- 美股股票代码通常是 1-5 个大写字母",
                "\n- A 股代码是 6 位数字，上海交易所代码以 60、68 开头，加.SS；深圳交易所代码以 00、30 开头，加.SZ",
                "\n- 港股代码是 4-5 位数字，加.HK（不需要补零）",
                "\n\n示例：",
                "\n- 用户说'谷歌/Google' → 返回 GOOGL",
                "\n- 用户说'中国银行 A 股' → 返回 601988.SS",
                "\n- 用户说'中国银行港股' → 返回 3988.HK",
                "\n- 用户说'腾讯' → 返回 0700.HK",
                "\n- 用户说'贵州茅台' → 返回 600519.SS",
                "\n- 用户说'比亚迪 A 股' → 返回 002594.SZ",
                "\n- 用户说'平安银行' → 返回 000001.SZ",
                "\n- 没有提到上市公司 → 返回 GENERAL",
                "\n\n只返回 Yahoo Finance 格式的股票代码或 GENERAL，不要有其他文字。")

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("测试问题：", test_question, "\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# ============================================
# 测试 MiniMax API
# ============================================
cat("\n--- 测试 MiniMax API ---\n")
cat("API Key 前 10 位：", substr(minimax_apiKey, 1, 10), "...\n")
cat("API Key 长度：", nchar(minimax_apiKey), "\n\n")

tryCatch({
  api_url <- "https://api.minimax.chat/v1/text/chatcompletion_v2"
  
  req_body <- list(
    model = "MiniMax-M2.5",
    messages = list(
      list(role = "user", content = query)
    ),
    temperature = 0.3,
    max_tokens = 500  # 增加到 500，因为 reasoning 会消耗大量 tokens
  )
  
  cat("请求 URL：", api_url, "\n")
  cat("请求模型：MiniMax-M2.5\n")
  cat("请求体（部分）：\n")
  cat("  temperature: 0.3\n")
  cat("  max_tokens: 500 (修复后)\n\n")
  
  start_time <- Sys.time()
  
  resp <- request(api_url) %>%
    req_method("POST") %>%
    req_headers(
      "Authorization" = paste0("Bearer ", minimax_apiKey),
      "Content-Type" = "application/json"
    ) %>%
    req_body_json(req_body) %>%
    req_timeout(30) %>%
    req_perform()
  
  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  cat("HTTP 状态码：", resp_status(resp), "\n")
  cat("响应时间：", round(elapsed, 2), "秒\n\n")
  
  result <- resp_body_json(resp)
  
  cat("完整响应（JSON）：\n")
  cat(toJSON(result, pretty = TRUE, auto_unbox = TRUE), "\n\n")
  
  if (!is.null(result$choices) && length(result$choices) > 0) {
    ai_response <- result$choices[[1]]$message$content
    reasoning_content <- result$choices[[1]]$message$reasoning_content
    
    cat("AI 响应内容 (content)：", if(is.null(ai_response) || ai_response == "") "(空)" else ai_response, "\n")
    cat("AI 推理内容 (reasoning_content)：", if(is.null(reasoning_content) || reasoning_content == "") "(空)" else substr(reasoning_content, 1, 200), "...\n")
    
    # 模拟修复后的逻辑
    final_response <- ai_response
    if (is.null(final_response) || final_response == "") {
      final_response <- reasoning_content
      cat("\n--- 修复：从 reasoning_content 提取内容 ---\n")
    }
    cat("最终使用的响应：", if(is.null(final_response)) "(空)" else final_response, "\n")
  } else {
    cat("AI 响应内容：(空)\n")
    if (!is.null(result$error)) {
      cat("错误信息：", result$error$message, "\n")
    }
  }
  
}, error = function(e) {
  cat("MiniMax API 错误：", e$message, "\n")
})

# ============================================
# 测试阿里云 API
# ============================================
cat("\n\n--- 测试阿里云 API ---\n")
cat("API Key 前 10 位：", substr(aliyun_apiKey, 1, 10), "...\n")
cat("API Key 长度：", nchar(aliyun_apiKey), "\n\n")

tryCatch({
  api_url <- "https://coding.dashscope.aliyuncs.com/v1/chat/completions"
  
  req_body <- list(
    model = "qwen3.5-plus",
    messages = list(
      list(role = "user", content = query)
    ),
    temperature = 0.3,
    max_tokens = 50
  )
  
  cat("请求 URL：", api_url, "\n")
  cat("请求模型：qwen3.5-plus\n")
  cat("请求体（部分）：\n")
  cat("  temperature: 0.3\n")
  cat("  max_tokens: 50\n\n")
  
  start_time <- Sys.time()
  
  resp <- request(api_url) %>%
    req_method("POST") %>%
    req_headers(
      "Authorization" = paste0("Bearer ", aliyun_apiKey),
      "Content-Type" = "application/json"
    ) %>%
    req_body_json(req_body) %>%
    req_timeout(30) %>%
    req_perform()
  
  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  cat("HTTP 状态码：", resp_status(resp), "\n")
  cat("响应时间：", round(elapsed, 2), "秒\n\n")
  
  result <- resp_body_json(resp)
  
  cat("完整响应（JSON）：\n")
  cat(toJSON(result, pretty = TRUE, auto_unbox = TRUE), "\n\n")
  
  if (!is.null(result$choices) && length(result$choices) > 0) {
    ai_response <- result$choices[[1]]$message$content
    cat("AI 响应内容：", ai_response, "\n")
  } else {
    cat("AI 响应内容：(空)\n")
    if (!is.null(result$error)) {
      cat("错误信息：", result$error$message, "\n")
    }
  }
  
}, error = function(e) {
  cat("阿里云 API 错误：", e$message, "\n")
})

cat("\n\n", paste(rep("=", 60), collapse = ""), "\n")
cat("测试完成\n")