library(httr2)
library(jsonlite)

# 1. 配置参数
apiKey <- "AIzaSyBZhIF9oMieIuk4VMV-Qg_hsuP1hpNz6Y8" # 在此处填入您的 API Key（Canvas 环境下通常会自动处理）
model_endpoint <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"

# 2. 构建联网搜索请求
# 修正：由于 jsonlite 可能无法直接访问 empty_object，我们使用结构化 list 配合编码技巧
# 确保在 JSON 中呈现为 {"google_search": {}}
search_request <- list(
  contents = list(
    list(
      role = "user",
      parts = list(list(text = "请搜索并告诉我今天（2025年12月24日）纳斯达克指数的最新点位和主要涨跌原因。"))
    )
  ),
  tools = list(
    list(
      google_search = setNames(list(), character(0)) # 产生空对象 {} 的标准 R 做法
    )
  ),
  generationConfig = list(
    temperature = 0.5
  )
)

# 3. 发送请求
tryCatch({
  resp <- request(model_endpoint) %>%
    req_url_query(key = apiKey) %>%
    req_method("POST") %>%
    req_body_json(search_request) %>%
    req_retry(max_tries = 3) %>%
    req_perform()
  
  # 4. 解析结果
  result <- resp_body_json(resp)
  
  # 提取 AI 回复文本
  ai_text <- result$candidates[[1]]$content$parts[[1]]$text
  
  cat("--- Gemini 联网搜索测试结果 ---\n")
  if (is.null(ai_text)) {
    cat("AI 未返回直接文本内容，可能正在处理搜索数据。\n")
  } else {
    cat(ai_text, "\n")
  }
  
  # 检查是否有引用来源 (Grounding Metadata)
  metadata <- result$candidates[[1]]$groundingMetadata
  if (!is.null(metadata)) {
    cat("\n--- 搜索来源与参考 (Grounding) ---\n")
    
    # 打印搜索状态
    if (!is.null(metadata$searchEntryPoint$renderedContent)) {
      cat("搜索功能已成功触发并获取实时数据。\n")
    }
    
    # 打印具体来源链接
    attributions <- metadata$groundingAttributions
    if (!is.null(attributions)) {
      for (s in attributions) {
        # 尝试从 web 对象提取标题和链接
        title <- s$web$title %||% "网页来源"
        uri <- s$web$uri %||% "无链接"
        cat(sprintf("来源: %s\n链接: %s\n\n", title, uri))
      }
    }
  }
  
}, error = function(e) {
  cat("请求失败，详细错误分析：\n")
  if (!is.null(e$response)) {
    cat("HTTP 状态码:", resp_status(e$response), "\n")
    # 打印原始响应以检查具体的 JSON 验证错误
    error_detail <- resp_body_string(e$response)
    cat("API 报错详情:", error_detail, "\n")
  } else {
    print(e)
  }
})

# 辅助函数：处理 NULL 值
`%||%` <- function(a, b) if (!is.null(a)) a else b