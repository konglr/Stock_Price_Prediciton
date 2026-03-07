# ==============================================================================
# StockAI - AI 研报模块
# ==============================================================================
# 本文件包含 AI 研报生成相关的函数
# ==============================================================================

# ------------------------------------------------------------------------------
# 构建 AI 研报提示词
# ------------------------------------------------------------------------------
#' 构建 AI 联网分析提示词
#' @param ticker 股票代码
#' @return 提示词字符串
build_ai_report_prompt <- function(ticker) {
  paste0(
    "你是一位顶尖的华尔街量化投资专家。请针对股票 ", ticker, " 进行深度联网投研分析。",
    "\n请根据当前市场环境、近期新闻、财务状况和技术面，提供以下 JSON 格式的回复：",
    "\n{",
    "\n  \"trend\": \"看多/看空/中性\",",
    "\n  \"news\": \"最近 3 条核心新闻摘要及影响\",",
    "\n  \"financial\": \"关键财务指标评估（营收、PE、PB 等）\",",
    "\n  \"reasoning\": \"投资逻辑深度推导（200字以内）\",",
    "\n  \"prediction_5d\": [\"第一天价格\", \"第二天价格\", ...],",
    "\n  \"support_level\": \"支撑位\",",
    "\n  \"resistance_level\": \"压力位\",",
    "\n  \"trade_advice\": { \"action\": \"买入/持有/卖出\", \"buy_price\": \"建议入场价\" }",
    "\n}",
    "\n\n只返回 JSON 格式，不要包含任何 Markdown 代码块包裹（不要有 ```json 标签）。"
  )
}

# ------------------------------------------------------------------------------
# 运行 AI 研报分析
# ------------------------------------------------------------------------------
#' 运行 AI 联网分析
#' @param ticker 股票代码
#' @param provider AI 提供商
#' @param model_id 模型 ID
#' @param temperature 温度参数
#' @param max_tokens 最大 token 数
#' @param enable_search 是否启用联网搜索
#' @return 包含 prediction 和 grounding 的列表
run_ai_report <- function(ticker, provider, model_id, temperature = 0.7, max_tokens = 1024, enable_search = FALSE) {
  prompt <- build_ai_report_prompt(ticker)
  
  result <- call_ai_api(
    provider     = provider,
    model_id     = model_id,
    prompt       = prompt,
    temperature  = temperature,
    max_tokens   = max_tokens,
    enable_search = enable_search
  )
  
  # 解析 JSON 响应
  prediction <- extract_json(result$text)
  if (is.null(prediction)) {
    # 尝试直接解析
    tryCatch({
      prediction <- jsonlite::fromJSON(result$text)
    }, error = function(e) {
      prediction <- list(error = paste("JSON 解析失败:", e$message))
    })
  }
  
  list(
    prediction = prediction,
    grounding  = result$grounding
  )
}

# ------------------------------------------------------------------------------
# 构建股票问题分析提示词
# ------------------------------------------------------------------------------
#' 构建股票问题分析提示词
#' @param user_question 用户问题
#' @param current_ticker 当前股票代码
#' @return 提示词字符串
build_question_analysis_prompt <- function(user_question, current_ticker = NULL) {
  current_ticker_info <- if (!is.null(current_ticker) && current_ticker != "") {
    paste0("\n\n当前用户正在查看的股票是：", current_ticker)
  } else {
    ""
  }
  
  paste0(
    "你是一个股票问题分析助手，需要返回 JSON 格式的分析结果。",
    "\n\n用户问题是：\"", user_question, "\"",
    current_ticker_info,
    "\n\n请分析用户问题，返回以下信息（JSON 格式）：",
    "\n1. ticker: 用户提到的股票代码（Yahoo Finance 格式），如果没有提到则返回当前股票或 \"GENERAL\"",
    "\n2. need_data: 用户是否需要股票数据来回答问题（true/false）",
    "\n3. switch_stock: 是否需要切换到新股票（true/false，仅当用户明确提到新股票时为 true）",
    "\n\nYahoo Finance 股票代码格式：",
    "\n- 美股：GOOGL、AAPL、TSLA、NVDA 等",
    "\n- A 股上海：601988.SS、600519.SS 等",
    "\n- A 股深圳：000001.SZ、002594.SZ 等",
    "\n- 港股：0700.HK、9988.HK 等",
    "\n\n判断 need_data 的规则：",
    "\n- 用户询问股票分析、技术面、走势、买卖建议等 → need_data = true",
    "\n- 用户询问股票代码对应的 公司名称 → need_data = false",
    "\n- 用户问\"这个股票怎么样\"等指代当前股票的问题 → need_data = true",
    "\n- 用户闲聊、问概念性问题（如\"什么是 RSI\"）→ need_data = false",
    "\n\n判断 switch_stock 的规则：",
    "\n- 用户明确提到新的股票名称或代码 → switch_stock = true",
    "\n- 用户说\"这个股票\"、\"当前股票\"等指代 → switch_stock = false",
    "\n- 用户没有提到具体股票 → switch_stock = false",
    "\n\n只返回 JSON，格式如：{\"ticker\": \"AAPL\", \"need_data\": true, \"switch_stock\": false}"
  )
}

# ------------------------------------------------------------------------------
# 分析用户问题类型
# ------------------------------------------------------------------------------
#' 分析用户问题是否涉及股票
#' @param user_question 用户问题
#' @param current_ticker 当前股票代码
#' @param provider AI 提供商
#' @param model_id 模型 ID
#' @param temperature 温度参数
#' @return 包含 ticker, need_data, switch_stock 的列表
analyze_user_question <- function(user_question, current_ticker = NULL, provider, model_id, temperature = 0.3) {
  prompt <- build_question_analysis_prompt(user_question, current_ticker)
  
  result <- call_ai_api(
    provider    = provider,
    model_id    = model_id,
    prompt      = prompt,
    temperature = temperature,
    max_tokens  = 500
  )
  
  # 解析 JSON 响应
  q_info <- extract_json(result$text)
  if (is.null(q_info)) {
    # 返回默认值
    q_info <- list(ticker = "GENERAL", need_data = FALSE, switch_stock = FALSE)
  }
  
  q_info
}

# ------------------------------------------------------------------------------
# 构建聊天系统提示词
# ------------------------------------------------------------------------------
#' 构建聊天系统提示词
#' @param need_data 是否需要数据
#' @param stock_data_text 股票数据文本
#' @return 系统提示词
build_chat_system_prompt <- function(need_data = FALSE, stock_data_text = "") {
  if (need_data && nchar(stock_data_text) > 0) {
    paste0(
      "你是一位专业的金融投资顾问 AI 助手。我已经为你提供了该股票的实时数据和 K 线数据，请基于这些数据进行分析和回答。不要说你'没有数据'或'无法获取数据'。",
      stock_data_text
    )
  } else {
    "你是一位专业的金融投资顾问 AI 助手。请用中文回答��户的问题，保持专业和详细。"
  }
}