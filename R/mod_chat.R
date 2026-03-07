# ==============================================================================
# StockAI - AI 金融助手模块
# ==============================================================================
# 本文件包含 AI 金融助手相关的辅助函数
# ==============================================================================

# ------------------------------------------------------------------------------
# 构建股票数据文本
# ------------------------------------------------------------------------------
#' 构建用于 AI 分析的股票数据文本
#' @param data xts OHLC 数据
#' @param ticker 股票代码
#' @return 股票数据文本
build_stock_data_text <- function(data, ticker) {
  if (is.null(data) || nrow(data) == 0) return("")
  
  latest <- tail(data, 1)
  prev <- tail(data, 2)[1]
  price <- as.numeric(Cl(latest))
  change <- price - as.numeric(Cl(prev))
  pct_change <- (change / as.numeric(Cl(prev))) * 100
  
  # 计算技术指标
  sma5 <- mean(tail(Cl(data), 5), na.rm = TRUE)
  sma20 <- mean(tail(Cl(data), 20), na.rm = TRUE)
  rsi <- as.numeric(tail(TTR::RSI(Cl(data), n = 14), 1))
  
  # 格式化 K 线数据
  kline_text <- format_kline_data(data, 30)
  
  paste0(
    "\n\n当前股票数据 (", ticker, "):\n",
    "- 最新价格: ", round(price, 2), "\n",
    "- 今日涨跌: ", round(change, 2), " (", round(pct_change, 2), "%)\n",
    "- 技术指标: SMA5=", round(sma5, 2), ", SMA20=", round(sma20, 2), ", RSI14=", round(rsi, 1), "\n",
    "- 最近 30 天 K 线数据:\n", kline_text
  )
}

# ------------------------------------------------------------------------------
# 运行 AI 聊天分析
# ------------------------------------------------------------------------------
#' 运行 AI 聊天分析
#' @param user_input 用户输入
#' @param history 聊天历史
#' @param question_info 问题分析结果
#' @param ticker 股票代码
#' @param data xts OHLC 数据
#' @param provider AI 提供商
#' @param model_id 模型 ID
#' @param temperature 温度参数
#' @param max_tokens 最大 token 数
#' @param enable_search 是否启用联网搜索
#' @return AI 响应文本
run_ai_chat <- function(user_input, history, question_info, ticker, data,
                         provider, model_id, temperature = 0.7, max_tokens = 1024,
                         enable_search = FALSE) {
  
  need_data <- question_info$need_data
  
  # 构建股票数据文本
  stock_data_text <- ""
  if (need_data && !is.null(data) && nrow(data) > 0) {
    stock_data_text <- build_stock_data_text(data, ticker)
  }
  
  # 构建系统提示词
  system_prompt <- build_chat_system_prompt(need_data, stock_data_text)
  
  # 调用 AI API
  result <- call_ai_api_chat(
    provider      = provider,
    model_id      = model_id,
    messages      = history,
    temperature   = temperature,
    max_tokens    = max_tokens,
    enable_search = enable_search,
    system_prompt = system_prompt
  )
  
  result$text
}

# ------------------------------------------------------------------------------
# 处理用户消息
# ------------------------------------------------------------------------------
#' 处理用户消息的主函数
#' @param user_input 用户输入
#' @param current_ticker 当前股票代码
#' @param current_data 当前股票数据
#' @param provider AI 提供商
#' @param model_id 模型 ID
#' @param temperature 温度参数
#' @param max_tokens 最大 token 数
#' @param enable_search 是否启用联网搜索
#' @return 包含 response, q_info, needs_switch 的列表
handle_chat_message <- function(user_input, current_ticker, current_data,
                                 provider, model_id, temperature = 0.7, max_tokens = 1024,
                                 enable_search = FALSE) {
  
  # 分析用户问题
  q_info <- analyze_user_question(
    user_question  = user_input,
    current_ticker = current_ticker,
    provider       = provider,
    model_id       = model_id,
    temperature    = 0.3
  )
  
  # 判断是否需要切换股票
  needs_switch <- q_info$need_data && q_info$switch_stock && 
                  q_info$ticker != current_ticker && 
                  q_info$ticker != "GENERAL"
  
  if (needs_switch) {
    # 返回切换信息
    return(list(
      response    = paste0("\U0001F504 识别到新股票：**", q_info$ticker, "**，正在加载数据..."),
      q_info      = q_info,
      needs_switch = TRUE,
      new_ticker  = q_info$ticker
    ))
  }
  
  # 构建聊天历史 (包含当前用户消息)
  history <- list(list(role = "user", content = user_input))
  
  # 运行 AI 聊天
  if (q_info$need_data) {
    status_msg <- "\U0001F50D 正在分析当前股票数据..."
  } else {
    status_msg <- NULL
  }
  
  response <- run_ai_chat(
    user_input    = user_input,
    history       = history,
    question_info = q_info,
    ticker        = current_ticker,
    data          = current_data,
    provider      = provider,
    model_id      = model_id,
    temperature   = temperature,
    max_tokens    = max_tokens,
    enable_search = enable_search
  )
  
  list(
    response     = response,
    q_info       = q_info,
    needs_switch = FALSE,
    status_msg   = status_msg
  )
}