# ==============================================================================
# StockAI - 数据模块
# ==============================================================================
# 本文件包含股票数据获取和处理相关函数
# ==============================================================================

# ------------------------------------------------------------------------------
# 获取股票数据
# ------------------------------------------------------------------------------
#' 从 Yahoo Finance 获取股票数据
#' @param ticker 股票代码
#' @param from 开始日期
#' @param to 结束日期
#' @return xts OHLC 数据，失败返回 NULL
fetch_ticker_data <- function(ticker, from = Sys.Date() - 3650, to = Sys.Date()) {
  if (is.null(ticker) || ticker == "") return(NULL)
  
  tryCatch({
    quantmod::getSymbols(
      ticker,
      from = from,
      to = to,
      auto.assign = FALSE,
      src = "yahoo"
    )
  }, error = function(e) {
    message(paste("获取股票数据失败:", e$message))
    return(NULL)
  })
}

# ------------------------------------------------------------------------------
# 转换数据周期
# ------------------------------------------------------------------------------
#' 将日线数据转换为指定周期
#' @param data xts OHLC 数据
#' @param interval 周期类型 (daily, weekly, monthly, yearly)
#' @return 转换后的 xts 数据
convert_interval <- function(data, interval = "daily") {
  if (is.null(data)) return(NULL)
  
  switch(interval,
    "daily"   = data,
    "weekly"  = quantmod::to.weekly(data, indexAt = "lastof", OHLC = TRUE),
    "monthly" = quantmod::to.monthly(data, indexAt = "lastof", OHLC = TRUE),
    "yearly"  = quantmod::to.yearly(data, indexAt = "lastof", OHLC = TRUE),
    data
  )
}

# ------------------------------------------------------------------------------
# 处理股票数据
# ------------------------------------------------------------------------------
#' 处理股票数据：转换周期、统一列名
#' @param data xts OHLC 数据
#' @param interval 周期类型
#' @return 处理后的 xts 数据
process_ticker_data <- function(data, interval = "daily") {
  if (is.null(data)) return(NULL)
  
  # 转换周期
  data <- convert_interval(data, interval)
  
  # 确保列名格式统一 (Yahoo Finance 格式)
  colnames(data) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  
  data
}

# ------------------------------------------------------------------------------
# 计算收益率
# ------------------------------------------------------------------------------
#' 计算指定天数后的收益率
#' @param data xts OHLC 数据
#' @param days 天数
#' @return 收益率百分比
calc_return <- function(data, days) {
  if (is.null(data) || nrow(data) < days) return(NA)
  
  current_price <- as.numeric(Cl(tail(data, 1)))
  old_price <- as.numeric(Cl(tail(data, days)[1]))
  
  (current_price - old_price) / old_price * 100
}

# ------------------------------------------------------------------------------
# 获取股票统计信息
# ------------------------------------------------------------------------------
#' 获取最新交易统计信息
#' @param data xts OHLC 数据
#' @return 包含统计信息的列表
get_stock_stats <- function(data) {
  if (is.null(data) || nrow(data) < 2) return(NULL)
  
  latest <- tail(data, 1)
  prev <- tail(data, 2)[1]
  
  # 价格数据
  price <- as.numeric(Cl(latest))
  open_price <- as.numeric(Op(latest))
  high_price <- as.numeric(Hi(latest))
  low_price <- as.numeric(Lo(latest))
  volume <- as.numeric(Vo(latest))
  change <- price - as.numeric(Cl(prev))
  pct_change <- (change / as.numeric(Cl(prev))) * 100
  
  # 计算平均成交量 (20日)
  avg_volume <- mean(tail(Vo(data), 20), na.rm = TRUE)
  
  # 计算52周最高/最低 (约252个交易日)
  data_1y <- tail(data, min(252, nrow(data)))
  wk52_high <- max(Hi(data_1y), na.rm = TRUE)
  wk52_low <- min(Lo(data_1y), na.rm = TRUE)
  
  # 简单的技术指标计算
  sma5 <- mean(tail(Cl(data), 5), na.rm = TRUE)
  sma20 <- mean(tail(Cl(data), 20), na.rm = TRUE)
  rsi <- as.numeric(tail(TTR::RSI(Cl(data), n = 14), 1))
  
  list(
    price       = price,
    open        = open_price,
    high        = high_price,
    low         = low_price,
    volume      = volume,
    avg_volume  = avg_volume,
    change      = change,
    pct_change  = pct_change,
    wk52_high   = wk52_high,
    wk52_low    = wk52_low,
    sma5        = sma5,
    sma20       = sma20,
    rsi         = rsi,
    vol_ratio   = if (!is.na(volume) && !is.na(avg_volume) && avg_volume > 0) volume / avg_volume else NA
  )
}

# ------------------------------------------------------------------------------
# 获取多阶段收益率
# ------------------------------------------------------------------------------
#' 获取 1月/3月/6月/1年收益率
#' @param data xts OHLC 数据
#' @return 包含收益率信息的列表
get_performance_stats <- function(data) {
  if (is.null(data) || nrow(data) < 252) return(NULL)
  
  current_price <- as.numeric(Cl(tail(data, 1)))
  
  list(
    ret_1m = calc_return(data, 21),
    ret_3m = calc_return(data, 63),
    ret_6m = calc_return(data, 126),
    ret_1y = calc_return(data, 252)
  )
}