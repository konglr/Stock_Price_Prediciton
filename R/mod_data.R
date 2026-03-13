# ==============================================================================
# StockAI - 数据模块
# ==============================================================================
# 本文件包含股票数据获取和处理相关函数
# ==============================================================================

# ------------------------------------------------------------------------------
# 股票代码处理函数
# ------------------------------------------------------------------------------

#' 提取标准股票代码 (去除后缀)
#' @param ticker 原始代码 (如 600519.SS, 0700.HK)
#' @return 标准代码 (如 600519, 0700)
extract_standard_ticker <- function(ticker) {
  if (is.null(ticker) || ticker == "") return("")
  
  # 正则匹配：提取点号前的部分
  # 同时也处理特殊前缀如 ^IXIC
  std <- gsub("\\.(SS|SZ|HK|SHH|SHZ|HKG)$", "", ticker)
  return(std)
}

#' 根据数据源转换代码格式
#' @param ticker 标准代码
#' @param source 数据源 ("yahoo" 或 "alphavantage")
#' @return 格式化后的代码
convert_ticker_for_source <- function(ticker, source = "yahoo") {
  if (is.null(ticker) || ticker == "") return("")
  
  # 如果已经是带后缀的格式，先提取标准代码
  std_ticker <- extract_standard_ticker(ticker)
  
  # 获取后缀配置
  suffixes <- DATA_SOURCE_INFO[[source]]$suffixes
  
  # A 股识别逻辑
  if (grepl("^[603]", std_ticker) && nchar(std_ticker) == 6) {
    if (startsWith(std_ticker, "6")) {
      return(paste0(std_ticker, suffixes$sh)) # 上海
    } else {
      return(paste0(std_ticker, suffixes$sz)) # 深圳
    }
  }
  
  # 港股识别逻辑 (4-5位数字)
  if (grepl("^[0-9]+$", std_ticker) && (nchar(std_ticker) == 4 || nchar(std_ticker) == 5)) {
    # 港股通常需要补齐 4 位 (Yahoo) 或 5 位 (Alpha Vantage 可能需要)
    # 这里保持原有数字并加上后缀
    return(paste0(std_ticker, suffixes$hk))
  }
  
  # 美股或其他 (通常不需要后缀)
  return(std_ticker)
}

# ------------------------------------------------------------------------------
# 获取股票数据
# ------------------------------------------------------------------------------

#' 从指定数据源获取股票数据
#' @param ticker 股票代码
#' @param from 开始日期
#' @param to 结束日期
#' @param source 数据源 (默认 "yahoo")
#' @return xts OHLC 数据，失败返回 NULL
fetch_ticker_data <- function(ticker, from = Sys.Date() - 3650, to = Sys.Date(), source = "yahoo") {
  if (is.null(ticker) || ticker == "") return(NULL)

  # 转换代码格式
  target_ticker <- convert_ticker_for_source(ticker, source)

  tryCatch({
    if (source == "yahoo") {
      quantmod::getSymbols(
        target_ticker,
        from = from,
        to = to,
        auto.assign = FALSE,
        src = "yahoo"
      )
    } else if (source == "alphavantage") {
      fetch_alphavantage_data(target_ticker, from = from, to = to)
    } else if (source == "twelvedata") {
      fetch_twelvedata_data(target_ticker, from = from, to = to)
    } else if (source == "eastmoney") {
      # 东方财富需要判断市场类型
      market <- detect_market(ticker)
      fetch_eastmoney_data(ticker, market = market, from = from, to = to)
    } else {
      stop("不支持的数据源")
    }
  }, error = function(e) {
    message(paste("获取股票数据失败 (", source, "): ", e$message))
    return(NULL)
  })
}

#' 从 Alpha Vantage 获取股票数据
#' @param ticker 格式化后的股票代码
#' @param from 开始日期
#' @param to 结束日期
#' @return xts OHLC 数据
fetch_alphavantage_data <- function(ticker, from, to) {
  api_key <- Sys.getenv("ALPHA_VANTAGE_API_KEY")
  if (api_key == "") stop("未配置 ALPHA_VANTAGE_API_KEY 环境变量")
  
  # 构造请求 (使用 TIME_SERIES_DAILY，免费版限制)
  # - outputsize="compact": 最近 100 个数据点 (免费)
  # - outputsize="full": 完整历史 (付费)
  req <- request("https://www.alphavantage.co/query") |>
    req_url_query(
      `function` = "TIME_SERIES_DAILY",
      symbol = ticker,
      outputsize = "compact",
      apikey = api_key
    ) |>
    req_retry(max_tries = 3)
  
  resp <- req_perform(req)
  content <- resp_body_json(resp)
  
  # 检查频率限制或错误
  if (!is.null(content$Note)) {
    stop(paste("Alpha Vantage API 限制:", content$Note))
  }
  
  if (!is.null(content$`Error Message`)) {
    stop(paste("Alpha Vantage API 错误:", content$`Error Message`))
  }
  
  if (!is.null(content$Information)) {
    stop(paste("Alpha Vantage API 信息:", content$Information))
  }
  
  # 解析时间序列数据
  ts_key <- "Time Series (Daily)"
  if (is.null(content[[ts_key]])) {
    stop("未找到时间序列数据，请检查代码是否正确")
  }
  
  ts_data <- content[[ts_key]]
  
  # 转换为 data.frame
  df <- do.call(rbind, lapply(names(ts_data), function(date) {
    item <- ts_data[[date]]
    data.frame(
      Date = as.Date(date),
      Open = as.numeric(item$`1. open`),
      High = as.numeric(item$`2. high`),
      Low = as.numeric(item$`3. low`),
      Close = as.numeric(item$`4. close`),
      Volume = as.numeric(item$`5. volume`),
      stringsAsFactors = FALSE
    )
  }))
  
  # 排序并转换为 xts
  df <- df[order(df$Date), ]
  res_xts <- xts::xts(df[, -1], order.by = df$Date)
  
  # 裁剪日期范围
  res_xts <- res_xts[paste0(from, "/", to)]
  
  return(res_xts)
}

# ------------------------------------------------------------------------------
# 从 Twelve Data 获取股票数据
# ------------------------------------------------------------------------------
#' 从 Twelve Data 获取股票数据
#' @param ticker 格式化后的股票代码
#' @param from 开始日期
#' @param to 结束日期
#' @return xts OHLC 数据
fetch_twelvedata_data <- function(ticker, from, to) {
  api_key <- Sys.getenv("TWELVEDATA_API_KEY")
  if (api_key == "") stop("未配置 TWELVEDATA_API_KEY 环境变量")

  # 转换日期格式 (YYYY-MM-DD)
  from_str <- as.character(from)
  to_str <- as.character(to)

  # 构造请求
  req <- request("https://api.twelvedata.com/time_series") |>
    req_url_query(
      symbol = ticker,
      interval = "1day",
      start_date = from_str,
      end_date = to_str,
      adjusted = "true",  # 获取调整后数据（考虑分红、拆股等）
      apikey = api_key
    ) |>
    req_retry(max_tries = 3)

  resp <- req_perform(req)
  content <- resp_body_json(resp)

  # 检查错误
  if (!is.null(content$code) && content$code != 200) {
    stop(paste("Twelve Data API 错误:", content$message))
  }

  if (is.null(content$values)) {
    stop("未找到时间序列数据，请检查代码是否正确")
  }

  # 解析数据
  ts_data <- content$values

  # 转换为 data.frame (按日期升序)
  df <- do.call(rbind, lapply(seq_along(ts_data), function(i) {
    item <- ts_data[[i]]
    data.frame(
      Date = as.Date(item$datetime),
      Open = as.numeric(item$open),
      High = as.numeric(item$high),
      Low = as.numeric(item$low),
      Close = as.numeric(item$close),
      Volume = as.numeric(item$volume),
      stringsAsFactors = FALSE
    )
  }))

  # 排序并转换为 xts
  df <- df[order(df$Date), ]
  res_xts <- xts::xts(df[, -1], order.by = df$Date)

  # 裁剪日期范围
  res_xts <- res_xts[paste0(from, "/", to)]

  return(res_xts)
}

# ------------------------------------------------------------------------------
# 市场类型识别
# ------------------------------------------------------------------------------
#' 自动识别股票代码所属市场
#' @param ticker 股票代码
#' @return 市场类型: "A", "HK", "US"
detect_market <- function(ticker) {
  if (is.null(ticker) || ticker == "") return("A")

  # 去除后缀
  std_ticker <- extract_standard_ticker(ticker)

  # 港股: 4-5位数字 (如 00700, 0700, 9988)
  if (grepl("^[0-9]+$", std_ticker) && nchar(std_ticker) >= 4 && nchar(std_ticker) <= 5) {
    return("HK")
  }

  # A股: 6位数字，以 0, 3, 6 开头
  if (grepl("^[036][0-9]{5}$", std_ticker)) {
    return("A")
  }

  # 美股: 字母为主 (如 AAPL, MSFT)
  if (grepl("^[A-Za-z]+$", std_ticker)) {
    return("US")
  }

  # 默认返回 A 股
  return("A")
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
  
  # 统一列名格式
  # Alpha Vantage 返回 5 列，Yahoo 返回 6 列
  if (ncol(data) == 5) {
    colnames(data) <- c("Open", "High", "Low", "Close", "Volume")
  } else if (ncol(data) >= 6) {
    colnames(data) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
  }
  
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