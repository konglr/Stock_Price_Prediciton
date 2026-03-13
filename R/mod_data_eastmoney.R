# ==============================================================================
# 东方财富 A股/美股数据获取
# ==============================================================================

library(httr)
library(jsonlite)

# ------------------------------------------------------------------------------
# 获取单只股票的 K 线历史
# ------------------------------------------------------------------------------
#' 从东方财富获取 K 线数据
#' @param code 股票代码 (如 "600519", "AAPL", "00700")
#' @param market 市场: "A" (A股), "US" (美股), "HK" (港股)
#' @param klt K线类型: 101=日线, 102=周线, 103=月线
#' @param fqt 复权类型: 0=不复权, 1=前复权, 2=后复权 (仅A股有效)
#' @param from 开始日期 (YYYYMMDD)
#' @param to 结束日期 (YYYYMMDD)
#' @return data.frame 包含 K 线数据
get_eastmoney_kline <- function(code, market = "A", klt = 101, fqt = 1,
                                 from = "19900101",
                                 to = format(Sys.Date(), "%Y%m%d")) {

  # 根据市场判断 secid
  if (market == "A") {
    # A股: 上海=1.代码, 深圳=0.代码
    if (substr(code, 1, 1) == "6") {
      secid <- paste0("1.", code)
    } else {
      secid <- paste0("0.", code)
    }
  } else if (market == "US") {
    # 美股: 105.代码
    secid <- paste0("105.", code)
  } else if (market == "HK") {
    # 港股: 116.代码(5位补零) - 优先使用 116. 主板
    # 如果失败，代码会返回 NULL，外部可尝试 128. 创业板
    code_hk <- formatC(as.numeric(code), width = 5, format = "d", flag = "0")
    secid <- paste0("116.", code_hk)
  } else if (market == "COMMODITY") {
    # 大宗商品: 112.代码
    secid <- paste0("112.", code)
  } else {
    stop("不支持的市场: ", market, " (支持: A, US, HK, COMMODITY)")
  }

  # 构造请求
  query_params <- list(
    fields1 = "f1",
    fields2 = "f51,f52,f53,f54,f55,f56,f57,f61",
    beg = from,
    end = to,
    rtntype = "2",
    secid = secid,
    klt = as.character(klt),
    fqt = as.character(fqt)
  )

  url <- "https://push2his.eastmoney.com/api/qt/stock/kline/get"

  tryCatch({
    response <- GET(url, query = query_params, add_headers(`User-Agent` = "Mozilla/5.0"))
    data <- content(response, as = "text", encoding = "UTF-8")
    data <- fromJSON(data)

    if (is.null(data$data) || is.null(data$data$klines)) {
      message(paste("股票", code, "未获取到 K 线数据"))
      return(NULL)
    }

    # 解析 klines 数据
    klines <- data$data$klines
    fields <- c("date", "open", "close", "high", "low", "volume", "amount", "turnover")

    # 转换为 data.frame
    df_list <- lapply(klines, function(x) {
      parts <- strsplit(x, ",")[[1]]
      row <- as.data.frame(t(parts), stringsAsFactors = FALSE)
      return(row)
    })

    df <- do.call(rbind, df_list)
    colnames(df) <- fields

    # 添加股票代码
    df$code <- code

    # 转换日期格式
    df$date <- as.Date(df$date)

    # 转换数值列
    numeric_cols <- c("open", "close", "high", "low", "volume")
    for (col in numeric_cols) {
      df[[col]] <- as.numeric(df[[col]])
    }
    df$amount <- as.numeric(df$amount)
    df$turnover <- as.numeric(df$turnover)

    return(df)

  }, error = function(e) {
    message(paste("获取", code, "数据时出错:", e$message))
    return(NULL)
  })
}

# ------------------------------------------------------------------------------
# 便捷函数: 获取日线数据
# ------------------------------------------------------------------------------
#' 获取东方财富股票数据
#' @param code 股票代码 (如 "600519", "AAPL", "00700")
#' @param market 市场: "A" (A股), "US" (美股), "HK" (港股)
#' @param from 开始日期 (YYYY-MM-DD)
#' @param to 结束日期 (YYYY-MM-DD)
#' @param adjusted 是否前复权 (仅A股有效，默认 TRUE)
#' @return xts 对象
#' @export
fetch_eastmoney_data <- function(code, market = "A", from = NULL, to = NULL, adjusted = TRUE) {
  if (is.null(from)) from <- "1990-01-01"
  if (is.null(to)) to <- Sys.Date()

  # 转换日期格式
  from_str <- gsub("-", "", from)
  to_str <- gsub("-", "", to)

  # fqt: 1=前复权, 0=不复权
  fqt <- if (adjusted) 1 else 0

  # 获取数据
  df <- get_eastmoney_kline(code, market = market, klt = 101, fqt = fqt,
                             from = from_str, to = to_str)

  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  # 转换为 xts
  df <- df[order(df$date), ]
  res_xts <- xts::xts(df[, c("open", "high", "low", "close", "volume")],
                      order.by = df$date)

  return(res_xts)
}