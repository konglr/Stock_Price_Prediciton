# ==============================================================================
# StockAI - 技术指标模块
# ==============================================================================
# 本文件包含技术指标计算和添加到图表的函数
# ==============================================================================

# ------------------------------------------------------------------------------
# 获取技术指标列表
# ------------------------------------------------------------------------------
#' 返回可用的技术指标列表
#' @return 指标名称向量
get_indicator_list <- function() {
  c("SMA", "BBands", "MACD", "RSI", "ADX", "SAR", "OBV", "MFI", "CLV", "TR", "ATR", "SuperTrend")
}

# ------------------------------------------------------------------------------
# 添加 SMA (简单移动平均)
# ------------------------------------------------------------------------------
#' 添加 SMA 指标到图表
#' @param cs chart_Series 对象
#' @param data xts OHLC 数据
#' @return 更新后的 chart_Series 对象
add_indicator_sma <- function(cs, data) {
  if (nrow(data) >= 5) {
    cs <- add_SMA(n = 5, col = "blue")
  }
  if (nrow(data) >= 20) {
    cs <- add_SMA(n = 20, col = "red")
  }
  cs
}

# ------------------------------------------------------------------------------
# 添加 BBands (布林带)
# ------------------------------------------------------------------------------
#' 添加布林带指标到图表
#' @param cs chart_Series 对象
#' @param data xts OHLC 数据
#' @param n 周期
#' @return 更新后的 chart_Series 对象
add_indicator_bbands <- function(cs, data, n = 20) {
  if (nrow(data) >= n) {
    cs <- add_BBands(n = n)
  }
  cs
}

# ------------------------------------------------------------------------------
# 添加 MACD
# ------------------------------------------------------------------------------
#' 添加 MACD 指标到图表
#' @param cs chart_Series 对象
#' @param data xts OHLC 数据
#' @return 更新后的 chart_Series 对象
add_indicator_macd <- function(cs, data) {
  if (nrow(data) >= 26) {
    cs <- add_MACD()
  }
  cs
}

# ------------------------------------------------------------------------------
# 添加 RSI (相对强弱指标)
# ------------------------------------------------------------------------------
#' 添加 RSI 指标到图表
#' @param cs chart_Series 对象
#' @param data xts OHLC 数据
#' @param n 周期
#' @return 更新后的 chart_Series 对象
add_indicator_rsi <- function(cs, data, n = 14) {
  if (nrow(data) >= n) {
    cs <- add_RSI(n = n)
  }
  cs
}

# ------------------------------------------------------------------------------
# 添加 ADX (平均趋向指标)
# ------------------------------------------------------------------------------
#' 添加 ADX 指标到图表
#' @param cs chart_Series 对象
#' @param data xts OHLC 数据
#' @param n 周期
#' @return 更新后的 chart_Series 对象
add_indicator_adx <- function(cs, data, n = 14) {
  if (nrow(data) >= n) {
    cs <- add_ADX(n = n)
  }
  cs
}

# ------------------------------------------------------------------------------
# 添加 SAR (抛物线转向)
# ------------------------------------------------------------------------------
#' 添加 SAR 指标到图表 (主图叠加)
#' @param cs chart_Series 对象
#' @param data xts OHLC 数据
#' @return 更新后的 chart_Series 对象
add_indicator_sar <- function(cs, data) {
  if (nrow(data) >= 5) {
    sar_values <- SAR(HLC(data))
    cs <- add_TA(sar_values, on = 1, col = "purple", lwd = 2)
  }
  cs
}

# ------------------------------------------------------------------------------
# 添加 OBV (能量潮)
# ------------------------------------------------------------------------------
#' 添加 OBV 指标到图表 (新面板)
#' @param cs chart_Series 对象
#' @param data xts OHLC 数据
#' @return 更新后的 chart_Series 对象
add_indicator_obv <- function(cs, data) {
  if (nrow(data) >= 2) {
    obv_values <- OBV(Cl(data), Vo(data))
    cs <- add_TA(obv_values, col = "blue", lwd = 2)
  }
  cs
}

# ------------------------------------------------------------------------------
# 添加 MFI (资金流量指标)
# ------------------------------------------------------------------------------
#' 添加 MFI 指标到图表
#' @param cs chart_Series 对象
#' @param data xts OHLC 数据
#' @param n 周期
#' @return 更新后的 chart_Series 对象
add_indicator_mfi <- function(cs, data, n = 14) {
  if (nrow(data) >= n) {
    mfi_values <- MFI(HLC(data), Vo(data), n = n)
    cs <- add_TA(mfi_values, col = "orange", lwd = 2)
  }
  cs
}

# ------------------------------------------------------------------------------
# 添加 CLV (收盘位置值)
# ------------------------------------------------------------------------------
#' 添加 CLV 指标到图表
#' @param cs chart_Series 对象
#' @param data xts OHLC 数据
#' @return 更新后的 chart_Series 对象
add_indicator_clv <- function(cs, data) {
  clv_values <- CLV(HLC(data))
  cs <- add_TA(clv_values, col = "darkgreen", lwd = 2)
  cs
}

# ------------------------------------------------------------------------------
# 添加 TR (真实波幅)
# ------------------------------------------------------------------------------
#' 添加 TR 指标到图表
#' @param cs chart_Series 对象
#' @param data xts OHLC 数据
#' @return 更新后的 chart_Series 对象
add_indicator_tr <- function(cs, data) {
  if (nrow(data) >= 2) {
    tr_values <- TR(HLC(data))[, "tr"]  # 提取 tr 列
    cs <- add_TA(tr_values, col = "brown", lwd = 2)
  }
  cs
}

# ------------------------------------------------------------------------------
# 添加 ATR (平均真实波幅)
# ------------------------------------------------------------------------------
#' 添加 ATR 指标到图表
#' @param cs chart_Series 对象
#' @param data xts OHLC 数据
#' @param n 周期
#' @return 更新后的 chart_Series 对象
add_indicator_atr <- function(cs, data, n = 14) {
  if (nrow(data) >= n) {
    atr_values <- ATR(HLC(data), n = n)[, "atr"]  # 提取 atr 列
    cs <- add_TA(atr_values, col = "red", lwd = 2)
  }
  cs
}

# ------------------------------------------------------------------------------
# 添加 SuperTrend
# ------------------------------------------------------------------------------
#' 添加 SuperTrend 指标到图表 (主图叠加)
#' @param cs chart_Series 对象
#' @param data xts OHLC 数据
#' @param n ATR 周期
#' @param factor 乘数因子
#' @return 更新后的 chart_Series 对象
add_indicator_supertrend <- function(cs, data, n = 10, factor = 3) {
  # SuperTrend 需要自定义函数支持
  if (exists("SuperTrend", mode = "function")) {
    st_res <- SuperTrend(HLC(data), n = n, factor = factor)
    st_line <- st_res$supertrend
    st_dir <- st_res$direction
    
    # 分离上涨和下跌趋势线
    st_up <- st_line
    st_up[st_dir == -1] <- NA
    st_down <- st_line
    st_down[st_dir == 1] <- NA
    
    cs <- add_TA(st_up, on = 1, col = "green", lwd = 2)
    cs <- add_TA(st_down, on = 1, col = "red", lwd = 2)
  }
  cs
}

# ------------------------------------------------------------------------------
# 批量添加技术指标
# ------------------------------------------------------------------------------
#' 根据选择批量添加技术指标到图表
#' @param cs chart_Series 对象
#' @param data xts OHLC 数据
#' @param indicators 指标名称向量
#' @return 更新后的 chart_Series 对象
add_indicators <- function(cs, data, indicators) {
  if (is.null(indicators) || length(indicators) == 0) return(cs)
  
  # 标准指标 (使用 add_ 前缀)
  if ("SMA" %in% indicators) cs <- add_indicator_sma(cs, data)
  if ("BBands" %in% indicators) cs <- add_indicator_bbands(cs, data)
  if ("MACD" %in% indicators) cs <- add_indicator_macd(cs, data)
  if ("RSI" %in% indicators) cs <- add_indicator_rsi(cs, data)
  if ("ADX" %in% indicators) cs <- add_indicator_adx(cs, data)
  
  # 需要使用 add_TA() 的指标
  if ("SAR" %in% indicators) cs <- add_indicator_sar(cs, data)
  if ("OBV" %in% indicators) cs <- add_indicator_obv(cs, data)
  if ("MFI" %in% indicators) cs <- add_indicator_mfi(cs, data)
  if ("CLV" %in% indicators) cs <- add_indicator_clv(cs, data)
  if ("TR" %in% indicators) cs <- add_indicator_tr(cs, data)
  if ("ATR" %in% indicators) cs <- add_indicator_atr(cs, data)
  if ("SuperTrend" %in% indicators) cs <- add_indicator_supertrend(cs, data)
  
  cs
}