# ==============================================================================
# StockAI - 辅助函数模块
# ==============================================================================
# 本文件包含项目中使用的通用辅助函数和操作符
# ==============================================================================

# ------------------------------------------------------------------------------
# NULL 合并操作符
# ------------------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ------------------------------------------------------------------------------
# 格式化成交量
# ------------------------------------------------------------------------------
#' 将成交量格式化为易读格式 (K, M, B)
#' @param v 成交量数值
#' @return 格式化后的字符串
fmt_volume <- function(v) {
  if (is.na(v)) return("--")
  if (v >= 1e9) sprintf("%.2fB", v / 1e9)
  else if (v >= 1e6) sprintf("%.2fM", v / 1e6)
  else if (v >= 1e3) sprintf("%.2fK", v / 1e3)
  else sprintf("%.0f", v)
}

# ------------------------------------------------------------------------------
# 格式化价格
# ------------------------------------------------------------------------------
#' 将价格格式化为美元格式
#' @param p 价格数值
#' @param digits 小数位数
#' @return 格式化后的字符串
fmt_price <- function(p, digits = 2) {
  if (is.na(p)) return("--")
  sprintf("$%s", format(round(p, digits), nsmall = digits, big.mark = ","))
}

# ------------------------------------------------------------------------------
# 格式化百分比
# ------------------------------------------------------------------------------
#' 将数值格式化为百分比
#' @param val 数值 (如 0.05 或 5)
#' @param is_decimal 是否已经是小数形式 (默认 FALSE，假设输入已经是百分比数值)
#' @return 格式化后的字符串 (如 "+5.00%")
fmt_pct <- function(val, is_decimal = FALSE) {
  if (is.na(val)) return("--")
  pct_val <- if (is_decimal) val * 100 else val
  sprintf("%+.2f%%", pct_val)
}

# ------------------------------------------------------------------------------
# 获取涨跌颜色
# ------------------------------------------------------------------------------
#' 根据涨跌返回对应颜色
#' @param change 涨跌数值
#' @param up_color 上涨颜色
#' @param down_color 下跌颜色
#' @return 颜色字符串
get_change_color <- function(change, up_color = "#51cf66", down_color = "#ff6b6b") {
  if (is.na(change)) return("#fff")
  if (change >= 0) up_color else down_color
}

# ------------------------------------------------------------------------------
# 获取涨跌图标
# ------------------------------------------------------------------------------
#' 根据涨跌返回对应图标
#' @param change 涨跌数值
#' @return 图标字符串 (▲ 或 ▼)
get_change_icon <- function(change) {
  if (is.na(change)) return("--")
  if (change >= 0) "▲" else "▼"
}

# ------------------------------------------------------------------------------
# 格式化 K 线数据
# ------------------------------------------------------------------------------
#' 将 K 线数据格式化为文本
#' @param data xts OHLC 数据
#' @param days 天数
#' @return 格式化后的字符串
format_kline_data <- function(data, days = 30) {
  if (is.null(data) || nrow(data) < 2) return("")
  recent <- tail(data, days)
  lines <- character(nrow(recent))
  for (i in seq_len(nrow(recent))) {
    row <- recent[i, ]
    date_str <- format(index(row), "%Y-%m-%d")
    o <- round(as.numeric(Op(row)), 2)
    h <- round(as.numeric(Hi(row)), 2)
    l <- round(as.numeric(Lo(row)), 2)
    c <- round(as.numeric(Cl(row)), 2)
    v <- round(as.numeric(Vo(row)) / 1000000, 2)
    lines[i] <- sprintf("%s O:%.2f H:%.2f L:%.2f C:%.2f V:%.2fM", date_str, o, h, l, c, v)
  }
  paste(lines, collapse = "\n")
}

# ------------------------------------------------------------------------------
# 安全获取数值
# ------------------------------------------------------------------------------
#' 安全地从可能为 NULL 或 NA 的值中获取数值
#' @param x 值
#' @param default 默认值
#' @return 数值或默认值
safe_numeric <- function(x, default = 0) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(default)
  as.numeric(x)
}

# ------------------------------------------------------------------------------
# JSON 提取辅助函数
# ------------------------------------------------------------------------------
#' 从文本中提取 JSON 对象
#' @param text 包含 JSON 的文本
#' @return 解析后的列表，或 NULL
extract_json <- function(text) {
  text <- trimws(text)
  json_match <- regmatches(text, regexpr("\\{[^{}]*\\}", text))
  if (length(json_match) > 0) {
    tryCatch(
      return(jsonlite::fromJSON(json_match[1])),
      error = function(e) return(NULL)
    )
  }
  NULL
}

# ------------------------------------------------------------------------------
# 获取模型友好名称
# ------------------------------------------------------------------------------
#' 获取模型的友好显示名称
#' @param model_id 模型 ID
#' @return 友好名称
get_model_friendly_name <- function(model_id) {
  switch(model_id,
    "gemini-3.1-flash-lite-preview" = "Gemini 3.1 Flash Lite",
    "MiniMax-M2.5"         = "MiniMax-M2.5",
    "qwen3.5-plus"         = "Qwen 3.5 Plus",
    "qwen3-max-2026-01-23" = "Qwen 3 Max",
    "qwen3-coder-next"     = "Qwen 3 Coder Next",
    "qwen3-coder-plus"     = "Qwen 3 Coder Plus",
    "glm-5"                = "GLM-5",
    "glm-4.7"              = "GLM-4.7",
    "kimi-k2-5"            = "Kimi K2.5",
    model_id
  )
}