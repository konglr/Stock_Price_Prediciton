# ==============================================================================
# StockAI - 回测模块
# ==============================================================================
# 本文件包含回测系统相关的辅助函数
# ==============================================================================

# ------------------------------------------------------------------------------
# 格式化回测数值
# ------------------------------------------------------------------------------
#' 格式化回测数值 (带千位分隔符)
#' @param x 数值
#' @return 格式化后的字符串
fmt_backtest_value <- function(x) {
  format(round(as.numeric(x), 2), big.mark = ",")
}

# ------------------------------------------------------------------------------
# 渲染回测绩效统计 UI
# ------------------------------------------------------------------------------
#' 渲染回测绩效统计 UI
#' @param results 回测结果列表
#' @return UI 元素
render_backtest_stats_ui <- function(results) {
  if (is.null(results)) {
    return(htmltools::p("待回测完成后生成报告...", class = "text-muted"))
  }
  
  final_equity <- tail(results$equity, 1)
  init_equity <- results$equity[1]
  profit <- final_equity - init_equity
  roi <- (profit / init_equity) * 100
  
  bslib::layout_column_wrap(
    width = 1/4,
    bslib::value_box(
      title = "初始本金",
      value = sprintf("$%s", fmt_backtest_value(init_equity)),
      theme = "secondary"
    ),
    bslib::value_box(
      title = "累计盈亏",
      value = sprintf("$%s (%+.2f%%)", fmt_backtest_value(profit), roi),
      theme = if (profit >= 0) "success" else "danger"
    ),
    bslib::value_box(
      title = "最终净值",
      value = sprintf("$%s", fmt_backtest_value(final_equity)),
      theme = "primary"
    ),
    bslib::value_box(
      title = "交易次数",
      value = nrow(results$log[results$log$Action == "SELL", ]),
      theme = "info"
    )
  )
}

# ------------------------------------------------------------------------------
# 渲染回测交易记录表格
# ------------------------------------------------------------------------------
#' 渲染回测交易记录表格
#' @param results 回测结果列表
#' @return 数据框
render_backtest_trade_log <- function(results) {
  if (is.null(results) || nrow(results$log) == 0) {
    return(data.frame(Message = "尚无交易记录。"))
  }
  
  df <- results$log
  data.frame(
    日期     = as.character(df$Date),
    动作     = df$Action,
    成交价   = round(df$Price, 2),
    数量     = round(df$Shares, 2),
    账户余额 = round(df$Total, 2),
    原因     = df$Reason,
    盈亏     = sprintf("%+.2f", df$PnL),
    "盈亏%"  = sprintf("%+.2f%%", df$PnL_Pct),
    check.names = FALSE
  )
}

# ------------------------------------------------------------------------------
# 加载策略代码模板
# ------------------------------------------------------------------------------
#' 从文件加载策略代码模板
#' @param strategy_name 策略名称
#' @return 策略代码字符串
load_strategy_template <- function(strategy_name) {
  if (strategy_name == "custom") return("")
  
  file_path <- sprintf("trading_strategy/%s.R", strategy_name)
  if (file.exists(file_path)) {
    code_lines <- readLines(file_path, warn = FALSE)
    paste(code_lines, collapse = "\n")
  } else {
    ""
  }
}

# ------------------------------------------------------------------------------
# 运行回测
# ------------------------------------------------------------------------------
#' 运行简单回测
#' @param data xts OHLC 数据
#' @param strategy_code 策略代码
#' @param init_capital 初始本金
#' @param trade_pct 交易比例
#' @param stop_loss_pct 止损比例
#' @param take_profit_pct 止盈比例
#' @return 回测结果列表
run_backtest <- function(data, strategy_code, init_capital = 10000, trade_pct = 20, 
                         stop_loss_pct = 0, take_profit_pct = 0) {
  
  # 调用 functions/backtest.R 中的函数
  if (exists("run_simple_backtest", mode = "function")) {
    run_simple_backtest(
      data            = data,
      strategy_code   = strategy_code,
      init_capital    = init_capital,
      trade_pct       = trade_pct,
      stop_loss_pct   = stop_loss_pct,
      take_profit_pct = take_profit_pct
    )
  } else {
    stop("run_simple_backtest function not found. Please load functions/backtest.R")
  }
}