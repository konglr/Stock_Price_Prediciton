# ==============================================================================
# StockAI - 图表渲染模块
# ==============================================================================
# 本文件包含图表渲染相关的辅助函数
# ==============================================================================

# ------------------------------------------------------------------------------
# 创建数据单元格 (用于统计卡片)
# ------------------------------------------------------------------------------
#' 创建数据单元格 UI 组件
#' @param label 标签
#' @param value 值
#' @param value_color 值颜色
#' @param border 是否有右边框
#' @return div 元素
data_cell <- function(label, value, value_color = "#fff", border = TRUE) {
  border_style <- if (border) "border-right: 1px solid rgba(255,255,255,0.2);" else ""
  htmltools::div(
    class = "col",
    style = paste0("padding: 6px 8px; font-size: 0.8rem; ", border_style),
    htmltools::span(class = "text-white-50", style = "font-size: 0.65rem; margin-right: 4px;", label),
    htmltools::span(style = paste0("font-weight: 600; color: ", value_color, ";"), value)
  )
}

# ------------------------------------------------------------------------------
# 渲染市场统计卡片
# ------------------------------------------------------------------------------
#' 渲染市场统计信息 UI
#' @param stats get_stock_stats() 返回的统计列表
#' @return UI 元素
render_market_stats_ui <- function(stats) {
  if (is.null(stats)) return("等待数据加载...")
  
  # 涨跌颜色和图标
  change_color <- get_change_color(stats$change)
  change_icon <- get_change_icon(stats$change)
  
  # RSI 颜色
  rsi_color <- if (is.na(stats$rsi)) "#fff" else if (stats$rsi > 70) "#ff6b6b" else if (stats$rsi < 30) "#51cf66" else "#fff"
  
  # 量比
  vol_ratio <- if (!is.na(stats$vol_ratio)) sprintf("%.2fx", stats$vol_ratio) else "--"
  
  htmltools::div(
    class = "d-flex align-items-center",
    # 左侧：价格区域
    htmltools::div(
      style = "padding-right: 15px; margin-right: 15px; border-right: 1px solid rgba(255,255,255,0.2);",
      htmltools::div(sprintf("$%.2f", stats$price), style = "font-size: 1.5rem; font-weight: 700; color: #fff;"),
      htmltools::div(
        sprintf("%s %.2f (%+.2f%%)", change_icon, abs(stats$change), stats$pct_change),
        style = paste0("font-size: 0.85rem; font-weight: 600; color: ", change_color, ";")
      )
    ),
    # 右侧：数据表格
    htmltools::div(
      style = "flex: 1;",
      htmltools::div(
        class = "row g-0",
        data_cell("Open", sprintf("$%.2f", stats$open)),
        data_cell("High", sprintf("$%.2f", stats$high), "#51cf66"),
        data_cell("Low", sprintf("$%.2f", stats$low), "#ff6b6b"),
        data_cell("Vol", fmt_volume(stats$volume)),
        data_cell("Avg.Vol", fmt_volume(stats$avg_volume), "#fff", FALSE)
      ),
      htmltools::div(
        class = "row g-0 border-top",
        style = "border-top: 1px solid rgba(255,255,255,0.15);",
        data_cell("Ratio", vol_ratio),
        data_cell("52wk-H", sprintf("$%.2f", stats$wk52_high), "#51cf66"),
        data_cell("52wk-L", sprintf("$%.2f", stats$wk52_low), "#ff6b6b"),
        data_cell("SMA5", round(stats$sma5, 2)),
        data_cell("RSI", if (!is.na(stats$rsi)) round(stats$rsi, 1) else "--", rsi_color, FALSE)
      )
    )
  )
}

# ------------------------------------------------------------------------------
# 渲染收益率卡片
# ------------------------------------------------------------------------------
#' 渲染收益率概览 UI
#' @param perf get_performance_stats() 返回的收益率列表
#' @return UI 元素
render_performance_ui <- function(perf) {
  if (is.null(perf)) return("数据不足 (需1年以上历史)")
  
  fmt_ret <- function(val) {
    if (is.na(val)) return(htmltools::span("--", class = "text-muted"))
    color <- if (val >= 0) "#2ecc71" else "#e74c3c"
    htmltools::span(sprintf("%+.1f%%", val), style = paste0("color: ", color, "; font-weight: 600; font-size: 1.1rem;"))
  }
  
  htmltools::div(
    class = "d-flex justify-content-between text-center",
    htmltools::div(htmltools::div("近1月", class = "text-muted", style = "font-size: 0.75rem;"), fmt_ret(perf$ret_1m)),
    htmltools::div(htmltools::div("近3月", class = "text-muted", style = "font-size: 0.75rem;"), fmt_ret(perf$ret_3m)),
    htmltools::div(htmltools::div("近6月", class = "text-muted", style = "font-size: 0.75rem;"), fmt_ret(perf$ret_6m)),
    htmltools::div(htmltools::div("近1年", class = "text-muted", style = "font-size: 0.75rem;"), fmt_ret(perf$ret_1y))
  )
}

# ------------------------------------------------------------------------------
# 渲染 AI 研报 UI
# ------------------------------------------------------------------------------
#' 渲染 AI 研报 UI
#' @param prediction AI 预测结果列表
#' @param loading 是否加载中
#' @return UI 元素
render_ai_report_ui <- function(prediction, loading = FALSE) {
  if (loading) {
    return(htmltools::div(
      class = "d-flex justify-content-center p-5",
      htmltools::div(class = "spinner-border text-primary", role = "status")
    ))
  }
  
  if (is.null(prediction)) {
    return(htmltools::p("点击按钮启动 AI 联网投研深度分析", class = "text-muted p-3 text-center"))
  }
  
  if (!is.null(prediction$error)) {
    return(htmltools::div(class = "alert alert-danger", prediction$error))
  }
  
  htmltools::tagList(
    htmltools::div(
      class = "mb-3 d-flex align-items-center justify-content-between",
      htmltools::h5(paste0("综合研判：", prediction$trend), class = "text-primary fw-bold mb-0"),
      htmltools::span(class = "badge bg-success", "联网数据已接入")
    ),
    htmltools::div(
      class = "mb-3 d-flex align-items-center justify-content-between",
      htmltools::h5(paste0("综合研判：", prediction$trend), class = "text-primary fw-bold mb-0"),
      htmltools::span(class = "badge bg-success", "联网数据已接入")
    ),
    htmltools::div(
      class = "row g-3",
      htmltools::div(
        class = "col-md-6",
        htmltools::div(
          class = "card border-0 shadow-sm h-100",
          style = "background: #f8fbff;",
          htmltools::div(
            class = "card-body",
            htmltools::strong("\U0001F4F0 核心新闻动态"),
            htmltools::p(prediction$news, style = "font-size: 0.88rem; margin-top: 10px; color: #333;")
          )
        )
      ),
      htmltools::div(
        class = "col-md-6",
        htmltools::div(
          class = "card border-0 shadow-sm h-100",
          style = "background: #f8fbff;",
          htmltools::div(
            class = "card-body",
            htmltools::strong("\U0001F4B0 财务与估值评估"),
            htmltools::p(prediction$financial, style = "font-size: 0.88rem; margin-top: 10px; color: #333;")
          )
        )
      ),
      htmltools::div(
        class = "col-md-12",
        htmltools::div(
          class = "card border-0 shadow-sm",
          style = "background: #ffffff; border-left: 5px solid #0d6efd !important;",
          htmltools::div(
            class = "card-body",
            htmltools::strong("\U0001F9E0 专家投资逻辑"),
            htmltools::p(prediction$reasoning, style = "font-size: 0.92rem; margin-top: 10px;")
          )
        )
      ),
      htmltools::div(
        class = "col-md-6",
        htmltools::div(
          class = "card border-0 shadow-sm h-100",
          htmltools::div(
            class = "card-body text-center",
            htmltools::strong("未来 5 日价格走势预测"),
            htmltools::div(
              class = "mt-3 fw-bold text-primary",
              style = "letter-spacing: 1px;",
              paste(prediction$prediction_5d, collapse = " \u2192 ")
            )
          )
        )
      ),
      htmltools::div(
        class = "col-md-6",
        htmltools::div(
          class = "card border-0 shadow-sm h-100",
          style = "background: #eef2f7;",
          htmltools::div(
            class = "card-body py-2",
            htmltools::div(
              class = "d-flex justify-content-between mb-1",
              htmltools::span("建议操作", class = "small"),
              htmltools::span(prediction$trade_advice$action, class = "fw-bold text-danger")
            ),
            htmltools::div(
              class = "d-flex justify-content-between",
              htmltools::span("支撑/阻力", class = "small text-muted"),
              htmltools::span(paste0(prediction$support_level, " / ", prediction$resistance_level), class = "small fw-bold")
            )
          )
        )
      )
    )
  )
}