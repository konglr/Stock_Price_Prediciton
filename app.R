# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyWidgets)
library(tidyquant)
library(ggplot2)
library(quantmod)
library(TTR)
library(htmltools)
library(bslib)
library(httr2)
library(jsonlite)

# 定义 UI
ui <- page_sidebar(
  header = tags$head(
    HTML('
      <!-- Global site tag (gtag.js) - Google Analytics -->
      <script async src="https://www.googletagmanager.com/gtag/js?id=G-8LL329L0WC"></script>
      <script>
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag("js", new Date());
        gtag("config", "G-8LL329L0WC");
      </script>
    ')
  ),
  
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "StockAI - Gemini 股票智能预测",
  
  sidebar = sidebar(
    width = 300,
    pickerInput(
      inputId = "ticker_preset",
      label = "常用股票选择",
      choices = c("AAPL", "AMZN", "GOOGL", "MSFT", "TSLA", "NVDA", "SQQQ", "^IXIC", "000001.SZ"),
      selected = "AAPL",
      options = list(`live-search` = TRUE)
    ),
    
    textInput(
      inputId = "ticker_custom",
      label = "手动输入股票代码",
      value = "AAPL"
    ),
    
    radioButtons(
      inputId = "period",
      label = "时间跨度 (Time Period)",
      choices = c("20天" = 20, "1个月" = 30, "3个月" = 90, "6个月" = 180, "1年" = 365),
      selected = 30
    ),
    
    selectInput(
      inputId = "plot_type",
      label = "图表类型 (Plot Type)",
      choices = c("折线图" = "line", "条形图" = "bars", "蜡烛图" = "candlesticks", "针状图" = "matchsticks"),
      selected = "candlesticks"
    ),
    
    hr(),
    h5("Gemini AI 预测模型"),
    actionButton("run_ai", "运行 Gemini AI 预测", class = "btn-primary w-100"),
    
    hr(),
    checkboxInput("show_points", "在图表标记极值点 (addPoints)", TRUE),
    checkboxInput("show_prediction", "显示历史数据明细", TRUE)
  ),
  
  # 主界面内容
  div(
    class = "d-flex flex-column", 
    layout_column_wrap(
      width = 1/2,
      fill = FALSE, 
      gap = "10px",
      style = "margin-bottom: 10px;", 
      # 左边：关键统计指标
      value_box(
        title = "最新交易数据",
        value = uiOutput("vbox_market_stats"),
        showcase = bsicons::bs_icon("bar-chart-fill"),
        theme = "primary"
      ),
      # 右边：多阶段收益率
      value_box(
        title = "收益率概览",
        value = uiOutput("vbox_performance"),
        showcase = bsicons::bs_icon("graph-up-arrow"),
        theme = "light"
      )
    ),
    
    card(
      full_screen = TRUE,
      style = "height: 50vh; min-height: 500px;", 
      card_header("股票价格趋势分析 (quantmod)"),
      card_body(
        padding = 0, 
        plotOutput(outputId = "plot", height = "100%") 
      )
    ),
    
    card(
      style = "margin-top: 10px;",
      card_header("Gemini AI 智能分析报告"),
      card_body(uiOutput("ai_report_ui"))
    ),
    
    conditionalPanel(
      condition = "input.show_prediction == true",
      card(
        style = "margin-top: 10px;",
        card_header("历史数据明细 (最近10日)"),
        tableOutput(outputId = "data")
      )
    )
  )
)

# 定义 Server
server <- function(input, output, session) {
  
  options(HTTPUserAgent = "Mozilla/5.0")
  apiKey = "AIzaSyBZhIF9oMieIuk4VMV-Qg_hsuP1hpNz6Y8" # 环境填充
  
  observeEvent(input$ticker_preset, {
    updateTextInput(session, "ticker_custom", value = input$ticker_preset)
  })
  
  current_ticker <- reactive({
    req(input$ticker_custom)
    toupper(input$ticker_custom)
  })
  
  ticker_data <- reactive({
    ticker <- current_ticker()
    tryCatch({
      getSymbols(ticker, from = Sys.Date() - 600, to = Sys.Date(), auto.assign = FALSE, src = "yahoo")
    }, error = function(e) return(NULL))
  })
  
  # 1. 关键统计指标
  output$vbox_market_stats <- renderUI({
    data <- ticker_data()
    if (is.null(data) || nrow(data) < 2) return("等待数据...")
    
    rows <- tail(data, 2)
    latest_row <- rows[2]
    prev_row <- rows[1]
    
    cl <- as.numeric(Cl(latest_row))
    prev_cl <- as.numeric(Cl(prev_row))
    
    diff <- cl - prev_cl
    pct_diff <- (diff / prev_cl) * 100
    diff_color <- if(diff >= 0) "#2ecc71" else "#e74c3c" 
    
    op <- as.numeric(Op(latest_row))
    hi <- as.numeric(Hi(latest_row))
    lo <- as.numeric(Lo(latest_row))
    vol <- as.numeric(Vo(latest_row))
    
    data_52w <- tail(data, 252)
    hi_52w <- max(Hi(data_52w), na.rm = TRUE)
    lo_52w <- min(Lo(data_52w), na.rm = TRUE)
    
    avg_vol_30d <- mean(tail(Vo(data), 30), na.rm = TRUE)
    amplitude <- ((hi - lo) / prev_cl) * 100
    
    div(
      class = "w-100",
      style = "display: flex; align-items: center; justify-content: space-between;",
      div(
        style = "min-width: 150px;",
        span(style="font-size: 2.4rem; font-weight: 800; display: block; line-height: 1;", paste0("$", round(cl, 2))),
        span(style=paste0("font-size: 1.1rem; font-weight: 600; color: ", diff_color, ";"),
             sprintf("%+.2f (%+.2f%%)", diff, pct_diff))
      ),
      div(style = "width: 1px; height: 80px; background: rgba(255,255,255,0.3); margin: 0 20px;"),
      div(
        style = "flex-grow: 1; display: grid; grid-template-columns: 1fr 1fr; gap: 4px 25px; font-size: 0.8rem; line-height: 1.4;",
        div(style="display: flex; justify-content: space-between; border-bottom: 1px solid rgba(255,255,255,0.1);", 
            span("开盘", style="opacity: 0.8;"), span(round(op, 2), style="font-weight: 600;")),
        div(style="display: flex; justify-content: space-between; border-bottom: 1px solid rgba(255,255,255,0.1);", 
            span("成交量", style="opacity: 0.8;"), span(paste0(round(vol/1e6, 2), "M"), style="font-weight: 600;")),
        div(style="display: flex; justify-content: space-between; border-bottom: 1px solid rgba(255,255,255,0.1);", 
            span("当日最高", style="opacity: 0.8;"), span(round(hi, 2), style="font-weight: 600;")),
        div(style="display: flex; justify-content: space-between; border-bottom: 1px solid rgba(255,255,255,0.1);", 
            span("30日均量", style="opacity: 0.8;"), span(paste0(round(avg_vol_30d/1e6, 2), "M"), style="font-weight: 600;")),
        div(style="display: flex; justify-content: space-between; border-bottom: 1px solid rgba(255,255,255,0.1);", 
            span("当日最低", style="opacity: 0.8;"), span(round(lo, 2), style="font-weight: 600;")),
        div(style="display: flex; justify-content: space-between; border-bottom: 1px solid rgba(255,255,255,0.1);", 
            span("52W 最高", style="opacity: 0.8;"), span(round(hi_52w, 2), style="font-weight: 600;")),
        div(style="display: flex; justify-content: space-between; border-bottom: 1px solid rgba(255,255,255,0.1);", 
            span("当日振幅", style="opacity: 0.8;"), span(sprintf("%.2f%%", amplitude), style="font-weight: 600;")),
        div(style="display: flex; justify-content: space-between; border-bottom: 1px solid rgba(255,255,255,0.1);", 
            span("52W 最低", style="opacity: 0.8;"), span(round(lo_52w, 2), style="font-weight: 600;"))
      )
    )
  })
  
  # 2. 阶段收益率
  output$vbox_performance <- renderUI({
    data <- ticker_data()
    if (is.null(data) || nrow(data) < 2) return("--")
    
    calc_ret <- function(d, days) {
      if(nrow(d) <= days) return(NA)
      curr <- as.numeric(tail(Cl(d), 1))
      prev <- as.numeric(Cl(d)[nrow(d) - days])
      ((curr - prev) / prev) * 100
    }
    
    periods <- c(7, 30, 60, 90, 180, 360)
    labels <- c("7天", "30天", "60天", "90天", "180天", "1年")
    
    items <- lapply(seq_along(periods), function(i) {
      val <- calc_ret(data, periods[i])
      color <- if(is.na(val)) "text-muted" else if(val >= 0) "text-success" else "text-danger"
      div(style = "flex: 1; text-align: center; border-right: 1px solid #eee; last-child: border-none;",
          div(style = "font-size: 0.65rem; color: #666; font-weight: bold;", labels[i]),
          div(class = color, style = "font-weight: 800; font-size: 0.85rem;", 
              if(is.na(val)) "--" else sprintf("%+.1f%%", val))
      )
    })
    
    div(class = "d-flex justify-content-between w-100", style="padding-top: 5px;", items)
  })
  
  # Gemini AI 逻辑
  ai_prediction <- reactiveVal(NULL)
  ai_loading <- reactiveVal(FALSE)
  
  observeEvent(input$run_ai, {
    data <- ticker_data()
    if (is.null(data)) return()
    
    ai_loading(TRUE)
    ai_prediction(NULL) # 重置状态
    
    recent_data <- tail(data, 360)
    data_summary <- paste(capture.output(print(recent_data)), collapse = "\n")
    
    system_prompt <- "你是一位拥有20年经验的资深美股投资专家。
    任务：基于用户提供的过去360个交易日数据，进行多维度的技术和量价分析。
    要求返回 JSON 格式，包含：
    - trend: 简短描述当前趋势
    - prediction_5d: 预测未来 5 个交易日的估计收盘价数组
    - reasoning: 详细的投资逻辑分析
    - support_level: 主要支撑位置价格
    - resistance_level: 主要阻力位置价格
    - trade_advice: 交易策略对象，包含 action, buy_price, take_profit, stop_loss。"
    
    user_query <- paste0("股票代码: ", current_ticker(), "\n交易历史明细数据：\n", data_summary)
    
    tryCatch({
        req_method("POST") %>%
        req_body_json(list(
          contents = list(list(parts = list(list(text = user_query)))), 
          systemInstruction = list(parts = list(list(text = system_prompt))), 
          generationConfig = list(responseMimeType = "application/json")
        )) %>%
        req_retry(max_tries = 3) %>%
        req_perform()
      
      raw_text <- resp_body_json(resp)$candidates[[1]]$content$parts[[1]]$text
      parsed_res <- fromJSON(raw_text)
      ai_prediction(parsed_res)
      
    }, error = function(e) {
      ai_prediction(list(error = paste("AI 分析失败:", e$message)))
    })
    ai_loading(FALSE)
  })
  
  output$ai_report_ui <- renderUI({
    if (ai_loading()) {
      return(div(class="d-flex justify-content-center p-5", div(class="spinner-border text-primary", role="status")))
    }
    
    res <- ai_prediction()
    if (is.null(res)) {
      return(p("点击按钮启动资深投研分析", class="text-muted p-3 text-center"))
    }
    
    if (!is.null(res$error)) {
      return(div(class="alert alert-danger", as.character(res$error)))
    }
    
    trend_text <- if(!is.null(res$trend)) as.character(res$trend) else "未知趋势"
    reasoning_text <- if(!is.null(res$reasoning)) as.character(res$reasoning) else "暂无逻辑分析"
    pred_text <- if(!is.null(res$prediction_5d)) paste(res$prediction_5d, collapse = " → ") else "暂无预测数据"
    support_val <- if(!is.null(res$support_level)) as.character(res$support_level) else "--"
    resistance_val <- if(!is.null(res$resistance_level)) as.character(res$resistance_level) else "--"
    
    tagList(
      h5(paste0("趋势研判：", trend_text), class="text-primary fw-bold mb-3"),
      div(class="row g-3",
          # 左栏：专家逻辑分析
          div(class="col-md-4",
              div(class="card border-0 h-100", style="background: rgba(0,0,0,0.03);",
                  div(class="card-body p-3",
                      strong("AI专家逻辑分析"),
                      p(reasoning_text, style="font-size: 0.9rem; margin-top: 8px; color: #444;")
                  )
              )
          ),
          # 中栏：预测趋势
          div(class="col-md-4",
              div(class="card border-0 h-100", style="background: rgba(0,0,0,0.03);",
                  div(class="card-body p-3 text-center",
                      strong("未来5日预测趋势"),
                      div(style="margin-top: 15px; font-weight: 600; color: #555;", pred_text),
                      div(style="margin-top: 10px;",
                          bsicons::bs_icon("arrow-right-circle", size = "1.5rem", class="text-primary")
                      )
                  )
              )
          ),
          # 右栏：交易建议
          div(class="col-md-4",
              div(class="card border-0 h-100", style="background: rgba(0,0,0,0.03);",
                  div(class="card-body p-3",
                      strong("关键点位与实战建议"),
                      div(class="mt-2 d-flex flex-column gap-2",
                          div(class="d-flex justify-content-between border-bottom pb-1", 
                              span("主要支撑", class="small text-muted"), span(support_val, class="fw-bold text-success")),
                          div(class="d-flex justify-content-between border-bottom pb-1", 
                              span("主要阻力", class="small text-muted"), span(resistance_val, class="fw-bold text-danger")),
                          div(class="d-flex justify-content-between border-bottom pb-1", 
                              span("操作建议", class="small text-muted"), span(as.character(res$trade_advice$action), class="fw-bold text-info")),
                          div(class="d-flex justify-content-between border-bottom pb-1", 
                              span("建议入场", class="small text-muted"), span(as.character(res$trade_advice$buy_price), class="fw-bold")),
                          div(class="d-flex justify-content-between border-bottom pb-1", 
                              span("止盈目标", class="small text-muted"), span(as.character(res$trade_advice$take_profit), class="fw-bold text-success")),
                          div(class="d-flex justify-content-between", 
                              span("止损参考", class="small text-muted"), span(as.character(res$trade_advice$stop_loss), class="fw-bold text-danger"))
                      )
                  )
              )
          )
      )
    )
  })
  
  # 图表渲染
  output$plot <- renderPlot({
    data <- ticker_data()
    if (is.null(data)) return(NULL)
    
    n_days <- as.numeric(input$period)
    subset_str <- paste("last", n_days, "days")
    
    chartSeries(data, 
                type = input$plot_type, 
                name = current_ticker(), 
                theme = chartTheme("white"),
                subset = subset_str,
                TA = c(addVo(),
                       addSMA(n = 5, col = "brown"),
                       addSMA(n = 20, col = "orange"),
                       addSMA(n = 60, col = "purple"),
                       addSMA(n = 120, col = "blue"))) 
    
    if(input$show_points) {
      subset_data <- data[subset_str]
      if(!is.null(subset_data) && nrow(subset_data) > 0) {
        max_val <- max(Hi(subset_data), na.rm = TRUE)
        min_val <- min(Lo(subset_data) , na.rm = TRUE)
        max_idx <- which(Hi(subset_data) == max_val)[1]
        min_idx <- which(Lo(subset_data) == min_val)[1]
        all_dates <- index(subset_data)
        max_vals <- rep(NA, length(all_dates))
        min_vals <- rep(NA, length(all_dates))
        max_vals[max_idx] <- max_val
        min_vals[min_idx] <- min_val
        pts_max <- xts(max_vals, order.by = all_dates)
        pts_min <- xts(min_vals, order.by = all_dates)
        addPoints(pts_max, pch = 19, col = "red", cex = 1.5, on = 1)
        addPoints(pts_min, pch = 19, col = "darkgreen", cex = 1.5, on = 1)
        addLines(h = as.numeric(max_val), col = "red", on = 1)
        addLines(h = as.numeric(min_val), col = "darkgreen", on = 1)
      }
    }
  })
  
  output$data <- renderTable({
    data <- ticker_data(); if (is.null(data)) return(NULL)
    data.frame(Date = as.character(index(tail(data, 10))), coredata(tail(data, 10)))
  }, striped = TRUE, hover = TRUE)
}

# Run app
shinyApp(ui = ui, server = server)