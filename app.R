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
  title = "StockAI - Gemini 驱动的股票智能预测",
  
  sidebar = sidebar(
    width = 300,
    pickerInput(
      inputId = "ticker_preset",
      label = "常用股票选择",
      choices = c("AAPL", "AMZN", "GOOGL", "MSFT", "TSLA", "NVDA", "SQQQ", ".IXIC", "000001.SZ"),
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
      # 左边：最新交易数据
      value_box(
        title = "最新交易数据",
        value = uiOutput("vbox_market_stats"),
        showcase = bsicons::bs_icon("bar-chart-fill"),
        theme = "primary"
      ),
      # 右边：阶段收益率概览
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
  apiKey = "" # 环境填充
  
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
  
  # 1. 关键统计指标与变动数据
  output$vbox_market_stats <- renderUI({
    data <- ticker_data()
    if (is.null(data) || nrow(data) < 2) return("等待数据...")
    
    last_row <- tail(data, 1)
    prev_row <- tail(data, 2)[1, ]
    
    cl <- as.numeric(Cl(last_row))
    op <- as.numeric(Op(last_row))
    hi <- as.numeric(Hi(last_row))
    lo <- as.numeric(Lo(last_row))
    vol <- as.numeric(Vo(last_row))
    
    # 变动金额与百分比 (相对于前一交易日收盘)
    prev_cl <- as.numeric(Cl(prev_row))
    change_val <- cl - prev_cl
    change_pct <- (change_val / prev_cl) * 100
    
    # 52周范围
    data_52w <- tail(data, 252)
    hi_52w <- max(Hi(data_52w), na.rm = TRUE)
    lo_52w <- min(Lo(data_52w), na.rm = TRUE)
    
    # 平均成交量 (30日)
    avg_vol_30d <- mean(tail(Vo(data), 30), na.rm = TRUE)
    
    # 振幅
    amplitude <- ((hi - lo) / prev_cl) * 100
    
    color_class <- if(change_val >= 0) "text-success" else "text-danger"
    change_icon <- if(change_val >= 0) "▲" else "▼"
    
    div(
      # 第一行：现价与涨跌
      div(style="display: flex; align-items: baseline; gap: 10px;",
          div(style="font-size: 1.8rem; font-weight: bold;", paste0("$", round(cl, 2))),
          div(class = color_class, style="font-size: 1.1rem; font-weight: bold;", 
              sprintf("%s %.2f (%+.2f%%)", change_icon, abs(change_val), change_pct))
      ),
      # 详细指标排版
      div(style="font-size: 0.85rem; line-height: 1.4; margin-top: 5px; color: rgba(255,255,255,0.9);",
          div(style="display: flex; justify-content: space-between; border-bottom: 1px solid rgba(255,255,255,0.2); padding: 2px 0;",
              span(strong("开盘: "), round(op, 2)),
              span(strong("成交量: "), paste0(round(vol/1e6, 2), "M"))
          ),
          div(style="display: flex; justify-content: space-between; border-bottom: 1px solid rgba(255,255,255,0.2); padding: 2px 0;",
              span(strong("当日范围: "), paste0(round(lo, 2), " - ", round(hi, 2))),
              span(strong("平均成交量: "), paste0(round(avg_vol_30d/1e6, 2), "M"))
          ),
          div(style="display: flex; justify-content: space-between; padding: 2px 0;",
              span(strong("52周范围: "), paste0(round(lo_52w, 2), " - ", round(hi_52w, 2))),
              span(strong("振幅: "), sprintf("%.2f%%", amplitude))
          )
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
    labels <- c("7天", "30天", "60天", "90天", "180天", "360天")
    
    items <- lapply(seq_along(periods), function(i) {
      val <- calc_ret(data, periods[i])
      color <- if(is.na(val)) "text-muted" else if(val >= 0) "text-success" else "text-danger"
      div(style = "flex: 1; text-align: center; border-right: 1px solid #eee;",
          div(style = "font-size: 0.7rem; color: #666; font-weight: bold;", labels[i]),
          div(class = color, style = "font-weight: bold; font-size: 0.9rem;", 
              if(is.na(val)) "--" else sprintf("%+.2f%%", val))
      )
    })
    
    div(class = "d-flex justify-content-between w-100", style="padding-top: 10px;", items)
  })
  
  # Gemini AI 逻辑
  ai_prediction <- reactiveVal(NULL)
  ai_loading <- reactiveVal(FALSE)
  observeEvent(input$run_ai, {
    data <- ticker_data(); if (is.null(data)) return()
    ai_loading(TRUE); ai_prediction("分析中...")
    recent_data <- tail(data, 30); data_summary <- paste(capture.output(print(recent_data)), collapse = "\n")
    system_prompt <- "分析股票JSON: trend, prediction(5), reasoning, trade_advice(action, buy_price, take_profit, stop_loss)。"
    user_query <- paste0("股票: ", current_ticker(), "\n数据：\n", data_summary)
    tryCatch({
      resp <- request("https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-preview-09-2025:generateContent") %>%
        req_url_query(key = apiKey) %>% req_method("POST") %>%
        req_body_json(list(contents = list(list(parts = list(list(text = user_query)))), systemInstruction = list(parts = list(list(text = system_prompt))), generationConfig = list(responseMimeType = "application/json"))) %>%
        req_perform()
      ai_prediction(fromJSON(resp_body_json(resp)$candidates[[1]]$content$parts[[1]]$text))
    }, error = function(e) ai_prediction(list(error = "AI 暂时离线")))
    ai_loading(FALSE)
  })
  
  output$ai_report_ui <- renderUI({
    res <- ai_prediction(); if (is.null(res)) return(p("点击按钮运行分析", class="text-muted"))
    if (ai_loading()) return(div(class="spinner-border text-primary"))
    tagList(h6("趋势: ", res$trend, class="text-primary fw-bold"), p(res$reasoning), hr(), div(class="row", div(class="col-6", strong("建议: "), res$trade_advice$action), div(class="col-6", strong("入场: "), res$trade_advice$buy_price)))
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
                       addSMA(n = 20, col = "orange"))) 
    
    if(input$show_points) {
      subset_data <- data[subset_str]
      if(!is.null(subset_data) && nrow(subset_data) > 0) {
        max_val <- max(Hi(subset_data), na.rm = TRUE)
        min_val <- min(Lo(subset_data), na.rm = TRUE)
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