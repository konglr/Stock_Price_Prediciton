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

# 加载自定义外部函数
source("functions/supertrend.R")
source("functions/backtest.R")

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
  title = "StockAI - Gemini股票智能预测",
  
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
    
    sliderTextInput(
      inputId = "period",
      label = "时间跨度 (Time Period)",
      choices = c("20天", "1月", "3月", "6月", "1年", "2年", "5年", "10年"),
      selected = "1年",
      grid = TRUE
    ),
    
    selectInput(
      inputId = "interval",
      label = "K线周期 (Interval)",
      choices = c("日" = "daily", "周" = "weekly", "月" = "monthly", "年" = "yearly"),
      selected = "daily"
    ),
    
    selectInput(
      inputId = "plot_type",
      label = "图表类型 (Plot Type)",
      choices = c("折线图" = "line", "条形图" = "bars", "蜡烛图" = "candlesticks", "针状图" = "matchsticks"),
      selected = "candlesticks"
    ),
    
    hr(),
    hr(),
    h5("技术指标 (Technical Indicators)"),
    checkboxGroupInput(
      inputId = "indicators",
      label = NULL,
      choices = c(
        "SMA (简单移动平均)" = "SMA",
        "BBands (布林带)" = "BBands",
        "MACD" = "MACD",
        "RSI (相对强弱指标)" = "RSI",
        "ADX (平均趋向指标)" = "ADX",
        "SAR (抛物线转向)" = "SAR",
        "OBV (能量潮)" = "OBV",
        "MFI (资金流量指标)" = "MFI",
        "CLV (收盘位置值)" = "CLV",
        "TR (真实波幅)" = "TR",
        "ATR (平均真实波幅)" = "ATR",
        "SuperTrend" = "SuperTrend"
      ),
      selected = c("SMA")
    ),
    
    hr(),
    h5("Gemini AI 预测模型"),
    # 新增：模型选择列表
    selectInput(
      inputId = "ai_model",
      label = "选择 AI 模型",
      choices = c(
        "Gemini 3.0 Flash (最新)" = "gemini-3-flash",
        "Gemini 2.5 Flash (平衡)" = "gemini-2.5-flash",
        "Gemini 2.5 Flash-lite (更快)" = "gemini-2.5-flash-lite",
        "Gemini 2.0 Flash (更快)" = "gemini-2.0-flash-exp",
        "Gemini 1.5 Pro " = "gemini-1.5-pro",
        "Gemini 1.5 Flash" = "gemini-1.5-flash"
      ),
      selected = "gemini-2.5-flash"
    ),
    # 修改：显示当前模型状态
    div(
      class = "mb-2",
      span("当前使用: ", style = "font-size: 0.8rem; color: #666;"),
      uiOutput("selected_model_badge", inline = TRUE)
    ),
    actionButton("run_ai", "运行 AI 全球联网预测", class = "btn-primary w-100"),
    

    
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
      # 左边：关键统计指标与技术面
      value_box(
        title = "最新交易与技术指标",
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
      style = "height: 70vh; min-height: 500px;", 
      card_header("股票价格趋势分析 (quantmod)"),
      card_body(
        padding = 0, 
        plotOutput(outputId = "plot", height = "100%") 
      )
    ),
    
    card(
      style = "margin-top: 10px;",
      card_header("Gemini AI 深度研报 (含实时新闻与财务评估)"),
      card_body(uiOutput("ai_report_ui"))
    ),
    
    card(
      style = "margin-top: 10px;",
      card_header("交易策略回测系统 (Backtesting Suite)"),
      navset_tab(
        nav_panel(
          title = "1. 策略与资金配置",
          div(class = "p-3",
              layout_column_wrap(
                width = 1/2,
                div(
                  h6("资金管理"),
                  numericInput("init_capital", "初始资金 (USD)", value = 10000, min = 100),
                  sliderInput("trade_size", "单笔交易仓位 (%)", min = 1, max = 100, value = 20)
                ),
                div(
                  h6("策略引擎与逻辑编辑器"),
                  layout_column_wrap(
                    width = 1/2,
                    div(
                      selectInput("bt_strategy", "快速代码模板", 
                                  choices = c("自定义代码" = "custom",
                                             "SuperTrend 趋势追踪" = "supertrend", 
                                             "ADX + BBands 趋势突破" = "adx_bbands",
                                             "SMA 交叉 (5/20)" = "sma_cross",
                                             "RSI 超买超卖" = "rsi_logic"),
                                  selected = "supertrend"),
                      layout_column_wrap(
                        width = 1/2,
                        numericInput("stop_loss", "止损位 (%) (0为不设)", value = 0, min = 0),
                        numericInput("take_profit", "止盈位 (%) (0为不设)", value = 0, min = 0)
                      )
                    ),
                    div(
                      textAreaInput("strategy_code", "策略信号逻辑 (R Code)", 
                                    value = "", 
                                    height = "180px",
                                    placeholder = "在这里编写您的交易逻辑..."),
                      tags$style("#strategy_code { font-family: 'Courier New', monospace; font-size: 12px; background: #fdf6e3; }")
                    )
                  )
                )
              ),
              hr(),
              actionButton("run_backtest", "启动历史回测", class = "btn-success w-100")
          )
        ),
        nav_panel(
          title = "2. 交易记录明细",
          div(class = "p-2",
              tableOutput("bt_trade_log")
          )
        ),
        nav_panel(
          title = "3. 绩效分析报告",
          div(class = "p-3",
              uiOutput("bt_performance_stats")
          )
        )
      )
    )
  )
)

# 定义 Server
server <- function(input, output, session) {
  
  options(HTTPUserAgent = "Mozilla/5.0")
  apiKey = Sys.getenv("GEMINI_API_KEY") # 从环境变量读取 API Key
  
  # 辅助函数：处理 NULL 值
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # 在侧边栏显示当前选中的模型标签
  output$selected_model_badge <- renderUI({
    model_name <- switch(input$ai_model,
                         "gemini-3-flash" = "3.0 Flash",
                         "gemini-2.5-flash" = "2.5 Flash",
                         "gemini-2.5-flash-lite" = "2.5 Flash-lite",
                         "gemini-2.0-flash-exp" = "2.0 Flash",
                         "gemini-1.5-pro" = "1.5 Pro",
                         "gemini-1.5-flash" = "1.5 Flash"
    )
    span(model_name, class = "badge bg-secondary", style = "font-size: 0.75rem;")
  })
  
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
      # 增加获取历史数据的范围以支持周、月、年线
      getSymbols(ticker, from = Sys.Date() - 3650, to = Sys.Date(), auto.assign = FALSE, src = "yahoo")
    }, error = function(e) return(NULL))
  })
  
  # 根据选择的周期处理数据
  processed_ticker_data <- reactive({
    data <- ticker_data()
    req(data)
    interval <- input$interval
    
    if (interval == "daily") {
      return(data)
    } else if (interval == "weekly") {
      return(to.weekly(data, indexAt = "endof", OHLC = TRUE))
    } else if (interval == "monthly") {
      return(to.monthly(data, indexAt = "endof", OHLC = TRUE))
    } else if (interval == "yearly") {
      return(to.yearly(data, indexAt = "endof", OHLC = TRUE))
    }
    data
  })
  
  # 将时间跨度文字转换为天数
  period_days <- reactive({
    period_map <- c(
      "20天" = 20, "1月" = 30, "3月" = 90, "6月" = 180,
      "1年" = 360, "2年" = 720, "5年" = 1825, "10年" = 3650
    )
    result <- as.numeric(period_map[input$period])
    # 如果找不到匹配，返回默认值360（1年）
    if (is.na(result)) result <- 360
    result
  })
  
  # 1. 关键统计指标
  output$vbox_market_stats <- renderUI({
    data <- ticker_data()
    if (is.null(data) || nrow(data) < 100) return("等待数据 (需至少100日数据以计算指标)...")
    
    rows <- tail(data, 2)
    latest_row <- rows[2]
    prev_row <- rows[1]
    
    cl_vec <- Cl(data)
    cl <- as.numeric(last(cl_vec))
    prev_cl <- as.numeric(prev_row[,4]) # Assuming 4th col is Close
    
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
    
    # 指标计算
    bb <- BBands(cl_vec, n = 20)
    latest_bb <- tail(bb, 1)
    
    sma5 <- as.numeric(last(SMA(cl_vec, n = 5)))
    sma20 <- as.numeric(last(SMA(cl_vec, n = 20)))
    sma50 <- as.numeric(last(SMA(cl_vec, n = 50)))
    sma100 <- as.numeric(last(SMA(cl_vec, n = 100)))
    
    # ADX 计算
    adx_data <- ADX(HLC(data), n = 14)
    latest_adx <- tail(adx_data, 1)
    val_adx <- as.numeric(latest_adx$ADX)
    val_dx <- as.numeric(latest_adx$DX)
    val_dip <- as.numeric(latest_adx$DIp)
    val_din <- as.numeric(latest_adx$DIn)
    
    # MACD 计算
    macd_data <- MACD(cl_vec, nFast = 12, nSlow = 26, nSig = 9)
    latest_macd <- tail(macd_data, 1)
    val_macd <- as.numeric(latest_macd$macd)
    val_msig <- as.numeric(latest_macd$signal)
    val_mhist <- val_macd - val_msig
    
    # RSI 计算
    val_rsi <- as.numeric(last(RSI(cl_vec, n = 14)))
    
    # TR 深度计算
    tr_obj <- TR(HLC(data))
    atr_obj <- ATR(HLC(data), n = 14)
    val_tr <- as.numeric(last(tr_obj$tr))
    val_th <- as.numeric(last(tr_obj$trueHigh))
    val_tl <- as.numeric(last(tr_obj$trueLow))
    val_atr <- as.numeric(last(atr_obj$atr))
    
    div(
      class = "w-100",
      style = "display: flex; align-items: start; justify-content: space-between; color: white;",
      # 1. 价格动态
      div(
        style = "flex: 0 0 130px; border-right: 1px solid rgba(255,255,255,0.2); padding-right: 8px; margin-right: 8px;",
        span(style="font-size: 0.7rem; opacity: 0.7; display: block;", "最新价"),
        span(style="font-size: 2rem; font-weight: 800; display: block; line-height: 1;", paste0("$", round(cl, 2))),
        span(style=paste0("font-size: 0.9rem; font-weight: 600; color: ", diff_color, ";"),
             sprintf("%+.2f (%+.2f%%)", diff, pct_diff))
      ),
      
      # 2. 均线与布林带
      div(
        style = "flex: 1; display: grid; grid-template-columns: 1fr; gap: 2px; font-size: 0.7rem; border-right: 1px solid rgba(255,255,255,0.2); padding-right: 8px; margin-right: 8px;",
        div(style="font-weight: bold; opacity: 0.5; text-transform: uppercase; font-size: 0.6rem;", "SMA & BB"),
        div(style="display: flex; justify-content: space-between;", span("SMA 5/20", style="opacity: 0.7;"), span(paste0(round(sma5, 1), "/", round(sma20, 1)))),
        div(style="display: flex; justify-content: space-between;", span("SMA 50/100", style="opacity: 0.7;"), span(paste0(round(sma50, 1), "/", round(sma100, 1)))),
        div(style="display: flex; justify-content: space-between;", span("BB Up", style="opacity: 0.7;"), span(round(as.numeric(latest_bb$up), 2))),
        div(style="display: flex; justify-content: space-between;", span("BB Low", style="opacity: 0.7;"), span(round(as.numeric(latest_bb$dn), 2)))
      ),
      
      # 3. MACD & RSI (强弱指标)
      div(
        style = "flex: 1; display: grid; grid-template-columns: 1fr; gap: 2px; font-size: 0.7rem; border-right: 1px solid rgba(255,255,255,0.2); padding-right: 8px; margin-right: 8px;",
        div(style="font-weight: bold; opacity: 0.5; text-transform: uppercase; font-size: 0.6rem;", "MACD & RSI"),
        div(style="display: flex; justify-content: space-between;", span("DIF", style="color: #60a5fa;"), span(round(val_macd, 2))),
        div(style="display: flex; justify-content: space-between;", span("DEA", style="color: #f87171;"), span(round(val_msig, 2))),
        div(style="display: flex; justify-content: space-between;", span("MACD", style="font-weight: bold;"), 
            span(round(val_mhist, 2), style=paste0("color: ", if(val_mhist >= 0) "#2ecc71" else "#e74c3c"))),
        div(style="display: flex; justify-content: space-between;", span("RSI(14)", style="color: #fbbf24; font-weight: bold;"), span(round(val_rsi, 1)))
      ),
      
      # 4. ADX (趋向指标)
      div(
        style = "flex: 1; display: grid; grid-template-columns: 1fr; gap: 2px; font-size: 0.7rem; border-right: 1px solid rgba(255,255,255,0.2); padding-right: 8px; margin-right: 8px;",
        div(style="font-weight: bold; opacity: 0.5; text-transform: uppercase; font-size: 0.6rem;", "ADX 趋向指标"),
        div(style="display: flex; justify-content: space-between;", span("ADX", style="font-weight: bold;"), span(round(val_adx, 1))),
        div(style="display: flex; justify-content: space-between;", span("DI+", style="color: #60a5fa;"), span(round(val_dip, 1))),
        div(style="display: flex; justify-content: space-between;", span("DI-", style="color: #f87171;"), span(round(val_din, 1))),
        div(style="display: flex; justify-content: space-between;", span("DX", style="opacity: 0.7;"), span(round(val_dx, 1)))
      ),
      
      # 5. TR & ATR (波动详情)
      div(
        style = "flex: 1.1; display: grid; grid-template-columns: 1fr; gap: 2px; font-size: 0.7rem;",
        div(style="font-weight: bold; opacity: 0.5; text-transform: uppercase; font-size: 0.6rem;", "TR/ATR 波动率"),
        div(style="display: flex; justify-content: space-between;", span("TR", style="font-weight: bold; color: #ff5e00;"), span(round(val_tr, 2))),
        div(style="display: flex; justify-content: space-between;", span("ATR (14)", style="font-weight: bold;"), span(round(val_atr, 2))),
        div(style="display: flex; justify-content: space-between;", span("T-High", style="opacity: 0.7;"), span(round(val_th, 2))),
        div(style="display: flex; justify-content: space-between;", span("T-Low", style="opacity: 0.7;"), span(round(val_tl, 2)))
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
    
    periods <- c(7, 30, 90, 180, 360, 720, 1825, 3650)
    labels <- c("7天", "1月", "3月", "6月", "1年", "2年", "5年", "10年")
    
    items <- lapply(seq_along(periods), function(i) {
      val <- calc_ret(data, periods[i])
      color <- if(is.na(val)) "text-muted" else if(val >= 0) "text-success" else "text-danger"
      div(style = "flex: 1; text-align: center; min-width: 50px;",
          div(style = "font-size: 0.7rem; color: #888; margin-bottom: 2px;", labels[i]),
          div(class = color, style = "font-weight: 700; font-size: 0.95rem;", if(is.na(val)) "--" else sprintf("%+.1f%%", val))
      )
    })
    
    div(class = "d-flex justify-content-between align-items-center w-100", 
        style = "gap: 8px; flex-wrap: wrap;",
        items)
  })
  
  # Gemini AI 联网预测逻辑
  ai_prediction <- reactiveVal(NULL)
  ai_grounding <- reactiveVal(NULL)
  ai_loading <- reactiveVal(FALSE)
  
  observeEvent(input$run_ai, {
    data <- ticker_data()
    if (is.null(data)) return()
    
    ai_loading(TRUE)
    ai_prediction(NULL)
    ai_grounding(NULL)
    
    recent_data <- tail(data, 120) 
    data_summary <- paste(capture.output(print(recent_data)), collapse = "\n")
    
    system_prompt <- "你是一位拥有20年经验的资深美股投资专家。
    任务：通过搜索网上的股票信息，包括财务信息、行业对比、估值评估，同时基于用户提供的历史交易数据，进行多维度的技术和量价分析。
    
    注意：请直接返回一个合法的 JSON 字符串，不要包含任何 Markdown 格式。JSON 必须包含以下字段：
    - news：股票相关核心新闻动态
    - financial: 财务信息、行业对比、估值评估总结
    - trend: 简短描述当前走势趋势
    - prediction_5d: 预测未来 5 个交易日的估计收盘价数组
    - reasoning: 详细的投资逻辑分析
    - support_level: 主要支撑位置价格
    - resistance_level: 主要阻力位置价格
    - trade_advice: { action: '买入价格/盈利价格/止顺价格', buy_price, take_profit, stop_loss }。"
    
    user_query <- paste0("股票代码: ", current_ticker(), "\n最近半年历史交易明细数据：\n", data_summary)
    
    tryCatch({
      # 动态获取所选模型
      model_id <- input$ai_model
      api_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_id, ":generateContent")
      
      resp <- request(api_url) %>%
        req_url_query(key = apiKey) %>% 
        req_method("POST") %>%
        req_body_json(list(
          contents = list(
            list(
              role = "user",
              parts = list(list(text = user_query))
            )
          ), 
          systemInstruction = list(parts = list(list(text = system_prompt))), 
          tools = list(
            list(
              google_search = setNames(list(), character(0))
            )
          ),
          generationConfig = list(
            temperature = 0.2
          )
        )) %>%
        req_retry(max_tries = 5, backoff = ~ 1 * 2^(.x - 1)) %>%
        req_perform()
      
      result <- resp_body_json(resp)
      raw_text <- result$candidates[[1]]$content$parts[[1]]$text
      
      # 处理 Grounding Metadata (来源参考)
      metadata <- result$candidates[[1]]$groundingMetadata
      if (!is.null(metadata) && !is.null(metadata$groundingAttributions)) {
        sources <- lapply(metadata$groundingAttributions, function(s) {
          list(
            title = s$web$title %||% "网页来源",
            uri = s$web$uri %||% "#"
          )
        })
        ai_grounding(sources)
      }
      
      # 鲁棒性处理：从回答中提取 JSON 部分
      json_start <- regexpr("\\{", raw_text)
      json_end <- regexpr("\\}[^\\}]*$", raw_text)
      if (json_start > 0 && json_end > 0) {
        clean_json <- substr(raw_text, json_start, json_end)
        parsed_res <- fromJSON(clean_json)
        ai_prediction(parsed_res)
      } else {
        stop("AI 返回的格式无法解析为 JSON")
      }
      
    }, error = function(e) {
      ai_prediction(list(error = paste("AI 联网分析失败:", e$message)))
    })
    ai_loading(FALSE)
  })
  
  output$ai_report_ui <- renderUI({
    if (ai_loading()) {
      return(div(class="d-flex justify-content-center p-5", div(class="spinner-border text-primary", role="status")))
    }
    
    res <- ai_prediction()
    sources <- ai_grounding()
    
    if (is.null(res)) {
      return(p("点击按钮启动 Gemini 联网投研深度分析", class="text-muted p-3 text-center"))
    }
    
    if (!is.null(res$error)) {
      return(div(class="alert alert-danger", as.character(res$error)))
    }
    
    tagList(
      div(class="mb-3 d-flex align-items-center justify-content-between",
          h5(paste0("综合研判：", res$trend), class="text-primary fw-bold mb-0"),
          span(class="badge bg-success", "Google Search 联网数据已接入")
      ),
      
      div(class="row g-3",
          div(class="col-md-6",
              div(class="card border-0 shadow-sm h-100", style="background: #f8fbff;",
                  div(class="card-body",
                      strong("📰 核心新闻动态"),
                      p(res$news, style="font-size: 0.88rem; margin-top: 10px; color: #333;")
                  )
              )
          ),
          div(class="col-md-6",
              div(class="card border-0 shadow-sm h-100", style="background: #f8fbff;",
                  div(class="card-body",
                      strong("💰 财务与估值评估"),
                      p(res$financial, style="font-size: 0.88rem; margin-top: 10px; color: #333;")
                  )
              )
          ),
          
          div(class="col-md-12",
              div(class="card border-0 shadow-sm", style="background: #ffffff; border-left: 5px solid #0d6efd !important;",
                  div(class="card-body",
                      strong("🧠 专家投资逻辑"),
                      p(res$reasoning, style="font-size: 0.92rem; margin-top: 10px;")
                  )
              )
          ),
          
          div(class="col-md-6",
              div(class="card border-0 shadow-sm h-100",
                  div(class="card-body text-center",
                      strong("未来 5 日价格走势预测"),
                      div(class="mt-3 fw-bold text-primary", style="letter-spacing: 1px;", 
                          paste(res$prediction_5d, collapse = " → "))
                  )
              )
          ),
          div(class="col-md-6",
              div(class="card border-0 shadow-sm h-100", style="background: #eef2f7;",
                  div(class="card-body py-2",
                      div(class="d-flex justify-content-between mb-1", 
                          span("建议操作", class="small"), span(res$trade_advice$action, class="fw-bold text-danger")),
                      div(class="d-flex justify-content-between mb-1", 
                          span("建议入场", class="small"), span(res$trade_advice$buy_price, class="fw-bold")),
                      div(class="d-flex justify-content-between", 
                          span("支撑/阻力", class="small text-muted"), 
                          span(paste0(res$support_level, " / ", res$resistance_level), class="small fw-bold"))
                  )
              )
          )
      ),
      
      # 渲染 Grounding 来源链接列表
      if (!is.null(sources)) {
        div(class="mt-4 p-3 bg-light rounded",
            strong("🔍 信息来源与参考："),
            tags$ul(class="list-unstyled mt-2",
                    lapply(sources, function(s) {
                      tags$li(class="mb-1", 
                              tags$a(href=s$uri, target="_blank", style="font-size: 0.82rem; text-decoration: none;",
                                     bsicons::bs_icon("link-45deg"), s$title))
                    })
            )
        )
      }
    )
  })

  # ---------------------------------------------------------
  # 交易策略回测引擎逻辑
  # ---------------------------------------------------------
  bt_results <- reactiveVal(NULL)
  
  # 策略模板库
  observeEvent(input$bt_strategy, {
    if (input$bt_strategy == "custom") return()
    
    file_path <- sprintf("trading_strategy/%s.R", input$bt_strategy)
    if (file.exists(file_path)) {
      code_lines <- readLines(file_path, warn = FALSE)
      updateTextAreaInput(session, "strategy_code", value = paste(code_lines, collapse = "\n"))
    }
  })
  
  observeEvent(input$run_backtest, {
    data <- processed_ticker_data()
    req(data)
    
    # 消息提示
    showNotification("正在运行自定义策略回测...", type = "message")
    
    # 运行回测引擎
    res <- tryCatch({
       # 鲁棒性处理：如果输入为空则默认为 0
       sl_val <- if(is.na(input$stop_loss)) 0 else input$stop_loss
       tp_val <- if(is.na(input$take_profit)) 0 else input$take_profit
       
       run_simple_backtest(
        data = data,
        strategy_code = input$strategy_code,
        init_capital = input$init_capital,
        trade_pct = input$trade_size,
        stop_loss_pct = sl_val,
        take_profit_pct = tp_val
      )
    }, error = function(e) {
      showNotification(paste("策略执行错误:", e$message), type = "error")
      return(NULL)
    })
    
    if(!is.null(res)) {
      bt_results(res)
      showNotification("回测完成！", type = "message")
    }
  })
  
  # 回测交易记录表格
  output$bt_trade_log <- renderTable({
    res <- bt_results()
    if (is.null(res) || nrow(res$log) == 0) return(data.frame(Message = "尚无交易记录，请配置策略后启动回测。"))
    
    df <- res$log
    # 颜色渲染函数
    color_pnl <- function(val, text) {
      if (is.na(val)) return("--")
      col <- if(val > 0) "#2ecc71" else if(val < 0) "#e74c3c" else "inherit"
      sprintf("<span style='color: %s; font-weight: bold;'>%s</span>", col, text)
    }
    
    # 格式化输出
    df_display <- data.frame(
      日期 = as.character(df$Date),
      动作 = df$Action,
      成交价 = round(df$Price, 2),
      数量 = round(df$Shares, 2),
      账户余额 = round(df$Total, 2),
      原因 = df$Reason,
      盈亏 = mapply(color_pnl, df$PnL, sprintf("%+.2f", df$PnL)),
      "盈亏%" = mapply(color_pnl, df$PnL_Pct, sprintf("%+.2f%%", df$PnL_Pct)),
      "最大浮盈" = ifelse(is.na(df$MaxProfit_Trade), "--", sprintf("%.2f%%", df$MaxProfit_Trade)),
      "最大回撤" = ifelse(is.na(df$MaxDD_Trade), "--", sprintf("%.2f%%", df$MaxDD_Trade)),
      持仓天数 = ifelse(is.na(df$HoldDays), "--", as.character(df$HoldDays))
    )
    df_display
  }, striped = TRUE, hover = TRUE, align = "c", sanitize.text.function = function(x) x)
  
  # 绩效分析面板
  output$bt_performance_stats <- renderUI({
    res <- bt_results()
    data <- processed_ticker_data()
    if (is.null(res) || is.null(data)) return(p("待回测完成后生成报告...", class="text-muted"))
    
    log <- res$log
    init <- input$init_capital
    final <- tail(res$equity, 1)
    profit <- final - init
    roi <- (profit / init) * 100
    
    # 时间范围计算
    start_date <- start(data)
    end_date <- end(data)
    total_days <- as.numeric(difftime(end_date, start_date, units = "days"))
    annualized_roi <- ((final / init)^(365 / total_days) - 1) * 100
    
    # 交易统计
    sells <- log[log$Action == "SELL", ]
    win_rate <- 0
    avg_pnl <- 0
    avg_hold <- 0
    max_win_amt <- 0; max_win_pct <- 0
    max_loss_amt <- 0; max_loss_pct <- 0
    
    if(nrow(sells) > 0) {
      win_rate <- (sum(sells$PnL > 0) / nrow(sells)) * 100
      avg_pnl <- mean(sells$PnL, na.rm = TRUE)
      avg_hold <- mean(sells$HoldDays, na.rm = TRUE)
      max_win_amt <- max(sells$PnL, na.rm = TRUE)
      max_win_pct <- max(sells$PnL_Pct, na.rm = TRUE)
      max_loss_amt <- min(sells$PnL, na.rm = TRUE)
      max_loss_pct <- min(sells$PnL_Pct, na.rm = TRUE)
    }
    
    tagList(
      div(class = "alert alert-primary mb-3 py-2",
          span(bsicons::bs_icon("info-circle"), " "),
          strong("回测基础信息："),
          sprintf("策略: %s | 股票: %s | 周期: %s 至 %s", 
                  input$bt_strategy, current_ticker(), start_date, end_date)
      ),
      layout_column_wrap(
        width = 1/4,
        value_box(title = "累计收益率", value = sprintf("%+.2f%%", roi), theme = if(roi>=0) "success" else "danger"),
        value_box(title = "年化收益率", value = sprintf("%+.2f%%", annualized_roi), theme = "light"),
        value_box(title = "回测胜率", value = sprintf("%.1f%%", win_rate), theme = "info"),
        value_box(title = "总交易次数", value = nrow(log), theme = "light")
      ),
      layout_column_wrap(
          style = "margin-top: 15px;",
          width = 1/3,
          div(class = "p-2 border rounded bg-white",
              div(class="small text-muted", "盈利能力"),
              div(strong("平均盈亏: "), sprintf("$%.2f", avg_pnl)),
              div(strong("最大单笔盈利: "), sprintf("$%.2f (%.2f%%)", max_win_amt, max_win_pct))
          ),
          div(class = "p-2 border rounded bg-white",
              div(class="small text-muted", "风险控制"),
              div(strong("持仓习惯: "), sprintf("平均 %.1f 天", avg_hold)),
              div(strong("最大单笔亏损: "), sprintf("$%.2f (%.2f%%)", max_loss_amt, max_loss_pct))
          ),
          div(class = "p-2 border rounded bg-white",
              div(class="small text-muted", "资金状态"),
              div(strong("初始资金: "), sprintf("$%s", init)),
              div(strong("最终权益: "), sprintf("$%.2f", final))
          )
      )
    )
  })

  # ---------------------------------------------------------
  # 简化后的绘图逻辑：确保 subset 与 visible_data 严格一致
  # ---------------------------------------------------------
  output$plot <- renderPlot({
    data <- processed_ticker_data()
    if (is.null(data) || nrow(data) < 2) return(NULL)
    
    # 根据选择的按钮计算回溯天数 (对周/月/年线可见点数会自动调整)
    days_to_show <- period_days()
    visible_data <- tail(data, days_to_show)
    
    # 绘图：直接传入 processed 数据
    subset_range <- paste0(start(visible_data), "::")
    
    cs <- chart_Series(data, 
                       name = current_ticker(), 
                       subset = subset_range,
                       type = input$plot_type, 
                       theme = chart_theme())
    
    # 叠加指标 - 始终显示成交量
    cs <- add_Vo()
    
    # 根据用户选择添加技术指标
    n_points <- nrow(visible_data)
    selected_indicators <- input$indicators
    
    # SMA - 简单移动平均
    if ("SMA" %in% selected_indicators) {
      if (n_points >= 5) cs <- add_SMA(n = 5, col = "blue")
      if (n_points >= 20) cs <- add_SMA(n = 20, col = "red")
      if (n_points >= 50) cs <- add_SMA(n = 50, col = "orange")
      if (n_points >= 100) cs <- add_SMA(n = 100, col = "purple")
    }
    
    # BBands - 布林带
    if ("BBands" %in% selected_indicators && n_points >= 20) {
      cs <- add_BBands(n = 20)
    }
    
    # MACD
    if ("MACD" %in% selected_indicators && n_points >= 26) {
      cs <- add_MACD()
    }
    
    # RSI - 相对强弱指标
    if ("RSI" %in% selected_indicators && n_points >= 14) {
      cs <- add_RSI(n = 14)
    }
    
    # ADX - 平均趋向指标
    if ("ADX" %in% selected_indicators && n_points >= 14) {
      cs <- add_ADX(n = 14)
    }
    
    # SAR - 抛物线转向 (使用 add_TA)
    if ("SAR" %in% selected_indicators && n_points >= 5) {
      cs <- add_TA(SAR(HLC(data)), on = 1, col = "blue")
    }
    
    # OBV - 能量潮 (使用 add_TA)
    if ("OBV" %in% selected_indicators) {
      cs <- add_TA(OBV(Cl(data), Vo(data)))

    }
    
    # MFI - 资金流量指标 (使用 add_TA)
    if ("MFI" %in% selected_indicators && n_points >= 14) {
      cs <- add_TA(MFI(HLC(data), Vo(data)))
    }
    
    # CLV - 收盘位置值 (使用 add_TA)
    if ("CLV" %in% selected_indicators) {
      cs <- add_TA(CLV(HLC(data)))
    }
    
    # TR - 真实波幅 (使用 add_TA)
    if ("TR" %in% selected_indicators) {
      cs <- add_TA(TR(HLC(data))$tr, col = "darkred")
    }
    
    # ATR - 平均真实波幅 (使用 add_TA)
    if ("ATR" %in% selected_indicators) {
      cs <- add_TA(ATR(HLC(data), n = 14)$atr, col = "darkblue")
    }
    
    # SuperTrend - 超级趋势线 (Overlay on main chart)
    if ("SuperTrend" %in% selected_indicators) {
      st_res <- SuperTrend(HLC(data), n = 10, factor = 3)
      st_line <- st_res$supertrend
      st_dir <- st_res$direction
      
      # 分离上涨和下跌线条进行着色
      st_up <- st_line; st_up[st_dir == -1] <- NA
      st_down <- st_line; st_down[st_dir == 1] <- NA
      
      cs <- add_TA(st_up, on = 1, col = "green", lwd = 2)
      cs <- add_TA(st_down, on = 1, col = "red", lwd = 2)
    }
    
    # --- 回测交易数据准备 ---
    res <- bt_results()
    
    # 计算极值 (直接在 visible_data 上操作)
    hi_v <- Hi(visible_data); lo_v <- Lo(visible_data)
    max_idx <- which.max(hi_v); max_val <- as.numeric(hi_v[max_idx])
    min_idx <- which.min(lo_v); min_val <- as.numeric(lo_v[min_idx])
    
    # --- 绘图后叠加：文字与图标 ---
    # 基础图表绘制
    print(cs)
    
    # 在 Plot 结束后，使用标准 R 绘图函数叠加 (叠加在当前活动设备上)
    if (!is.null(res) && nrow(res$log) > 0) {
      log <- res$log
      # 仅处理可见范围内的记录
      visible_log <- log[log$Date >= start(visible_data), ]
      
      if(nrow(visible_log) > 0) {
        # 允许在绘图区外书写
        par(xpd = TRUE)
        
        for(k in 1:nrow(visible_log)) {
          # 找到当前日期在可见数据中的索引位置（即 X 坐标）
          match_idx <- which(index(visible_data) == as.Date(visible_log$Date[k]))
          
          if(length(match_idx) > 0) {
            target_date <- as.Date(visible_log$Date[k])
            # 获取该日的最高/最低价作为基准
            day_hi <- as.numeric(Hi(visible_data[target_date]))
            day_lo <- as.numeric(Lo(visible_data[target_date]))
            
            if(visible_log$Action[k] == "BUY") {
              # 买入：在当日最低价下方
              y_icon <- day_lo * 0.985
              points(x = match_idx, y = y_icon, pch = 24, col = "darkgreen", bg = "#2ecc71", cex = 1.5)
              text(x = match_idx, y = y_icon, labels = paste(visible_log$Action[k], visible_log$Reason[k]), 
                   pos = 1, col = "#27ae60", cex = 0.8, font = 2)
            } else {
              # 卖出：在当日最高价上方
              y_icon <- day_hi * 1.015
              points(x = match_idx, y = y_icon, pch = 25, col = "darkred", bg = "#e74c3c", cex = 1.5)
              text(x = match_idx, y = y_icon, labels = paste(visible_log$Action[k], visible_log$Reason[k]), 
                   pos = 3, col = "#c0392b", cex = 0.8, font = 2)
            }
          }
        }
      }
    }

    # 叠加极值文字
    if (input$show_points && length(max_idx) > 0) {
      try({
        text(x = max_idx, y = max_val, labels = paste0("高: ", round(max_val, 2)), pos=3, col="red", font=2)
        text(x = min_idx, y = min_val, labels = paste0("低: ", round(min_val, 2)), pos=1, col="darkgreen", font=2)
      }, silent = TRUE)
    }
  })

  
  output$data <- renderTable({
    data <- processed_ticker_data(); if (is.null(data)) return(NULL)
    # 转换列名以便清晰展示 (例如 Open, High, Low, Close, Volume)
    df <- data.frame(Date = as.character(index(tail(data, 10))), coredata(tail(data, 10)))
    names(df) <- gsub(".*\\.", "", names(df))
    df
  }, striped = TRUE, hover = TRUE)
}

# Run app
shinyApp(ui = ui, server = server)