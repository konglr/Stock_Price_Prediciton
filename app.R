# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinyWidgets)
library(tidyquant)
library(quantmod)
library(TTR)
library(htmltools)
library(bslib)
library(httr2)
library(jsonlite)
library(markdown)
library(shinychat)


# 加载 .Renviron 文件中的环境变量
readRenviron(".Renviron")

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
  title = "StockAI - AI股票智能预测",
  
  sidebar = sidebar(
    width = 300,
    pickerInput(
      inputId = "ticker_preset",
      label = "常用股票选择",
      choices = c("AAPL", "AMZN", "GOOGL", "MSFT", "TSLA", "NVDA", "ORCL", "SQQQ", "^IXIC", "000001.SZ"),
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
      choiceNames = list(
        "SMA (简单移动平均)",
        "BBands (布林带)",
        "MACD",
        "RSI (相对强弱指标)",
        "ADX (平均趋向指标)",
        "SAR (抛物线转向)",
        "OBV (能量潮)",
        "MFI (资金流量指标)",
        "CLV (收盘位置值)",
        tags$span(
          "TR (真实波幅)", 
          title = "TR（真实波幅）的计算。TR 代表一个特定时段内价格运动的真实范围。为了确保将价格跳空考虑在内，TR 取以下三个值中的最大值：1. 当前时段最高价减去当前时段最低价 (High-Low)。2. 当前时段最高价与前一收盘价之差的绝对值。3. 当前时段最低价与前一收盘价之差的绝对值。计算公式：TR = max[(High-Low), abs(High-PrevClose), abs(Low-PrevClose)]"
        ),
        tags$span(
          "ATR (平均真实波幅)", 
          title = "ATR（平均真实波幅）的计算。ATR 是对上述 TR 值进行的平滑平均处理，用以反映一段时间内的平均波动水平。1. 周期设置：标准默认设置通常为 14 个周期，但在 Supertrend 等指标中也常使用 10 个周期。2. 计算方法：递归平滑法（Wilder 原版）：这是最常用的方法，公式为 ATR=(前一日ATR*(n-1)+当日TR)/n。简单移动平均法 (SMA)：直接计算指定周期内 TR 的算术平均值。指数移动平均法 (EMA)：部分交易系统为了提高对近期波动的敏感度，会使用 EMA 对 TR 进行平滑。"
        ),
        "SuperTrend"
      ),
      choiceValues = list(
        "SMA", "BBands", "MACD", "RSI", "ADX", "SAR", "OBV", "MFI", "CLV", "TR", "ATR", "SuperTrend"
      ),
      selected = c("SMA")
    ),
    
    hr(),
    h5("AI 预测模型"),
    
    # AI 提供商选择
    selectInput(
      inputId = "ai_provider",
      label = "选择 AI 提供商",
      choices = c(
        "Google Gemini" = "gemini",
        "MiniMax" = "minimax",
        "阿里云百炼" = "aliyun"
      ),
      selected = "minimax"
    ),
    
    # 动态渲染模型选择
    uiOutput("ai_model_ui"),
    
    # 显示当前模型状态
    div(
      class = "mb-2",
      span("当前使用: ", style = "font-size: 0.8rem; color: #666;"),
      uiOutput("selected_model_badge", inline = TRUE)
    ),
    
    # AI 参数控制
    hr(),
    h6("AI 参数设置", class = "mb-2"),
    sliderInput(
      inputId = "ai_temperature",
      label = "Temperature (创造性)",
      min = 0, max = 1, value = 0.7, step = 0.1
    ),
    sliderInput(
      inputId = "ai_max_tokens",
      label = "Max Tokens (输出长度)",
      min = 256, max = 4096, value = 1024, step = 256
    ),
    checkboxInput("ai_enable_search", "启用联网搜索 (Gemini / MiniMax)", value = FALSE),
    
    actionButton("run_ai", "运行 AI 联网预测", class = "btn-primary w-100"),

    
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
      card_header("AI 深度研报 (含实时新闻与财务评估)"),
      card_body(uiOutput("ai_report_ui"))
    ),
    
    card(
      style = "margin-top: 10px;",
      card_header("交易策略回测系统 (Backtesting Suite)"),
      navset_tab(
        nav_panel(
          title = "1. 策略与资金配置",
          div(class = "p-3",
              layout_column_wrap(width = 1/4,
                div(class = "bt-col-sep",
                    span(class="bt-section-title", "① 资金管理"),
                    numericInput("init_capital", "初始本金 (USD)", value = 10000, min = 100),
                    helpText("账户起始本金额度")
                ),
                # 2. 资金策略
                div(class = "bt-col-sep",
                    span(class="bt-section-title", "② 资金策略"),
                    selectInput("entry_rule", "建仓规则", choices = c("固定百分比" = "fixed_pct", "固定股数" = "fixed_qty"), selected = "fixed_pct"),
                    numericInput("trade_size", "建仓比例 (%)", value = 20, min = 1, max = 100),
                    hr(),
                    selectInput("pyramid_rule", "加仓规则", choices = c("不启用" = "none", "正金字塔" = "up", "等额加仓" = "equal"), selected = "none"),
                    numericInput("pyramid_size", "加仓比例 (%)", value = 10, min = 0, max = 100)
                ),
                # 3. 风险管理
                div(class = "bt-col-sep",
                    span(class="bt-section-title", "③ 风险管理"),
                    numericInput("stop_loss", "止损规则 (%)", value = 0, min = 0),
                    numericInput("take_profit", "止盈规则 (%)", value = 0, min = 0),
                    helpText("设置为 0 禁用规则")
                ),
                # 4. 交易策略
                div(
                    span(class="bt-section-title", "④ 交易策略"),
                    selectInput("bt_strategy", "代码模板", 
                                choices = c("自定义代码" = "custom",
                                           "SuperTrend 趋势追踪" = "supertrend", 
                                           "ADX + BBands 趋势突破" = "adx_bbands",
                                           "SMA 交叉 (5/20)" = "sma_cross",
                                           "RSI 超买超卖" = "rsi_logic"),
                                selected = "supertrend"),
                    textAreaInput("strategy_code", "信号逻辑代码", value = "", height = "165px", placeholder = "代码...")
                )
              ),
              tags$style("
                .bt-col-sep { border-right: 1px solid #dee2e6; height: 100%; padding-right: 15px; }
                .bt-section-title { font-weight: bold; margin-bottom: 15px; color: #2c3e50; border-bottom: 2px solid #3498db; display: inline-block; }
                #strategy_code { font-family: 'Courier New', monospace !important; font-size: 12px !important; background: #fdf6e3 !important; }
              "),
              hr(),
              actionButton("run_backtest", "启动历史回测引擎", class = "btn-success w-100", icon = icon("rocket"))
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
          div(class="p-3",
            uiOutput("bt_performance_stats"),
            hr(),
            div(style="background: #fcfcfc; border-radius: 8px; padding: 15px;",
              h6(bsicons::bs_icon("graph-up"), " 净值走势与回撤分析 (模拟 TradingView 样式)"),
              plotOutput("bt_equity_plot", height = "450px")
            )
          )
        ),
        nav_panel(
          title = "AI金融助手",
          div(class = "p-3",
            # 使用 shinychat 的聊天组件
            div(class = "row g-0",
                # 左侧：聊天消息区域
                div(class = "col-md-10",
                    # shinychat 聊天 UI
                    chat_ui("chat", 
                            placeholder = "输入您的问题，例如：帮我分析一下苹果公司的股票...",
                            height = "500px"),
                    # 聊天字体样式 - 强制统一
                    tags$style("
                      /* 强制将聊天窗口内所有元素的字体大小统一为 0.8rem */
                      .chat-message * {
                        font-size: 0.8rem !important;
                        line-height: 1.4 !important;
                      }
                    "),
                    # 操作按钮
                    div(class = "mt-2 d-flex justify-content-between align-items-center",
                        actionButton("clear_chat", "清空对话", 
                                    class = "btn-outline-secondary btn-sm",
                                    icon = icon("trash")),
                        uiOutput("chat_status")
                    )
                ),
                # 右侧：快捷操作
                div(class = "col-md-2 ps-3",
                    div(class = "card border-0 shadow-sm h-100",
                        div(class = "card-header bg-white border-0",
                            h6(class = "mb-0", bsicons::bs_icon("lightbulb"), " 快捷问题")
                        ),
                        div(class = "card-body p-2",
                            div(class = "d-grid gap-2",
                                actionButton("quick_q1", "📊 帮我分析当前股票的技术面", 
                                           class = "btn-outline-primary btn-sm text-start"),
                                actionButton("quick_q2", "💰 解读最新的财务指标", 
                                           class = "btn-outline-primary btn-sm text-start"),
                                actionButton("quick_q3", "📈 预测未来走势", 
                                           class = "btn-outline-primary btn-sm text-start"),
                                actionButton("quick_q4", "🎯 给出买卖建议", 
                                           class = "btn-outline-primary btn-sm text-start"),
                                actionButton("quick_q5", "📰 最近的重大新闻", 
                                           class = "btn-outline-primary btn-sm text-start"),
                                actionButton("quick_q6", "❓ 解释什么是RSI指标", 
                                           class = "btn-outline-primary btn-sm text-start")
                            )
                        )
                    )
                )
            )
          )
        )
      )
    )
  )
)

# 定义 Server
server <- function(input, output, session) {
  
  options(HTTPUserAgent = "Mozilla/5.0")
  
  # API Keys 从环境变量读取
  gemini_apiKey <- Sys.getenv("GEMINI_API_KEY")
  minimax_apiKey <- Sys.getenv("MINIMAX_API_KEY")
  aliyun_apiKey <- Sys.getenv("ALIYUNCS_API_KEY")
  
  # 辅助函数：处理 NULL 值
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  # =========================================================
  # 后台询问 AI 判断问题是否涉及股票（不显示给用户）
  # =========================================================
  ask_ai_question_info <- function(user_question, current_ticker = NULL) {
    provider <- input$ai_provider
    model_id <- input$ai_model %||% "gemini-2.5-flash"
    
    temperature <- input$ai_temperature %||% 0.3
    max_tokens <- 500
    
    current_ticker_info <- if (!is.null(current_ticker) && current_ticker != "") {
      paste0("\n\n当前用户正在查看的股票是：", current_ticker)
    } else {
      ""
    }
    
    query <- paste0("你是一个股票问题分析助手，需要返回 JSON 格式的分析结果。",
                    "\n\n用户问题是：\"", user_question, "\"",
                    current_ticker_info,
                    "\n\n请分析用户问题，返回以下信息（JSON 格式）：",
                    "\n1. ticker: 用户提到的股票代码（Yahoo Finance 格式），如果没有提到则返回当前股票或 \"GENERAL\"",
                    "\n2. need_data: 用户是否需要股票数据来回答问题（true/false）",
                    "\n3. switch_stock: 是否需要切换到新股票（true/false，仅当用户明确提到新股票时为 true）",
                    "\n\nYahoo Finance 股票代码格式：",
                    "\n- 美股：GOOGL、AAPL、TSLA、NVDA 等",
                    "\n- A 股上海：601988.SS、600519.SS 等",
                    "\n- A 股深圳：000001.SZ、002594.SZ 等",
                    "\n- 港股：0700.HK、9988.HK 等",
                    "\n\n判断 need_data 的规则：",
                    "\n- 用户询问股票分析、技术面、走势、买卖建议等 → need_data = true",
                    "\n- 用户询问股票代码对应的 公司名称 → need_data = false",
                    "\n- 用户问\"这个股票怎么样\"等指代当前股票的问题 → need_data = true",
                    "\n- 用户闲聊、问概念性问题（如\"什么是 RSI\"）→ need_data = false",
                    "\n\n判断 switch_stock 的规则：",
                    "\n- 用户明确提到新的股票名称或代码 → switch_stock = true",
                    "\n- 用户说\"这个股票\"、\"当前股票\"等指代 → switch_stock = false",
                    "\n- 用户没有提到具体股票 → switch_stock = false",
                    "\n\n只返回 JSON，格式如：{\"ticker\": \"AAPL\", \"need_data\": true, \"switch_stock\": false}")
    
    tryCatch({
      if (provider == "gemini") {
        api_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_id, ":generateContent")
        req_body <- list(contents = list(list(role = "user", parts = list(list(text = query)))), generationConfig = list(temperature = temperature, maxOutputTokens = max_tokens))
        resp <- request(api_url) %>% req_url_query(key = gemini_apiKey) %>% req_method("POST") %>% req_body_json(req_body) %>% req_timeout(15) %>% req_perform()
        result <- resp_body_json(resp)
        ai_response <- result$candidates[[1]]$content$parts[[1]]$text
      } else if (provider == "minimax") {
        api_url <- "https://api.minimax.chat/v1/text/chatcompletion_v2"
        resp <- request(api_url) %>% req_method("POST") %>% req_headers(Authorization = paste0("Bearer ", minimax_apiKey), "Content-Type" = "application/json") %>% req_body_json(list(model = model_id, messages = list(list(role = "user", content = query)), temperature = temperature, max_tokens = max_tokens)) %>% req_timeout(60) %>% req_perform()
        result <- resp_body_json(resp)
        ai_response <- result$choices[[1]]$message$content
      } else if (provider == "aliyun") {
        api_url <- "https://coding.dashscope.aliyuncs.com/v1/chat/completions"
        resp <- request(api_url) %>% req_method("POST") %>% req_headers(Authorization = paste0("Bearer ", aliyun_apiKey), "Content-Type" = "application/json") %>% req_body_json(list(model = model_id, messages = list(list(role = "user", content = query)), temperature = temperature, max_tokens = max_tokens)) %>% req_timeout(15) %>% req_perform()
        result <- resp_body_json(resp)
        ai_response <- result$choices[[1]]$message$content
      }
      
      result_text <- trimws(ai_response)
      json_match <- regmatches(result_text, regexpr("\\{[^{}]*\\}", result_text))
      if (length(json_match) > 0) {
        return(fromJSON(json_match[1]))
      }
      # Fallback for non-json response
      return(list(ticker = "GENERAL", need_data = FALSE, switch_stock = FALSE))
      
    }, error = function(e) {
      message(paste("后台询问 AI 判断问题类型失败:", e$message))
      return(list(ticker = "GENERAL", need_data = FALSE, switch_stock = FALSE))
    })
  }
  
  # 切换股票的辅助函数
  switch_ticker <- function(ticker) {
    if (is.null(ticker) || ticker == "") return()
    updateTextInput(session, "ticker_custom", value = ticker)
    if (!is.null(input$ticker_preset)) {
      preset_choices <- c("AAPL", "AMZN", "GOOGL", "MSFT", "TSLA", "NVDA", "ORCL", "SQQQ", "^IXIC", "000001.SZ")
      if (ticker %in% preset_choices) {
        updatePickerInput(session, "ticker_preset", selected = ticker)
      }
    }
    showNotification(paste("已切换到股票:", ticker), type = "message")
  }
  
  # 格式化K线数据
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
    return(paste(lines, collapse = "\n"))
  }

  output$ai_model_ui <- renderUI({
    provider <- input$ai_provider
    choices <- if (provider == "gemini") {
      c("Gemini 1.5 Pro" = "gemini-1.5-pro", "Gemini 1.5 Flash" = "gemini-1.5-flash")
    } else if (provider == "minimax") {
      c("MiniMax-M2.5 (推荐)" = "MiniMax-M2.5")
    } else if (provider == "aliyun") {
      c("通义千问 3.5 Plus" = "qwen3.5-plus")
    }
    selectInput("ai_model", "选择模型", choices = choices)
  })

  output$selected_model_badge <- renderUI({
    model <- input$ai_model %||% "gemini-2.5-flash"
    friendly_name <- switch(model,
                           "gemini-1.5-pro" = "Gemini 1.5 Pro",
                           "gemini-1.5-flash" = "Gemini 1.5 Flash",
                           "MiniMax-M2.5" = "MiniMax-M2.5",
                           "qwen3.5-plus" = "Qwen 3.5 Plus",
                           model)
    span(class = "badge bg-info", friendly_name)
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
      getSymbols(ticker, from = Sys.Date() - 3650, to = Sys.Date(), auto.assign = FALSE, src = "yahoo")
    }, error = function(e) return(NULL))
  })
  
  processed_ticker_data <- reactive({
    data <- ticker_data()
    if (is.null(data)) return(NULL)
    
    # 转换周期
    data <- switch(input$interval,
                   "daily" = data,
                   "weekly" = to.weekly(data, indexAt = "lastof", OHLC = TRUE),
                   "monthly" = to.monthly(data, indexAt = "lastof", OHLC = TRUE),
                   "yearly" = to.yearly(data, indexAt = "lastof", OHLC = TRUE))
    
    # 确保列名格式统一 (Yahoo Finance 格式)
    colnames(data) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
    data
  })

  period_days <- reactive({
    switch(input$period,
           "20天" = 20, "1月" = 30, "3月" = 90, "6月" = 180,
           "1年" = 365, "2年" = 730, "5年" = 1825, "10年" = 3650, 365)
  })

  output$vbox_market_stats <- renderUI({
    data <- processed_ticker_data()
    if (is.null(data) || nrow(data) < 2) return("等待数据加载...")
    
    latest <- tail(data, 1)
    prev <- tail(data, 2)[1]
    
    price <- as.numeric(Cl(latest))
    change <- price - as.numeric(Cl(prev))
    pct_change <- (change / as.numeric(Cl(prev))) * 100
    
    # 简单的技术指标计算
    sma5 <- mean(tail(Cl(data), 5), na.rm = TRUE)
    sma20 <- mean(tail(Cl(data), 20), na.rm = TRUE)
    rsi <- tail(RSI(Cl(data), n = 14), 1)
    
    div(
      div(class = "d-flex justify-content-between",
          span(sprintf("$%.2f", price), style = "font-size: 1.5rem; font-weight: bold;"),
          span(sprintf("%+.2f (%+.2f%%)", change, pct_change), 
               style = paste0("color: ", if(change >= 0) "#2ecc71" else "#e74c3c", "; font-weight: bold;"))),
      div(class = "mt-2", style = "font-size: 0.8rem; opacity: 0.8;",
          span(paste0("SMA(5): ", round(sma5, 2))),
          span(" | ", style = "margin: 0 5px;"),
          span(paste0("SMA(20): ", round(sma20, 2))),
          span(" | ", style = "margin: 0 5px;"),
          span(paste0("RSI(14): ", if(!is.na(rsi)) round(rsi, 1) else "--")))
    )
  })

  output$vbox_performance <- renderUI({
    data <- ticker_data() # 使用原始日线数据计算收益率更准确
    if (is.null(data) || nrow(data) < 252) return("数据不足 (需1年以上历史)")
    
    current_price <- as.numeric(Cl(tail(data, 1)))
    
    calc_ret <- function(days) {
      if (nrow(data) < days) return(NA)
      old_price <- as.numeric(Cl(tail(data, days)[1]))
      (current_price - old_price) / old_price * 100
    }
    
    ret_1m <- calc_ret(21)
    ret_3m <- calc_ret(63)
    ret_6m <- calc_ret(126)
    ret_1y <- calc_ret(252)
    
    fmt_ret <- function(val) {
      if (is.na(val)) return(span("--", class = "text-muted"))
      color <- if(val >= 0) "#2ecc71" else "#e74c3c"
      span(sprintf("%+.1f%%", val), style = paste0("color: ", color, "; font-weight: bold;"))
    }
    
    div(
      layout_column_wrap(
        width = 1/2,
        div(div("近1月", style = "font-size: 0.7rem;"), fmt_ret(ret_1m)),
        div(div("近3月", style = "font-size: 0.7rem;"), fmt_ret(ret_3m)),
        div(div("近6月", style = "font-size: 0.7rem;"), fmt_ret(ret_6m)),
        div(div("近1年", style = "font-size: 0.7rem;"), fmt_ret(ret_1y))
      )
    )
  })

  ai_prediction <- reactiveVal(NULL)
  ai_grounding <- reactiveVal(NULL)
  ai_loading <- reactiveVal(FALSE)

  observeEvent(input$run_ai, {
    req(processed_ticker_data())
    ai_loading(TRUE)
    ai_prediction(NULL)
    ai_grounding(NULL)
    
    data <- processed_ticker_data()
    ticker <- current_ticker()
    
    # 联网分析提示词
    system_prompt <- paste0(
      "你是一位顶尖的华尔街量化投资专家。请针对股票 ", ticker, " 进行深度联网投研分析。",
      "\n请根据当前市场环境、近期新闻、财务状况和技术面，提供以下 JSON 格式的回复：",
      "\n{",
      "\n  \"trend\": \"看多/看空/中性\",",
      "\n  \"news\": \"最近 3 条核心新闻摘要及影响\",",
      "\n  \"financial\": \"关键财务指标评估（营收、PE、PB 等）\",",
      "\n  \"reasoning\": \"投资逻辑深度推导（200字以内）\",",
      "\n  \"prediction_5d\": [\"第一天价格\", \"第二天价格\", ...],",
      "\n  \"support_level\": \"支撑位\",",
      "\n  \"resistance_level\": \"压力位\",",
      "\n  \"trade_advice\": { \"action\": \"买入/持有/卖出\", \"buy_price\": \"建议入场价\" }",
      "\n}",
      "\n\n只返回 JSON 格式，不要包含任何 Markdown 代码块包裹（不要有 ```json 标签）。"
    )
    
    provider <- input$ai_provider
    model_id <- input$ai_model %||% "gemini-2.5-flash"
    temperature <- input$ai_temperature %||% 0.7
    max_tokens <- input$ai_max_tokens %||% 1024
    
    tryCatch({
      raw_text <- ""
      if (provider == "gemini") {
        # Gemini API 调用 logic
        api_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_id, ":generateContent")
        req_body <- list(
          contents = list(list(parts = list(list(text = system_prompt)))),
          generationConfig = list(temperature = temperature, maxOutputTokens = max_tokens)
        )
        if (input$ai_enable_search) {
          req_body$tools <- list(list(google_search = setNames(list(), character(0))))
        }
        
        resp <- request(api_url) %>% req_url_query(key = gemini_apiKey) %>% req_method("POST") %>% req_body_json(req_body) %>% req_retry(max_tries = 3) %>% req_perform()
        result <- resp_body_json(resp)
        raw_text <- result$candidates[[1]]$content$parts[[1]]$text
        # 处理 Grounding 来源
        if (!is.null(result$candidates[[1]]$groundingMetadata$searchEntryPoint$renderedContent)) {
          ai_grounding(result$candidates[[1]]$groundingMetadata$groundingChunks)
        }
      } else if (provider == "minimax") {
        api_url <- "https://api.minimax.chat/v1/text/chatcompletion_v2"
        minimax_req <- list(
          model = model_id,
          messages = list(list(role = "system", content = system_prompt)),
          temperature = temperature, max_tokens = max_tokens
        )
        resp <- request(api_url) %>% req_method("POST") %>% req_headers(Authorization = paste0("Bearer ", minimax_apiKey), "Content-Type" = "application/json") %>% req_body_json(minimax_req) %>% req_retry(max_tries = 3) %>% req_perform()
        result <- resp_body_json(resp)
        raw_text <- result$choices[[1]]$message$content
      } else if (provider == "aliyun") {
        api_url <- "https://coding.dashscope.aliyuncs.com/v1/chat/completions"
        aliyun_req <- list(
          model = model_id,
          messages = list(list(role = "system", content = system_prompt)),
          temperature = temperature, max_tokens = max_tokens
        )
        resp <- request(api_url) %>% req_method("POST") %>% req_headers(Authorization = paste0("Bearer ", aliyun_apiKey), "Content-Type" = "application/json") %>% req_body_json(aliyun_req) %>% req_retry(max_tries = 3) %>% req_perform()
        result <- resp_body_json(resp)
        raw_text <- result$choices[[1]]$message$content
      }
      
      # 解析 JSON
      json_match <- regmatches(raw_text, regexpr("\\{[^{}]*\\}", raw_text))
      if (length(json_match) > 0) {
        ai_prediction(fromJSON(json_match[1]))
      } else {
        # 如果不是严格 JSON，尝试直接解析
        ai_prediction(fromJSON(raw_text))
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
    if (is.null(res)) return(p("点击按钮启动 AI 联网投研深度分析", class="text-muted p-3 text-center"))
    
    if (!is.null(res$error)) return(div(class="alert alert-danger", res$error))
    
    tagList(
      div(class="mb-3 d-flex align-items-center justify-content-between",
          h5(paste0("综合研判：", res$trend), class="text-primary fw-bold mb-0"),
          span(class="badge bg-success", "联网数据已接入")
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
                      div(class="d-flex justify-content-between", 
                          span("支撑/阻力", class="small text-muted"), 
                          span(paste0(res$support_level, " / ", res$resistance_level), class="small fw-bold"))
                  )
              )
          )
      )
    )
  })

  bt_results <- reactiveVal(NULL)

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
    showNotification("正在运行自定义策略回测...", type = "message")
    res <- tryCatch({
       sl_val <- if(is.na(input$stop_loss)) 0 else input$stop_loss
       tp_val <- if(is.na(input$take_profit)) 0 else input$take_profit
       run_simple_backtest(
        data = data, strategy_code = input$strategy_code, init_capital = input$init_capital,
        trade_pct = input$trade_size, stop_loss_pct = sl_val, take_profit_pct = tp_val
      )
    }, error = function(e) {
      showNotification(paste("策略执行错误:", e$message), type = "error")
      return(NULL)
    })
    if(!is.null(res)) bt_results(res)
  })

  output$bt_trade_log <- renderTable({
    res <- bt_results()
    if (is.null(res) || nrow(res$log) == 0) return(data.frame(Message = "尚无交易记录。"))
    df <- res$log
    data.frame(
      日期 = as.character(df$Date), 动作 = df$Action,
      成交价 = round(df$Price, 2), 数量 = round(df$Shares, 2),
      账户余额 = round(df$Total, 2), 原因 = df$Reason,
      盈亏 = sprintf("%+.2f", df$PnL), "盈亏%" = sprintf("%+.2f%%", df$PnL_Pct)
    )
  }, striped = TRUE, hover = TRUE, align = "c", sanitize.text.function = function(x) x)

  output$bt_performance_stats <- renderUI({
    res <- bt_results()
    if (is.null(res)) return(p("待回测完成后生成报告...", class="text-muted"))
    fmt_c <- function(x) format(round(as.numeric(x), 2), big.mark=",")
    final_equity <- tail(res$equity, 1)
    init_equity <- res$equity[1]
    profit <- final_equity - init_equity
    roi <- (profit / init_equity) * 100
    layout_column_wrap(
      width = 1/4,
      value_box(title = "初始本金", value = sprintf("$%s", fmt_c(init_equity)), theme = "secondary"),
      value_box(title = "累计盈亏", value = sprintf("$%s (%+.2f%%)", fmt_c(profit), roi), theme = if(profit>=0) "success" else "danger"),
      value_box(title = "最终净值", value = sprintf("$%s", fmt_c(final_equity)), theme = "primary"),
      value_box(title = "交易次数", value = nrow(res$log[res$log$Action == "SELL", ]), theme = "info")
    )
  })

  output$bt_equity_plot <- renderPlot({
    res <- bt_results()
    if (is.null(res)) return(NULL)
    plot(res$equity, type = "l", col = "#2ecc71", lwd = 2, main = "账户权益曲线", ylab = "Equity (USD)")
    abline(h = res$equity[1], col = "red", lty = 2)
  })

  output$plot <- renderPlot({
    data <- processed_ticker_data()
    if (is.null(data) || nrow(data) < 2) return(NULL)
    days_to_show <- period_days()
    visible_data <- tail(data, days_to_show)
    subset_range <- paste0(start(visible_data), "::")
    
    cs <- chart_Series(data, name = current_ticker(), subset = subset_range, type = input$plot_type, theme = chart_theme())
    cs <- add_Vo()
    selected_indicators <- input$indicators
    if ("SMA" %in% selected_indicators) {
      if (nrow(data) >= 5) cs <- add_SMA(n = 5, col = "blue")
      if (nrow(data) >= 20) cs <- add_SMA(n = 20, col = "red")
    }
    if ("BBands" %in% selected_indicators && nrow(data) >= 20) cs <- add_BBands(n = 20)
    if ("MACD" %in% selected_indicators && nrow(data) >= 26) cs <- add_MACD()
    if ("RSI" %in% selected_indicators && nrow(data) >= 14) cs <- add_RSI(n = 14)
    if ("SuperTrend" %in% selected_indicators) {
      st_res <- SuperTrend(HLC(data), n = 10, factor = 3)
      st_line <- st_res$supertrend; st_dir <- st_res$direction
      st_up <- st_line; st_up[st_dir == -1] <- NA
      st_down <- st_line; st_down[st_dir == 1] <- NA
      cs <- add_TA(st_up, on = 1, col = "green", lwd = 2)
      cs <- add_TA(st_down, on = 1, col = "red", lwd = 2)
    }
    print(cs)
  })

  output$data <- renderTable({
    data <- processed_ticker_data(); if (is.null(data)) return(NULL)
    df <- data.frame(Date = as.character(index(tail(data, 10))), coredata(tail(data, 10)))
    names(df) <- gsub(".*\\.", "", names(df))
    df
  }, striped = TRUE, hover = TRUE)
  
  # --------------------------------------------------------------------
  # AI 金融助手 (REFACTORED for proper reactive data flow)
  # --------------------------------------------------------------------
  
  chat_history <- reactiveVal(list())
  chat_loading <- reactiveVal(FALSE)
  pending_ai_request <- reactiveVal(NULL)

  run_ai_chat_analysis <- function(user_input, history_to_use, question_info) {
    req(user_input, history_to_use, question_info)
    chat_loading(TRUE)
    
    tryCatch({
      data <- ticker_data()
      ticker <- current_ticker()
      need_data <- question_info$need_data
      
      stock_data_text <- ""
      if (need_data && !is.null(data) && nrow(data) > 0) {
        latest <- tail(data, 1)
        price <- as.numeric(Cl(latest))
        prev <- tail(data, 2)[1]
        change <- price - as.numeric(Cl(prev))
        pct_change <- (change / as.numeric(Cl(prev))) * 100
        
        # 计算一些常用指标
        sma5 <- mean(tail(Cl(data), 5), na.rm = TRUE)
        sma20 <- mean(tail(Cl(data), 20), na.rm = TRUE)
        rsi <- tail(RSI(Cl(data), n = 14), 1)
        
        stock_data_text <- paste0(
          "\n\n当前股票数据 (", ticker, "):\n",
          "- 最新价格: ", round(price, 2), "\n",
          "- 今日涨跌: ", round(change, 2), " (", round(pct_change, 2), "%)\n",
          "- 技术指标: SMA5=", round(sma5, 2), ", SMA20=", round(sma20, 2), ", RSI14=", round(rsi, 1), "\n",
          "- 最近 30 天 K 线数据:\n", format_kline_data(data, 30)
        )
      }
      
      system_prompt <- if (need_data && nchar(stock_data_text) > 0) {
        paste0("你是一位专业的金融投资顾问 AI 助手。我已经为你提供了该股票的实时数据和 K 线数据，请基于这些数据进行分析和回答。不要说你'没有数据'或'无法获取数据'。", stock_data_text)
      } else {
        "你是一位专业的金融投资顾问 AI 助手。请用中文回答用户的问题，保持专业和详细。"
      }
      
      provider <- input$ai_provider
      model_id <- input$ai_model %||% "gemini-2.5-flash"
      temperature <- input$ai_temperature %||% 0.7
      max_tokens <- input$ai_max_tokens %||% 1024
      
      ai_response <- ""
      
      if (provider == "gemini") {
        api_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_id, ":generateContent")
        msg_contents <- lapply(history_to_use, function(m) {
          role_map <- c("user" = "user", "assistant" = "model")
          list(role = role_map[m$role], parts = list(list(text = m$content)))
        })
        req_body <- list(
          contents = msg_contents,
          systemInstruction = list(parts = list(list(text = system_prompt))),
          generationConfig = list(temperature = temperature, maxOutputTokens = max_tokens)
        )
        if (input$ai_enable_search) {
          req_body$tools <- list(list(google_search = setNames(list(), character(0))))
        }
        resp <- request(api_url) %>% req_url_query(key = gemini_apiKey) %>% req_method("POST") %>% req_body_json(req_body) %>% req_perform()
        result <- resp_body_json(resp)
        ai_response <- result$candidates[[1]]$content$parts[[1]]$text
      } else if (provider == "minimax") {
        api_url <- "https://api.minimax.chat/v1/text/chatcompletion_v2"
        msgs <- list(list(role = "system", content = system_prompt))
        for (m in history_to_use) msgs[[length(msgs) + 1]] <- list(role = m$role, content = m$content)
        resp <- request(api_url) %>% req_method("POST") %>% req_headers(Authorization = paste0("Bearer ", minimax_apiKey), "Content-Type" = "application/json") %>% req_body_json(list(model = model_id, messages = msgs, temperature = temperature, max_tokens = max_tokens)) %>% req_perform()
        result <- resp_body_json(resp)
        ai_response <- result$choices[[1]]$message$content
      } else if (provider == "aliyun") {
        api_url <- "https://coding.dashscope.aliyuncs.com/v1/chat/completions"
        msgs <- list(list(role = "system", content = system_prompt))
        for (m in history_to_use) msgs[[length(msgs) + 1]] <- list(role = m$role, content = m$content)
        resp <- request(api_url) %>% req_method("POST") %>% req_headers(Authorization = paste0("Bearer ", aliyun_apiKey), "Content-Type" = "application/json") %>% req_body_json(list(model = model_id, messages = msgs, temperature = temperature, max_tokens = max_tokens)) %>% req_perform()
        result <- resp_body_json(resp)
        ai_response <- result$choices[[1]]$message$content
      }
      
      updated_history <- history_to_use
      updated_history[[length(updated_history) + 1]] <- list(role = "assistant", content = ai_response)
      chat_history(updated_history)
      
      # 处理 AI 回复中的 Markdown (清理多余的标题符号以便在聊天框中更好看)
      processed_response <- gsub("^(#+)\\s+", "", ai_response)
      chat_append("chat", response = processed_response, role = "assistant")
      
    }, error = function(e) {
      error_msg <- paste("抱歉，处理您的请求时出现错误：", e$message)
      chat_append("chat", response = error_msg, role = "assistant")
    })
    
    chat_loading(FALSE)
  }

  observeEvent(ticker_data(), {
    req(pending_ai_request())
    request_details <- pending_ai_request()
    pending_ai_request(NULL)
    
    invalidateLater(250, session)
    
    isolate({
      run_ai_chat_analysis(
        user_input = request_details$question,
        history_to_use = request_details$history,
        question_info = request_details$q_info
      )
    })
  }, ignoreInit = TRUE, priority = -10)

  observeEvent(input$chat_user_input, {
    user_input <- input$chat_user_input
    req(trimws(user_input) != "")
    
    current_h <- chat_history()
    current_h[[length(current_h) + 1]] <- list(role = "user", content = user_input)
    chat_history(current_h)
    
    showNotification("AI 正在分析问题...", type = "message")
    
    q_info <- ask_ai_question_info(user_input, isolate(current_ticker()))
    
    is_switching <- q_info$need_data && q_info$switch_stock && q_info$ticker != isolate(current_ticker())
    
    if (is_switching) {
      chat_append("chat", response = paste0("🔄 识别到新股票：**", q_info$ticker, "**，正在加载数据..."), role = "assistant")
      pending_ai_request(list(question = user_input, history = current_h, q_info = q_info))
      switch_ticker(q_info$ticker)
    } else {
      if (q_info$need_data) {
         chat_append("chat", response = "🔍 正在分析当前股票数据...", role = "assistant")
      }
      run_ai_chat_analysis(user_input = user_input, history_to_use = current_h, question_info = q_info)
    }
  })

  session$onFlushed(function() {
    chat_append("chat", response = "您好！我是您的AI金融助手。", role = "assistant")
  }, once = TRUE)

  output$chat_status <- renderUI({
    if (chat_loading()) {
      span(class = "badge bg-warning", "AI 正在分析中...")
    } else {
      model <- input$ai_model %||% "gemini-2.5-flash"
      span(class = "badge bg-success", paste("已连接", model))
    }
  })

  observeEvent(input$clear_chat, {
    chat_history(list())
    showNotification("对话已清空", type = "message")
  })
  
  observeEvent(input$quick_q1, { update_chat_user_input("chat", value = "请帮我分析当前股票的技术面，包括趋势、支撑阻力位和技术指标信号", submit = TRUE) })
  observeEvent(input$quick_q2, { update_chat_user_input("chat", value = "请解读一下最新的财务指标，包括营收、利润、估值等", submit = TRUE) })
  observeEvent(input$quick_q3, { update_chat_user_input("chat", value = "基于目前的技术面和基本面，请预测一下未来走势", submit = TRUE) })
  observeEvent(input$quick_q4, { update_chat_user_input("chat", value = "基于当前分析，请给出具体的买卖建议和仓位管理", submit = TRUE) })
  observeEvent(input$quick_q5, { update_chat_user_input("chat", value = "这只股票最近有什么重大新闻或事件吗？", submit = TRUE) })
  observeEvent(input$quick_q6, { update_chat_user_input("chat", value = "请解释一下什么是RSI指标，如何使用它来辅助交易？", submit = TRUE) })
  
}

# Run app
shinyApp(ui = ui, server = server)

