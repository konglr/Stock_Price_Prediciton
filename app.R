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
library(markdown)
library(rvest)


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
            # 聊天界面布局
            div(class = "row g-0",
                # 左侧：聊天消息区域
                div(class = "col-md-9",
                    div(style = "height: 450px; display: flex; flex-direction: column;",
                        # 聊天消息容器
                        div(id = "chat_container", 
                            class = "flex-grow-1 overflow-auto p-3", 
                            style = "background: #f8f9fa; border-radius: 10px; border: 1px solid #e9ecef;",
                            uiOutput("chat_messages_ui")
                        ),
                        # 输入区域
                        div(class = "mt-3 d-flex gap-2",
                            textInput("chat_input", NULL, 
                                      placeholder = "输入您的问题，例如：帮我分析一下苹果公司的股票...",
                                      width = "100%"),
                            actionButton("send_chat", "发送", 
                                        class = "btn-primary",
                                        icon = icon("paper-plane"))
                        ),
                        # 操作按钮
                        div(class = "mt-2 d-flex justify-content-between align-items-center",
                            actionButton("clear_chat", "清空对话", 
                                        class = "btn-outline-secondary btn-sm",
                                        icon = icon("trash")),
                            uiOutput("chat_status")
                        )
                    )
                ),
                # 右侧：快捷操作
                div(class = "col-md-3 ps-3",
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
            ),
            # 自定义样式
            tags$style("
              .chat-message { max-width: 85%; padding: 12px 16px; border-radius: 15px; margin-bottom: 12px; font-size: 0.9rem; }
              .chat-user { background: #007bff; color: white; margin-left: auto; border-bottom-right-radius: 3px; }
              .chat-ai { background: #e9ecef; color: #333; margin-right: auto; border-bottom-left-radius: 3px; }
              .chat-time { font-size: 0.7rem; opacity: 0.7; margin-top: 4px; }
              .chat-avatar { width: 28px; height: 28px; border-radius: 50%; display: inline-flex; align-items: center; justify-content: center; margin-right: 8px; }
              .chat-avatar-user { background: #007bff; color: white; }
              .chat-avatar-ai { background: #28a745; color: white; }
              /* Markdown 格式化样式 */
              .markdown-body { line-height: 1.6; }
              .markdown-body h1, .markdown-body h2, .markdown-body h3 { 
                font-size: 1.1rem; font-weight: 600; margin: 12px 0 8px 0; color: #2c3e50; border-bottom: 1px solid #eee; padding-bottom: 4px; 
              }
              .markdown-body h1 { font-size: 1.3rem; }
              .markdown-body h2 { font-size: 1.15rem; }
              .markdown-body h3 { font-size: 1.05rem; }
              .markdown-body p { margin: 8px 0; }
              .markdown-body ul, .markdown-body ol { padding-left: 20px; margin: 8px 0; }
              .markdown-body li { margin: 4px 0; }
              .markdown-body table { 
                width: 100%; border-collapse: collapse; margin: 10px 0; font-size: 0.85rem; 
              }
              .markdown-body th, .markdown-body td { 
                border: 1px solid #ddd; padding: 6px 8px; text-align: left; 
              }
              .markdown-body th { background: #f5f5f5; font-weight: 600; }
              .markdown-body tr:nth-child(even) { background: #fafafa; }
              .markdown-body code { 
                background: #f0f0f0; padding: 2px 5px; border-radius: 3px; font-family: 'Courier New', monospace; font-size: 0.85em; 
              }
              .markdown-body pre { 
                background: #f5f5f5; padding: 10px; border-radius: 5px; overflow-x: auto; font-size: 0.8rem; 
              }
              .markdown-body blockquote { 
                border-left: 3px solid #3498db; padding-left: 10px; margin: 10px 0; color: #555; 
              }
              .markdown-body strong { font-weight: 600; color: #2c3e50; }
              .markdown-body em { font-style: italic; }
              .markdown-body hr { border: none; border-top: 1px solid #eee; margin: 12px 0; }
            ")
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
  
  # 动态渲染模型选择 UI
  output$ai_model_ui <- renderUI({
    provider <- input$ai_provider
    
    if (provider == "gemini") {
      selectInput(
        inputId = "ai_model",
        label = "选择模型",
        choices = c(
          "Gemini 3.1 Pro (预览)" = "gemini-3.1-pro-preview",
          "Gemini 3.0 Flash (预览)" = "gemini-3-flash-preview",
          "Gemini 2.5 Flash (预览)" = "gemini-2.5-flash",
          "Gemini 2.0 Flash (最新)" = "gemini-2.0-flash",
          "Gemini 1.5 Pro" = "gemini-1.5-pro",
          "Gemini 1.5 Flash" = "gemini-1.5-flash"
        ),
        selected = "gemini-2.5-flash"
      )
    } else if (provider == "minimax") {
      selectInput(
        inputId = "ai_model",
        label = "选择模型",
        choices = c(
          "MiniMax-M2.5 (推荐)" = "MiniMax-M2.5",
          "MiniMax-M2.1" = "MiniMax-M2.1"
        ),
        selected = "MiniMax-M2.5"
      )
    } else if (provider == "aliyun") {
      selectInput(
        inputId = "ai_model",
        label = "选择模型",
        choices = c(
          "通义千问 3.5 Plus (推荐)" = "qwen3.5-plus",
          "通义千问 3 Max" = "qwen3-max-2026-01-23",
          "通义千问 3 Coder Next" = "qwen3-coder-next",
          "通义千问 3 Coder Plus" = "qwen3-coder-plus",
          "智谱 GLM-4.7" = "glm-4.7",
          "Moonshot Kimi K2.5" = "kimi-k2-5"
        ),
        selected = "qwen3.5-plus"
      )
    }
  })
  
  # 在侧边栏显示当前选中的模型标签
  output$selected_model_badge <- renderUI({
    provider <- input$ai_provider
    model <- input$ai_model %||% "gemini-2.5-flash"
    
    if (provider == "gemini") {
      model_name <- switch(model,
                          "gemini-3.1-pro-preview" = "Gemini 3.1 Pro",
                          "gemini-3-flash-preview" = "Gemini 3.0 Flash",
                          "gemini-2.5-flash" = "Gemini 2.5 Flash",
                          "gemini-2.0-flash" = "Gemini 2.0 Flash",
                          "gemini-1.5-pro" = "Gemini 1.5 Pro",
                          "gemini-1.5-flash" = "Gemini 1.5 Flash",
                         model)
      span(model_name, class = "badge bg-primary", style = "font-size: 0.75rem;")
    } else if (provider == "minimax") {
      model_name <- switch(model,
                         "MiniMax-M2.5 (推荐)" = "MiniMax-M2.5",
                         "MiniMax-M2.1" = "MiniMax-M2.1",
                         model)
      span(model_name, class = "badge bg-success", style = "font-size: 0.75rem;")
    } else if (provider == "aliyun") {
      model_name <- switch(model,
                          "qwen3.5-plus" = "Qwen 3.5 Plus",
                          "qwen3-max-2026-01-23" = "Qwen 3 Max",
                          "qwen3-coder-next" = "Qwen 3 Coder Next",
                          "qwen3-coder-plus" = "Qwen 3 Coder Plus",
                          "glm-4.7" = "GLM-4.7",
                          "kimi-k2-5" = "Kimi K2.5",
                          model)
      span(model_name, class = "badge bg-info", style = "font-size: 0.75rem;")
    } else {
      span(model, class = "badge bg-secondary", style = "font-size: 0.75rem;")
    }
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
  
  # AI 联网预测逻辑
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
    
    # AI 分析核心提示词
    base_prompt <- "您是一位拥有20年经验的资深美股投资专家。
    任务：基于用户提供的历史交易数据以及最新的市场信息，进行多维度的量化分析与基本面评估。"
    
    if (input$ai_enable_search) {
      base_prompt <- paste0(base_prompt, "\n你可以通过联网搜索工具获取最新的财务数据、行业对比数据和新闻。")
    }
    
    system_prompt <- paste0(base_prompt, "\n
    ### 强制响应格式：
    必须仅返回一个合法的 JSON 字符串，禁止包含 ```json 标签或任何前后导言。
    JSON 结构模板：
    {
      \"news\": \"(这里放核心新闻总结)\",
      \"financial\": \"(这里放财务、行业对比、估值分析)\",
      \"trend\": \"(这里放当前走势描述)\",
      \"prediction_5d\": [价格1, 价格2, 价格3, 价格4, 价格5],
      \"reasoning\": \"(这里放详细投资逻辑)\",
      \"support_level\": 支撑价位,
      \"resistance_level\": 阻力价位,
      \"trade_advice\": {
        \"action\": \"买入/持有/观望\",
        \"buy_price\": 建议买入价,
        \"take_profit\": 止盈价,
        \"stop_loss\": 止损价
      }
    }")
    
    user_query <- paste0("股票代码: ", current_ticker(), "\n最近半年历史交易明细数据：\n", data_summary)
    
    provider <- input$ai_provider
    model_id <- input$ai_model %||% "gemini-2.5-flash"
    
    # 获取用户设置的参数
    temperature <- input$ai_temperature %||% 0.7
    max_tokens <- input$ai_max_tokens %||% 1024
    
    tryCatch({
      if (provider == "gemini") {
        # Gemini API 调用
        api_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_id, ":generateContent")
        
        # 构建请求体
        req_body <- list(
          contents = list(
            list(
              role = "user",
              parts = list(list(text = user_query))
            )
          ), 
          systemInstruction = list(parts = list(list(text = system_prompt))), 
          generationConfig = list(
            temperature = temperature,
            maxOutputTokens = max_tokens
          )
        )
        
        # 如果启用搜索，添加 Google Search
        if (input$ai_enable_search) {
          req_body$tools <- list(
            list(google_search = setNames(list(), character(0)))
          )
        }
        
        resp <- request(api_url) %>%
          req_url_query(key = gemini_apiKey) %>% 
          req_method("POST") %>%
          req_body_json(req_body) %>%
          req_retry(max_tries = 5, backoff = ~ 1 * 2^(.x - 1)) %>%
          req_perform()
        
        # 解析响应，增加错误处理
        result <- tryCatch({
          resp_body_json(resp)
        }, error = function(e) {
          stop(paste("API 响应解析失败:", e$message))
        })
        
        # 检查响应是否有效
        if (is.null(result) || is.null(result$candidates) || length(result$candidates) == 0) {
          stop("API 返回结果为空，请检查 API Key 是否正确")
        }
        
        # 检查 content 是否存在
        if (is.null(result$candidates[[1]]$content) || is.null(result$candidates[[1]]$content$parts) || 
            length(result$candidates[[1]]$content$parts) == 0) {
          stop("API 返回的内容为空")
        }
        
        # 增强的内容提取：寻找包含 text 的第一个 part
        raw_text <- ""
        parts <- result$candidates[[1]]$content$parts
        for (p in parts) {
          if (!is.null(p$text)) {
            raw_text <- paste0(raw_text, p$text)
          }
        }
        
        if (raw_text == "") {
           # 如果没有文字，检查是否因为安全过滤
           reason <- result$candidates[[1]]$finishReason %||% "UNKNOWN"
           stop(paste("Gemini 未返回文字内容。结束原因:", reason))
        }
        
        # 处理 Grounding Metadata - 增加对不同结构的兼容性
        metadata <- result$candidates[[1]]$groundingMetadata
        if (!is.null(metadata)) {
          sources <- list()
          
          # 尝试从 groundingAttributions 获取 (旧版/部分模型)
          if (!is.null(metadata$groundingAttributions)) {
            sources <- lapply(metadata$groundingAttributions, function(s) {
              list(title = s$web$title %||% "网页来源", uri = s$web$uri %||% "#")
            })
          } 
          # 尝试从 groundingChunks 获取 (新版 v1beta)
          else if (!is.null(metadata$groundingChunks)) {
            sources <- lapply(metadata$groundingChunks, function(c) {
              if (!is.null(c$web)) {
                list(title = c$web$title %||% "网页来源", uri = c$web$uri %||% "#")
              } else {
                NULL
              }
            })
            sources <- Filter(Negate(is.null), sources)
          }
          
          if (length(sources) > 0) {
            ai_grounding(sources)
          }
        }
        
      } else if (provider == "minimax") {
        # MiniMax API 调用
        api_url <- "https://api.minimax.chat/v1/text/chatcompletion_v2"
        
        resp <- request(api_url) %>%
          req_method("POST") %>%
          req_headers(
            "Authorization" = paste0("Bearer ", minimax_apiKey),
            "Content-Type" = "application/json"
          ) %>%
          req_body_json(list(
            model = model_id,
            messages = list(
              list(role = "system", content = system_prompt),
              list(role = "user", content = user_query)
            ),
            tools = if (input$ai_enable_search) list(list(type = "web_search", web_search = setNames(list(), character(0)))) else NULL,
            temperature = temperature,
            max_tokens = max_tokens
          )) %>%
          req_retry(max_tries = 5, backoff = ~ 1 * 2^(.x - 1)) %>%
          req_perform()
        
        # 解析 MiniMax 响应，增加错误处理
        result <- tryCatch({
          resp_body_json(resp)
        }, error = function(e) {
          stop(paste("MiniMax API 响应解析失败:", e$message))
        })
        
        # 检查响应是否有效
        if (is.null(result) || is.null(result$choices) || length(result$choices) == 0) {
          stop("MiniMax API 返回结果为空，请检查 API Key 和模型名称是否正确")
        }
        
        # 检查 message 是否存在
        if (is.null(result$choices[[1]]$message) || is.null(result$choices[[1]]$message$content)) {
          stop("MiniMax API 返回的内容为空")
        }
        
        raw_text <- result$choices[[1]]$message$content
        
        # MiniMax 没有 grounding，返回空
        ai_grounding(NULL)
      } else if (provider == "aliyun") {
        # 阿里云百炼 API 调用
        api_url <- "https://coding.dashscope.aliyuncs.com/v1/chat/completions"
        
        resp <- request(api_url) %>%
          req_method("POST") %>%
          req_headers(
            "Authorization" = paste0("Bearer ", aliyun_apiKey),
            "Content-Type" = "application/json"
          ) %>%
          req_body_json(list(
            model = model_id,
            messages = list(
              list(role = "system", content = system_prompt),
              list(role = "user", content = user_query)
            ),
            temperature = temperature,
            max_tokens = max_tokens
          )) %>%
          req_retry(max_tries = 5, backoff = ~ 1 * 2^(.x - 1)) %>%
          req_perform()
        
        # 解析阿里云响应，增加错误处理
        result <- tryCatch({
          resp_body_json(resp)
        }, error = function(e) {
          stop(paste("阿里云 API 响应解析失败:", e$message))
        })
        
        # 检查响应是否有效
        if (is.null(result) || is.null(result$choices) || length(result$choices) == 0) {
          stop("阿里云 API 返回结果为空，请检查 API Key 和模型名称是否正确")
        }
        
        # 检查 message 是否存在
        if (is.null(result$choices[[1]]$message) || is.null(result$choices[[1]]$message$content)) {
          stop("阿里云 API 返回的内容为空")
        }
        
        raw_text <- result$choices[[1]]$message$content
        
        # 阿里云没有 grounding，返回空
        ai_grounding(NULL)
      }
      
      # 解析 JSON - 采用更鲁棒的嵌套块检测逻辑
      parsed_res <- NULL
      
      # 辅助函数：从文本中提取最外层的 {} 块
      extract_outer_blocks <- function(text) {
        chars <- strsplit(text, "")[[1]]
        stack <- 0
        start <- -1
        blocks <- c()
        for (i in seq_along(chars)) {
          if (chars[i] == "{") {
            if (stack == 0) start <- i
            stack <- stack + 1
          } else if (chars[i] == "}") {
            stack <- stack - 1
            if (stack == 0 && start != -1) {
              blocks <- c(blocks, substr(text, start, i))
              start <- -1
            }
          }
        }
        return(blocks)
      }
      
      json_candidates <- extract_outer_blocks(raw_text)
      
      # 如果没找到，尝试清理常见干扰字符
      if (length(json_candidates) == 0) {
        # 去掉 Markdown 标签、不可见字符
        clean_tmp <- gsub("```json|```", "", raw_text)
        clean_tmp <- trimws(clean_tmp)
        json_candidates <- extract_outer_blocks(clean_tmp)
      }
      
      if (length(json_candidates) > 0) {
        # 优先从后往前找包含核心预测字段的有效 JSON
        for (candidate in rev(json_candidates)) {
          # 检查是否包含预测字段关键字，忽略包含 tool => 的非 JSON 块
          if (grepl("\"news\"|\"prediction_5d\"|\"trend\"", candidate, fixed = FALSE)) {
            try({
              tmp_res <- fromJSON(candidate)
              if (!is.null(tmp_res$news) || !is.null(tmp_res$trend)) {
                parsed_res <- tmp_res
                break
              }
            }, silent = TRUE)
          }
        }
      }
      
      # 如果还是没找到，尝试清理 Markdown 代码块后再找
      if (is.null(parsed_res) && grepl("```", raw_text)) {
        clean_text <- gsub("```json|```", "", raw_text)
        json_candidates <- extract_outer_blocks(clean_text)
        for (candidate in rev(json_candidates)) {
          if (grepl("\"news\"|\"prediction_5d\"|\"trend\"", candidate)) {
            try({
              tmp_res <- fromJSON(candidate)
              if (!is.null(tmp_res$news) || !is.null(tmp_res$trend)) {
                parsed_res <- tmp_res
                break
              }
            }, silent = TRUE)
          }
        }
      }

      if (!is.null(parsed_res)) {
        ai_prediction(parsed_res)
      } else {
        # 失败时输出完整内容到控制台辅助调试
        message("--- DEBUG: AI RAW OUTPUT ---")
        message(raw_text)
        message("--- DEBUG: END ---")
        
        sample_text <- substr(raw_text, 1, 100)
        stop(paste0("AI 返回内容未找到有效 JSON 预测。内容摘要：", sample_text, "..."))
      }
      
    }, error = function(e) {
      # 捕获详细错误日志
      err_msg <- e$message
      if (!is.null(e$response)) {
        try({
          resp_err <- resp_body_json(e$response)
          if (!is.null(resp_err$error$message)) {
            err_msg <- paste0(err_msg, " - ", resp_err$error$message)
          }
        }, silent = TRUE)
      }
      ai_prediction(list(error = paste("AI 联网分析失败:", err_msg)))
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
      provider <- input$ai_provider
      provider_name <- ifelse(provider == "gemini", "Gemini", "MiniMax")
      return(p(paste("点击按钮启动", provider_name, "联网投研深度分析"), class="text-muted p-3 text-center"))
    }
    
    if (!is.null(res$error)) {
      return(div(class="alert alert-danger", as.character(res$error)))
    }
    
      # 获取友好的模型名称
      model_name <- switch(input$ai_model,
                           "gemini-3.1-pro-preview" = "Gemini 3.1 Pro",
                           "gemini-3-flash-preview" = "Gemini 3.0 Flash",
                           "gemini-2.5-flash" = "Gemini 2.5 Flash",
                           "gemini-2.0-flash" = "Gemini 2.0 Flash",
                           "gemini-1.5-pro" = "Gemini 1.5 Pro",
                           "gemini-1.5-flash" = "Gemini 1.5 Flash",
                           "MiniMax-M2.5" = "MiniMax-M2.5",
                           "MiniMax-M2.1" = "MiniMax-M2.1",
                           "qwen3.5-plus" = "Qwen 3.5 Plus",
                           "qwen3-max-2026-01-23" = "Qwen 3 Max",
                           "qwen3-coder-next" = "Qwen 3 Coder Next",
                           "qwen3-coder-plus" = "Qwen 3 Coder Plus",
                           "glm-4.7" = "GLM-4.7",
                           "kimi-k2-5" = "Kimi K2.5",
                           input$ai_model)
      
      tagList(
      div(class="mb-3 d-flex align-items-center justify-content-between",
          h5(paste0("综合研判：", res$trend), class="text-primary fw-bold mb-0"),
          div(class="d-flex gap-2",
              span(class="badge bg-secondary", model_name),
              if (input$ai_provider == "gemini") {
                span(class="badge bg-success", "Google Search 联网数据已接入")
              } else if (input$ai_provider == "minimax") {
                span(class="badge bg-info", "MiniMax AI 智能分析")
              } else if (input$ai_provider == "aliyun") {
                span(class="badge bg-warning", "阿里云百炼 AI 智能分析")
              }
          )
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
      
      # 渲染 Grounding 来源链接列表 (仅 Gemini)
      if (!is.null(sources) && input$ai_provider == "gemini") {
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
  
  # 绩效分析面板指标 (响应时间跨度 + 全局对比)
  output$bt_performance_stats <- renderUI({
    res <- bt_results()
    data <- processed_ticker_data()
    if (is.null(res) || is.null(data)) return(p("待回测完成后生成报告...", class="text-muted"))
    
    # 通用千分位格式化函数
    fmt_c <- function(x) format(round(as.numeric(x), 2), big.mark=",")
    
    # --- 1. 全局统计计算 ---
    log_all <- res$log
    sells_all <- log_all[log_all$Action == "SELL", ]
    
    init_all <- res$equity[1]
    final_all <- tail(res$equity, 1)
    profit_all <- final_all - init_all
    roi_all <- (profit_all / init_all) * 100
    
    win_rate_all <- if(nrow(sells_all)>0) (sum(sells_all$PnL > 0) / nrow(sells_all)) * 100 else 0
    max_p_pct_all <- if(nrow(sells_all)>0) max(sells_all$PnL_Pct, na.rm=TRUE) else 0
    max_p_amt_all <- if(nrow(sells_all)>0) max(sells_all$PnL, na.rm=TRUE) else 0
    max_l_pct_all <- if(nrow(sells_all)>0) min(sells_all$PnL_Pct, na.rm=TRUE) else 0
    max_l_amt_all <- if(nrow(sells_all)>0) min(sells_all$PnL, na.rm=TRUE) else 0
    
    peaks_all <- cummax(res$equity)
    max_dd_all <- min((res$equity - peaks_all) / peaks_all * 100)
    
    # --- 2. 周期统计计算 ---
    days_to_show <- switch(input$period, "20天"=20, "1月"=30, "3月"=90, "6月"=180, "1年"=365, "2年"=730, "5年"=1825, "10年"=3650, 365)
    start_date <- end(data) - days_to_show
    
    visible_idx <- which(index(data) >= start_date)
    v_equity <- res$equity[visible_idx]
    v_log <- res$log[res$log$Date >= start_date, ]
    v_sells <- v_log[v_log$Action == "SELL", ]
    
    init_v <- v_equity[1]
    final_v <- tail(v_equity, 1)
    profit_v <- final_v - init_v
    roi_v <- (profit_v / init_v) * 100
    
    win_rate_v <- if(nrow(v_sells)>0) (sum(v_sells$PnL > 0) / nrow(v_sells)) * 100 else 0
    max_p_pct_v <- if(nrow(v_sells)>0) max(v_sells$PnL_Pct, na.rm=TRUE) else 0
    max_p_amt_v <- if(nrow(v_sells)>0) max(v_sells$PnL, na.rm=TRUE) else 0
    max_l_pct_v <- if(nrow(v_sells)>0) min(v_sells$PnL_Pct, na.rm=TRUE) else 0
    max_l_amt_v <- if(nrow(v_sells)>0) min(v_sells$PnL, na.rm=TRUE) else 0
    
    v_peaks <- cummax(v_equity)
    v_max_dd <- min((v_equity - v_peaks) / v_peaks * 100)
    
    tagList(
      # 第一行：项目全局 (7 列)
      h6(bsicons::bs_icon("archive"), " 1. 项目全生命周期 (Global Summary)", class="mb-3", style="color: #6c757d; font-weight: bold;"),
      layout_column_wrap(
        width = 1/7,
        style = "margin-bottom: 25px;",
        value_box(title = "初期总投入", value = sprintf("$%s", fmt_c(init_all)), theme = "secondary"),
        value_box(title = "累计总收益", value = sprintf("$%s (%+.2f%%)", fmt_c(profit_all), roi_all), theme = if(profit_all>=0) "success" else "danger"),
        value_box(title = "总胜率", value = sprintf("%.1f%%", win_rate_all), theme = "info"),
        value_box(title = "交易总次数", value = nrow(sells_all), theme = "light"),
        value_box(title = "最大单笔收益", value = sprintf("$%s (%+.2f%%)", fmt_c(max_p_amt_all), max_p_pct_all), theme = "success"),
        value_box(title = "最大单笔亏损", value = sprintf("$%s (%.2f%%)", fmt_c(max_l_amt_all), max_l_pct_all), theme = "danger"),
        value_box(title = "全局最大回撤", value = sprintf("%.2f%%", max_dd_all), theme = "danger")
      ),
      
      # 第二行：所选周期 (7 列)
      h6(bsicons::bs_icon("calendar-range"), paste(" 2. 所选周期绩效 (", input$period, ")"), class="mb-3", style="color: #6c757d; font-weight: bold;"),
      layout_column_wrap(
        width = 1/7,
        value_box(title = "周期起始投入", value = sprintf("$%s", fmt_c(init_v)), theme = "secondary"),
        value_box(title = "周期内收益", value = sprintf("$%s (%+.2f%%)", fmt_c(profit_v), roi_v), theme = if(profit_v>=0) "success" else "danger"),
        value_box(title = "周期胜率", value = sprintf("%.1f%%", win_rate_v), theme = "info"),
        value_box(title = "周期交易次数", value = nrow(v_sells), theme = "light"),
        value_box(title = "周期最大收益", value = sprintf("$%s (%.2f%%)", fmt_c(max_p_amt_v), max_p_pct_v), theme = "success"),
        value_box(title = "周期最大亏损", value = sprintf("$%s (%.2f%%)", fmt_c(max_l_amt_v), max_l_pct_v), theme = "danger"),
        value_box(title = "周期最大回撤", value = sprintf("%.2f%%", v_max_dd), theme = "danger")
      )
    )
  })
  
  # TradingView 风格权益曲线绘图 (响应时间跨度)
  output$bt_equity_plot <- renderPlot({
    res <- bt_results()
    data <- processed_ticker_data()
    if (is.null(res) || is.null(data)) return(NULL)
    
    days_to_show <- switch(input$period, "20天" = 20, "1月" = 30, "3月" = 90, "6月" = 180, "1年" = 365, "2年" = 730, "5年" = 1825, "10年" = 3650, 365)
    start_dt <- end(data) - days_to_show
    
    visible_select <- index(data) >= start_dt
    dates <- index(data)[visible_select]
    equity <- res$equity[visible_select]
    
    # 1. 计算局部回撤
    peaks <- cummax(equity)
    dd_pct <- (equity - peaks) / peaks * 100
    
    # 2. 局部权益范围
    y_min <- min(equity)
    y_max <- max(equity)
    scale_y <- function(val) { (val - y_min) / (y_max - y_min) * 75 }
    
    par(mar = c(3, 4, 3, 4), bg = "#ffffff")
    plot.new()
    plot.window(xlim = range(dates), ylim = c(-20, 100))
    
    # 绘制回撤背景 (顶部向下)
    for(i in seq(1, length(dates), length.out = min(length(dates), 400))) {
        rect(dates[i], 85 + (dd_pct[i]/100 * 15), dates[i], 100, col = "#f1e1ff", border = NA)
    }
    
    # 绘制渐变曲线
    polygon(c(dates, rev(dates)), c(scale_y(equity), rep(0, length(dates))), 
            col = rgb(46, 204, 113, 50, maxColorValue = 255), border = NA)
    lines(dates, scale_y(equity), col = "#2ecc71", lwd = 2.5)
    
    # Y-轴增加详细刻度
    y_ticks <- pretty(c(y_min, y_max), n = 6)
    axis(2, at = scale_y(y_ticks), labels = sprintf("$%.0f", y_ticks), las = 1, cex.axis = 0.8)
    
    # X-轴：增加详细且对齐的日期标识
    x_at <- seq(from = min(dates), to = max(dates), length.out = 10)
    axis.Date(1, at = x_at, format = "%m-%d", cex.axis = 0.8, col = "#e0e0e0")
    
    # 增加垂直网格线辅助对齐
    abline(v = x_at, col = "#f0f0f0", lty = "dotted")
    abline(h = scale_y(y_ticks), col = "#f0f0f0", lty = "dotted")
    
    legend("bottomright", legend = c("策略净值"), col = c("#2ecc71"), lty = 1, bty = "n", cex = 0.9)
    mtext(paste("Time Span:", input$period), side = 3, line = -1, adj = 0.98, col = "#bdc3c7", cex = 0.7)
    mtext("资金回撤分布(%)", side = 3, line = -2, adj = 0.05, col = "#9b59b6", cex = 0.7)
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
  
  # ---------------------------------------------------------
  # AI 金融助手 (聊天功能)
  # ---------------------------------------------------------
  
  # 聊天历史存储
  chat_history <- reactiveVal(list(
    list(role = "assistant", content = "您好！我是您的AI金融助手。我可以帮您分析股票、解读财务指标、讨论投资策略等。请随时向我提问！")
  ))
  
  # 聊天状态
  chat_loading <- reactiveVal(FALSE)
  
  # 渲染聊天消息
  output$chat_messages_ui <- renderUI({
    messages <- chat_history()
    
    if (length(messages) == 0) {
      return(div(class = "text-center text-muted p-4",
                 h5("💬 开始对话"),
                 p("输入您的问题，开始与AI讨论金融话题")))
    }
    
    # 构建消息列表
    msg_tags <- lapply(seq_along(messages), function(i) {
      msg <- messages[[i]]
      is_user <- msg$role == "user"
      
      div(class = if(is_user) "d-flex flex-row-reverse mb-3" else "d-flex mb-3",
          # 头像
          div(class = paste0("chat-avatar ", if(is_user) "chat-avatar-user" else "chat-avatar-ai"),
              if(is_user) "您" else "AI"),
          # 消息气泡
          div(class = paste0("chat-message ", if(is_user) "chat-user" else "chat-ai"),
              if(is_user) {
                # 用户消息：直接显示纯文本
                div(class = "chat-content", msg$content)
              } else {
                # AI消息：渲染Markdown格式内容
                div(class = "chat-content markdown-body",
                    HTML(markdown::markdownToHTML(text = msg$content, fragment.only = TRUE)))
              },
              div(class = "chat-time", format(Sys.time(), "%H:%M"))
          )
      )
    })
    
    # 自动滚动到底部
    session$sendCustomMessage("scrollToBottom", list(id = "chat_container"))
    
    tagList(msg_tags)
  })
  
  # 聊天状态显示
  output$chat_status <- renderUI({
    if (chat_loading()) {
      span(class = "badge bg-warning", "AI 正在思考中...")
    } else {
      provider <- input$ai_provider
      model <- input$ai_model %||% "gemini-2.5-flash"
        provider_name <- ifelse(provider == "gemini", "Gemini", 
                              ifelse(provider == "minimax", "MiniMax", "阿里云百炼"))
      span(class = "badge bg-success", paste0("已连接 ", provider_name))
    }
  })
  
  # 发送聊天消息
  observeEvent(input$send_chat, {
    user_input <- input$chat_input
    if (is.null(user_input) || trimws(user_input) == "") return()
    
    # 添加用户消息
    current_history <- chat_history()
    current_history[[length(current_history) + 1]] <- list(role = "user", content = user_input)
    chat_history(current_history)
    
    # 清空输入框
    updateTextInput(session, "chat_input", value = "")
    
    # 调用 AI
    chat_loading(TRUE)
    
    # 获取当前股票数据作为上下文
    data <- ticker_data()
    ticker <- current_ticker()
    
    # 构建系统提示 - 针对 MiniMax 特殊处理
    system_prompt <- paste0("你是一位专业的金融投资顾问AI助手。当前用户正在分析股票。",
    "请用中文回答用户的问题，保持专业和详细说明。",
    "如果用户询问具体的股票分析，你可以结合技术指标和基本面进行分析。",
    "重要提示：当需要获取网上信息时，请在回答中直接提供相关的URL链接（如 https://finance.yahoo.com/quote/AAPL 等），",
    "格式可以是任何可点击的链接形式。我会自动抓取这些网页来补充信息。")
    
    # 准备消息历史 (转换为 API 格式)
    provider <- input$ai_provider
    model_id <- input$ai_model %||% "gemini-2.5-flash"
    
    # 获取用户设置的参数
    temperature <- input$ai_temperature %||% 0.7
    max_tokens <- input$ai_max_tokens %||% 1024
    enable_search <- input$ai_enable_search %||% TRUE
    
    tryCatch({
      if (provider == "gemini") {
        # Gemini API 调用
        api_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_id, ":generateContent")
        
        # 构建消息内容 - Gemini 角色必须是 user/model
        msg_contents <- lapply(current_history, function(m) {
          role_map <- c("user" = "user", "assistant" = "model")
          list(role = role_map[m$role], parts = list(list(text = m$content)))
        })
        
        # 构建请求体
        chat_req_body <- list(
          contents = msg_contents,
          systemInstruction = list(parts = list(list(text = system_prompt))),
          generationConfig = list(
            temperature = temperature,
            maxOutputTokens = max_tokens
          )
        )
        
        # 根据用户设置决定是否启用 Google Search
        if (enable_search) {
          chat_req_body$tools <- list(
            list(google_search = setNames(list(), character(0)))
          )
        }
        
        resp <- request(api_url) %>%
          req_url_query(key = gemini_apiKey) %>% 
          req_method("POST") %>%
          req_body_json(chat_req_body) %>%
          req_retry(max_tries = 3) %>%
          req_perform()
        
        result <- resp_body_json(resp)
        ai_response <- result$candidates[[1]]$content$parts[[1]]$text
        
      } else if (provider == "minimax") {
        # MiniMax API 调用 - 简化版本：不使用 tools，直接让AI提供URL
        api_url <- "https://api.minimax.chat/v1/text/chatcompletion_v2"

        # MiniMax 消息格式
        msgs <- list(
          list(role = "system", content = system_prompt)
        )
        for (m in current_history) {
          msgs[[length(msgs) + 1]] <- list(role = m$role, content = m$content)
        }

        # 首次调用 API - 不再传递 tools 参数
        minimax_req <- list(
          model = model_id,
          messages = msgs,
          temperature = temperature,
          max_tokens = max_tokens
        )

        resp <- request(api_url) %>%
          req_method("POST") %>%
          req_headers(
            "Authorization" = paste0("Bearer ", minimax_apiKey),
            "Content-Type" = "application/json"
          ) %>%
          req_body_json(minimax_req) %>%
          req_retry(max_tries = 3) %>%
          req_perform()

        # 解析响应，增加错误处理
        result <- tryCatch({
          resp_body_json(resp)
        }, error = function(e) {
          stop(paste("MiniMax API 响应解析失败:", e$message))
        })
        
        # 检查响应是否有效
        if (is.null(result) || is.null(result$choices) || length(result$choices) == 0) {
          stop("MiniMax API 返回结果为空，请检查 API Key 和模型名称是否正确")
        }
        
        # 检查 message 是否存在
        if (is.null(result$choices[[1]]$message) || is.null(result$choices[[1]]$message$content)) {
          stop("MiniMax API 返回的内容为空")
        }
        
        ai_response <- result$choices[[1]]$message$content
        
      } else if (provider == "aliyun") {
        # 阿里云百炼 API 调用
        api_url <- "https://coding.dashscope.aliyuncs.com/v1/chat/completions"
        
        # 阿里云消息格式
        msgs <- list(
          list(role = "system", content = system_prompt)
        )
        for (m in current_history) {
          msgs[[length(msgs) + 1]] <- list(role = m$role, content = m$content)
        }
        
        aliyun_req <- list(
          model = model_id,
          messages = msgs,
          temperature = temperature,
          max_tokens = max_tokens
        )
        
        resp <- request(api_url) %>%
          req_method("POST") %>%
          req_headers(
            "Authorization" = paste0("Bearer ", aliyun_apiKey),
            "Content-Type" = "application/json"
          ) %>%
          req_body_json(aliyun_req) %>%
          req_retry(max_tries = 3) %>%
          req_perform()
        
        # 解析响应，增加错误处理
        result <- tryCatch({
          resp_body_json(resp)
        }, error = function(e) {
          stop(paste("阿里云 API 响应解析失败:", e$message))
        })
        
        # 检查响应是否有效
        if (is.null(result) || is.null(result$choices) || length(result$choices) == 0) {
          stop("阿里云 API 返回结果为空，请检查 API Key 和模型名称是否正确")
        }
        
        # 检查 message 是否存在
        if (is.null(result$choices[[1]]$message) || is.null(result$choices[[1]]$message$content)) {
          stop("阿里云 API 返回的内容为空")
        }
        
        ai_response <- result$choices[[1]]$message$content
      }
      
      # 添加 AI 响应到历史
      current_history <- chat_history()
      current_history[[length(current_history) + 1]] <- list(role = "assistant", content = ai_response)
      chat_history(current_history)
      
    }, error = function(e) {
      # 添加错误消息
      current_history <- chat_history()
      current_history[[length(current_history) + 1]] <- list(
        role = "assistant", 
        content = paste("抱歉，处理您的请求时出现错误：", e$message)
      )
      chat_history(current_history)
    })
    
    chat_loading(FALSE)
  })
  
  # 回车发送
  observeEvent(input$chat_input, {
    if (is.null(input$chat_input)) return()
  })
  
  # 清空对话
  observeEvent(input$clear_chat, {
    chat_history(list(
      list(role = "assistant", content = "对话已清空。请问有什么可以帮助您的？")
    ))
    showNotification("对话已清空", type = "message")
  })
  
  # 快捷问题按钮
  observeEvent(input$quick_q1, {
    updateTextInput(session, "chat_input", value = "请帮我分析当前股票的技术面，包括趋势、支撑阻力位和技术指标信号")
    session$sendCustomMessage("focusElement", list(id = "chat_input"))
  })
  
  observeEvent(input$quick_q2, {
    updateTextInput(session, "chat_input", value = "请解读一下最新的财务指标，包括营收、利润、估值等")
    session$sendCustomMessage("focusElement", list(id = "chat_input"))
  })
  
  observeEvent(input$quick_q3, {
    updateTextInput(session, "chat_input", value = "基于目前的技术面和基本面，请预测一下未来走势")
    session$sendCustomMessage("focusElement", list(id = "chat_input"))
  })
  
  observeEvent(input$quick_q4, {
    updateTextInput(session, "chat_input", value = "基于当前分析，请给出具体的买卖建议和仓位管理")
    session$sendCustomMessage("focusElement", list(id = "chat_input"))
  })
  
  observeEvent(input$quick_q5, {
    updateTextInput(session, "chat_input", value = "这只股票最近有什么重大新闻或事件吗？")
    session$sendCustomMessage("focusElement", list(id = "chat_input"))
  })
  
  observeEvent(input$quick_q6, {
    updateTextInput(session, "chat_input", value = "请解释一下什么是RSI指标，如何使用它来辅助交易？")
    session$sendCustomMessage("focusElement", list(id = "chat_input"))
  })
  
}

# 自定义 JavaScript 消息处理
tags$script(HTML("
  Shiny.addCustomMessageHandler('scrollToBottom', function(message) {
    var el = document.getElementById(message.id);
    if (el) {
      setTimeout(function() {
        el.scrollTop = el.scrollHeight;
      }, 100);
    }
  });
  
  Shiny.addCustomMessageHandler('focusElement', function(message) {
    var el = document.getElementById(message.id);
    if (el) {
      el.focus();
    }
  });
"))

# Run app
shinyApp(ui = ui, server = server)
