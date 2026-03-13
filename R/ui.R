# ==============================================================================
# StockAI - UI 定义模块
# ==============================================================================
# 本文件包含 Shiny UI 的完整定义
# ==============================================================================

# ------------------------------------------------------------------------------
# UI 辅助组件
# ------------------------------------------------------------------------------

# 技术指标选项 (带 tooltip)
indicator_choice_names <- list(
  "SMA (简单移动平均)",
  "BBands (布林带)",
  "MACD",
  "RSI (相对强弱指标)",
  "ADX (平均趋向指标)",
  "SAR (抛物线转向)",
  "OBV (能量潮)",
  "MFI (资金流量指标)",
  "CLV (收盘位置值)",
  htmltools::tags$span(
    "TR (真实波幅)",
    title = "TR（真实波幅）的计算。TR 代表一个特定时段内价格运动的真实范围。为了确保将价格跳空考虑在内，TR 取以下三个值中的最大值：1. 当前时段最高价减去当前时段最低价 (High-Low)。2. 当前时段最高价与前一收盘价之差的绝对值。3. 当前时段最低价与前一收盘价之差的绝对值。计算公式：TR = max[(High-Low), abs(High-PrevClose), abs(Low-PrevClose)]"
  ),
  htmltools::tags$span(
    "ATR (平均真实波幅)",
    title = "ATR（平均真实波幅）的计算。ATR 是对上述 TR 值进行的平滑平均处理，用以反映一段时间内的平均波动水平。1. 周期设置：标准默认设置通常为 14 个周期，但在 Supertrend 等指标中也常使用 10 个周期。2. 计算方法：递归平滑法（Wilder 原版）：这是最常用的方法，公式为 ATR=(前一日ATR*(n-1)+当日TR)/n。简单移动平均法 (SMA)：直接计算指定周期内 TR 的算术平均值。指数移动平均法 (EMA)：部分交易系统为了提高对近期波动的敏感度，会使用 EMA 对 TR 进行平滑。"
  ),
  "SuperTrend"
)

# ------------------------------------------------------------------------------
# 主 UI 定义
# ------------------------------------------------------------------------------
ui <- bslib::page_sidebar(
  header = htmltools::tags$head(
    # Google Analytics
    htmltools::HTML('
      <!-- Global site tag (gtag.js) - Google Analytics -->
      <script async src="https://www.googletagmanager.com/gtag/js?id=G-8LL329L0WC"></script>
      <script>
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag("js", new Date());
        gtag("config", "G-8LL329L0WC");
      </script>
    '),
    # 自定义样式
    htmltools::tags$style("
      /* 聊天 Markdown 标题样式优化 */
      .shiny-chat-messages h1, .shiny-chat-messages h2, .shiny-chat-messages h3 {
        font-size: 0.8rem !important;
        font-weight: bold;
        margin-top: 10px;
        margin-bottom: 5px;
        border-bottom: 1px solid #eee;
      }
      .shiny-chat-messages p {
        margin-bottom: 8px;
        line-height: 1.5;
      }
    ")
  ),
  
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  title = "StockAI - AI股票智能预测",
  
  # ==========================================================================
  # 侧边栏
  # ==========================================================================
  sidebar = bslib::sidebar(
    width = 300,
    
    # 数据源选择
    shiny::selectInput(
      inputId = "data_source",
      label = "数据源 (Data Source)",
      choices = DATA_SOURCES,
      selected = "yahoo"
    ),
    
    # 动态显示数据源提示
    shiny::uiOutput("data_source_help"),
    
    # 股票选择
    shinyWidgets::pickerInput(
      inputId = "ticker_preset",
      label = "常用股票选择",
      choices = PRESET_TICKERS,
      selected = "AAPL",
      options = list(`live-search` = TRUE)
    ),
    
    shiny::textInput(
      inputId = "ticker_custom",
      label = "手动输入股票代码",
      value = "AAPL"
    ),
    
    # 时间周期
    shinyWidgets::sliderTextInput(
      inputId = "period",
      label = "时间跨度 (Time Period)",
      choices = names(PERIOD_DAYS),
      selected = DEFAULT_PERIOD,
      grid = TRUE
    ),
    
    # K线周期
    shiny::selectInput(
      inputId = "interval",
      label = "K线周期 (Interval)",
      choices = INTERVAL_CHOICES,
      selected = "daily"
    ),
    
    # 图表类型
    shiny::selectInput(
      inputId = "plot_type",
      label = "图表类型 (Plot Type)",
      choices = PLOT_TYPE_CHOICES,
      selected = "candlesticks"
    ),
    
    shiny::hr(),
    shiny::h5("技术指标"),
    
    # 技术指标
    shiny::checkboxGroupInput(
      inputId = "indicators",
      label = NULL,
      choiceNames = indicator_choice_names,
      choiceValues = INDICATOR_CHOICES$values,
      selected = c("SMA")
    ),
    
    shiny::hr(),
    shiny::h5("AI 预测模型"),
    
    # AI 提供商选择
    shiny::selectInput(
      inputId = "ai_provider",
      label = "选择 AI 提供商",
      choices = AI_PROVIDER_CHOICES,
      selected = "minimax"
    ),
    
    # 动态渲染模型选择
    shiny::uiOutput("ai_model_ui"),
    
    # 显示当前模型状态
    htmltools::div(
      class = "mb-2",
      htmltools::span("当前使用: ", style = "font-size: 0.8rem; color: #666;"),
      shiny::uiOutput("selected_model_badge", inline = TRUE)
    ),
    
    # AI 参数控制
    shiny::hr(),
    shiny::h6("AI 参数设置", class = "mb-2"),
    shiny::sliderInput(
      inputId = "ai_temperature",
      label = "Temperature",
      min = 0, max = 1, value = 0.7, step = 0.1
    ),
    shiny::sliderInput(
      inputId = "ai_max_tokens",
      label = "Max Tokens",
      min = 256, max = 4096, value = 1024, step = 256
    ),
    shiny::checkboxInput("ai_enable_search", "启用联网搜索", value = FALSE),
    
    shiny::actionButton("run_ai", "运行 AI 联网预测", class = "btn-primary w-100"),
    
    shiny::hr(),
    shiny::checkboxInput("show_points", "标记极值点", TRUE),
    shiny::checkboxInput("show_prediction", "显示数据明细", TRUE)
  ),
  
  # ==========================================================================
  # 主界面内容
  # ==========================================================================
  htmltools::div(
    class = "d-flex flex-column",
    
    # 顶部统计卡片
    bslib::layout_column_wrap(
      width = 1/2,
      fill = FALSE,
      gap = "10px",
      style = "margin-bottom: 10px;",
      
      # 左边：关键统计指标与技术面
      bslib::value_box(
        title = "最新交易与技术指标",
        value = shiny::uiOutput("vbox_market_stats"),
        showcase = bsicons::bs_icon("bar-chart-fill"),
        theme = "primary"
      ),
      
      # 右边：多阶段收益率
      bslib::value_box(
        title = "收益率概览",
        value = shiny::uiOutput("vbox_performance"),
        showcase = bsicons::bs_icon("graph-up-arrow"),
        theme = "light"
      )
    ),
    
    # 股票图表
    bslib::card(
      full_screen = TRUE,
      style = "height: 70vh; min-height: 500px;",
      bslib::card_header("股票价格趋势分析 (quantmod)"),
      bslib::card_body(
        padding = 0,
        shiny::plotOutput(outputId = "plot", height = "100%")
      )
    ),
    
    # AI 研报
    bslib::card(
      style = "margin-top: 10px;",
      bslib::card_header("AI 深度研报 (含实时新闻与财务评估)"),
      bslib::card_body(shiny::uiOutput("ai_report_ui"))
    ),
    
    # 回测系统
    bslib::card(
      style = "margin-top: 10px;",
      bslib::card_header("交易策略回测系统 (Backtesting Suite)"),
      bslib::navset_tab(
        # Tab 1: 策略与资金配置
        bslib::nav_panel(
          title = "1. 策略与资金配置",
          htmltools::div(
            class = "p-3",
            bslib::layout_column_wrap(
              width = 1/4,
              
              # 资金管理
              htmltools::div(
                class = "bt-col-sep",
                htmltools::span(class = "bt-section-title", "[1] 资金管理"),
                shiny::numericInput("init_capital", "初始本金 (USD)", value = 10000, min = 100),
                shiny::helpText("账户起始本金额度")
              ),
              
              # 资金策略
              htmltools::div(
                class = "bt-col-sep",
                htmltools::span(class = "bt-section-title", "[2] 资金策略"),
                shiny::selectInput("entry_rule", "建仓规则", choices = ENTRY_RULE_CHOICES, selected = "fixed_pct"),
                shiny::numericInput("trade_size", "建仓比例 (%)", value = 20, min = 1, max = 100),
                shiny::hr(),
                shiny::selectInput("pyramid_rule", "加仓规则", choices = PYRAMID_RULE_CHOICES, selected = "none"),
                shiny::numericInput("pyramid_size", "加仓比例 (%)", value = 10, min = 0, max = 100)
              ),
              
              # 风险管理
              htmltools::div(
                class = "bt-col-sep",
                htmltools::span(class = "bt-section-title", "[3] 风险管理"),
                shiny::numericInput("stop_loss", "止损规则 (%)", value = 0, min = 0),
                shiny::numericInput("take_profit", "止盈规则 (%)", value = 0, min = 0),
                shiny::helpText("设置为 0 禁用规则")
              ),
              
              # 交易策略
              htmltools::div(
                htmltools::span(class = "bt-section-title", "[4] 交易策略"),
                shiny::selectInput("bt_strategy", "代码模板", choices = BACKTEST_STRATEGIES, selected = "supertrend"),
                shiny::textAreaInput("strategy_code", "信号逻辑代码", value = "", height = "165px", placeholder = "代码...")
              )
            ),
            
            # 样式
            htmltools::tags$style("
              .bt-col-sep { border-right: 1px solid #dee2e6; height: 100%; padding-right: 15px; }
              .bt-section-title { font-weight: bold; margin-bottom: 15px; color: #2c3e50; border-bottom: 2px solid #3498db; display: inline-block; }
              #strategy_code { font-family: 'Courier New', monospace !important; font-size: 12px !important; background: #fdf6e3 !important; }
            "),
            
            shiny::hr(),
            shiny::actionButton("run_backtest", "启动历史回测引擎", class = "btn-success w-100", icon = shiny::icon("rocket"))
          )
        ),
        
        # Tab 2: 交易记录明细
        bslib::nav_panel(
          title = "2. 交易记录明细",
          htmltools::div(class = "p-2", shiny::tableOutput("bt_trade_log"))
        ),
        
        # Tab 3: 绩效分析报告
        bslib::nav_panel(
          title = "3. 绩效分析报告",
          htmltools::div(
            class = "p-3",
            shiny::uiOutput("bt_performance_stats"),
            shiny::hr(),
            htmltools::div(
              style = "background: #fcfcfc; border-radius: 8px; padding: 15px;",
              htmltools::h6(bsicons::bs_icon("graph-up"), " 净值走势与回撤分析"),
              shiny::plotOutput("bt_equity_plot", height = "450px")
            )
          )
        ),
        
        # Tab 4: AI 金融助手
        bslib::nav_panel(
          title = "4. AI 金融助手",
          htmltools::div(
            class = "p-3",
            htmltools::div(
              class = "row g-0",
              
              # 左侧：聊天消息区域
              htmltools::div(
                class = "col-md-10",
                shinychat::chat_ui("chat", placeholder = "输入您的问题...", height = "500px"),
                
                # 操作按钮
                htmltools::div(
                  class = "mt-2 d-flex justify-content-between align-items-center",
                  shiny::actionButton("clear_chat", "清空对话", class = "btn-outline-secondary btn-sm", icon = shiny::icon("trash")),
                  shiny::uiOutput("chat_status")
                )
              ),
              
              # 右侧：快捷操作
              htmltools::div(
                class = "col-md-2 ps-3",
                htmltools::div(
                  class = "card border-0 shadow-sm h-100",
                  htmltools::div(
                    class = "card-header bg-white border-0",
                    htmltools::h6(class = "mb-0", bsicons::bs_icon("lightbulb"), " 快捷问题")
                  ),
                  htmltools::div(
                    class = "card-body p-2",
                    htmltools::div(
                      class = "d-grid gap-2",
                      shiny::actionButton("quick_q1", "分析技术面", class = "btn-outline-primary btn-sm text-start"),
                      shiny::actionButton("quick_q2", "解读财务", class = "btn-outline-primary btn-sm text-start"),
                      shiny::actionButton("quick_q3", "预测走势", class = "btn-outline-primary btn-sm text-start"),
                      shiny::actionButton("quick_q4", "买卖建议", class = "btn-outline-primary btn-sm text-start"),
                      shiny::actionButton("quick_q5", "重大新闻", class = "btn-outline-primary btn-sm text-start"),
                      shiny::actionButton("quick_q6", "指标解释", class = "btn-outline-primary btn-sm text-start")
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
