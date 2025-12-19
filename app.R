# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# 加载必要的库
library(shiny)
library(shinyWidgets)
library(tidyquant)
library(plotly)
library(quantmod)
library(bslib)
library(httr2)
library(jsonlite)

# --- UI 部分 ---
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  title = "StockAI - 智能股票预测系统",
  
  sidebar = sidebar(
    width = 300,
    pickerInput(
      inputId = "ticker",
      label = "选择股票代码 (Ticker)",
      choices = c("AAPL", "AMZN", "GOOGL", "MSFT", "TSLA", "NVDA", "SQQQ", ".IXIC", "000001.SZ"),
      selected = "AAPL",
      options = list(`live-search` = TRUE)
    ),
    
    radioGroupButtons(
      inputId = "period",
      label = "时间跨度",
      choices = c("20天" = 20, "1个月" = 30, "3个月" = 90, "半年" = 180, "1年" = 365),
      selected = 30,
      justified = TRUE
    ),
    
    selectInput(
      inputId = "plot_type",
      label = "图表类型",
      choices = c("折线图" = "line", "蜡烛图" = "candlesticks"),
      selected = "candlesticks"
    ),
    
    hr(),
    h5("AI 预测设置"),
    checkboxInput("do_ai", "启用 Gemini AI 深度分析", TRUE),
    actionButton("predict_btn", "运行 AI 预测", class = "btn-primary w-100"),
    
    hr(),
    numericInput("table_rows", "显示历史数据行数:", 5, min = 1, max = 50)
  ),
  
  # 主界面布局
  layout_column_wrap(
    width = 1,
    # 顶部统计卡片
    layout_column_wrap(
      width = 1/3,
      value_box(
        title = "最新收盘价",
        value = uiOutput("last_price"),
        showcase = icon("dollar-sign")
      ),
      value_box(
        title = "当日涨跌幅",
        value = uiOutput("price_change"),
        showcase = icon("chart-line")
      ),
      value_box(
        title = "AI 预测情绪",
        value = uiOutput("ai_sentiment"),
        showcase = icon("robot")
      )
    ),
    
    # 图表展示
    card(
      full_screen = TRUE,
      card_header("股价走势与指标 (SMA 5/10/20)"),
      plotlyOutput("stock_plot")
    ),
    
    # AI 分析结论与表格
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Gemini AI 预测分析报告"),
        uiOutput("ai_report")
      ),
      card(
        card_header("近期历史数据"),
        tableOutput("data_table")
      )
    )
  )
)

# --- Server 部分 ---
server <- function(input, output, session) {
  
  # 响应式获取数据
  stock_data <- reactive({
    req(input$ticker)
    tryCatch({
      df <- getSymbols(input$ticker, from = Sys.Date() - 500, to = Sys.Date(), auto.assign = FALSE)
      # 转换为 Data Frame 方便处理
      df_final <- data.frame(Date = index(df), coredata(df))
      colnames(df_final) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
      df_final
    }, error = function(e) return(NULL))
  })
  
  # 计算指标
  processed_data <- reactive({
    df <- stock_data()
    if(is.null(df)) return(NULL)
    
    df$SMA5 <- SMA(df$Close, n = 5)
    df$SMA10 <- SMA(df$Close, n = 10)
    df$SMA20 <- SMA(df$Close, n = 20)
    
    tail(df, as.numeric(input$period))
  })
  
  # 渲染 Plotly 图表
  output$stock_plot <- renderPlotly({
    df <- processed_data()
    req(df)
    
    p <- plot_ly(df, x = ~Date)
    
    if(input$plot_type == "candlesticks") {
      p <- p %>% add_ohlc(open = ~Open, high = ~High, low = ~Low, close = ~Close, name = "价格")
    } else {
      p <- p %>% add_lines(y = ~Close, name = "收盘价", line = list(color = '#2563eb'))
    }
    
    p %>% 
      add_lines(y = ~SMA5, name = "SMA 5", line = list(color = 'brown', width = 1)) %>%
      add_lines(y = ~SMA10, name = "SMA 10", line = list(color = 'purple', width = 1)) %>%
      add_lines(y = ~SMA20, name = "SMA 20", line = list(color = 'orange', width = 1)) %>%
      layout(
        xaxis = list(title = "", rangeslider = list(visible = FALSE)),
        yaxis = list(title = "Price (USD/CNY)"),
        legend = list(orientation = 'h', x = 0.5, y = 1.1, xanchor = 'center'),
        margin = list(t = 50)
      )
  })
  
  # 顶部卡片内容
  output$last_price <- renderUI({
    df <- stock_data()
    req(df)
    val <- tail(df$Close, 1)
    span(paste0("$", round(val, 2)), style = "font-weight: bold;")
  })
  
  output$price_change <- renderUI({
    df <- stock_data()
    req(df)
    prices <- tail(df$Close, 2)
    change <- (prices[2] - prices[1]) / prices[1] * 100
    color <- if(change >= 0) "text-success" else "text-danger"
    span(class = color, sprintf("%+.2f%%", change))
  })
  
  # --- Gemini AI 预测逻辑 ---
  ai_res <- eventReactive(input$predict_btn, {
    df <- tail(stock_data(), 15)
    data_str <- paste(apply(df, 1, function(x) paste0(x['Date'], ": ", x['Close'])), collapse = "; ")
    
    prompt <- paste0("分析以下股票数据并预测未来5天趋势: ", input$ticker, ". 数据: ", data_str, 
                     ". 请以JSON格式返回: {'sentiment': '看涨/看跌/中性', 'target': '预测价', 'reason': '一句话理由'}")
    
    # API 调用逻辑 (符合系统提供的 Gemini API 规范)
    tryCatch({
      req <- request("https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash-preview-09-2025:generateContent") %>%
        req_url_query(key = "") %>% # 环境变量会自动处理 Key
        req_body_json(list(
          contents = list(list(parts = list(list(text = prompt)))),
          generationConfig = list(responseMimeType = "application/json")
        )) %>%
        req_perform()
      
      res <- resp_body_json(req)
      fromJSON(res$candidates[[1]]$content$parts[[1]]$text)
    }, error = function(e) {
      list(sentiment = "未知", target = "N/A", reason = "API 连接失败或数据不足")
    })
  })
  
  output$ai_sentiment <- renderUI({
    res <- ai_res()
    color <- switch(res$sentiment, "看涨" = "green", "看跌" = "red", "gray")
    span(res$sentiment, style = paste0("color: ", color, "; font-weight: bold;"))
  })
  
  output$ai_report <- renderUI({
    res <- ai_res()
    tagList(
      h4(paste("目标价格:", res$target)),
      p(class = "text-muted", res$reason),
      tags$small("注: AI 预测仅供参考，不构成投资建议。")
    )
  })
  
  # 数据表格
  output$data_table <- renderTable({
    df <- stock_data()
    req(df)
    tail(df[, c("Date", "Open", "High", "Low", "Close", "Volume")], input$table_rows)
  }, striped = TRUE, hover = TRUE, width = "100%")
}

# 启动应用
shinyApp(ui = ui, server = server)