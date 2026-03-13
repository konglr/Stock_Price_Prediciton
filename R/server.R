
# StockAI - Server 模块
# ==============================================================================
# 本文件包含 Shiny Server 的完整逻辑
# ==============================================================================

# ------------------------------------------------------------------------------
# Server 函数定义
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # 设置 HTTP User Agent
  options(HTTPUserAgent = "Mozilla/5.0")
  
  # ==========================================================================
  # Reactive Values
  # ==========================================================================
  
  ai_prediction <- reactiveVal(NULL)
  ai_grounding <- reactiveVal(NULL)
  ai_loading <- reactiveVal(FALSE)
  
  chat_history <- reactiveVal(list())
  chat_loading <- reactiveVal(FALSE)
  pending_ai_request <- reactiveVal(NULL)
  
  bt_results <- reactiveVal(NULL)
  
  # ==========================================================================
  # 数据获取与处理
  # ==========================================================================
  
  # 监听预设股票选择
  observeEvent(input$ticker_preset, {
    updateTextInput(session, "ticker_custom", value = input$ticker_preset)
  })
  
  # 当前股票代码
  current_ticker <- reactive({
    req(input$ticker_custom)
    toupper(input$ticker_custom)
  })
  
  # 获取原始数据
  ticker_data <- reactive({
    ticker <- current_ticker()
    fetch_ticker_data(ticker)
  })
  
  # 处理后的数据 (周期转换)
  processed_ticker_data <- reactive({
    data <- ticker_data()
    if (is.null(data)) return(NULL)
    process_ticker_data(data, input$interval)
  })
  
  # 时间跨度 (天数)
  period_days <- reactive({
    PERIOD_DAYS[input$period] %||% 365
  })
  
  # ==========================================================================
  # AI 模型选择 UI
  # ==========================================================================
  
  output$ai_model_ui <- renderUI({
    provider <- input$ai_provider
    choices <- AI_MODELS[[provider]] %||% AI_MODELS$gemini
    selectInput("ai_model", "选择模型", choices = choices)
  })
  
  output$selected_model_badge <- renderUI({
      model <- input$ai_model %||% "gemini-3.1-flash-lite-preview"
    friendly_name <- get_model_friendly_name(model)
    span(class = "badge bg-info", friendly_name)
  })
  
  # ==========================================================================
  # 市场统计卡片
  # ==========================================================================
  
  output$vbox_market_stats <- renderUI({
    data <- processed_ticker_data()
    if (is.null(data) || nrow(data) < 2) return("等待数据加载...")
    
    stats <- get_stock_stats(data)
    render_market_stats_ui(stats)
  })
  
  output$vbox_performance <- renderUI({
    data <- ticker_data()  # 使用原始日线数据
    if (is.null(data) || nrow(data) < 252) return("数据不足 (需1年以上历史)")
    
    perf <- get_performance_stats(data)
    render_performance_ui(perf)
  })
  
  # ==========================================================================
  # AI 研报
  # ==========================================================================
  
  observeEvent(input$run_ai, {
    req(processed_ticker_data())
    ai_loading(TRUE)
    ai_prediction(NULL)
    ai_grounding(NULL)
    
    ticker <- current_ticker()
    data <- processed_ticker_data()
    stats <- get_stock_stats(data)
    provider <- input$ai_provider
    model_id <- input$ai_model %||% "gemini-3.1-flash-lite-preview"
    temperature <- input$ai_temperature %||% 0.7
    max_tokens <- input$ai_max_tokens %||% 2048
    
    tryCatch({
      result <- run_ai_report(
        ticker        = ticker,
        data          = data,
        stats         = stats,
        search_result = NULL,  # 可以后续添加联网搜索结果
        provider      = provider,
        model_id      = input$ai_model %||% "gemini-3.1-flash-lite-preview",
        temperature   = temperature,
        max_tokens    = max_tokens,
        enable_search = input$ai_enable_search
      )
      
      ai_prediction(result$prediction)
      ai_grounding(result$grounding)
      
    }, error = function(e) {
      ai_prediction(list(error = paste("AI 联网分析失败:", e$message)))
    })
    
    ai_loading(FALSE)
  })
  
  output$ai_report_ui <- renderUI({
    render_ai_report_ui(ai_prediction(), ai_loading())
  })
  
  # ==========================================================================
  # 股票图表
  # ==========================================================================
  
  output$plot <- renderPlot({
    data <- processed_ticker_data()
    
    if (is.null(data) || nrow(data) < 2) return(NULL)
    
    days_to_show <- period_days()
    visible_data <- tail(data, days_to_show)
    subset_range <- paste0(start(visible_data), "::")
    
    # 创建基础图表
    cs <- chart_Series(data, name = current_ticker(), subset = subset_range, 
                       type = input$plot_type, theme = chart_theme())
    cs <- add_Vo()
    
    # 添加技术指标
    cs <- add_indicators(cs, data, input$indicators)
    
    print(cs)
  })
  
  # ==========================================================================
  # 回测系统
  # ==========================================================================
  
  # 加载策略模板
  observeEvent(input$bt_strategy, {
    if (input$bt_strategy == "custom") return()
    code <- load_strategy_template(input$bt_strategy)
    updateTextAreaInput(session, "strategy_code", value = code)
  })
  
  # 运行回测
  observeEvent(input$run_backtest, {
    data <- processed_ticker_data()
    req(data)
    
    showNotification("正在运行自定义策略回测...", type = "message")
    
    tryCatch({
      sl_val <- if (is.na(input$stop_loss)) 0 else input$stop_loss
      tp_val <- if (is.na(input$take_profit)) 0 else input$take_profit
      
      res <- run_backtest(
        data            = data,
        strategy_code   = input$strategy_code,
        init_capital    = input$init_capital,
        trade_pct       = input$trade_size,
        stop_loss_pct   = sl_val,
        take_profit_pct = tp_val
      )
      
      if (!is.null(res)) bt_results(res)
      
    }, error = function(e) {
      showNotification(paste("策略执行错误:", e$message), type = "error")
    })
  })
  
  # 回测结果输出
  output$bt_trade_log <- renderTable({
    res <- bt_results()
    render_backtest_trade_log(res)
  }, striped = TRUE, hover = TRUE, align = "c", sanitize.text.function = function(x) x)
  
  output$bt_performance_stats <- renderUI({
    render_backtest_stats_ui(bt_results())
  })
  
  output$bt_equity_plot <- renderPlot({
    res <- bt_results()
    if (is.null(res)) return(NULL)
    
    plot(res$equity, type = "l", col = "#2ecc71", lwd = 2, 
         main = "账户权益曲线", ylab = "Equity (USD)")
    abline(h = res$equity[1], col = "red", lty = 2)
  })
  
  # ==========================================================================
  # AI 金融助手
  # ==========================================================================
  
  # 切换股票的辅助函数
  switch_ticker <- function(ticker) {
    if (is.null(ticker) || ticker == "") return()
    updateTextInput(session, "ticker_custom", value = ticker)
    
    if (!is.null(input$ticker_preset)) {
      if (ticker %in% PRESET_TICKERS) {
        updatePickerInput(session, "ticker_preset", selected = ticker)
      }
    }
    showNotification(paste("已切换到股票:", ticker), type = "message")
  }
  
  # 运行 AI 聊天分析
  run_ai_chat_analysis <- function(user_input, history_to_use, question_info) {
    req(user_input, history_to_use, question_info)
    chat_loading(TRUE)
    
    tryCatch({
      data <- ticker_data()
      ticker <- current_ticker()
      
      result <- handle_chat_message(
        user_input    = user_input,
        current_ticker = ticker,
        current_data  = data,
        provider      = input$ai_provider,
        model_id      = input$ai_model %||% "gemini-3.1-flash-lite-preview",
        temperature   = input$ai_temperature %||% 0.7,
        max_tokens    = input$ai_max_tokens %||% 1024,
        enable_search = input$ai_enable_search
      )
      
      # 更新聊天历史
      updated_history <- history_to_use
      updated_history[[length(updated_history) + 1]] <- list(role = "assistant", content = result$response)
      chat_history(updated_history)
      
      # 追加到聊天 UI
      chat_append("chat", response = result$response, role = "assistant")
      
    }, error = function(e) {
      error_msg <- paste("抱歉，处理您的请求时出现错误：", e$message)
      chat_append("chat", response = error_msg, role = "assistant")
    })
    
    chat_loading(FALSE)
  }
  
  # 监听股票数据加载完成后处理待处理请求
  observeEvent(ticker_data(), {
    req(pending_ai_request())
    request_details <- pending_ai_request()
    pending_ai_request(NULL)
    
    invalidateLater(250, session)
    
    isolate({
      run_ai_chat_analysis(
        user_input     = request_details$question,
        history_to_use = request_details$history,
        question_info  = request_details$q_info
      )
    })
  }, ignoreInit = TRUE, priority = -10)
  
  # 处理用户消息
  handle_user_message <- function(user_input) {
    req(trimws(user_input) != "")
    
    current_h <- chat_history()
    current_h[[length(current_h) + 1]] <- list(role = "user", content = user_input)
    chat_history(current_h)
    
    showNotification("AI 正在分析问题...", type = "message")
    
    q_info <- analyze_user_question(
      user_question  = user_input,
      current_ticker = isolate(current_ticker()),
      provider       = input$ai_provider,
      model_id       = input$ai_model %||% "gemini-3.1-flash-lite-preview"
    )
    
    is_switching <- q_info$need_data && q_info$switch_stock && 
                    q_info$ticker != isolate(current_ticker())
    
    if (is_switching) {
      chat_append("chat", response = paste0("\U0001F504 识别到新股票：**", q_info$ticker, "**，正在加载数据..."), role = "assistant")
      pending_ai_request(list(question = user_input, history = current_h, q_info = q_info))
      switch_ticker(q_info$ticker)
    } else {
      if (q_info$need_data) {
        chat_append("chat", response = "\U0001F50D 正在分析当前股票数据...", role = "assistant")
      }
      run_ai_chat_analysis(user_input = user_input, history_to_use = current_h, question_info = q_info)
    }
  }
  
  # 监听聊天输入
  observeEvent(input$chat_user_input, {
    handle_user_message(input$chat_user_input)
  })
  
  # 快捷问题点击事件
  observeEvent(input$quick_q1, { 
    chat_append("chat", response = "\U0001F4CA 分析当前技术面", role = "user")
    handle_user_message("帮我分析一下当前股票的技术面走势")
  })
  observeEvent(input$quick_q2, { 
    chat_append("chat", response = "\U0001F4B0 解读财务指标", role = "user")
    handle_user_message("请解读一下该股票最新的财务报表和关键指标")
  })
  observeEvent(input$quick_q3, { 
    chat_append("chat", response = "\U0001F4C8 预测走势", role = "user")
    handle_user_message("基于当前数据，预测一下未来一周的走势")
  })
  observeEvent(input$quick_q4, { 
    chat_append("chat", response = "\U0001F3AF 买卖建议", role = "user")
    handle_user_message("给出现在该股票的买卖策略建议")
  })
  observeEvent(input$quick_q5, { 
    chat_append("chat", response = "\U0001F4F0 重大新闻", role = "user")
    handle_user_message("该股票最近有什么重大的新闻或公告？")
  })
  observeEvent(input$quick_q6, { 
    chat_append("chat", response = "\u2753 指标解释", role = "user")
    handle_user_message("请解释一下什么是 RSI 指标，在当前股票中如何应用？")
  })
  
  # 初始化欢迎消息
  session$onFlushed(function() {
    chat_append("chat", response = "您好！我是您的 AI 金融助手。您可以询问我关于股票的技术分析、财务解读或走势预测。", role = "assistant")
  }, once = TRUE)
  
  # 聊天状态显示
  output$chat_status <- renderUI({
    if (chat_loading()) {
      span(class = "badge bg-warning", "AI 正在分析中...")
    } else {
      model <- input$ai_model %||% "gemini-3.1-flash-lite-preview"
      span(class = "badge bg-success", paste("已连接", model))
    }
  })
  
  # 清空聊天
  observeEvent(input$clear_chat, {
    chat_history(list())
    chat_clear("chat")
    chat_append("chat", response = "对话已清空。您可以开始新的咨询。", role = "assistant")
    showNotification("对话已清空", type = "message")
  })
  
}