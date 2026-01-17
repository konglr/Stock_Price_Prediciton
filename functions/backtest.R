# --- 基础交易回测引擎 ---
run_simple_backtest <- function(data, strategy_code, init_capital, trade_pct, stop_loss_pct, take_profit_pct) {
  # 准备数据
  n <- nrow(data)
  prices <- Cl(data)
  lows <- Lo(data)
  
  # 初始化信号向量
  signals <- rep(0, n) # 1: Buy, -1: Sell
  
  # --- 动态执行用户编写的策略逻辑 ---
  # 环境中已具备 data, prices, signals, n
  tryCatch({
    eval(parse(text = strategy_code))
  }, error = function(e) {
    stop(paste("策略代码运行错误:", e$message))
  })
  
  # 执行回测逻辑
  cash <- init_capital
  position <- 0
  trade_log <- data.frame()
  equity_curve <- rep(init_capital, n)
  
  # 临时追踪变量
  entry_price <- 0
  entry_date <- NULL
  peak_price <- 0
  max_drawdown_trade <- 0
  
  for (i in 1:n) {
    curr_price <- as.numeric(prices[i])
    curr_low <- as.numeric(lows[i])
    curr_hi <- as.numeric(Hi(data)[i])
    curr_date <- index(prices)[i]
    
    # 持仓管理
    if (position > 0) {
      peak_price <- max(peak_price, curr_price)
      dd = (curr_low - peak_price) / peak_price * 100
      max_drawdown_trade <- min(max_drawdown_trade, dd)
      
      # 初始设置
      exit_triggered <- FALSE
      sell_reason <- ""
      exit_price <- curr_price # 默认按收盘价/当前价
      
      # 1. 优先判定止损 (如果设置了 > 0)
      if (stop_loss_pct > 0 && ((curr_low - entry_price) / entry_price * 100 <= -stop_loss_pct)) {
        exit_triggered <- TRUE
        sell_reason <- "STOP LOSS"
        exit_price <- entry_price * (1 - stop_loss_pct / 100)
      } 
      # 2. 其次判定止盈 (如果设置了 > 0)
      else if (take_profit_pct > 0 && ((curr_hi - entry_price) / entry_price * 100 >= take_profit_pct)) {
        exit_triggered <- TRUE
        sell_reason <- "TAKE PROFIT"
        exit_price <- entry_price * (1 + take_profit_pct / 100)
      }
      # 3. 最后判定策略信号
      else if (signals[i] == -1) {
        exit_triggered <- TRUE
        sell_reason <- "SIGNAL"
        exit_price <- curr_price
      }
      
      if (exit_triggered) {
        pnl <- (exit_price - entry_price) * position
        pnl_pct <- (exit_price / entry_price - 1) * 100
        hold_days <- as.numeric(difftime(curr_date, entry_date, units = "days"))
        max_pnl_trade <- (peak_price / entry_price - 1) * 100
        
        cash <- cash + position * exit_price
        trade_log <- rbind(trade_log, data.frame(
          Date = curr_date, Action = "SELL", Price = exit_price, 
          Shares = position, Total = cash, Reason = sell_reason,
          PnL = pnl, PnL_Pct = pnl_pct, MaxDD_Trade = max_drawdown_trade, 
          MaxProfit_Trade = max_pnl_trade, HoldDays = hold_days
        ))
        position <- 0
      }
    }
    
    # 信号触发 (买入)
    if (signals[i] == 1 && position == 0) {
      entry_price <- curr_price
      entry_date <- curr_date
      peak_price <- curr_price
      max_drawdown_trade <- 0
      
      buy_amount <- cash * (trade_pct / 100)
      shares <- buy_amount / curr_price
      cash <- cash - buy_amount
      position <- shares
      trade_log <- rbind(trade_log, data.frame(
        Date = curr_date, Action = "BUY", Price = curr_price, 
        Shares = shares, Total = cash, Reason = "SIGNAL",
        PnL = NA, PnL_Pct = NA, MaxDD_Trade = NA, MaxProfit_Trade = NA, HoldDays = NA
      ))
    }
    
    equity_curve[i] <- cash + position * curr_price
  }
  
  return(list(log = trade_log, equity = equity_curve, signals = signals))
}
