# 导入所需的包
library(quantmod)
library(xts)

# 下载ticker股票价格数据

ticker <-getSymbols("002077.SZ", auto.assign = TRUE)
ticker <- na.omit(adjustOHLC(ticker_data$GOOGL,use.Adjusted = TRUE))
# 计算5日和50日的简单移动平均线（SMA）
sma5 <- SMA(Cl(ticker), n = 5)
sma10 <- SMA(Cl(ticker), n = 10)
sma20 <- SMA(Cl(ticker), n = 20)
# check the ticker data 
ticker_return <- Delt(Cl(ticker))
over_threshold_days <- Cl(ticker) > sma5 * 1.1
ticker[over_threshold_days,1:4]
ticker_return[over_threshold_days]
length(over_threshold_days)
Return.calculate(to.yearly(ticker), method = "discrete")
chartSeries(Return.calculate(to.weekly(ticker), method = "discrete")[,6], theme = "white")
chart.Histogram(Return.calculate(to.weekly(ticker)[,4]))

#Base Data
portfolio <- xts::xts(order.by = index(ticker), 
                      data.frame(PortfolioValue = rep(0, nrow(ticker)),
                                 SharesHold = rep(0, nrow(ticker)),
                                 Margin = rep(0, nrow(ticker)), 
                                 BuyOrder=rep(0, nrow(ticker)),
                                 SellOrder=rep(0, nrow(ticker)),
                                 StopLoss=rep(0, nrow(ticker)),
                                 Profit=rep(0, nrow(ticker)),
                                 OrderStatus=rep(0, nrow(ticker)))), # 1 is Open, 0 is Close
orders <- xts::xts(order.by = index(ticker), 
                      data.frame(OrderType = rep(0, nrow(ticker)), #  1 is buy, 0 is sell
                                 OpenDate = rep(0, nrow(ticker)),
                                 CloseDae = rep(0, nrow(ticker)), 
                                 OpenPrice=rep(0, nrow(ticker)),
                                 ClosePrice=rep(0, nrow(ticker)),
                                 OpenType=rep(0, nrow(ticker)),  # Strategy 1 , Strategy 2
                                 CloseType=rep(0, nrow(ticker)),  # 1 is normal, 2 Stop Loss
                                 Profit=rep(0, nrow(ticker)),   # Closed order profits
                                 OrderStatus=rep(0, nrow(ticker)))), # 1 is Open, 0 is Close
trade_back_days <- 700 #回测天数
cash_invest <-300000 # 初始资金
cash <- cash_invest  
shares_to_buy <- 100
shares <- 0    # 持有股票数量
max_shares_hold <-100 # Max shares to hold
orders_to_buy <- 0  #Date and share
orders_to_sell<- 0  #Date and share
stop_loss_triggered<-0 
stop_loss_percent <- 0.0
sell_margin_threshold <- 1
sell_margin_threshold_H <- 1.3
sell_margin_threshold_L <- 1.0
price_increase_threshold <- 0.03
price_increase_threshold_H <- 0.02 # High Frequency
price_increase_threshold_L <- 0.03 # Low Frequency
below_sma5 <- TRUE
open_portfolio_value <- 0  # 在持股票价值

# 初始化买入和卖出信号的变量
buy_signal <- rep(0, nrow(ticker))
sell_signal <- rep(0, nrow(ticker))

# 根据策略生成买入和卖出信号
for (i in 22:nrow(ticker)) {
  
  # sma修正买入策略 
  if(sma5[i] > sma10[i] & sma5[i] > sma20[i]){
    price_increase_threshold <- price_increase_threshold_H
  }else
  {price_increase_threshold <- price_increase_threshold_L}
  
  if (Cl(ticker[i]) > sma5[i] 
      & sma5[i] > sma10[i]
      & as.numeric(Cl(ticker[i])) > (as.numeric(Cl(ticker[i-1])) * (1 + price_increase_threshold))) {
    # Closing price higher than SMA 5 and last trading day close price
     buy_signal[i] <- 1
  }
  
  if (Delt(Cl(ticker[i-1]), Cl(ticker[i])) < 0.08) {
    sell_signal[i] <- 1
  }
}

# Backtest Trading

for (i in (nrow(ticker)-trade_back_days):nrow(ticker)) {
 
  # sma & Value修正卖出策略
  if(sma5[i] > sma10[i]){
    sell_margin_threshold <- sell_margin_threshold_H 
  }else
  {sell_margin_threshold <- sell_margin_threshold_L}
  
  if (buy_signal[i] == 1) {
    # First share buy
    if (shares == 0 & as.numeric(sma5[i-1]) < as.numeric(sma10[i-1])) { 
      shares <- shares + shares_to_buy
      open_portfolio_value <- shares_to_buy * as.numeric(Cl(ticker[i]))
      cash <- cash - open_portfolio_value
      orders_to_buy <-  orders_to_buy + 1
      portfolio[i, "BuyOrder"] <- shares
    } 
    else {
      # Add on more shares
      if (shares < max_shares_hold) {
        shares <- shares + shares_to_buy
        open_portfolio_value <- open_portfolio_value + shares_to_buy * as.numeric(Cl(ticker[i]))
        cash <- cash - shares_to_buy * as.numeric(Cl(ticker[i]))
        orders_to_buy <-  orders_to_buy + 1
        portfolio[i, "BuyOrder"] <- shares
      }
    }
  }
  # Execute Margin Sell Signal
  if ((shares * as.numeric(Cl(ticker[i]))) > (open_portfolio_value * sell_margin_threshold) 
      & sell_signal[i] == 1 & !shares == 0) {
    cash_to_receive <- shares * as.numeric(Cl(ticker[i]))
    cash <- cash + cash_to_receive
    portfolio[i, "SellOrder"] <- shares
    portfolio[i, "Profit"] <- cash_to_receive - open_portfolio_value
    shares <- 0
    shares_buy_cost <- 0
    orders_to_sell <- orders_to_sell + 1
    open_portfolio_value <- 0
   
  }
  
  # Stop loss triggered
  if (open_portfolio_value > 0 && shares * as.numeric(Cl(ticker[i])) < stop_loss_percent * open_portfolio_value) {
    cash_to_receive <- shares * as.numeric(Cl(ticker[i]))
    cash <- cash + cash_to_receive
    portfolio[i, "StopLoss"] <- shares
    portfolio[i, "Profit"] <- cash_to_receive - open_portfolio_value
    shares <- 0
    shares_buy_cost <-0
    open_portfolio_value <- 0
    stop_loss_triggered<- stop_loss_triggered + 1
  }
  
  #check the cash status
  if (cash < 0) {
    print("cash is less than 0")
    break  # Stop the loop if cash is below 0
  }
  
  # Record every day trading
  portfolio[i, "PortfolioValue"] <- cash + shares * as.numeric(Cl(ticker[i]))
  portfolio[i, "SharesHold"] <- shares
  portfolio[i, "Margin"] <- shares * as.numeric(Cl(ticker[i]))-open_portfolio_value
}

tail(portfolio, n = 1)
cash
shares
orders_to_buy
orders_to_sell
stop_loss_triggered
paste("Return is", tail(portfolio$PortfolioValue, n=1) - cash_invest, 
      "Return Rate",
      ((tail(portfolio$PortfolioValue, n=1)-cash_invest)/cash_invest) * 100, "%")


portfolio_df <- data.frame(Date = index(ticker), PortfolioValue = portfolio)

# 绘制回测结果
chartSeries(ticker[(nrow(portfolio)-trade_back_days):nrow(portfolio),1:4], theme = 'white',
            main = "Backtest Results", xlab = "Date", ylab = "Value")
addSMA(n=5, on=1)
addSMA(n=10, on=1, col = "orange")
addSMA(n=20, on=1, col = "purple")
addTA(portfolio$PortfolioValue, legend = "Portfolio Value", type = "l",col = "darkgreen")
addTA(portfolio$SharesHold, legend = "Shares", col = "purple")
addTA(portfolio$StopLoss[portfolio$StopLoss != 0], on = 3, legend = "Stop Loss", type = "p", pch = 1, col = "red", cex = 1)
addTA(portfolio$StopLoss[portfolio$SellOrder != 0], on = 3, legend = "Sell Order",type = "p", pch = 1, col = "orange", cex = 1)
addTA(portfolio$Margin,legend = "Margin", col = "blue")

