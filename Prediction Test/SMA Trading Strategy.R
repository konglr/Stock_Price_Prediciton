# 导入所需的包
library(quantmod)
library(xts)

# 下载ticker股票价格数据

ticker <-getSymbols("000001.SZ", auto.assign = TRUE)
ticker <- na.omit(adjustOHLC(`000001.SZ`,use.Adjusted = TRUE))
# 计算5日和50日的简单移动平均线（SMA）
sma5 <- SMA(Cl(ticker), n = 5)
sma10 <- SMA(Cl(ticker), n = 10)

# check the ticker data 
ticker_return <- Delt(Cl(ticker))
over_threshold_days <- Cl(ticker) > sma5 * 1.05
ticker[over_threshold_days,1:4]
ticker_return[over_threshold_days]
length(over_threshold_days)
Return.calculate(to.yearly(ticker), method = "discrete")
chartSeries(Return.calculate(to.monthly(ticker), method = "discrete")[,6], theme = "white")

#Base Data
portfolio <- xts::xts(order.by = index(ticker), 
                      data.frame(PortfolioValue = rep(0, nrow(ticker)),
                                 SharesHold = rep(0, nrow(ticker))))

cash_invest <-2000000 # 初始资金
cash <- cash_invest  
shares_to_buy <- 100
shares <- 0    # 持有股票数量
orders_to_buy <- 0  #Date and share
orders_to_sell<- 0  #Date and share
stop_loss_triggered<-0 
stop_loss_percent <- 0.8
sell_sma_threshold <- 1.05
price_increase_threshold <- 0.02
below_sma5 <- FALSE
open_portfolio_value <- 0  # 初始股票价值

# 初始化买入和卖出信号的变量
buy_signal <- rep(0, nrow(ticker))
sell_signal <- rep(0, nrow(ticker))

# 根据策略生成买入和卖出信号
for (i in 11:nrow(ticker)) {
  if (Cl(ticker[i]) > sma5[i] 
      & sma5[i] > sma10[i]
      & as.numeric(Cl(ticker[i])) > (as.numeric(Cl(ticker[i-1])) * (1 + price_increase_threshold))) {
    # Closing price higher than SMA 5 and last trading day close price
    buy_signal[i] <- 1
  }
  
  if (Hi(ticker[i]) > sma5[i]*sell_sma_threshold & Delt(Cl(ticker[i-1]), Cl(ticker[i])) < 0.08) {
    sell_signal[i] <- 1
  }
}


# Backtest Trading

for (i in (nrow(ticker)-1000):nrow(ticker)) {
  
  if (Cl(ticker[i]) < sma5[i]) {
    below_sma5 <- FALSE
  }
  
  if (buy_signal[i] == 1) {
      # First share buy
     if (shares == 0) { 
      shares <- shares + shares_to_buy
      open_portfolio_value <- shares_to_buy * as.numeric(Cl(ticker[i]))
      cash <- cash - open_portfolio_value
      orders_to_buy <-  orders_to_buy + 1
    } 
    else {
      # Add on more shares
      if (as.numeric(Cl(ticker[i])) > (1 + price_increase_threshold) * as.numeric(Cl(ticker[i - 1]))) {
        shares <- shares + shares_to_buy
        open_portfolio_value <- open_portfolio_value + shares_to_buy * as.numeric(Cl(ticker[i]))
        cash <- cash - shares_to_buy * as.numeric(Cl(ticker[i]))
        orders_to_buy <-  orders_to_buy + 1
      }
    }
  }
    
  if (sell_signal[i] == 1 & !shares==0 ) {
    cash_to_receive <- shares * as.numeric(Cl(ticker[i]))
    cash <- cash + cash_to_receive
    shares <- 0
    orders_to_sell <-  orders_to_sell + 1
    open_portfolio_value <- 0
  }
     # Stop loss triggered
  if (open_portfolio_value > 0 && shares * as.numeric(Cl(ticker[i])) < stop_loss_percent * open_portfolio_value) {
    cash_to_receive <- shares * as.numeric(Cl(ticker[i]))
    cash <- cash + cash_to_receive
    shares <- 0
    orders_to_sell <- orders_to_sell + 1
    open_portfolio_value <- 0
    stop_loss_triggered<- stop_loss_triggered + 1
  }
  
  if (cash < 0) {
    print("cash is less than 0")
    break  # Stop the loop if cash is below 0
  }
  
  portfolio[i, "PortfolioValue"] <- cash + shares * as.numeric(Cl(ticker[i]))
  portfolio[i, "SharesHold"] <- shares
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
chartSeries(portfolio$PortfolioValue[(nrow(portfolio)-1000):nrow(portfolio)], type = "l", theme = 'white', main = "Backtest Results", xlab = "Date", ylab = "Value")
addTA(Cl(AAPL), col = "red")
addTA(portfolio$SharesHold, col = "purple")
