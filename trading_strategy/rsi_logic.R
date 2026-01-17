# RSI 超买超卖策略
rsi <- RSI(prices, n = 14)
for(i in 2:n) {
  if (!is.na(rsi[i-1])) {
    # 跌破 30 后回升买入
    if (rsi[i-1] < 30 && rsi[i] >= 30) signals[i] <- 1
    # 涨破 70 后回撤卖出
    if (rsi[i-1] > 70 && rsi[i] <= 70) signals[i] <- -1
  }
}
