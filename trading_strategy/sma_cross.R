# SMA 均线交叉策略 (5/20)
sma5 <- SMA(prices, n = 5)
sma20 <- SMA(prices, n = 20)
for(i in 2:n) {
  if (!is.na(sma5[i-1]) && !is.na(sma20[i-1])) {
    # 金叉买入
    if (sma5[i-1] <= sma20[i-1] && sma5[i] > sma20[i]) signals[i] <- 1
    # 死叉卖出
    if (sma5[i-1] >= sma20[i-1] && sma5[i] < sma20[i]) signals[i] <- -1
  }
}
