# SuperTrend 趋势追踪策略
st <- SuperTrend(HLC(data), n = 10, factor = 3)
dir <- st$direction
for(i in 2:n) {
  if (!is.na(dir[i-1]) && !is.na(dir[i])) {
    # 变绿买入
    if (dir[i-1] == -1 && dir[i] == 1) signals[i] <- 1
    # 变红卖出
    if (dir[i-1] == 1 && dir[i] == -1) signals[i] <- -1
  }
}
