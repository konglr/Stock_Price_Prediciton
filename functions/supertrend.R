# --- SuperTrend 函数实现 (基于 Pine Script 逻辑) ---
SuperTrend <- function(HLC, n = 10, factor = 3) {
  atr <- ATR(HLC, n = n)$atr
  hl2 <- (Hi(HLC) + Lo(HLC)) / 2
  
  upper_basic <- hl2 + factor * atr
  lower_basic <- hl2 - factor * atr
  
  upper_final <- upper_basic
  lower_final <- lower_basic
  
  # 初始化
  n_rows <- nrow(HLC)
  st <- rep(NA, n_rows)
  direction <- rep(1, n_rows) # 1 为看多, -1 为看空
  
  # 算法核心: 确保轨迹不“回撤”的阶梯逻辑
  # 找到第一个非 NA 的 ATR 位置
  start_idx <- which(!is.na(atr))[1]
  if (is.na(start_idx) || start_idx >= n_rows) return(xts(cbind(rep(NA, n_rows), rep(NA, n_rows)), order.by = index(HLC)))
  
  for (i in (start_idx + 1):n_rows) {
    # 提取当前和之前的值，并显式转换成数字以防万一
    curr_low_basic <- as.numeric(lower_basic[i])
    curr_up_basic <- as.numeric(upper_basic[i])
    prev_lower <- as.numeric(lower_final[i-1])
    prev_upper <- as.numeric(upper_final[i-1])
    prev_close <- as.numeric(Cl(HLC)[i-1])
    curr_close <- as.numeric(Cl(HLC)[i])
    
    # 下轨：只能上移
    if (is.na(prev_lower) || is.na(prev_close) || is.na(curr_low_basic)) {
      lower_final[i] <- curr_low_basic
    } else if (curr_low_basic > prev_lower || prev_close < prev_lower) {
      lower_final[i] <- curr_low_basic
    } else {
      lower_final[i] <- prev_lower
    }
    
    # 上轨：只能下移
    if (is.na(prev_upper) || is.na(prev_close) || is.na(curr_up_basic)) {
      upper_final[i] <- curr_up_basic
    } else if (curr_up_basic < prev_upper || prev_close > prev_upper) {
      upper_final[i] <- upper_basic[i]
    } else {
      upper_final[i] <- prev_upper
    }
    
    # 确定方向
    prev_dir <- direction[i-1]
    if (is.na(prev_dir)) {
      direction[i] <- 1
    } else if (prev_dir == 1 && !is.na(curr_close) && !is.na(lower_final[i]) && curr_close < lower_final[i]) {
      direction[i] <- -1
    } else if (prev_dir == -1 && !is.na(curr_close) && !is.na(upper_final[i]) && curr_close > upper_final[i]) {
      direction[i] <- 1
    } else {
      direction[i] <- prev_dir
    }
    
    # 赋值 Supertrend 线
    st[i] <- if (direction[i] == 1) lower_final[i] else upper_final[i]
  }
  
  res <- xts(cbind(st, direction), order.by = index(HLC))
  colnames(res) <- c("supertrend", "direction")
  return(res)
}
