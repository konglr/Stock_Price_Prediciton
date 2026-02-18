# ADX & Bollinger Bands 趋势突破策略
# 逻辑：使用 ADX 确认趋势强度，使用布林带确认价格位置与动能
adx_vals <- ADX(HLC(data), n = 14)
bb_vals <- BBands(Cl(data), n = 20, sd = 2)

for(i in 2:n) {
  if (!is.na(adx_vals$ADX[i]) && !is.na(bb_vals$mavg[i]) && !is.na(bb_vals$up[i]) &&
      !is.na(bb_vals$dn[i]) && !is.na(bb_vals$mavg[i-1])) {
    # 提取指标
    adx <- adx_vals$ADX[i]
    dip <- adx_vals$DIp[i]
    din <- adx_vals$DIn[i]
    cl <- as.numeric(Cl(data)[i])
    mid_b <- bb_vals$mavg[i]
    up_b <- bb_vals$up[i]
    dn_b <- bb_vals$dn[i]
    
    # 新增过滤条件：需从下方突破或盘中回踩
    cl_prev <- as.numeric(Cl(data)[i-1])
    mid_prev <- as.numeric(bb_vals$mavg[i-1])
    lo_curr <- as.numeric(Lo(data)[i])
    
    # --- 核心信号生成逻辑 ---
    # 使用 if-else 结构避免买入信号被同日的卖出逻辑覆盖
    
    # 1. 优先判定买入逻辑
    is_buy <- FALSE
    
    # 策略 A (最高优先级)：超跌抄底 - 价格低于布林下轨
    if (lo_curr < dn_b) {
      signals[i] <- 1
      is_buy <- TRUE
    } 
    # 策略 B (次高优先级)：趋势突破
    else {
      trigger_condition <- (cl_prev < mid_prev || lo_curr <= mid_b)
      if (adx > 25 && dip > din && cl > mid_b && trigger_condition) {
        signals[i] <- 1
        is_buy <- TRUE
      }
    }
    
    # 2. 判定卖出逻辑 (仅在当日未发出买入信号时判定)
    if (!is_buy) {
      # 卖出条件：触碰上轨止盈 OR 趋势反转(DI) OR 跌破中轴支撑
      if (cl > up_b || din > dip || cl < mid_b) {
        signals[i] <- -1
      }
    }
  }
}
