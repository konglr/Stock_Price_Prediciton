# ADX & Bollinger Bands 趋势突破策略
# 逻辑：使用 ADX 确认趋势强度，使用布林带确认价格位置与动能
adx_vals <- ADX(HLC(data), n = 14)
bb_vals <- BBands(Cl(data), n = 20, sd = 2)

for(i in 2:n) {
  if (!is.na(adx_vals$ADX[i]) && !is.na(bb_vals$mavg[i]) && !is.na(bb_vals$up[i])) {
    # 提取指标
    adx <- adx_vals$ADX[i]
    dip <- adx_vals$DIp[i]
    din <- adx_vals$DIn[i]
    cl <- as.numeric(Cl(data)[i])
    mid_b <- bb_vals$mavg[i]
    up_b <- bb_vals$up[i]
    
    # 买入逻辑：
    # 1. ADX > 25 (确认市场处于强趋势状态)
    # 2. DI+ > DI- (确认趋势向上)
    # 3. 价格在布林中轨之上 (确认处于上升通道)
    if (adx > 25 && dip > din && cl > mid_b) {
      signals[i] <- 1
    }
    
    # 卖出逻辑：
    # 1. 价格超过上轨 (止盈)
    # 2. DI- > DI+ (方向发生改变)
    # 3. 或者价格跌破布林中轨 (支撑位失效)
    if (cl > up_b || din > dip || cl < mid_b) {
      signals[i] <- -1
    }
  }
}
