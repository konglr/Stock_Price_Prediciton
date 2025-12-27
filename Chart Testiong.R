

library(quantmod)
library(TTR)

# 1. 模拟数据准备 (包含解决 Volume 匹配问题的清洗逻辑)
test_ticker <- "AAPL"
data <- getSymbols(test_ticker, from = Sys.Date() - 1000, to = Sys.Date(), auto.assign = FALSE)

# 强制清洗列名：移除 "AAPL." 前缀，确保 add_Vo() 能找到 "Volume"
# 这是解决 "replacement has length zero" 报错的关键步骤
names(data) <- gsub(paste0("^", gsub("\\^", "\\\\^", test_ticker), "\\."), "", names(data))

# 2. 测试用例 A: 标准 chart_Series 配合 list 格式的 TA
# 注意：在 chart_Series 中，TA 必须是 list() 而不能是 c()
# 使用 add_Vo() 增加成交量面板
cat("正在测试用例 A: 标准渲染...\n")
cs <- chart_Series(data, 
                   name = "测试图表 - 标准 (含成交量)", 
                   type = "candlesticks",
                   TA = list(add_Vo(), 
                             add_SMA(n = 20, col = "blue"), 
                             add_SMA(n = 60, col = "red")),
                   theme = chart_theme())

ticker_data <-getSymbols(test_ticker, auto.assign = FALSE)

chart_Series(ticker_data, 
             type = "candlesticks", 
             subset = '2025',
             TA = c( "add_MACD()", "add_Vo()"),
             theme = chart_theme())
add_Vo()
add_SMA(n=20,on=1)
add_SMA(n=60,on=1,col = "purple")

# chartSeries
chartSeries(ticker_data, 
             type = "candlesticks", 
             TA = c(addVo(),
                    addSMA(n = 5 ),
                    addSMA(n = 20),
                    addSMA(n = 60)
                    ),
             theme = chartTheme("white"))