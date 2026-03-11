# Alpha Vantage API - 股票数据获取

## 概述

Alpha Vantage 提供免费的股票 API，支持美股、A 股、港股等多种市场。

## API 配置

- **API Key 环境变量**: `ALPHA_VANTAGE_API_KEY` (从 `.Renviron` 读取)
- **Base URL**: `https://www.alphavantage.co/query`
- **免费限制**: 5 次/分钟, 500 次/天
- **R 包依赖**: `httr`, `jsonlite`, `xts`, `quantmod`

## MCP (Model Context Protocol) 支持

Alpha Vantage 提供 MCP 服务器，允许 AI 助手直接调用 API：

### 连接方式

| 方式 | 地址 | 说明 |
|------|------|------|
| **远程** | `https://mcp.alphavantage.co/mcp?apikey=YOUR_API_KEY` | 直接使用 |
| **本地** | `uvx av-mcp YOUR_API_KEY` | 需要 uv 工具 |

### 支持的平台

- Claude / Claude Code
- Cursor
- VS Code (需 MCP 扩展)
- OpenAI Codex
- OpenAI Agent Builder
- Gemini CLI

### MCP 工具分类

| 工具类别 | 可用函数 |
|----------|----------|
| `core_stock_apis` | TIME_SERIES_DAILY, TIME_SERIES_INTRADAY, GLOBAL_QUOTE, SYMBOL_SEARCH |
| `technical_indicators` | SMA, EMA, MACD, RSI, BBANDS, ADX, ATR, STOCH |
| `fundamental_data` | COMPANY_OVERVIEW, INCOME_STATEMENT, BALANCE_SHEET, CASH_FLOW |
| `alpha_intelligence` | NEWS_SENTIMENT, TOP_GAINERS_LOSERS, EARNINGS_CALL_TRANSCRIPT |
| `commodities` | WTI, BRENT, GOLD_SILVER_SPOT, ALL_COMMODITIES |
| `economic_indicators` | REAL_GDP, TREASURY_YIELD, CPI |

### Python MCP 测试脚本

已创建 `test_mcp_alpha_vantage.py`，测试通过 (2026-03-11):

```python
import requests

BASE_URL = "https://www.alphavantage.co/query"
API_KEY = os.getenv("ALPHA_VANTAGE_API_KEY")

# 日 K 线
params = {"function": "TIME_SERIES_DAILY", "symbol": "IBM", "apikey": API_KEY}
data = requests.get(BASE_URL, params=params).json()

# 技术指标
params = {"function": "SMA", "symbol": "IBM", "interval": "daily", 
         "time_period": 20, "series_type": "close", "apikey": API_KEY}
data = requests.get(BASE_URL, params=params).json()
```

---

## 股票代码格式对比

| 市场 | Yahoo Finance | Alpha Vantage | 状态 |
|------|---------------|---------------|------|
| 美股 | `AAPL`, `IBM` | `AAPL`, `IBM` | ✅ 相同 |
| 上海 A 股 | `600519.SS` | `600519.SHH` | ✅ 已验证 |
| 深圳 A 股 | `000001.SZ` | `000001.SHZ` | ✅ 已验证 |
| 港股 ADR | - | `TCEHY` (腾讯) | ✅ 已验证 |
| 港股伦敦 (HKD) | `0700.HK` | `0Z4S.LON` | ✅ 已验证 |

## 常用 API 函数

### 1. 日 K 线数据 (TIME_SERIES_DAILY)

```r
# 获取日 K 线数据
params <- list(
  "function" = "TIME_SERIES_DAILY",
  symbol = "AAPL",
  outputsize = "compact",  # "compact": 100天, "full": 全部历史
  apikey = API_KEY
)

response <- GET(BASE_URL, query = params)
content <- content(response, "parsed")
daily_data <- content$`Time Series (Daily)`

# 转换为 data.frame
df <- data.frame(
  Date = as.Date(names(daily_data)),
  Open = as.numeric(sapply(daily_data, `[[`, "1. open")),
  High = as.numeric(sapply(daily_data, `[[`, "2. high")),
  Low = as.numeric(sapply(daily_data, `[[`, "3. low")),
  Close = as.numeric(sapply(daily_data, `[[`, "4. close")),
  Volume = as.numeric(sapply(daily_data, `[[`, "5. volume"))
)
```

### 2. 股票搜索 (SYMBOL_SEARCH)

```r
# 搜索股票代码
params <- list(
  "function" = "SYMBOL_SEARCH",
  keywords = "Tencent",
  apikey = API_KEY
)

response <- GET(BASE_URL, query = params)
content <- content(response, "parsed")
matches <- content$bestMatches

# 返回数据框
df <- data.frame(
  Symbol = sapply(matches, `[[`, "1. symbol"),
  Name = sapply(matches, `[[`, "2. name"),
  Type = sapply(matches, `[[`, "3. type"),
  Region = sapply(matches, `[[`, "4. region"),
  Currency = sapply(matches, `[[`, "8. currency")
)
```

### 3. 其他时间序列

- `TIME_SERIES_INTRADAY` - 分钟级数据
- `TIME_SERIES_DAILY_ADJUSTED` - 调整后日线（含股息拆分）
- `TIME_SERIES_WEEKLY` - 周线
- `TIME_SERIES_MONTHLY` - 月线

### 4. 技术指标

Alpha Vantage 内置技术指标：
- `SMA` - 简单移动平均
- `EMA` - 指数移动平均
- `RSI` - 相对强弱指数
- `MACD` - 移动平均收敛散度
- `BBANDS` - 布林带
- `STOCH` - 随机指标
- `ADX` - 平均趋向指数

```r
# 获取 RSI 指标
params <- list(
  "function" = "RSI",
  symbol = "AAPL",
  interval = "daily",
  time_period = 14,
  series_type = "close",
  apikey = API_KEY
)
```

## R 函数封装

```r
# 获取日 K 线数据
get_daily_data <- function(symbol, outputsize = "compact") {
  params <- list(
    "function" = "TIME_SERIES_DAILY",
    symbol = symbol,
    outputsize = outputsize,
    apikey = API_KEY
  )
  
  response <- GET(BASE_URL, query = params)
  content <- content(response, "parsed", encoding = "UTF-8")
  
  if (!is.null(content$`Error Message`)) {
    stop("API Error:", content$`Error Message`)
  }
  
  daily_data <- content$`Time Series (Daily)`
  
  dates <- names(daily_data)
  df <- data.frame(
    Date = as.Date(dates),
    Open = as.numeric(sapply(daily_data, `[[`, "1. open")),
    High = as.numeric(sapply(daily_data, `[[`, "2. high")),
    Low = as.numeric(sapply(daily_data, `[[`, "3. low")),
    Close = as.numeric(sapply(daily_data, `[[`, "4. close")),
    Volume = as.numeric(sapply(daily_data, `[[`, "5. volume"))
  )
  
  df <- df[order(df$Date), ]
  
  # 转换为 xts 对象
  xts_data <- xts(df[, c("Open", "High", "Low", "Close", "Volume")], 
                  order.by = df$Date)
  
  return(xts_data)
}

# 搜索股票
search_stock <- function(keywords) {
  params <- list(
    "function" = "SYMBOL_SEARCH",
    keywords = keywords,
    apikey = API_KEY
  )
  
  response <- GET(BASE_URL, query = params)
  content <- content(response, "parsed")
  
  if (is.null(content$bestMatches)) {
    return(data.frame())
  }
  
  matches <- content$bestMatches
  df <- data.frame(
    Symbol = sapply(matches, `[[`, "1. symbol"),
    Name = sapply(matches, `[[`, "2. name"),
    Type = sapply(matches, `[[`, "3. type"),
    Region = sapply(matches, `[[`, "4. region"),
    Currency = sapply(matches, `[[`, "8. currency"),
    stringsAsFactors = FALSE
  )
  
  return(df)
}
```

## 港股代码查找流程

由于港股在 Alpha Vantage 没有直接代码，需使用搜索功能：

1. 使用 `search_stock("腾讯")` 或 `search_stock("Tencent")` 搜索
2. 找到对应代码：
   - ADR: `TCEHY` (美元)
   - 伦敦上市: `0Z4S.LON` (港币)
3. 获取数据后注意货币单位

## 注意事项

1. **R 保留关键字**: `function` 是 R 保留关键字，API 参数需用引号：`"function" = "..."`
2. **API 限流**: 免费版 5 次/分钟，需使用 `Sys.sleep(6)` 间隔
3. **数据延迟**: 日线数据有 15 分钟延迟
4. **数据质量**: A 股数据与 Yahoo 对比一致

## 更新记录

- 2026-03-11: 新增 Alpha Vantage SKILL.md