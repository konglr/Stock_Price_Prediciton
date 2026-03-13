# Twelve Data API

* **状态**: ✅ 2026-03-11 测试通过 (Python & R)
* **API Key 环境变量**: `TWELVEDATA_API_KEY`
* **Base URL**: `https://api.twelvedata.com`
* **认证方式**: URL 参数 `?apikey=api_key` 或 Header `TD-API-KEY: api_key`
* **文档**: https://twelvedata.com/docs

## 数据可用性 (免费版)

| 数据类型 | 免费限制 | 备注 |
|----------|----------|------|
| **Time Series (K线)** | 800 API calls/day, 5年历史 | ⚠️ 仅美股可获取5年，A/港股受限 |
| **Quote (实时报价)** | ✅ | |
| **Stock Search** | ✅ | |
| **Company Info** | ✅ | |
| **Technical Indicators** | ✅ | |
| **Cryptocurrency** | ✅ | |
| **Forex** | ✅ | |

## 免费版市场支持 (2026-03-13 测试)

| 市场 | 免费支持 | 备注 |
|------|----------|------|
| **美股** | ✅ 完整 | 主要支持，无限制 |
| **A股 (深圳)** | ⚠️ 少数 | 仅 000001 等少数股票 |
| **A股 (上海)** | ❌ 付费 | 大部分需要 Pro/Venture |
| **港股** | ❌ 付费 | 需要 Pro/Venture |
| **美股中概股** | ⚠️ 有限 | BABA, JD 等需付费 |

### 测试结果

| 股票代码 | 名称 | 结果 |
|----------|------|------|
| AAPL | 苹果 | ✅ 成功 |
| MSFT | 微软 | ✅ 成功 |
| 000001 | 平安银行 | ✅ 成功 (23条) |
| 600519 | 茅台 | ❌ 需 Pro/Venture |
| 00700 | 腾讯 | ❌ 需 Pro/Venture |
| 9988 | 阿里 | ❌ 需 Pro/Venture |

## 主要端点

| 端点 | 功能 | 备注 |
|------|------|------|
| `/time_series` | K线 OHLCV | 支持 1min-1month |
| `/quote` | 实时报价 | 多种市场 |
| `/symbol_search` | 股票搜索 | 支持模糊搜索 |
| `/stocks` | 股票列表 | 按交易所 |
| `/company_profile` | 公司信息 | 基本面数据 |
| `/technical_indicators` | 技术指标 | SMA, EMA, MACD 等 |

## K线数据 (Time Series)

### 请求参数

| 参数 | 说明 | 示例 |
|------|------|------|
| `symbol` | 股票代码 (支持逗号分隔多个) | `AAPL`, `AAPL,MSFT,GOOGL` |
| `interval` | 时间框架 | `1min`, `5min`, `15min`, `30min`, `1hour`, `4hour`, `1day`, `1week`, `1month` |
| `start_date` | 开始日期 (YYYY-MM-DD) | `2025-01-01` |
| `end_date` | 结束日期 (YYYY-MM-DD) | `2025-12-31` |
| `outputsize` | 输出条数 | 默认 5 |
| `adjusted` | 调整分割 | `true`/`false` |
| `apikey` | API 密钥 | 必需 |

### 返回字段

| 字段 | 说明 |
|------|------|
| `datetime` | 时间戳 |
| `open` | 开盘价 |
| `high` | 最高价 |
| `low` | 最低价 |
| `close` | 收盘价 |
| `volume` | 成交量 |

### Python 示例

```python
import requests
import os

BASE_URL = "https://api.twelvedata.com"
API_KEY = os.getenv("TWELVEDATA_API_KEY")

# K线数据 - 多只股票
params = {
    "symbol": "AAPL,MSFT",
    "interval": "1day",
    "start_date": "2025-01-01",
    "end_date": "2025-12-31",
    "outputsize": 100,
    "apikey": API_KEY
}
response = requests.get(f"{BASE_URL}/time_series", params=params)
data = response.json()

print(f"Status: {data.get('code', 'OK')}")

# 解析数据
for symbol, bars in data.get('data', {}).items():
    if bars:
        latest = bars[-1]
        print(f"{symbol}: ${latest['close']} ({latest['datetime']})")

# 单只股票详细
params = {
    "symbol": "AAPL",
    "interval": "1day",
    "outputsize": 30,
    "apikey": API_KEY
}
response = requests.get(f"{BASE_URL}/time_series", params=params)
data = response.json()
```

### R 示例

```r
library(httr)
library(jsonlite)

BASE_URL <- "https://api.twelvedata.com"
API_KEY <- Sys.getenv("TWELVEDATA_API_KEY")

# K线数据
url <- paste0(BASE_URL, "/time_series")
params <- list(
  symbol = "AAPL,MSFT",
  interval = "1day",
  start_date = "2025-01-01",
  end_date = "2025-12-31",
  outputsize = 100,
  apikey = API_KEY
)

response <- GET(url, query = params)
data <- content(response, as = "parsed")

cat("Status:", data$code, "\n")

# 解析数据
for (symbol in names(data$data)) {
  bars <- data$data[[symbol]]
  latest <- bars[[length(bars)]]
  cat(sprintf("%s: $%s (%s)\n", symbol, latest$close, latest$datetime))
}
```

## 实时报价 (Quote)

### Python

```python
# 实时报价
params = {"symbol": "AAPL,MSFT", "apikey": API_KEY}
response = requests.get(f"{BASE_URL}/quote", params=params)
quote = response.json()

for symbol, data in quote.items():
    print(f"{symbol}: bid={data.get('bid')}, ask={data.get('ask')}, last={data.get('last')}")
```

### R

```r
# 实时报价
params <- list(symbol = "AAPL,MSFT", apikey = API_KEY)
response <- GET(paste0(BASE_URL, "/quote"), query = params)
quote <- content(response, as = "parsed")

for (symbol in names(quote)) {
  data <- quote[[symbol]]
  cat(sprintf("%s: bid=%s, ask=%s, last=%s\n", 
              symbol, data$bid, data$ask, data$last))
}
```

## 股票搜索 (Symbol Search)

### Python

```python
# 股票搜索
params = {"symbol": "Tencent", "apikey": API_KEY}
response = requests.get(f"{BASE_URL}/symbol_search", params=params)
results = response.json()

for item in results.get('data', []):
    print(f"{item['symbol']}: {item['description']} ({item['type']})")
```

### R

```r
# 股票搜索
params <- list(symbol = "Tencent", apikey = API_KEY)
response <- GET(paste0(BASE_URL, "/symbol_search"), query = params)
results <- content(response, as = "parsed")

for (i in seq_along(results$data)) {
  item <- results$data[[i]]
  cat(sprintf("%s: %s (%s)\n", item$symbol, item$description, item$type))
}
```

## 技术指标

### 支持的指标

- SMA (Simple Moving Average)
- EMA (Exponential Moving Average)
- MACD
- RSI (Relative Strength Index)
- BBANDS (Bollinger Bands)
- ADX (Average Directional Index)
- ATR (Average True Range)
- 等...

### Python 示例

```python
# 技术指标 - SMA
params = {
    "symbol": "AAPL",
    "interval": "1day",
    "function": "SMA",
    "time_period": 20,
    "series_type": "close",
    "apikey": API_KEY
}
response = requests.get(f"{BASE_URL}/technical_indicators", params=params)
data = response.json()

for point in data.get('values', [])[:5]:
    print(f"{point['datetime']}: SMA={point['value']}")
```

## 数据源对比

| 数据源 | 历史K线 | 免费/付费 | 备注 |
|--------|---------|-----------|------|
| **Twelve Data** | ✅ 5年 | 免费 800次/天 | **性价比最高** |
| **Alpha Vantage** | ✅ 20年 | 免费有限制 | 5 calls/min |
| **Alpaca (IEX)** | ❌ 仅15分钟 | 免费 | 实时交易 |
| **Finnhub** | ❌ 需Premium | 付费 | K线需付费 |
| **Yahoo/quantmod** | ✅ 完整 | 免费 | 最常用 |

## 限制与注意事项

1. **免费账户限制**: 800 次/天调用，5 年历史数据
2. **股票代码格式**: 
   - 美股: `AAPL`, `IBM`
   - 国际: `IBM:US`, `TCEHY:US` (需添加市场后缀)
3. **时间框架**: 支持 1min 到 1month
4. **数据调整**: `adjusted=true` 可获取调整后价格
5. **速率限制**: 尊重 API 限制，避免过度调用

## 替代方案

如果需要更多历史数据或更高调用频率:
- **Alpha Vantage**: 20年历史，免费有限制
- **Yahoo Finance (quantmod)**: 完全免费，历史完整
- **付费订阅 Twelve Data**: 无限制调用
