# Finnhub API - 股票数据

* **API Key 环境变量**: `FINNHUB_API_KEY` (从 `.Renviron` 读取)
* **Base URL**: `https://finnhub.io/api/v1`
* **认证方式**: Header `X-Finnhub-Token: api_key` 或 URL 参数 `?token=api_key`
* **官方文档**: https://finnhub.io/docs/api

---

## 数据可用性对比

| 数据类型 | 免费 | 付费 (Premium) |
|----------|------|----------------|
| 实时报价 `/quote` | ✅ | ✅ |
| K线 `/stock/candle` | ❌ | ✅ |
| 股票搜索 | ✅ | ✅ |
| 公司基本信息 | ✅ | ✅ |
| 新闻 (1年) | ✅ | ✅ |
| 财务指标 | ✅ | ✅ |
| 分析师评级 | ✅ | ✅ |
| 美股财报 | ✅ (4季度) | ✅ |
| 日内K线 | ❌ | ✅ |
| 技术分析形态 | ❌ | ✅ |
| ESG 评分 | ❌ | ✅ |
| 机构持仓 | ❌ | ✅ |

---

## 免费可用端点

### 1. 实时报价 `/quote`

返回当前和历史报价数据。

```
GET https://finnhub.io/api/v1/quote?symbol=AAPL&token=API_KEY
```

**参数**: `symbol`, `token`

**返回**:
```json
{
  "c": 257.83,      // current price
  "d": -1.23,       // change
  "dp": -0.48,      // percent change
  "h": 262.48,      // high price of the day
  "l": 256.95,      // low price of the day
  "o": 259.50,      // open price of the day
  "pc": 259.06,     // previous close price
  "t": 1709571600   // timestamp
}
```

### 2. 股票搜索 `/search`

搜索股票代码和公司名称。

```
GET https://finnhub.io/api/v1/search?q=apple&token=API_KEY
```

**返回**:
```json
{
  "count": 3,
  "result": [
    {
      "description": "APPLE INC",
      "displaySymbol": "AAPL",
      "symbol": "AAPL",
      "type": "Common Stock"
    }
  ]
}
```

### 3. 公司基本信息 `/stock/profile2`

```
GET https://finnhub.io/api/v1/stock/profile2?symbol=AAPL&token=API_KEY
```

**返回**: 公司地址、CEO、行业、员工数、市值、股份等。

### 4. 新闻 `/news`, `/company-news`

```r
# 市场新闻
GET https://finnhub.io/api/v1/news?category=general&token=API_KEY

# 公司新闻
GET https://finnhub.io/api/v1/company-news?symbol=AAPL&from=2026-01-01&to=2026-03-10&token=API_KEY
```

### 5. 财务指标 `/stock/metric`

```
GET https://finnhub.io/api/v1/stock/metric?symbol=AAPL&metric=all&token=API_KEY
```

返回 10+ 年历史财务比率数据。

### 6. 分析师评级 `/stock/recommendation`

```
GET https://finnhub.io/api/v1/stock/recommendation?symbol=AAPL&token=API_KEY
```

### 7. 美股财报 `/stock/earnings`

```
GET https://finnhub.io/api/v1/stock/earnings?symbol=AAPL&token=API_KEY
```

返回最近 4 个季度的财报数据。

---

## K线数据 (付费 Premium)

### `/stock/candle`

```
GET https://finnhub.io/api/v1/stock/candle?symbol=AAPL&resolution=D&from=1704067200&to=1709571600&token=API_KEY
```

**参数**:
| 参数 | 说明 | 示例 |
|------|------|------|
| `symbol` | 股票代码 | `AAPL`, `GOOGL` |
| `resolution` | 时间分辨率 | `1`, `5`, `15`, `30`, `60`, `D`, `W`, `M` |
| `from` | 开始时间 (UNIX) | `1704067200` |
| `to` | 结束时间 (UNIX) | `1709571600` |

**返回**:
```json
{
  "c": [260.83, 261.50, ...],  // close
  "h": [262.48, 263.00, ...],  // high
  "l": [256.95, 257.00, ...],  // low
  "o": [259.50, 260.00, ...],  // open
  "t": [1709500800, ...],       // timestamp
  "v": [30750595, ...],         // volume
  "s": "ok"                     // status
}
```

**注意**:
- 日K线 (`D`) 已调整股票分割
- 分钟K线 (`1`, `5`, `15` 等) 未调整分割

---

## Python 示例

```python
import requests
import os
import time

# 加载 .Renviron
def load_renviron(path=".Renviron"):
    if os.path.exists(path):
        with open(path, 'r', encoding='utf-8') as f:
            for line in f:
                if '=' in line and not line.startswith('#'):
                    k, v = line.split('=', 1)
                    os.environ[k.strip()] = v.strip().strip('"\'')

load_renviron()
API_KEY = os.getenv("FINNHUB_API_KEY")
BASE_URL = "https://finnhub.io/api/v1"

# 1. 实时报价 (免费)
url = f"{BASE_URL}/quote"
params = {"symbol": "AAPL", "token": API_KEY}
data = requests.get(url, params=params).json()
print(f"AAPL: ${data['c']} ({data['dp']:.2f}%)")

# 2. K线数据 (付费)
from_ts = int(time.time()) - 365 * 24 * 3600  # 1年前
to_ts = int(time.time())
url = f"{BASE_URL}/stock/candle"
params = {
    "symbol": "AAPL",
    "resolution": "D",
    "from": from_ts,
    "to": to_ts,
    "token": API_KEY
}
data = requests.get(url, params=params).json()
print(f"K线数据条数: {len(data.get('c', []))}")

# 3. 股票搜索 (免费)
url = f"{BASE_URL}/search"
params = {"q": "Tencent", "token": API_KEY}
data = requests.get(url, params=params).json()
for r in data.get('result', [])[:3]:
    print(f"  {r['symbol']}: {r['description']}")
```

---

## R 示例

```r
library(httr)
library(jsonlite)

api_key <- Sys.getenv("FINNHUB_API_KEY")
base_url <- "https://finnhub.io/api/v1"

# 1. 实时报价 (免费)
url <- paste0(base_url, "/quote")
params <- list(symbol = "AAPL", token = api_key)
response <- GET(url, query = params)
data <- fromJSON(content(response, as = "text"))
cat(sprintf("AAPL: $%.2f (%.2f%%)\n", data$c, data$dp))

# 2. K线数据 (付费)
from_ts <- as.integer(Sys.time() - 365 * 24 * 3600)
to_ts <- as.integer(Sys.time())
url <- paste0(base_url, "/stock/candle")
params <- list(
  symbol = "AAPL",
  resolution = "D",
  from = from_ts,
  to = to_ts,
  token = api_key
)
response <- GET(url, query = params)
data <- fromJSON(content(response, as = "text"))
cat(sprintf("K线数据条数: %d\n", length(data$c)))

# 3. 股票搜索 (免费)
url <- paste0(base_url, "/search")
params <- list(q = "Tencent", token = api_key)
response <- GET(url, query = params)
data <- fromJSON(content(response, as = "text"))
for (r in data$result[1:3]) {
  cat(sprintf("  %s: %s\n", r$symbol, r$description))
}
```

---

## 数据源对比总结

| 数据源 | K线数据 | 免费/付费 | 历史深度 |
|--------|---------|-----------|----------|
| **Finnhub** | 需要 Premium | 付费 | 完整 (Premium) |
| **Alpaca (IEX)** | 仅15分钟 | 免费 | 15分钟 |
| **Alpha Vantage** | 20年 (有限制) | 免费 | 20年 |
| **Yahoo/quantmod** | 完整历史 | 免费 | 完整 |

---

## 结论

- **Finnhub K线数据需要付费订阅 (Premium)**
- **免费功能**: 实时报价、股票搜索、公司信息、新闻、财报
- 如需免费获取历史K线数据，推荐使用 **Alpha Vantage** 或 **Yahoo Finance (quantmod)**