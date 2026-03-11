# Alpaca Markets API

本文件详细说明 Alpaca Markets 股票数据 API 的使用方法。

## 概述

Alpaca Markets 提供免费的股票市场数据 API，支持实时和历史数据。

**状态**: ✅ 2026-03-11 测试通过 (Python & R)

## 认证

### 环境变量

从 `.Renviron` 读取：
- `APCA_API_KEY_ID`
- `APCA_API_SECRET_KEY`

### 认证方式

通过 HTTP Headers 传递：
```
APCA_API_KEY_ID: your_api_key
APCA-API-SECRET_KEY: your_secret_key
```

## 数据端点

| 类型 | 端点 |
|------|------|
| 股票 K 线 | `https://data.alpaca.markets/v2/stocks/bars` |
| 股票报价 | `https://data.alpaca.markets/v2/stocks/quotes/latest` |
| 最新交易 | `https://data.alpaca.markets/v2/stocks/trades/latest` |

## API 参数

### 股票 K 线 (Bars)

| 参数 | 必填 | 说明 | 示例 |
|------|------|------|------|
| `symbols` | 是 | 股票代码，多个用逗号分隔 | `"AAPL,MSFT"` |
| `start` | 是 | 开始日期 (ISO 8601) | `"2026-03-01"` |
| `end` | 是 | 结束日期 (ISO 8601) | `"2026-03-10"` |
| `timeframe` | 是 | 时间框架 | `1Min`, `5Min`, `15Min`, `1Hour`, `1Day` |
| `limit` | 否 | 每只股票最大条数 (默认 10000) | `100` |
| `feed` | 否 | 数据源 (`iex` 免费, `sip` 付费) | `iex` |
| `adjustment` | 否 | 调整类型 (`raw`, `splits`, `dividends`) | `raw` |

### 数据源对比

| 数据源 | 价格 | 历史数据 | 成交量 |
|--------|------|----------|--------|
| `iex` | 免费 | 仅最近 15 分钟 | ~2.5% |
| `sip` | $99/月 | 完整历史 (2016起) | 100% |
| `boats` | 付费 | 夜盘数据 | - |
| `overnight` | 便宜 | 夜间数据，15分钟延迟 | - |

## 返回字段

### K 线数据 (OHLCV)

| 字段 | 说明 | 类型 |
|------|------|------|
| `t` | 时间戳 | string |
| `o` | 开盘价 (Open) | float |
| `h` | 最高价 (High) | float |
| `l` | 最低价 (Low) | float |
| `c` | 收盘价 (Close) | float |
| `v` | 成交量 (Volume) | integer |

### 响应格式

```json
{
  "bars": {
    "AAPL": [
      {
        "t": "2026-03-10T04:00:00Z",
        "o": 257.645,
        "h": 262.48,
        "l": 256.95,
        "c": 260.83,
        "v": 30750595
      }
    ],
    "MSFT": [
      {
        "t": "2026-03-10T04:00:00Z",
        "o": 403.50,
        "h": 408.20,
        "l": 402.10,
        "c": 405.76,
        "v": 18453200
      }
    ]
  },
  "next_page_token": null
}
```

## 使用示例

### Python

```python
import requests

url = "https://data.alpaca.markets/v2/stocks/bars"
params = {
    "symbols": "AAPL,MSFT",
    "start": "2026-03-01",
    "end": "2026-03-10",
    "timeframe": "1Day",
    "limit": 100,
    "feed": "iex"
}
headers = {
    "APCA_API_KEY_ID": api_key,
    "APCA_API_SECRET_KEY": secret_key
}

response = requests.get(url, params=params, headers=headers)
data = response.json()

# 访问数据
for symbol, bars in data['bars'].items():
    latest = bars[-1]
    print(f"{symbol}: ${latest['c']}")
```

### R

```r
library(httr2)

url <- "https://data.alpaca.markets/v2/stocks/bars"

req <- request(url) |>
  req_url_query(
    symbols = "AAPL,MSFT",
    start = "2026-03-01",
    end = "2026-03-10",
    timeframe = "1Day",
    limit = 100,
    feed = "iex"
  ) |>
  req_headers(
    "APCA_API_KEY_ID" = api_key,
    "APCA_API_SECRET_KEY" = secret_key
  )

response <- req_perform(req)
data <- resp_body_json(response, simplifyVector = TRUE)

# 访问数据
for (symbol in names(data$bars)) {
  bars <- data$bars[[symbol]]
  latest <- bars[nrow(bars), ]
  cat(sprintf("%s: $%.2f\n", symbol, latest$c))
}
```

## 限制与注意事项

1. **免费账户限制**: 只能获取最近 15 分钟的历史数据
2. **付费账户 ($99/月)**: 可获取自 2016 年起的完整历史数据
3. **请求频率**:
   - 免费: 200 次/分钟
   - 付费: 10000 次/分钟
4. **IEX 数据**: 仅覆盖约 2.5% 的成交量，可能与真实市场价格有差异

## 替代方案

如需免费获取历史日K线数据，建议使用：

1. **Yahoo Finance** (通过 `quantmod` R 包)
2. **Alpha Vantage** (免费 5次/分钟，500次/天)
3. **Polygon.io** (免费 tier 有限制)

详见 `skills-summary.md` 第 7 节 Alpha Vantage API。

## 官方文档

- [Alpaca Markets Documentation](https://docs.alpaca.markets/)
- [Historical API](https://docs.alpaca.markets/docs/historical-api)
- [Market Data API](https://docs.alpaca.markets/docs/about-market-data-api)