# Alpha Vantage API - 股票数据获取

## 概述

Alpha Vantage 提供免费的股票 API，支持美股、A 股、港股等多种市场。

## API 配置

- **API Key 环境变量**: `ALPHA_VANTAGE_API_KEY` (从 `.Renviron` 读取)
- **Base URL**: `https://www.alphavantage.co/query`
- **免费限制**: 5 次/分钟, 500 次/天
- **R 包依赖**: `httr2`, `jsonlite`, `xts`, `quantmod`

---

## 免费版 API 端点限制测试 (2026-03-13)

| 端点 | 免费限制 | 数据量 | 说明 |
|------|---------|--------|------|
| `TIME_SERIES_DAILY` (compact) | ✅ 免费 | **100 条** | 最近 100 个交易日 |
| `TIME_SERIES_DAILY` (full) | 💰 付费 | 完整历史 | Premium 功能 |
| `TIME_SERIES_DAILY_ADJUSTED` | 💰 付费 | - | Premium 端点 |
| `TIME_SERIES_WEEKLY` | ✅ 免费 | **1375 条** | 约 26 年周线数据 |
| `TIME_SERIES_MONTHLY` | ✅ 免费 | **316 条** | 约 26 年月线数据 |
| `TIME_SERIES_INTRADAY` | 💰 付费 | - | Premium 端点 |
| `GLOBAL_QUOTE` | ✅ 免费 | 1 条 | 仅最新报价 |

### 测试验证结果 (2026-03-13)

```
=== 测试: TIME_SERIES_DAILY | outputsize: compact ===
数据行数: 100 
日期范围: 2025-10-17 至 2026-03-12 

=== 测试: TIME_SERIES_DAILY | outputsize: full ===
信息: outputsize=full 是付费功能

=== 测试: TIME_SERIES_WEEKLY ===
数据行数: 1375 
日期范围: 1999-11-12 至 2026-03-12 
```

### 推荐使用策略

| 数据需求 | 推荐方案 |
|----------|----------|
| 短期分析 (< 100 天) | Alpha Vantage `TIME_SERIES_DAILY` |
| 长期历史数据 | Yahoo Finance (无限制) |
| 周线分析 | Alpha Vantage `TIME_SERIES_WEEKLY` |

---

## 项目集成实现 (2026-03-13)

### 核心函数

```r
# 获取 Alpha Vantage 数据
fetch_alphavantage_data(ticker, from, to)

# 股票代码智能识别
extract_standard_ticker(ticker)  # 600519.SS → 600519

# 根据数据源转换格式
convert_ticker_for_source(ticker, source)  # 600519 → 600519.SHH (alphavantage)
```

### 股票代码格式自动转换

| 市场 | 标准格式 | Yahoo Finance | Alpha Vantage |
|------|----------|---------------|---------------|
| 美股 | `AAPL` | `AAPL` | `AAPL` |
| 上海 A 股 | `600519` | `600519.SS` | `600519.SHH` |
| 深圳 A 股 | `000001` | `000001.SZ` | `000001.SHZ` |
| 港股 | `0700` | `0700.HK` | `0700.HKG` |

### 数据格式统一

Alpha Vantage 返回 5 列，Yahoo Finance 返回 6 列，系统自动统一为 quantmod 兼容格式：

```r
# Alpha Vantage 返回列名
# Open, High, Low, Close, Volume

# Yahoo Finance 返回列名  
# Open, High, Low, Close, Volume, Adjusted
```

---

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

---

## 常用 API 函数

### 1. 日 K 线数据 (TIME_SERIES_DAILY)

```r
# 使用 httr2 获取日 K 线数据
library(httr2)

api_key <- Sys.getenv("ALPHA_VANTAGE_API_KEY")
req <- request("https://www.alphavantage.co/query") |>
  req_url_query(
    `function` = "TIME_SERIES_DAILY",
    symbol = "AAPL",
    outputsize = "compact",  # 免费: "compact" (100天), 付费: "full"
    apikey = api_key
  )

resp <- req_perform(req)
content <- resp_body_json(resp)

# 检查错误
if (!is.null(content$Note)) stop("频率限制:", content$Note)
if (!is.null(content$Information)) stop("API 信息:", content$Information)

# 解析数据
ts_data <- content$`Time Series (Daily)`
```

### 2. 周 K 线数据 (TIME_SERIES_WEEKLY)

```r
# 周 K 线可获取 26 年历史数据（免费）
req <- request("https://www.alphavantage.co/query") |>
  req_url_query(
    `function` = "TIME_SERIES_WEEKLY",
    symbol = "AAPL",
    apikey = api_key
  )
```

### 3. 股票搜索 (SYMBOL_SEARCH)

```r
# 搜索股票代码
req <- request("https://www.alphavantage.co/query") |>
  req_url_query(
    `function` = "SYMBOL_SEARCH",
    keywords = "Tencent",
    apikey = api_key
  )

resp <- req_perform(req)
content <- resp_body_json(resp)
matches <- content$bestMatches
```

### 4. 技术指标

Alpha Vantage 内置技术指标（部分可能需要付费）：
- `SMA` - 简单移动平均
- `EMA` - 指数移动平均
- `RSI` - 相对强弱指数
- `MACD` - 移动平均收敛散度
- `BBANDS` - 布林带

## 注意事项

1. **R 保留关键字**: `function` 是 R 保留关键字，API 参数需用反引号或引号
2. **API 限流**: 免费版 5 次/分钟，建议请求间隔 ≥ 12 秒
3. **数据延迟**: 日线数据有延迟
4. **付费端点**: `TIME_SERIES_DAILY_ADJUSTED`、`TIME_SERIES_INTRADAY`、`outputsize=full` 均为付费功能

## 更新记录

- 2026-03-13: 更新免费版 API 限制测试结果，新增项目集成实现说明
- 2026-03-11: 新增 Alpha Vantage SKILL.md