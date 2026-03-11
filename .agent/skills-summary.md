# Stock Price Prediction - Skills Summary

本文件汇总了项目中使用的所有 AI API 配置、技术指标规范及项目架构。所有 Agent 在协助开发时应参考此文件中的规范。

---

## 📁 项目架构 (2026-03-07 重构)

### 目录结构

```
StockAI/
├── app.R                      # 主入口（~70行，模块加载和启动）
├── R/                         # 模块化代码目录
│   ├── constants.R            # 常量定义（股票代码、周期、AI配置）
│   ├── utils.R                # 辅助函数（格式化、NULL处理）
│   ├── mod_data.R             # 数据模块（获取、处理股票数据）
│   ├── api_providers.R        # AI API 统一接口（Gemini/MiniMax/阿里云）
│   ├── mod_indicators.R       # 技术指标模块（SMA/BBands/MACD等）
│   ├── mod_plots.R            # 图表渲染模块（统计卡片、研报UI）
│   ├── mod_ai_report.R        # AI 研报模块（联网分析、提示词构建）
│   ├── mod_backtest.R         # 回测系统模块（绩效统计、策略加载）
│   ├── mod_chat.R             # AI 金融助手模块（聊天分析）
│   ├── ui.R                   # UI 布局定义（侧边栏、主界面）
│   └── server.R               # Server 主逻辑（响应式对象、事件处理）
├── functions/                  # 核心函数库
│   ├── supertrend.R           # SuperTrend 指标计算
│   └── backtest.R             # 回测执行引擎
├── trading_strategy/          # 策略模板
│   ├── sma_cross.R            # SMA 交叉策略
│   ├── supertrend.R           # SuperTrend 趋势策略
│   ├── adx_bbands.R           # ADX+BBands 策略
│   └── rsi_logic.R            # RSI 超买超卖策略
├── test_api/                   # API 测试脚本
├── .Renviron                   # API 密钥
└── .agent/
    └── skills-summary.md      # 本文件
```

### 模块加载顺序

```r
# app.R 中的加载顺序（有依赖关系）
source("R/constants.R")      # 1. 常量（无依赖）
source("R/utils.R")          # 2. 辅助函数（依赖 constants.R）
source("R/mod_data.R")       # 3. 数据模块（依赖 utils.R）
source("R/api_providers.R")  # 4. API 接口（无依赖）
source("R/mod_indicators.R") # 5. 技术指标（依赖 quantmod/TTR）
source("R/mod_plots.R")      # 6. 图表渲染（依赖 utils.R, mod_data.R）
source("R/mod_ai_report.R")  # 7. AI 研报（依赖 api_providers.R, utils.R）
source("R/mod_backtest.R")   # 8. 回测模块（依赖 functions/backtest.R）
source("R/mod_chat.R")       # 9. 聊天模块（依赖 api_providers.R, mod_ai_report.R）
source("R/ui.R")             # 10. UI（依赖 constants.R, bslib）
source("R/server.R")         # 11. Server（依赖所有模块）
```

### 核心函数速查

| 模块 | 主要函数 | 说明 |
|------|----------|------|
| `mod_data.R` | `fetch_ticker_data()`, `process_ticker_data()`, `get_stock_stats()` | 数据获取与处理 |
| `api_providers.R` | `call_ai_api()`, `call_ai_api_chat()` | 统一 AI API 调用 |
| `mod_indicators.R` | `add_indicators()`, `add_indicator_sma()` 等 | 技术指标添加 |
| `mod_ai_report.R` | `run_ai_report()`, `analyze_user_question()` | AI 分析功能 |
| `mod_backtest.R` | `run_backtest()`, `load_strategy_template()` | 回测系统 |
| `mod_chat.R` | `handle_chat_message()`, `run_ai_chat()` | 聊天助手 |

---

## 1. 阿里云 DashScope API (Qwen)

* **API Key 环境变量**: `ALIYUNCS_API_KEY` (从 `.Renviron` 读取)
* **Base URLs**:
    * OpenAI 兼容协议：`https://coding.dashscope.aliyuncs.com/v1`
    * Anthropic 兼容协议：`https://coding.dashscope.aliyuncs.com/apps/anthropic`
* **已验证模型 (2026-03-02)**:
    * `qwen3.5-plus`
    * `qwen3-max-2026-01-23`
    * `qwen3-coder-next`
    * `qwen3-coder-plus`
    * `glm-5`
    * `glm-4.7`
    * `kimi-k2-5`
* **R 示例**:
    ```r
    apiKey <- Sys.getenv("ALIYUNCS_API_KEY")
    # 使用 httr2 调用 OpenAI 兼容接口
    base_url <- "https://coding.dashscope.aliyuncs.com/v1"
    ```

---

## 2. MiniMax API (✅ 已验证)

* **状态**: ✅ 2026-02-26 测试通过
* **API Key 环境变量**: `MINIMAX_API_KEY` (从 `.Renviron` 读取)
* **API Endpoints**:
    * **OpenAI 兼容**: `https://api.minimax.chat/v1` (使用 `/v1/chat/completions`)
    * **Native V2**: `https://api.minimax.chat/v1/text/chatcompletion_v2` (推荐，支持 reasoning)
* **推荐模型**: `MiniMax-M2.5`, `MiniMax-M2.1`
* **注意**: `MiniMax-M2.5` 支持推理模式，若 `content` 为空，请检查 `reasoning_content`。

---

## 3. NVIDIA NIM API (✅ 已验证)

* **API Key 环境变量**: `NV_API_KEY`
* **Base URL**: `https://integrate.api.nvidia.com/v1`
* **支持模型**:
    * `minimaxai/minimax-m2.1`
    * `z-ai/glm4.7`
    * `meta/llama-3.1-405b-instruct`

---

## 4. Google Gemini API

* **API Key 环境变量**: `GEMINI_API_KEY`
* **最新验证模型**: `gemini-3.1-flash-lite-preview`
* **特色功能**: 支持 `google_search` grounding 工具。
* **角色规范**: 历史消息中模型角色必须使用 `"model"` 而不是 `"assistant"`。

---

## 5. AI 模型联网支持情况

| 提供商 | 模型 | 联网搜索支持 | 说明 |
|--------|------|-------------|------|
| Google | Gemini 2.5 Flash | ✅ 支持 | 通过 `google_search` grounding 工具 |
| MiniMax | MiniMax-M2.5 | ❌ 不支持 | 模型本身不支持工具调用 |
| 阿里云 | Qwen 3.5 Plus | ❌ 不支持 | 模型本身不支持工具调用 |

---

## 6. quantmod chart_Series 指标

* **标准指示符** (使用 `add_` 前缀): `add_Vo()`, `add_SMA()`, `add_EMA()`, `add_BBands()`, `add_MACD()`, `add_RSI()`, `add_ADX()`
* **通用指示符** (使用 `add_TA()`): `SAR`, `OBV`, `MFI`, `CLV`, `TR`, `ATR`
* **注意**: `SuperTrend` 需自定义计算后使用 `add_TA()` 叠加。

**技术指标添加规则** (参见 `R/mod_indicators.R`):
```r
# 标准指标 - 使用 add_ 前缀
cs <- add_SMA(n = 20)
cs <- add_BBands()

# 自定义指标 - 使用 add_TA() 包装
cs <- add_TA(SAR(HLC(data)), on = 1, col = "purple")  # 主图叠加
cs <- add_TA(OBV(Cl(data), Vo(data)), col = "blue")   # 新面板

# 多列返回 - 提取特定列 (小写!)
atr_values <- ATR(HLC(data), n = 14)[, "atr"]
cs <- add_TA(atr_values, col = "red")
```

---

## 7. Alpha Vantage 股票数据 API

* **状态**: ✅ 2026-03-11 测试通过
* **API Key 环境变量**: `ALPHA_VANTAGE_API_KEY` (从 `.Renviron` 读取)
* **Base URL**: `https://www.alphavantage.co/query`
* **免费限制**: 5 次/分钟, 500 次/天
* **详细文档**: 参见 `.agent/skills/alphavantage/SKILL.md`

**股票代码格式** (与 Yahoo Finance 不同!):

| 市场 | Yahoo Finance | Alpha Vantage | 示例 |
|------|---------------|---------------|------|
| 美股 | `AAPL`, `IBM` | `AAPL`, `IBM` | ✅ 相同 |
| 上海 A 股 | `600519.SS` | `600519.SHH` | 贵州茅台 |
| 深圳 A 股 | `000001.SZ` | `000001.SHZ` | 平安银行 |
| 港股 ADR | - | `TCEHY` | 腾讯(USD) |
| 港股伦敦 | `0700.HK` | `0Z4S.LON` | 腾讯(HKD) |

**R 使用注意**:
- `function` 是 R 保留关键字，API 参数需用引号：`"function" = "TIME_SERIES_DAILY"`
- 港股需用 `SYMBOL_SEARCH` 查找正确代码

**MCP (Model Context Protocol) 支持**:
- **远程服务器**: `https://mcp.alphavantage.co/mcp?apikey=YOUR_API_KEY`
- **本地安装**: `uvx av-mcp YOUR_API_KEY`
- **支持平台**: Claude Code, Cursor, VS Code, OpenAI Codex
- **测试脚本**: `test_mcp_alpha_vantage.py` (2026-03-11 测试通过)

**MCP 可用工具**:
| 类别 | 工具 |
|------|------|
| `core_stock_apis` | TIME_SERIES_DAILY, GLOBAL_QUOTE, SYMBOL_SEARCH |
| `technical_indicators` | SMA, EMA, MACD, RSI, BBANDS, ADX, ATR |
| `fundamental_data` | COMPANY_OVERVIEW, INCOME_STATEMENT, BALANCE_SHEET |
| `commodities` | WTI, GOLD_SILVER_SPOT, ALL_COMMODITIES |
| `alpha_intelligence` | NEWS_SENTIMENT, TOP_GAINERS_LOSERS |

**Python 示例**:
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

**R 示例**:
```r
# 日 K 线数据
params <- list(
  "function" = "TIME_SERIES_DAILY",
  symbol = "600519.SHH",  # 茅台
  outputsize = "compact", # compact: 100天, full: 全部
  apikey = API_KEY
)

# 股票搜索
params <- list(
  "function" = "SYMBOL_SEARCH",
  keywords = "Tencent",
  apikey = API_KEY
)
```

---

## 8. 项目核心架构与流程

### AI 股票识别工作流

```
用户提问 → analyze_user_question() → 判断问题类型
                                        ↓
                    ┌──────────────────┼──────────────────┐
                    ↓                  ↓                  ↓
              需要数据            不需要数据          切换股票
                    ↓                  ↓                  ↓
           加载股票数据        直接回答问题      switch_ticker()
                    ↓                                    ↓
           run_ai_chat()                         pending_ai_request()
                    ↓                                    ↓
           显示分析结果                         等待数据加载
                                                       ↓
                                              触发 AI 分析
```

### 数据同步机制

已通过 `pending_ai_request()` 机制彻底解决切换股票时的数据竞争问题：

1. 用户问新股票 → `pending_ai_request()` 存储请求
2. `switch_ticker()` 更新输入
3. `ticker_data()` reactive 触发
4. `observeEvent(ticker_data())` 执行待处理请求

### 回测系统架构

```
策略选择 → load_strategy_template() → 代码填充
                                        ↓
参数配置 → run_backtest() → run_simple_backtest()
                                        ↓
                        ┌───────────────┼───────────────┐
                        ↓               ↓               ↓
                   交易记录        权益曲线        绩效统计
```

**策略文件规范** (`trading_strategy/*.R`):
```r
# 可用变量: data, prices, signals, n
sma5 <- SMA(prices, n = 5)
sma20 <- SMA(prices, n = 20)
for(i in 2:n) {
  if (!is.na(sma5[i-1]) && !is.na(sma20[i-1])) {
    if (sma5[i-1] <= sma20[i-1] && sma5[i] > sma20[i]) signals[i] <- 1   # Buy
    if (sma5[i-1] >= sma20[i-1] && sma5[i] < sma20[i]) signals[i] <- -1  # Sell
  }
}
```

---

## 9. Finnhub 股票数据 API

* **API Key 环境变量**: `FINNHUB_API_KEY` (从 `.Renviron` 读取)
* **Base URL**: `https://finnhub.io/api/v1`
* **认证方式**: Header `X-Finnhub-Token: api_key` 或 URL 参数 `?token=api_key`
* **文档**: https://finnhub.io/docs/api

### 数据可用性

| 数据类型 | 免费 | 付费 |
|----------|------|------|
| 实时报价 `/quote` | ✅ | ✅ |
| K线 `/stock/candle` | ❌ | ✅ Premium |
| 股票搜索 | ✅ | ✅ |
| 公司基本信息 | ✅ | ✅ |
| 新闻 (1年) | ✅ | ✅ |
| 财务指标 | ✅ | ✅ |
| 分析师评级 | ✅ | ✅ |
| 日内K线 | ❌ | ✅ |
| 美股财报 | ✅ (4季度) | ✅ |

### K线数据 (需付费)

* **端点**: `/stock/candle`
* **参数**:
  - `symbol`: 股票代码 (如 `AAPL`, `GOOGL`)
  - `resolution`: `1`, `5`, `15`, `30`, `60`, `D`, `W`, `M`
  - `from`, `to`: UNIX 时间戳
* **返回字段**: `c` (close), `h` (high), `l` (low), `o` (open), `t` (timestamp), `v` (volume), `s` (status)
* **注意**: 日K线已调整分割，分钟K线未调整

### 免费可用端点

| 端点 | 功能 |
|------|------|
| `/quote` | 实时报价 (c, h, l, o, pc, d, dp, t) |
| `/search` | 股票搜索 |
| `/stock/symbol` | 股票代码列表 |
| `/stock/profile2` | 公司基本信息 |
| `/news`, `/company-news` | 新闻 (1年历史) |
| `/stock/metric` | 财务指标 |
| `/stock/recommendation` | 分析师评级 |
| `/stock/earnings` | 美股财报 (最近4季度) |
| `/calendar/ipo` | IPO日历 |
| `/stock/split` | 股票分割 |
| `/stock/insider-transactions` | 内部交易 |
| `/forex/*`, `/crypto/*` | 外汇/加密货币 |

**Python 示例**:
```python
import requests
import time

API_KEY = os.getenv("FINNHUB_API_KEY")

# 实时报价 (免费)
url = "https://finnhub.io/api/v1/quote"
params = {"symbol": "AAPL", "token": API_KEY}
data = requests.get(url, params=params).json()
print(f"AAPL: ${data['c']}")  # current price

# K线数据 (付费)
from_ts = int(time.time()) - 365 * 24 * 3600  # 1年前
to_ts = int(time.time())
url = "https://finnhub.io/api/v1/stock/candle"
params = {
    "symbol": "AAPL",
    "resolution": "D",
    "from": from_ts,
    "to": to_ts,
    "token": API_KEY
}
data = requests.get(url, params=params).json()
```

**R 示例**:
```r
library(httr)
library(jsonlite)

api_key <- Sys.getenv("FINNHUB_API_KEY")

# 实时报价 (免费)
url <- "https://finnhub.io/api/v1/quote"
params <- list(symbol = "AAPL", token = api_key)
response <- GET(url, query = params)
data <- fromJSON(content(response, as = "text"))
cat(sprintf("AAPL: $%.2f\n", data$c))

# K线数据 (付费)
from_ts <- as.integer(Sys.time() - 365 * 24 * 3600)
to_ts <- as.integer(Sys.time())
url <- "https://finnhub.io/api/v1/stock/candle"
params <- list(
  symbol = "AAPL",
  resolution = "D",
  from = from_ts,
  to = to_ts,
  token = api_key
)
response <- GET(url, query = params)
data <- fromJSON(content(response, as = "text"))
```

### 结论

| 数据源 | K线数据 | 免费/付费 |
|--------|---------|-----------|
| **Finnhub** | 需要 Premium | 付费 |
| **Alpaca (IEX)** | 仅15分钟 | 免费 |
| **Alpha Vantage** | 20年 (有限制) | 免费 |
| **Yahoo/quantmod** | 完整历史 | 免费 |

---

## 10. Twelve Data 股票数据 API

* **状态**: ⚠️ 需验证 (注意: `.Renviron` 中变量名为 `TWELVEDATA_API_KEY` 大写K)
* **API Key 环境变量**: `TWELVEDATA_API_KEY` (从 `.Renviron` 读取)
* **Base URL**: `https://api.twelvedata.com`
* **认证方式**: URL 参数 `?apikey=api_key`
* **文档**: https://twelvedata.com/docs

### 数据可用性

| 数据类型 | 免费限制 | 付费 |
|----------|----------|------|
| **Time Series (K线)** | 800 API calls/day, 5年历史 | 更多 |
| **Quote (实时报价)** | ✅ | ✅ |
| **Stock Search** | ✅ | ✅ |
| **Company Info** | ✅ | ✅ |
| **Technical Indicators** | ✅ | ✅ |

### Python 示例
```python
import requests, os

API_KEY = os.getenv("TWELVEDATA_API_KEY")
url = "https://api.twelvedata.com/time_series"
params = {"symbol": "AAPL", "interval": "1day", "outputsize": 5, "apikey": API_KEY}
data = requests.get(url, params=params).json()
print(data["data"]["AAPL"][0])
```

### R 示例
```r
library(httr)
params <- list(symbol = "AAPL", interval = "1day", outputsize = 5, 
               apikey = Sys.getenv("TWELVEDATA_API_KEY"))
data <- content(GET("https://api.twelvedata.com/time_series", query = params), "text")
```

---

## 11. Financial Modeling Prep (FMP) API

* **状态**: ✅ 2026-03-11 测试通过 (Python)
* **API Key 环境变量**: `FMP_API_KEY`
* **Base URL**: `https://financialmodelingprep.com/api/v3`
* **文档**: https://site.financialmodelingprep.com/developer/docs

### 数据可用性

| 数据类型 | 免费限制 |
|----------|----------|
| **历史K线** | 最近 5 年, 250 API/day |
| **Quote** | ✅ |
| **Financial Statements** | 仅 3 recent |
| **Stock Screener** | ✅ |

### Python 示例
```python
import requests, os

API_KEY = os.getenv("FMP_API_KEY")
url = "https://financialmodelingprep.com/api/v3/historical-price-full/AAPL"
params = {"apikey": API_KEY, "serietype": "line"}
data = requests.get(url, params=params).json()
print(f"AAPL: ${data['historical'][0]['close']}")
```

---

## 12. EODHD API

* **状态**: ✅ 2026-03-11 测试通过 (Python)
* **API Key 环境变量**: `EODHD_API_KEY`
* **Base URL**: `https://eodhd.com/api`
* **文档**: https://eodhd.com/financial-apis/

### 数据可用性

| 数据类型 | 免费限制 |
|----------|----------|
| **历史K线 (EOD)** | 最近 5 年, 20 API/day |
| **Fundamental Data** | 部分 |
| **Screener** | ✅ |

### 注意
股票代码需要交易所后缀: `AAPL.US`, `700.HK`

### Python 示例
```python
import requests, os

API_KEY = os.getenv("EODHD_API_KEY")
url = "https://eodhd.com/api/eod/AAPL.US"
params = {"api_token": API_KEY, "from": "2025-01-01", "to": "2025-12-31"}
data = requests.get(url, params=params).text
print(data.split("\n")[-2])
```

---

## 13. AkShare (中国金融数据)

* **状态**: ✅ 2026-03-11 测试通过 (Python)
* **类型**: Python 开源库
* **安装**: `pip install akshare`
* **文档**: https://akshare.akfamily.xyz

### 数据覆盖

| 数据类型 | 说明 |
|----------|------|
| **股票数据** | A股、港股、美股 |
| **期货/期权** | 股指、商品 |
| **基金/债券** | 公募、国债 |
| **宏观经济** | GDP、CPI、M2 |
| **资金流向** | 北向资金、龙虎榜 |

### Python 示例
```python
import akshare as ak

# A股日K线
df = ak.stock_zh_a_hist(symbol="000001", period="daily", 
                        start_date="20250101", end_date="20251231")

# A股实时行情
df = ak.stock_zh_a_spot_em()

# 北向资金
df = ak.stock_hsgt_hist()
```

---

## 14. 数据源对比总结

### 国际股票数据

| 数据源 | 历史K线 | 免费限制 | 特色 |
|--------|---------|----------|------|
| **Twelve Data** | ✅ 5年 | 800次/天 | 性价比最高 |
| **FMP** | ✅ 5年 | 250次/天 | 财务报表丰富 |
| **EODHD** | ✅ 5年 | 20次/天 | 50+交易所 |
| **Alpha Vantage** | ✅ 20年 | 5次/分钟 | 技术指标 |
| **Alpaca (IEX)** | ❌ 仅15分钟 | 免费 | 实时交易 |
| **Finnhub** | ❌ 需Premium | 付费 | K线需付费 |

### 中国A股数据

| 数据源 | 特点 | 限制 |
|--------|------|------|
| **AkShare** | 最全中文数据 | 开源免费 |
| **Tushare Pro** | 需积分 | 付费 |
| **Yahoo Finance** | 免费 | 格式需转换 |

---

## 15. 项目核心架构与流程## 11. 项目核心架构与流程

### AI 股票识别工作流

```
用户提问 → analyze_user_question() → 判断问题类型
                                        ↓
                    ┌──────────────────┼──────────────────┐
                    ↓                  ↓                  ↓
              需要数据            不需要数据          切换股票
                    ↓                  ↓                  ↓
           加载股票数据        直接回答问题      switch_ticker()
                    ↓                                    ↓
           run_ai_chat()                         pending_ai_request()
                    ↓                                    ↓
           显示分析结果                         等待数据加载
                                                       ↓
                                              触发 AI 分析
```

### 数据同步机制

已通过 `pending_ai_request()` 机制彻底解决切换股票时的数据竞争问题：

1. 用户问新股票 → `pending_ai_request()` 存储请求
2. `switch_ticker()` 更新输入
3. `ticker_data()` reactive 触发
4. `observeEvent(ticker_data())` 执行待处理请求

### 回测系统架构

```
策略选择 → load_strategy_template() → 代码填充
                                        ↓
参数配置 → run_backtest() → run_simple_backtest()
                                        ↓
                        ┌───────────────┼───────────────┐
                        ↓               ↓               ↓
                   交易记录        权益曲线        绩效统计
```

**策略文件规范** (`trading_strategy/*.R`):
```r
# 可用变量: data, prices, signals, n
sma5 <- SMA(prices, n = 5)
sma20 <- SMA(prices, n = 20)
for(i in 2:n) {
  if (!is.na(sma5[i-1]) && !is.na(sma20[i-1])) {
    if (sma5[i-1] <= sma20[i-1] && sma5[i] > sma20[i]) signals[i] <- 1   # Buy
    if (sma5[i-1] >= sma20[i-1] && sma5[i] < sma20[i]) signals[i] <- -1  # Sell
  }
}
```

---

## 12. 开发规范

### 模块化原则

1. **单一职责**: 每个模块只负责一个功能域
2. **依赖注入**: 函数参数传递，避免全局变量
3. **命名约定**: 
   - 模块文件: `mod_*.R`
   - 函数前缀: 模块相关 (如 `data_`, `ai_`, `bt_`)
4. **文档注释**: 每个函数使用 roxygen2 风格注释

### Git 提交规范

```
feat: 添加新功能
fix: 修复 bug
refactor: 代码重构
docs: 文档更新
style: 代码格式
test: 测试相关
```

---

## 更新记录
- 2026-03-11: 新增 AkShare 中国金融数据 API (Python开源库)
- 2026-03-11: 新增 EODHD API，支持50+交易所 (免费20次/天)
- 2026-03-11: 新增 FMP (Financial Modeling Prep) API，财务报表丰富 (免费250次/天)
- 2026-03-11: 新增 Twelve Data 股票数据 API，支持5年历史K线 (免费800次/天)
- 2026-03-11: 新增 Finnhub 股票数据 API，包含实时报价（免费）和 K线数据（付费 Premium）
- 2026-03-11: 新增 Alpaca Markets 股票数据 API (Python/R 测试通过)，但免费账户仅支持15分钟数据限制
- 2026-03-11: 新增 Alpha Vantage 股票数据 API，修正港股代码格式 (0Z4S.LON)，增加 A 股格式对比 (SHH/SHZ)
- 2026-03-07: **重大重构** - 项目模块化，拆分为 12 个独立模块文件
- 2026-03-02: 更新阿里云 DashScope 已验证模型列表，修正 MiniMax API 端点，新增 Gemini 2.5 规范。
- 2026-02-26: 新增 MiniMax API 和 NVIDIA NIM API 配置。
- 2026-02-26: 修正 NVIDIA API Key 变量名为 `NV_API_KEY`。
