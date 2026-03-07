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
* **最新验证模型**: `gemini-2.5-flash`
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

## 7. 项目核心架构与流程

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

## 8. 开发规范

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
- 2026-03-07: **重大重构** - 项目模块化，拆分为 12 个独立模块文件
- 2026-03-02: 更新阿里云 DashScope 已验证模型列表，修正 MiniMax API 端点，新增 Gemini 2.5 规范。
- 2026-02-26: 新增 MiniMax API 和 NVIDIA NIM API 配置。
- 2026-02-26: 修正 NVIDIA API Key 变量名为 `NV_API_KEY`。
