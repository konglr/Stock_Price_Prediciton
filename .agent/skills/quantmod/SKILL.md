# Skill: quantmod chart_Series Indicators

When using the newer `chart_Series()` function (as opposed to the older `chartSeries()`), the naming convention for adding technical indicators changes.

## Core Principle
Standard indicators for `chart_Series()` use an underscore prefix: `add_`.

## Supported Standard Indicators
The following indicators have dedicated `add_` functions:
- `add_Vo()`: Volume
- `add_SMA()`: Simple Moving Average
- `add_EMA()`: Exponential Moving Average
- `add_BBands()`: Bollinger Bands
- `add_MACD()`: MACD
- `add_RSI()`: RSI
- `add_ADX()`: ADX

## Unsupported Indicators
If an indicator does not have a direct `add_` version (e.g., SAR, OBV, MFI, CLV), you should use the generic `add_TA()` function:
- `add_TA(SAR(OHLC), on = 1)`: Parabolic SAR
- `add_TA(OBV(Cl, Vo))`: On-Balance Volume
- `add_TA(MFI(HLC, Vo))`: Money Flow Index
- `add_TA(CLV(HLC))`: Close Location Value

## Implementation Example
```r
cs <- chart_Series(data)
cs <- add_Vo()
cs <- add_MACD()
cs <- add_TA(SAR(data), on = 1)
print(cs)
```

---

# Skill: AI 金融助手股票识别工作流 (Stock Detection Workflow)

在聊天功能中，通过 AI 判断用户问题是否涉及股票，并自动切换到对应股票显示 K 线图。

## 完整工作流程

```
用户发送消息（例如："帮我分析工商银行"）
    ↓
showNotification("AI 正在分析问题...")
    ↓
ask_ai_question_info() 调用 AI 判断问题类型
    ↓
AI 返回股票代码（如 601398.SS）或 "GENERAL"
    ↓
┌─────────────────────────────────────┐
│  如果返回股票代码：                   │
│  1. 存储请求到 pending_ai_request()   │
│  2. switch_ticker("601398.SS")      │
│  3. 退出当前 observer，不执行分析      │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│  如果返回 "GENERAL"：                 │
│  不切换股票，直接调用 AI 生成回复      │
└─────────────────────────────────────┘
    ↓
数据加载完成后 (observeEvent(ticker_data(), ...))
    ↓
检测到 pending_ai_request()，使用新数据生成分析回复
```

## 重要注意事项

### 1. 消除 Race Condition
不再使用 `Sys.sleep()`。通过 `pending_ai_request()` 和 `observeEvent(ticker_data())` 确保数据完全同步。

### 2. Yahoo Finance 股票代码格式
| 市场 | 格式 | 示例 | 
|------|------|------|
| 美股 | 1-5 个大写字母 | `AAPL`, `GOOGL`, `TSLA`, `NVDA` |
| A 股（上海）| 6 位数字 + .SS | `601988.SS`, `600519.SS` |
| A 股（深圳）| 6 位数字 + .SZ | `000001.SZ`, `002594.SZ` |
| 港股 | 4 位数字 + .HK | `0700.HK`, `9988.HK`, `3988.HK` |

### 3. MiniMax M2.5 推理模式
MiniMax M2.5 使用 reasoning 模式，`content` 可能为空，需要从 `reasoning_content` 提取。

---

# Skill: Refactoring Shiny Chat for True Reactivity

## Problem
The initial implementation suffered from race conditions. Using `Sys.sleep()` blocked the session and didn't guarantee data availability.

## Solution: Event-Driven Architecture
The chat logic is now synchronized with Shiny's reactive data flow.

### Core Changes:
1.  **State Management**: `pending_ai_request <- reactiveVal(NULL)` holds the user question during stock switching.
2.  **Data-Ready Observer**: `observeEvent(ticker_data(), ...)` only triggers *after* data is successfully updated.
3.  **Conditional Execution**: The main chat observer handles initial classification and stock switching, while the data-ready observer handles the actual AI analysis with fresh data.

### Workflow:
```
User asks about a NEW stock
    ↓
Main Chat Observer
    ├─ Store question in `pending_ai_request`
    └─ Call `switch_ticker()`
        ↓ (Wait for data download)
`ticker_data()` updated
    ↓
Data-Ready Observer fires
    ├─ Retrieve question from `pending_ai_request`
    └─ Execute AI analysis with NEW `ticker_data()`
```
This architecture ensures robust and synchronized AI responses.
