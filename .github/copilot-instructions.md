# AI Coding Agent Instructions - Stock Price Prediction

AI-powered stock analysis platform built with R Shiny, integrating multiple LLM APIs for intelligent predictions.

## 🏗️ Architecture

```
app.R (1829 lines) - Main Shiny app with AI chat, technical indicators, backtesting
├── trading_strategy/*.R - Modular strategies (sma_cross, supertrend, adx_bbands, rsi_logic)
├── functions/backtest.R - Execution engine expecting data, prices, signals, n in env
├── functions/supertrend.R - Custom indicator implementation
└── .agent/skills-summary.md - **ALWAYS reference first** for API configs
```

**Data Flow**: User Input → quantmod fetch → Strategy signals → Backtest engine → Multi-LLM analysis → Visualization

## 🔑 API Configurations (Verified 2026-03-05)

All keys in `.Renviron` (gitignored). **Reference `.agent/skills-summary.md` for latest**:

| Provider | Env Variable | Base URL | Verified Models |
|----------|-------------|----------|-----------------|
| MiniMax | `MINIMAX_API_KEY` | `https://api.minimax.chat/v1` | MiniMax-M2.5, MiniMax-M2.1 |
| NVIDIA NIM | `NV_API_KEY` | `https://integrate.api.nvidia.com/v1` | glm-4.7, minimax-m2.1 |
| 阿里云 DashScope | `ALIYUNCS_API_KEY` | `https://coding.dashscope.aliyuncs.com/v1` | qwen3.5-plus, qwen3-max, glm-5 |
| Google Gemini | `GEMINI_API_KEY` | - | gemini-2.5-flash (supports google_search) |

## 📐 Critical Code Patterns

### 1. Technical Indicators with chart_Series()

**Standard indicators** (use `add_` prefix):
```r
cs <- chart_Series(data)
cs <- add_SMA(n = 20)
cs <- add_BBands()
cs <- add_MACD()
cs <- add_RSI()
cs <- add_ADX()
```

**Custom indicators** (use `add_TA()` with column extraction):
```r
# ❌ WRONG - These functions don't have add_ versions
cs <- add_SAR()  # Error!
cs <- add_OBV()  # Error!

# ✅ CORRECT - Use add_TA() wrapper
cs <- add_TA(SAR(OHLC(data)), on = 1, col = "purple")  # SAR on main chart
cs <- add_TA(OBV(Cl(data), Vo(data)), col = "blue")   # OBV in new panel
cs <- add_TA(MFI(HLC(data), Vo(data), n = 14), col = "orange")

# Multi-column returns - extract specific column (LOWERCASE!)
tr_values <- TR(HLC(data))[, "tr"]           # TR returns: tr, trueHigh, trueLow
atr_values <- ATR(HLC(data), n = 14)[, "atr"] # ATR returns: atr, trueHigh, trueLow
cs <- add_TA(tr_values, col = "brown")
```

### 2. Strategy File Pattern

All strategies in `trading_strategy/*.R` follow this pattern:
```r
# Example: trading_strategy/sma_cross.R
sma5 <- SMA(prices, n = 5)
sma20 <- SMA(prices, n = 20)
for(i in 2:n) {
  if (!is.na(sma5[i-1]) && !is.na(sma20[i-1])) {
    if (sma5[i-1] <= sma20[i-1] && sma5[i] > sma20[i]) signals[i] <- 1   # Buy
    if (sma5[i-1] >= sma20[i-1] && sma5[i] < sma20[i]) signals[i] <- -1  # Sell
  }
}
```

**Requirements**:
- Must define `signals` vector (1 = buy, -1 = sell, 0 = hold)
- Variables `data`, `prices`, `signals`, `n` available in environment
- Use `for(i in 2:n)` pattern to avoid look-ahead bias

### 3. Backtest Engine Expectations

The `run_simple_backtest()` function expects:
```r
run_simple_backtest(data, strategy_code, init_capital, trade_pct, stop_loss_pct, take_profit_pct)
```

Strategy code is evaluated via `eval(parse(text = strategy_code))` with access to:
- `data` - OHLC xts object
- `prices` - Close prices
- `signals` - Pre-initialized vector of zeros
- `n` - Number of rows

### 4. Python API Testing Pattern

Use manual `.Renviron` parsing (no dotenv dependency):
```python
# From test_api/minimax_openai_sdk_test.py
def load_renviron(path=".Renviron"):
    if os.path.exists(path):
        with open(path, 'r', encoding='utf-8') as f:
            for line in f:
                if '=' in line and not line.startswith('#'):
                    k, v = line.split('=', 1)
                    os.environ[k.strip()] = v.strip().strip('"\'')
```

Error handling must distinguish:
- `RateLimitError` (402/429) → quota exhausted
- `AuthenticationError` (401) → invalid key
- `ConnectionError`/404 → wrong base URL

### 5. AI Stock Detection Workflow

When user asks about a stock in chat:
1. `ask_ai_question_info()` → AI returns ticker (e.g., "601398.SS") or "GENERAL"
2. If ticker: Store in `pending_ai_request()`, call `switch_ticker()`
3. `observeEvent(ticker_data())` → Waits for data load, then triggers AI analysis

**Yahoo Finance ticker formats**:
- US: `AAPL`, `GOOGL`, `TSLA`
- Shanghai A-share: `600519.SS`
- Shenzhen A-share: `000001.SZ`
- Hong Kong: `0700.HK`

## 🚫 Critical Restrictions

1. **NEVER** hardcode API keys - use `Sys.getenv()` (R) or `os.getenv()` (Python)
2. **NEVER** use command substitution `$(...)` in GitHub Actions (security policy)
3. **DO NOT** modify `.Renviron` - reference existing keys only
4. **AVOID** `python-dotenv` - use manual `.Renviron` parsing
5. **DO NOT** commit `.Renviron`, `manifest.json`, or API keys

## 🔧 Common Workflows

### Add New Strategy
1. Create `trading_strategy/{name}.R` following sma_cross.R pattern
2. Initialize `signals <- rep(0, n)` is done by backtest engine
3. Add to UI in `app.R` indicator selection

### Add New Technical Indicator
1. Check if `add_{INDICATOR}()` exists (standard)
2. If not, use `add_TA(CALCULATION, on = 1)` for main chart or `add_TA(CALCULATION)` for new panel
3. Extract columns for multi-column returns: `ATR()[, "atr"]`, `TR()[, "tr"]`

### Test New API
1. Add config to `.agent/skills-summary.md`
2. Create `test_api/{service}_test.py` following minimax_openai_sdk_test.py pattern
3. Use OpenAI SDK for compatibility (MiniMax, NVIDIA, DashScope all support it)

### Deploy to ShinyApps.io
```r
rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")
# Then deploy via rsconnect package
```

## 📚 Essential Reference Files

| File | Purpose |
|------|---------|
| `.agent/skills-summary.md` | API configs, model specs, verified endpoints |
| `.agent/skills/quantmod/SKILL.md` | Technical indicator patterns |
| `functions/backtest.R` | Backtest execution engine |
| `trading_strategy/sma_cross.R` | Strategy template |
| `test_api/minimax_openai_sdk_test.py` | API testing pattern |

---
**Last Updated**: 2026-03-05
**Source of Truth**: `.agent/skills-summary.md` for all API configurations
