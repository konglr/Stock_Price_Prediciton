# AI Coding Agent Instructions - Stock Price Prediction

This project is an **AI-powered stock analysis and trading platform** built with R Shiny, integrating multiple LLM APIs for intelligent predictions. All AI agents must understand the multi-model architecture and trading strategy framework.

## 🏗️ Architecture Overview

### Core Components
- **`app.R`** (1829 lines): Main Shiny application with AI integration, technical indicators, and backtesting UI
- **`trading_strategy/`**: Modular strategy implementations (SMA cross, SuperTrend, ADX+BBands, RSI logic)
- **`functions/`**: Core utilities including `backtest.R` (execution engine) and `supertrend.R` (custom indicator)
- **`keras stock prediction.R`**: Deep learning model (TensorFlow/Keras) - excluded from AI features
- **`.agent/skills-summary.md`**: **CRITICAL** - Contains all AI API configurations and model specifications

### Data Flow
```
User Input (Ticker/Period) → Data Fetch (quantmod) → Strategy Execution → 
Backtest Engine → AI Analysis (Multi-LLM) → Visualization (ggplot2/plotly)
```

## 🔑 Critical Developer Workflows

### 1. Environment Setup (REQUIRED)
All API keys stored in `.Renviron` (gitignored). Agents MUST reference `.agent/skills-summary.md` for current configurations:
- **MiniMax**: `MINIMAX_API_KEY` → `https://api.minimax.chat/v1` (✅ Verified 2026-02-26)
- **NVIDIA NIM**: `NV_API_KEY` → `https://integrate.api.nvidia.com/v1` (supports GLM-4.7, MiniMax-M2.1)
- **阿里云 DashScope**: `ALIYUNCS_API_KEY` → Multiple protocols (OpenAI/Anthropic compatible)

### 2. Testing API Integrations
Use verified test script: `test_api/minimax_openai_sdk_test.py`
```python
# Pattern: Manual .Renviron parsing (no dotenv dependency)
def load_renviron(path=".Renviron"):
    if os.path.exists(path):
        with open(path, 'r', encoding='utf-8') as f:
            for line in f:
                if '=' in line and not line.startswith('#'):
                    k, v = line.split('=', 1)
                    os.environ[k.strip()] = v.strip().strip('"\'')
```

### 3. Deployment Workflow
- Generate manifest: `rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")`
- Deploy to ShinyApps.io: Use `rsconnect` package with account configuration
- CI/CD: GitHub Actions workflows in `.github/workflows/` (Gemini CLI integration)

## 📐 Project-Specific Conventions

### R Code Patterns
- **Strategy files** (`trading_strategy/*.R`): Must define signals via `signals[i] <- 1/-1` pattern
- **Backtest engine** expects: `data`, `prices`, `signals`, `n` in environment
- **Technical indicators**: Use `add_` prefix for standard quantmod (`add_SMA`, `add_BBands`)
- **Custom indicators**: Use `add_TA()` wrapper (e.g., SuperTrend, SAR, OBV)

### Python Integration
- OpenAI SDK compatible APIs only (MiniMax, NVIDIA NIM, DashScope)
- Manual environment loading preferred over `python-dotenv`
- Error handling must distinguish: `RateLimitError` (quota), `AuthenticationError` (key), `ConnectionError` (URL)

### File Naming Conventions
- Strategies: `trading_strategy/{strategy_name}.R`
- Functions: `functions/{utility}.R`
- Tests: `test_api/{service}_test.py`

## 🚫 Critical Restrictions

1. **NEVER** hardcode API keys - always use `Sys.getenv()` or `os.getenv()`
2. **NEVER** use command substitution `$(...)` in GitHub Actions (security policy)
3. **DO NOT** modify `.Renviron` directly - reference existing keys only
4. **AVOID** `python-dotenv` dependency - use manual parsing (see test script)
5. **DO NOT** commit `.Renviron`, `manifest.json`, or deployment artifacts

## 🔍 Integration Points

### External Services
- **ShinyApps.io**: Primary deployment target (rsconnect)
- **GitHub Actions**: Gemini CLI workflows for issue triage and code review
- **Multiple LLMs**: Switchable via `.agent/skills-summary.md` configurations

### Key Dependencies
- R: `shiny`, `quantmod`, `TTR`, `tidyquant`, `httr2`, `jsonlite`
- Python: `openai`, `requests` (for API testing)

## 📚 Essential Reference Files

| File | Purpose | Agent Usage |
|------|---------|-------------|
| `.agent/skills-summary.md` | API configs, model specs, verified endpoints | **ALWAYS reference first** |
| `functions/backtest.R` | Trading execution logic | Understand signal flow |
| `trading_strategy/sma_cross.R` | Strategy template | Pattern for new strategies |
| `test_api/minimax_openai_sdk_test.py` | API integration test | Template for new API tests |
| `.github/workflows/gemini-*.yml` | CI/CD pipelines | Understand automation triggers |

## 🎯 Agent Task Guidelines

### When Adding New Strategies
1. Follow `trading_strategy/sma_cross.R` pattern (signals vector, loop structure)
2. Test with `functions/backtest.R` engine
3. Add to UI in `app.R` indicator selection

### When Testing New APIs
1. Add config to `.agent/skills-summary.md` (model name, base URL, env var)
2. Create test script following `test_api/minimax_openai_sdk_test.py` pattern
3. Verify with manual `.Renviron` parsing (no dotenv)

### When Modifying CI/CD
1. Review existing `.github/workflows/gemini-*.yml` patterns
2. Never use command substitution in shell commands
3. Test with `/gemini-invoke` command

---
**Last Updated**: 2026-02-26 (Verified MiniMax Base URL + NVIDIA NIM integration)
**Source of Truth**: `.agent/skills-summary.md` for all API configurations
