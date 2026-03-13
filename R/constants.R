# ==============================================================================
# StockAI - 常量定义模块
# ==============================================================================
# 本文件包含项目中使用的所有常量定义，包括：
# - 预设股票代码
# - 时间周期映射
# - AI 提供商配置
# - 模型列表
# ==============================================================================

# ------------------------------------------------------------------------------
# 预设股票代码 (Yahoo Finance 格式)
# ------------------------------------------------------------------------------
PRESET_TICKERS <- c("AAPL", "AMZN", "GOOGL", "MSFT", "TSLA", "NVDA", "ORCL", "SQQQ", "^IXIC", "000001.SZ")

# ------------------------------------------------------------------------------
# 时间周期映射 (天数)
# ------------------------------------------------------------------------------
PERIOD_DAYS <- c(
  "20天" = 20,
  "1月"  = 30,
  "3月"  = 90,
  "6月"  = 180,
  "1年"  = 365,
  "2年"  = 730,
  "5年"  = 1825,
  "10年" = 3650
)

# 默认周期
DEFAULT_PERIOD <- "1年"

# ------------------------------------------------------------------------------
# K线周期选项
# ------------------------------------------------------------------------------
INTERVAL_CHOICES <- c(
  "日" = "daily",
  "周" = "weekly",
  "月" = "monthly",
  "年" = "yearly"
)

# ------------------------------------------------------------------------------
# 图表类型选项
# ------------------------------------------------------------------------------
PLOT_TYPE_CHOICES <- c(
  "折线图"   = "line",
  "条形图"   = "bars",
  "蜡烛图"   = "candlesticks",
  "针状图"   = "matchsticks"
)

# ------------------------------------------------------------------------------
# 技术指标选项
# ------------------------------------------------------------------------------
INDICATOR_CHOICES <- list(
  names = list(
    "SMA (简单移动平均)",
    "BBands (布林带)",
    "MACD",
    "RSI (相对强弱指标)",
    "ADX (平均趋向指标)",
    "SAR (抛物线转向)",
    "OBV (能量潮)",
    "MFI (资金流量指标)",
    "CLV (收盘位置值)",
    "TR (真实波幅)",
    "ATR (平均真实波幅)",
    "SuperTrend"
  ),
  values = c("SMA", "BBands", "MACD", "RSI", "ADX", "SAR", "OBV", "MFI", "CLV", "TR", "ATR", "SuperTrend")
)

# ------------------------------------------------------------------------------
# AI 提供商配置
# ------------------------------------------------------------------------------
AI_PROVIDERS <- list(
  gemini = list(
    name       = "Google Gemini",
    value      = "gemini",
    base_url   = "https://generativelanguage.googleapis.com/v1beta",
    env_key    = "GEMINI_API_KEY",
    supports_search = TRUE
  ),
  minimax = list(
    name       = "MiniMax",
    value      = "minimax",
    base_url   = "https://api.minimax.chat/v1",
    env_key    = "MINIMAX_API_KEY",
    supports_search = FALSE
  ),
  aliyun = list(
    name       = "阿里云百炼",
    value      = "aliyun",
    base_url   = "https://coding.dashscope.aliyuncs.com/v1",
    env_key    = "ALIYUNCS_API_KEY",
    supports_search = FALSE
  )
)

# ------------------------------------------------------------------------------
# AI 模型列表 (按提供商分组)
# ------------------------------------------------------------------------------
AI_MODELS <- list(
  gemini = c(
    "Gemini 3.1 Flash Lite (推荐)" = "gemini-3.1-flash-lite-preview"
  ),
  minimax = c(
    "MiniMax-M2.5 (推荐)" = "MiniMax-M2.5",
    "MiniMax-M2.1" = "MiniMax-M2.1"
  ),
  aliyun = c(
    "Qwen 3.5 Plus" = "qwen3.5-plus",
    "Qwen 3 Max" = "qwen3-max-2026-01-23",
    "Qwen3 Coder Next" = "qwen3-coder-next",
    "Qwen3 Coder Plus" = "qwen3-coder-plus",
    "GLM-5" = "glm-5",
    "GLM-4.7" = "glm-4.7",
    "Kimi K2.5" = "kimi-k2-5",
    "MiniMax-M2.5" = "MiniMax-M2.5"
  )
)

# ------------------------------------------------------------------------------
# AI 提供商选择选项
# ------------------------------------------------------------------------------
AI_PROVIDER_CHOICES <- c(
  "Google Gemini" = "gemini",
  "MiniMax"       = "minimax",
  "阿里云百炼"    = "aliyun"
)

# ------------------------------------------------------------------------------
# 回测策略选项
# ------------------------------------------------------------------------------
BACKTEST_STRATEGIES <- c(
  "自定义代码"            = "custom",
  "SuperTrend 趋势追踪"   = "supertrend",
  "ADX + BBands 趋势突破" = "adx_bbands",
  "SMA 交叉 (5/20)"       = "sma_cross",
  "RSI 超买超卖"          = "rsi_logic"
)

# ------------------------------------------------------------------------------
# 建仓规则选项
# ------------------------------------------------------------------------------
ENTRY_RULE_CHOICES <- c(
  "固定百分比" = "fixed_pct",
  "固定股数"   = "fixed_qty"
)

# ------------------------------------------------------------------------------
# 加仓规则选项
# ------------------------------------------------------------------------------
PYRAMID_RULE_CHOICES <- c(
  "不启用"    = "none",
  "正金字塔"  = "up",
  "等额加仓"  = "equal"
)

# ------------------------------------------------------------------------------
# 快捷问题模板
# ------------------------------------------------------------------------------
QUICK_QUESTIONS <- list(
  list(id = "quick_q1", icon = "📊", label = "技术面分析", question = "帮我分析一下当前股票的技术面走势"),
  list(id = "quick_q2", icon = "💰", label = "财务解读", question = "请解读一下该股票最新的财务报表和关键指标"),
  list(id = "quick_q3", icon = "📈", label = "趋势预测", question = "基于当前数据，预测一下未来一周的走势"),
  list(id = "quick_q4", icon = "🎯", label = "买卖建议", question = "给出现在该股票的买卖策略建议"),
  list(id = "quick_q5", icon = "📰", label = "重大新闻", question = "该股票最近有什么重大的新闻或公告？"),
  list(id = "quick_q6", icon = "❓", label = "指标解释", question = "请解释一下什么是 RSI 指标，在当前股票中如何应用？")
)

# ------------------------------------------------------------------------------
# 数据源配置
# ------------------------------------------------------------------------------
DATA_SOURCES <- c(
  "Yahoo Finance" = "yahoo",
  "Alpha Vantage" = "alphavantage",
  "Twelve Data" = "twelvedata",
  "东方财富" = "eastmoney"
)

DATA_SOURCE_INFO <- list(
  "yahoo" = list(
    name       = "Yahoo Finance",
    env_key    = NULL,
    limit_text = "无明确频率限制，数据完整",
    suffixes = list(sh = ".SS", sz = ".SZ", hk = ".HK")
  ),
  "alphavantage" = list(
    name       = "Alpha Vantage",
    env_key    = "ALPHA_VANTAGE_API_KEY",
    limit_text = "免费版限制：5次/分钟，仅最近100天数据",
    suffixes = list(sh = ".SHH", sz = ".SHZ", hk = ".HKG")
  ),
  "twelvedata" = list(
    name       = "Twelve Data",
    env_key    = "TWELVEDATA_API_KEY",
    limit_text = "免费：800次/天，美股5年+，A股/港股需付费",
    suffixes = list(sh = "", sz = "", hk = "")
  ),
  "eastmoney" = list(
    name       = "东方财富",
    env_key    = NULL,  # 无需 API Key
    limit_text = "免费无需Key，支持A股/美股/港股",
    suffixes = list(sh = "", sz = "", hk = "")  # 代码转换在函数内部处理
  )
)

# ------------------------------------------------------------------------------
# 股票代码格式说明
# ------------------------------------------------------------------------------
YAHOO_TICKER_FORMAT <- list(
  us      = "美股：GOOGL、AAPL、TSLA、NVDA 等",
  shanghai = "A 股上海：601988.SS、600519.SS 等",
  shenzhen = "A 股深圳：000001.SZ、002594.SZ 等",
  hk      = "港股：0700.HK、9988.HK 等"
)

ALPHAVANTAGE_TICKER_FORMAT <- list(
  us      = "美股：GOOGL、AAPL、TSLA、NVDA 等",
  shanghai = "A 股上海：601988.SHH、600519.SHH 等",
  shenzhen = "A 股深圳：000001.SHZ、002594.SHZ 等",
  hk      = "港股：0700.HKG、9988.HKG 等"
)