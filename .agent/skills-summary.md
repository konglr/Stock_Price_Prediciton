# Stock Price Prediction - Skills Summary

本文件汇总了项目中使用的所有 AI API 配置、技术指标规范及项目架构。所有 Agent 在协助开发时应参考此文件中的规范。

---

## 1. 阿里云 DashScope API (Qwen)

* **API Key 环境变量**: `ALIYUNCS_API_KEY` (从 `.Renviron` 读取)
* **Base URLs**:
 * OpenAI 兼容协议：`https://coding.dashscope.aliyuncs.com/v1`
 * Anthropic 兼容协议：`https://coding.dashscope.aliyuncs.com/apps/anthropic`
* **已验证模型**: `qwen3.5-plus`, `qwen3-max-2026-01-23`, `glm-4.7`, `glm-5`,`kimi-k2-5`， `qwen3-coder-next`, `qwen3-coder-plus`， `MiniMax-M2.5`,
* **R 示例**:
 ```r
 apiKey <- Sys.getenv("ALIYUNCS_API_KEY")
 # 使用 httr2 调用 OpenAI 兼容接口
 ```

---

## 2. quantmod chart_Series 指标

* **标准指示符** (使用 `add_` 前缀): `add_Vo()`, `add_SMA()`, `add_EMA()`, `add_BBands()`, `add_MACD()`, `add_RSI()`, `add_ADX()`
* **通用指示符** (使用 `add_TA()`): `SAR`, `OBV`, `MFI`, `CLV`, `TR`, `ATR`
* **注意**: `SuperTrend` 需自定义计算后使用 `add_TA()` 叠加。

---

## 3. MiniMax API (✅ 已验证)

* **状态**: ✅ 2026-02-26 测试通过
* **API Key 环境变量**: `MINIMAX_API_KEY` (从 `.Renviron` 读取)
* **Base URL**: `https://api.minimax.chat/v1`
* **兼容协议**: OpenAI 兼容 (`/v1/chat/completions`)
* **推荐模型**: `MiniMax-M2.5`, `MiniMax-M2.1`
* **Python 调用示例** (原生实现，无需 dotenv):
 ```python
 import os
 from openai import OpenAI

 # 手动加载 .Renviron
 def load_renviron(path=".Renviron"):
 if os.path.exists(path):
 with open(path, 'r', encoding='utf-8') as f:
 for line in f:
 line = line.strip()
 if line and not line.startswith('#') and '=' in line:
 k, v = line.split('=', 1)
 os.environ[k.strip()] = v.strip().strip('"\'')
 load_renviron()

 api_key = os.getenv("MINIMAX_API_KEY")
 client = OpenAI(api_key=api_key, base_url="https://api.minimax.chat/v1")
 response = client.chat.completions.create(model="MiniMax-M2.5", messages=[{"role": "user", "content": "Hello"}])
 ```

---

## 4. NVIDIA NIM API (新增)

* **API Key 环境变量**: `NV_API_KEY` (注意：不是 `NVIDIA_API_KEY`)
* **Base URL**: `https://integrate.api.nvidia.com/v1`
* **兼容协议**: OpenAI 兼容 (`/v1/chat/completions`)
* **支持模型**:
 * `z-ai/glm4.7` (GLM-4.7)
 * `minimaxai/minimax-m2.1` (MiniMax M2.1, NVIDIA 托管版)
 * `meta/llama-3.1-405b-instruct` (Llama 3.1)
* **Python 调用示例**:
 ```python
 import os
 from openai import OpenAI

 api_key = os.getenv("NV_API_KEY")
 client = OpenAI(api_key=api_key, base_url="https://integrate.api.nvidia.com/v1")
 
 # 测试 GLM-4.7
 response = client.chat.completions.create(
 model="z-ai/glm4.7", 
 messages=[{"role": "user", "content": "Hello"}]
 )
 ```

---

## 5. 项目架构

```text
Stock_Price_Prediction/
├── app.R # 主应用入口
├── trading_strategy/ # 交易策略
│ ├── sma_cross.R
│ ├── supertrend.R
│ ├── adx_bbands.R
│ └── rsi_logic.R
├── functions/ # 核心功能
│ ├── backtest.R
│ └── supertrend.R
├── keras stock prediction.R # Keras/TensorFlow 模型
├── .agent/ # Agent 配置和技能
│ ├── skills-summary.md # 本文件
│ └── skills/
│ ├── aliyun/SKILL.md
│ └── quantmod/SKILL.md
├── rsconnect/ # 部署配置
└── .posit/ # Positron 配置
```

---

## 更新记录
- 2026-02-26: 新增 MiniMax API (已验证 Base URL) 和 NVIDIA NIM API 配置。
- 2026-02-26: 修正 NVIDIA API Key 变量名为 `NV_API_KEY`。
- 2026-02-26: 调整章节顺序，将项目架构移至最后。.posit/                    # Positron 配置
```

---

## 5. 更新记录

| 日期 | 更新内容 |
|------|----------|
| 2026-02-25 | 初始创建，汇总阿里云 DashScope 和 quantmod 技能 |
