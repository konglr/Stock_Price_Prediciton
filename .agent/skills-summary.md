# Stock Price Prediction - Skills Summary

本文件汇总了项目中使用的所有 AI API 配置、技术指标规范及项目架构。所有 Agent 在协助开发时应参考此文件中的规范。

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

---

## 7. 项目核心架构与流程

* **AI 股票识别工作流**: 
    1. AI 判断问题是否涉及股票（`ask_ai_question_info`）。
    2. 若涉及，调用 `switch_ticker()` 切换。
    3. 利用 `reactiveVal(pending_ai_request)` 存储待处理请求。
    4. 监听 `ticker_data()`，数据加载完成后自动触发 AI 分析。
* **数据同步**: 已通过 `pending_ai_request` 机制彻底解决切换股票时的数据竞争（Race Condition）问题。

---

## 更新记录
- 2026-03-02: 更新阿里云 DashScope 已验证模型列表，修正 MiniMax API 端点，新增 Gemini 2.5 规范。
- 2026-02-26: 新增 MiniMax API 和 NVIDIA NIM API 配置。
- 2026-02-26: 修正 NVIDIA API Key 变量名为 `NV_API_KEY`。
