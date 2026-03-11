#!/usr/bin/env Rscript
# 测试 AI 研报功能 - 诊断脚本

library(jsonlite)

# 读取环境变量
readRenviron(".Renviron")

cat("=== 环境变量检查 ===\n")
api_key <- Sys.getenv("GEMINI_API_KEY")
cat("GEMINI_API_KEY 长度:", nchar(api_key), "\n")
cat("API Key 前10位:", substr(api_key, 1, 10), "...\n\n")

# 测试 run_ai_report 函数
cat("=== 测试 run_ai_report 函数 ===\n")
source("R/utils.R")
source("R/api_providers.R")
source("R/mod_ai_report.R")

ticker <- "AAPL"
cat("测试股票:", ticker, "\n")

# 构建提示词
prompt <- build_ai_report_prompt(ticker)
cat("提示词预览 (前200字):", substr(prompt, 1, 200), "...\n\n")

# 调用 API
cat("正在调用 Gemini API (启用联网搜索)...\n")
result <- run_ai_report(
  ticker        = ticker,
  provider      = "gemini",
  model_id      = "gemini-3.1-flash-lite-preview",
  temperature   = 0.7,
  max_tokens    = 2048,
  enable_search = TRUE
)

cat("\n=== API 返回结果 ===\n")
cat("Prediction 类型:", class(result$prediction), "\n")
cat("Grounding:", !is.null(result$grounding), "\n")

if (!is.null(result$prediction)) {
  if (is.list(result$prediction)) {
    cat("\n预测结果:\n")
    print(result$prediction)
  } else {
    cat("\nPrediction 内容:\n")
    cat(result$prediction, "\n")
  }
}

if (!is.null(result$grounding)) {
  cat("\n=== Grounding 信息 ===\n")
  print(result$grounding)
}