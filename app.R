# ==============================================================================
# StockAI - AI 股票智能预测平台
# ==============================================================================
# 主入口文件 - 负责加载模块和启动应用
# 
# 项目结构说明:
#   R/constants.R      - 常量定义 (股票代码、周期、AI配置)
#   R/utils.R          - 辅助函数 (格式化、NULL处理等)
#   R/mod_data.R       - 数据模块 (获取、处理股票数据)
#   R/api_providers.R  - AI API 统一接口
#   R/mod_indicators.R - 技术指标模块
#   R/mod_plots.R      - 图表渲染模块
#   R/mod_ai_report.R  - AI 研报模块
#   R/mod_backtest.R   - 回测系统模块
#   R/mod_chat.R       - AI 金融助手模块
#   R/ui.R             - UI 布局定义
#   R/server.R         - Server 逻辑
# ==============================================================================

# ------------------------------------------------------------------------------
# 加载必要的包
# ------------------------------------------------------------------------------
library(shiny)
library(bslib)
library(shinyWidgets)
library(tidyquant)
library(quantmod)
library(TTR)
library(htmltools)
library(httr2)
library(jsonlite)
library(markdown)
library(shinychat)
library(bsicons)

# ------------------------------------------------------------------------------
# 加载环境变量
# ------------------------------------------------------------------------------
readRenviron(".Renviron")

# ------------------------------------------------------------------------------
# 加载自定义外部函数
# ------------------------------------------------------------------------------
source("functions/supertrend.R")
source("functions/backtest.R")

# ------------------------------------------------------------------------------
# 加载项目模块 (按依赖顺序)
# ------------------------------------------------------------------------------
# 1. 常量和辅助函数
source("R/constants.R")
source("R/utils.R")

# 2. 核心模块
source("R/mod_data.R")
source("R/mod_data_eastmoney.R")  # 东方财富数据模块
source("R/api_providers.R")

# 3. 功能模块
source("R/mod_indicators.R")
source("R/mod_plots.R")
source("R/mod_ai_report.R")
source("R/mod_backtest.R")
source("R/mod_chat.R")

# 4. UI 和 Server
source("R/ui.R")
source("R/server.R")

# ------------------------------------------------------------------------------
# 启动应用
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)