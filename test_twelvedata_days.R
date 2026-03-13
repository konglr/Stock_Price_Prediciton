# Twelve Data API - 验证免费版能获取多少天的K线数据
library(httr)
library(jsonlite)

# 读取 .Renviron
readRenviron(".Renviron")

API_KEY <- Sys.getenv("TWELVEDATA_API_KEY")

if (API_KEY == "") {
  stop("TWELVEDATA_API_KEY not found!")
}

cat("API Key:", substr(API_KEY, 1, 15), "...\n\n")

BASE_URL <- "https://api.twelvedata.com/time_series"

# 测试1: 不设置日期范围，看看默认返回多少条
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("测试1: 不设置日期范围 (默认 outputsize)\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

params1 <- list(
  symbol = "AAPL",
  interval = "1day",
  apikey = API_KEY
)

resp1 <- GET(BASE_URL, query = params1)
data1 <- content(resp1, as = "parsed")

if (!is.null(data1$values)) {
  cat("✅ 返回条数:", length(data1$values), "条\n")
  cat("   日期范围:", data1$values[[length(data1$values)]]$datetime,
      "至", data1$values[[1]]$datetime, "\n\n")
} else {
  cat("❌ 错误:", toJSON(data1), "\n\n")
}

# 测试2: 设置大日期范围，获取尽可能多的数据
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("测试2: 设置大日期范围 (2021-01-01 至 2026-03-13)\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

params2 <- list(
  symbol = "AAPL",
  interval = "1day",
  start_date = "2021-01-01",
  end_date = "2026-03-13",
  apikey = API_KEY
)

resp2 <- GET(BASE_URL, query = params2)
data2 <- content(resp2, as = "parsed")

if (!is.null(data2$values)) {
  cat("✅ 返回条数:", length(data2$values), "条\n")
  cat("   日期范围:", data2$values[[length(data2$values)]]$datetime,
      "至", data2$values[[1]]$datetime, "\n")

  # 计算大约多少年的数据
  first_date <- as.Date(data2$values[[1]]$datetime)
  last_date <- as.Date(data2$values[[length(data2$values)]]$datetime)
  days <- as.numeric(first_date - last_date)
  years <- days / 365
  cat("   约", round(years, 1), "年的数据\n\n")
} else {
  cat("❌ 错误:", toJSON(data2), "\n\n")
}

# 测试3: 不限制outputsize最大值
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("测试3: 请求更大的日期范围\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

params3 <- list(
  symbol = "AAPL",
  interval = "1day",
  start_date = "2020-01-01",
  end_date = "2026-03-13",
  apikey = API_KEY
)

resp3 <- GET(BASE_URL, query = params3)
data3 <- content(resp3, as = "parsed")

if (!is.null(data3$values)) {
  cat("✅ 返回条数:", length(data3$values), "条\n")
  cat("   日期范围:", data3$values[[length(data3$values)]]$datetime,
      "至", data3$values[[1]]$datetime, "\n")

  first_date <- as.Date(data3$values[[1]]$datetime)
  last_date <- as.Date(data3$values[[length(data3$values)]]$datetime)
  days <- as.numeric(first_date - last_date)
  years <- days / 365
  cat("   约", round(years, 1), "年的数据\n\n")
} else {
  cat("❌ 错误:", toJSON(data3), "\n\n")
}

cat("测试完成!\n")