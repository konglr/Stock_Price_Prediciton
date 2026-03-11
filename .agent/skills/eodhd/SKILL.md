# EODHD API

* **状态**: ✅ 2026-03-11 测试通过
* **API Key 环境变量**: `EODHD_API_KEY`
* **Base URL**: `https://eodhd.com/api`
* **认证方式**: URL 参数 `?api_token=api_key`
* **文档**: https://eodhd.com/financial-apis/

## 数据可用性

| 数据类型 | 免费限制 |
|----------|----------|
| **历史K线 (EOD)** | 最近 5 年, 20 API/day |
| **Fundamental Data** | 部分 |
| **Technical Indicators** | ✅ |
| **Screener** | ✅ |

## 股票代码格式

**重要**: 需要添加交易所后缀
- 美股: `AAPL.US`, `MSFT.US`
- 港股: `700.HK`, `0001.HK`
- A股: `600519.SS` (上海), `000001.SZ` (深圳)
- 伦敦: `VOD.L`

## 主要端点

| 端点 | 功能 |
|------|------|
| `/eod/{symbol}` | 历史K线 (EOD) |
| `/fundamental/{symbol}` | 财务数据 |
| `/technical/{symbol}` | 技术指标 |
| `/screener` | 股票筛选器 |

## Python 示例

```python
import requests, os

API_KEY = os.getenv("EODHD_API_KEY")

# 历史K线 (注意需要 .US 后缀)
url = "https://eodhd.com/api/eod/AAPL.US"
params = {
    "api_token": API_KEY,
    "from": "2025-01-01",
    "to": "2025-12-31",
    "period": "d"
}
response = requests.get(url, params=params)
data = response.text.split("\n")

# 解析最后几行
for line in data[-6:-1]:
    if line:
        print(line)

# 港股
url = "https://eodhd.com/api/eod/700.HK"
params = {"api_token": API_KEY, "from": "2025-01-01"}
```

## 数据源对比

| 数据源 | 历史K线 | 免费限制 | 特色 |
|--------|---------|----------|------|
| **EODHD** | ✅ 5年 | 20次/天 | 50+交易所 |
| **Twelve Data** | ✅ 5年 | 800次/天 | 性价比最高 |
| **FMP** | ✅ 5年 | 250次/天 | 财务报表 |
| **Alpha Vantage** | ✅ 20年 | 5次/分钟 | 技术指标 |
