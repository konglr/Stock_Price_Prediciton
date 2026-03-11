# Financial Modeling Prep (FMP) API

* **状态**: ✅ 2026-03-11 测试通过
* **API Key 环境变量**: `FMP_API_KEY`
* **Base URL**: `https://financialmodelingprep.com/api/v3`
* **认证方式**: URL 参数 `?apikey=api_key`
* **文档**: https://site.financialmodelingprep.com/developer/docs

## 数据可用性

| 数据类型 | 免费限制 | 付费 |
|----------|----------|------|
| **历史K线** | 最近 5 年, 250 API/day | 完整 |
| **Quote (报价)** | ✅ | ✅ |
| **Financial Statements** | 仅 3 recent | 完整 |
| **Stock Screener** | ✅ | ✅ |

## 主要端点

| 端点 | 功能 |
|------|------|
| `/quote/{symbol}` | 实时报价 |
| `/historical-price-full/{symbol}` | 历史K线 |
| `/profile/{symbol}` | 公司信息 |
| `/income-statement/{symbol}` | 损益表 |
| `/rating/{symbol}` | 分析师评级 |

## Python 示例

```python
import requests, os

API_KEY = os.getenv("FMP_API_KEY")

# 历史K线
url = "https://financialmodelingprep.com/api/v3/historical-price-full/AAPL"
params = {"apikey": API_KEY, "serietype": "line", "limit": 5}
data = requests.get(url, params=params).json()

for day in data["historical"][:5]:
    print(f"{day['date']}: C=${day['close']}")

# 公司信息
info = requests.get(f"https://financialmodelingprep.com/api/v3/profile/AAPL?apikey={API_KEY}").json()[0]
print(f"Market Cap: ${info.get('mktCap', 0):,.0f}")
```

## 数据源对比

| 数据源 | 历史K线 | 免费限制 | 特色 |
|--------|---------|----------|------|
| **FMP** | ✅ 5年 | 250次/天 | 财务报表丰富 |
| **Twelve Data** | ✅ 5年 | 800次/天 | 性价比最高 |
| **Alpha Vantage** | ✅ 20年 | 5次/分钟 | 技术指标 |
