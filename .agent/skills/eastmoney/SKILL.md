# 东方财富 (East Money) API

* **状态**: ✅ 2026-03-13 测试通过
* **API 类型**: 股票数据 API (无需 API Key)
* **Base URL**: `https://push2his.eastmoney.com/api/qt/stock/kline/get`
* **文档**: https://www.eastmoney.com/

## 数据可用性

| 市场 | 支持 | 备注 |
|------|------|------|
| **A股 (沪深)** | ✅ | 上海/深圳交易所 |
| **美股** | ✅ | 主流美股 |
| **港股** | ✅ | 主板 116.，创业板 128. |
| **大宗商品** | ✅ | 112. 前缀 |

## 市场代码规则 (secid)

| 市场 | secid 格式 | 优先级 | 示例 |
|------|------------|--------|------|
| A股上海 | `1.` + 6位代码 | 唯一 | `1.600519` (茅台) |
| A股深圳 | `0.` + 6位代码 | 唯一 | `0.000001` (平安) |
| 港股主板 | `116.` + 5位代码 | 优先 | `116.00700` (腾讯) |
| 港股创业板 | `128.` + 5位代码 | 备用 | `128.00700` |
| 美股 | `105.` + 代码 | 唯一 | `105.AAPL` |
| 大宗商品 | `112.` + 代码 | 唯一 | `112.B00Y` (布伦特原油) |

## 备用机制

对于港股，如果 `116.` 前缀请求失败，可以尝试 `128.` 前缀（创业板）。

## 市场 ID 发现方法 (f12/f13 逻辑)

最可靠的方式是查询市场列表，观察每只股票的 **`f13`** (市场ID) 字段来确定正确的前缀。

### 发现脚本 (Python)

```python
# 示例：查询大宗商品市场
url = 'https://push2.eastmoney.com/api/qt/clist/get?pn=1&pz=10&fields=f12,f13,f14&fs=m:112'
r = requests.get(url).json()
if r.get('data'):
    for v in r['data']['diff'].values():
        print(f"Code(f12): {v['f12']}, MarketID(f13): {v['f13']}, Name(f14): {v['f14']}")
```

### R 语言实现

```r
# 查询港股市场列表
url <- "https://push2.eastmoney.com/api/qt/clist/get"
params <- list(
  pn = 1,
  pz = 10,
  fields = "f12,f13,f14",
  fs = "m:0+t:81"  # 港股
)

response <- GET(url, query = params)
data <- content(response, as = "parsed")

# 解析市场 ID
for (item in data$data$diff) {
  cat(sprintf("代码: %s, 市场ID(f13): %d, 名称: %s\n",
              item$f12, item$f13, item$f14))
}
```

### 常用市场过滤器 (fs 参数)

| 市场 | fs 参数 | 说明 |
|------|---------|------|
| A股上海 | `m:0+t:6+f:!2` | 上海主板 |
| A股深圳 | `m:0+t:80+f:!2` | 创业板 |
| 港股 | `m:0+t:81` | 港股 |
| 美股 | `m:0+t:23` | 美股 |
| 大宗商品 | `m:112` | 商品期货 |

## K线类型 (klt)

| klt | 类型 |
|-----|------|
| 101 | 日线 |
| 102 | 周线 |
| 103 | 月线 |

## 复权类型 (fqt)

| fqt | 类型 | 备注 |
|-----|------|------|
| 0 | 不复权 | |
| 1 | 前复权 | 推荐使用 |
| 2 | 后复权 | |

## R 示例

```r
library(httr)
library(jsonlite)
library(xts)

# 获取 A 股数据 (前复权)
code <- "600519"  # 茅台
secid <- paste0("1.", code)  # 上海市场
params <- list(
  fields1 = "f1",
  fields2 = "f51,f52,f53,f54,f55,f56,f57,f61",
  beg = "20250101",
  end = "20260313",
  rtntype = "2",
  secid = secid,
  klt = "101",  # 日线
  fqt = "1"     # 前复权
)

url <- "https://push2his.eastmoney.com/api/qt/stock/kline/get"
response <- GET(url, query = params, add_headers(`User-Agent` = "Mozilla/5.0"))
data <- content(response, as = "text", encoding = "UTF-8")
data <- fromJSON(data)

# 解析数据
klines <- data$data$klines
df <- data.frame(do.call(rbind, lapply(klines, function(x) strsplit(x, ",")[[1]])))
colnames(df) <- c("date", "open", "close", "high", "low", "volume", "amount", "turnover")

# 转换为 xts
df$date <- as.Date(df$date)
res_xts <- xts(df[, c("open", "high", "low", "close", "volume")], order.by = df$date)
```

## 测试结果 (2026-03-13)

| 股票代码 | 市场 | 结果 |
|----------|------|------|
| 600519 | A股 | ✅ 成功 (287条) |
| 000001 | A股 | ✅ 成功 (287条) |
| AAPL | 美股 | ✅ 成功 (299条) |
| MSFT | 美股 | ✅ 成功 (299条) |
| 00700 | 港股 | ✅ 成功 (294条) |
| 09988 | 港股 | ✅ 成功 (294条) |

## 数据对比

| 数据源 | A股 | 美股 | 港股 | 大宗商品 | 免费 |
|--------|-----|------|------|----------|------|
| **东方财富** | ✅ | ✅ | ✅ | ✅ | ✅ 无需 API Key |
| Yahoo Finance | ✅ | ✅ | ✅ | ❌ | ✅ |
| Alpha Vantage | ✅ | ✅ | ✅ | ✅ | ⚠️ 有限制 |
| Twelve Data | ⚠️ 有限 | ✅ | ❌ | ❌ | ⚠️ 有限 |

## 优势与限制

### 优势
- **无需 API Key**：直接使用，无需注册
- **全市场支持**：A股、美股、港股全覆盖
- **支持复权**：前复权、后复权、不复权
- **数据量大**：港股约294条，美股约299条

### 限制
- 无官方文档，接口可能变化