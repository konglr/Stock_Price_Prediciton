# AkShare (中国金融数据)

* **状态**: ✅ 2026-03-11 测试通过
* **类型**: Python 开源库
* **安装**: `pip install akshare`
* **文档**: https://akshare.akfamily.xyz
* **GitHub**: https://github.com/akfamily/akshare

## 数据覆盖

| 数据类型 | 说明 | 状态 |
|----------|------|------|
| **A股数据** | 日K线、分钟K线、实时行情 | ✅ |
| **港股数据** | 历史、实时 | ✅ |
| **美股数据** | 历史、实时 | ✅ |
| **期货数据** | 股指期货、商品期货 | ✅ |
| **期权数据** | 股票期权、期货期权 | ✅ |
| **基金数据** | 公募基金、ETF | ✅ |
| **债券数据** | 国债、企业债 | ✅ |
| **宏观经济** | GDP、CPI、M2、社融 | ✅ |
| **资金流向** | 北向资金、龙虎榜 | ✅ |
| **舆情数据** | 新闻、公告 | ✅ |

## 主要接口

### 股票数据

```python
import akshare as ak

# A股日K线
df = ak.stock_zh_a_hist(
    symbol="000001",      # 股票代码
    period="daily",        # 日K
    start_date="20250101",
    end_date="20251231"
)

# A股实时行情 (全部)
df = ak.stock_zh_a_spot_em()

# A股实时行情 (个股)
df = ak.stock_zh_a_new_spots_em(symbol="拼多多")

# 港股历史数据
df = ak.stock_zh_his_sina(symbol="00700")  # 腾讯

# 港股实时行情
df = ak.stock_zh_hs_spot_em()

# 美股数据
df = ak.stock_us_daily(symbol="AAPL")
```

### 资金流向

```python
# 北向资金历史
df = ak.stock_hsgt_hist()

# 北向资金持仓
df = ak.stock_hsgt_stock_stats_em()

# 主力资金流向
df = ak.stock_fund_flow(stock="000001")

# 龙虎榜数据
df = ak.stock_lhb_detail_em(date="20250110")
```

### 宏观经济

```python
# 中国GDP
df = ak.macro_cn_gdp()

# 货币供应量M2
df = ak.macro_cn_m2()

# 社会融资
df = ak.macro_cn_shr()

# 利率
df = ak.macro_cn_lpr()
```

### 期货数据

```python
# 期货日K线 (主力合约)
df = ak.futures_zh_daily_sina(symbol="IF")  # 股指期货
df = ak.futures_zh_daily_sina(symbol="CU")  # 沪铜

# 期货实时行情
df = ak.futures_zh_spot_em()
```

### 基金数据

```python
# ETF列表
df = ak.fund_etf_spot_em()

# ETF历史
df = ak.fund_etf_hist_sina(symbol="上证50ETF")

# 基金净值
df = ak.fund_fq_fund_info(symbol="161039")
```

## A股股票代码格式

- 上海A股: `600519` (贵州茅台)
- 深圳A股: `000001` (平安银行)
- 创业板: `300750` (宁德时代)
- 科创板: `688111`

## 使用示例

```python
import akshare as ak
import pandas as pd

# 获取茅台最近一年日K线
maotai = ak.stock_zh_a_hist(
    symbol="600519",
    period="daily",
    start_date="20250101",
    end_date="20251231"
)

print(maotai.head())
print(f"\n数据形状: {maotai.shape}")
print(f"列名: {list(maotai.columns)}")
```

## 数据源对比

### 中国A股数据

| 数据源 | 特点 | 限制 |
|--------|------|------|
| **AkShare** | 最全中文数据，开源免费 | 无官方限制 |
| **Tushare Pro** | 需积分/付费 | 付费 |
| **JoinQuant** | 需注册 | 有限制 |
| **Yahoo Finance** | 免费 | 格式需转换 |

### 国际数据

| 数据源 | 历史K线 | 免费限制 | 特色 |
|--------|---------|----------|------|
| **AkShare** | ✅ 完整 | 开源免费 | 中国数据最全 |
| **Twelve Data** | ✅ 5年 | 800次/天 | 性价比最高 |
| **FMP** | ✅ 5年 | 250次/天 | 财务报表 |
| **Alpha Vantage** | ✅ 20年 | 5次/分钟 | 技术指标 |
