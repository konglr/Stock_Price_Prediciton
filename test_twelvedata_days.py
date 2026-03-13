# Twelve Data API - 验证免费版能获取多少天的K线数据
import os
import requests

# 加载 .Renviron
def load_renviron(env_file='.Renviron'):
    """读取 .Renviron 文件"""
    try:
        with open(env_file, 'r') as f:
            for line in f:
                line = line.strip()
                if line and not line.startswith('#'):
                    if '=' in line:
                        k, v = line.split('=', 1)
                        os.environ[k.strip()] = v.strip().strip('"')
    except FileNotFoundError:
        pass

load_renviron()

API_KEY = os.getenv("TWELVEDATA_API_KEY")

if not API_KEY:
    print("❌ TWELVEDATA_API_KEY not found!")
    exit(1)

print(f"API Key: {API_KEY[:10]}...")
print()

BASE_URL = "https://api.twelvedata.com/time_series"

# 测试1: 不设置日期范围，看看默认返回多少条
print("=" * 60)
print("测试1: 不设置日期范围 (默认 outputsize)")
print("=" * 60)
params = {
    "symbol": "AAPL",
    "interval": "1day",
    "apikey": API_KEY
}
resp = requests.get(BASE_URL, params=params)
data = resp.json()

if "values" in data:
    print(f"✅ 返回条数: {len(data['values'])} 条")
    print(f"   日期范围: {data['values'][-1]['datetime']} 至 {data['values'][0]['datetime']}")
else:
    print(f"❌ 错误: {data}")

print()

# 测试2: 设置大日期范围，获取尽可能多的数据
print("=" * 60)
print("测试2: 设置大日期范围 (2021-01-01 至 2026-03-13)")
print("=" * 60)
params2 = {
    "symbol": "AAPL",
    "interval": "1day",
    "start_date": "2021-01-01",
    "end_date": "2026-03-13",
    "apikey": API_KEY
}
resp2 = requests.get(BASE_URL, params=params2)
data2 = resp2.json()

if "values" in data2:
    print(f"✅ 返回条数: {len(data2['values'])} 条")
    print(f"   日期范围: {data2['values'][-1]['datetime']} 至 {data2['values'][0]['datetime']}")

    # 计算大约多少年的数据
    from datetime import datetime
    first_date = datetime.strptime(data2['values'][0]['datetime'], "%Y-%m-%d")
    last_date = datetime.strptime(data2['values'][-1]['datetime'], "%Y-%m-%d")
    days = (first_date - last_date).days
    years = days / 365
    print(f"   约 {years:.1f} 年的数据")
else:
    print(f"❌ 错误: {data2}")

print()

# 测试3: 使用 outputsize 参数
print("=" * 60)
print("测试3: outputsize=5000 (最大可能值)")
print("=" * 60)
params3 = {
    "symbol": "AAPL",
    "interval": "1day",
    "outputsize": 5000,
    "apikey": API_KEY
}
resp3 = requests.get(BASE_URL, params=params3)
data3 = resp3.json()

if "values" in data3:
    print(f"✅ 返回条数: {len(data3['values'])} 条")
    print(f"   日期范围: {data3['values'][-1]['datetime']} 至 {data3['values'][0]['datetime']}")
else:
    print(f"❌ 错误: {data3}")