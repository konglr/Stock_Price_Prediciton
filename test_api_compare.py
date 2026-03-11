import requests
import os

# 读取 .Renviron
def load_renviron(path='.Renviron'):
    if os.path.exists(path):
        with open(path, 'r', encoding='utf-8') as f:
            for line in f:
                if '=' in line and not line.startswith('#'):
                    k, v = line.split('=', 1)
                    os.environ[k.strip()] = v.strip().strip('"')

load_renviron()

# ======== Twelve Data ========
print('=' * 60)
print('Twelve Data API 测试')
print('=' * 60)

td_key = os.getenv('TWELVEDATA_API_Key')
print(f'API Key: {td_key[:15]}...')

td_url = 'https://api.twelvedata.com/time_series'
params = {'symbol': 'AAPL', 'interval': '1day', 'outputsize': 5, 'apikey': td_key}

resp = requests.get(td_url, params=params)
print(f'Status: {resp.status_code}')

td_data = resp.json()
print(f'Keys: {list(td_data.keys())}')

if 'values' in td_data:
    print('\n=== Twelve Data - AAPL 最近5天 ===')
    for bar in td_data['values']:
        print(f"  {bar['datetime']}: O={bar['open']} H={bar['high']} L={bar['low']} C={bar['close']} V={bar['volume']}")

print()

# ======== Alpha Vantage ========
print('=' * 60)
print('Alpha Vantage API 测试')
print('=' * 60)

av_key = os.getenv('ALPHA_VANTAGE_API_KEY')
print(f'API Key: {av_key[:10]}...')

av_url = 'https://www.alphavantage.co/query'
av_params = {'function': 'TIME_SERIES_DAILY', 'symbol': 'AAPL', 'outputsize': 'compact', 'apikey': av_key}

resp2 = requests.get(av_url, params=av_params)
print(f'Status: {resp2.status_code}')

av_data = resp2.json()
print(f'Keys: {list(av_data.keys())}')

if 'Time Series (Daily)' in av_data:
    ts = av_data['Time Series (Daily)']
    dates = sorted(ts.keys(), reverse=True)[:5]
    print('\n=== Alpha Vantage - AAPL 最近5天 ===')
    for d in dates:
        bar = ts[d]
        print(f"  {d}: O={bar['1. open']} H={bar['2. high']} L={bar['3. low']} C={bar['4. close']} V={bar['5. volume']}")