import os
import json
import urllib.request

# 读取 .Renviron
renviron_path = "/Users/clarkkong/Library/Mobile Documents/com~apple~CloudDocs/R Projects/Stock_Price_Prediction/.Renviron"
with open(renviron_path, 'r') as f:
    for line in f:
        if '=' in line and not line.startswith('#'):
            k, v = line.strip().split('=', 1)
            os.environ[k] = v.strip('"')

api_key = os.environ.get('GEMINI_API_KEY', '')
print(f"API Key exists: {len(api_key) > 0}")
print(f"API Key prefix: {api_key[:15]}...")

model_id = 'gemini-3.1-flash-lite-preview'
url = f'https://generativelanguage.googleapis.com/v1beta/models/{model_id}:generateContent?key={api_key}'

data = {'contents': [{'parts': [{'text': 'hello'}]}]}

req = urllib.request.Request(
    url, 
    data=json.dumps(data).encode('utf-8'), 
    headers={'Content-Type': 'application/json'}
)

try:
    with urllib.request.urlopen(req) as response:
        result = json.loads(response.read().decode('utf-8'))
        print('SUCCESS!')
        print(result['candidates'][0]['content']['parts'][0]['text'])
except urllib.error.HTTPError as e:
    print(f'HTTP {e.code}:')
    print(e.read().decode())
except Exception as e:
    print(f'Error: {e}')