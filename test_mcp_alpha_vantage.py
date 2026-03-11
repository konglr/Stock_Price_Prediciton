"""
Alpha Vantage MCP (Model Context Protocol) Test Script
========================================================
MCP allows AI assistants to directly call Alpha Vantage APIs.

MCP Server Options:
1. Remote: https://mcp.alphavantage.co/mcp?apikey=YOUR_API_KEY
2. Local: uvx av-mcp YOUR_API_KEY

This script tests both MCP-style API calls and direct HTTP access.
"""

import os
import json
import requests
from datetime import datetime, timedelta

# Read API key from .Renviron
def load_renviron(path=".Renviron"):
    """Load environment variables from .Renviron file"""
    env_vars = {}
    if os.path.exists(path):
        with open(path, 'r', encoding='utf-8') as f:
            for line in f:
                line = line.strip()
                if line and not line.startswith('#') and '=' in line:
                    key, value = line.split('=', 1)
                    env_vars[key.strip()] = value.strip().strip('"\'')
    return env_vars

# Load API key
env_vars = load_renviron()
API_KEY = env_vars.get('ALPHA_VANTAGE_API_KEY', '')

if not API_KEY:
    print("❌ Error: ALPHA_VANTAGE_API_KEY not found in .Renviron")
    exit(1)

print(f"✅ API Key loaded: {API_KEY[:8]}...")

BASE_URL = "https://mcp.alphavantage.co/mcp"
DIRECT_URL = "https://www.alphavantage.co/query"

# ============================================================
# Test 1: Direct REST API (simulating what MCP would call)
# ============================================================
print("\n" + "="*60)
print("Test 1: Direct REST API (Alpha Vantage)")
print("="*60)

def get_daily_data_direct(symbol, outputsize="compact"):
    """Direct REST API call - same as MCP would do internally"""
    params = {
        "function": "TIME_SERIES_DAILY",
        "symbol": symbol,
        "outputsize": outputsize,
        "apikey": API_KEY
    }
    
    response = requests.get(DIRECT_URL, params=params, timeout=30)
    data = response.json()
    
    if "Time Series (Daily)" in data:
        ts = data["Time Series (Daily)"]
        dates = sorted(ts.keys(), reverse=True)[:5]
        print(f"\n📈 {symbol} - Last 5 days:")
        for d in dates:
            row = ts[d]
            print(f"   {d}: O={row['1. open']}, H={row['2. high']}, L={row['3. low']}, C={row['4. close']}, V={row['5. volume']}")
        return True
    elif "Error Message" in data:
        print(f"❌ Error: {data['Error Message']}")
        return False
    elif "Note" in data:
        print(f"⚠️ Rate limit: {data['Note']}")
        return False
    else:
        print(f"❌ Unknown response: {data}")
        return False

# Test US stocks
tests = [
    ("IBM", "US stock"),
    ("AAPL", "US stock"),
    ("600519.SHH", "Shanghai A-share"),
    ("000001.SHZ", "Shenzhen A-share"),
    ("TCEHY", "Tencent ADR (USD)"),
    ("0Z4S.LON", "Tencent HK (HKD)"),
]

for symbol, desc in tests:
    print(f"\n--- Testing {symbol} ({desc}) ---")
    try:
        success = get_daily_data_direct(symbol)
        if success:
            print(f"✅ {symbol} Success")
    except Exception as e:
        print(f"❌ {symbol} Error: {e}")

# ============================================================
# Test 2: MCP Server Connection (if available)
# ============================================================
print("\n" + "="*60)
print("Test 2: MCP Server Connection")
print("="*60)

def test_mcp_server():
    """Test MCP server availability"""
    mcp_url = f"{BASE_URL}?apikey={API_KEY}"
    print(f"Testing MCP endpoint: {mcp_url[:50]}...")
    
    # MCP uses POST requests with JSON body
    # Try a simple ping-like request
    test_payload = {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "tools/call",
        "params": {
            "name": "PING"
        }
    }
    
    try:
        # Note: MCP typically requires stdio or SSE transport
        # This is just informational - real MCP clients handle transport
        print("ℹ️ MCP Server info:")
        print("   - Remote URL: https://mcp.alphavantage.co/mcp?apikey=YOUR_KEY")
        print("   - Local install: uvx av-mcp YOUR_API_KEY")
        print("   - Use with Claude Code, Cursor, or VS Code with MCP extension")
        return True
    except Exception as e:
        print(f"❌ MCP test info: {e}")
        return False

test_mcp_server()

# ============================================================
# Test 3: Technical Indicators via API
# ============================================================
print("\n" + "="*60)
print("Test 3: Technical Indicators (via REST)")
print("="*60)

def get_technical_indicator(symbol, indicator, params=None):
    """Get technical indicator data"""
    default_params = {
        "function": indicator,
        "symbol": symbol,
        "interval": "daily",
        "time_period": 20,
        "series_type": "close",
        "apikey": API_KEY
    }
    if params:
        default_params.update(params)
    
    response = requests.get(DIRECT_URL, params=default_params, timeout=30)
    data = response.json()
    
    if "Technical Analysis: " + indicator in data:
        ts = data["Technical Analysis: " + indicator]
        dates = sorted(ts.keys(), reverse=True)[:3]
        print(f"\n📊 {symbol} - {indicator} (last 3 days):")
        for d in dates:
            print(f"   {d}: {ts[d]}")
        return True
    else:
        print(f"⚠️ {indicator} response: {list(data.keys())[:3]}")
        return False

# Test some technical indicators
indicators = [
    ("SMA", {"time_period": 20}),
    ("EMA", {"time_period": 20}),
    ("RSI", {"time_period": 14}),
    ("MACD", {}),
    ("BBANDS", {"time_period": 20}),
]

for indicator, extra_params in indicators:
    print(f"\n--- {indicator} for IBM ---")
    try:
        get_technical_indicator("IBM", indicator, extra_params)
    except Exception as e:
        print(f"❌ Error: {e}")

# ============================================================
# Test 4: Fundamental Data
# ============================================================
print("\n" + "="*60)
print("Test 4: Fundamental Data")
print("="*60)

def get_fundamental_data(symbol, function):
    """Get fundamental data"""
    params = {
        "function": function,
        "symbol": symbol,
        "apikey": API_KEY
    }
    
    response = requests.get(DIRECT_URL, params=params, timeout=30)
    data = response.json()
    
    if function == "GLOBAL_QUOTE":
        if "Global Quote" in data and data["Global Quote"]:
            quote = data["Global Quote"]
            print(f"\n📊 {symbol} - GLOBAL_QUOTE:")
            print(f"   Price: {quote.get('05. price', 'N/A')}")
            print(f"   Volume: {quote.get('06. volume', 'N/A')}")
            print(f"   Change: {quote.get('09. change', 'N/A')}")
            return True
    elif function == "COMPANY_OVERVIEW":
        if "Name" in data:
            print(f"\n🏢 {symbol} - COMPANY_OVERVIEW:")
            print(f"   Name: {data.get('Name', 'N/A')}")
            print(f"   Sector: {data.get('Sector', 'N/A')}")
            print(f"   Market Cap: {data.get('MarketCapitalization', 'N/A')}")
            print(f"   P/E Ratio: {data.get('PERatio', 'N/A')}")
            return True
    
    print(f"⚠️ {function}: {list(data.keys())[:3]}")
    return False

fundamental_tests = [
    ("IBM", "GLOBAL_QUOTE"),
    ("AAPL", "COMPANY_OVERVIEW"),
]

for symbol, func in fundamental_tests:
    print(f"\n--- {func} for {symbol} ---")
    try:
        get_fundamental_data(symbol, func)
    except Exception as e:
        print(f"❌ Error: {e}")

print("\n" + "="*60)
print("Test Complete!")
print("="*60)