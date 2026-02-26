"""
使用 OpenAI SDK 调用 MiniMax API
MiniMax 兼容 OpenAI API 格式
"""
import os
from openai import OpenAI

# --- 1. 手动读取并解析 .Renviron 文件 ---
def load_renviron(path=".Renviron"):
    if not os.path.exists(path):
        # 尝试在当前工作目录查找
        path = os.path.join(os.getcwd(), ".Renviron")
        if not os.path.exists(path):
            print(f"⚠️ 未找到 .Renviron 文件")
            return
    
    print(f"📂 正在读取：{path}")
    with open(path, 'r', encoding='utf-8') as f:
        for line in f:
            line = line.strip()
            # 跳过空行和注释
            if not line or line.startswith('#'):
                continue
            # 简单的 KEY=VALUE 解析
            if '=' in line:
                key, value = line.split('=', 1)
                key = key.strip()
                value = value.strip()
                # 去除值两边的引号 (如果有的话)
                if (value.startswith('"') and value.endswith('"')) or \
                   (value.startswith("'") and value.endswith("'")):
                    value = value[1:-1]
                os.environ[key] = value
    print("✅ .Renviron 加载完成")

# 执行加载
load_renviron()

# --- 2. 获取 API Key ---
api_key = os.getenv("MINIMAX_API_KEY")

if not api_key:
    raise ValueError("❌ 错误：未找到 MINIMAX_API_KEY。请检查 .Renviron 文件内容格式。")

# --- 3. 初始化客户端 ---
# 注意：确保 base_url 以 /v1 结尾，且没有多余的斜杠
base_url = "https://api.minimax.chat/v1"
client = OpenAI(api_key=api_key, base_url=base_url)

print(f"🚀 开始测试 MiniMax API (Base: {base_url})...")

try:
    response = client.chat.completions.create(
        model="MiniMax-M2.5",
        messages=[
            {"role": "system", "content": "你是一个有用的AI助手。"},
            {"role": "user", "content": "你好，请你介绍一下自己。"}
        ],
        max_tokens=50
    )
    
    print("\n=== 响应成功 ===")
    print(f"模型：{response.model}")
    print(f"内容：{response.choices[0].message.content}")

except Exception as e:
    print(f"\n❌ 请求失败：{type(e).__name__}")
    error_msg = str(e)
    print(f"错误详情：{error_msg}")
    
    if "RateLimitError" in error_msg:
        print("💡 提示：配额已用完 (402/429)。")
    elif "AuthenticationError" in error_msg:
        print("💡 提示：API Key 无效 (401)。")
    elif "ConnectionError" in error_msg or "404" in error_msg:
        print("💡 提示：Base URL 错误或网络问题。")