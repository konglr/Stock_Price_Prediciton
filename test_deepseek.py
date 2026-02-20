import os
import sys
from openai import OpenAI


def read_renviron(filepath=".Renviron"):
    """读取 .Renviron 文件并返回环境变量字典"""
    env_vars = {}
    if os.path.exists(filepath):
        with open(filepath, 'r') as f:
            for line in f:
                line = line.strip()
                # 跳过注释和空行
                if line and not line.startswith('#') and '=' in line:
                    key, value = line.split('=', 1)
                    env_vars[key.strip()] = value.strip()
    return env_vars


# 加载 .Renviron 文件中的环境变量
renviron_vars = read_renviron()
if 'SILICONFLOW_API_KEY' in renviron_vars:
    os.environ['SILICONFLOW_API_KEY'] = renviron_vars['SILICONFLOW_API_KEY']

# 从环境变量读取 API Key
api_key = os.environ.get('SILICONFLOW_API_KEY')

if not api_key:
    print("请设置 SILICONFLOW_API_KEY 环境变量")
    print("例如: export SILICONFLOW_API_KEY='your_api_key'")
    sys.exit(1)

client = OpenAI(
    api_key=api_key,
    base_url="https://api.siliconflow.cn/v1")

response = client.chat.completions.create(
    model="deepseek-ai/DeepSeek-R1",
    messages=[
        {"role": "system", "content": "You are a helpful assistant"},
        {"role": "user", "content": "Hello"},
    ],
    stream=False
)

