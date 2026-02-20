"""
使用 OpenAI SDK 调用 MiniMax API
MiniMax 兼容 OpenAI API 格式
"""

from openai import OpenAI

# MiniMax API 配置
api_key = "sk-cp-mFLXPsKWQ8p5D-DnWfRAtzrU5-IXwlMnFAXK6EA7IgWLzZjDLXKFJzwyWUk6Q-cjwpMAwhYZZOhm1CGugzllLvBnWGEVU30_SVeUz0PJbZgtRi5GVVrMYeY"

client = OpenAI(
    api_key=api_key,
    base_url="https://api.minimax.chat/v1"
)

# 使用 Chat Completions API
response = client.chat.completions.create(
    model="MiniMax-M2.1",  # 或 MiniMax-M2.5
    messages=[
        {"role": "system", "content": "你是一个有用的AI助手。"},
        {"role": "user", "content": "你好，请介绍一下你自己。你的MiniMax版本是什么？"}
    ],
    max_tokens=1000,
    temperature=0.7
)

# 打印响应
print("=== 响应内容 ===")
print(f"模型: {response.model}")
print(f"选择: {response.choices[0].message.content}")

# 如果需要查看原始响应
print("\n=== 完整响应 ===")
print(response.model_dump_json(indent=2))
