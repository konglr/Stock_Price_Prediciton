import os
from anthropic import Anthropic

#os.environ["MINIMAX_API_KEY"] =
api_key ="sk-cp-mFLXPsKWQ8p5D-DnWfRAtzrU5-IXwlMnFAXK6EA7IgWLzZjDLXKFJzwyWUk6Q-cjwpMAwhYZZOhm1CGugzllLvBnWGEVU30_SVeUz0PJbZgtRi5GVVrMYeY" 

if not api_key:
    raise ValueError("未设置 MINIMAX_API_KEY 环境变量")

client = Anthropic(
    api_key=api_key,
    base_url="https://api.minimaxi.com/anthropic"
)

message = client.messages.create(
    model="MiniMax-M2.5",
    max_tokens=1000,
    system="You are a helpful assistant.",
    messages=[
        {
            "role": "user",
            "content": [
                {
                    "type": "text",
                    "text": "你好，请介绍一下你自己。你的MiniMax版本是什么？"
                }
            ]
        }
    ]
)
# 解析并打印响应
for block in message.content:
    if hasattr(block, "thinking"):
        print(f"Thinking:\n{block.thinking}\n")
    elif hasattr(block, "text"):
        print(f"Text:\n{block.text}\n")

# 返回完整消