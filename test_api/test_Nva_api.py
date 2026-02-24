from openai import OpenAI

client = OpenAI(
  base_url = "https://integrate.api.nvidia.com/v1",
  api_key = "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"
)

completion = client.chat.completions.create(
  #model="qwen/qwen2.5-coder-32b-instruct",
  model="minimaxai/minimax-m2.1",

  messages=[{"role":"user","content":"你好，请介绍一下你自己"}],
  temperature=0.2,
  top_p=0.7,
  max_tokens=1024,
  stream=False
)

print(completion.choices[0].message)
