library(httr2)
readRenviron('.Renviron')

api_key <- Sys.getenv('ALIYUNCS_API_KEY')

req <- request('https://coding.dashscope.aliyuncs.com/v1/chat/completions') |>
  req_method('POST') |>
  req_headers(
    'Authorization' = paste0('Bearer ', api_key),
    'Content-Type' = 'application/json'
  ) |>
  req_body_json(list(
    model = 'qwen-turbo',
    messages = list(list(role = 'user', content = 'hello'))
  ))

resp <- req_perform(req)
cat('Status:', resp_status(resp), '\n')
cat('Body:', resp_body_string(resp), '\n')
