library(httr2)
readRenviron('.Renviron')

api_key <- Sys.getenv('MINIMAX_API_KEY')
cat("MiniMax API Key loaded:", substring(api_key, 1, 20), "...\n")

api_url <- 'https://api.minimax.chat/v1/text/chatcompletion_v2'

req <- request(api_url) |>
  req_method('POST') |>
  req_headers(
    'Authorization' = paste0('Bearer ', api_key),
    'Content-Type' = 'application/json'
  ) |>
  req_body_json(list(
    model = 'MiniMax-M2.5',
    messages = list(list(
      role = 'user',
      content = 'What is AAPL stock price today?'
    ))
  ))

cat("Sending request to MiniMax...\n")
resp <- req_perform(req)
cat('Status:', resp_status(resp), '\n')

result <- resp_body_json(resp)
if (!is.null(result$choices)) {
  cat('Response:', substr(result$choices[[1]]$message$content, 1, 300), '\n')
} else {
  print(result)
}
