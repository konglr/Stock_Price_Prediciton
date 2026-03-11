library(httr2)
readRenviron('.Renviron')

api_url <- 'https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent'
api_key <- Sys.getenv('GEMINI_API_KEY')

cat("API Key loaded:", substring(api_key, 1, 20), "...\n")

req <- request(paste0(api_url, '?key=', api_key)) |>
  req_method('POST') |>
  req_headers('Content-Type' = 'application/json') |>
  req_body_json(list(
    tools = list(list(google_search = list())),
    contents = list(list(parts = list(list(text = 'What is AAPL price today?'))))
  ))

cat("Sending request with google_search...\n")
resp <- req_perform(req)
cat('Status:', resp_status(resp), '\n')

result <- resp_body_json(resp)
if (!is.null(result$candidates)) {
  cat('Response:', substr(result$candidates[[1]]$content$parts[[1]]$text, 1, 300), '\n')
} else {
  print(result)
}
