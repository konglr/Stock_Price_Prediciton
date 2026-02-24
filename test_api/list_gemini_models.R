library(httr2)
library(jsonlite)

# Load .Renviron
readRenviron(".Renviron")
apiKey <- Sys.getenv("GEMINI_API_KEY")

if (apiKey == "") {
  stop("GEMINI_API_KEY not found in .Renviron")
}

api_url <- "https://generativelanguage.googleapis.com/v1beta/models"

cat("Listing models...\n")
tryCatch({
  resp <- request(api_url) %>%
    req_url_query(key = apiKey) %>%
    req_method("GET") %>%
    req_perform()
  
  result <- resp_body_json(resp)
  models <- sapply(result$models, function(m) m$name)
  print(models)
}, error = function(e) {
  cat("Error listing models:", e$message, "\n")
  if (!is.null(e$response)) {
    print(resp_body_string(e$response))
  }
})
