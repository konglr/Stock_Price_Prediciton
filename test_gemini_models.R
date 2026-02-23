library(httr2)
library(jsonlite)

# Load .Renviron
readRenviron(".Renviron")
apiKey <- Sys.getenv("GEMINI_API_KEY")

if (apiKey == "") {
  stop("GEMINI_API_KEY not found in .Renviron")
}
# Common request body
req_body <- list(
  contents = list(
    list(
      role = "user",
      parts = list(list(text = "Hello, what is the current stock price of AAPL?"))
    )
  ),
  tools = list(
    list(google_search = setNames(list(), character(0)))
  )
)

# Test plan for gemini-2.5-flash
model_id <- "gemini-2.5-flash"
api_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_id, ":generateContent")

# 1. Test Grounding Metadata Structure
cat("\n--- Testing Grounding Metadata Structure ---\n")
tryCatch({
  resp <- request(api_url) %>%
    req_url_query(key = apiKey) %>%
    req_method("POST") %>%
    req_body_json(list(
      contents = list(list(role = "user", parts = list(list(text = "What is the current price of AAPL?")))),
      tools = list(list(google_search = setNames(list(), character(0))))
    )) %>%
    req_perform()
  
  result <- resp_body_json(resp)
  metadata <- result$candidates[[1]]$groundingMetadata
  cat("Grounding Metadata present:", !is.null(metadata), "\n")
  if (!is.null(metadata)) {
    print(str(metadata, max.level = 2))
  }
}, error = function(e) cat("Grounding test failed:", e$message, "\n"))

# 2. Test Chat Role ("assistant" vs "model")
cat("\n--- Testing Chat Role 'assistant' (should fail) ---\n")
tryCatch({
  resp <- request(api_url) %>%
    req_url_query(key = apiKey) %>%
    req_method("POST") %>%
    req_body_json(list(
      contents = list(
        list(role = "user", parts = list(list(text = "Hello"))),
        list(role = "assistant", parts = list(list(text = "Hi there!")))
      )
    )) %>%
    req_perform()
  cat("Success with 'assistant' role? (Unexpected)\n")
}, error = function(e) cat("Expected failure with 'assistant' role:", e$message, "\n"))

cat("\n--- Testing Chat Role 'model' (should succeed) ---\n")
tryCatch({
  resp <- request(api_url) %>%
    req_url_query(key = apiKey) %>%
    req_method("POST") %>%
    req_body_json(list(
      contents = list(
        list(role = "user", parts = list(list(text = "Hello"))),
        list(role = "model", parts = list(list(text = "Hi there!"))),
        list(role = "user", parts = list(list(text = "How are you?")))
      )
    )) %>%
    req_perform()
  cat("Success with 'model' role!\n")
}, error = function(e) cat("Failure with 'model' role:", e$message, "\n"))

test_config <- function(name, body) {
  cat("\n--- Testing:", name, "---\n")
  tryCatch({
    resp <- request(api_url) %>%
      req_url_query(key = apiKey) %>%
      req_method("POST") %>%
      req_body_json(body) %>%
      req_perform()
    
    result <- resp_body_json(resp)
    cat("Success!\n")
    if (!is.null(result$candidates[[1]]$content$parts[[1]]$text)) {
        print(substr(result$candidates[[1]]$content$parts[[1]]$text, 1, 100))
    } else {
        cat("Received response but no text content. Check candidates.\n")
        print(result$candidates[[1]]$finishReason)
    }
  }, error = function(e) {
    cat("Failed:", e$message, "\n")
    if (!is.null(e$response)) {
      cat("Response body:", resp_body_string(e$response), "\n")
    }
  })
}

# 1. Bare minimum
test_config("Bare Minimum", list(
  contents = list(list(role = "user", parts = list(list(text = "Hi"))))
))

# 2. System Instruction only
test_config("System Instruction Only", list(
  contents = list(list(role = "user", parts = list(list(text = "Hi")))),
  systemInstruction = list(parts = list(list(text = "Be brief.")))
))

# 3. Google Search Tool only
test_config("Google Search Tool Only", list(
  contents = list(list(role = "user", parts = list(list(text = "AAPL price")))),
  tools = list(list(google_search = setNames(list(), character(0))))
))

# 4. Full complex request (like in app.R)
test_config("Full Request (System + Tools)", list(
  contents = list(list(role = "user", parts = list(list(text = "AAPL analysis")))),
  systemInstruction = list(parts = list(list(text = "You are a financial expert."))),
  tools = list(list(google_search = setNames(list(), character(0)))),
  generationConfig = list(temperature = 0.7, maxOutputTokens = 1000)
))
