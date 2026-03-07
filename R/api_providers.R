# ==============================================================================
# StockAI - AI API 统一接口模块
# ==============================================================================
# 本文件提供统一的 AI API 调用接口，支持多个提供商
# 支持的提供商: Google Gemini, MiniMax, 阿里云 DashScope
# ==============================================================================

# ------------------------------------------------------------------------------
# 内部函数: 调用 Gemini API
# ------------------------------------------------------------------------------
.call_gemini <- function(prompt, model_id, temperature, max_tokens, api_key, enable_search = FALSE, system_prompt = NULL) {
  api_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_id, ":generateContent")
  
  # 构建请求体
  req_body <- list(
    contents = list(list(parts = list(list(text = prompt)))),
    generationConfig = list(
      temperature = temperature,
      maxOutputTokens = max_tokens
    )
  )
  
  # 添加系统指令
  if (!is.null(system_prompt)) {
    req_body$systemInstruction <- list(parts = list(list(text = system_prompt)))
  }
  
  # 启用联网搜索
  if (enable_search) {
    req_body$tools <- list(list(google_search = setNames(list(), character(0))))
  }
  
  resp <- httr2::request(api_url) |>
    httr2::req_url_query(key = api_key) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(req_body) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()
  
  result <- httr2::resp_body_json(resp)
  
  list(
    text = result$candidates[[1]]$content$parts[[1]]$text,
    grounding = result$candidates[[1]]$groundingMetadata
  )
}

# ------------------------------------------------------------------------------
# 内部函数: 调用 Gemini API (对话模式)
# ------------------------------------------------------------------------------
.call_gemini_chat <- function(messages, model_id, temperature, max_tokens, api_key, enable_search = FALSE, system_prompt = NULL) {
  api_url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_id, ":generateContent")
  
  # 转换消息格式 (Gemini 使用 "model" 而不是 "assistant")
  msg_contents <- lapply(messages, function(m) {
    role_map <- c("user" = "user", "assistant" = "model")
    list(role = role_map[m$role], parts = list(list(text = m$content)))
  })
  
  req_body <- list(
    contents = msg_contents,
    generationConfig = list(
      temperature = temperature,
      maxOutputTokens = max_tokens
    )
  )
  
  if (!is.null(system_prompt)) {
    req_body$systemInstruction <- list(parts = list(list(text = system_prompt)))
  }
  
  if (enable_search) {
    req_body$tools <- list(list(google_search = setNames(list(), character(0))))
  }
  
  resp <- httr2::request(api_url) |>
    httr2::req_url_query(key = api_key) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(req_body) |>
    httr2::req_perform()
  
  result <- httr2::resp_body_json(resp)
  
  list(
    text = result$candidates[[1]]$content$parts[[1]]$text,
    grounding = result$candidates[[1]]$groundingMetadata
  )
}

# ------------------------------------------------------------------------------
# 内部函数: 调用 MiniMax API
# ------------------------------------------------------------------------------
.call_minimax <- function(prompt, model_id, temperature, max_tokens, api_key, system_prompt = NULL) {
  api_url <- "https://api.minimax.chat/v1/text/chatcompletion_v2"
  
  msgs <- list()
  if (!is.null(system_prompt)) {
    msgs[[1]] <- list(role = "system", content = system_prompt)
  }
  msgs[[length(msgs) + 1]] <- list(role = "user", content = prompt)
  
  resp <- httr2::request(api_url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = paste0("Bearer ", api_key),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model_id,
      messages = msgs,
      temperature = temperature,
      max_tokens = max_tokens
    )) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()
  
  result <- httr2::resp_body_json(resp)
  
  list(
    text = result$choices[[1]]$message$content,
    grounding = NULL
  )
}

# ------------------------------------------------------------------------------
# 内部函数: 调用 MiniMax API (对话模式)
# ------------------------------------------------------------------------------
.call_minimax_chat <- function(messages, model_id, temperature, max_tokens, api_key, system_prompt = NULL) {
  api_url <- "https://api.minimax.chat/v1/text/chatcompletion_v2"
  
  msgs <- list()
  if (!is.null(system_prompt)) {
    msgs[[1]] <- list(role = "system", content = system_prompt)
  }
  for (m in messages) {
    msgs[[length(msgs) + 1]] <- list(role = m$role, content = m$content)
  }
  
  resp <- httr2::request(api_url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = paste0("Bearer ", api_key),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model_id,
      messages = msgs,
      temperature = temperature,
      max_tokens = max_tokens
    )) |>
    httr2::req_perform()
  
  result <- httr2::resp_body_json(resp)
  
  list(
    text = result$choices[[1]]$message$content,
    grounding = NULL
  )
}

# ------------------------------------------------------------------------------
# 内部函数: 调用阿里云 DashScope API
# ------------------------------------------------------------------------------
.call_aliyun <- function(prompt, model_id, temperature, max_tokens, api_key, system_prompt = NULL) {
  api_url <- "https://coding.dashscope.aliyuncs.com/v1/chat/completions"
  
  msgs <- list()
  if (!is.null(system_prompt)) {
    msgs[[1]] <- list(role = "system", content = system_prompt)
  }
  msgs[[length(msgs) + 1]] <- list(role = "user", content = prompt)
  
  resp <- httr2::request(api_url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model_id,
      messages = msgs,
      temperature = temperature,
      max_tokens = max_tokens
    )) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()
  
  result <- httr2::resp_body_json(resp)
  
  list(
    text = result$choices[[1]]$message$content,
    grounding = NULL
  )
}

# ------------------------------------------------------------------------------
# 内部函数: 调用阿里云 DashScope API (对话模式)
# ------------------------------------------------------------------------------
.call_aliyun_chat <- function(messages, model_id, temperature, max_tokens, api_key, system_prompt = NULL) {
  api_url <- "https://coding.dashscope.aliyuncs.com/v1/chat/completions"
  
  msgs <- list()
  if (!is.null(system_prompt)) {
    msgs[[1]] <- list(role = "system", content = system_prompt)
  }
  for (m in messages) {
    msgs[[length(msgs) + 1]] <- list(role = m$role, content = m$content)
  }
  
  resp <- httr2::request(api_url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model_id,
      messages = msgs,
      temperature = temperature,
      max_tokens = max_tokens
    )) |>
    httr2::req_perform()
  
  result <- httr2::resp_body_json(resp)
  
  list(
    text = result$choices[[1]]$message$content,
    grounding = NULL
  )
}

# ------------------------------------------------------------------------------
# 统一 API 调用接口 (单次提示)
# ------------------------------------------------------------------------------
#' 调用 AI API (单次提示模式)
#' @param provider 提供商 (gemini, minimax, aliyun)
#' @param model_id 模型 ID
#' @param prompt 提示文本
#' @param temperature 温度参数
#' @param max_tokens 最大 token 数
#' @param enable_search 是否启用联网搜索 (仅 Gemini 支持)
#' @param system_prompt 系统提示
#' @return 包含 text 和 grounding 的列表
call_ai_api <- function(provider, model_id, prompt, temperature = 0.7, max_tokens = 1024, 
                        enable_search = FALSE, system_prompt = NULL) {
  
  # 获取 API Key
  api_key <- switch(provider,
    "gemini" = Sys.getenv("GEMINI_API_KEY"),
    "minimax" = Sys.getenv("MINIMAX_API_KEY"),
    "aliyun" = Sys.getenv("ALIYUNCS_API_KEY"),
    stop("Unknown provider: ", provider)
  )
  
  if (api_key == "") {
    stop("API key not found for provider: ", provider)
  }
  
  tryCatch({
    if (provider == "gemini") {
      .call_gemini(prompt, model_id, temperature, max_tokens, api_key, enable_search, system_prompt)
    } else if (provider == "minimax") {
      .call_minimax(prompt, model_id, temperature, max_tokens, api_key, system_prompt)
    } else if (provider == "aliyun") {
      .call_aliyun(prompt, model_id, temperature, max_tokens, api_key, system_prompt)
    }
  }, error = function(e) {
    list(text = paste("API 调用失败:", e$message), grounding = NULL)
  })
}

# ------------------------------------------------------------------------------
# 统一 API 调用接口 (对话模式)
# ------------------------------------------------------------------------------
#' 调用 AI API (对话模式)
#' @param provider 提供商 (gemini, minimax, aliyun)
#' @param model_id 模型 ID
#' @param messages 消息列表 (每项包含 role 和 content)
#' @param temperature 温度参数
#' @param max_tokens 最大 token 数
#' @param enable_search 是否启用联网搜索 (仅 Gemini 支持)
#' @param system_prompt 系统提示
#' @return 包含 text 和 grounding 的列表
call_ai_api_chat <- function(provider, model_id, messages, temperature = 0.7, max_tokens = 1024,
                             enable_search = FALSE, system_prompt = NULL) {
  
  # 获取 API Key
  api_key <- switch(provider,
    "gemini"  = Sys.getenv("GEMINI_API_KEY"),
    "minimax" = Sys.getenv("MINIMAX_API_KEY"),
    "aliyun"  = Sys.getenv("ALIYUNCS_API_KEY"),
    stop("Unknown provider: ", provider)
  )
  
  if (api_key == "") {
    stop("API key not found for provider: ", provider)
  }
  
  tryCatch({
    if (provider == "gemini") {
      .call_gemini_chat(messages, model_id, temperature, max_tokens, api_key, enable_search, system_prompt)
    } else if (provider == "minimax") {
      .call_minimax_chat(messages, model_id, temperature, max_tokens, api_key, system_prompt)
    } else if (provider == "aliyun") {
      .call_aliyun_chat(messages, model_id, temperature, max_tokens, api_key, system_prompt)
    }
  }, error = function(e) {
    list(text = paste("API 调用失败:", e$message), grounding = NULL)
  })
}