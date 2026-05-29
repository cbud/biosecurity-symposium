#------------------------------------------------
# AI Interaction Function
#------------------------------------------------

library(openai)

f_ai_text_analysis <- function(text_vector, model, system_text, user_text) {
  results <- rep(NA, length(text_vector))
  pb <- progress::progress_bar$new(
    format = "[:bar] :current/:total (:percent) Elapsed: :elapsed Remaining: :eta",
    total = length(text_vector), width = 60, clear = FALSE
  )
  
  for (i in seq_along(text_vector)) {
    pb$tick()
    if (is.na(text_vector[i]) || text_vector[i] == "") {
      results[i] <- NA; next
    }
    
    attempt <- 1; max_attempts <- 5; success <- FALSE
    instruction <- paste("You will be provided an abstract from a science publication:", text_vector[i])
    
    while (attempt <= max_attempts && !success) {
      tryCatch({
        result <- create_chat_completion(
          model = model,
          max_tokens = 100,
          messages = list(
            list(role = "system", content = system_text),
            list(role = "assistant", content = instruction),
            list(role = "user", content = user_text)
          )
        )
        results[i] <- result$choices$message.content[[1]]
        success <- TRUE
      }, error = function(e) {
        if (grepl("rate limit", e$message, ignore.case = TRUE)) {
          Sys.sleep(2^attempt); attempt <- attempt + 1
        } else {
          warning(paste("Error at row", i, ":", e$message))
          results[i] <- NA; success <- TRUE
        }
      })
    }
    if (!success) results[i] <- NA
  }
  return(results)
}