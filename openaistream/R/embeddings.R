#' embeddings Class
embeddings <- R6Class(
  "embeddings",
  inherit = base_api,
  public = list(
    #' @description Creates an embedding vector representing the input text.
    #' @param model character Required. ID of the model to use. You can use
    #'              the List models API to see all of your available models,
    #'              or see our Model overview for descriptions of them.
    #' @param input character Required. Input text to embed, encoded as a string or array of tokens.
    #'              To embed multiple inputs in a single request, pass an array of strings or array of token arrays.
    #'              The input must not exceed the max input tokens for
    #'              the model (8192 tokens for text-embedding-ada-002), cannot be an empty string,
    #'              and any array must be 2048 dimensions or less.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ... Additional parameters as required by the OpenAI API.For example:encoding_format;user....
    #' @return Embeddings for the input data.
    create=function(model,input,...,verbosity=0){
      option <- list(...)
      option$model <- model
      option$input <- input
      result <- private$api_call("embeddings", body = option,method = "POST", headers = list(`Content-Type` = "application/json"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    }
  )
)
