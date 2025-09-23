#' moderations Class
moderations <- R6Class(
  "moderations",
  inherit = base_api,
  public = list(
    #' @description Classifies if text violates OpenAI's Content Policy.
    #' @param input character Required. The input text to classify.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ...  Additional parameters as required by the OpenAI API.For example:model...
    #' @return Returns a list of image objects.
    create=function(input,...,verbosity=0){
      option <- list(...)
      option$input <- input
      result<-private$api_call(endpoint = "moderations", headers = list(`Content-Type` = "application/json"),method = "POST",body = option,verbosity=verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    }
  )
)
