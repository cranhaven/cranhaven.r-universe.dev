#' Batch Class
batch <- R6Class(
  "batch",
  inherit = base_api,
  public = list(
    #' @description Creates and executes a batch from an uploaded file of requests
    #' @param input_file_id character Required. The ID of an uploaded file that contains requests for the new batch.
    #' @param endpoint character Required. The endpoint to be used for all requests in the batch. 
    #'                 Currently /v1/chat/completions, /v1/embeddings, and /v1/completions are supported. 
    #'                 Note that /v1/embeddings batches are also restricted to a maximum of 50,000 
    #'                 embedding inputs across all requests in the batch.
    #' @param completion_window character Required. The time frame within which the batch should be processed.
    #'                          Currently only 24h is supported.                
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ... Additional parameters as required by the OpenAI API.For example:metadata ......
    #' @return The created Batch object.
    create=function(input_file_id,endpoint,completion_window="24h",...,verbosity=0){
      option <- list(...)
      option$input_file_id <- input_file_id
      option$endpoint <- endpoint
      option$completion_window <- completion_window
      result <- private$api_call("batchs", body = option, method = "POST", headers = list(`Content-Type` = "application/json"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description List your organization's batches.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ... Additional parameters as required by the OpenAI API. For example:after,limit.
    #' @return A list of paginated Batch objects.
    list=function(verbosity=0,...){
      option = list(...)
      result <- private$api_call("batchs", query = option, verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Retrieves a batch.
    #' @param batch_id character Required. The ID of the batch to retrieve.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return The Batch object matching the specified ID.
    retrieve=function(batch_id,verbosity=0){
      result <- private$api_call("batchs", paste0("/", batch_id), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Cancels an in-progress batch. The batch will be in status cancelling for up to 10 minutes,
    #'              before changing to cancelled, where it will have partial results (if any) available 
    #'              in the output file.
    #' @param batch_id character Required.The ID of the batch to cancel.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return The Batch object matching the specified ID.
    cancel=function(batch_id,verbosity=0){
      result <- private$api_call("batchs", paste0("/", batch_id, "/", "cancel"), method = "POST", verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    }
  )
)
