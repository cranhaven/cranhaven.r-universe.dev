#' Models Class
models <- R6Class(
  "models",
  inherit = base_api,
  public = list(
    #' @description Lists the currently available models, and provides basic information
    #'              about each one such as the owner and availability.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return A list of model objects.
    list=function(verbosity=0){
      result <- private$api_call("models", headers = list(`Content-Type` = "application/json"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data$data)
      }
    },
    #' @description Retrieves a model instance, providing basic information about
    #'              the model such as the owner and permissioning.
    #' @param model character Required. The ID of the model to use for this request
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return The model object matching the specified ID.
    retrieve=function(model,verbosity=0){
      result <- private$api_call("models", paste0("/", model), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Delete a fine-tuned model. You must have the Owner role in
    #'              your organization to delete a model.
    #' @param model character Required. The model to delete
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return Deletion status.
    delete=function(model,verbosity=0){
      result <- private$api_call("models", paste0("/", model), method = "DELETE", verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    }
  )
)
