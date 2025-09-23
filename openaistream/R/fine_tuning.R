#' fine_tuning Class
fine_tuning <- R6Class(
  "fine_tuning",
  inherit = base_api,
  public = list(
    #' @description Creates a job that fine-tunes a specified model from a given dataset.
    #'              Response includes details of the enqueued job including job status and
    #'              the name of the fine-tuned models once complete.
    #' @param model character Required. The model ID.
    #' @param training_file character Required. The file used for training.
    #' @param hyperparameters list. The hyperparameters used for the fine-tuning job. include batch_size;learning_rate_multiplier;n_epochs.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ... Additional parameters as required by the OpenAI API.For example:validation_file......
    #' @return Response indicating the success or failure of the fine-tuning job creation.
    create=function(model,training_file,hyperparameters=list(n_epochs=1),...,verbosity=0){
      option <- list(...)
      option$model <- model
      option$training_file <- training_file
      option$hyperparameters <- hyperparameters
      result <- private$api_call("fine_tuning_jobs", body = option, method = "POST", headers = list(`Content-Type` = "application/json"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description List your organization's fine-tuning jobs
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ... Additional parameters as required by the OpenAI API. For example:after,limit.
    #' @return A list of paginated fine-tuning job objects.
    list=function(verbosity=0,...){
      option = list(...)
      result <- private$api_call("fine_tuning_jobs", query = option, verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Get info about a fine-tuning job.
    #' @param job_id character Required. The ID of the fine-tuning job.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return The fine-tuning object with the given ID.
    retrieve=function(job_id,verbosity=0){
      result <- private$api_call("fine_tuning_jobs", paste0("/", job_id), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Immediately cancel a fine-tune job.
    #' @param job_id character Required. The ID of the fine-tuning job to cancel.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return The cancelled fine-tuning object.
    cancel=function(job_id,verbosity=0){
      result <- private$api_call("fine_tuning_jobs", paste0("/", job_id, "/", "cancel"), method = "POST", verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Get status updates for a fine-tuning job.
    #' @param job_id character Required. The ID of the fine-tuning job to get events for.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ... Additional parameters as required by the OpenAI API. For example:after,limit.
    #' @return A list of fine-tuning event objects.
    events=function(job_id,...,verbosity=0){
      option = list(...)
      result <- private$api_call("fine_tuning_jobs", query = option, paste0("/", job_id, "/", "events"),method = "GET", verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description List checkpoints for a fine-tuning job.
    #' @param job_id character Required. The ID of the fine-tuning job to get checkpoints for.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ... Additional parameters as required by the OpenAI API. For example:after,limit.
    #' @return A list of fine-tuning checkpoint objects for a fine-tuning job.
    checkpoints=function(job_id,...,verbosity=0){
      option = list(...)
      result <- private$api_call("fine_tuning_jobs", query = option, paste0("/", job_id, "/", "checkpoints"),method = "GET", verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    }
  )
)
