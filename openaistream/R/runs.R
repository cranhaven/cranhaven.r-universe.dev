#' runs Class
runs <- R6Class(
  "runs",
  inherit = base_api,
  public = list(
    #' @description Create a run.
    #' @param thread_id character Required. The ID of the thread to run.
    #' @param assistant_id character Required. The ID of the assistant to use to execute this run.
    #' @param stream boolean or null. If true, returns a stream of events that happen during the Run as server-sent events,
    #'               terminating when the Run enters a terminal state with a data: [DONE] message.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @param ... Additional parameters as required by the OpenAI API.For example:model,instructions,tools,metadata
    #' @return A run object.
    create=function(thread_id,assistant_id,stream=F,num=2,...,verbosity=0){
      option <- list(...)
      option$assistant_id <- assistant_id
      option$stream <- stream
      if (option$stream) {
        handle <- private$handle_call("threads", paste0("/", thread_id,"/runs"), body=option, headers=list(Accept="text/event-stream", `Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"))
        return(DataStream$new(requery = handle , num = num))
      } else {
        result<-private$api_call("threads", paste0("/", thread_id,"/runs"),body = option, method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
        if (inherits(result, "openai_error")) {
          return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
        }else{
          return(result$data)
        }
      }
    },
    #' @description Retrieves a run.
    #' @param thread_id character Required The ID of the thread the run belongs to.
    #' @param run_id character Required The ID of the run to retrieve.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The run object matching the specified ID.
    retrieve=function(thread_id,run_id,verbosity=0){
      result <- private$api_call("threads", paste0("/", thread_id,"/runs/",run_id), method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Modifies a run.
    #' @param thread_id character Required The ID of the thread the run belongs to.
    #' @param run_id character Required The ID of the run to retrieve.
    #' @param ... Additional parameters as required by the OpenAI API.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The modified run object matching the specified ID.
    modify=function(thread_id,run_id,...,verbosity=0){
      option <- list(...)
      result <- private$api_call("threads", paste0("/", thread_id,"/runs/",run_id),body = option, method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Returns a list of runs for a given thread.
    #' @param thread_id character Required The ID of the thread the run belongs to.
    #' @param ... Additional parameters as required by the OpenAI API.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return A list of run objects.
    list=function(thread_id,...,verbosity=0){
      option <- list(...)
      result <- private$api_call("threads", paste0("/", thread_id,"/runs"),query = option, method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description When a run has the status: "requires_action" and required_action.type is submit_tool_outputs,
    #'              this endpoint can be used to submit the outputs from the tool calls once they're all completed.
    #'              All outputs must be submitted in a single request.
    #' @param thread_id character Required The ID of the thread the run belongs to.
    #' @param run_id character Required The ID of the run to retrieve.
    #' @param tool_outputs character Required. A list of tools for which the outputs are being submitted.
    #' @param stream boolean or null. If true, returns a stream of events that happen during the Run as server-sent events,
    #'               terminating when the Run enters a terminal state with a data: [DONE] message.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The modified run object matching the specified ID.
    submit_tool_outputs=function(thread_id,run_id,tool_outputs,stream=F,num=2,verbosity=0){
      option<-list()
      option$tool_outputs=tool_outputs
      option$stream=stream
      if (option$stream) {
        handle <- private$handle_call("threads", paste0("/", thread_id,"/runs/",run_id,"/submit_tool_outputs"), body=option, headers=list(Accept="text/event-stream", `Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"))
        return(DataStream$new(requery = handle , num = num))
      } else {
        result <- private$api_call("threads", paste0("/", thread_id,"/runs/",run_id,"/submit_tool_outputs"),body = option, method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
        if (inherits(result, "openai_error")) {
          return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
        }else{
          return(result$data)
        }
      }
    },
    #' @description Cancels a run that is in_progress.
    #' @param thread_id character Required The ID of the thread the run belongs to.
    #' @param run_id character Required The ID of the run to retrieve.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The modified run object matching the specified ID.
    cancel=function(thread_id,run_id,verbosity=0){
      result <- private$api_call("threads", paste0("/", thread_id,"/runs/",run_id,"/cancel"), method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Create a thread and run it in one request.
    #' @param assistant_id character Required The ID of the assistant to use to execute this run.
    #' @param stream boolean or null. If true, returns a stream of events that happen during the Run as server-sent events,
    #'               terminating when the Run enters a terminal state with a data: [DONE] message.
    #' @param ... Additional parameters as required by the OpenAI API.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return A run object.
    create_tread=function(assistant_id,stream=F,num=2,...,verbosity=0){
      option <- list(...)
      option$assistant_id <- assistant_id
      option$stream<-stream
      if (option$stream) {
        handle <- private$handle_call("threads",path="/runs", body=option, headers=list(Accept="text/event-stream", `Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"))
        return(DataStream$new(requery = handle , num = num))
      } else {
        result<-private$api_call("threads",path="/runs",body = option, method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
        if (inherits(result, "openai_error")) {
          return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
        }else{
          return(result$data)
        }
      }
    },
    #' @description Retrieves a run step.
    #' @param thread_id character Required. The ID of the thread to which the run and run step belongs.
    #' @param run_id character Required. The ID of the run the step belongs to.
    #' @param step_id character Required. The ID of the step to retrieve.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The run step object matching the specified ID.
    steps_retrieve=function(thread_id,run_id,step_id,verbosity=0){
      result <- private$api_call("threads", paste0("/", thread_id,"/runs/",run_id,"/steps/",step_id), method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Returns a list of run steps belonging to a run.
    #' @param thread_id character Required The ID of the thread the run belongs to.
    #' @param run_id character Required The ID of the run the step belongs to.
    #' @param ... Additional parameters as required by the OpenAI API.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return A list of run step objects.
    steps_list=function(thread_id,run_id,...,verbosity=0){
      option <- list(...)
      result <- private$api_call("threads", paste0("/", thread_id,"/runs/",run_id,"/steps"),query = option, method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    }
  )
)
