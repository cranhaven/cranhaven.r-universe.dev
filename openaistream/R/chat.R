#' chat Class
chat <- R6Class(
  "chat",
  inherit = base_api,
  public = list(
    #' @description Creates a model response for the given chat conversation.
    #' @param messages list Required. A list of messages comprising the conversation so far.
    #' @param model character Required. The model to use for generating chat completions.
    #' @param stream logical. Whether to stream back partial progress. If set, tokens will be sent
    #'              as data-only server-sent events as they become available.
    #' @param n integer. How many chat completion choices to generate for each input message. Note that you will be
    #'           charged based on the number of generated tokens across all of the choices. Keep n as 1 to minimize costs.
    #'           NOTE: The parameter sometimes fails to work when 'num' is not assigned a value, and the reason for this
    #'           is currently unclear. When failure occurs, try assigning any integer greater than 0 to the 'num' parameter.
    #' @param num The num parameter controls the number of text entries returned by a stream in one go.
    #'            Note that this is different from the n parameter, which specifies the number of results returned.
    #'            For detailed information on the n parameter, please refer to OpenAI's API documentation.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ... Additional parameters as required by the OpenAI API.For example:max_tokens;n;stop;temperature......
    #' @return Returns a chat completion object, or a streamed sequence of chat completion chunk objects if the request is streamed.
    create=function(messages,model,stream=F,n=1,num=2,verbosity = 0,...){
      option = list(...)
      option$messages = messages
      option$model = model
      option$stream = stream
      option$n = n
      if (option$stream) {
        handle <- private$handle_call("chat_completions", body=option, headers=list(Accept="text/event-stream", `Content-Type` = "application/json"))
        return(DataStream$new(requery = handle , num = num))
      } else {
        result <- private$api_call("chat_completions", body=option, headers=list(`Content-Type` = "application/json"),method = "POST", verbosity=verbosity)
        if (inherits(result, "openai_error")) {
          return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
        }else{
          return(list(all_resp=result$data, vres = result$data$choices))
        }
      }
    }
  )
)
