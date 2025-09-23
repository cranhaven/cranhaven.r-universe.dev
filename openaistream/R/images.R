#' images Class
images <- R6Class(
  "images",
  inherit = base_api,
  public = list(
    #' @description Creates an image given a prompt.
    #' @param prompt character Required. A text description of the desired image(s).
    #'               The maximum length is 1000 characters for dall-e-2 and 4000 characters for dall-e-3
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ...  Additional parameters as required by the OpenAI API.For example:n;quality;response_format...
    #' @return Returns a list of image objects.
    create=function(prompt,...,verbosity=0){
      option <- list(...)
      option$prompt <- prompt
      result<-private$api_call(endpoint = "images",path = "/generations", headers = list(`Content-Type` = "application/json"),method = "POST",body = option,verbosity=verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Creates an edited or extended image given an original image and a prompt.
    #' @param image character Required. The image to edit. Must be a valid PNG file, less than 4MB,
    #'              and square. If mask is not provided, image must have transparency, which will be used as the mask.
    #' @param prompt character Required. A text description of the desired image(s). The maximum length is 1000 characters.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ...  Additional parameters as required by the OpenAI API.For example:mask;model;n...
    #' @return Returns a list of image objects.
    edit=function(image,prompt,...,verbosity=0){
      ff<-private$check_path(image)
      if(!ff$success){return(ff)}
      option <- list(...)
      option$prompt <- prompt
      option$image<-curl::form_file(image)
      if(!is.null(option$mask)){
        ff<-private$check_path(option$mask)
        if(!ff$success){return(ff)}
        option$mask<-curl::form_file(option$mask)
      }
      result<-private$file_call(endpoint = "images",path = "/edits",body = option,verbosity=verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Creates a variation of a given image.
    #' @param image character Required. The image to use as the basis for the variation(s). Must be a valid PNG file, less than 4MB, and square.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ...  Additional parameters as required by the OpenAI API.For example:model;n;response_format
    #' @return Returns a list of image objects.
    variation=function(image,...,verbosity=0){
      ff<-private$check_path(image)
      if(!ff$success){return(ff)}
      option <- list(...)
      option$image<-curl::form_file(image)
      result<-private$file_call(endpoint = "images",path = "/variations",body = option,verbosity=verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    }
  )
)
