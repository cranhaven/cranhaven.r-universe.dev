#' Files Class
files <- R6Class(
  "files",
  inherit = base_api,
  public = list(
    #' @description Upload a file that can be used across various endpoints. The size of all the
    #'              files uploaded by one organization can be up to 100 GB.The size of individual
    #'              files can be a maximum of 512 MB or 2 million tokens for Assistants.
    #'              See the Assistants Tools guide to learn more about the types of files supported.
    #'              The Fine-tuning API only supports .jsonl files.
    #' @param path character Required. Path to the file that needs to be uploaded.
    #' @param purpose The intended purpose of the uploaded file.
    #'                Use "fine-tune" for Fine-tuning and "assistants" for Assistants and Messages.
    #'                This allows us to validate the format of the uploaded file is correct for fine-tuning.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return The uploaded File object.
    upload=function(path=NULL,verbosity=0,purpose = "fine-tune"){
      ff<-private$check_path(path)
      if(!ff$success){return(ff)}
      result<-private$file_call(endpoint = "files",body = list(file = curl::form_file(path),purpose=purpose),verbosity=verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Returns a list of files that belong to the user's organization.
    #' @param ... Additional parameters as required by the OpenAI API.For example:purpose
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return A list of File objects.
    list=function(...,verbosity=0){
      option <- list(...)
      result <- private$api_call("files",query = option, verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Delete a file.
    #' @param file_id character Required. The ID of the file to use for this request.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return Deletion status.
    delete=function(file_id,verbosity=0){
      result <- private$api_call("files", paste0("/", file_id), method = "DELETE", verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Returns information about a specific file.
    #' @param file_id character Required. The ID of the file to retrieve details for.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return The File object matching the specified ID.
    retrieve=function(file_id,verbosity=0){
      result <- private$api_call("files", paste0("/", file_id), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Returns information about a specific file content.
    #' @param file_id character Required. The ID of the file to retrieve details for.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return The File object matching the specified ID.
    retrieve_content=function(file_id,verbosity=0){
      result <- private$api_call("files", paste0("/", file_id,"/content"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    }
  )
)
