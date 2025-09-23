#' vector_stores Class
vector_stores <- R6Class(
  "vector_stores",
  inherit = base_api,
  public = list(
    #' @description Create a vector store.
    #' @param ...  Additional parameters as required by the OpenAI API.For example:file_ids;name;expires_after;metadata
    #' @return A vector store object.
    create=function(...,verbosity=0){
      option <- list(...)
      result <- private$api_call("vector_stores", body = option,method = "POST", headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Retrieves a vector store.
    #' @param vector_store_id character Required. The ID of the vector store to retrieve.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return The vector store object matching the specified ID.
    retrieve=function(vector_store_id,verbosity=0){
      result <- private$api_call("vector_stores",path=paste0("/",vector_store_id),method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Modifies a vector store.
    #' @param vector_store_id character Required. The ID of the vector store to modify.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ...  Additional parameters as required by the OpenAI API.For example:name;expires_after;metadata
    #' @return The modified vector store object.
    modify=function(vector_store_id,...,verbosity=0){
      option <- list(...)
      result <- private$api_call("vector_stores",path=paste0("/",vector_store_id), body = option,method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Delete a vector store.
    #' @param vector_store_id character Required. The ID of the vector store to delete.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @return Deletion status
    delete=function(vector_store_id,verbosity=0){
      result <- private$api_call("vector_stores", paste0("/", vector_store_id), method = "DELETE",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Returns a list of vector stores.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.).
    #' @param ...  Additional parameters as required by the OpenAI API.For example:limit;order;after;before;
    #' @return A list of vector store objects.
    list=function(...,verbosity=0){
      option <- list(...)
      result <- private$api_call("vector_stores",query = option,method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Create a vector store file by attaching a File to a vector store.
    #' @param vector_store_id character Required. The ID of the vector store for which to create a File.
    #' @param file_id character Required. A File ID that the vector store should use. Useful for tools like file_search that can access files.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @param ...  Additional parameters as required by the OpenAI API. For example:file_ids,metadata
    #' @return A vector store file object.
    file_create=function(vector_store_id,file_id,verbosity=0){
      option <- list()
      option$file_id <- file_id
      result<-private$api_call("vector_stores", paste0("/", vector_store_id,"/files"),body = option, method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Retrieves a vector store file.
    #' @param vector_store_id character Required. The ID of the vector store that the file belongs to.
    #' @param file_id character Required. The ID of the file being retrieved.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The vector store file object.
    file_retrieve=function(vector_store_id,file_id,verbosity=0){
      result <- private$api_call("vector_stores", paste0("/", vector_store_id,"/files/",file_id), method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Returns a list of vector store files.
    #' @param vector_store_id character Required. The ID of the vector store that the files belong to.
    #' @param ... Additional parameters as required by the OpenAI API. For example:limit;order;after;before;filter
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return A list of vector store file objects.
    file_list=function(vector_store_id,...,verbosity=0){
      option <- list(...)
      result <- private$api_call("vector_stores", paste0("/", vector_store_id,"/files"),query = option, method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Delete a vector store file. This will remove the file from the vector store
    #'              but the file itself will not be deleted. To delete the file, use the delete file endpoint.
    #' @param vector_store_id character Required. The ID of the vector store that the file belongs to.
    #' @param file_id character Required. The ID of the file to delete.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return Deletion status
    file_delete=function(vector_store_id,file_id,verbosity=0){
      result <- private$api_call("vector_stores", paste0("/", vector_store_id,"/files/",file_id), method = "DELETE",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Create a vector store file batch.
    #' @param vector_store_id character Required. The ID of the vector store for which to create a File Batch.
    #' @param file_ids character Required. A list of File IDs that the vector store should use.
    #'                 Useful for tools like file_search that can access files.
    #' @param ... Additional parameters as required by the OpenAI API. For example:chunking_strategy
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return A vector store file batch object.
    file_batche_create=function(vector_store_id,file_ids,...,verbosity=0){
      option <- list(...)
      option$file_ids <- file_ids
      result<-private$api_call("vector_stores", paste0("/", vector_store_id,"/file_batches"),body = option, method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Retrieves a vector store file batch.
    #' @param vector_store_id character Required. The ID of the vector store that the file batch belongs to.
    #' @param batch_id character Required. The ID of the file batch being retrieved.
    #' @param verbosity numeric. Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The vector store file batch object.
    file_batche_retrieve=function(vector_store_id,batch_id,verbosity=0){
      result <- private$api_call("vector_stores", paste0("/", vector_store_id,"/file_batches/",batch_id), method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Returns a list of vector store files in a batch.
    #' @param vector_store_id character Required. The ID of the vector store that the files belong to.
    #' @param batch_id character Required. The ID of the file batch that the files belong to.
    #' @param ... Additional parameters as required by the OpenAI API. For example:limit;order;after;before;filter
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return A list of vector store file objects.
    file_batche_list=function(vector_store_id,batch_id,...,verbosity=0){
      option <- list(...)
      result <- private$api_call("vector_stores", paste0("/", vector_store_id,"/file_batches/",batch_id,"/files"),query = option, method = "GET",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    },
    #' @description Cancel a vector store file batch. This attempts to cancel the processing of files in this batch as soon as possible.
    #' @param vector_store_id character Required. The ID of the vector store that the file batch belongs to.
    #' @param batch_id character Required. The ID of the file batch to cancel.
    #' @param verbosity numeric Verbosity level for the API call(0:no output;1:show headers;
    #'                  2:show headers and bodies;3: show headers, bodies, and curl status messages.)
    #' @return The modified vector store file batch object.
    file_batche_cancel=function(vector_store_id,batch_id,verbosity=0){
      result <- private$api_call("vector_stores", paste0("/", vector_store_id,"/file_batches/",batch_id,"/cancel"), method = "POST",headers = list(`Content-Type` = "application/json",`OpenAI-Beta`="assistants=v2"), verbosity = verbosity)
      if (inherits(result, "openai_error")) {
        return(list(success=FALSE, message=result$get_message(), type=result$get_type()))
      }else{
        return(result$data)
      }
    }
  )
)