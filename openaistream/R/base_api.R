#' base_api Class
base_api <- R6Class(
  "openai",
  private = list(
    etc = NULL,
    #base_call
    base_call = function(endpoint, path="", body=NULL, method="GET", headers=list(), query=list()){
      url <- private$etc$get_api_endpoints()[[endpoint]]
      req <- request(url) %>%
        req_headers(Authorization = paste0("Bearer ",private$etc$get_api_key()),!!!headers) %>%
        req_method(method)
      if (path != "") {
        req <- req %>% req_url_path_append(strsplit(path, "/")[[1]])
      }
      if (length(query) > 0) {
        req <- req %>%req_url_query(!!!query)
      }

      if(length(private$etc$get_proxy()) == 2) {
        req <- req %>%
          req_proxy(private$etc$get_proxy()$ip, private$etc$get_proxy()$port)
      }
      req
    },
    #get and post and delete call
    api_call = function(endpoint, path="", body=NULL, method="GET", headers=list(), query=list(), verbosity=0,fpath=NULL) {
      req<-private$base_call(endpoint, path=path, method=method, headers=headers, query=query)
      if (!is.null(body)) {
        req <- req %>%req_body_json(body)
      }
      tryCatch({
        parsed <- req %>% req_perform(verbosity = verbosity,path=fpath)
        if(path=="/speech"){
          return(list(success=TRUE, data=parsed$body))
        }else if(grepl(x = path,pattern = ".*content$")&endpoint=="files"){
          return(list(success=TRUE, data=rawToChar(parsed$body)))
        }else{
          return(list(success=TRUE, data=fromJSON(rawToChar(parsed$body))))
        }
      }, error=function(e) {
        return(openai_error$new(as.character(e)))
      })
    },
    #stream call
    handle_call=function(endpoint, path="", body=NULL, headers=list(), query=list()){
      req<-private$base_call(endpoint, path=path, method="POST", headers=headers, query=query)
      req <- req %>% req_body_json(body)
      return(curl::curl(req$url, handle = httr2:::req_handle(req)))
    },
    #file call
    file_call=function(endpoint, path="", body=NULL, headers=list(), query=list(), verbosity=0){
      req<-private$base_call(endpoint, path=path, method="POST", headers=headers, query=query)
      req<-req %>% req_body_multipart(!!!body)
      tryCatch({
        parsed <- req %>% req_perform(verbosity = verbosity)
        return(list(success=TRUE, data=fromJSON(rawToChar(parsed$body))))
      }, error=function(e) {
        return(openai_error$new(as.character(e)))
      })
    },
    check_path=function(path){
      if(is.null(path)){
        return(list(success=FALSE, data="path is NULL"))
      }else if(!file.exists(path)){
        return(list(success=FALSE, data=paste0("path (",path,") is not exists")))
      }else{
        return(list(success=T))
      }
    }
  ),
  public = list(
    #' @description Initialize the OpenAI API interface with the provided API key and other.
    #' @param etc Config.
    initialize = function(etc) {
      private$etc<-etc
    }
  )
)
