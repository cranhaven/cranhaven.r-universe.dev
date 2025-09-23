#' An R6 Class Interface to OpenAI API
#'
#' @description Provides methods to interact with OpenAI API including
#'   fetching model details, generating completions, managing files, and more.
#'   Always ensure that the API key is kept private.
#' @examples
#' \donttest{
#' Sys.setenv(OPENAI_KEY="you openai key")
#' aaa <- openai$new(Sys.getenv("OPENAI_KEY"))
#' #if need proxy
#' #aaa$set_proxy("127.0.0.1", 10890)
#' # List model
#' aaa$models$list()
#' }
#' @export
openai <- R6Class(
  "openai",
  private = list(
    etc = NULL,
    models_n=NULL,
    files_n=NULL,
    chat_n=NULL,
    fine_tuning_n=NULL,
    audio_n=NULL,
    embeddings_n=NULL,
    images_n=NULL,
    moderations_n=NULL,
    assistants_n=NULL,
    threads_n=NULL,
    messages_n=NULL,
    runs_n=NULL,
    batch_n=NULL,
    vector_stores_n=NULL,
    act_fun=function(v,nm=NULL){
      if (missing(v)) {
        return(private[[nm]])
      } else {
        stop("This property is read-only.")
      }
    }
  ),
  public = list(
    #' @description Initialize the OpenAI API interface with the provided API key.
    #' @param api_key The OpenAI API key.
    initialize = function(api_key) {
      private$etc = api_config$new()
      private$etc$set_api_key(api_key)
      private$models_n<-models$new(private$etc)
      private$files_n<-files$new(private$etc)
      private$fine_tuning_n<-fine_tuning$new(private$etc)
      private$chat_n<-chat$new(private$etc)
      private$audio_n<-audio$new(private$etc)
      private$embeddings_n<-embeddings$new(private$etc)
      private$images_n<-images$new(private$etc)
      private$moderations_n<-moderations$new(private$etc)
      private$assistants_n<-assistants$new(private$etc)
      private$threads_n<-threads$new(private$etc)
      private$messages_n<-messages$new(private$etc)
      private$runs_n<-runs$new(private$etc)
      private$batch_n<-batch$new(private$etc)
      private$vector_stores_n<-vector_stores$new(private$etc)
    },
    #' @description Configure the proxy settings.
    #' @param proxy_ip character Required. The IP address of the proxy.
    #' @param proxy_port character Required. The port number of the proxy.
    set_proxy = function(proxy_ip,proxy_port){
      if(!grepl("^([0-9]{1,3}\\.){3}[0-9]{1,3}$", proxy_ip)) {
        stop("Invalid proxy IP address.")
      }
      ip_components <- unlist(strsplit(proxy_ip, split = "\\."))
      if(any(as.numeric(ip_components) > 255) || any(as.numeric(ip_components) < 0)) {
        stop("Invalid proxy IP address.")
      }
      if(!is.numeric(proxy_port) || proxy_port < 1 || proxy_port > 65535) {
        stop("Invalid proxy port number.")
      }
      private$etc$set_proxy(proxy_ip,proxy_port)
    }
  ),
  active=list(
    #' @field models class
    models=function(value){
      private$act_fun(value,nm="models_n")
    },
    #' @field files class
    files=function(value){
      private$act_fun(value,nm="files_n")
    },
    #' @field fine_tuning class
    fine_tuning=function(value){
      private$act_fun(value,nm="fine_tuning_n")
    },
    #' @field chat class
    chat=function(value){
      private$act_fun(value,nm="chat_n")
    },
    #' @field audio class
    audio=function(value){
      private$act_fun(value,nm="audio_n")
    },
    #' @field embeddings class
    embeddings=function(value){
      private$act_fun(value,nm="embeddings_n")
    },
    #' @field images class
    images=function(value){
      private$act_fun(value,nm="images_n")
    },
    #' @field moderations class
    moderations=function(value){
      private$act_fun(value,nm="moderations_n")
    },
    #' @field assistants class
    assistants=function(value){
      private$act_fun(value,nm="assistants_n")
    },
    #' @field threads class
    threads=function(value){
      private$act_fun(value,nm="threads_n")
    },
    #' @field messages class
    messages=function(value){
      private$act_fun(value,nm="messages_n")
    },
    #' @field runs class
    runs=function(value){
      private$act_fun(value,nm="runs_n")
    },
    #' @field batch class
    batch=function(value){
      private$act_fun(value,nm="batch_n")
    },
    #' @field vector_stores class
    vector_stores=function(value){
      private$act_fun(value,nm="vector_stores_n")
    }
  )
)
