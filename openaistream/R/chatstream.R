#' DataStream Class
#'
#' A R6 class to manage data streams.
#'
DataStream <- R6::R6Class(
  "DataStream",
  private = list(
    requery = NULL,
    status = NULL,
    iterator = NULL,
    num = 2,
    check = function() {
      if (!inherits(private$requery, "curl")||(Sys.getenv("TEST_EX_COND")=="error chatstream check is not curl")) {
        return("is not curl")
      }
      is_valid <- TRUE
      tryCatch({
        isOpen(private$requery)
        if(Sys.getenv("TEST_EX_COND")=="error chatstream active isOpen"){
          stop(Sys.getenv("TEST_EX_COND"))
        }
        is_valid <- FALSE
      }, error = function(e) {
        is_valid <- TRUE
      })
      if (is_valid) {
        return("httr2_invalid")
      }
      if (summary(private$requery)$opened == "opened") {
        return("httr2_open")
      } else {
        return("httr2_close")
      }
    },
    destroy = function(status="close") {
      try({
        if (!is.null(private$requery) && inherits(private$requery, "curl")) {
          base::close(private$requery)
        }
      },silent = T)
      private$requery <- NULL
      private$status <- status
      private$iterator <- NULL
      private$num <- NULL
      gc()
    },
    data_source = function() {
      private$status <- private$check()
      if (private$status == "httr2_close") {
        tryCatch({
          open(private$requery, "rbf")
          if(Sys.getenv("TEST_EX_COND")=="error chatstream data_source open is fail"){
            stop(Sys.getenv("TEST_EX_COND"))
          }
          private$status = "httr2_open"
        }, error = function(e) {
          private$status <- paste0(private$status, " open is fail")
          return(private$status)
        })
      }
      if (private$status =="httr2_open") {
        #获取流数据endpoint
        ept<-summary(private$requery)$description
        if(grepl(ept,pattern = "api.openai.com/v1/audio/speech")){
          buf <- readBin(private$requery,what = "raw", private$num * 2)
          if(length(buf)==0){
            private$destroy("complete")
          }
          return(buf)
        }else{#else if(grepl(summary(private$requery)$description,pattern = "api.openai.com//v1//threads")){}
          #browser()
          #这部分处理对话链接，使用连接分段,这里在处理run数据时不知为啥会遇到空行
          buf <- readLines(private$requery, private$num * 2)
          lstr <- lapply(buf, function(v) {
            if (nchar(v) < 20) {
              if ("data: [DONE]" == v) {
                private$destroy("complete")
              }
              return("")
            } else {
              return(gsub(v, replacement = "", pattern = "^data: "))
            }
          })
          lstr_cleaned <- lstr[nchar(unlist(lstr)) > 1]
          #browser()
          if(length(lstr_cleaned)==0){
            private$destroy("complete")
            return("complete")
          }
          #browser()
          #print(lstr_cleaned)
          if(grepl(ept,pattern = "api.openai.com/v1/threads")){
            #这里处理run的数据
            vres <- lapply(grep(lstr_cleaned,pattern = "^event: ",value = T), function(v) {
              fromJSON(v)
            })
          }else{
            #这里处理chat数据
            vres <- lapply(lstr_cleaned, function(v) {
              pr = fromJSON(v)
              choices <- pr$choices
              if(length(choices)==0&!is.null(pr$usage)){
                return(data.frame(index="-2",content=paste0("usage:",pr$usage$total_tokens)))
              }else{
                #else if(length(choices)==0){
                #return(data.frame(index="-1",content=""))
                #}
                return(data.frame(index=choices$index[1],choices$delta))
              }
            })
          }
          #lstr_cleaned length is zero complete
          list(all_resp = lstr_cleaned, vres = vres)
        }
      } else {
        return(private$status)
      }
    }
  ),
  public = list(
    #' @description Initialize the DataStream object
    #' @param requery The requery object, usually of class 'curl'
    #' @param num Numeric. Defines the number of lines to read from the data source.
    initialize = function(requery, num = 2) {
      private$requery <- requery
      private$status <- "initialized"
      private$num <- num
      private$iterator <- iter(private$data_source)
    },
    #' @description Close the DataStream
    #'
    #' This method tries to destroy the object, and closes the requery if opened.
    #' @return A character message indicating the status of the close operation.
    close = function(){
      tryCatch({
        private$destroy()
        if(Sys.getenv("TEST_EX_COND")=="error chatstream close"){
          stop(Sys.getenv("TEST_EX_COND"))
        }
        return("close success")
      }, error = function(e) {
        return(e)
      })
    },
    #' @description Get the status of the DataStream
    #'
    #' @return A character string indicating the current status of the DataStream.
    get_state =function(){
      return(private$status)
    }

  ),
  active = list(
    #' @field next_value The next data value from the DataStream or an error message.
    next_value = function() {
      tryCatch({
        if(is.null(private$iterator)){
          return(private$status)
        }
        if(Sys.getenv("TEST_EX_COND")=="error chatstream active next value"){
            stop(Sys.getenv("TEST_EX_COND"))
        }
        nextElem(private$iterator)
      }, error = function(e) {
        return(e)
      })
    }
  )
)



