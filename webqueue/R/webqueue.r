
#' Queues and Services HTTP Requests
#'
#' @name WebQueue
#'
#' @description
#' 
#' Connects the 'httpuv' and 'jobqueue' R packages.
#' 
#' 
#' @param handler  A `function (request)` that will be run on a background 
#'        worker process. The returned value will be passed through 
#'        `reformat`, then sent as the server's response to the web client.
#' 
#' @param host   A string that is a valid IPv4 address that is owned by this 
#'        server, or `'0.0.0.0'` to listen on all IP addresses.
#' 
#' @param port   A number or integer that indicates the server port that should 
#'        be listened on. Note that on most Unix-like systems including Linux 
#'        and macOS, port numbers smaller than 1024 require root privileges.
#' 
#' @param parse  A `function (req)` that is run on the foreground process to 
#'        transform the HTTP request prior to passing it to `handler`. `req` is 
#'        the environment object provided by 'httpuv', amended with `$ARGS` and 
#'        `$COOKIES`. Return value is used as `req` going forward.
#' 
#' @param globals  A list of variables to add to `handler`'s evaluation 
#'        environment.
#'        
#' @param packages  Character vector of package names to load on workers.
#' 
#' @param namespace  The name of a package to attach to the worker's 
#'        environment.
#' 
#' @param init  A call or R expression wrapped in curly braces to evaluate on 
#'        each worker just once, immediately after start-up. Will have access 
#'        to variables defined by `globals` and assets from `packages` and 
#'        `namespace`. Returned value is ignored.
#' 
#' @param max_cpus  Total number of CPU cores that can be reserved by all 
#'        running Jobs (`sum(cpus)`). Does not enforce limits on actual CPU 
#'        utilization.
#' 
#' @param workers  How many background [jobqueue::Worker] processes to start. 
#'        Set to more than `max_cpus` to enable interrupted workers to be 
#'        quickly swapped out with standby Workers while a replacement Worker 
#'        boots up.
#' 
#' @param timeout  A named numeric vector indicating the maximum number of 
#'        seconds allowed for each state the job passes through, or 'total' to
#'        apply a single timeout from 'submitted' to 'done'. Example:
#'        `timeout = c(total = 2.5, running = 1)`.
#' 
#' @param hooks  A list of functions to run when the Job state changes, of the 
#'        form `hooks = list(created = function (job) {...}, done = ~{...})`.
#'        See `vignette('hooks')`.
#' 
#' @param reformat  A `function (job)` that is run in the foreground process to 
#'        transform the output from `handler`. The default, `reformat = NULL`, 
#'        is essentially `function (job) { job$output }`.
#' 
#' @param stop_id  A `function (job)`. If two Jobs generate the same value from
#'        this function, then the earlier Job will be aborted. If the returned 
#'        value is `NULL`, no Jobs will be stopped.
#'                 
#' @param copy_id  A `function (job)`. If two Jobs generate the same value from
#'        this function, then the later Job will clone its output from the 
#'        earlier Job. If the returned value is `NULL`, no Jobs will be cloned.
#' 
#' @param bg   Where/how to run the server. `TRUE`: on a separate R process.
#'        `FALSE`: blocking on the current R process. `NULL`: non-blocking on 
#'        the current R process.
#' 
#' @param quiet   If `TRUE`, suppress error messages from starting the 'httpuv' 
#'        server.
#' 
#' @param onHeaders   A `function (request)` triggered when headers are 
#'        received by 'httpuv'. Return NULL to continue normal processing of 
#'        the request, or a Rook response to send that response, stop 
#'        processing the request, and ask the client to close the connection. 
#'        (This can be used to implement upload size limits, for example.)
#' 
#' @param staticPaths   A named list of paths that will be served without 
#'        invoking `handler()` or `onHeaders()`. The name of each one is the 
#'        URL path, and the value is either a string referring to a local path, 
#'        or an object created by the `httpuv::staticPath()` function.
#' 
#' @param staticPathOptions   A set of default options to use when serving 
#'        static paths. If not set or NULL, then it will use the result from 
#'        calling `httpuv::staticPathOptions()` with no arguments.
#' 
#'
#' @export
#' @examples
#'     
#'     library(webqueue)
#'     
#'     wq <- WebQueue$new(function (req) 'Hello World!\n')
#'     readLines(wq$url)
#'     wq$stop()
#' 

WebQueue <- R6Class(
  classname = "WebQueue",
  cloneable = FALSE,
  
  public = list(
    
    #' @description
    #' Creates an `httpuv::WebServer` with requests handled by a `jobqueue::Queue`.
    #'
    #' @return A `WebQueue` object.
    initialize = function (
        handler,
        host      = '0.0.0.0',
        port      = 8080L,
        parse     = NULL,
        globals   = list(),
        packages  = NULL,
        namespace = NULL,
        init      = NULL,
        max_cpus  = availableCores(),
        workers   = ceiling(max_cpus * 1.2),
        timeout   = NULL,
        hooks     = NULL,
        reformat  = NULL,
        stop_id   = NULL,
        copy_id   = NULL,
        bg        = TRUE,
        quiet             = FALSE,
        onHeaders         = NULL,
        staticPaths       = NULL,
        staticPathOptions = NULL ) {
      
      # Capture curly-brace expression
      init_subst <- substitute(init)
      if (isa(init_subst, '{')) init <- init_subst
      
      # Convert lambda syntax to functions.
      if (is_formula(handler))   handler   <- as_function(handler)
      if (is_formula(parse))     parse     <- as_function(parse)
      if (is_formula(onHeaders)) onHeaders <- as_function(onHeaders)
      
      # Sanity check `handler` and `globals`.
      if (!is.function(handler)) cli_abort('`handler` must be a function, not {.type {handler}}.')
      if (!is.list(globals))     cli_abort('`globals` must be a list, not {.type {globals}}.')
      for (i in c('.wq_handler', '.wq_request'))
        if (hasName(globals, i)) cli_abort('`globals` cannot have a {.field {i}} entry.')
      
      # Custom parsing prior to queue submission.
      if (!(is.function(parse) || is.null(parse)))
        cli_abort('`parse` must be a function or NULL, not {.type {parse}}.')
      
      # Create static paths as needed.
      for (i in seq_along(staticPaths)) {
        staticPaths[[i]] %<>% normalizePath(winslash = '/', mustWork = FALSE)
        fp <- staticPaths[[i]]
        if (!file.exists(fp) && !dir.exists(fp))
          dir.create(fp, recursive = TRUE)
      }
      
      
      # Launch WebQueue on a different R process
      if (isTRUE(bg)) {
        
        worker  <- jobqueue::Worker$new()
        sem     <- create_semaphore()
        start_t <- Sys.time()
        
        job <- jobqueue::Job$new(
          vars = environment(), 
          expr = { # nocov start
            
            # signals an error if unable to start
            webqueue::WebQueue$new(
              handler   = handler,
              host      = host,
              port      = port,
              parse     = parse,
              globals   = globals,
              packages  = packages,
              namespace = namespace,
              init      = init,
              max_cpus  = max_cpus,
              workers   = workers,
              timeout   = timeout,
              hooks     = hooks,
              reformat  = reformat,
              stop_id   = stop_id,
              copy_id   = copy_id,
              bg        = FALSE,
              quiet             = quiet,
              onHeaders         = onHeaders,
              staticPaths       = staticPaths,
              staticPathOptions = staticPathOptions )
            
            semaphore::increment_semaphore(sem)
            
            httpuv::service(timeoutMs = Inf)
            
          } # nocov end
        )
        
        private$worker <- worker$run(job)
        
        
        cnd <- catch_cnd({
          
          while (!decrement_semaphore(sem, wait = FALSE)) {
            
            if (job$is_done) {
              output <- job$output
              if (inherits(output, 'error')) cnd_signal(output)
              cli_abort('Unable to start WebQueue')  #nocov
            }
            
            later::run_now(timeoutSecs = 0.5)
          }
          
        })
        
        if (!is.null(cnd)) worker$stop()
        remove_semaphore(sem)
        if (!is.null(cnd)) cnd_signal(cnd)
        
      } 
      
      # Launch WebQueue on this R process
      else {
        
        private$parse            <- parse
        globals[['.wq_handler']] <- handler
        
        cnd <- catch_cnd({
        
          # Start a Queue.
          private$.jobqueue <- jobqueue::Queue$new(
            globals   = globals,
            packages  = packages,
            namespace = namespace,
            init      = init,
            max_cpus  = max_cpus,
            workers   = workers,
            timeout   = timeout,
            hooks     = hooks,
            reformat  = reformat,
            signal    = TRUE,
            stop_id   = stop_id,
            copy_id   = copy_id )
          
          later::run_now()
          if (!identical(private$.jobqueue$state, 'idle'))
            stop('Unable to start jobqueue::Queue')  #nocov
          
            
          # Start a Server.
          private$.httpuv <- httpuv::startServer(
            host  = host,
            port  = port,
            quiet = quiet,
            app   = list(
              call              = private$app_call,
              onHeaders         = onHeaders,
              staticPaths       = staticPaths,
              staticPathOptions = staticPathOptions ))
          
          later::run_now()
          if (!isTRUE(private$.httpuv$isRunning()))
            stop('Unable to start httpuv')  #nocov
          
        })
        
        if (inherits(cnd, 'error')) {
          self$stop()      #nocov
          cnd_signal(cnd)  #nocov
        }
        
      }
      
      
      private$.url <- paste0(
        'http://',
        ifelse(host == '0.0.0.0', '127.0.0.1', host),
        ifelse(port == 80L, '', paste0(':', port)) )
      
      return (self)
    },
    
    
    #' @description
    #' Print method for a WebQueue.
    #' @param ... Arguments are not used currently.
    print = function (...) {
      cli_text('{.cls {class(self)}} on {.url {self$url}}')
    },
    
    
    #' @description
    #' Shuts down the WebQueue and all associated subprocesses. Stopped Jobs
    #' will have their `$output` set to a object of class `<interrupt/condition>`
    #' 
    #' @param reason   A brief message for the condition object.
    #' 
    #' @return This WebQueue, invisibly.
    stop = function (reason = 'server stopped') {
      private$finalize(reason)
      return (invisible(self))
    }
  ),
  
  private = list(
    
    .jobqueue = NULL,
    .httpuv   = NULL,
    .url      = NULL,
    handler   = NULL,
    parse     = NULL,
    worker    = NULL,
    
    app_call = function (req) {
      
      cnd <- catch_cnd({
        
        req$ARGS    <- tryCatch(parse_args(req),    error = function (e) list())
        req$COOKIES <- tryCatch(parse_cookies(req), error = function (e) list())
        req$HEADERS <- js_obj(req$HEADERS)
        
        if (is.function(private$parse))
          stopifnot(is_environment(req <- private$parse(req)))
        
        # Exclude extraneous fields from req
        drop <- c(
          'httpuv.version', 'rook.errors', 'rook.input', 
          'rook.url_scheme', 'rook.version', 'SCRIPT_NAME', 
          'accept', 'accept-encoding', 'content-length', 
          'cookie', 'CONTENT_LENGTH', 'CONTENT_TYPE', 
          'REMOTE_PORT', 'SCRIPT_NAME', 'QUERY_STRING', 
          paste0('HTTP_', gsub('-', '_', toupper(names(req$HEADERS)))) )
        env_unbind(env = req, nms = drop)
        req$HEADERS <- req$HEADERS[setdiff(names(req$HEADERS), drop)]
          
      })
      if (!is.null(cnd)) return (format_500(cnd))
      
      job <- private$.jobqueue$run(
        expr = quote(.wq_handler(.wq_request)),
        vars = list(.wq_request = req),
        req  = req )
      
      then(
        promise     = as.promise(job),
        onFulfilled = format_200,
        onRejected  = format_500 )
    },
    
    
    finalize = function (reason = 'server stopped') {
      
      if (!is.null(private$worker)) {
        private$worker$stop()
      } else {
        if (!is.null(jq  <- private$.jobqueue)) jq$stop(reason)
        if (!is.null(svr <- private$.httpuv))   svr$stop()
      }
      
      invisible()
    }
  ),
  
  active = list(
    
    #' @field url
    #' URL where the server is available.
    url = function () private$.url
    
  )
)



parse_args <- function (req) {
  
  args <- js_obj()
  
  if (hasName(req, 'REQUEST_METHOD') && isTRUE(nzchar(req[['REQUEST_METHOD']])))
    if (identical(toupper(req[['REQUEST_METHOD']]), 'POST'))
      if (hasName(req, 'CONTENT_TYPE') && isTRUE(nzchar(req[['CONTENT_TYPE']])))
        if (is.function(req[['rook.input']]$read))
          args <- parse_http(req[['rook.input']]$read(), req[['CONTENT_TYPE']])
  
  if (hasName(req, 'QUERY_STRING') && isTRUE(nzchar(req[['QUERY_STRING']])))
    args <- c(args, parse_query(req[['QUERY_STRING']]))
  
  return (args)
}


parse_cookies <- function (req) {
  
  cookies <- js_obj()
  
  if (hasName(req, 'HTTP_COOKIE') && isTRUE(nzchar(req[['HTTP_COOKIE']]))) {
    for (cookie in strsplit(req[['HTTP_COOKIE']], ";", fixed = TRUE)[[1]]) {
      cookie <- trimws(strsplit(cookie, "=", fixed = TRUE)[[1]])
      if (length(cookie) == 1) cookie <- c('', cookie)
      if (length(cookie) >= 3) cookie <- c(cookie[[1]], paste(collapse = '=', cookie[-1]))
      cookies[[cookie[[1]]]] <- cookie[[2]]
    }
  }
  
  return (cookies)
}


format_200 <- function (result) {
  
  #________________________________________________________
  # Hand 'AsIs' objects back to httpuv unchanged.
  #________________________________________________________
  if (inherits(result, 'AsIs')) return (result)
  
  result <- response(result)
  return (result)
}


format_500 <- function (result) {
  
  #________________________________________________________
  # Hand 'AsIs' objects back to httpuv unchanged.
  #________________________________________________________
  if (inherits(result, 'AsIs')) return (result)
  
  
  #________________________________________________________
  # Convert error object to HTTP status code.
  #________________________________________________________
  
  body   <- ''
  parent <- if (hasName(result, 'parent')) result$parent
  
  if (is_int(result))                      { status <- result }
  else if (inherits(result, 'timeout'))    { status <- 408L; body <- result } # Request Timeout
  else if (inherits(parent, 'timeout'))    { status <- 408L; body <- parent } # Request Timeout
  else if (inherits(result, 'superseded')) { status <- 409L; body <- result } # Conflict
  else if (inherits(parent, 'superseded')) { status <- 409L; body <- parent } # Conflict
  else if (inherits(result, 'interrupt'))  { status <- 499L; body <- result } # Client Closed Request
  else if (inherits(parent, 'interrupt'))  { status <- 499L; body <- parent } # Client Closed Request
  else                                     { status <- 500L; body <- result } # Internal Server Error
  
  result <- response(
    status = status, 
    body   = ansi_strip(paste(collapse='\n', as.character(body))) )
  
  return (result)
}


# Using `hooks` to alter the handler
# hooks <- function (job, queue) {
#   job$vars$my_str <- function (x) capture.output(ls.str(x[order(names(x))]))
#   job$expr <- switch(
#     EXPR = job$req$PATH_INFO,
#     '/hello' = quote('Hello World'),
#     '/date'  = quote(date()),
#     '/sleep' = quote({x <- date(); Sys.sleep(5); c(x, date())}),
#     '/req'   = quote(my_str(req)),
#     '/query' = quote(my_str(webutils::parse_query(req$QUERY_STRING))),
#     job$expr )
# }

