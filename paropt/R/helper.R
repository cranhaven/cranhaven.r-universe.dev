check_fct <- function(f, optimizer = TRUE) {
  e <- new.env()
  e$found <- FALSE
  walk_ast <- function(code) {
    if(!is.call(code)) {
      return(code)
    }
    code <- as.list(code)

    if(code[[1]] == as.name("return")) {
      e$found <- TRUE
      e$found_what <- c(e$found_what, code[[2]])
    } else if(deparse(code[[1]]) %in% e$not_thread_safe ) {
      stop(paste("The function", deparse(code[[1]]), "is not thread safe."))
    }
    lapply(code, e$walk_ast)
  }
  e$walk_ast <- walk_ast
  if(optimizer == TRUE) {
    e$not_thread_safe <- c("print",
                           "dunif", "punif", "qunif", "runif",
                           "dnorm", "pnorm", "qnorm", "rnorm",
                           "dlnorm", "plnorm", "qlnorm", "rlnorm",
                           "dgamma", "pgamma", "qgamma", "rgamma")
  } else {
    e$not_thread_safe <- NULL
  }

  e$found_what <- NULL
  trash <- e$walk_ast(body(f))
  stopifnot("Found no return statement"=e$found==TRUE)

  return(e$found_what)
}


