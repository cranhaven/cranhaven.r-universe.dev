
namnum2expr <- function(x) {
  # Remove unchanged covariates
  x <- x[x != 0]
  
  # Prepare names
  xnam <- names(x)
  if (any(xnam == '')) {
    stop('all names of named vector passed to "delta" must be specified')
  }
  pos <- grepl('^[[:alpha:]]{1}[[:alnum:]]*$', xnam)
  xnam[pos == FALSE] %<>% paste0('`', ., '`')
  
  # Initialisation
  out <- ''
  
  # Generate expression
  for (j in seq_along(x)) {
    depo <- paste(x[j], '*', xnam[j])
    if (j > 1) { depo %<>% stringr::str_replace_all('-', ' - ') }
    if (x[j] == 1)  { depo <- xnam[j] }
    if (x[j] == -1) { depo <- paste(' -', xnam[j]) }
    if ((x[j] > 0) & (j > 1)) { depo %<>% paste(' +', .) }
    out %<>% paste0(depo)
  }
  
  # Output
  return(out)
}



handle_block_delta <- function(block, covnames, pattern = '<<(.+?)>>') {
  if (!is.numeric(block$delta) & (stringr::str_detect(block$delta[1], pattern))) {
  	# Find matches
    cand <- stringr::str_match(block$delta, pattern)[1, 2]
 
    # List of matching covariates
  	grep(paste0('^', cand), covnames, value = TRUE) %>%
  	  # Generate new blocks
  	  lapply(function(x) {
  	  	newblock <- block
  	  	newblock$delta <- stringr::str_replace(
  	  	  string = newblock$delta,
  	  	  pattern = pattern,
  	  	  replacement = paste0('`', x, '`')
  	  	)
  	  	#newblock$label2 %<>% c(paste0(
  	  	#  cand, ': ', stringr::str_replace(x, paste0('^', cand), '')
  	  	#))
  	  	newblock$label2 %<>% c(stringr::str_replace(x, paste0('^', cand), ''))
  	  	return(newblock)
  	  }) %>%
  	  # Recursion
  	  lapply(handle_block_delta, covnames = covnames, pattern = pattern) -> block
  	  
  	  if (is.list(block[[1]][[1]])) {
  	    block %<>% unlist(recursive = FALSE)
  	  }
  } else { block %<>% list }
  
  return(block)
}



pre_process_delta <- function(delta, model) {
  # Check and structure
  if (is.numeric(delta)) {
    if (is.null(names(delta))) {
      delta <- list(list(delta = delta))
    } else {
      delta <- list(list(delta = namnum2expr(delta)))
    }
  } else if (is.character(delta)) {
  	delta <- lapply(delta, function(x) { list(delta = x) })
  } else {
  	delta <- as.list(delta)
  }
  
  # Elaborate
  lapply(delta, handle_block_delta, covnames = rownames(model$B)) %>%
    # Reduce last level
    unlist(recursive = FALSE) %>%
    # Prepare labels
    lapply(function(x) {
    	  if (!is.null(x$label2)) {
    	    x[['label']] %<>% paste0(paste(x$label2, collapse = '; '))
    	    x$label2 <- NULL
    	  }
    	  x
    }) %>%
    # Output
    return()
}



#' It computes the vector of covariate change
#'
#' Given the argument `delta` passed to [`field3logit`] either as
#' a `numeric` vector, a `character` or an `expression` (see
#' [`field3logit`]), `get_vdelta` returns the `numeric` vector of
#' covariate change \eqn{\Delta\in\textbf{R}^k}.
#'
#' @param model object of class `model3logit`returned by [extract3logit()].
#' @param delta see [`field3logit`].
#'
#' @returns `numeric` vector of covariate change \eqn{\Delta\in\textbf{R}^k}.
#'
#' @keywords internal
get_vdelta <- function(delta, model) {
  if (!is.numeric(delta) & (length(delta) > 1)) { delta %<>% extract(1) }

  if (is.character(delta)) { delta %<>% parse(text = .) }
  
  if (is.expression(delta)) {
  	depoE <- new.env()
  	n <- nrow(model$B)
  	mapply(
  	  FUN = function(x, y) assign(x, versor(y, n), envir = depoE),
  	  rownames(model$B),
  	  1:n
  	)
  	delta <- eval(delta, depoE)
  }
  delta
}


