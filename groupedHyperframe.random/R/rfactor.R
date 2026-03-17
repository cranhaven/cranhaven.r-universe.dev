



#' @title Generate Random \link[base]{factor}
#' 
#' @description
#' To generate random \link[base]{factor}.
#' 
#' @param n \link[base]{integer} scalar
#' 
#' @param prob \link[base]{numeric} \link[base]{vector}, see function \link[base]{sample.int}
#' 
#' @param levels \link[base]{character} \link[base]{vector}, see function \link[base]{factor}
#' 
# @param ... additional parameters, currently not in use
#' 
#' @details
#' The function [rfactor()] is a wrapper of \link[base]{sample.int}.
#' 
#' @returns 
#' The function [rfactor()] returns a \link[base]{factor}.
#' 
#' @note
#' The function \link[stats]{rmultinom} is **not** what we need!
#' 
#' @examples
#' rfactor(n = 100L, prob = c(4,2,3))
#' rfactor(n = 100L, prob = c(4,2,3), levels = letters[1:3])
#' @keywords internal
#' @export
rfactor <- function(n, prob, levels = as.character(seq_len(nprob))) {
  nprob <- length(prob)
  if (length(levels) != nprob) stop('`levels` and `prob` must have same length')
  ret <- sample.int(n = nprob, size = n, prob = prob, replace = TRUE)
  attr(ret, which = 'levels') <- levels
  attr(ret, which = 'class') <- 'factor'
  return(ret)
}


