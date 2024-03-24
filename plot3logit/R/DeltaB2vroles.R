
#' Identification of roles of vertices in non-degenerate cases
#'
#' `DeltaB2vroles_cat3logit` and `DeltaB2vroles_ord3logit` identify
#' (in a categorical and an ordinal model respectively)
#' the role of vertices of a field associated to a change in
#' covariate values \eqn{\Delta\in\textbf{R}^k}.
#'
#' @inheritParams DeltaB2pc
#'
#' @returns Named `list` of three components:
#'
#' \item{vo}{coordinates of vertex where the field originates from.}
#' \item{vt}{coordinates of transition vertex.}
#' \item{vs}{coordinates of vertex where the field is directed to.}
#'
#' @keywords internal
#' @name DeltaB2vroles
NULL



#' @rdname DeltaB2vroles
#' @keywords internal
DeltaB2vroles_cat3logit<- function(DeltaB) {
  lapply(1:3, function(x) rep(0, 3)) %>%
    set_names(c('vo', 'vt', 'vs')) -> depo
    
  DeltaB %>%
    c(0,.) %>%
    order %>%
    mapply(function(x,y) { x[y] <- 1; x }, depo, .,SIMPLIFY=FALSE) %>%
    return()
}



#' @rdname DeltaB2vroles
#' @keywords internal
DeltaB2vroles_ord3logit<- function(DeltaB) {
  out <- list(vo = NULL, vt = c(0, 1, 0), vs = NULL)
  
  if (DeltaB > 0) {
  	out$vo <- c(1, 0, 0)
  	out$vs <- c(0, 0, 1)
  } else {
  	out$vo <- c(0, 0, 1)
  	out$vs <- c(1, 0, 0)
  }
  
  out
}

