#' @title Checking the plausibility and conversion of dom and af
#' \code{link{struc_preparation}}

#' @description Before calling \code{struc_preparation} it is necessary to bring the genetics into the correct form, which is mainly the correct dimensions.
#' @template af
#' @template dom
#' @template epis
#' @template Rmx

#' @details If no genetics are required \code{af} has to be set to \code{NA}. The value of \code{dom} is adjusted to the length of \code{af}. If there are mismatches, the \code{dom} is cut or the first given value is repeated to fit the number of given alleles in \code{af}. The variables af and \code{dom} are corrected and \code{n_loci} is created, which is 0 when no genetics is included. If no value is given to \code{epis} it is set to 0.


gen_check <- function(Rmx, af, dom, epis){
cat("Check genetical model settings... ")
if(anyNA(af)){
              af <- NULL
              n_loci <- 0
}else{
n_loci <- length(af)}
if(is.na(epis)) {epis <- 0}

if(!is.null(af) & anyNA(dom)) stop("gen_check: value(s) for dom is missen.")
if(is.na(Rmx)){stop("Rmx need to be defined")}

if(n_loci > 0){
          if(length(dom)< n_loci) dom <- rep(dom[1],n_loci)
          if(length(dom) > n_loci) dom <- dom[seq_along(n_loci)]

          }
assign("n_loci", n_loci, pos = -1, envir=parent.frame(n = 1))                   #.GlobalEnv          parent.frame(n = 1)
assign("dom", dom, pos = -1, envir=parent.frame(n = 1))
assign("af", af, pos = -1, envir=parent.frame(n = 1))
assign("n_loci", n_loci, pos = -1, envir=parent.frame(n = 2))
assign("epis", epis, pos = -1, envir=parent.frame(n = 2))
cat(" finished!\n")
#return()
}