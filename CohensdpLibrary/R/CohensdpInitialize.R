#' @useDynLib CohensdpLibrary
#
# what follows needed to avoid devtools::check() error
#' @importFrom Rdpack reprompt
#
# things to initialize
.onLoad <- function(libname, pkgname) {    
    # set the default arguments for the iterative functions:
    options(
        "CohensdpLibrary.MAXITER" = 32500,     # this is the maximum in short integer; 
                                               # that should be more than enough!
                                               # The nbre of iterations only rarely exceeds 2000
        "CohensdpLibrary.TOLERAN" = 0.0000001, # less than 7 decimals in the additional steps...
        "CohensdpLibrary.FORMAT"  = "%5.3f",   # printing results with 3 decimals 
                                               # Should be more than enough!
                                               # See Cousineau, 2020, JMP, for the number of decimals
        "CohensdpLibrary.SHOWWARNINGS" = TRUE  # use to inhibit messages and warnings
    )

    # load the external dynamically-link library Cohensdp
    # dyn.load("CohensdpLibrary") #loaded automatically by R

}

# things to clean?
.onUnload <- function(libpath) {
    # unload the dll
    # dyn.unload("CohensdpLibrary") #unloaded automatically by R

}

# header to messages
hm = sprintf(">>= CohensdpLibrary %s=>> ", packageVersion("CohensdpLibrary") )

# define the error messages here as they are used in many functions
messageSnotL <- function()       {paste(hm, "Argument `statistics` is missing or is not a list. Exiting...", sep="")}
messageSnotE <- function()       {paste(hm, "Argument `statistics` is empty. Exiting...", sep="") }
messageDnotG <- function(design) {paste(hm, "Not a known `design` \"",design,"\". Use within, between, single. Exiting...", sep="") }
messageDempt <- function()       {"Mandatory argument `design` not given. Exiting..."}
messageGnotG <- function(gamma)  {paste(hm, "The confidence level `gamma` \"",gamma,"\" is not between 0 and 1. Exiting...", sep="")}
messageSinct <- function(sname)  {paste(hm, "The list of statistics is incomplete. Are needed: ", 
                                        paste(sname, collapse = ", "), 
                                        ". Exiting...", sep = "")}
messageWier  <- function( fctname, ier) {paste(hm, "The subroutine ",fctname," signals convergence problems ", ier, sep="" ) }
messageNtsm  <- function(n)      {paste(hm, "Sample size ",n," too small. Exiting...",sep="")}
messageSneg  <- function(s)      {paste(hm, "Sample standard deviation ",s," cannot be negative. Exiting...",sep="")}
messageRwrg  <- function(r)      {paste(hm, "Correlation ",r," must be between -1 and +1. Exiting...",sep="")}
messageSexa  <- function(m)      {paste(hm, "Method `", m, "` unknown in \"single\" design. Only 'exact' is implemented. Exiting...")}
messageBexa  <- function(m)      {paste(hm, "Method `", m, "` unknown in \"between\" design. Only 'exact' is implemented. Exiting...")}
messageWexa  <- function(m)      {paste(hm, "Method `", m, "` unknown in \"within\" design. Only 'exact' or 'piCI' (default), 'morris2000', 'alginakeselman2003', 'adjustedlambdaprime', and 'regressionapproximation' are implemented. Exiting...")}

messageNoEx  <- function()       {"There is no exact method known in within-subject design when the population rho is unkown. We suggest method='adjustedlambdaprime'"}
messageNoRh  <- function()       {"Either rho or r is missing from the statistics. Exiting..."}
messageNoCI  <- function()       {paste(hm, "There is no confidence interval for an (unbiased) Hedges's gp...", sep="")}
messageNotg  <- function(g)      {paste(hm, "The confidence level ",g, " has not been calibrated yet. (only .90, .99, and .99 are at this time). Exiting...", sep="" ) }
messageNoMh  <- function()       {"Only the exact method can be used when rho is known. Exiting..."}

# Are the named statistics of statlist in the list statname?
# Accept specification like "a|b" which test that either an attribute a or an 
# attribute b is in the list
is.inIt <- function( statlist, statname ) {
    for (i in statname) {
        d <- unlist(strsplit(i, split="[|]"))
        if (!any(d %in% names(statlist)))
            return(FALSE)
    }
    return(TRUE)
}


# Verify that the named statistics in statlist are in the list or else issue an error message
vfyStat <- function(statlist, statname) {
    # check that the required statistics were provided
    if (!(is.inIt(statlist, statname)))
        stop( messageSinct(statname) )


    # check the domain of the statistics
    if ("n" %in% names(statlist) )  {if (statlist$n  <2) stop(messageNtsm(statlist$n)) }
    if ("n1" %in% names(statlist) ) {if (statlist$n1 <2) stop(messageNtsm(statlist$n1)) }
    if ("n2" %in% names(statlist) ) {if (statlist$n2 <2) stop(messageNtsm(statlist$n2)) }
    if ("s" %in% names(statlist) )  {if (statlist$s  <0) stop(messageSneg(statlist$s)) }
    if ("s1" %in% names(statlist) ) {if (statlist$s1 <0) stop(messageSneg(statlist$s1)) }
    if ("s2" %in% names(statlist) ) {if (statlist$s2 <0) stop(messageSneg(statlist$s2)) }
    if ("rho" %in% names(statlist) ){if ((statlist$rho < -1) | (statlist$rho > +1)) stop(messageRwrg(statlist$rho)) }
    if ("r" %in% names(statlist) )  {if (  (statlist$r < -1) |   (statlist$r > +1)) stop(messageRwrg(statlist$r)) }

    statlist
}

####################################################################
## new METHODS : here only router function and generic
####################################################################

#' @title explain 
#' 
#' @md
#'
#' @description
#' ``explain()`` provides a human-readable, exhaustive, description of
#' the results. It also provides references to the key results.
#' 
#' @param x   an object to explain
#' @param ... ignored
#' 
#' @return a human-readable output with details of computations.
#' 
#' @export
explain <- function(x, ...) {  UseMethod("explain") }

#' @export 
explain.default <- function(x, ...) { print(x) } 



#' @title summarize 
#' 
#' @md
#'
#' @description
#' ``summarize()`` provides a human-readable output of a dpObject. it is 
#' synonym of ``summary()`` (but as actions are verbs, I used a verb).
#' 
#' @param x   an object to summarize
#' @param ... ignored
#' 
#' @return a human-readable output as per articles.
#' 
#' @export 
summarize <- function(x, ...) {  UseMethod("summarize") }

#' @method summarize default 
#' @export 
summarize.default <- function(x, ...) { print(x) }



