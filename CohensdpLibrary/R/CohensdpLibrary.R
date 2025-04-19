#' @details
#' CohensdpLibrary is a library that computes the Cohen's $d_p$ along with confidence intervals. 
#' Its main contribution is to allow confidence intervals in the repeated-measure designs.
#' The main function is 
#' 
#' Cohensdp( statistics = list(...), design, gamma, method )  
#' 
#' see help(Cohensdp) for more on this function
#' 
#' It also provides functions
#'   Hedgesgp( statistics = list(...), design, method )
#' for an unbiased Cohen's dp and
#'   J( statistics = list(...), design )
#' to get the correction factor used in un-biasing dp. 
#' 
#' Subsidiary functions are
#' plprime(x, nu, ncp) cumulative probability of the lambda prime distribution with parameters nu, ncp
#' dlprime(x, nu, ncp) density
#' qlprime(x, nu, ncp) quantile
#'  
#' pkprime(x, nu1, nu2, ncp) cumulative probability of the K prime distribution with parameters nu1, nu2, ncp
#' dkprime(x, nu1, nu2, ncp) density
#' qkprime(x, nu1, nu2, ncp) quantile
#'  
#' These functions are implemented from the FORTRAN source of Poitevineau & Lecoutre, 2010.
#' Note that the library sadists also has implementations for these two distributions. However,
#' the sadists::kprime distribution is inaccurate for small nu1 or small nu2.
#' 
#' @keywords internal
"_PACKAGE"
#> [1] "_PACKAGE"

#' @title CohensdpLibrary
#'
#' @description
#' CohensdpLibrary provides the main command Cohensdp to compute Cohen's d (noted d_p) and its 
#' confidence interval in either within-subject, between-subject design
#' and single-group design. For the between-subject design, MBESS already 
#' has an implementation based on the "pivotal" method. This method is exact for between-subject  
#' design but slower to compute than the "lprime" method.
#'  
#'
#' 
#'
#' @author Denis Cousineau, \email{denis.cousineau@@uottawa.ca}
#' @references \url{https://.../...}
#'
#' @examples
#' # Let's define a vector (it could be a 1-colum matrix or a one-column data.frame)
#' x1 <- c(3,4,5)
#' x2 <- c(6,7,8,9)
#'
#' # Get the Cohen's dp with its confidence interval (95% default):
#' Cohensdp( statistics = list(m1=mean(x1), m2=mean(x2), s1=sd(x1), 
#'          s2=sd(x2), n1=length(x1), n2=length(x2)),
#'          design = "between")
#' ###
#' # in the above, the design is assumed by default to be a between-group design, and the relevant
#' # statistics are means m1 and m2, standard deviations s1 and s2, sample sizes n1 and n2.
#'
#' # This example specifies the design and change the confident level (gamma):
#' Cohensdp( statistics = list(m1=15, m2=20, s1=4, s2=4, n1=25, n2=25), 
#'           design = "between", gamma = .80)
#' ###
#'
#' # Finally, this example computes a within-subject design
#' Cohensdp( statistics = list(m1=15, m2=20, s1=4, s2=4, n=25, rho = 0.333), 
#'           design = "within")
#' ###
#' The sample size n is a unique number; the population correlation is specified with rho.
#'
#' # By default, the functions assumes maximum iterations and tolerance values.
#' # You can change these defaults with the options:
#' options(CohensdpLibrary.MAXITER = 10000)     # this is the default maximum iterations
#' options(CohensdpLibrary.TOLERAN = 0.0000001) # this is the default tolerance value
#' # Increasing MAXITER or decreasing TOLERAN slows computations.
#' 
#' # Increasing the display of decimals
#' options(CohensdpLibrary.FORMAT = "%5.3f")     
#' # this is the default format (5 digits including 3 decimals)
