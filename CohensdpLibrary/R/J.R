#' @name J
#'
#' @md
#'
#' @title The correction factor J for a standardized mean difference.
#'
#' @aliases J
#'
#' @description
#' ``J()`` computes the correction factor to get an unbiased Cohen's $d_p$ in either within-
#' subject, between-subject design and single-group design. See
#' \insertCite{l22,gc18,;textual}{CohensdpLibrary}.
#'
#' @usage
#' J(statistics, design)
#'
#' @param statistics    a list of pre-computed statistics. The statistics to provide
#'                      depend on the design:
#'                        - for "between": ``n1``, ``n2``, the sample sizes of the two groups;
#'                        - for "within":  ``n``, and ``r`` or ``rho`` the correlation between  
#'                          the measure; 
#'                        - for "single":  ``n``.
#' @param design        the design of the measures (``"within"``, ``"between"``, or ``"single"``);
#'
#' @return   The correction factor for unbiasing a Cohen's $d_p$. 
#'           The return value is internally a dpObject which can be 
#'           displayed with print, explain or summary/summarize.
#'
#' @details
#' This function decreases the degrees of freedom by 1 in within-subject design when the population 
#' rho is unknown.
#' 
#' @references
#' \insertAllCited{}
#'
#'
#' @examples
#'
#' # example in which the means are 114 vs. 101 with sds of 14.3 and 12.5 respectively
#' J( statistics = list( n1 = 12, n2 = 12 ), 
#'    design     = "between")
#'
#' # example in a repeated-measure design
#' J( statistics = list( n = 12, rho = 0.53 ), 
#'    design     = "within")
#'
#' # example with a single-group design where mu is a population parameter
#' J( statistics = list( n = 12 ), 
#'    design     = "single")
#'
#' # The results can be displayed in three modes
#' res <- J( statistics = list( n = 12 ), 
#'           design     = "single")
#'
#' # a raw result of the Cohen's d_p and its confidence interval
#' res              
#'
#' # a human-readable output
#' summarize( res ) 
#'
#' # ...and a human-readable ouptut with additional explanations
#' explain( res )   
#'                  

#' @export
J <- function( 
            statistics = NULL, 
            design     = NULL
    ) {

    ##############################################################################
    # STEP 0: Load required libraries
    ##############################################################################


    ##############################################################################
    # STEP 1: Input validation
    ##############################################################################

    # 1.1: check that statistics is a non-empty list
    if(!(is.list(statistics)))   stop( messageSnotL() ) 
    if(length(statistics) == 0)  stop( messageSnotE() ) 

    # 1.2: check that the designs are legitimate
    if (is.null(design)) stop(messageDempt() )
    if (!(design %in% c("within","between","single"))) {
        stop( messageDnotG(design) )
    }




    ##############################################################################
    # STEP 2: let's do the computation and return everything
    ##############################################################################
    fct = paste("J", design, sep=".")
    res = lapply(list(statistics), fct )[[1]]
    
    JObject = list(
        type       = "J",
        estimate   = res, 
        statistics = statistics, 
        design     = design
    )
    class(JObject) <- c("CohensdpObject", class(JObject) )
    return( JObject )

}



##############################################################################
# DEFINITIONS of all the designs x methods subfunctions
# there are 6 methods implemented in this function:
#
# within
#    using exact = within using lambdasecond mixture (Cousineau, 2022)
#    using alginakesselman (Algina & Kesselman, 2003) --MBESS implementation (slow)
#    Other methods not implemented: Morris, 2000, Goulet-Pelletier & Cousineau, 2018, Adjusted Lambdaprime;
#       see Cousineau & Goulet-Pelletier, 2020, TQMP, for a review.
#
# between 
#    using exact = between using lambdaprime(Lecoutre, 1999, 2007)
#    using steigerfouladi (Steiger & Fouladi, 1997) --MBESS implementation (slow)
#    Other methods not implemented: Bayes, 1763, Hedges, 1981; 
#       see Cousineau & Goulet-Pelletier, 2020, PsyArXiv, for a review.)
#
# single
#    using pivotal        (Here, 2022) --MBESS implementation (slow)
#    using lambdaprime    (Here, 2022)
#
##############################################################################


##############################################################################
##### single #################################################################
##############################################################################
J.single <- function( statistics ) {
    sts  <- vfyStat(statistics, c("n"))

    df = sts$n-1

    # compute unbiasing factor; works for small or large df; thanks to Robert Calin-Jageman
    exp ( lgamma(df/2) - log(sqrt(df/2)) - lgamma((df-1)/2) )

}


##############################################################################
##### between ################################################################
##############################################################################
J.between <- function( statistics ) {
    sts  <- vfyStat(statistics, c("n1","n2"))

    df = sts$n1 + sts$n2 - 2

    # compute unbiasing factor; works for small or large df; thanks to Robert Calin-Jageman
    exp ( lgamma(df/2) - log(sqrt(df/2)) - lgamma((df-1)/2) )

}


##############################################################################
##### within #################################################################
##############################################################################
J.within <- function( statistics ) {
    sts  <- vfyStat(statistics, c("n","r|rho"))

    # both versions are identical except that rho is named or r...
    if ( "rho" %in% names(statistics) ) 
        rho = statistics$rho
    else
        rho = statistics$r

    df = 2*(statistics$n-1)
    exp ( lgamma(df/2) - log(sqrt(df/2)) - lgamma((df-1)/2) ) *
      (1-rho^2)^(-(df-2)/4) /
      hypergeometric2F1((df-1)/4, (df+1)/4, (df+2)/4, rho^2)
}



##############################################################################
##### This is it!#############################################################
##############################################################################
