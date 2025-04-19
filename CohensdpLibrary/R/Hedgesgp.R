#' @name Hedgesgp
#'
#' @md
#'
#' @title The unbiased Hedges' standardized mean difference.
#'
#' @aliases Hedgesgp
#'
#' @description
#' ``Hedgesgp()`` computes the unbiased Cohen's d (noted $g_p$) in either within-subject,
#' between-subject design and single-group design. See
#' \insertCite{h81,gc18;textual}{CohensdpLibrary}.
#'
#' @usage
#' Hedgesgp(statistics, design)
#'
#' @param statistics    a list of pre-computed statistics. The statistics to provide 
#'                      depend on the design:
#'                        - for "between": ``m1``, ``m2`` the means of the two groups, 
#'                          ``s1``, ``s2`` the standard deviation of the two groups, and 
#'                          ``n1``, ``n2``, the sample sizes of the two groups;
#'                        - for "within": ``m1``, ``m2``, ``s1``, ``s2``, ``n``, and 
#'                          ``r`` or ``rho`` the correlation between the measure; 
#'                        - for "single": ``m``, ``s``, ``n`` and ``m0`` the reference 
#'                          mean from which ``m`` is standardized).
#' @param design        the design of the measures (``"within"``, ``"between"``, or ``"single"``);
#'
#' @return   The unbiased Cohen's $d_p$ statistic, commonly called a Hedges' $g_p$.
#'           The return value is internally a dpObject which can be 
#'           displayed with print, explain or summary/summarize.
#'
#' @details
#' This function returns the Cohen's d_p statistics corrected for bias but no confidence
#' interval as this estimate is not used to build such interval.
#' This function uses r when rho is unknown.
#'
#' @references
#' \insertAllCited{}
#'
#'
#' @examples
#'
#' # example in which the means are 114 vs. 101 with sDs of 14.3 and 12.5 respectively 
#' Hedgesgp( statistics = list( m1= 101, m2= 114, s1= 12.5, s2= 14.3, n1= 12, n2= 12 ), 
#'           design     = "between")
#'
#' # example in a repeated-measure design
#' Hedgesgp( statistics = list( m1= 101, m2= 114, s1= 12.5, s2= 14.3, n= 12, rho= 0.53 ), 
#'           design     = "within")
#'
#' # example with a single-group design where mu is a population parameter
#' Hedgesgp( statistics = list( m = 101, m0 = 114, s = 12.5, n = 10 ), 
#'           design     = "single")
#'
#' # The results can be displayed in three modes
#' res <- Hedgesgp( statistics = list( m = 101, m0 = 114, s = 12.5, n = 10 ), 
#'                  design     = "single")
#'
#' # a raw result of the Cohen's d_p and its confidence interval
#' res              
#'
#' # a human-readable output
#' summarize( res ) 
#'
#' # ... and a human-readable ouptut with additional explanations 
#' explain( res )   
#'                  
#' 

#' @export 
Hedgesgp <- function( 
            statistics = NULL, 
            design     = NULL
    ) {

    ##############################################################################
    # STEP 1: Input validation
    ##############################################################################
    # 1.1: check that statistics is a non-empty list
    if(!(is.list(statistics)))   stop( messageSnotL() ) 
    if(length(statistics) == 0)  stop( messageSnotE()) 

    # 1.2: check that the designs are legitimate
    if (is.null(design)) stop(messageDempt)
    if (!(design %in% c("within","between","single"))) {
        stop( messageDnotG(design) )
    }

    if( getOption("CohensdpLibrary.SHOWWARNINGS") )
        message(  messageNoCI() )

    ##############################################################################
    # STEP 2: let's do the computation and return everything
    ##############################################################################
    fct = paste("Hedgesgp", design, sep=".")
    res = lapply( list(statistics), fct )[[1]]
    
    gpObject = list(
        type       = "gp",
        estimate   = res[1],
        interval   = "no known confidence intervals for an (unbiased) Hedges' gp",
        statistics = statistics, 
        design     = design
    )
    class(gpObject) <- c("CohensdpObject", class(gpObject) )
    return( gpObject )

}






##############################################################################
# DEFINITIONS of all the designs x methods subfunctions
# there are 6 methods implemented in this function:
#   between using exact = between using lambdaprime(Lecoutre, 1999, 2007)
#   within using exact = within using lambdasecond mixture (Cousineau, 2022)
#   single using exact = lambdaprime   (Here, 2022)
#
##############################################################################


##############################################################################
##### single #################################################################
##############################################################################
Hedgesgp.single <- function( statistics ) {
    sts  <- vfyStat(statistics, c("m","m0","s","n"))

    #get pairwise statistics Delta means and pooled SD
    dmn  <- sts$m - sts$m0
    n    <- 2 / (1/sts$n1 + 1/sts$n2)           #harmonic mean
    #compute biased Cohen's d_p
    dp   <- dmn / sts$s  
    # compute correction factor
    j <- J.single( statistics )

    dp * j
}


##############################################################################
##### between ################################################################
##############################################################################
Hedgesgp.between <- function( statistics ) {
    sts  <- vfyStat(statistics, c("m1","s1","n1","m2","s2","n2"))

    #get pairwise statistics Delta means and pooled SD
    dmn  <- sts$m1 - sts$m2
    sdp  <- sqrt((sts$s1^2 + sts$s2^2)/2)
    n    <- 2 / (1/sts$n1 + 1/sts$n2)           #harmonic mean
    #compute biased Cohen's d_p
    dp   <- dmn / sdp  
    # compute correction factor
    j <- J.between( statistics )

    dp * j


}


##############################################################################
##### within #################################################################
##############################################################################
Hedgesgp.within <- function( statistics ) {
    sts  <- vfyStat(statistics, c("m1","s1","m2","s2","n","r|rho"))

    #get pairwise statistics Delta means and pooled SD
    dmn  <- sts$m1 - sts$m2
    sdp  <- sqrt((sts$s1^2 + sts$s2^2)/2)
    #compute biased Cohen's d_p 
    dp   <- dmn / sdp  
    # compute correction factor
    j <- J.within( statistics )

    dp * j
}



##############################################################################
##### This is it!#############################################################
##############################################################################
