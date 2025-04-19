#' @name Cohensdp
#'
#' @md
#'
#' @title Cohen's standardized mean difference.
#'
#' @aliases Cohensdp
#'
#' @description
#' ``Cohensdp()`` computes the Cohen's d (noted $d_p$) and its confidence interval in 
#' either within-subject, between-subject design and single-group design. For 
#' the between-subject design, MBESS already has an implementation based on the 
#' "pivotal" method but the present method is faster, 
#' using the method based on the Lambda prime
#' distribution \insertCite{l07}{CohensdpLibrary}. See 
#' \insertCite{h81,c22a,c22b,gc18;textual}{CohensdpLibrary}.
#'
#' @usage
#' Cohensdp(statistics, design, gamma, method )
#'
#' @param statistics    a list of pre-computed statistics. The statistics to provide 
#'                      depend on the design:
#'                      - for "between": ``m1``, ``m2`` the means of the two groups, 
#'                              ``s1``, ``s2`` the standard deviation of the two groups,
#'                               and ``n1``, ``n2``, the sample sizes of the two groups;
#'                        - for "within": ``m1``, ``m2``, ``s1``, ``s2``, ``n``, and 
#'                              ``r`` or ``rho`` the correlation between the measure; 
#'                        - for "single": ``m``, ``s``, ``n`` and ``m0`` the reference mean  
#'                              from which ``m`` is standardized).
#' @param design        the design of the measures (``"within"``, ``"between"``, or ``"single"``);
#' @param gamma         the confidence level of the confidence interval (default 0.95) 
#' @param method        In "within"-subject design only, choose among methods ``"piCI"``, or
#'                      ``"adjustedlambdaprime"`` (default), ``"alginakeselman2003"``, ``"morris2000"``, and
#'                      ``"regressionapproximation"``.
#'
#' @return   The Cohen's $d_p$ statistic and its confidence interval.
#'           The return value is internally a dpObject which can be 
#'           displayed with print, explain or summary/summarize.
#'
#' @details
#' This function uses the exact method in "single"-group and "between"-subject designs. 
#' In "within"-subject design, the default is the adjusted Lambda prime confidence interval
#' ("adjustedlambdaprime") which is based on an approximate method. This method is described in
#' \insertCite{c22b;textual}{CohensdpLibrary}. Other methods are available, described in
#' \insertCite{m00,ak03,CG057-1,f22;textual}{CohensdpLibrary}
#'
#'
#'
#'
#' @references
#' \insertAllCited{}
#'
#'
#' @examples
#'
#' # example in which the means are 114 vs. 101 with sds of 14.3 and 12.5 respectively
#' Cohensdp( statistics = list( m1= 101, m2= 114, s1= 12.5, s2= 14.3, n1= 12, n2= 12 ), 
#'           design     = "between")
#'
#' # example in a repeated-measure design
#' Cohensdp(statistics =list( m1= 101, m2= 114, s1= 12.5, s2= 14.3, n= 12, rho= 0.53 ),
#'          design     ="within" )
#'
#' # example with a single-group design where mu is a population parameter
#' Cohensdp( statistics = list( m = 101, m0 = 114, s = 12.5, n = 10 ), 
#'           design     = "single")
#'
#' # The results can be displayed in three modes
#' res <- Cohensdp( statistics = list( m = 101, m0 = 114, s = 12.5, n = 10), 
#'                  design     = "single")
#' 
#' # a raw result of the Cohen's d_p and its confidence interval
#' res              
#' 
#' # a human-readable output
#' summarize( res ) 
#' 
#' # ... and a human-readable ouptut with additional explanations.
#' explain( res )   
#' 
#' # example in a repeated-measure design with a different method than piCI
#' Cohensdp(statistics =list( m1= 101, m2= 114, s1= 12.5, s2= 14.3, n= 12, r= 0.53 ),
#'          design     ="within", method = "adjustedlambdaprime")
#'
#' 

#' @export 
Cohensdp <- function( 
            statistics = NULL, 
            design     = NULL, 
            gamma      = 0.95,
            method     = "exact"
    ) {

    ##############################################################################
    # STEP 1: Input validation
    ##############################################################################
    # 1.1: check that statistics is a non-empty list
    if( !(is.list(statistics)) )   stop( messageSnotL() ) 
    if( length(statistics) == 0 )  stop( messageSnotE() ) 

    # 1.2: check that the designs are legitimate
    if (is.null(design)) stop(messageDempt)
    if (!(design %in% c("within","between","single"))) 
        stop( messageDnotG(design) )

    # 1.3: check that the confidence level gamma is legitimate
    if (gamma < 0 | gamma > 1) 
        stop( messageGnotG(gamma)  )

    # 1.4: check method
    if (design == "single" & !( method == "exact"))
        stop( messageSexa(method) )
    if (design == "between" & !( method == "exact"))
        stop( messageBexa(method) )
    if (design == "within" & !( method %in% c("exact","piCI","adjustedlambdaprime","alginakeselman2003","morris2000","regressionapproximation") ) )
        stop( messageWexa(method) )


    ##############################################################################
    # STEP 2: let's do the computation and return everything
    ##############################################################################
    fct = paste("Cohensdp", design, sep=".")
    res = lapply(list(statistics), fct, gamma = gamma, method = method)[[1]]

    # preserve everything in an object of class CohensdpObject
    dpObject = list(
        type       = "dp",
        estimate   = res[1],
        interval   = c(res[2], res[3]),
        statistics = statistics, 
        design     = design, 
        gamma      = gamma,
        method     = method
    )
    class(dpObject) <- c("CohensdpObject", class(dpObject) )
    return( dpObject )
}





##############################################################################
# DEFINITIONS of all the designs subfunctions
# there are 3 functions in this package:
#
# within
#    using 
#    - exact = within using lsecond mixture (Cousineau, 2022)
#    - piCI = prior-informed credible interval when rho is unknown.
#    - Algina & Kesselman, 2003 -- see MBESS implementation (slow)
#    - Morris, 2000, 
#    - Adjusted Lprime -- see Goulet-Pelletier & Cousineau, 2018.
#    See Cousineau & Goulet-Pelletier, 2020, TQMP, for a review.
#
# between 
#    using exact = between using lprime(Lecoutre, 1999, 2007)
#    NO OTHER ARE IMPLEMENTED AS THE ABOVE IS EXACT
#        Steiger & Fouladi, 1997 -- see MBESS implementation (slow)
#        Bayes, 1763, Hedges, 1981; 
#    See Cousineau & Goulet-Pelletier, 2020, PsyArXiv, for a review.
#
# single
#    using exact = lprime    (Cousineau, 2022)
#    NO OTHER ARE IMPLEMENTED AS THE ABOVE IS EXACT
#        using pivotal -- see MBESS implementation (slow)
#
##############################################################################

##############################################################################
##### single #################################################################
##############################################################################
Cohensdp.single <- function(statistics, gamma = .95, method ) {
    sts  <- vfyStat(statistics, c("m","m0","s","n"))

    #get pairwise statistics Delta means and pooled SD
    dmn  <- sts$m - sts$m0
    #compute biased Cohen's d_p
    dp   <- dmn / sts$s  

    dlow <- qlprime(1/2-gamma/2, nu = sts$n-1, ncp = dp * sqrt(sts$n) ) 
    dhig <- qlprime(1/2+gamma/2, nu = sts$n-1, ncp = dp * sqrt(sts$n) ) 

    limits <- c(dlow, dhig) / sqrt(sts$n)
    c(dp, limits)

}


##############################################################################
##### between ################################################################
##############################################################################
Cohensdp.between <- function(statistics, gamma = .95, method ) {
    sts  <- vfyStat(statistics, c("m1","s1","n1","m2","s2","n2"))

    #get pairwise statistics Delta means and pooled SD
    dmn  <- sts$m1 - sts$m2
    sdp  <- sqrt((sts$s1^2 + sts$s2^2)/2)
    n    <- 2 / (1/sts$n1 + 1/sts$n2)           #harmonic mean
    #compute biased Cohen's d_p
    dp   <- dmn / sdp  

    dlow <- qlprime(1/2-gamma/2, nu = 2*(n-1), ncp = dp * sqrt(n/2) ) 
    dhig <- qlprime(1/2+gamma/2, nu = 2*(n-1), ncp = dp * sqrt(n/2) ) 

    limits <- c(dlow, dhig) / sqrt(n/2)
    c(dp, limits)

}


##############################################################################
##### within #################################################################
##############################################################################
#' @importFrom utils modifyList
Cohensdp.within <- function(statistics, gamma = .95, method ) {
    res <- if ("rho" %in% names(statistics)) {
                if (method!="exact") stop( messageNoMh() )
                if (statistics$rho == 0) # this is a between-subject design!
                    Cohensdp.between( 
                        modifyList(statistics, list(n1=statistics$n, n2=statistics$n)),
                        gamma )
                else
                    Cohensdp.within.rhoknown( statistics, gamma )
            } else if ("r" %in% names(statistics)) {
                if(statistics$r == 0) statistics=modifyList(statistics, list(r = 0.0000001))
                switch( method,
                    "exact" =                   stop( messageNoEx() ),
                    "piCI" =                    Cohensdp.within.piCI( statistics, gamma ),
                    "adjustedlambdaprime" =     Cohensdp.within.adjustedlambdaprime( statistics, gamma ),
                    "alginakeselman2003" =      Cohensdp.within.alginakeselman2003( statistics, gamma ),
                    "morris2000" =              Cohensdp.within.morris2000( statistics, gamma ),
                    "regressionapproximation" = Cohensdp.within.regressionapproximation( statistics, gamma )
                )
            } else {
                stop( messageNoRh() )
            }
    res
}

Cohensdp.within.rhoknown <- function(statistics, gamma = .95) {
    sts  <- vfyStat(statistics, c("m1","s1","m2","s2","n", "rho"))

    #get pairwise statistics Delta means and pooled SD
    dmn  <- sts$m1 - sts$m2
    sdp  <- sqrt((sts$s1^2 + sts$s2^2)/2)

    #compute biased Cohen's d_p 
    dp   <- dmn / sdp  

    #quantile of the (noncentral, nonstandard) lambda'' distribution 
    dlow = qlsecond(1/2-gamma/2., n = sts$n, d = dp, rho = sts$rho )
    dhig = qlsecond(1/2+gamma/2., n = sts$n, d = dp, rho = sts$rho )

    limits <- c(dlow, dhig) 
    c(dp, limits)

}

Cohensdp.within.piCI <- function(statistics, gamma = .95) {
    sts  <- vfyStat(statistics, c("m1","s1","m2","s2","n", "r"))

    #get pairwise statistics Delta means and pooled SD
    dmn  <- sts$m1 - sts$m2
    sdp  <- sqrt((sts$s1^2 + sts$s2^2)/2)

    #compute biased Cohen's d_p 
    dp <- dmn / sdp

    #quantile of the prior-informed lambda'' distribution
    dlow = qpilsecond(1/2-gamma/2, n = sts$n, d = dp, r = sts$r )
    dhig = qpilsecond(1/2+gamma/2, n = sts$n, d = dp, r = sts$r )

    limits <- c(dlow, dhig) 
    c(dp, limits)
}

Cohensdp.within.adjustedlambdaprime <- function(statistics, gamma = .95) {
    sts  <- vfyStat(statistics, c("m1","s1","m2","s2","n", "r"))
    GM<- function(ns) {length(ns) / sum(1/ns)}  # Geometric mean
    J <- function(df) { exp ( lgamma(df/2) - log(sqrt(df/2)) - lgamma((df-1)/2) ) }

    #get pairwise statistics Delta means and pooled SD
    dmn  <- sts$m1 - sts$m2
    sdp  <- sqrt((sts$s1^2 + sts$s2^2)/2)

    #compute biased Cohen's d_p 
    dp <- dmn / sdp

    W <- GM(c(sts$s1^2, sts$s2^2)) / mean(c(sts$s1^2, sts$s2^2))
    rW <- sts$r * W
    lambda <- dp * J(sts$n-1) * sqrt(sts$n/(2*(1-rW)))
    #quantile of the noncentral t distribution
    dlow = qlprime(1/2-gamma/2, nu = 2/(1+sts$r^2)*(sts$n-1), ncp = lambda )
    dhig = qlprime(1/2+gamma/2, nu = 2/(1+sts$r^2)*(sts$n-1), ncp = lambda )
    limits <- c(dlow, dhig) / sqrt(sts$n/(2*(1-rW))) / J( 2/(1+sts$r^2)*(sts$n-1) )
    c(dp, limits)
}

#' @importFrom stats qnorm
Cohensdp.within.morris2000 <- function(statistics, gamma = .95) {
    sts  <- vfyStat(statistics, c("m1","s1","m2","s2","n", "r"))
    J <- function(df) { exp ( lgamma(df/2) - log(sqrt(df/2)) - lgamma((df-1)/2) ) }

    #get pairwise statistics Delta means and pooled SD
    dmn  <- sts$m1 - sts$m2
    sdp  <- sqrt((sts$s1^2 + sts$s2^2)/2)

    #compute biased Cohen's d_p 
    dp <- dmn / sdp

    vd <- (sts$n-1)/(sts$n-3) * 2*(1-sts$r)/sts$n * (1+dp^2 * sts$n/(2*(1-sts$r))) - dp^2/J(sts$n-1)^2
    vd <- vd * J(sts$n-1)^2
    dlow <- dp + qnorm(1/2-gamma/2) * sqrt(vd)
    dhig <- dp + qnorm(1/2+gamma/2) * sqrt(vd)
    limits <- c(dlow, dhig)
    c(dp, limits)
}

Cohensdp.within.alginakeselman2003 <- function(statistics , gamma = .95) {
    sts  <- vfyStat(statistics, c("m1","s1","m2","s2","n", "r"))
    GM<- function(ns) {length(ns) / sum(1/ns)}  # Geometric mean
    J <- function(df) { exp ( lgamma(df/2) - log(sqrt(df/2)) - lgamma((df-1)/2) ) }

    #get pairwise statistics Delta means and pooled SD
    dmn  <- sts$m1 - sts$m2
    sdp  <- sqrt((sts$s1^2 + sts$s2^2)/2)

    #compute biased Cohen's d_p 
    dp <- dmn / sdp

    W <- GM(c(sts$s1^2, sts$s2^2)) / mean(c(sts$s1^2, sts$s2^2))
    rW <- sts$r * W
    ## MBESS is creating R Run errors, so implemented it with the true pivot distribution
    #    tCI <- MBESS::conf.limits.nct(dp * sqrt(sts$n/(2*(1-rW))), sts$n-1, conf.level = gamma)
    #    tCI.low <- tCI$Lower.Limit
    #    tCI.hig <- tCI$Upper.Limit
    tCI.low <- qlprime(1/2-gamma/2, nu=sts$n-1, ncp=dp * sqrt(sts$n/(2*(1-rW))) )
    tCI.hig <- qlprime(1/2+gamma/2, nu=sts$n-1, ncp=dp * sqrt(sts$n/(2*(1-rW))) )

    limits <- c(tCI.low, tCI.hig) / sqrt(sts$n/(2*(1-rW)))
    c(dp, limits)
}


fittsAdjustedGamma <- function(gamma, n) {
    # compute corrected confidence level from regression equations; see Fitts, 2022
    switch(toString(gamma),
      "0.9"  = gamma2 <- 1 - (0.1001 - 0.1087 / n + 0.3589 / n^2),
      "0.95" = gamma2 <- 1 - (0.0495 - 0.0843 / n),
      "0.99" = gamma2 <- 1 - (0.0098 - 0.0461 / n),
      stop( messageNotg(gamma) )
    )  
    gamma2
}


Cohensdp.within.regressionapproximation <- function(statistics, gamma = .95) {
    sts  <- vfyStat(statistics, c("m1","s1","m2","s2","n", "r"))
    GM<- function(ns) {length(ns) / sum(1/ns)}  # Geometric mean

    #get pairwise statistics Delta means and pooled SD
    dmn  <- sts$m1 - sts$m2
    sdp  <- sqrt((sts$s1^2 + sts$s2^2)/2)

    #compute biased Cohen's d_p 
    dp <- dmn / sdp

    # compute corrected confidence level from regression equations; see Fitts, 2022
    gamma2 <- fittsAdjustedGamma( gamma, sts$n )
    W   <- GM(c(sts$s1^2, sts$s2^2)) / mean(c(sts$s1^2, sts$s2^2))
    rW  <- sts$r * W
    rOP <- sts$r * (1 + (1-sts$r^2)/(2 * (sts$n - 3)))
   
    tCI.low <- qlprime( p = 0.5 - gamma2 /2, ncp = dp * sqrt(sts$n/(2*(1-rW))), nu = 2/(1+rOP^2)*(sts$n-1) )
    tCI.hig <- qlprime( p = 0.5 + gamma2 /2, ncp = dp * sqrt(sts$n/(2*(1-rW))), nu = 2/(1+rOP^2)*(sts$n-1) )
    limits <- c(tCI.low, tCI.hig) / sqrt(sts$n/(2*(1-rW)))
    c( dp, limits)
}


##############################################################################
##### This is it!#############################################################
##############################################################################
