#' Test Bernoulli distribution using buckets
#'
#' Test if the success probability of a Bernoulli experiment lies
#' within a desired 'bucket'. This used the sequential procedure
#' described in
#' \insertCite{GandyHahnDing:pvaluebuckets;textual}{mcunit}.
#'
#' @param object Function that performs one sampling step.  Returns 0 or 1.
#' @param J  Buckets to use. A matrix with two rows, each column
#'          describes an interval bucket. Column names give names for
#'          the bucket(s).
#' @param ok Name of bucket(s) that pass the Unit test.
#' @param epsilon Error bound. 
#' @param ... Further parameters to be passed on to 'mctest'.
#' @references
#' \insertAllCited{}
#' @examples
#'    J <- matrix(nrow=2,c(0,0.945, 0.94,0.96, 0.955,1))
#'    colnames(J) <- c("low","ok","high")
#'    gen <- function() as.numeric(runif(1)<0.95)
#'    expect_bernoulli(gen,J=J,ok="ok")
#' @return The first argument, invisibly, to allow chaining of expectations.
#' @export
expect_bernoulli <- function(object, J, ok, epsilon=1e-3,...) {
    
    act <- testthat::quasi_label(rlang::enquo(object), arg = "object")

                                        #check that column names exist
    if (is.null(colnames(J)))
        testthat::fail("J must have column names")
    if (length(ok)>1)
        testthat::fail("ok must be one string")
    if (!any(colnames(J)==ok))
        testthat::fail("At least one of the column names of J must match the string in ok")
    
    ## check intervals are overlapping
    {
        o <- order(J[1,])
        if (J[1,o[1]]>0) testthat::fail("Buckets must cover [0,1]")
        last <- J[2,o[1]]
        for (i in 2:(dim(J)[2])){
            if (last<=J[1,o[i]])
                testthat::fail("Buckets must be overlapping")
            last <- max(last,J[2,o[i]])
        }
        if (last<1)
            testthat::fail("Buckets must cover [0,1]")
    }
    
    res <- simctest::mctest(gen=object,J=J, epsilon=epsilon,...)  
    
    if (res$decision ==ok){
        return(invisible(act$val))
    }
    message <- sprintf("Test returned bucket [%g,%g], called '%s', not a bucket called '%s'.",res$decision.interval[1],res$decision.interval[2],res$decision, ok)
    testthat::fail(message)
}
