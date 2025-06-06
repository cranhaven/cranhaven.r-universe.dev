lltLS <-
function(y, B, tau, lambdashort_glatt, lambdashort_orig, DD, NB, glatterms) {
    .Call('_expectreg_lltLS', PACKAGE = 'expectreg', y, B, tau, lambdashort_glatt, lambdashort_orig, DD, NB, glatterms)
}
