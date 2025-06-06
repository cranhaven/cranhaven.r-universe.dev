smoothCPPP <-
function(y, B, tau, lambdashort_glatt, lambdashort_orig, DD, NB, glatterms, smoothtype) {
    .Call('_expectreg_smoothCPPP', PACKAGE = 'expectreg', y, B, tau, lambdashort_glatt, lambdashort_orig, DD, NB, glatterms, smoothtype)
}
