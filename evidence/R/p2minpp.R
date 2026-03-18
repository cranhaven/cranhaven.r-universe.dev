p2minpp <- function(p) {
    ## convert frequentist p-value (< 0.37) into lower
    ## bound of posterior probability of H0 under an
    ## equal odds prior see SellkeBayariBerger2001
    if (p >= 0.37) 
        stop("sorry, this conversion only works for p < 0.37")
    return((1 + (-exp(1) * p * log(p))^(-1))^(-1))
}
