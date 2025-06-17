bracket <-
function(g, a, b, nlm=FALSE, ...) {
    # bracket finds a point x between 'a' and 'b' such that g(x) has the
    # opposite sign of g(a).
    #
    # Arguments
    #  g : a real valued function of a real variable
    #  a : a real number
    #  b : a real number
    #
    # Returns
    #  A real number x such that g(x) and g(a) have opposite signe
    
    va <- g(a, nlm=nlm, ...)

    f <- 0.5
    while (TRUE) {

        c <- f*a + (1-f)*b 
        vc <- g(c, nlm=nlm,...)
        if (va*vc < 0) {
            return(c)
        }

        f <- f/2
        if (f < 1e-10) {
            stop("Unable to bracket likelihood.")
        }
    }
}
