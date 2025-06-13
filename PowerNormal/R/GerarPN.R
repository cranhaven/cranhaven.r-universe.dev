
dpn <- function(x, alpha) {
if (alpha <= 0 )
 stop("Error: The power parameter 'alpha' must be greater than zero")
pn.d(x, alpha )
}

ppn <- function(q, alpha) {
if (alpha <= 0 )
 stop("Error: The power parameter 'alpha' must be greater than zero")
pnorm(q)^alpha
}

qpn <- function(p, alpha) {
if (alpha <= 0 )
 stop("Error: The power parameter 'alpha' must be greater than zero")
qnorm(p^(1/alpha))
}


rpn <- function(n, alpha) {
if (alpha <= 0 )
 stop("Error: The power parameter 'alpha' must be greater than zero")

u <- runif(n)
qnorm(u^(1/alpha))
}

