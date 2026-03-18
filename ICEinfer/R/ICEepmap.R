"ICEepmap" <-
function (lambda = 1, beta = 1, gamma = 3 + 2 * sqrt(2)) 
{
    if (lambda <= 0) 
        stop("The Lambda argument to ICEepmap must be strictly positive.")
    if (beta <= 0) 
        stop("The Beta argument to ICEepmap must be strictly positive.")
    if (gamma <= 0) 
        stop("The Gamma = eta*beta argument to ICEepmap must be strictly positive.")
    ICEepmol <- list(lambda = lambda, beta = beta, gamma = gamma)
    class(ICEepmol) <- "ICEepmap"
    ICEepmol
}

"ICEomega" <-
function (lambda = 1, beta = 1, eta = 3 + 2 * sqrt(2)) 
{
    if (lambda <= 0) 
        stop("The Lambda argument to ICEomega must be strictly positive.")
    if (beta <= 0) 
        stop("The Beta argument to ICEomega must be strictly positive.")
    if (eta <= 0) 
        stop("The Eta = gamma/beta ratio argument to ICEomega must be strictly positive.")
    gamma = eta * beta
    ICEepmol <- list(lambda = lambda, beta = beta, gamma = gamma)
    class(ICEepmol) <- "ICEepmap"
    ICEepmol
}

"plot.ICEepmap" <-
function (x, xygrid = FALSE, ...) 
{
    if (missing(x)) 
        stop("The first argument to plot.ICEepmap must be an ICEepmap object.")
    if (xygrid == FALSE) {
        xp <- seq(-10, +10, length = 201)
        yp <- xp
        xygrid <- expand.grid(x = xp, y = yp)
    }
    r <- as.vector(sqrt(xygrid$x^2 + xygrid$y^2/x$lambda^2))
    abcos <- abs(xygrid$x - xygrid$y/x$lambda)/r
    xygrid$z <- sign(xygrid$x - xygrid$y/x$lambda) * r^x$beta * 
        abcos^x$gamma
    eta <- x$gamma/x$beta
    contourplot(z ~ x * y, xygrid, cuts = 50, xlab = "Delta Effectiveness (Cost Units)", 
        ylab = "Delta Cost", main = "ICE Economic Preference Map", 
        sub = paste("lambda =", round(x$lambda, digits = 3), 
            ", beta =", round(x$beta, digits = 3), ", gamma=", 
            round(x$gamma, digits = 3), ", eta=", round(eta, 
                digits = 3)), contour = TRUE, labels = FALSE, 
        region = TRUE, col.regions = rainbow(100, end = 0.33), 
        bg = "white")
}

"print.ICEepmap" <-
function (x, ...) 
{
    cat("\nICEepmap: Economic Preference Map...\n")
    cat("Shadow Price of Health, Lambda:", x$lambda, "\n")
    cat("Returns-to-Scale Power, Beta:", x$beta, "\n")
    cat("Preference Shape Power, Gamma:", x$gamma, "\n")
    eta <- x$gamma/x$beta
    if (eta > 3 + 2 * sqrt(2)) {
        cat("Power Parameter Ratio, Eta:", eta, " > Omega = 3+2*sqrt(2)\n")
        cat("This is a Highly Directional ICE map that lacks Monotonicity.\n")
    }
    else if (eta < 3 - 2 * sqrt(2)) {
        cat("Power Parameter Ratio, Eta:", eta, " < 1/Omega = 3-2*sqrt(2)\n")
        cat("This is a Roundish ICE map that lacks Monotonicity.\n")
    }
    else {
        cat("Power Parameter Ratio, Eta:", eta, "\n")
        cat("This is an ICE map with Monotonicity and non-negative Willingness.\n")
    }
}
