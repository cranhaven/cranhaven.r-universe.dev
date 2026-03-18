"ICEcolor" <-
function (ICEw, lfact = 1, beta = 1, gamma = 3 + 2 * sqrt(2)) 
{
    if (missing(ICEw) || !inherits(ICEw, "ICEwedge")) 
        stop("The first argument to ICEcolor must be an existing ICEwedge object.")
    if (lfact <= 0) 
        stop("The lfact argument to ICEcolor must be strictly positive.")
    if (beta <= 0) 
        stop("The beta argument to ICEcolor must be strictly positive.")
    if (gamma <= 0) 
        stop("The gamma = eta*beta argument to ICEcolor must be strictly positive.")
    lambda <- lfact * ICEw$lambda
    t1 <- ICEw$t1
    ceunit <- ICEw$ceunit
    conf <- ICEw$conf
    axys <- ICEw$axys
    xmax <- ICEw$xmax
    ymax <- ICEw$ymax
    if (lfact != 1) {
        if (ceunit == "cost") {
            t1[1] <- t1[1] * lfact
            axys[, 2] <- axys[, 2] * lfact
            xmax <- xmax * lfact
        }
        else {
            t1[2] <- t1[2]/lfact
            axys[, 3] <- axys[, 3]/lfact
            ymax <- ymax/lfact
        }
    }
    r <- as.vector(sqrt(axys[, 2]^2 + axys[, 3]^2))
    dif <- (axys[, 2] - axys[, 3])/sqrt(2)
    abcos <- abs(dif)/r
    pref <- sign(dif) * r^beta * abcos^gamma
    ICEclwol <- list(lambda = lambda, beta = beta, gamma = gamma, 
        ceunit = ceunit, axys = axys, conf = conf, pref = pref, xmax = xmax, 
        ymax = ymax, jlo = ICEw$jlo, kup = ICEw$kup)
    class(ICEclwol) <- "ICEcolor"
    ICEclwol
}

"plot.ICEcolor" <-
function (x, alibi = FALSE, show = "Both", ...) 
{
    if (missing(x) || !inherits(x, "ICEcolor")) 
        stop("The first argument to plot.ICEcolor must be an ICEcolor object.")
    if (show != "Hist" && show != "RBOW")
        show <- "Both"
    cv <- rainbow(12, start = 0, end = 0.33)
    pmax <- max(c(abs(max(x$pref, na.rm = TRUE)), abs(min(x$pref, 
        na.rm = TRUE))))
    eta <- x$gamma/x$beta
    if (show == "Both" || show == "Hist") {
        hist(x$pref[x$axys[, 4] == 1], main = "", xlab = "Preference Score")
        title(main = "Economic Preference Distribution within ICE Wedge", 
            font.main = 3)
    }
    if (show == "Both") {
        cat("\nICEcolor ...Press ENTER to display the ICEcolor MAP.\n")
        scan()  # This PAUSE allows the user to SAVE the displayed PREFERENCE distribution.
    }
    if (show == "Both" || show == "RBOW") {
        if (alibi == FALSE) {
            plot(x$axys[x$axys[, 4] == 1, 2], x$axys[x$axys[, 4] == 1, 
                3], main = "ICE Alias Wedge with Preference Colors", 
                xlab = "Effectiveness Difference", ylab = "Cost Difference", 
                sub = paste("lambda =", round(x$lambda, digits = 3), ", beta =", 
                    round(x$beta, digits = 3), ", gamma =", round(x$gamma, 
                    digits = 3), ", eta =", round(eta, digits = 3)), 
                xlim = c(-1 * x$xmax, x$xmax), ylim = c(-1 * x$ymax, x$ymax),
                pch = 20, bg = "white", col = cv[round(5.5 * (x$pref[x$axys[, 
                    4] == 1] + pmax)/pmax) + 1])
            points(x$axys[x$axys[, 4] == 0, 2], x$axys[x$axys[, 4] == 0, 3],
                col = "black", pch = 20)
            par(lty = 1)
            abline(h = 0, v = 0)
            mfac <- 10 * max(x$xmax, x$ymax)/sqrt(x$axys[x$kup, 2]^2 + 
                x$axys[x$kup, 3]^2)
            xray <- c(0, x$axys[x$kup, 2]) * mfac
            yray <- c(0, x$axys[x$kup, 3]) * mfac
            lines(xray, yray)
            mfac <- 10 * max(x$xmax, x$ymax)/sqrt(x$axys[x$jlo, 2]^2 + 
                x$axys[x$jlo, 3]^2)
            xray <- c(0, x$axys[x$jlo, 2]) * mfac
            yray <- c(0, x$axys[x$jlo, 3]) * mfac
            lines(xray, yray)
            par(lty = 3)
            abline(c(0, 1))
            }
        else {
            amax = max(x$xmax, x$ymax)
            plot(x$axys[x$axys[, 4] == 1, 2], x$axys[x$axys[, 4] == 1, 
                3], main = "ICE Alibi Wedge with Preference Colors", 
                xlab = "Effectiveness Difference", ylab = "Cost Difference", 
                sub = paste("lambda =", round(x$lambda, digits = 3), ", beta =", 
                    round(x$beta, digits = 3), ", gamma =", round(x$gamma, 
                    digits = 3), ", eta =", round(eta, digits = 3)), 
                xlim = c(-amax, amax), asp = 1, pch = 20, bg = "white",
                col = cv[round(5.5 * (x$pref[x$axys[, 4] == 1] + pmax)/pmax) + 1])
            points(x$axys[x$axys[, 4] == 0, 2], x$axys[x$axys[, 4] == 0, 3],
                col = "black", pch = 20)
            par(lty = 1)
            abline(h = 0, v = 0)
            mfac <- 10 * max(x$xmax, x$ymax)/sqrt(x$axys[x$kup, 2]^2 + 
                x$axys[x$kup, 3]^2)
            xray <- c(0, x$axys[x$kup, 2]) * mfac
            yray <- c(0, x$axys[x$kup, 3]) * mfac
            lines(xray, yray)
            mfac <- 10 * max(x$xmax, x$ymax)/sqrt(x$axys[x$jlo, 2]^2 + 
                x$axys[x$jlo, 3]^2)
            xray <- c(0, x$axys[x$jlo, 2]) * mfac
            yray <- c(0, x$axys[x$jlo, 3]) * mfac
            lines(xray, yray)
            par(lty = 3)
            abline(c(0, 1))   # show as "standardized" x$lambda == 1
            }
        par(lty = 1)
    }
}

"print.ICEcolor" <-
function (x, ...) 
{
    cat("\nICEcolor: Economic Preference Coloring of ICE Uncertainty Distribution...\n")
    cat(paste("Shadow Price of Health, lambda:", x$lambda, "\n"))
    cat(paste("Black Points are outside ICE Wedge with Confidence =", 
        100 * x$conf, "%\n"))
    cat(paste("Returns-to-Scale Power, beta:", x$beta, "\n"))
    cat(paste("Preference Shape Power, gamma:", round(x$gamma, 
        digits = 3), "\n"))
    eta <- x$gamma/x$beta
    if (eta > 3 + 2 * sqrt(2)) {
        cat("Power Parameter Ratio, Eta:", round(eta, digits = 3), 
            " > Omega = 3+2*sqrt(2)\n")
        cat("Uncertainty Scatter colored with Highly Directional Preferences lacking Monotonicity.\n\n")
    }
    else if (eta < 3 - 2 * sqrt(2)) {
        cat("Power Parameter Ratio, Eta:", round(eta, digits = 3), 
            " < 1/Omega = 3-2*sqrt(2)\n")
        cat("Uncertainty Scatter colored with Roundish Preferences lacking Monotonicity.\n\n")
    }
    else {
        cat("Power Parameter Ratio, Eta:", round(eta, digits = 3), 
            "\n")
        cat("Uncertainty Scatter colored with Monotonic Preferences & Non-Negative Willingness.\n\n")
    }
    summary(x$pref[x$axys[, 4] == 1])
}
