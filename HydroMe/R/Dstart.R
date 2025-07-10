Dstart =function (data)
{
    xy <- data.frame(data)
    n <- nrow(xy)
    nlast <- max(3, round(n/2))
    y1 <- xy[1, ][["y"]]
    yn <- xy[n, ][["y"]]
    dfirst <- y1[1]
    dlast  <- yn[1]
    Thr1 <- ifelse(((y1 - yn) < y1/2), 0.01, yn)
    Ths1 <- dfirst
    dmid <- xy[(n/2 - 2):(n/2 + 1), ]
    pars2 <- coef(lm(y ~ log(x), data = dmid))
    ymid <- xy[1:nlast, ][["y"]][nlast]
    ax <- (ymid - pars2[1])/pars2[2]
    slopep <- pars2[2]/(dfirst - dlast)
    m1 <- ifelse(abs(slopep) < 1,
                 1 - exp(-0.8 * abs(slopep)),
                 1 - (0.5755/abs(slopep)) + (0.1/abs(slopep)^2) +(0.025/abs(slopep)^3))
    scal1 <- 1/(1 - m1)
    alp1 <- (((2^(1/m1)) - 1)^(1 - m1))/exp(ax)
    pars <- list(thr = Thr1, ths = Ths1, alp = alp1, nscal = scal1, mscal=m1)
    pars <- (c(pars))
   as.numeric(pars)
}
