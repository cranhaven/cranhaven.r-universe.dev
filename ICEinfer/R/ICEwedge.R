"ICEwedge" <-
function (ICEu, lfact = 1, conf = 0.95) 
{
    if (missing(ICEu) || !inherits(ICEu, "ICEuncrt")) 
        stop("The first argument to ICEwedge must be an ICEuncrt object.")
    if (lfact < 0)
        stop("The lfact argument to ICEwedge must be non-negative.")
    if (conf < 0.5 || conf > 0.99) 
        stop("Wedge Confidence Level must be within [0.50, 0.99].")
    ceunit <- ICEu$ceunit
    R <- ICEu$R
    t <- ICEu$t
    t1 <- ICEu$t1	
    iru <- ICEu$icer.unbi
    ab <- "alibi"
    if (lfact == 0) {
        lfact <- max(c(abs(max(t[, 2])), abs(min(t[, 2])))) /
                 max(c(abs(max(t[, 1])), abs(min(t[, 1]))))
        ab <- "alias"                
    }
    lambda <- lfact * ICEu$lambda
    if (lfact != 1) {
        if (ceunit == "cost") {
            t1[1] <- t1[1] * lfact
            t[,1] <- t[,1] * lfact
        }
        else {
            t1[2] <- t1[2]/lfact
            t[,2] <- t[,2]/lfact
        }
    }
    ICEwdgol <- list(ICEinp = deparse(substitute(ICEu)), lambda = lambda, 
        lfact = lfact, ceunit = ceunit, conf = conf)
    t1u <- t1[2] * iru       # value of t1[1] yielding unbiased "ia1" angle
    ia1 <- ICEangle(t1u, t1[2])
    ia <- rep(90, R)
    for (i in 1:R) ia[i] <- ICEangle(t[i, 1], t[i, 2])
    axys <- data.frame(cbind(ia, t, rep(0, R)))
    axys <- axys[do.call(order, axys), ]
    if (ia1 < 0) {
        if (ia1 < axys[1, 1]) 
            center <- 1
        else {
            for (j in 1:(R - 1)) {
                i <- j + 1
                if (axys[j, 1] <= ia1 && ia1 < axys[i, 1]) {
                  center <- j
                  break
                }
            }
        }
    }
    else {
        if (ia1 > axys[R, 1]) 
            center <- R
        else {
            for (j in (R - 1):1) {
                i <- j + 1
                if (axys[j, 1] < ia1 && ia1 <= axys[i, 1]) {
                  center <- j
                  break
                }
            }
        }
    }
    j <- center - floor(R * conf/2)
    if (j < 1) 
        j <- j + R
    k <- j + floor(R * conf)
    if (k > R) 
        k <- k - R
    subangle <- axys[k, 1] - axys[j, 1]
    if (subangle < 0) 
        subangle <- subangle + 360
    if (j < k) 
        axys[j:k, 4] <- 1
    else {
        axys[1:k, 4] <- 1
        axys[j:R, 4] <- 1
    }
    xmax <- max(c(abs(max(axys[, 2])), abs(min(axys[, 2]))))
    ymax <- max(c(abs(max(axys[, 3])), abs(min(axys[, 3]))))
    ICEwdgol <- c(ICEwdgol, list(R = R, axys = axys, t1 = t1, iru = iru,
        ia1 = ia1, center = center, jlo = j, kup = k, subangle = subangle, 
        xmax = xmax, ymax = ymax, ab = ab))
    class(ICEwdgol) <- "ICEwedge"
    ICEwdgol
}

"plot.ICEwedge" <-
function (x, ...) 
{
    if (missing(x) || !inherits(x, "ICEwedge")) 
        stop("The first argument to plot.ICEwedge must be an ICEwedge object.")
    plot(x$axys[, 2], x$axys[, 3], ann = FALSE, type = "p", pch = 20, 
        col = c("black", "cyan")[unclass(x$axys[, 4]) + 1], bg = "white", 
        ylim = c(-x$ymax, x$ymax), xlim = c(-x$xmax, x$xmax))
    par(lty = 1)
    abline(v = 0)
    abline(h = 0)
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
    par(lty = 2)
    mfac <- 10 * max(x$xmax, x$ymax)/sqrt(x$axys[x$center, 2]^2 + 
        x$axys[x$center, 3]^2)
    xray <- c(0, x$axys[x$center, 2]) * mfac
    yray <- c(0, x$axys[x$center, 3]) * mfac
    lines(xray, yray)
    par(lty = 1)
    title(main = paste("ICE Wedge with Confidence =", 
        100 * x$conf, "%"), xlab = "Effectiveness Difference", 
        ylab = "Cost Difference", sub = paste("Units =", x$ceunit, 
            "; lambda =", round(x$lambda, digits = 3), "; Angles =", x$ab))
}

"print.ICEwedge" <-
function (x, ...) 
{
    cat("\nICEwedge: Incremental Cost-Effectiveness Bootstrap Confidence Wedge...\n")
    cat(paste("\nShadow Price of Health, lambda =", x$lambda))
    cat(paste("\nShadow Price of Health Multiplier, lfact =", 
        x$lfact))
    cat(paste("\nICE Differences in both Cost and Effectiveness expressed in", 
        x$ceunit, "units."))
    cat(paste("\nICE Angle of the Observed Outcome =", round(x$ia1, 
        digits = 3)))
    cat(paste("\nICE Ratio of the Observed Outcome =", round(x$t1[2]/x$t1[1], 
        digits = 5)))
    cat(paste("\nCount-Outwards  Central ICE Angle Order Statistic =", 
        x$center, "of", x$R))
    cat(paste("\nCounter-Clockwise Upper ICE Angle Order Statistic =", 
        x$kup))
    cat(paste("\nCounter-Clockwise Upper ICE Angle =", round(x$axys[x$kup, 
        1], digits = 3)))
    cat(paste("\nCounter-Clockwise Upper ICE Ratio =", round(x$axys[x$kup, 
        3]/x$axys[x$kup, 2], digits = 5)))
    cat(paste("\n    Clockwise     Lower ICE Angle Order Statistic =", 
        x$jlo))
    cat(paste("\n    Clockwise     Lower ICE Angle =", round(x$axys[x$jlo, 
        1], digits = 3)))
    cat(paste("\n    Clockwise     Lower ICE Ratio =", round(x$axys[x$jlo, 
        3]/x$axys[x$jlo, 2], digits = 5)))
    cat(paste("\nICE Angle Computation Perspective =", x$ab))
    cat(paste("\nConfidence Wedge Subtended ICE Polar Angle =", 
        round(x$subangle, digits = 3)))        
    cat("\n\n")
}
