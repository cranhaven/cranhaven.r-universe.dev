"ICEalice" <-
function (ICEw) 
{
    if (missing(ICEw) || !inherits(ICEw, "ICEwedge")) 
        stop("The first argument to ICEalice must be an ICEwedge object.")
    lambda <- ICEw$lambda
    ceunit <- ICEw$ceunit
    ia <- as.vector(ICEw$axys[, 1])
    n <- length(ia)
    a11 <- c(45, 52.5, 60, 67.5, 75, 82.5, 90, 97.5, 105, 112.5, 
        120, 127.5, 135)
    wtp <- round(lambda * tan((a11 - 45)/57.29578), digits = 3)
    wtp[13] <- Inf
    wta <- round(lambda/tan((a11 - 45)/57.29578), digits = 3)
    wta[1] <- Inf
    acc <- as.matrix(cbind(a11, wtp, rep(0, 13), wta, rep(0, 
        13)))
    dimnames(acc) <- list(rep(1:13), c("ICEangle", "WTP", "VAGR", 
        "WTA", "ALICE"))
    neqcl <- swqcl <- 0    
    for (i in 1:n) {
        for (j in 1:13) {
            if (ia[i] <= acc[j, 1] && ia[i] >= acc[j, 1] - 180) 
                acc[j, 3] <- acc[j, 3] + 1
            if (ia[i] <= acc[j, 1] && ia[i] >= -acc[j, 1]) 
                acc[j, 5] <- acc[j, 5] + 1
        }
        if (ia[i] > 45 && ia[i] <= 135) 
            neqcl <- neqcl + 1
        if (ia[i] < -45 && ia[i] >= -135) 
            swqcl <- swqcl + 1     
    }
    acc[, 3] <- acc[, 3]/n
    acc[, 5] <- acc[, 5]/n
    neqcl <- neqcl/n
    swqcl <- swqcl/n
    qcl <- round(100 * as.vector(cbind(acc[1,5], neqcl, swqcl, 1-acc[13,5])), digits = 1)
    ICEaccol <- list(lambda = lambda, ceunit = ceunit, ia = ia, acc = acc, qcl = qcl)
    class(ICEaccol) <- "ICEalice"
    ICEaccol
}

"plot.ICEalice" <-
function (x, show = "Both", ...) 
{
    if (missing(x) || !inherits(x, "ICEalice")) 
        stop("The first argument to plot.ICEalice must be an ICEalice object.")
    if (show != "VAGR" && show != "Alice")
        show <- "Both"
    a7 <- c(45, 60, 75, 90, 105, 120, 135)
    x1 <- x$acc[, 1]
    x2 <- x$acc[, 2]
    x2[13] <- 4 * x2[12]
    y3 <- x$acc[, 3]
    y5 <- x$acc[, 5]
    if (show == "Both" || show == "VAGR") {
        matplot(x2, y3, type = "n", xlim = c(0, 3 * x2[12]), xlab = "ICE Willingness To Pay", 
            ylim = c(0, 1), ylab = "Acceptability", main = "VAGR Acceptability Curve")
        matlines(x2, y3)
    }	
    if (show == "Both") {	
        cat("\nICEplots ...Press ENTER to display the corresponding ALICE curve.\n")
        scan()  # This PAUSE allows the user to SAVE the currently displayed VAGR curve.
    }
    if (show == "Both" || show == "Alice") {
        matplot(x1, y5, type = "n", xlim = c(45, 135), xlab = "ICE Angle (theta)", 
            ylim = c(0, 1), ylab = "Acceptability", main = "ALICE Curve", 
            axes = FALSE)
        axis(1, a7)
        axis(2)
        box()
        matlines(x1, y5)
        abline(v = 90)
    }
}

"print.ICEalice" <-
function (x, ...) 
{
    cat("\nICEalice: Acceptability Curves from Bootstrap Uncertainty Distribution...\n")
    cat(paste("\nShadow Price of Health, lambda =", x$lambda))
    cat(paste("\nICE Differences in both Cost and Effectiveness expressed in", 
        x$ceunit, "units.\n\n"))
    print(x$acc)
    cat("\nICE Quadrant Confidence Level Percentages... (SE, NE, SW, NW)\n")
    print(x$qcl) 
    cat("\n")
}
