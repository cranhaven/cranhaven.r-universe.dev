looicplot <- function(looiclist, modnames, perc = 90) {
    n <- length(looiclist)
    model <- numeric(n)
    looic <- numeric(n)
    loose <- numeric(n)
    ic <- numeric(n)
    se <- numeric(n)
    lwr <- numeric(n)
    upr <- numeric(n)
    for(i in 1:n) {
        fit <- get(looiclist[[i]])
        LL <- log_lik(fit)
        ref <- relative_eff(LL, chain_id=rep(1, 4000))
        Loo <- loo(LL, r_eff = ref, k_threshold = 7, cores = 1)
        looic[i] <- Loo$estimates[3, 1]
        loose[i] <- Loo$estimates[3, 2]
        upr[i] <- looic[i] + loose[i] * (-qnorm(((100 - perc)/2)/100))
        lwr[i] <- looic[i] - loose[i] * (-qnorm(((100 - perc)/2)/100))
    }
    df <- data.frame("model" = modnames, "looic"=looic, "se"=loose,
                     "lwr" = lwr, "upr" = upr)
    rdr <- rev(order(df$looic))
    df <- df[rdr, ]
    xmn <- min(df$lwr)
    xmx <- max(df$upr)
    print(df)
    sumchart(df, df$model, perc = perc)
    invisible(df)
}
sumchart <- function(df, rownames=df$modnames, groups=rownames[1], perc=90) {
   n <- dim(df)[1]
   mn <- min(df$lwr) 
   mx <- max(df$upr) 
   xrng <- c(mn, mx)
   xtr1 <- floor(xrng[1]/10)
   xtr2 <- ceiling(xrng[2]/10)
   xrng <- c(mn - xtr1, mx + xtr2)
   dotchart(df$looic, labels=rownames, xlim=xrng, xlab="Values")
   title("LOOIC-values")
   for(i in 1:n){
       segments(df$lwr[i], i, df$upr[i], i, lwd=4, col="skyblue")
       }
   }
        
