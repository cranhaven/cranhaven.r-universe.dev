aiStaHau <- function(x, instr, choice = FALSE, 
  exog = c("none", "all", "partial"), shift.new = NULL)
{
    if (!inherits(x, "aiStaFit")) {stop("Need an 'aiStaFit' object.\n")}
    if (!inherits(instr, "ts")) {stop("Need a time series for 'instr'.\n")}
    if (!identical(tsp(x$y), tsp(instr))) {
        stop("x and instr should have the same imension.\n")}
    frq <- tsp(x$y)[3]
       
    # 1. Data and formula -------------------------------------------------
    # shift variables (dummy, seasonality): none, all, partial from x$y
    exog <- match.arg(arg = exog, choices = c("none", "all", "partial"))
    if (exog == "none") {
      shift.new <- NULL
    } else if (exog == "all") {
      shift.new <- x$shift
    } else {
      pos <- match(shift.new, x$shift)
      if (any(is.na(pos))) {stop("'shift.new' is not within 'x$shift'.\n")}
    }
    
    d <- choice + 0  # whether price and instr should be lagged by one
    m.ins <- deparse(substitute(instr))
    exp.fit <- paste(x$expen, ".fit", sep = "")
    nam <- c(paste(x$expen, ".t_", 0, sep = ""),
             paste(x$expen, ".t_", 1, sep = ""),
             paste(x$price, ".t_", d, sep = ""),
             paste(m.ins,   ".t_", d, sep = ""))
    if (!is.null(shift.new)) {
      nam <- c(nam, paste(shift.new, ".t_", 0, sep = ""))
    }
    daIns <- ts.union(x$y, log(instr))
    colnames(daIns) <- c(colnames(x$y), m.ins)    
    daHau <- bsLag(daIns, lag = 1)[, nam]
    formuHau <- as.formula(paste(nam[1], "~."))

    # 2. Auxiliary linear reg, residual, and predicted value --------------
    aux <- lm(formula = formuHau, data = daHau)
    res <- ts(data = residuals(aux), start = start(daHau), frequency = frq)
    fit <- ts(data = fitted(aux),    start = start(daHau), frequency = frq)
    daFit <- window(x = ts.union(x$y, res, fit), 
      start = start(x$y) + c(0, 1), frequency = frq)
    colnames(daFit) <- c(colnames(x$y), "resid", exp.fit)

    # 3. Likelihood ration test -------------------------------------------
    aiBase <- aiStaFit(y = daFit, share = x$share, price = x$price, 
      expen = x$expen, shift=x$shift, omit=x$omit, hom=FALSE, sym=FALSE)
    aiHaus <- update(aiBase, shift = c(x$shift, "resid"))
    ratio <- lrtest(aiBase$est, aiHaus$est)

    # 4. ouput ------------------------------------------------------------
    result <- listn(daHau, formuHau, regHau = aux, daFit, 
      aiBase, aiHaus, ratio)
    class(result) <- c("aiStaHau", "aiFit")
    return(result)
} 