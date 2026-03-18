# main plotting function
plot.mrss <- plot.power <- plot.mdes <- function(x, ypar = "power", xpar = NULL,
                                    xlim = NULL, ylim = NULL,
                                    xlab = NULL, ylab = NULL,
                                    main = NULL, sub = NULL,
                                    locate = FALSE, ...){

  if(any(c("med211", "med221", "med311", "med321", "med331", "med_pn21", "med_pn31", "med_pn32") %in% class(x))) {

    if(ypar == "mdes") {
      warning("MDES plots are not available for mediation designs", call. = FALSE)
    }

    .plot.med(x, ypar = "power", xpar = xpar,
              xlim = xlim, ylim = ylim, locate = locate, ...)

  } else {

    if(!ypar %in% c("mdes", "power")){
      stop("Incorrect value for argument 'ypar'", call. = FALSE)
    }

    # design characteristics
    fun.parsed <- scan(text = x$fun, what = "character", sep=".", quiet = TRUE)
    design <- fun.parsed[2]
    nlevels <- substr(design, nchar(design) - 2, nchar(design) - 2)
    if(substr(design, 1, nchar(design) - 3) == "mod") {
      block <- "r"
    } else {
      block <- substr(design, nchar(design) - 1, nchar(design) - 1)
    }

    # default xpar if NULL
    if(is.null(xpar)) {
      if(block == "r") {
        xpar <- switch(nlevels,
                       "1" = "n",
                       "2" = "J",
                       "3" = "K",
                       "4" = "L")
      } else {
        xpar <- switch(nlevels,
                       "2" = "n",
                       "3" = "J",
                       "4" = "K")
      }
    } else {
      if(!xpar %in% c("n","J","K","L")[1:nlevels]){
        stop("Incorrect value for argument 'xpar'", call. = FALSE)
      }
    }

    # object conversions
    capture.output({
      if(ypar == "mdes") {
        if(inherits(x, "mrss"))  x <- mrss.to.mdes(x)
        if(inherits(x, "mdes"))  x <- x
        if(inherits(x, "power"))  x <- power.to.mdes(x)
      } else {
        if(inherits(x, "mrss"))  x <- mrss.to.power(x)
        if(inherits(x, "mdes"))  x <- mdes.to.power(x)
        if(inherits(x, "power"))  x <- x
      }
    })

    # default xlim if NULL
    if(is.null(xlim)) {
      if(substr(design, nchar(design) - 2, nchar(design) - 1) %in% c("22", "33")) {
        xlim <-  c(max(x$parms$g + 5, .5 * x$parms[[xpar]]), 1.5 * x$parms[[xpar]])
      } else {
        xlim <-  c(max(x$parms$g + 3, .5 * x$parms[[xpar]]), 1.5 * x$parms[[xpar]])
      }
    } else {
      if(xlim[1] <= 0 || !is.numeric(xlim) || length(xlim) > 2) {
        stop("Incorrect value for argument 'xlim'", call. = FALSE)
      }
    }

    if(xlim[2] - xlim[1] > 20) {
      xseq <- seq(xlim[1], xlim[2], .25)
    } else {
      xseq <- seq(xlim[1], xlim[2], .125)
    }

    # current values
    names.parms <-  names(x$parms)
    idx <- match(xpar, names.parms)
    x0 <- x$parms[[idx]]
    idy <- match(ypar, names(x))
    y0 <- ifelse(ypar == "power", x[[idy]], x[[idy]][1])

    # plot data
    yout <- matrix(NA, nrow = length(xseq), ncol = 3)
    for(i in 1:nrow(yout)){
      x$parms[idx] <- xseq[i]
      capture.output(
        if(ypar == "mdes"){
          yout[i,] <- do.call(x$fun, x$parms)$mdes
        }else if(ypar == "power"){
          yout[i,1] <- do.call(x$fun, x$parms)$power
        }
      )
    }

    # default ylim if NULL
    if(is.null(ylim)) {
      ifelse(ypar == "mdes", ylim <- range(min(yout[,2]), max(yout[,3])), ylim <- c(0,1))
    }

    # labels
    ifelse(!is.null(ylab), ylab,
           ifelse(ypar == "mdes",
                  ifelse(substr(design, 1, nchar(design) - 3) == "mod",
                         ylab <- "Minimum Detectable Effect Size Difference",
                         ylab <- "Minimum Detectable Effect Size"),
                  ylab <- "Statistical Power"))


    # plot
    plot.new()
    plot.window(xlim = range(xseq),
                ylim = ylim, ...)
    polygon(c(rev(xseq), xseq), c(rev(yout[,3]), yout[,2]), border = NA,
            col = adjustcolor(4, alpha.f = 0.2))
    lines(xseq, yout[,1], col = adjustcolor(4, alpha.f = 0.5), lty = 1, lwd = 2)
    if(ypar == "mdes") {
      lines(xseq, yout[,2], col = adjustcolor(4, alpha.f = 0.2), lty = 1, lwd = 1.5)
      lines(xseq, yout[,3], col = adjustcolor(4, alpha.f = 0.2), lty = 1, lwd = 1.5)
    }
    title(main = main, sub = sub,
          xlab = ifelse(!is.null(xlab), xlab, xpar),
          ylab = ylab)
    axis(1)
    axis(2)
    box()

    # locate parameters for the current design
    if(locate) {
      points(x0, y0, pch=21, bg = adjustcolor(2, alpha.f = 0.5), cex=1.5)
      abline(v = x0, lty = 5, col = adjustcolor(2, alpha.f = 0.5))
    }

    # benchmark values
    abline(h = ifelse(ypar == "mdes", .20, .80), lty = 5, col = adjustcolor(2, alpha.f = 0.5))

  }

}


# plots for mediation effects
.plot.med <- function(x, ypar = "power", xpar = NULL,
                      xlim = NULL, ylim = NULL,
                      locate = FALSE, ...){

  # overwrite MC specification for efficiency
  x$parms$mc <- FALSE

  # if xpar = NULL select the top level
  if(is.null(xpar)) {

   if(class(x)[2] %in% c("med_pn21", "med_pn31", "med_pn32")) {
     ifelse(inherits(x, c("med_pn21")),
            xpar <- "J_trt",
            xpar <- "K_trt")
   } else {
     ifelse(inherits(x, c("med211", "med221")),
            xpar <- "J",
            xpar <- "K")
   }

  } else {

    if(!xpar %in% c("n", "n_trt", "n_ctrl", "J", "J_trt", "K", "K_trt")){
      stop("Incorrect value for argument 'xpar'", call. = FALSE)
    }

  }

  # default xlim if NULL
  if(is.null(xlim)) {

    if(class(x)[2] %in% c("med_pn21", "med_pn31", "med_pn32")) {
      if(xpar %in% c("n", "n_ctrl", "n_trt")){
        if(xpar == "n") warning("Assuming `n` is defined for treatment arm. Otherwise specify `n_ctrl`", call. = FALSE)
        ifelse(xpar %in% c("n","n_trt"),
               xlim <-  c(max(6, .5 * x$parms$n_trt), 1.5 * x$parms$n_trt),
               xlim <-  c(max(6, .5 * x$parms$n_ctrl), 1.5 * x$parms$n_ctrl))
      } else if(xpar %in% c("J", "J_trt")) {
        xlim <-  c(max(6, .5 * x$parms$J_trt), 1.5 * x$parms$J_trt)
      } else if(xpar %in% c("K", "K_trt")) {
        xlim <-  c(max(7, .5 * x$parms$K_trt), 1.5 * x$parms$K_trt)
      }
    } else {
      if(xpar == "n"){
        xlim <-  c(max(6, .5 * x$parms$n), 1.5 * x$parms$n)
      } else if(xpar == "J") {
        xlim <-  c(max(6, .5 * x$parms$J), 1.5 * x$parms$J)
      } else if(xpar == "K") {
        xlim <-  c(max(7, .5 * x$parms$K), 1.5 * x$parms$K)
      }
    }

  } else {
    if(xlim[1] <= 0 || !is.numeric(xlim) || length(xlim) > 2) {
      stop("Incorrect value for argument 'xlim'", call. = FALSE)
    }
  }

  # computation intervals
  if(xlim[2] - xlim[1] > 20) {
    xseq <- seq(xlim[1], xlim[2], .25)
  } else {
    xseq <- seq(xlim[1], xlim[2], .125)
  }

  # indices for subsetting output
  if(inherits(x, "med211")) {
    idx.t <- 1:3
    idx.sj <- 4:6
  } else if(class(x)[2] %in% c("med221", "med311", "med321", "med331", "med_pn21", "med_pn31", "med_pn32")) {
    idx.t <- 1:2
    idx.sj <- 3
  }

  # current values
  names.parms <-  names(x$parms)
  idx <- match(xpar, names.parms)
  x0 <- x$parms[[idx]]
  y0.t <- x$power[idx.t,"t"]
  y0.sobel <- x$power[idx.sj,"sobel"]
  y0.joint <- x$power[idx.sj,"joint"]

  # value grid for plotting data
  yout <- matrix(NA, nrow = length(xseq), ncol = length(idx.t)^2)
  for(i in 1:nrow(yout)) {
    x$parms[idx] <- xseq[i]
    capture.output({
      pwr <- do.call(x$fun, x$parms)$power
      if(inherits(x, "med211")) {
        yout[i,1:3] <- pwr[idx.t, "t"]
        yout[i,4:6] <- pwr[idx.sj, "sobel"]
        yout[i,7:9] <- pwr[idx.sj, "joint"]
        # colnames(yout) <- c("a.t", "b1.t", "B.t",
        #                     "ab1.sobel", "ab2.sobel", "aB.sobel",
        #                     "ab1.joint", "ab2.joint", "aB.joint")
      } else if(inherits(x, "med221")){
        yout[i,1:2] <- pwr[idx.t, "t"]
        yout[i,3] <- pwr[idx.sj, "sobel"]
        yout[i,4] <- pwr[idx.sj, "joint"]
        # colnames(yout) <- c("a.t", "b.t", "ab.sobel", "ab.joint")
      } else if(class(x)[2] %in% c("med311", "med321", "med331", "med_pn21", "med_pn31", "med_pn32")) {
        yout[i,1:2] <- pwr[idx.t, "t"]
        yout[i,3] <- pwr[idx.sj, "sobel"]
        yout[i,4] <- pwr[idx.sj, "joint"]
        # colnames(yout) <- c("a.t", "B.t", "aB.sobel", "aB.joint")
      }
    })
  }

  # plots
  par(mfrow=c(3, 1))
  ylab <- c("Power for t-test", "Power for Sobel Test", "Power for Joint Test")

  for(i in 1:3) {

    # default ylim if NULL
    if(is.null(ylim)) {
      ifelse(i == 1,
             ylim <- range(yout),
             ylim <- range(yout))
    }

    ifelse(i < 3,  par(mar=c(4.5, 4.1, 2, 2)), par(mar=c(4.5, 4.1, 2, 2)))
    plot.new()
    plot.window(xlim = range(xseq), ylim = ylim, ...)

    if(i == 1) {

      if(inherits(x, "med211")) {
        lines(xseq, yout[, 1], col = adjustcolor(4, alpha.f = 0.5), lty = 1, lwd = 2)
        lines(xseq, yout[, 2], col = adjustcolor(4, alpha.f = 0.5), lty = 2, lwd = 2)
        lines(xseq, yout[, 3], col = adjustcolor(4, alpha.f = 0.5), lty = 3, lwd = 2)
        nlines <- 3
        legend.labels <- c(expression(a), expression(b[1]), expression(B))
        if(locate) {
          points(rep(x0, 3), y0.t, pch = 21, bg = adjustcolor(2, alpha.f = 0.5), cex = 1.5)
        }
      } else if(class(x)[2] %in% c("med221", "med311", "med321", "med331", "med_pn21", "med_pn31", "med_pn32")) {
        lines(xseq, yout[, 1], col = adjustcolor(4, alpha.f = 0.5), lty = 1, lwd = 2)
        lines(xseq, yout[, 2], col = adjustcolor(4, alpha.f = 0.5), lty = 2, lwd = 2)
        nlines <- 2
        ifelse(inherits(x, "med221"),
               legend.labels <- c(expression(a), expression(b)),
               legend.labels <- c(expression(a), expression(B)))
        if(locate) {
          points(rep(x0, 2), y0.t, pch = 21, bg = adjustcolor(2, alpha.f = 0.5), cex = 1.5)
        }

      }

    } else {
      ifelse(i == 2, y0 <- y0.sobel, y0  <- y0.joint)
      if(inherits(x, "med211")) {
        ifelse(i == 2, kidx <- 0, kidx  <- 3)
        lines(xseq, yout[, idx.sj[1] + kidx], col = adjustcolor(4, alpha.f = 0.5), lty = 1, lwd = 2)
        lines(xseq, yout[, idx.sj[2] + kidx], col = adjustcolor(4, alpha.f = 0.5), lty = 2, lwd = 2)
        lines(xseq, yout[, idx.sj[3] + kidx], col = adjustcolor(4, alpha.f = 0.5), lty = 3, lwd = 2)
        nlines <- 3
        legend.labels <- c(expression(a*b[1]), expression(a*b[2]), expression(a*B))
        if(locate) {
          points(rep(x0, 3), y0, pch=21, bg = adjustcolor(2, alpha.f = 0.5), cex=1.5)
        }
      } else if(class(x)[2] %in% c("med221", "med311", "med321", "med331", "med_pn21", "med_pn31", "med_pn32")) {
        ifelse(i == 2, kidx <- 0, kidx  <- 1)
        lines(xseq, yout[, idx.sj + kidx], col = adjustcolor(4, alpha.f = 0.5), lty = 1, lwd = 2)
        nlines <- 1
        ifelse(inherits(x, "med221"),
               legend.labels <- expression(a*b),
               legend.labels <- expression(a*B))
        if(locate) {
          points(x0, y0, pch = 21, bg = adjustcolor(2, alpha.f = 0.5), cex = 1.5)
        }
      }

    }

    if(locate) {
      abline(v = x0, lty = 5, col = adjustcolor(2, alpha.f = 0.5))
    }
    # benchmark for power
    abline(h = .80, lty = 5, col = adjustcolor(2, alpha.f = 0.5))

    legend("topright",
           legend.labels,
           lwd = rep(2, nlines), lty = 1:nlines, cex = 1,
           col = adjustcolor(4, alpha.f = 0.5))

    title(xlab = xpar,
          ylab = ylab[i])

    axis(1)
    axis(2)
    box()

  }

  # change par() back to the default values
  par(mfrow=c(1, 1), mar=c(5.1, 4.1, 4.1, 2.1))

}
