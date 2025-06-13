plot.sROCt <- function(x, tcr, xlab, ylab, type = "l", lwd = 5, ...)  {
  if (missing (tcr)){
   stop(message("Type of time-dependent ROC curve estimation should be indicated."))
  }
  else{
   if (!(tcr == "C" | tcr == "I" | tcr == "B")){
    stop(message("Only values C, I and B are available for tcr parameter."))
   }
   else{
    t <- length(unique(x$tcr))
    if (tcr == "B" & t < 2){
     warning("Only the available time-dependent ROC curve type will be plotted.", call. = FALSE)
     tcr <- x$tcr[1]
    }
    else{
     if(!(tcr == "B")){
       t <- length(which(x$tcr == tcr))
       if (t == 0){
        stop(message(paste("Not available", tcr, "/D ROC curve.")))
       }
     }
    }
   }
  }
  if (missing(xlab)){
   xlab <- "False-Positive Rate"
  }
  if (missing(ylab)){
    ylab <- "True-Positive Rate"
  }
  v <- unique(x$t)
  for (i in 1:length(v)) {
   d <- round(as.numeric(v[i]), 2)
   if(tcr == "B"){
   par(mfrow = c(1,2))
   par(mar = c(5,5,1,1))
   C <-  which(x$t == v[i] & x$tcr == "C")
   m <- paste("C/D ROC at time", d)
   plot(x$p[C], x$ROC[C], type = type, lwd = lwd, xlab = xlab, ylab = ylab, main = m, ...)
   lines(c(0,1),c(0,1),lty=2,col="gray")
   N <-  which(x$t == v[i] & x$tcr == "I")
   m <- paste("I/D ROC at time", d)
   plot(x$p[N], x$ROC[N], type = type, lwd = lwd, xlab = xlab, ylab = ylab, main = m, ...)
   lines(c(0,1),c(0,1),lty=2,col="gray")
   }
   else{
    par(mar = c(5,5,1,1))
    J <- which(x$t == v[i] & x$tcr == tcr )
    m <- ifelse (tcr == "C", paste ("C/D ROC curve at time: ", d), paste ("I/D ROC curve at time: ", d))
    plot(x$p[J], x$ROC[J], type = type, lwd = lwd, xlab = xlab, ylab = ylab, main = m, cex = 1.25, ...)
    lines(c(0,1),c(0,1),lty=2,col="gray")
   }
  }
}
