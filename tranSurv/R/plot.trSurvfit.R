#' Plot the survival estimation based on the structural transformation model
#'
#' Plot the survival estimation for an \code{trSurvfit}/\code{trReg} object.
#'
#' @keywords Plot
#' @export
#'
#' @param x an object of class \code{trSurvfit} returned by the \code{trSurvfit()} or the \code{trReg()} function.
#' @param ... graphical parameters to be passed to methods.
#' 
#' @return A \code{ggplot} object.
#' @example inst/examples/ex_plot_trSurvfit.R
#' 
plot.trSurvfit <- function(x, ...) {
    par(mar = c(3.5, 3.5, 2.5, 2.5))
    with(x$surv, plot(Time, trSurv, xlab = "", ylab = "", "s"))
    mtext(expression(bold("Survival estimation")), 3, line = .5, cex = 1.2)
    title(xlab = "Time", ylab = "Survival probability", line = 2, cex.lab = 1)
    with(x$surv, lines(Time, kmSurv, lty = 2, "s"))
    legend("topright", c("Transformation model", "Kaplan-Meier estimate"), lty = 1:2, bty = "n")
}

#' @export
plot.trReg <- function(x, ...) {
    S0 <- survfit(Surv(start, stop, status) ~ 1, data = x$.data)
    x$.data$tran <- x$tFun(x$.data$stop, x$.data$start, x$a)
    S1 <- survfit(Surv(tran, stop, status) ~ 1, data = x$.data)
    par(mar = c(3.5, 3.5, 2.5, 2.5))
    with(S1, plot(time, surv, xlab = "", ylab = "", "s"))
    mtext(expression(bold("Survival estimation")), 3, line = .5, cex = 1.2)
    title(xlab = "Time", ylab = "Survival probability", line = 2, cex.lab = 1)
    with(S0, lines(time, surv, lty = 2, "s"))
    legend("topright", c("Transformation model", "Kaplan-Meier estimate"), lty = 1:2, bty = "n")
}

