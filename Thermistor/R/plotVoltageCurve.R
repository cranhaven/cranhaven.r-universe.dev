#' @title Plot the V-DeltaT Curve
#' @description Plot the temperature-change-voltage curve under a particular
#'    components setting v.s. the target curve. using \code{ggplot}.
#' @param Tdata a vector of temperature-change values
#' @param OnlyTarget logical. If \code{TRUE}, plot the target curve only.
#' @param Pdata the values returning by \code{voltageCurve} or
#'    \code{tempCompCurve}
#'
#' @return the graph
#' @export
#'
#' @examples
#' ### only target curve
#' Tdata <- seq(-40, 85, by = 5)
#' plot_voltageCurve(Tdata)
#' ### a particular curve and the target curve
#' data(CompValues)
#' Tdata <- seq(-40, 85, by=5)
#' R_id <- c(43, 36, 29, 15, 9, 3)
#' Res <- CompValues$Res
#' ThVal <- CompValues$ThVal
#' ThBeta <- CompValues$ThBeta
#' Vnew <- voltageCurve(Tdata, R_id, Res, ThVal, ThBeta)
#' plot_voltageCurve(Tdata, OnlyTarget = FALSE, Pdata = Vnew)
plot_voltageCurve <- function(Tdata, OnlyTarget = TRUE, Pdata = NULL){
  Vdata <- 1.026e-1 + -1.125e-4 * Tdata + 1.125e-5 * Tdata^2
  xid <- Vnew <- NULL
  if(OnlyTarget == TRUE){
    plotdata <- data.frame(xid = Tdata, Vdata = Vdata)
    ggplot2::ggplot(data = plotdata) + ggplot2::geom_line(ggplot2::aes(x = xid, y = Vdata), colour = "black") +
      ggplot2::xlab(bquote('temperature change ('~Delta*~'T)')) + ggplot2::ylab("voltage (V)")+
      ggplot2::scale_x_continuous(breaks = seq(min(Tdata), max(Tdata), by = 20),
                         labels = seq(min(Tdata), max(Tdata), by = 20)) +
      ggplot2::theme_set(ggplot2::theme_bw()) +
      ggplot2::theme(text=ggplot2::element_text(size = 10.5), legend.position = "top",
            legend.background = ggplot2::element_blank(), panel.grid = ggplot2::element_blank())
  }else{

    plotdata <- data.frame(xid = Tdata, Vdata = Vdata, Vnew = Pdata)
    ggplot2::ggplot(data = plotdata) + ggplot2::geom_line(ggplot2::aes(x = xid, y = Vdata, colour = "target",
                                          linetype = "target",shape = "target")) +
      ggplot2::geom_line(ggplot2::aes(x = xid, y = Vnew, colour = "particular", linetype = "particular")) +
      ggplot2::geom_point(ggplot2::aes(x = xid, y = Vnew, colour = "particular", shape = "particular" )) +
      ggplot2::scale_colour_manual("",values = c("target" = "black", "particular"='#FF6666')) +
      ggplot2::scale_shape_manual("",values = c("target" = NA, "particular" = 5)) +
      ggplot2::scale_linetype_manual("", values = c("target" = 1, "particular" = 2)) +
      ggplot2::xlab(bquote('temperature change ('~Delta*~'T)')) + ggplot2::ylab("voltage (V)") +
      ggplot2::scale_x_continuous(breaks = seq(min(Tdata), max(Tdata), by = 20),
                         labels = seq(min(Tdata), max(Tdata), by = 20)) +
      ggplot2::theme_set(ggplot2::theme_bw()) +
      ggplot2::theme(text=ggplot2::element_text(size=10.5),legend.position = "top",
            legend.background = ggplot2::element_blank(), panel.grid = ggplot2::element_blank())
  }
}
