#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: plotmethod_lcc.R                                              #
# Contains: lccPlot function                                          #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 11/10/2017                                           #
# Last update: 29/07/2019                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################
##' @title Plot Fitted Curves from an \code{lcc} Object.
##'
##' @description A plot of predictions versus the time covariate is
##'   generated. Predicted values are joined by lines while sampled
##'   observations are represented by circles. If the argument
##'   \code{components=TRUE} is considered in the \code{lcc} object,
##'   single plots of each statistics are returned on differents pages.
##'
##' @usage
##' lccPlot(obj, type, control, ...)
##'
##' @param obj an object inheriting from class "lcc", representing a
##'   fitted lcc model.
##'
##' @param type character string. If \code{type = "lcc"}, the output is
##'   the LCC plot; if \code{type = "lpc"}, the output is the LPC plot;
##'   and if \code{type = "la"} the output is the LA plot. Types "lpc"
##'   and "la" are available only if \code{components = TRUE}.
##'
##' @param control a list of control values or character strings
##'   returned by the function \code{\link{plotControl}}. Defaults to an
##'   empty list.  The list may contain the following components:
##'   \describe{ \item{\code{shape}:}{draw points considering a shape
##'   parameter. Possible shape values are the numbers 0 to 25, and 32
##'   to 127; see
##'   \code{\link[ggplot2]{aes_linetype_size_shape}}. Default is
##'   \code{1}.}
##'
##' \item{\code{colour}:}{specification for lines color. Default is
##' \code{"black"}.}
##'
##' \item{\code{size}:}{specification for lines size. Should be
##' specified with a numerical value (in millimetres); see
##' \code{\link[ggplot2]{aes_linetype_size_shape}}. Default is
##' \code{0.5}.}
##'
##' \item{\code{xlab}:}{title for the \code{x} axis.  Default is
##' \code{"Time"}.}
##'
##' \item{\code{ylab}:}{title for the \code{y} axis. Default is "LCC",
##' "LPC", or "LA"}
##'
##' \item{\code{scale_y_continuous}:}{numeric vector of length two
##' providing limits of the scale. Default is
##' \code{c(0,1)}.}
##'
##' \item{\code{all.plot}:}{\code{viewport} functions for the \code{lcc}
##' class. If \code{TRUE}, the default, returns an object created by the
##' \code{\link[grid]{viewport}} function with multiple plots on a
##' single page. If \code{FALSE} returns a single
##' \code{\link[ggplot2]{ggplot}} object by different pages using the
##' \code{\link[gridExtra]{marrangeGrob}} function.}}
##'
##' @param ... arguments to be passed to
##'   \code{\link[ggplot2]{facet_wrap}} function
##'
##' @return No return value, called for side effects
##'
##' @author Thiago de Paula Oliveira, \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @references Lin, L. A Concordance Correlation Coefficient to
##'   Evaluate Reproducibility. \emph{Biometrics}, 45, n. 1, 255-268,
##'   1989.
##' @references Oliveira, T.P.; Hinde, J.; Zocchi S.S. Longitudinal
##'   Concordance Correlation Function Based on Variance Components: An
##'   Application in Fruit Color Analysis. \emph{Journal of
##'   Agricultural, Biological, and Environmental Statistics}, v. 23,
##'   n. 2, 233–254, 2018.
##'
##' @seealso \code{\link[lcc]{lcc}}.
##'
##' @examples
##'
##' data(hue)
##' ## Second degree polynomial model with random intercept, slope and
##' ## quadratic term
##' fm1<-lcc(data = hue, subject = "Fruit", resp = "H_mean",
##'          method = "Method", time = "Time", qf = 2, qr = 2,
##'          components=TRUE)
##' lccPlot(fm1, type="lcc")
##' lccPlot(fm1, type="lpc")
##' lccPlot(fm1, type="la")
##'
##' ## Using themes of ggplot2 package
##' lccPlot(fm1, type = "lpc")+
##'  ylim(0,1) +
##'  geom_hline(yintercept = 1, linetype = "dashed") +
##'  scale_x_continuous(breaks = seq(1,max(hue$Time),2))+
##'  theme_bw() +
##'  theme(legend.position = "none", aspect.ratio = 1,
##'   axis.line.x = element_line(color="black", size = 0.5),
##'   axis.line.y = element_line(color="black", size = 0.5),
##'   axis.title.x = element_text(size=14),
##'   axis.title.y = element_text(size=14),
##'   axis.text.x = element_text(size = 14, face = "plain"),
##'   axis.text.y = element_text(size = 14, face = "plain"))
##'
##' @examples
##' ## Using the key (+) to constructing sophisticated graphics
##' lccPlot(fm1, type="lcc") +
##'  scale_y_continuous(limits=c(-1, 1)) +
##'  labs(title="My title",
##'  y ="Longitudinal Concordance Correlation",
##'  x = "Time (Days)")
##'
##' @examples
##' ## Runing all.plots = FALSE and saving plots as pdf
##' \dontrun{
##' data(simulated_hue_block)
##' attach(simulated_hue_block)
##' fm2<-lcc(data = simulated_hue_block, subject = "Fruit",
##'          resp = "Hue", method = "Method",time = "Time",
##'          qf = 2, qr = 1, components = TRUE, covar = c("Block"),
##'          time_lcc = list(n=50, from=min(Time), to=max(Time)))
##' ggsave("myplots.pdf",
##'        lccPlot(fm2, type="lcc", scales = "free"))
##' }
##'
##' @export

lccPlot<-function(obj, type = "lcc", control = list(), ...){
  if (!inherits(obj, "lcc")) stop("Object must inherit from class \"lcc\"",
                             call.=FALSE)
  # Arguments for the plot
  plot.cons<-plotControl(shape=1, colour="black",
                         size=0.5, xlab = "Time",
                         ylab = "LCC")
  if (type == "lpc") plot.cons$ylab = "LPC"
  if (type == "la") plot.cons$ylab = "LA"
  if(length(control)){
    nms <- names(control)
    if (!is.list(control) || is.null(nms))
      stop("'control' argument must be a named list")
    pos <- pmatch(nms, names(plot.cons))
    if (any(nap <- is.na(pos))) {
      warning(sprintf(ngettext(length(nap), "unrecognized plot element named %s ignored",
        "unrecognized plot elements named %s ignored"),
        paste(sQuote(nms[nap]), collapse = ", ")), domain = NA)
      pos <- pos[!nap]
      control <- control[!nap]
    }
    for(i in seq_len(length(pos))){
      plot.cons[[pos[i]]]<-control[[i]]
    }
  }
  #---------------------------------------------------------------------
  #Standard arguments
  #---------------------------------------------------------------------
  nd<-obj$plot_info$nd
  model<-obj$model
  tk.plot<-obj$plot_info$tk.plot
  tk.plot2<-obj$plot_info$tk.plot2
  ldb<-obj$plot_info$ldb
  ci<-obj$plot_info$ci
  components<-obj$plot_info$components
  if (components == FALSE &  type != "lcc") {
    stop("'lpc' and 'la' plots are only available if 'components = TRUE' in the 'lcc' call",
         call.= FALSE)
  }
  #---------------------------------------------------------------------
    if(ci==FALSE) {
      if(ldb == 1) {
        if (type == "lcc") {
          lccplot <- plot_lcc(rho=obj$plot_info$rho, tk.plot= tk.plot,
                              tk.plot2=tk.plot2, ldb=ldb,
                              model=model, ci = ci,
                              arg=plot.cons, ...)
        }
        if(components==TRUE){
          if (type == "lpc") {
            lccplot <- plot_lpc(LPC=obj$plot_info$rho.pearson,
                                tk.plot= tk.plot,
                                tk.plot2=tk.plot2, ldb=ldb,
                                model=model, ci = ci, arg = plot.cons,
                                ...)
          }
          if (type == "la") {
            lccplot <- plot_la(Cb=obj$plot_info$Cb, tk.plot= tk.plot,
                               tk.plot2=tk.plot2, ldb=ldb,
                               model=model, ci = ci, arg = plot.cons,
                               ...)
          }
         }
      } else {
         if (type == "lcc") {
          lccplot <- plot_lcc(rho=obj$plot_info$rho, tk.plot= tk.plot,
                              tk.plot2=tk.plot2, ldb=ldb, model=model,
                              ci = ci, arg = plot.cons, ...)
          }
         if(components==TRUE){
           if (type == "lpc") {
             lccplot <- plot_lpc(LPC=obj$plot_info$rho.pearson,
                                 tk.plot= tk.plot,
                                tk.plot2=tk.plot2, ldb=ldb, model=model,
                                ci = ci, arg = plot.cons, ...)
           }
           if (type == "la") {
            lccplot <- plot_la(Cb=obj$plot_info$Cb, tk.plot= tk.plot,
                               tk.plot2=tk.plot2, ldb=ldb, model=model,
                               ci = ci, arg = plot.cons, ...)
           }
          }
      }
    }else{
      ENV.LCC<-obj$plot_info$ENV.LCC
      if (type == "lcc") {
        lccplot <- plot_lcc(rho=obj$plot_info$rho, ENV.LCC=ENV.LCC,
                            tk.plot= tk.plot, tk.plot2=tk.plot2, ldb=ldb,
                            model=model, ci = ci, arg = plot.cons, ...)
        }
      if(components==TRUE){
        if (type == "lpc") {
        ENV.LPC<-obj$plot_info$ENV.LPC
        lccplot <- plot_lpc(LPC=obj$plot_info$rho.pearson,ENV.LPC=ENV.LPC,
                            tk.plot= tk.plot, tk.plot2=tk.plot2, ldb=ldb,
                            model=model, ci = ci, arg = plot.cons, ...)
        }
        if (type == "la") {
        ENV.Cb<-obj$plot_info$ENV.LA
        lccplot <- plot_la(Cb=obj$plot_info$Cb,ENV.Cb = ENV.Cb, tk.plot= tk.plot,
                           tk.plot2=tk.plot2, ldb=ldb, model=model, ci = ci,
                           arg = plot.cons, ...)
        }
       }
    }
  return(invisible(lccplot))
}
