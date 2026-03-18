\title{Plots}
\name{plots}
\alias{plot.power}
\alias{plot.mdes}
\alias{plot.mrss}

\description{Plots statistical power, minimum detectable effect size (MDES), or MDES difference (MDESD) curves with (1-\eqn{\alpha})x100 \% confidence interval.
}

\usage{
  \method{plot}{power}(x, ypar = "power",  xpar = NULL,
          xlim = NULL, ylim = NULL,
          xlab = NULL, ylab = NULL,
          main = NULL, sub = NULL,
          locate = FALSE, ...)

  \method{plot}{mdes}(x, ypar = "power",  xpar = NULL,
          xlim = NULL, ylim = NULL,
          xlab = NULL, ylab = NULL,
          main = NULL, sub = NULL,
          locate = FALSE, ...)

  \method{plot}{mrss}(x, ypar = "power",  xpar = NULL,
          xlim = NULL, ylim = NULL,
          xlab = NULL, ylab = NULL,
          main = NULL, sub = NULL,
          locate = FALSE, ...)
}

\arguments{
  \item{x}{an object returned from one of the \code{PowerUpR} functions.}
  \item{ypar}{character; \code{"mdes"} or  \code{"power"} on \code{y} axis .}
  \item{xpar}{character; one of the sample sizes on \code{x} axis.}
  \item{xlim}{limits for \code{xpar}.}
  \item{ylim}{limits for \code{ypar}.}
  \item{xlab}{\code{x} axis label (ignored for objects returned from \code{power.med211()}, \code{power.med221()}, and \code{power.med321()} functions).}
  \item{ylab}{\code{y} axis label (ignored for objects returned from \code{power.med211()}, \code{power.med221()}, and \code{power.med321()} functions).}
  \item{main}{title for the plot (ignored for objects returned from \code{power.med221()} and \code{power.med211()} functions).}
  \item{sub}{subtitle for the plot (ignored for objects returned from \code{power.med221()} and \code{power.med211()} functions).}
  \item{locate}{logical; \code{TRUE} locates parameter values for design \code{x} on the plot.}
  \item{...}{other graphical parameters to pass to \code{plot.new()}.}
}


\examples{
design1 <- mdes.cra3(rho3=.06, rho2=.17, n=15, J=3, K=60)
plot(design1, ypar = "mdes", xpar = "K", xlim = c(30, 100))
plot(design1, ypar = "power", xpar = "K", xlim = c(30, 100))

design2 <- power.cra3(es=.269, rho3=.06, rho2=.17, n=15, J=3, K=60)
plot(design2, ypar = "mdes", xpar = "K", xlim = c(30, 100))
plot(design2, ypar = "power", xpar = "K", xlim = c(30, 100))
}
