\encoding{UTF-8}
\name{GDPIP}
\alias{GDPIP}
\title{A Real-time Vintage of GDP and IP for the US Economy}
\description{
  US GDP (Gross Domestic Product) and IP (Industrial Production)
  ranging from from 1947 (Q1) to 2013 (Q4); the data correspond to a
  real-time vintage.
}
\usage{GDPIP}
\format{A bivariate time series with 268 observations on two variables:
  \code{GDP} and \code{IP}. The object is of class \code{mts}.}
\references{
  de Carvalho, M. and Rua, A. (2017). Real-time nowcasting the US output
  gap: Singular spectrum analysis at work. \emph{International Journal
  of Forecasting}, \bold{33}, 185-198.
}
\source{
  Federal Reserve Bank of Philadelphia.
}
\seealso{\url{https://www.maths.ed.ac.uk/~mdecarv/decarvalho2017sh.html}}
\examples{
data(GDPIP)
plot(GDPIP)

## Plotting GDP against IP (de Carvalho and Rua, 2017; Fig. 4)
data(GDPIP)
oldpar <- par(mar = c(5, 4, 4, 5) + .1)
plot(GDPIP[, 1], type = "l", 
     xlab = "Time", ylab = "Gross Domestic Product (GDP)",
     lwd = 3, col = "red", cex.lab = 1.4, cex.axis = 1.4)
par(new = TRUE)
plot(GDPIP[, 2], type = "l", xaxt = "n", yaxt = "n",
     xlab = "", ylab = "", lwd = 3, col = "blue", cex.axis = 1.4)
axis(4)
mtext("Industrial Production (IP)", side = 4, line = 3, cex = 1.4)
legend("topleft", col = c("red", "blue"),
       lty = 1, lwd = 3, legend = c("GDP", "IP"))
par(oldpar)

\dontrun{
    ## Tracking the US Business Cycle (de Carvalho et al, 2017; Fig. 6)
    ## Install the package ASSA, if not installed
    if (!require("ASSA")) install.packages("ASSA")
    data(GDPIP)
    fit <- bmssa(log(GDPIP))
    plot(fit)
    print(fit)
}
}
\keyword{Economics}
