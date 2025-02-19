\encoding{UTF-8}
\name{GDP}
\alias{GDP}
\title{GDP of the US Economy}
\description{
  US GDP (Gross Domestic Product) ranging from from 1950 (Q1) to 2009
  (Q4).
}
\usage{GDP}
\format{A time series with 268 observations on two
  variables. The object is of class \code{ts}.}
\source{
  de Carvalho, M., Rodrigues, P. and Rua, A. (2012)
  Tracking the US business cycle with a singular spectrum analysis.
  \emph{Economics Letters}, \bold{114}, 32-35.
}
\references{
  de Carvalho, M. and Rua, A. (2017) Real-time nowcasting the US output
  gap: Singular spectrum analysis at work. \emph{International Journal of
    Forecasting}, \bold{33}, 185-198.
}
\examples{
data(GDP)
plot(GDP, ylab = "Gross Domestic Product")

\dontrun{
%% Tracking the US Business Cycle (de Carvalho et al, 2012; Fig. 2) via
%% the Fisher g statistic of de Carvalho and Rua (2017). Install the
%% package ASSA if not installed
if (!require("ASSA")) install.packages("ASSA")
data(GDP)
fit <- bssa(log(GDP[, 1]))
plot(fit)
print(fit)
}
}
\seealso{
  \url{https://www.maths.ed.ac.uk/~mdecarv/decarvalho2012dsh.html}
}
\keyword{Economics}
