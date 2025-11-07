\name{C_corrFdKt}
\alias{corrFdKt}
\alias{Ktd}
\alias{Ktm}
\alias{Kti}
\alias{FdKtPage}
\alias{FdKtLJ}
\alias{FdKtCPR}
\alias{FdKtEKDd}
\alias{FdKtCLIMEDd}
\alias{FdKtEKDh}
\alias{FdKtCLIMEDh}
\alias{FdKtBRL}
\Rdversion{1.0}
\encoding{UTF-8}

\title{Correlations between the fraction of diffuse irradiation and the
  clearness index.}
\description{A set of correlations between the fraction of diffuse irradiation and the
  clearness index used by \code{\link{fCompD}} and \code{\link{fCompI}}.
}
\usage{
## Monthly means of daily values
Ktm(sol, G0dm)
FdKtPage(sol, G0dm)
FdKtLJ(sol, G0dm)

## Daily values
Ktd(sol, G0d)
FdKtCPR(sol, G0d)
FdKtEKDd(sol, G0d)
FdKtCLIMEDd(sol, G0d)

## Intradaily values
Kti(sol, G0i)
FdKtEKDh(sol, G0i)
FdKtCLIMEDh(sol, G0i)
FdKtBRL(sol, G0i)
}
\arguments{
  \item{sol}{A \code{\linkS4class{Sol}} object, it may be the result of
    the \code{\link{calcSol}} function.}

  \item{G0dm}{A \code{\linkS4class{Meteo}} object with monthly means of
    radiation. It may be the result of the \code{\link{readG0dm}}
    function.}

  \item{G0d}{A \code{\linkS4class{Meteo}} object with daily values of
    radiation. It may be the result of the \code{\link{readBDd}} (or
    equivalent) function.}

  \item{G0i}{A \code{\linkS4class{Meteo}} object with intraidaily values
    of radiation. It may be the result of the \code{\link{readBDi}} (or
    equivalent) function.}
}

\value{A data.table, with two columns:
  \item{Fd}{A numeric, the diffuse fraction.}
  \item{Kt}{A numeric, the clearness index(provided by the Kt functions).}
}


\references{\itemize{
    \item Page, J. K., The calculation of monthly mean solar radiation for horizontal and inclined surfaces from
      sunshine records for latitudes 40N-40S. En U.N. Conference on New Sources of Energy, vol. 4, págs.
      378–390, 1961.
    \item Collares-Pereira, M. y Rabl, A.,  The average distribution of solar radiation: correlations between
      diffuse and hemispherical and between daily and hourly insolation
      values. Solar Energy, 22:155–164, 1979.
    \item Erbs, D.G, Klein, S.A. and Duffie, J.A., Estimation of the diffuse
      radiation fraction for hourly, daily and monthly-average global
      radiation. Solar Energy, 28:293:302, 1982.
    \item De Miguel, A. et al., Diffuse solar irradiation model evaluation
      in the north mediterranean belt area, Solar Energy, 70:143-153, 2001.
    \item Ridley, B., Boland, J. and Lauret, P., Modelling of diffuse solar
      fraction with multiple predictors, Renewable Energy, 35:478-482, 2010.
  }
}
\seealso{
  \code{\link{fCompD}},
  \code{\link{fCompI}}
}
\examples{
library("data.table")
\dontshow{setDTthreads(2)}

lat <- 37.2
BTd <- fBTd(mode = 'prom')
G0dm <- c(2.766, 3.491, 4.494, 5.912, 6.989, 7.742, 7.919, 7.027, 5.369,
    3.562, 2.814, 2.179)*1000;
Ta <- c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2,
    15.2)

prom <- readG0dm(G0dm = G0dm, Ta = Ta, lat = lat)
sol <- calcSol(lat = lat, BTd = BTd)

Kt <- Ktm(sol = sol, G0dm = prom)
Kt

Page <- FdKtPage(sol = sol, G0dm = prom)
LJ <- FdKtLJ(sol = sol, G0dm = prom)
Monthly <- merge(Page, LJ, by = 'Kt',
                suffixes = c('.Page', '.LJ'))
Monthly

xyplot(Fd.Page+Fd.LJ~Kt, data = Monthly,
       type = c('l', 'g'), auto.key = list(space = 'right'))

Kt = Ktd(sol = sol, G0d = prom)
Kt

CPR <- FdKtCPR(sol = sol, G0d = prom)
CLIMEDd <- FdKtCLIMEDd(sol = sol, G0d = prom)
Daily <- merge(CPR, CLIMEDd, by = 'Kt',
              suffixes = c('.CPR', '.CLIMEDd'))
Daily

xyplot(Fd.CPR + Fd.CLIMEDd ~ Kt, data = Daily,
       type = c('l', 'g'), auto.key = list(space = 'right'))

}
\author{Oscar Perpiñán Lamigueiro, Francisco Delgado López; The BRL model was suggested by Kevin Ummel.}
\keyword{utilities}
