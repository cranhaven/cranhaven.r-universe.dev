\docType{data}

\name{data.spurious}

\alias{data.spurious}

\title{Non-Stationary Time Series Data}

\format{
A data frame with yearly observations from 1880 to 2022 on the following five variables:
\tabular{ll}{
\code{year} \tab year of the observation.\cr
\code{temp} \tab deviation of the pre-industrial average global temperature.\cr
\code{elements} \tab number of discovered elements in chemistry (periodic table).\cr
\code{gold} \tab price for 1 ounce of fine gold in US-Dollar (not inflation-adjusted) starting in 1968.\cr
\code{cpi} \tab consumer price index: total all items for the United States (index 2015 = 100) starting in 1968.\cr
}
}

\source{
NASA (GISTEMP Team, 2023: GISS Surface Temperature Analysis (GISTEMP), version 4. NASA Goddard Institute for Space Studies. Dataset accessed 2023-05-11 at \url{https://data.giss.nasa.gov/gistemp/}).

IUPAC (\url{https://iupac.org/what-we-do/periodic-table-of-elements/}).

LBMA (retrieved from Deutsche Bundesbank Zeitreihen-Datenbanken, BBEX3.A.XAU.USD.EA.AC.C08).

OECD (retrieved from FRED, \url{https://fred.stlouisfed.org/series/CPALTT01USA661S}).
}

\usage{
data.spurious
}

\description{
The variables in this data set are non-stationary and help to understand spurious regression in the context of time series analysis.
}

\details{
In Auer et al. (2023, Chap. 22) these data are used to illustrate the estimation of dynamic regression models.
}

\references{
Auer, L.v., Hoffmann, S. & Kranz, T. (2023): Ökonometrie - Das R-Arbeitsbuch, 2nd ed., Springer-Gabler (\href{https://www.uni-trier.de/index.php?id=15929}{https://www.oekonometrie-lernen.de}).

Lenssen, N., Schmidt, G., Hansen, J., Menne, M., Persin, A., Ruedy, R., & Zyss, D. (2019): Improvements in the GISTEMP uncertainty model. J. Geophys. Res. Atmos., 124, no. 12, 6307-6326, doi:10.1029/2018JD029522.
}

\keyword{datasets}
