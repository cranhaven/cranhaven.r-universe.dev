\docType{data}

\name{data.sick}

\alias{data.sick}

\title{Sick Leave and Unemployment}

\format{
A data frame with 23 observations on the following three variables:
\tabular{ll}{
\code{year} \tab year.\cr
\code{jobless} \tab average unemployment rate during that year (in percent).\cr
\code{sick} \tab average of employees' sick leave during that year (in percent).\cr
}
}

\source{
Imported from \href{https://www.destatis.de/DE/Themen/Arbeit/Arbeitsmarkt/Qualitaet-Arbeit/_qualitaet-arbeit.html}{Federal Statistical Office of Germany}.
}

\usage{
data.sick
}

\description{
This is a data set on the unemployment rates and the sick leave in Germany in the years 1992 to 2014.
}

\details{
In Auer et al. (2023, Chap. 18) these data are used to illustrate the consequences of autocorrelated error terms.
}

\references{
Auer, L.v., Hoffmann, S. & Kranz, T. (2023): Ökonometrie - Das R-Arbeitsbuch, 2nd ed., Springer-Gabler (\href{https://www.uni-trier.de/index.php?id=15929}{https://www.oekonometrie-lernen.de}).
}

\keyword{datasets}
