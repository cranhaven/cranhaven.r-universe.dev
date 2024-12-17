\docType{data}

\name{data.trade}

\alias{data.trade}

\title{Gravity Model Applied to Germany}

\format{
A data frame with 27 observations on the following five variables:
\tabular{ll}{
\code{country} \tab name of member state.\cr
\code{imports} \tab German imports from member state (in million euro).\cr
\code{exports} \tab German exports to member state (in million euro).\cr
\code{gdp} \tab gross domestic product of member state (in million euro).\cr
\code{dist} \tab distance between member state and Germany (in km).\cr
}
}

\source{
Imported from Eurostat \href{https://ec.europa.eu/eurostat/home}{Eurostat}.
Distances computed with \href{https://www.freemaptools.com}{FreeMapTools}.
}

\usage{
data.trade
}

\description{
This is a data set on German trade with its 27 EU-partners in 2014.
}

\details{
In Auer et al. (2023, Chaps. 9 & 14) these data are used to illustrate the estimation and functional specification of a multivariate linear regression model.
}

\references{
Auer, L.v., Hoffmann, S. & Kranz, T. (2023): Ökonometrie - Das R-Arbeitsbuch, 2nd ed., Springer-Gabler (\href{https://www.uni-trier.de/index.php?id=15929}{https://www.oekonometrie-lernen.de}).
}

\keyword{datasets}
