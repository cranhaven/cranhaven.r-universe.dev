\docType{data}

\name{data.regional}

\alias{data.regional}

\title{Regional Cost of Living in Germany}

\format{
A data frame with 401 observations on the following seven variables:
\tabular{ll}{
\code{id} \tab identifies the region.\cr
\code{region} \tab the German name of the region.\cr
\code{area} \tab the region's area (in square kilometers).\cr
\code{pop} \tab the region's population in 2019.\cr
\code{coli} \tab the region's index number of the cost of living in May 2019 (German average = 100).\cr
\code{wage} \tab the region's median wage in December 2016 (in euro).\cr
\code{unempl} \tab the region's unemployment rate in December 2016 (in percent).\cr
}
}

\source{
The wage data are taken from Fuchs (2018) while the cost of living data are taken from Auer and Weinand (2022). The unemployment data can be found in the report "Arbeitsmarkt in Zahlen" provided by the Bundesagentur für Arbeit. For each German State and each month, one report is published. Each report is available as Excel-sheet.
}

\usage{
data.regional
}

\description{
This is a data set on regional wages and the regional levels of the cost of living. The data set covers the 401 counties and cities of Germany.
}

\details{
In Auer et al. (2023, Chap. 22) these data are used to illustrate the estimation of simultaneous equations models.
}

\references{
Auer, L.v., Hoffmann, S. & Kranz, T. (2023): Ökonometrie - Das R-Arbeitsbuch, 2nd ed., Springer-Gabler (\href{https://www.uni-trier.de/index.php?id=15929}{https://www.oekonometrie-lernen.de}).\cr

Auer, L.v., Weinand, S. (2022): A nonlinear generalization of the Country-Product-Dummy method, Discussion Paper No. 45/2022, Deutsche Bundesbank.\cr

Fuchs, M. (2018): Aktuelle Daten und Indikatoren - Regionale Lohnunterschiede zwischen Männern und Frauen in Deutschland, Februar 2018, Institut für Arbeitsmarkt- und Berufsforschung (IAB).
}

\keyword{datasets}
