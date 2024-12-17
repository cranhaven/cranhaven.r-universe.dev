\docType{data}

\name{data.macro}

\alias{data.macro}

\title{Macroeconomic Data from Germany}

\format{
A data frame with 129 observations on the following seven variables:
\tabular{ll}{
\code{quarter} \tab identifies the time period in combination with \code{year}. \cr
\code{year} \tab identifies the time period in combination with \code{quarter}. \cr
\code{consump} \tab private consumption in the observed quarter. \cr
\code{invest} \tab gross investment in the observed quarter. \cr
\code{gov} \tab government expenditure in the observed quarter. \cr
\code{netex} \tab net exports (exports - imports) in the observed quarter. \cr
\code{gdp} \tab gross domestic product in the observed quarter. \cr
}
}

\source{
Imported from \href{https://www-genesis.destatis.de/genesis/online}{Federal Statistical Office of Germany}, data ID: 81000-0020.
}

\usage{
data.macro
}

\description{
This is a (time series) data set on macroeconomic data from Germany covering 129 consecutive quarters (Q1 1990 -- Q1 2023).
}

\details{
These National Accounts data are measured in real quantities (billions of chained 2015 Euros)
and are calendar and seasonally-adjusted (method: X13 JDemetra+). Theoretically, private consumption, gross investment, government expenditure, and net exports should exactly sum up to the gross domestic product. However, in practice, there are often some minor discrepancies in the data. As a result, for didactical purposes, we calculated gross investment as residuals rather than using the actual data.
}

\references{
Auer, L.v., Hoffmann, S. & Kranz, T. (2023): Ökonometrie - Das R-Arbeitsbuch, 2nd ed., Springer-Gabler (\href{https://www.uni-trier.de/index.php?id=15929}{https://www.oekonometrie-lernen.de}).
}

\keyword{datasets}
