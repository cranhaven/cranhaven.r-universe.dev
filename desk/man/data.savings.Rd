\docType{data}

\name{data.savings}

\alias{data.savings}

\title{International Life-Cycle Savings and Disposable Income}

\format{
A data frame with 50 observations on the following five variables.
\tabular{ll}{
\code{sr} \tab ratio of the country's private savings to its disposable income.\cr
\code{pop15} \tab share of the country's population under 15.\cr
\code{pop75} \tab share of the country's population over 75.\cr
\code{dpi} \tab country's real per capita disposable income (in dollar).\cr
\code{ddpi} \tab growth rate of the country's disposable income per capita (in percent).\cr
}
}

\source{
R package \pkg{datasets} (object LifeCycleSavings).
}

\usage{
data.savings
}

\description{
This data set describes the savings behavior of 50 countries in 1960-1970. The data set includes demographical variables as well as variables on disposable income.
}

\details{
Under the life-cycle savings hypothesis as developed by Franco Modigliani, the savings ratio (aggregate personal saving divided by disposable income) is explained by per-capita disposable income, the percentage rate of change in per-capita disposable income, and two demographic variables: the percentage of population less than 15 years old and the percentage of the population over 75 years old. The data are averaged over the decade 1960-1970 to remove the business cycle or other short-term fluctuations.

In Auer et al. (2023, Chaps. 9, 10 & 12) the data set is used to illustrate the econometric analysis of a multivariate linear regression model.
}

\references{
Auer, L.v., Hoffmann, S. & Kranz, T. (2023): Ökonometrie - Das R-Arbeitsbuch, 2nd ed., Springer-Gabler (\href{https://www.uni-trier.de/index.php?id=15929}{https://www.oekonometrie-lernen.de}).
}

\keyword{datasets}
