\docType{data}

\name{data.govexpend}

\alias{data.govexpend}

\title{Government Expenditures of US-States}

\format{
A data frame with 50 observations on the following 5 variables:
\tabular{ll}{
\code{state} \tab name of the state.\cr
\code{expend} \tab total state expenditures per capita (in dollar).\cr
\code{aid} \tab federal aid received by this state (in million dollar).\cr
\code{gdp} \tab gross domestic product (in million dollar).\cr
\code{pop} \tab population (in million).\cr
}
}

\source{
Different datasets based on National Association of State imported in 2015:

State Expenditure Report, Table 1: Total State Expenditures - Capital Inclusive from (\href{https://www.kff.org/state-category/demographics-and-the-economy/state-budgets/}{Budget Officers}).

Annual Surveys of State and Local Government Finances, Table 1: State and Local Government Finances by Level of Government and by State 2012-13 from \href{https://www.census.gov/data/datasets.html}{U.S. Census}.

Real GDP by State, 2011-2014, Table 1 from \href{https://apps.bea.gov/regional/histdata/}{U.S. Bureau of Economic Analysis}.

Annual Estimates of the Resident Population for the United States, Regions, States, and Puerto Rico, Table 1 from \href{https://www.census.gov/data/datasets.html}{U.S. Census}.
}

\usage{
data.govexpend
}

\description{
This is a data set on the yearly expenditures of the US-States in 2013. Furthermore, the data describe some relevant characteristics of these states.
}

\details{
In Auer et al. (2023, Chap. 17) these data are used to illustrate the consequences of heteroscedastic error terms.
}

\references{
Auer, L.v., Hoffmann, S. & Kranz, T. (2023): Ökonometrie - Das R-Arbeitsbuch, 2nd ed., Springer-Gabler (\href{https://www.uni-trier.de/index.php?id=15929}{https://www.oekonometrie-lernen.de}).
}

\keyword{datasets}
