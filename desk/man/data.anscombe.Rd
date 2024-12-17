\docType{data}

\name{data.anscombe}

\alias{data.anscombe}

\title{Anscombe's Quartet}

\format{
A data frame of 4 data sets, each with 11 observations of the two variables x and y.
\tabular{ll}{
\code{x1 to x4} \tab x-variables of the four data sets.\cr
\code{y1 to y4} \tab y-variables of the four data sets.\cr
}
}

\source{
This dataset was manually generated from: Anscombe, F.J. (1973): Graphs in Statistical Analysis. American Statistician, 27(1), 17-21. Also available in the R package \pkg{datasets}.
}

\usage{
data.anscombe
}

\description{
This data set comprises four individual x-y-data sets which have the same statistical properties (mean, variance, correlation, regression line, etc.), yet are quite different.
}

\details{
In Auer et al. (2023, Chap. 3) these data are used to illustrate the simple regression model and the importance to visually evaluate datasets before a numerical analysis is performed.
}

\references{
Tufte, E.R. (1989): The Visual Display of Quantitative Information, 13-14. Graphics Press.

Auer, L.v., Hoffmann, S. & Kranz, T. (2023): Ökonometrie - Das R-Arbeitsbuch, 2nd ed., Springer-Gabler (\href{https://www.uni-trier.de/index.php?id=15929}{https://www.oekonometrie-lernen.de}).
}

\keyword{datasets}
