\name{Infill}
\alias{Infill}
\alias{plot.Infill}
\alias{summary.Infill}

\title{Rarefaction curves}
\description{Rarefaction curves}

\usage{
Infill(x, n=10)
\method{plot}{Infill}(x, ...)
\method{summary}{Infill}(object, ...)
}

\arguments{
\item{x}{Data frame where columns are species}
\item{object}{Object of the class "Infill"}
\item{n}{Number of permutations}
\item{...}{Arguments to 'plot()' or 'summary()'}
}

\details{
'Infill()' returns matrix to draw accumulation curves (each column is one curve).

'Infill' uses checklists of biological organisms to build rarefaction curves. You can estimate how many taxa will appear in the next sample to plan your investigations (e.g. revealing flora or fauna of the certain area).

If cells contain taxa abundance it will be automatically replaced with 1 or 0. Permutation is a random shuffle of the samples to get more valid estimation of the taxa accumulation process. It does not matter which sample appeared first. The resulting plot gives information on the process of taxa revealing during the investigation. High number of permutations gives more precise results, but the calculations are more slow. Empirically, 100 permutations are enough. The plot indicates full taxa number which has been accumulated in this and all the previous samples.
}

\value{Object of the class "Infill", or nothing}

\references{
Diaz-Frances E., Soberon J. 2005. Statistical estimation and model selection of species accumulation curves. Conservation Biology. Vol. 19, N 2. P. 569-573.

Gotelli N.J., Colwell R.C. 2001. Quantifying biodiversity: procedures and pitfalls in the measurement and comparison of species richness. Ecology Letters. Vol. 4. P. 379-391.

Soberon J.M., Llorente J.B. 1993. The use of species accumulation functions for the prediction of species richness. Conservation Biology. Vol. 7. N 3. P. 480-488.
}

\author{Alexey Shipunov, Eugeny Altshuler}

% \seealso{}

\examples{
x <- t(dolbli)
data <- x[1:45, ] # one of two lakes selected
data.I <- Infill(data)
summary(data.I)
plot(data.I)
}

\keyword{multivariate}
