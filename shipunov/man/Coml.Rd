\name{Coml}
\alias{Coml}
\alias{summary.Coml}
\title{Compare checklists}
\description{Compare species checklists}

\usage{
Coml(df1, df2)
\method{summary}{Coml}(object, ..., n=10)
}

\arguments{
\item{df1}{First data frame with species presence/absence data, species as row names}
\item{df2}{Second data frame}
\item{object}{Object of the class 'Coml'}
\item{n}{Number of indicator species}
\item{...}{Additional arguments}
}

\details{Compare two (groups of) checklists (Abramova et al., 2003).

Calculates difference (in \%) between checklists with \emph{common base}, i.e., species occurrence/abundance columns of data frame with species names as row names.

Finds names of "indicators" most characteristic to each group
}

\value{Object of the class 'Coml', or nothing}

\references{Abramova L. A., Rimskaya-Korsakova N. N., Shipunov A. B. 2003. The comparative study of the flora of Kiv Gulf, Chupa Gulf and Keret' Archipelago islands (Kandalaksha Bay of White Sea). Proceedings of the Pertsov White Sea Biological Station. Vol. 9. Moscow. P. 22--33. in Russian (English abstract)}

\author{Alexey Shipunov}

% \seealso{}

\examples{
y.Coml <- Coml(dolbli[1:45], dolbli[46:79])
summary(y.Coml, n=5)
}

\keyword{manip}
