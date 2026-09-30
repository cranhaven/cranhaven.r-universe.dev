\name{corba}
\alias{corba}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mean correlation before and after batch effect adjustment
}
\description{
For each variable Pearson's correlation of the values before and after batch effect adjustment is calculated. Then the mean is taken over all these correlations.
}
\usage{
corba(xba, x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xba}{
matrix. The covariate matrix after batch effect adjustment. observations in rows, variables in columns.
}
  \item{x}{
matrix. The covariate matrix before batch effect adjustment. observations in rows, variables in columns.
}
}

\value{
Value of the metric
}
\references{
Lazar, C., Meganck, S., Taminau, J., Steenhoff, D., Coletta, A., Molter,C., Weiss-Solís, D. Y., Duque, R., Bersini, H., Nowé, A. (2012). Batch effect removal methods for microarray gene expression data integration: a survey. Briefings in Bioinformatics 14(4):469-490, <\doi{10.1093/bib/bbs037}>.
}
\author{
Roman Hornung
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
data(autism)

params <- ba(x=X, y=y, batch=batch, method = "combat")
Xadj <- params$xadj

corba(xba=Xadj, x=X)
}
