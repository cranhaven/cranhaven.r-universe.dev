\name{tumors}
\alias{tumors}
\docType{data}
\title{
Brain tumors data.
}
\description{
43 brain tumors and 356 continuous variables corresponding to the
expression data and 1 categorical variable corresponding to the type of tumors (4 types). 
}

\usage{data(tumors)}

\format{
A data frame with 43 rows and 357 columns. Rows represent the tumors,
columns represent the expression and the type of tumor.
}
\details{
A genetic data frame.
}
\source{
M. de Tayrac, S. Le, M. Aubry, J. Mosser, and F. Husson. Simultaneous analysis of distinct omics data sets with integration of biological knowledge: Multiple factor analysis approach. BMC Genomics, 10(1):32, 2009.
}

\examples{
data(tumors)
\dontrun{
res.ada <- adashrink(tumors[, -ncol(tumors)], method = "SURE")
res.hcpc <- HCPC(as.data.frame(res.ada$mu.hat), graph=F, consol = FALSE)
plot.HCPC(res.hcpc, choice = "map", draw.tree = "FALSE")
}
}
\keyword{datasets}



