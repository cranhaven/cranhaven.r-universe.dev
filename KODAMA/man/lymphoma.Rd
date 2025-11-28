\docType{data}

\name{lymphoma}

\alias{lymphoma}

\title{Lymphoma Gene Expression Dataset}

\description{
This dataset consists of gene expression profiles of the three most prevalent adult lymphoid malignancies: diffuse large B-cell lymphoma (DLBCL), follicular lymphoma (FL), and B-cell chronic lymphocytic leukemia (B-CLL). The dataset consists of 4,682 mRNA genes for 62 samples (42 samples of DLBCL, 9 samples of FL, and 11 samples of B-CLL). Missing value are imputed and data are standardized as described in Dudoit, \emph{et al}. (2002).

}

\usage{data(lymphoma)}

\value{
A list with the following elements:
	\item{data}{Gene expression data. A matrix with 62 rows and 4,682 columns.}
	\item{class}{Class index. A vector with 62 elements.}

}



\references{
Cacciatore S, Luchinat C, Tenori L \cr
Knowledge discovery by accuracy maximization. \cr
\emph{Proc Natl Acad Sci U S A} 2014;111(14):5117-5122. doi: 10.1073/pnas.1220873111. \doi{10.1073/pnas.1220873111} \cr\cr

Cacciatore S, Tenori L, Luchinat C, Bennett PR, MacIntyre DA \cr
KODAMA: an updated R package for knowledge discovery and data mining.\cr
\emph{Bioinformatics} 2017;33(4):621-623. doi: 10.1093/bioinformatics/btw705. \doi{10.1093/bioinformatics/btw705}
\cr	\cr
Alizadeh AA, Eisen MB, Davis RE, \emph{et al.}\cr
Distinct types of diffuse large B-cell lymphoma identified by gene expression profiling. \cr
\emph{Nature} 2000;403(6769):503-511.
\cr
\cr
Dudoit S, Fridlyand J, Speed TP\cr
Comparison of discrimination methods for the classification of tumors using gene expression data. \cr
\emph{J Am Stat Assoc} 2002;97(417):77-87.
}
\examples{
\donttest{
data(lymphoma)
class=1+as.numeric(lymphoma$class)
cc=pca(lymphoma$data)$x[,1:50]
plot(cc,pch=21,bg=class)

kk=KODAMA.matrix(cc,ncomp=2)

custom.settings=tsne.defaults
custom.settings$perplexity = 10
cc=KODAMA.visualization(kk,"t-SNE",config=custom.settings)

plot(cc,pch=21,bg=class)

}
}
\keyword{datasets}
