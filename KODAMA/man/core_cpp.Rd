\name{core_cpp}

\alias{core_cpp}

\title{Maximization of Cross-Validateed Accuracy Methods}

\description{This function performs the maximization of cross-validated accuracy by an iterative process}

\usage{
core_cpp(x,
         xTdata = NULL, 
         clbest, 
         Tcycle = 20, 
         FUN = c("fastpls","simpls"), 
         f.par.pls = 5, 
         constrain = NULL, 
         fix = NULL) 
}

\arguments{
  \item{x}{a matrix.}
  \item{xTdata}{a matrix for projections. This matrix contains samples that are not used for the maximization of the cross-validated accuracy. Their classification is obtained by predicting samples on the basis of the final classification vector.}
  \item{clbest}{a vector to optimize.}
  \item{Tcycle}{number of iterative cycles that leads to the maximization of cross-validated accuracy.}
  \item{FUN}{classifier to be consider. Choices are "\code{fastpls}" and "\code{simpls}".}
  \item{f.par.pls}{parameters of the classifier. If the classifier is \code{KNN}, \code{fpar} represents the number of neighbours. If the classifier is \code{PLS-DA}, \code{fpar} represents the number of components.}
  \item{constrain}{
  	a vector of \code{nrow(data)} elements. Supervised constraints can be imposed by linking some samples in such a way that if one of them is changed, all other linked samples change in the same way (\emph{i.e.}, they are forced to belong to the same class) during the maximization of the cross-validation accuracy procedure. Samples with the same identifying constrain will be forced to stay together.
  	}
  \item{fix}{
  	a vector of \code{nrow(data)} elements. The values of this vector must  be \code{TRUE} or \code{FALSE}. By default all elements are \code{FALSE}. Samples with the \code{TRUE} fix value will not change the class label defined in \code{W} during the maximization of the cross-validation accuracy procedure. For more information refer to Cacciatore, \emph{et al.} (2014).
  	}
}

\value{
    The function returns a list with 3 items:
    \item{clbest}{a classification vector with a maximized cross-validated accuracy.}
    \item{accbest}{the maximum cross-validated accuracy achieved.}
    \item{vect_acc}{a vector of all cross-validated accuracies obtained.}
    \item{vect_proj}{a prediction of samples in \code{xTdata} matrix using the vector clbest. This output is present only if \code{xTdata} is not \code{NULL}.}
    
  }
  \author{Stefano Cacciatore and Leonardo Tenori}
  
\references{
Abdel-Shafy EA, Kassim M, Vignol A, \emph{et al.} \cr
KODAMA enables self-guided weakly supervised learning in spatial transcriptomics. \cr
\emph{bioRxiv} 2025. doi: 10.1101/2025.05.28.656544. \doi{10.1101/2025.05.28.656544} \cr\cr

Cacciatore S, Luchinat C, Tenori L \cr
Knowledge discovery by accuracy maximization. \cr
\emph{Proc Natl Acad Sci U S A} 2014;111(14):5117-5122. doi: 10.1073/pnas.1220873111. \doi{10.1073/pnas.1220873111} \cr\cr

Cacciatore S, Tenori L, Luchinat C, Bennett PR, MacIntyre DA \cr
KODAMA: an updated R package for knowledge discovery and data mining.\cr
\emph{Bioinformatics} 2017;33(4):621-623. doi: 10.1093/bioinformatics/btw705. \doi{10.1093/bioinformatics/btw705}
}

  \seealso{\code{\link{KODAMA.matrix}},\code{\link{KODAMA.visualization}}}

  \examples{
# Here, the famous (Fisher's or Anderson's) iris data set was loaded
data(iris)
u=as.matrix(iris[,-5])
s=sample(1:150,150,TRUE)

# The maximization of the accuracy of the vector s is performed
results=core_cpp(u, clbest=s,f.par.pls  = 4)


print(as.numeric(results$clbest))

}

