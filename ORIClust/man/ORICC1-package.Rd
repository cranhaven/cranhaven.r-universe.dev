
% --- Source file: ORICC.Rd ---

\name{ORICC1}
\Rdversion{1.0}
\alias{ORICC1}
\docType{package}
\title{One-stage ORICC}
\description{ One-stage ORICC is a computationally efficient information criterion-based clustering algorithm for selecting and
clustering genes according to their time-course or dose-response
profiles. This algorithm takes account of the ordering in time-course or dose-response experiments by 
embedding the order-restricted inference into a model selection framework. This algorithm mainly consist of two steps. In the first step,
candidate  profiles are defined in terms of inequalities
among mean expression levels at different time points or doses levels. In the
second step, genes are assigned to the best matched profiles
determined by an information criterion for
order-restricted inference.}

\usage{

ORICC1(data,data.col,id.col,n.rep,n.top,transform,
       name.profile,cyclical.profile,complete.profile,
       onefile,plot.format)

}
\arguments{
\item{data}{
 A matrix containing the gene expressions.
}


\item{data.col}{Column indices of the gene expression data.
  }
\item{id.col}{Column index of the gene ID. Defaults to 1. 
  }
\item{n.rep}{A vector consisting of the number of replicate arrays at time points (1, 2,\ldots ,T), where T is the total number of time points.
  }
\item{n.top}{The number of genes kept for the final clustering result. Genes are ranked based on expression variation across time or dose levels. Defaults to all genes ORICC1 selects
  }
\item{transform}{Transformation of the original data:

 0=None, 1=natural log, 2=square root, 3=cubic root.  Defaults to 0.      
}
\item{name.profile}{A character string specifying the collection of candidate profiles. This option only supports monotone, up-down and down-up profiles specified as by 

\code{"decreasing";} 

\code{"increasing".}

\code{paste("up down max at",i,sep=" ");}

\code{paste("down up min at",j,sep=" ");}

If \code{name.profile="all"}, the `decreasing', `increasing' and all `up-down' and `down-up' profiles will be included. 

If \code{name.profile=NULL}, `decreasing', `increasing' and all `up-down' and `down-up' profiles will be absent. Defaults to NULL.

One can also specify  several up-down or down-up profiles together as follows. 

\code{profile1=paste("up down max at",c(2,4),sep=" ");}

\code{profile2=paste("down up min at",c(3,5),sep=" ");}

\code{name.profile=c(profile1,profile2);}

then up-down profile with maxima at 2 and 4 as well as down-up profile with  minima  at 3 and 5 will be included. 
}

\item{cyclical.profile}{A matrix with 2 columns. Each element of the matrix must be  a number in the set \{2,3,\ldots,T-1\}.  Each row of the matrix represents a cyclical profile with minima at the first entry of the row and maxima at the 2nd entry. As a result, two elements in the same row must be different.  For example, if 

\code{cyclical.profile=matrix(c(2,3,4,3),2,2,byrow=T),}
then the cyclical profile with minima at 2 and maxima at 3 and the cyclical profile with minima at 4 and maxima at 3 will be included as candidate profiles.

If \code{cyclical.profile=NULL}, all cyclical profiles will be absent. Defaults to NULL.

 }
\item{complete.profile}{The \code{complete.profile} means a profile in which there is no defined inequality constraint. 

If the \code{complete.profile} is a candidate profile, 


\code{complete.profile=1}, otherwise, 


\code{complete.profile=NULL}. Defaults to NULL.
}

\item{onefile}{logical: if true (the default) multiple figures for different clusters are output in one file. If FALSE, each cluster is plotted in a seperate file. Defaults to TRUE. 
  }

\item{plot.format}{The format of the output file containing plots of gene clusters.Users can choose between `eps' and `jpg'. Defaults to `eps'.
}
}
\details{
 The gene expression dataset should be in a tab-delimited txt file, in which the first two columns contain the gene names and their
accession numbers or descriptions, and the remaining columns, in their orders, are the geneexpression data (contain multiple columns, i.e. \code{data.col}).The dataset is assumed to have been processed so that each row contains the expressions of only one gene.
}

\value{
The results are displayed in a graphical form. The graphics can be stored in a JPG or EPS format. Both the raw gene expression values and the estimated mean expressions are output to external files `cluster of raw data.txt' and `cluster of fitted mean data.txt', respectively. 

}
\author{
Tianqing Liu, Nan Lin, Ningzhong Shi and Baoxue Zhang 

Maintainer: Tianqing Liu <tianqingliu@gmail.com>
}
\references{
Liu, T., Lin, N., Shi, N. and Zhang, B. (2009), \emph{Information criterion-based clustering with order-restricted candidate profiles in short time-course microarray experiments.} BMC Bioinformatics, \bold{10}: 146.
}

\keyword{package}

\examples{
data(Breast)
ORICC1(Breast,data.col=3:50,id.col=1,n.rep=rep(8,6),
       n.top=50,transform=1,name.profile="all",plot.format="eps")
}
























































