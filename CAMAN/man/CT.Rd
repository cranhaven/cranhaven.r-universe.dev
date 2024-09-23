\name{CT}
\docType{data}
\alias{CT}
\title{Meta-anaysis: noninvasive coronary angiography using computed tomography (CT)}
\description{ CT  for ruling out clinically significant coronary
artery disease (CAD) in adults with suspected or known CAD. The accuracy and clinical value of CT  was assessed in this meta-analysis.

MEDLINE, EMBASE, and ISI Web of Science searches from inception through 2 June 2009 and bibliographies of reviews.
 Prospective English- or German-language studies that compared CT or MRI with conventional coronary angiography in all patients and included
sufficient data for compilation of 2 x 2 tables. Two investigators independently extracted patient and study characteristics; differences were resolved by consensus.
89 studies comprising 7516 assessed the diagnostic value of CT. 
}
\usage{data("CT")}
\format{A data frame consisting of 91 data sets (rows) and 10 attributes (columns)
\describe{
Variable Names in order from left to right:
	\item{Author}{Author} \cr 
	\item{Year}{Year } \cr 
        \item{TP}{true positive} \cr
        \item{FP}{False positive} \cr
        \item{FN}{False negative } \cr
        \item{TN}{True negative} \cr
        \item{logitTPR}{logit-true positive rate} \cr
        \item{logitTNR}{logit-true negative rate} \cr
        \item{varlogitTPR}{Variance of logit TPR} \cr
        \item{varlogitTNR}{Variance of logit TPR} \cr
  
}
}

\examples{
#Use the EM-algorithm for a diagnostic meta-analysis based on a mixture 
#of bivariate  normal densities.
#Here fixed study specific variances are calculated based on logit 
#transformed sensitivity and specificity. 
data(CT)
p2 <- c(0.4,0.6)
lamlog12 <- c(2.93,3.22)
lamlog22 <- c(2.5,1.5)

m0 <- bivariate.EM(obs1=logitTPR,obs2=logitTNR,
                   var1=varlogitTPR,var2=varlogitTNR,
                   type="meta",lambda1=lamlog12,lambda2=lamlog22,
                   p=p2,data=CT,class="FALSE")
}
\references{
Schuetz GM, Zacharopoulou NM, Schlattmann P, Dewey M. Meta-analysis:
noninvasive coronary angiography using computed tomography versus magnetic
resonance imaging. Ann Intern Med. 2010 Feb 2;152(3):167-77. doi:
10.7326/0003-4819-152-3-201002020-00008.
}
\keyword{datasets}

