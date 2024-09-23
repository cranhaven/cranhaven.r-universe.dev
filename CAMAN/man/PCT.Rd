\name{PCT}
\docType{data}
\alias{PCT}
\title{Procalcitonin as diagnostic marker for sepsis}
\description{
 Procalcitonin is a promising marker for identification of bacterial infections. The accuracy and clinical value of procalcitonin for
 diagnosis of sepsis in critically ill patients was assessed in this meta-analysis.


Medline, Embase, ISI Web of Knowledge, the Cochrane Library, Scopus, BioMed Central, and Science Direct were searched, from inception to Feb 21, 2012,
and reference lists of identified primary studies. We included articles written in English, German, or French that investigated procalcitonin
for differentiation of septic patients--those with sepsis, severe sepsis, or septic shock--from those with a systemic inflammatory response syndrome of 
non-infectious origin. Studies of healthy people, patients without probable infection, and children younger than 28 days were excluded. 
Two independent investigators extracted patient and study characteristics; discrepancies were resolved by consensus. 

This search returned 3487 reports, of which 30 fulfilled the inclusion criteria, accounting for 3244 patients. 

}
\usage{data("PCT")}
\format{A data frame consisting of 35 data sets (rows) and 10 attributes (columns)
\describe{
Variable Names in order from left to right:
   	\item{Study}{Study} \cr 
   	\item{Year}{Year} \cr 
        \item{TP}{True positive} \cr
        \item{FP}{False positive} \cr
        \item{FN}{False negative } \cr
        \item{TN}{ True negative} \cr
        \item{logitTPR}{logit-true positive rate} \cr
        \item{logitTNR}{logit-true negative rate} \cr
        \item{varlogitTPR}{Variance of logit TPR} \cr
        \item{varlogitTNR}{Variance of logit TPR} \cr
  
}
}
\examples{
#Use the VEM-algorithm for a diagnostic meta-analysis based on a mixture 
#of bivariate  normal densities.
#Study specific fixed variances are based on logit transformed 
#sensitivity and specificity. 

data(PCT)
names(PCT)
   
m0 <- bivariate.VEM(obs1 = logitTPR, obs2 = logitTNR,
                    var1 = varlogitTPR, var2 = varlogitTNR,
                    type = "meta", data=PCT, startk=20)
}
\references{
Wacker C, Prkno A, Brunkhorst FM, Schlattmann P. Procalcitonin as a diagnostic
marker for sepsis: a systematic review and meta-analysis. Lancet Infect Dis. 2013
May;13(5):426-35. doi: 10.1016/S1473-3099(12)70323-7
}
\keyword{datasets}

