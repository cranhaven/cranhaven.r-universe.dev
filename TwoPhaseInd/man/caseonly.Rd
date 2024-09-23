\name{caseonly}
\alias{caseonly}

\title{
A function to deal with case-only designs
}
\description{
This function estimates parameters of case-only designs.
}
\usage{
caseonly(data, treatment, BaselineMarker, extra = NULL, fraction = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame used to access the following data.
}
  \item{treatment}{
A character string of column name, corresponds to one column of the data frame, which
 is used to store the binary vector of treatment variable (1: treatment, 0: placebo). 
}
  \item{BaselineMarker}{
A character string of column name, corresponds to one column of the data frame, 
which is used to store a vector of biomarker.
}
  \item{extra}{
A string vector of column name(s), corresponds to more or more column(s) of the data 
frame, which is/are used to store the extra baseline covariate(s) to be included in 
case-only regression. Note that extra covariates are not needed unless the 
interactions of treatment and extra coviarates are of interest.
}
  \item{fraction}{
The randomization fraction of active treatment assignment.
}
}
\details{
This function estimates parameters of case-only designs. It estimates two parameters 
for "treatment effect when baselineMarker=0"" and treatment+baselineMarker interaction".
}
\value{
For each paramter, it returns:
\item{beta}{Estimated parameter}
\item{stder}{Standard error}
\item{pVal}{p value}
}

\references{
J. Y. Dai, S. S. Li, and P. B. Gilbert. Case-only methods for competing risks models 
with application to assessing differential vaccine efficacy by viral and host genetics. 
Biometrics, 15(1):196-203, 2014.
}

\author{
James Y. Dai
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
#form the data
data(acodata)
cdata=acodata[acodata[,2]==1,]
cfit=caseonly(data=cdata,
              treatment="f_treat",
              BaselineMarker="fcgr2a.3",
              extra=c("f_agele30","f_hsv_2","f_ad5gt18","f_crcm",
              "any_drug","num_male_part_cat","uias","uras"))
cfit              
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ case-only designs }
