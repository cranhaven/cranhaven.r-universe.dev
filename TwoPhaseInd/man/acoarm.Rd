\name{acoarm}
\alias{acoarm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to estimate parameters in Cox proportional hazard models by augmented 
case-only designs for randomized clinical trials with failure time endpoints.
}
\description{
This function estimates parameters of proportional hazards models with gene-treatment 
interactions. It employs classical case-cohort estimation methods, incorporating the 
case-only estimators. The method was published in Dai et al. (2016) Biometrics.
}
\usage{
acoarm(data, svtime, event, treatment, BaselineMarker,subcohort, esttype = 1, 
augment = 1, weight=NULL, extra = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame used to access the following data.
}
  \item{svtime}{
A character string of column name, corresponds to one column of the data frame, 
which is used to store the failure time variable (numeric). 
}
  \item{event}{
A character string of column name, corresponds to one column of the data frame, 
which is used to store the indicator of failure event (1: failure, 0: not failure).
}
  \item{treatment}{
A character string of column name, corresponds to one column of the data frame, which is 
used to store the binary vector of treatment variable (1: treatment, 0: placebo). 
}
  \item{BaselineMarker}{
A character string of column name, corresponds to one column of the data frame, which is 
used to store a vector of baseline biomarker that is under investigation for interaction 
with treatment. The BaselineMarker variable is missing for those who are not sampled 
in the case-cohort.
}
  \item{subcohort}{
A character string of column name, corresponds to one column of the data frame, which 
is used to store the indicator of sub-cohort (1: sample belong to the sub-cohort, 
0: not belong to the sub-cohort)
}
  \item{esttype}{
The option of estimation methods (1: Self-Prentice estimator, 0: Lin-Ying estimator).
}
  \item{augment}{
The indicator of whether subcohort was drawn from the placebo arm (augment=0), from the 
active treatment arm (augment=1), or from both arms (augment=2).
}
  \item{weight}{
If the genotype data are obtained through case-control sampling, weight is a vector 
of sampling weights (inverse of sampling probability) corresponding to rows of data. 
If the genotype data are obtained through case-cohort sampling, 
weight is NULL. If a vector of weights have been supplied by user, 
then esttype is automatically set to 0: Lin-Ying estimator.
}
  \item{extra}{
A string vector of column name(s), corresponds to more or more column(s) of the data 
frame, which is/are used to store the extra baseline covariate(s) to be adjusted for 
in addition to treatment and biomarker.
}
}
\details{
The function returns point estimates and standard error estimates of parameters in the 
proportional hazards model. The method was published in Dai et al. (2015) Biometrics.
}
\value{
\item{beta}{Estimated parameter}
\item{stder}{Estimated standard error of parameter estimates}
\item{pVal}{p value}
}
\references{
J. Y. Dai, X. C. Zhang,C. Y. Wang, and C. Kooperberg. Augmented case-only designs for 
randomized clinical trials with failure time endpoints. 
Biometrics, DOI: 10.1111/biom.12392, 2016.
}
\author{
James Y. Dai
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
## Load the example data
data(acodata)
## ACO in placebo arm
rfit0 <- acoarm(data=acodata,
                 svtime="vacc1_evinf",
                 event="f_evinf",
                 treatment="f_treat",
                 BaselineMarker="fcgr2a.3",
                 subcohort="subcoh",
                 esttype=1,
                 augment=0,
                 weight=NULL,
                 extra=c("f_agele30","f_hsv_2","f_ad5gt18","f_crcm",
                 "any_drug","num_male_part_cat","uias","uras")) 
rfit0

## ACO in active arm
rfit1 <- acoarm(data=acodata,
                 svtime="vacc1_evinf",
                 event="f_evinf",
                 treatment="f_treat",
                 BaselineMarker="fcgr2a.3",
                 subcohort="subcoh",
                 esttype=1,
                 augment=1,
                 weight=NULL,
                 extra=c("f_agele30","f_hsv_2","f_ad5gt18","f_crcm",
                 "any_drug","num_male_part_cat","uias","uras")) 
rfit1

## ACO in both arms
rfit2 <- acoarm(data=acodata,
                 svtime="vacc1_evinf",
                 event="f_evinf",
                 treatment="f_treat",
                 BaselineMarker="fcgr2a.3",
                 subcohort="subcoh",
                 esttype=1,
                 augment=2,
                 weight=NULL,
                 extra=c("f_agele30","f_hsv_2","f_ad5gt18","f_crcm",
                 "any_drug","num_male_part_cat","uias","uras")) 
rfit2

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ case-only designs }
\keyword{ case-cohort designs }% __ONLY ONE__ keyword per line

