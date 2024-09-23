\name{mele}
\alias{mele}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{function to compute the maximum estimated likelihood estimator}
\description{
This function computes the maximum estimated likelihood estimator (MELE) of 
regression parameters, which assess treatment-biomarker interactions in studies 
with two-phase sampling in randomized clinical trials. The function has an option 
to incorporate the independence between a randomized treatment and the baseline markers.
}
\usage{
mele(data, response, treatment, BaselineMarker, extra = NULL, phase, 
ind = TRUE, maxit=2000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A data frame used to access the following data. Each row contains the response and 
predictors of a study participant. All variables are numerical.
}
  \item{response}{
A character string of column name, corresponds to one column of the data frame, which 
is used to store a numeric vector of response. The response variable should be coded 
as 1 for cases and 0 for controls.
}
  \item{treatment}{
A character string of column name, corresponds to one column of the data frame, which
 is used to store a binary vector of the treatment . The treatment variable should 
be coded as 1 for treatment and 0 for placebo.
}
  \item{BaselineMarker}{
A character string of column name, corresponds to one column of the data frame, which 
is used to store a vector of biomarker that is assessed for interaction with the 
treatment. The BaselineMarker variable is missing for those who are not sampled in 
the second phase.
}
  \item{extra}{
A string vector of column name(s), corresponds to one or more column(s) of the data 
frame, which are used to store the extra covariate(s) to be adjusted for in addition 
to treatment and biomarker. All extra variables are missing for those 
who are not sampled in the second phase.
}
  \item{phase}{
A character string of column name, correspond to one column of the data frame, which 
is used to store the  indicator of two-phase sampling (1: not being sampled for 
measuring biomarker; 2: being sampled for measuring biomarker).
}
  \item{ind}{
A logical flag. TRUE indicates incorporating the independence between the randomized 
treatment and the baseline markers.
}
  \item{maxit}{
A integer number of the maximal number of iteration.
}
}
\details{
The function returns estimates, standard errors, and p values for MELE of a regression 
model for treatment-biomarker interaction studies with two-phase sampling in randomized 
trials, response ~ treatment + biomarker + treatment*biomarker + other covariates. 
Treatment and response are available for all the samples, while baseline biomarker data 
are available for a subset of samples. The mele can incorporate the independence 
between the treatment and baseline biomarkers ascertained in the phase-two sample.
}
\value{
\item{beta}{Estimated parameter}
\item{stder}{Standard error}
\item{pVal}{p value}
}
\references{
J. Y. Dai, M. LeBlanc, and C. Kooperberg. Semiparametric estimation exploiting covariate 
independence in two-phase randomized trials. Biometrics, 65(1):178-187, 2009.
}
\author{
James Y. Dai
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{spmle}}
}
\examples{
## Load the example data
data(whiBioMarker)
## Here is an example of MELE with exploiting independent and with confounding factors:

melIndExtra <- mele(data=whiBioMarker,  ## dataset
          response="stroke",	## response variable
          treatment="hrtdisp",		## treatment variable
          BaselineMarker="papbl",		## environment variable
          extra=c(
             "age" 	## age
                		## physical activity levels
              , "dias" 	## diabetes
              , "hyp" ## hypertension
              , "syst" 	## systolic
              , "diabtrt"	## diastolic BP
              , "lmsepi" ## waist:hip ratio
              ),	## extra variable(s)
          phase="phase",	## phase indicator
          ind=TRUE	## independent or non-indepentent
)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ two-phase sampling }
\keyword{ semiparametric maximum likelihood estimate }% __ONLY ONE__ keyword per line

