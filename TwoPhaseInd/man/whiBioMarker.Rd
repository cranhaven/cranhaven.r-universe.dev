\name{whiBioMarker}
\alias{whiBioMarker}
\docType{data}
\title{An example dataset to demostrate the usage of MELE and SPMLE
}
\description{
A dataset from a Women's Health Initiative (WHI) hormone trial to study the interaction 
between biomarker and hormone therapy on stroke.
}
\usage{data("whiBioMarker")}
\format{
A data frame consisting of 10 observations, with the following columns:
  \describe{
    \item{\code{stroke}}{a binary indicator vector of stroke; 1=has stroke}
    \item{\code{hrtdisp}}{a binary indicator vector of treatment in the Estrogen Plus 
         Progestin Trial; 1="Estrogen Plus Progestin", 0="placebo"}
    \item{\code{papbl}}{a numeric vector of Biomarker PAP (plasmin-antiplasmin complex)
          in logarithmic scale (base 10)}
    \item{\code{age}}{an integer vector of age}
    \item{\code{dias}}{A binary indicator vector of Diastolic BP; 1="Yes"}
    \item{\code{hyp}}{a vector of hypertension with levels \code{Missing}, 
          \code{No}, \code{Yes}}
    \item{\code{syst}}{an integer vector of Systolic BP}
    \item{\code{diabtrt}}{A vector of Diabetes with levels: \code{Missing}, 
          \code{No}, \code{Yes}}
    \item{\code{lmsepi}}{A vector of episodes per week of moderate and strenuous 
recreational physical activity of >= 20 minutes duration with levels 
          \code{2 - <4 episodes per week}, \code{4+ episodes per week}, 
           \code{Missing}, \code{No activity}, \code{Some activity}}
    \item{\code{phase}}{a numeric vector of phase; 1: phase 1, 2:phase 2}
  }

}
\details{
It is an two-phase sampling example dataset adapted from Kooperberg et al. (2007) to 
demostrate the usage of MELE and SPMLE algorithms in Dai et al. (2009).
}
\source{
C. Kooperberg, M. Cushman, J. Hsia, J. G. Robinson, A. K. Aragaki, J. K. Lynch, A. E. 
Baird, K. C. Johnson, L. H. Kuller, S. A. Beresford, and B. Rodriguez. Can biomarkers 
identify women at increased stroke risk? the women's health initiative hormone trials. 
PLoS clinical trials, 2(6):e28, Jun 15 2007.
}
\references{
J. Y. Dai, M. LeBlanc, and C. Kooperberg. Semiparametric estimation exploiting 
co-variate independence in two-phase randomized trials. Biometrics, 65(1):178-187, 2009.
}
\examples{
data(whiBioMarker)
str(whiBioMarker)
colnames(whiBioMarker)
}
\keyword{datasets}
