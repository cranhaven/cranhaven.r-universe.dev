\name{betaplasma}
\docType{data}
\alias{betaplasma}
\title{Determinants of Beta-Carotene Levels}
\description{
Observational studies have suggested that low dietary intake or low plasma 
concentrations of retinol, beta-carotene, or other carotenoids might be associated 
with increased risk of developing certain types of cancer.  However, relatively few 
studies have investigated the determinants of plasma concentrations of these 
micronutrients. Nierenberg et al. designed a cross-sectional study to investigate the relationship between personal characteristics and dietary factors, and plasma concentrations of retinol, beta-carotene and other carotenoids. Study subjects (N = 315) were patients 
who had an elective surgical procedure during a three-year period to biopsy or 
remove a lesion of the lung, colon, breast, skin, ovary or uterus that was found to be non-cancerous. 
}
\usage{data("betaplasma")}
\format{datafile contains 315 observations on 15 variables.  This data set can be used to demonstrate multiple regression, transformations, categorical variables, outliers, pooled tests of significance and model building strategies. 
\describe{
Variable Names in order from left to right:
        \item{item}{participant id} \cr
        \item{age}{Age (years)} \cr
        \item{sex}{Sex, Factor with 2 levels (M=Male, F=Female).} \cr
        \item{smokestat}{Smoking status, Factor with 3 levels (Never, Former, Current Smoker)} \cr
        \item{bmi}{body mass index (weight/(height^2))} \cr
        \item{vituse}{Vitamin Use, Factor with 3 levels (Yes (fairly often), Yes (not often), No)} \cr
        \item{calories}{Number of calories consumed per day.} \cr
        \item{fat}{Grams of fat consumed per day.} \cr
        \item{fiber}{Grams of fiber consumed per day.} \cr
        \item{alcohol}{Number of alcoholic drinks consumed per week.} \cr
        \item{chol}{Cholesterol consumed (mg per day).} \cr
        \item{betadiet}{Dietary beta-carotene consumed (mcg per day).} \cr
        \item{retdiet}{Dietary retinol consumed (mcg per day)} \cr
        \item{betacaro}{Plasma beta-carotene (ng/ml)} \cr
        \item{retplasma}{Plasma Retinol (ng/ml)} \cr
}
}
\examples{
data(betaplasma)
beta4 <- mixcov(dep="betacaro", fixed=c("chol","sex","bmi"), random="betadiet",
                data=betaplasma,k=4,family="gaussian")

}
\references{
Schlattmann, P.(2009) \emph{Medical Applications of Finite Mixture Models.} Berlin: Springer. 

These data have not been published yet but a related reference is

Nierenberg DW, Stukel TA, Baron JA, Dain BJ, Greenberg ER (1989) \dQuote{Determinants of plasma levels of beta-carotene and retinol.}  \emph{American Journal of Epidemiology}, \bold{130}, 511--521.

The use of the data is kindly permitted Dr. Therese Stukel, Dartmouth Hitchcock Medical Center, USA

The data can also be found at StatLib: http://lib.stat.cmu.edu/index.php
}
\keyword{datasets}
