\name{significance_Dx}
\alias{significance_Dx}
\alias{Heterocitation Significance}
\concept{Heterocitation Balance}
\concept{Glass Effect Size}
\concept{p value}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to evaluate the significance of the heterocitation balance value}
\description{
This function assesses to what extent the heterocitation balance (Dx value) calculated for a graph departs from baseline situation. The latter typically represents Dx values to be expected by chance, i.e. through random permutation of corpus assignation at the node/vertex level (see \code{\link{MC_baseline_distribution}}). A Shapiro-Wilk test is first executed on the control distribution (using \code{\link[stats]{shapiro.test}}) and if the normality hypothesis is not rejected, a one-sample t test (see \code{\link[stats]{t.test}}) is used to test whether value is significantly different from the control distribution. The strength of this difference is additionally assessed through Glass' delta, an estimator of effect size (Glass, McGraw, and Smith, 1981).
}
\usage{
significance_Dx(value, control, normality_threshold=0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{value}{Heterocitation balance (Dx) calculated for the citation network studied}
  \item{control}{Baseline distribution of Dx values in control experiments}
  \item{normality_threshold}{P value threshold under which the hypothesis of normality is rejected in the preliminary Shapiro-Wilk test}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Returns a list containing the p-value obtained in a one-sample t test comparing value and the control distribution (with null hypothesis being that value could come from the control distribution) or NA if the control distribution is not normal based on a Shapiro-Wilk normality test, and Glass' estimator of effect size.}
\references{
Glass, G. V., McGraw, B., & Smith, M. L. (1981). Meta-analysis in social research. Beverly Hills: Sage Publications.
}
\author{
Christian Vincenot (christian@vincenot.biz)
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{significance_Dx}}, \code{\link{heterocitation}}
}
\examples{
\dontrun{
# Heterocitation in our graph
heterocitation(gr_sx, labels=labels, 1987, 2005)
### [1] "Sx ALL /  ABM  /  IBM"
### [1] "0.047 / 0.214 / 0.007"
### [1] "Dx ALL /  ABM  /  IBM"
### [1] "-0.927 / -0.690 / -0.982"


# Generate a baseline distribution for Dx values obtained through chance
# Here, we run 200 iterations of node corpus permutations
baseline<-MC_baseline_distribution(gr_sx, labels, 1987, 2018, 200)

# Assess whether our observed Dx is possibly due to chance
significance_Dx(-0.927, baseline[["Dx ALL"]])
### [1] "Distribution is normal. Performing t-test."
###
###   One Sample t-test
###
### data:  value - control
### t = -323.0017, df = 319, p-value < 2.2e-16
### alternative hypothesis: true mean is not equal to 0
### 95 percent confidence interval:
###  -0.9159834 -0.9048923
### sample estimates:
###  mean of x 
### -0.9104379 
###
### [1] "Glass' effect size: -18.0563442219448"
}
}
%\keyword{ ~kwd1 }
