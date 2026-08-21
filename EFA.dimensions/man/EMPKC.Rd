\name{EMPKC}
\alias{EMPKC}
\title{The empirical Kaiser criterion  method}
\description{A test for the number of common factors using the Empirical 
	Kaiser Criterion  method (Braeken & van Assen, 2017).	
}
\usage{EMPKC(data, corkind='pearson', Ncases=NULL, verbose=TRUE)}
\arguments{
 \item{data}{
 \code{}An all-numeric dataframe where the rows are cases & the columns are the variables, 
 or a correlation matrix with ones on the diagonal. The function internally determines 
 whether the data are a correlation matrix.}

 \item{corkind}{
 \code{}The kind of correlation matrix to be used if data is not a correlation matrix. 
 The options are 'pearson', 'kendall', 'spearman', 'gamma', and 'polychoric'. Required  
 only if the entered data is not a correlation matrix.}
                  
 \item{Ncases}{
 \code{}The number of cases. Required only if data is a correlation matrix.}

 \item{verbose}{
 \code{}Should detailed results be displayed in console? TRUE (default) or FALSE}
}
\details{
The code for this function was adapted from the code provided by Auerswald & Moshagen (2019).
	
From Braeken & van Assen (2017):

"We developed a new factor retention method, the Empirical Kaiser Criterion, which 
is directly linked to statistical theory on eigenvalues and to researchers' goals to 
obtain reliable scales. EKC is easily visualized, and easy to compute and apply (no 
specialized software or simulations are needed). EKC can be seen as a sample-variant 
of the original Kaiser criterion (which is only effective at the population level), 
yet with a built-in empirical correction factor that is a function of the 
variables-to-sample-size ratio and the prior observed eigenvalues in the series. 
The links with statistical theory and practically relevant scales allowed us to derive 
conditions under which EKC accurately retrieves the number of acceptable scales, 
that is, sufficiently reliable scales and strong enough items.

"Our simulations verified our derivations, and showed that (a) EKC performs about as 
well as parallel analysis for data arising from the null, 1-factor, or orthogonal 
factors model; and (b) clearly outperforms parallel analysis for the specific case of 
oblique factors, particularly whenever interfactor correlation is moderate to high and 
the number of variables per factor is small, which is characteristic of many applications 
these days. Moreover, additional simulations suggest that our method for predicting 
conditions of accurate factor retention also work for the more computer- intensive 
methods ... The ease-of-use and effectiveness of EKC make this method a prime candidate 
for replacing parallel analysis, and the original Kaiser criterion that, although it 
empirically does not perform too well, is still the number one method taught in 
introductory multivariate statistics courses and the default in many commercial 
software packages. Furthermore, the link to statistical theory opens up possibilities 
for generic power curves and sample size planning for exploratory factor analysis studies.

"Generally, the EKC accurately retrieved the number of factors in conditions whenever 
it was predicted to work well, and its performance was worse when it was not predicted 
to work well. More precisely, hit rate or power exceeded .8 in accordance with 
predictions under the null model, 1-factor model, the orthogonal factor model, and 
the oblique factor model with more than three variables per scale. Only in the case 
of minimal scales, that is, with three items per scale, did EKC sometimes not accurately 
retrieve the number of factors as predicted; dropping the restriction that eigenvalues 
should exceed 1 then mended EKC's performance. A general guideline for application that 
can be derived from our results (and would not need a study-specific power study), is 
that EKC will accurately retrieve the number of factors in samples of at least 100 
persons, when there is no factor, one practically relevant scale, or up to five 
practically relevant uncorrelated scales with a reliability of at least .8." (pp. 463-464)


From Auerswald & Moshagen (2019):

"The Empirical Kaiser Criterion (EKC; Braeken & van Assen, 2017) is an approach 
that incorporates random sample variations of the eigenvalues in Kaiser's criterion. 
On a population level, the criterion is equivalent to Kaiser's criterion and extractions 
all factors with associated eigenvalues of the correlation matrix greater than one. 
However, on a sample level, the criterion takes the distribution of eigenvalues for 
normally distributed data into account." (p. 474)
}
\value{The number of factors according to the EMPKC test.
}
\references{
{Auerswald, M., & Moshagen, M. (2019). How to determine the number of factors to 
	retain in exploratory factor analysis: A comparison of extraction methods under 
	realistic conditions. \emph{Psychological Methods, 24(4),} 468-491.}
\cr\cr{Braeken, J., & van Assen, M. A. (2017). An empirical Kaiser criterion. 
	\emph{Psychological Methods, 22,} 450 - 466.}
}
\author{Brian P. O'Connor }
\examples{
\donttest{
# the Harman (1967) correlation matrix
EMPKC(data_Harman, Ncases = 305)

# Rosenberg Self-Esteem scale items, using polychoric correlations
EMPKC(data_RSE, corkind='polychoric')

# NEO-PI-R scales
EMPKC(data_NEOPIR)
}
}
