\name{INTERNAL_CONSISTENCY}
\alias{INTERNAL_CONSISTENCY}
\alias{INTERNAL.CONSISTENCY}
\title{Internal consistency reliability coefficients}
\description{Internal consistency reliability coefficients}
\usage{INTERNAL_CONSISTENCY(data, extraction = 'minres', reverse_these = NULL, 
	                        auto_reverse = TRUE, verbose=TRUE, factormodel)}  
\arguments{
  \item{data}{
  \code{}An all-numeric dataframe where the rows are cases & the columns are the variables.}

  \item{extraction}{ 
  \code{}(optional) The factor extraction method to be used in the omega computations. The options are:
         'ML' for maximum likelihood (the default); and
  	     'PAF' for principal axis / common factor analysis.}
                  
  \item{reverse_these}{
  \code{}(optional) A vector of the names of items that should be reverse-coded}

  \item{auto_reverse}{
  \code{}(optional) Should reverse-coding of items be conducted when warranted? TRUE (default) or FALSE}

  \item{verbose}{
  \code{}(optional) Should detailed results be displayed in console? TRUE (default) or FALSE}
  
  \item{factormodel}{ 
  \code{}(Deprecated.) Use 'extraction' instead.}
}
\details{
When 'auto_reverse = TRUE', the item loadings on the first principal component
are computed and items with negative loadings are reverse-coded.

If error messages are produced, try using 'auto_reverse = FALSE'.
	
If item names are provided for the 'reverse_these' argument, then auto_reverse is not conducted.
	
Run one of the following commands for descriptions of 
the alpha, omega, and other coefficients produced by this function: \cr
\itemize{

\item{RShowDoc("Coefficient_descriptions_vignettes", package = "EFA.dimensions")} \cr

\item{vignette("Coefficient_descriptions_vignettes")} \cr
}
}
\value{A list with the following elements:
 \item{int.consist_scale}{A vector with the scale omega, Cronbach's alpha, 
 	standardized Cronbach's alpha, the mean of the off-diagonal correlations, 
 	the median of the off-diagonal correlations, and
 	the rmsr fit coefficient for a 1-factor model}
 \item{int.consist_dropped}{A matrix of the int.consist_scale values for when each item, 
 	in turn, is int.consist_dropped from the analyses}
 \item{item_stats}{The item means, standard deviations, and item-total correlations}
 \item{resp_opt_freqs}{The response option frequencies}
 \item{resp_opt_props}{The response option proportions}
 \item{new_data}{The data that was used for the analyses, including any item reverse-codings}
}
\references{ 
       {Flora, D. B. (2020). Your coefficient alpha is probably wrong, but which 
       	coefficient omega is right? A tutorial on using R to obtain better reliability estimates. 
       	\emph{Advances in Methods and Practices in Psychological Science, 3(4),} 484501.}
\cr\cr {McNeish, D. (2018). Thanks coefficient alpha, we'll take it from here. 
	    \emph{Psychological Methods, 23(3),} 412433.}
\cr\cr {Revelle, W., & Condon, D. M. (2019). Reliability from alpha to omega: A tutorial. 
	    \emph{Psychological Assessment, 31(12),} 13951411.}
}
\author{Brian P. O'Connor }
\examples{
# Rosenberg Self-Esteem scale items -- without reverse-coding
INTERNAL_CONSISTENCY(data_RSE_not_recoded, extraction = 'minres', 
                     reverse_these = NULL, auto_reverse = FALSE, verbose=TRUE)
\donttest{
# Rosenberg Self-Esteem scale items -- with auto_reverse-coding
INTERNAL_CONSISTENCY(data_RSE_not_recoded, extraction = 'minres',
                     reverse_these = NULL, auto_reverse = TRUE, verbose=TRUE)

# Rosenberg Self-Esteem scale items -- another way of reverse-coding
INTERNAL_CONSISTENCY(data_RSE_not_recoded, extraction = 'minres',
                     reverse_these = c('Q1','Q2','Q4','Q6','Q7'), verbose=TRUE)
}
}