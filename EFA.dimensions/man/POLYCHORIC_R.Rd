\name{POLYCHORIC_R}
\alias{POLYCHORIC_R}
\title{Polychoric correlation matrix}
\description{Produces a polychoric correlation matrix}
\usage{POLYCHORIC_R(data, method, verbose)}
\arguments{
  \item{data}{
  \code{}An all-numeric dataframe where the rows are cases & the columns are the 
  variables. All values should be integers, as in the values for Likert rating scales.}

  \item{method}{
  \code{}(optional) The source package used to estimate the polychoric correlations:  
  	    'Revelle' for the psych package (the default);  	    
  	    'Fox' for the polycor package.}
                  
  \item{verbose}{
  \code{}Should detailed results be displayed in console? TRUE (default) or FALSE}
}
\details{Applying familiar factor analysis procedures to item-level data can produce 
	misleading or uninterpretable results. Common factor analysis, maximum likelihood 
	factor analysis, and principal components analysis produce meaningful results 
	only if the data are continuous and multivariate normal.  Item-level data 
	almost never meet these requirements.
	
	The correlation between any two items is affected by both their substantive 
	(content-based) similarity and by the similarities of their statistical 
	distributions.  Items with similar distributions tend to correlate more 
	strongly with one another than do with items with dissimilar distributions.  
	Easy or commonly endorsed items tend to form factors that are distinct from 
	difficult or less commonly endorsed items, even when all of the items measure 
	the same unidimensional latent variable.  Item-level factor analyses using 
	traditional methods are almost guaranteed to produce at least some factors 
	that are based solely on item distribution similarity.  The items may appear 
	multidimensional when in fact they are not.  Conceptual interpretations of 
	the nature of item-based factors will often be erroneous.
	
	A common, expert recommendation is that factor analyses of item-level data 
	(e.g., for binary response options or for ordered response option categories) 
	or should be conducted on matrices of polychoric correlations. Factor analyses 
	of polychoric correlation matrices are essentially factor analyses of the 
	relations among latent response variables that are assumed to underlie the 
	data and that are assumed to be continuous and normally distributed.
	
	This is a cpu-intensive function. It is probably not necessary when there 
	are > 8 item response categories.
	
	By default, the function uses the polychoric function from William Revelle's' 
	psych package to produce a full matrix of polychoric correlations. The function 
	uses John Fox's hetcor function from the polycor package when requested or 
	when the number of item response categories is > 8.
}
\value{The polychoric correlation matrix}
\author{Brian P. O'Connor }
\examples{
\donttest{
# Revelle polychoric correlation matrix for the Rosenberg Self-Esteem Scale (RSE)
POLYCHORIC_R(data_RSE, method = 'Revelle')

# Fox polychoric correlation matrix for the Rosenberg Self-Esteem Scale (RSE)
POLYCHORIC_R(data_RSE, method = 'Fox')
}
}
