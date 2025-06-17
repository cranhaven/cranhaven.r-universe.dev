\name{cooccur}
\alias{cooccur}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to calculate pairwise co-occurrence patterns from a community dataset
}
\description{
This is the main function in the package \code{cooccur}. This R package applies the probabilistic model of species co-occurrence (Veech 2013) to a set of species distributed among a set of survey or sampling sites. The algorithm calculates the observed and expected frequencies of co-occurrence between each pair of species. The expected frequency is based on the distribution of each species being random and independent of the other species. The analysis returns the probabilities that a more extreme (either low or high) value of co-occurrence could have been obtained by chance. The package also includes functions for visualizing species co-occurrence results and preparing data for downstream analyses.

This function takes a community dataset (data frame or matrix) of species by site presence-absence data and classifies species pairs as having positive, negative, and random associations based on the probabilistic model of specie co-occurrence from Veech (2013). It produces an object of class \code{cooccur}.
}
\usage{
cooccur(mat, type = "spp_site", thresh = TRUE, spp_names = FALSE,
        true_rand_classifier = 0.1, prob = "hyper", 
        site_mask = NULL, only_effects = FALSE,
        eff_standard = TRUE, eff_matrix = FALSE)
        }
\arguments{
  \item{mat}{
A species by site (r,c) data.frame or matrix of presence-absence (or abundance) data. Or, a site by species dataset with \code{type="site_spp"}.
}
  \item{type}{
Default is \code{"spp_site"} meaning \code{mat} specifies species by site (r,c) data. For site by species use \code{"site_spp"}.
}
  \item{thresh}{
Logical. If TRUE then species pairs that are expected have less than 1 co-occurrences are filtered from the analysis. This will remove pairs from the results.
}
  \item{spp_names}{
Logical. Are there species names in either the column names or row names of the species matrix? If so then \code{spp_names} should be TRUE to use them. 
}
  \item{true_rand_classifier}{
When a species pair is not classified as positive or negative, the species pair can be truely randomly distributed or they can be unclassifiable due to low statistical power. We classify truly random associations as those that do not deviate from their expected co-occurrences by more than 0.1 x the total number of sites. Therefore the default value is 0.1 but any proportion can be specified to be more or less strict.
}
  \item{prob}{
Should co-occurrence probabilities be calculated using the hypergeometric distribution (\code{prob="hyper"}) or the combinatorics approach from Veech 2013 (\code{prob="comb"}).
}
  \item{site_mask}{
A matrix with the same dimensions as \code{mat}, where each cell is a 1 or 0. 1 indicates sites that a species can exist and 0 indicates sites where the species cannot exist.
}
  \item{only_effects}{
Logical. Probability calculations can be time consuming; if only effect sizes are required from the analysis, then specific TRUE.
}
  \item{eff_standard}{
Logical. If \code{only_effects=TRUE} then should the effect sizes be standardized?
}
  \item{eff_matrix}{
Logical. If \code{only_effects=TRUE} then should the effect sizes be returned in a distance matrix? The alternative is a list of pairs.}
}
\value{
Returns a list of class \code{cooccur}.
\item{call }{Original function call.}
\item{results }{Probability table containing all analyzed species pairs and their observed and expected co-occurrences and probabilities for classifying positive and negative associations.}
\item{positive }{Number of positive pairs.}
\item{negative }{Number of negative pairs.}
\item{co_occurrences }{Sum of positive and negative pairs.}
\item{pairs }{Number of analyzed species pairs.}
\item{random }{Number of truly random species pairs.}
\item{unclassifiable }{Number of unclassifiable species pairs.}
\item{sites }{Number of sites.}
\item{species }{Number of species.}
\item{percent_sig }{Percent of pairs classified as either positive or negative.}
\item{true_rand_classifier }{The proportion used to classify truly random pairs.}
\item{spp_key }{A key relating the species numbers and names.}
\item{spp.names }{Ordered list of species names.}
\item{omitted }{Number of pairs removed due to the threshold filter.}
\item{pot_pairs }{Number of pairs before application of the threshold.}

}
\references{
Veech (2013). A probabilistic model for analysing species co-occurrence. Global Ecology and Biogeography, DOI: 10.1111/j.1466-8238.2012.00789.x

Griffith, Veech, and Marsh (2016). cooccur: Probabilistic Species Co-Occurrence Analysis in R. Journal of Statistical Software, 69(2), 1-17. doi:10.18637/jss.v069.c02
}
\author{
Daniel M Griffith
}

\examples{

#data(finches)
#cooccur.finches <- cooccur(mat=finches,
#			   type="spp_site",
#			   thresh=TRUE,
#			   spp_names=TRUE)
#summary(cooccur.finches)
#plot(cooccur.finches)
}

\keyword{co-occurrence}
