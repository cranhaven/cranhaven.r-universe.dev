% varrank.Rd ---
% Author           : Gilles Kratzer
% Created on :       29.11.2017
% Last modification :
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{varrank}
\alias{varrank}
\title{Heuristics Tools Based on Mutual Information for Variable Ranking and Feature Selection}

\usage{
varrank(data.df = NULL, variable.important = NULL, method =
                 c("battiti", "kwak", "peng", "estevez"), algorithm =
                 c("forward", "backward"),
                 scheme = c("mid", "miq"),
                 discretization.method = NULL, ratio = NULL, n.var =
                 NULL, verbose = TRUE)
                 }

\arguments{
  \item{data.df}{a named data frame with either numeric or factor variables.}
  \item{variable.important}{a list of variables that is the target variables.}
  \item{method}{the method to be used. See \sQuote{Details}. Default is \code{"estevez"}.}
  \item{algorithm}{the algorithm scheme to be used. Default is '\code{"forward"}.}
  \item{scheme}{the scheme search to be used. \code{"mid"} and \code{"miq"} stands for the Mutual Information Difference and Quotient schemes, respectively. Those are two ways to combine the relevance and redundancy. They are the two most used mRMRe schemes}
  \item{discretization.method}{a character vector giving the discretization method to use. See \code{\link{discretization}}.}
  \item{ratio}{parameter to be used in \code{"battiti"} and \code{"kwak"}.}
  \item{n.var}{number of variables to be returned.}
  \item{verbose}{logical. Should a progress bar be plotted? As the search scheme.}
  }

\description{
This function heuristically estimates the variables ranks based on mutual information with multiple model and multiple search schemes.
}

\details{
By default \code{varrank} performs a variable ranking based on forward search algorithm using mutual information. The scoring is based on one of the four models implemented. The input dataset can be discrete, continuous or mixed variables. The target variables can be a set.
The filter approach based on mutual information is the Minimum Redundancy Maximum Relevance (mRMRe) algorithm. A general formulation of the ensemble of mRMRe techniques is, given a set of features F, a subset of important features C, a candidate feature f_i and possibly some already selected features f_s in S. The local score function for a mid scheme (Mutual Information Difference) is expressed as:

\deqn{g_i(A, C, S, f_i) = MI(f_i;C) - \sum_{f_s} A(f_i, f_s, C) MI(f_i; f_s)}{%
g_i(A, C, S, F) = MI(f_i;C) - \sum_{f_s} A(f_i, f_s, C) MI(f_i; f_s)}

Depending of the value method, the value of A and B will be set accordingly to:

\code{battiti} defined A=B, where B is a user defined parameter (called ratio). Battiti (1994).

\code{kwak} defined A = B MI(f_s;C)/H(f_s), where B a user defined parameter (called ratio). Kwak et al. (2002).

\code{peng} defined A=1/|S|. Peng et al. (2005).

\code{estevez} defined A = 1/|S| min(H(f_i), H(f_s)). Estévez et al. (2009).

The search algorithm implemented are a forward search i.e. start with an empty set S and fill in. The the returned list is ranked by decreasing order of relevance. A backward search which start with a full set i.e. all variables except \code{variable.importance} are considered as selected and the returned list is in increasing order of relevance based solely on mutual information estimation. Thus a backward search will only check for relevance and not for redundancy. \code{n.var} is optional if it is not provided, all candidate variables will be ranked.
}

\value{A list with an entry for the variables ranked and another entry for the score for each ranked variables.}

\author{Gilles Kratzer}

\references{Kratzer, G. and Furrer, R.  "varrank: an R package for variable ranking based on mutual information with applications to system epidemiology"}

\examples{

if (requireNamespace("mlbench", quietly = TRUE)) {
library(mlbench)
data(PimaIndiansDiabetes)

##forward search for all variables
out1 <- varrank(data.df = PimaIndiansDiabetes,
  method = "estevez",
  variable.important = "diabetes",
  discretization.method = "sturges",
  algorithm = "forward", scheme = "mid")

##forward search for 3 variables
out2 <- varrank(data.df = PimaIndiansDiabetes,
  method = "estevez",
  variable.important = "diabetes",
  discretization.method = "sturges",
  algorithm = "forward",
  scheme = "mid",
  n.var=3)

##backward search for all variables
out3 <- varrank(data.df = PimaIndiansDiabetes,
  method = "peng",
  variable.important = "diabetes",
  discretization.method = "sturges",
  algorithm = "backward",
  scheme = "mid",
  n.var=NULL)
  }

}
