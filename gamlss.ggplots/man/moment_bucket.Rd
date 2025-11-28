\name{moment_bucket}
\alias{moment_bucket}
\alias{moment_bucket_wrap}
\alias{model_mom_bucket}

\title{Moment bucket plot}
\description{
A bucket plot is a graphical way to check the skewness and kurtosis of a continuous variable or the residuals of a fitted GAMLSS model. It plots the transformed moment skewness and  transformed moment kurtosis of the variable (or residuals) together with a cloud of points obtained using a non-parametric bootstrap from the original variable (or residuals). It also provides a graphical way of performing the  Jarque-Bera test (JarqueandBera,1980). 

There are two function here:

i) \code{moment_bucket()} for a single bucket plot. Note that  \code{model_mom_bucket()}  and \code{moment_bucket()} are synonymous.

ii) \code{moment_bucket_wrap()} for multiple bucket plots cut according to  terms in the model. 

}
\usage{
moment_bucket(x, ..., weights = NULL, no_bootstrap = 99, 
           col_bootstrap = hcl.colors(length.obj, palette = "Set 2"), 
           alpha_bootstrap = 1, text_to_show = NULL, 
           cex_text = 5, col_text = "black", colour_bucket = FALSE, 
           line_width = 0.5, col_JB_test = gray(.7), alpha_JB_test = .1)
           
model_mom_bucket(x, ..., weights = NULL, no_bootstrap = 99, 
          col_bootstrap = hcl.colors(length.obj, palette = "Set 2"), 
          alpha_bootstrap = 1, text_to_show = NULL, 
          cex_text = 5, col_text = "black", colour_bucket = FALSE, 
          line_width = 0.5, col_JB_test = gray(.7), alpha_JB_test = .1)           
           
moment_bucket_wrap(x, ..., weights = NULL, xvar = NULL, n_inter = 4,
          no_bootstrap = 99, 
          col_bootstrap = hcl.colors(length.obj, palette = "Set 2"), 
          alpha_bootstrap = 1, text_to_show = NULL, 
          check_overlap_text = FALSE, cex_text = 5, 
          col_text = "black", colour_bucket = FALSE,
          col_JB_test = gray(.7), alpha_JB_test = .1)           
           
}
\arguments{
  \item{x}{x should be a continuous vector of a GAMLSS fitted model. }
  \item{\dots}{this for more that one continuous vectors or fitted models}
  \item{weights}{if priors weights are needed}
  \item{no_bootstrap}{the number of bootstrap samples for the cloud around the point of skewness and kurtosis.}
  \item{col_bootstrap}{The colour of the bootstrap samples}
  \item{alpha_bootstrap}{The transparency parameter of the bootstrap samples.}
  \item{text_to_show}{what text to show in the plots, default the names of vectors ot models}
  \item{cex_text}{the character size of the text}
  \item{col_text}{the colour of the text}
  \item{colour_bucket}{whether colour or gray lines in the bucket}
  \item{line_width}{the line width}
  \item{xvar}{the x term}
  \item{n_inter}{how many intervals needed}
  \item{check_overlap_text}{whether to check overlapping text}
  \item{col_JB_test}{the colour for the Jarque-Bera test}
  \item{alpha_JB_test}{the transparency constant for the Jarque-Bera test}
}

\value{
A plot displaying the transformed moment skewness and  transformed moment kurtosis of the sample or residual of a model.
}
\references{
De Bastiani, F.,   Stasinopoulos, D. M., Rigby, R. A., Heller, G. Z., and Lucas A. (2022) Bucket Plot: A Visual Tool for Skewness and Kurtosis Comparisons. To be published.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. \doi{10.1201/9780429298547}
	An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC. \doi{10.1201/b21973}

Stasinopoulos, M. D., Rigby, R. A., and De Bastiani F., (2018) GAMLSS: a distributional regression approach, \emph{Statistical Modelling}, Vol. \bold{18}, 
pp, 248-273, SAGE Publications Sage India: New Delhi, India. 

Stasinopoulos, M.D., Kneib, T., Klein, N., Mayr, A. and Heller, G.Z., (2024). \emph{Generalized Additive Models for Location, Scale and Shape: A Distributional Regression Approach, with Applications} (Vol. \bold{56}). Cambridge University Press.


(see also  \url{https://www.gamlss.com/}).

}
\author{Mikis Stasinopoulos,  Bob Rigby and Fernanda De Bastiani}

\note{
The  bucket plot provides an additional residual diagnostic tool that can be used for fitted model checking, alongside other diagnostic tools, for example   worm plots, and Q (and Z) statistics.
}

\seealso{\code{\link[gamlss]{wp}, \link[gamlss]{Q.stats}}}
\examples{
m1 <- gamlss(R~pb(Fl)+pb(A), data=rent, family=GA)
moment_bucket(m1)
moment_bucket_wrap(m1, xvar=rent$A)
}

\keyword{regression}
\concept{diagnostics}
