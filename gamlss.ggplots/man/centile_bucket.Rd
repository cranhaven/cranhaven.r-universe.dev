\name{centile_bucket}
\alias{centile_bucket}
\alias{centile_bucket_wrap}
\alias{model_cent_bucket}

\title{
Centile bucket plot
}
\description{
A bucket plot is a graphical way to check the skewness and kurtosis of a continuous variable or the residuals of a fitted GAMLSS model. It plots the centile skewness (tail or central) and  transformed centile kurtosis of the variable (or residuals) together with a cloud of points obtained using a non-parametric bootstrap from the original variable (or residuals). It also provides a graphical way of performing a Monte Carlo simulation test on whether the centile skewness and transformed centile kurtosis of the variable of interest are simultaneously equal to zero. 

There are two function here:

i) \code{cenlile_bucket()} for a single bucket plot. Note that  \code{model_cent_bucket()}  and \code{centile_bucket()} are synonymous.

ii) \code{centile_bucket_wrap()} for multiple bucket plots cut according to  terms in the model. 



}
\usage{
centile_bucket(x, ..., type = c("tail", "central"), weights = NULL, 
       no_bootstrap = 99, col_bootstrap = hcl.colors(length.obj, 
       palette = "Set 2"), alpha_bootstrap = 1, text_to_show = NULL, 
       cex_text = 5, col_text = "black", colour_bucket = FALSE, 
       line_width = 0.5, sim_test = FALSE, no_sim_test = 1000, 
       col_sim_test = gray(0.7), alpha_sim_test = 0.1, seed_test = 1234)

model_cent_bucket(x, ..., type = c("tail", "central"), weights = NULL, 
       no_bootstrap = 99, col_bootstrap = hcl.colors(length.obj, 
       palette = "Set 2"), alpha_bootstrap = 1, text_to_show = NULL, 
       cex_text = 5, col_text = "black", colour_bucket = FALSE, 
       line_width = 0.5, sim_test = FALSE, no_sim_test = 1000, 
       col_sim_test = gray(0.7), alpha_sim_test = 0.1, seed_test = 1234)


centile_bucket_wrap(x, ..., type = c("tail", "central"), weights = NULL, 
      xvar = NULL, n_inter = 4, no_bootstrap = 99, 
      col_bootstrap = hcl.colors(length.obj, palette = "Set 2"), 
      alpha_bootstrap = 1, text_to_show = NULL, check_overlap_text = FALSE, 
      cex_text = 5, col_text = "black", colour_bucket = FALSE, 
      line_width = 0.5, sim_test = FALSE, no_sim_test = 1000, 
      col_sim_test = gray(0.7), alpha_sim_test = 0.1, seed_test = 1234)       
       
}
\arguments{
 \item{x}{x should be a continuous vector of a GAMLSS fitted model. }
 \item{\dots}{for more that one continuous vectors or fitted models}
 \item{type}{whether "tail" of "central" skewness and kurtosis}
 \item{weights}{if priors weights are needed}
 \item{no_bootstrap}{the number of bootstrap samples for the cloud around the point of skewness and kurtosis.}
 \item{col_bootstrap}{The colour of the bootstrap samples}
 \item{alpha_bootstrap}{The transparency parameter of the bootstrap samples.}
 \item{text_to_show}{what text to show in the plots, default the names of vectors or models}
 \item{cex_text}{the character size of the text}
 \item{col_text}{the colour of the text}
 \item{colour_bucket}{whether colour or gray lines in the bucket}
 \item{line_width}{the line width}
 \item{sim_test}{whether to Monde Carlo simulation is needed to check the null hypothesis that there is no centile skewness and transformed centile kurtosis in the sample.}
 \item{no_sim_test}{The number of simulation for the test}
 \item{col_sim_test}{the colour used for displaying the Monde Carlo test values}
 \item{alpha_sim_test}{The transparency parameter of the Monde Carlo samples.}
 \item{seed_test}{A seed value for the Monde Carlo simulation.}
 \item{xvar}{the x term}
 \item{n_inter}{how many intervals needed}
 \item{check_overlap_text}{whether to check overlapping text}
}
\details{
More details about centile bucket plots is given in De Bastiani \emph{et al.} (2022)
}
\value{
A plot displaying the centile skewness and  transformed centile kurtosis of the sample or residual of a model.
}
\references{
De Bastiani, F.,   Stasinopoulos, D. M., Rigby, R. A., Heller, G. Z., and Lucas A. (2022) Bucket Plot: A Visual Tool for Skewness and Kurtosis Comparisons. To be  published.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. \doi{10.1201/9780429298547}
	An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC. \doi{10.1201/b21973}

Stasinopoulos, M. D., Rigby, R. A., and De Bastiani F., (2018) GAMLSS: a distributional regression approach, \emph{Statistical Modelling}, Vol. \bold{18}, pp, 248-273, SAGE Publications Sage India: New Delhi, India.


Stasinopoulos, M.D., Kneib, T., Klein, N., Mayr, A. and Heller, G.Z., (2024). \emph{Generalized Additive Models for Location, Scale and Shape: A Distributional Regression Approach, with Applications} (Vol. \bold{56}). Cambridge University Press.

(see also  \url{https://www.gamlss.com/}).
}
\author{Mikis Stasinopoulos, Bob Rigby and Fernanda De Bastiani}
\note{
The  bucket plot provides an additional residual diagnostic tool that can be used for fitted model checking, alongside other diagnostic tools, for example   worm plots, and Q (and Z) statistics.
}

\seealso{\code{\link[gamlss]{wp}, \link[gamlss]{Q.stats}}}

\examples{
m1 <- gamlss(R~pb(Fl)+pb(A), data=rent, family=GA)
centile_bucket(m1)
\donttest{
centile_bucket_wrap(m1, xvar=rent$A)
}
}

\keyword{regression}
