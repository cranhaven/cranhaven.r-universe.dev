\encoding{latin1}
\name{maxent}
\Rdversion{1.1}
\alias{maxent}
\concept{maximum entropy}
\concept{statistical mechanics}
\title{
Estimating Probabilities via Maximum Entropy: Improved Iterative Scaling
}
\description{
NOTE: This is a copy of the FD::maxent function, included in rexpokit to avoid the 
dependency on the package FD. \code{maxent} returns the probabilities that 
maximize the entropy conditional on a series of constraints that are linear 
in the features. It relies on the Improved Iterative Scaling algorithm of 
Della Pietra et al. (1997). It has been used to predict the relative abundances 
of a set of species given the trait values of each species and the 
community-aggregated trait values at a site (Shipley et al. 2006; 
Shipley 2009; Sonnier et al. 2009).
}

\usage{
maxent(constr, states, prior, tol = 1e-07, lambda = FALSE)
}

\arguments{
  \item{constr}{
vector of macroscopical constraints (e.g. community-aggregated trait values). Can also be a matrix or data frame, with constraints as columns and data sets (e.g. sites) as rows.
}
  \item{states}{
vector, matrix or data frame of states (columns) and their attributes (rows).
}
  \item{prior}{
vector, matrix or data frame of prior probabilities of states (columns). Can be missing, in which case a maximally uninformative prior is assumed (i.e. uniform distribution).
}
  \item{tol}{
tolerance threshold to determine convergence. See \sQuote{details} section. 
}
  \item{lambda}{
Logical. Should \eqn{\lambda}{lambda}-values be returned? 
}
}
\details{
This is a copy of the FD::maxent function, included in rexpokit to avoid the dependency on the package FD.
Its authorship information is Authored by: Bill Shipley \email{bill.shipley@usherbrooke.ca} 
(http://pages.usherbrooke.ca/jshipley/recherche/); Ported to FD by Etienne Laliberte. It was 
copied to rexpokit by Nick Matzke (just in order to avoid the dependency on package "FD").

Having BioGeoBEARS depend on package "FD" was sometimes problematic, as it had a variety of FORTRAN
code and dependencies that could slow/stall installation, particularly on older Windows machines
or machines without appropriate compilers. The maxent function uses only the FORTRAN file itscale5.f,
so that code was included in rexpokit, in order to include all of the FORTRAN code in a single 
package (greatly simplifying the compilation and code-review process for BioGeoBEARS, which is 
pure R.)

The function maxent is used in BioGeoBEARS only for the simple purpose of putting a probability 
distribution on the ordered variable "number of areas in the smaller daughter range" at cladogenesis.
For example, if mx01v = 0.0001 (the DEC model default), then the smaller daughter range will 
have a 100 percent probability of being of size 1 area during a vicariance event (thus, the "v" in 
"mx01v"). If mx01v = 0.5 (the DIVALIKE model default), then the smaller daughter range will 
have an equal chance of being any range of size less than the parent range. If mx01y = 0.9999 
(the BAYAREALIKE default), then the "smaller" daughter at sympatry (mx01y, y is sYmpatry) will 
have 100 percent probability of being the same size as its sister (i.e., the same range as the 
sister, i.e. "perfect sympatry" or "sympatry across all areas").

Original description from FD::maxent follows for completeness, but is not relevant for 
rexpokit/BioGeoBEARS.
	
The biological model of community assembly through trait-based habitat
filtering (Keddy 1992) has been translated mathematically
via a maximum entropy (maxent) model by Shipley et al. (2006) and
Shipley (2009). A maxent model contains three components: (i) a set
of possible states and their attributes, (ii) a set of macroscopic empirical constraints,
and (iii) a prior probability distribution \eqn{\mathbf{q}=[q_j]}{q = [qj]}.

In the context of community assembly, states are species, macroscopic
empirical constraints are community-aggregated traits, and prior probabilities
\eqn{\mathbf{q}}{q} are the relative abundances of species of the regional
pool (Shipley et al. 2006, Shipley 2009). By default, these prior
probabilities \eqn{\mathbf{q}}{q} are maximally uninformative (i.e. a uniform distribution),
but can be specificied otherwise (Shipley 2009, Sonnier et al. 2009).

To facilitate the link between the biological model and the mathematical
model, in the following description of the algorithm states are species and constraints are traits.

Note that if \code{constr} is a matrix or data frame containing several sets (rows),
a maxent model is run on each individual set. In this case if \code{prior} is a vector,
the same prior is used for each set. A different prior can also be specified for each set.
In this case, the number of rows in \code{prior} must be equal to the number of rows in \code{constr}. 

If \eqn{\mathbf{q}}{q} is not specified, set \eqn{p_{j}=1/S}{pj = 1 / S} for each of the
\eqn{S}{S} species (i.e. a uniform distribution), where \eqn{p_{j}}{pj} is the
probability of species \eqn{j}{j}, otherwise \eqn{p_{j}=q_{j}}{pj = qj}.

Calulate a vector \eqn{\mathbf{c=\left[\mathrm{\mathit{c_{i}}}\right]}=\{c_{1},\; c_{2},\;\ldots,\; c_{T}\}}{c = [ci] = {c1, c2, ..., cT}},
where \eqn{c_{i}={\displaystyle \sum_{j=1}^{S}t_{ij}}}{ci = sum(tij)}; i.e. each \eqn{c_{i}}{ci}
is the sum of the values of trait \eqn{i}{i} over all species, and \eqn{T}{T}
is the number of traits.

Repeat for each iteration \eqn{k}{k} until convergence:

1. For each trait \eqn{t_{i}}{ti} (i.e. row of the constraint matrix) calculate:

\deqn{
\gamma_{i}(k)=ln\left(\frac{\bar{t}_{i}}{{\displaystyle \sum_{j=1}^{S}\left(p_{j}(k)\; t_{ij}\right)}}\right)\left(\frac{1}{c_{i}}\right)}{%
gamma_i(k) = ln(t.mean_i / [sum(pj(k) tij)]) (1 / ci)}

This is simply the natural log of the known community-aggregated
trait value to the calculated community-aggregated trait value at
this step in the iteration, given the current values of the probabilities.
The whole thing is divided by the sum of the known values of the trait
over all species.

2. Calculate the normalization term \eqn{Z}{Z}:

\deqn{Z(k)=\left({\displaystyle \sum_{j=1}^{S}p_{j}(k)\; e^{\left({\displaystyle \sum_{i=1}^{T}\gamma_{i}(k)}\; t_{ij}\right)}}\right)}{%
Z(k) = sum(pj(k) e^(gamma_i(k) tij) ) }

3. Calculate the new probabilities \eqn{p_{j}}{pj} of each species at iteration \eqn{k+1}{k+1}:

\deqn{p_{j}(k+1)=\frac{{\displaystyle p_{j}(k)\; e^{\left({\displaystyle \sum_{i=1}^{T}\gamma_{i}(k)}\; t_{ij}\right)}}}{Z(k)}}{%
pj(k+1) = [pj(k) e^(gamma_i(k) tij)/ Z]}

4. If \eqn{|max\left(p\left(k+1\right)-p\left(k\right)\right)|\leq}{|max(pj(k+1) - pj(k))| <=} tolerance threshold (i.e. argument \code{tol}) then stop, else repeat steps 1 to 3.

When convergence is achieved then the resulting probabilities (\eqn{\hat{p}_{j}}{pj_hat})
are those that are as close as possible to \eqn{q_j}{qj} while simultaneously maximize
the entropy conditional on the community-aggregated traits. The solution to this problem is
the Gibbs distribution:

\deqn{\hat{p}_{j}=\frac{q_{j}e^{\left({\displaystyle -}{\displaystyle \sum_{i=1}^{T}\lambda_{i}t_{ij}}\right)}}{{\displaystyle \sum_{j=1}^{S}q_{j}}e^{\left({\displaystyle -}{\displaystyle \sum_{i=1}^{T}\lambda_{i}t_{ij}}\right)}}=\frac{q_{j}e^{\left({\displaystyle -}{\displaystyle \sum_{i=1}^{T}\lambda_{i}t_{ij}}\right)}}{Z}}{%
Note: equation not shown in HTML help file: please refer to PDF manual.}

This means that one can solve for the Langrange multipliers (i.e.
weights on the traits, \eqn{\lambda_{i}}{lamda_i}) by solving the linear system
of equations:

\deqn{\left(\begin{array}{c}
ln\left(\hat{p}_{1}\right)\\
ln\left(\hat{p}_{2}\right)\\
\vdots\\
ln\left(\hat{p}_{S}\right)\end{array}\right)=\left(\lambda_{1},\;\lambda_{2},\;\ldots,\;\lambda_{T}\right)\left[\begin{array}{cccc}
t_{11} & t_{12} & \ldots & t_{1S}-ln(Z)\\
t_{21} & t_{22} & \vdots & t_{2S}-ln(Z)\\
\vdots & \vdots & \vdots & \vdots\\
t_{T1} & t_{T2} & \ldots & t_{TS}-ln(Z)\end{array}\right]-ln(Z)}{%
Note: equation not shown in HTML help file: please refer to PDF manual.}

This system of linear equations has \eqn{T+1}{T+1} unknowns (the \eqn{T}{T} values
of \eqn{\lambda}{lambda} plus \eqn{ln(Z)}{ln(Z)}) and \eqn{S}{S} equations. So long as the number
of traits is less than \eqn{S-1}{S-1}, this system is soluble. In fact, the
solution is the well-known least squares regression: simply regress
the values \eqn{ln(\hat{p}_{j})}{ln(pj_hat} of each species on the trait values
of each species in a multiple regression.

The intercept is the value of \eqn{ln(Z)}{ln(Z)} and the slopes are the values
of \eqn{\lambda_{i}}{lambda_i} and these slopes (Lagrange multipliers) measure
by how much the \eqn{ln(\hat{p}_{j})}{ln(pj_hat)}, i.e. the \eqn{ln}{ln}(relative abundances),
changes as the value of the trait changes.

\code{FD::maxent.test} provides permutation tests for maxent models (Shipley 2010).

}

\value{
  \item{prob}{vector of predicted probabilities}
  \item{moments}{vector of final moments}
  \item{entropy}{Shannon entropy of \code{prob}}
  \item{iter}{number of iterations required to reach convergence}
  \item{lambda}{\eqn{\lambda}{lambda}-values, only returned if \code{lambda = T}}
  \item{constr}{macroscopical constraints}
  \item{states}{states and their attributes}
  \item{prior}{prior probabilities}
}
\references{
Della Pietra, S., V. Della Pietra, and J. Lafferty (1997) Inducing features of random fields. \emph{IEEE Transactions Pattern Analysis and Machine Intelligence} \bold{19}:1-13.

Keddy, P. A. (1992) Assembly and response rules: two goals for predictive community ecology. \emph{Journal of Vegetation Science} \bold{3}:157-164.

Shipley, B., D. Vile, and \enc{E}{E}. Garnier (2006) From plant traits to plant communities: a statistical mechanistic approach to biodiversity. \emph{Science} \bold{314}: 812--814.

Shipley, B. (2009) From Plant Traits to Vegetation Structure: Chance and Selection in the Assembly of Ecological Communities. Cambridge University Press, Cambridge, UK. 290 pages.

Shipley, B. (2010) Inferential permutation tests for maximum entropy models in ecology. \emph{Ecology} \bold{in press}.

Sonnier, G., Shipley, B., and M. L. Navas. 2009. Plant traits, species pools and the prediction of relative abundance in plant communities: a maximum entropy approach. \emph{Journal of Vegetation Science} \bold{in press}.
}
\author{
Bill Shipley \email{bill.shipley@usherbrooke.ca}, original URL: pages.usherbrooke.ca/jshipley/recherche/

Ported to \pkg{FD} by Etienne \enc{Laliberte}{Laliberte}.
}

\seealso{
\code{FD::functcomp} to compute community-aggregated traits,
and \code{FD::maxent.test} for the permutation tests proposed by Shipley (2010).

Another faster version of \code{maxent} for multicore processors called \code{maxentMC} is available from Etienne \enc{Laliberte}{Laliberte} (\email{etiennelaliberte@gmail.com}). It's exactly the same as \code{maxent} but makes use of the \pkg{multicore}, \pkg{doMC}, and \pkg{foreach} packages. Because of this, \code{maxentMC} only works on POSIX-compliant OS's (essentially anything but Windows).
}
\examples{

\dontrun{
# an unbiased 6-sided dice, with mean = 3.5
# what is the probability associated with each side,
# given this constraint?

maxent(3.5, 1:6)

# a biased 6-sided dice, with mean = 4
maxent(4, 1:6)
}
}
