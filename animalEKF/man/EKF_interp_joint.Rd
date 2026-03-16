\name{EKF_interp_joint}
\alias{EKF_interp_joint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extended Kalman Filter (EKF) for joint shark movement with interpolation
}
\description{
Extended Kalman Filter (EKF) for joint shark movement with interpolation
}
\usage{
EKF_interp_joint(area_map=NULL, d, npart=100, sigma_pars, tau_pars,
                 mu0_pars=list(alpha=c(-4.5 ,-2), beta=c(0,0)),
                 V0_pars=list(alpha=c(0.25, 0.25), beta=c(0.25, 0.25)), 
                 Errvar0=rep(list(diag(2)), 2),
                 Errvar_df=c(20, 20), Particle_errvar0, Particle_err_df=20,
                 dirichlet_init=c(9,2,2,7), logvelocity_truncate=c(-10, 15),
                 maxStep=NULL, delaysample=1, state_favor=c(1,1),
                 nstates=2,centroids=matrix(c(0,0), ncol=2),
                 truncate_to_map=TRUE, enforce_full_line_in_map=TRUE,
                 do_trunc_adjust=TRUE, lowvarsample=TRUE,
                 time_radius=60*30, spat_radius=300, min_num_neibs=10, 
                 interact=TRUE, interact_pars=list(mu0=0, precision0=2, 
                 known_precision=2), neff_sample=1, time_dep_trans=FALSE,
                 time_dep_trans_init=dirichlet_init, smoothing=FALSE,
                 fix_smoothed_behaviors=TRUE, smooth_parameters=TRUE, 
                 reg_dt=120, max_int_wo_obs=NULL, resamp_full_hist=TRUE, 
                 compare_with_known=FALSE, known_trans_prob=NULL, 
                 known_foraging_prob=NULL, known_regular_step_ds=NULL,	
                 update_eachstep=FALSE, update_params_for_obs_only=FALSE,
                 output_plot=TRUE, loc_pred_plot_conf=0.5,
                 output_dir=getwd(), pdf_prefix="EKF_2D", verbose=3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{area_map}{
Shapefile within which the observations are located (optional).  Should be the output of applying \code{sf::st_geometry} on an object of class \code{sf}.
If input is NULL, a default rectangular one is created that contains the observed X-Y points in \code{d}.
}
  \item{d}{
Dataset of observations, with required variable columns: tag, X, Y, logvelocity, speed, turn.angle.rad, region (optional), date_as_sec, time_to_next, state.guess2, prev.guess2.
}
  \item{npart}{
Number of particles to be used in simulation.
}
  \item{sigma_pars}{
Vector of inverse-gamma parameters for sigma^2 (logV variance).  Two elements for each state.  The inverse gamma parameters are specified in pairs.
}
  \item{tau_pars}{
Vector of inverse-gamma parameters for tau^2 (turn angle variance).
}
  \item{mu0_pars}{
List of initial values of mean logV (alpha) and turn (beta) for one or two behavioral states.
}
  \item{V0_pars}{
List of initial values of degrees of freedom of inverse-gamma sigma and tau (variances of alpha and beta) for 
one or two behavioral state.
}
  \item{Errvar0}{
List of prior 2x2 covariance matrices for predicting y from x, one for each behavioral state.
}
  \item{Errvar_df}{
Vector of degrees of freedom of \code{Errvar0} covariance matrices.
}
  \item{Particle_errvar0}{
Prior 2x2 covariance matrix for predicting x_t from x_[t-1].
}
  \item{Particle_err_df}{
Degree of freedom of \code{Particle_errvar0}.
}
  \item{dirichlet_init}{
List of 4-element vectors specifying Dirichlet parameters for transition matrices for each region.  Will be replicated to equal number of regions.
}
  \item{logvelocity_truncate}{
When simulating log-velocity, a vector of the allowable range (values outside will be truncated to fall in this range).
Log-velocity is simulated by a normal distribution (which is symmetric but can be positive or negative), so that speed (=exp(log_velocity)) will be 
positive.  However, the transformation has asymmetric impact in that, say, a fixed error in underestimating log-velocity results in a smaller
displacement (when translated to speed and thus distance) than the same error over-estimated.  The variance of 
log-velocity takes into account low and high values equally.  This restriction prevents the variance from growing too large
from low (e.g. very negative) values of log-velocity, which will then cause large over-estimates of speed and distance traveled.
The difference between, say, log-velocity of -2 and -50 is very small in practical terms of distance, but the effect
on the variance will be much larger for the -50.
}
  \item{maxStep}{
Maximum number of regular steps to simulate.  Default is NULL, meaning that the number of regular steps simulated will be the minimum number
required to cover the range of observed data.  If not NULL, maxStep will be the minimum of the submitted value or the the above.
}
  \item{delaysample}{
Number of regular steps at which resampling will begin.  The default =1 means resampling will begin immediately.
}
  \item{state_favor}{
Vector of weights to favor states when resampling (but not propagating).  For instance c(1,3)
will favor state 2 weight 3 times as much as state 1 weights for particles.  By default, they are equally weighted.
}
  \item{nstates}{
Number of behavioral states.  For now restricted to a maximum of 2.
}
  \item{centroids}{
Matrix with two columns specifying the centroids of regions.
}
  \item{truncate_to_map}{
Logical.  If TRUE, make sure that coordinate predictions are inside the boundary \code{area_map} by truncated sampling.
}
  \item{enforce_full_line_in_map}{
Logical.  If TRUE, when conducting truncated sampling (\code{truncate_to_map==TRUE}), count the prediction of the next
location as being inside the boundary if the full line segment connecting it to the current location is inside the map.
Otherwise, only the predicted point (and not the line connecting them) must be inside the map.  The idea is that the truncation
allows only 'feasible' straight-line moves to be made, and so the full line segment must be inside the map.  However,
there may be situations in which this restriction prevents the algorithm from making good predictions, such as if the time gap
\code{reg_dt} is too long, or if the map contains 'narrow' areas where requiring the line to be inside would 
prevent a prediction and the algorithm would get 'stuck'.
}
  \item{do_trunc_adjust}{
Logical.  If TRUE, adjust particle posterior weights by the fraction of their predictions that are within the truncation boundary.
}
  \item{lowvarsample}{
Logical.  If TRUE, use low-variance sampling when resampling particles to ensure particles are resampled proportionately to weight.  Otherwise there is some sampling variance when drawing random samples.
The setting applies to smoothing as well.
}
  \item{time_radius}{
Time in seconds to consider for spatial neighbors.
}
  \item{spat_radius}{
Radius in meters of (circular) spatial neighborhood.
}
  \item{min_num_neibs}{
Minimum number of time and spatial radius observations that need to exist to constitute a neighborhood.
}
  \item{interact}{
Logical.  If TRUE, simulate interaction parameters of neighborhood.   If \code{nstates}=1, or if only one shark, automatically set to FALSE.
}
  \item{interact_pars}{
List of interaction priors: \code{mu0} and \code{precision0} are prior mu and precision for normal draws of interaction parameter.  \code{known_precision} is the known precision of the lognormal intensity.
}
  \item{neff_sample}{
Number between 0 and 1.  If effective sample size < \code{neff_sample}, then resample.  Recommended to always resample if interpolating, so set neff_sample=1 as default.
}
  \item{time_dep_trans}{
Logical. If TRUE, state transition matrices are time-dependent meaning that probability depends on the number of steps a shark has remained in the current state.
}
  \item{time_dep_trans_init}{
4-element numeric vector of Dirichlet parameters for \code{time_dep_trans}.
}
  \item{smoothing}{
Logical.  If TRUE, perform smoothing at the end.
}
  \item{fix_smoothed_behaviors}{
Logical.  If TRUE, when performing smoothing, keep behavior modes fixed for each particle history from what was originally predicted during filtering, 
before smoothing.  This means the particles will be smoothed backwards with each particle weight at each time point being conditioned on the 
behavior predicted in filtering.  Thus, the behavioral agreement with, say, the observed or true behaviors is the same for smoothing as for 
filtering, since behaviors are not allowed to change.  If \code{nstates==1}, then automatically \code{fix_smoothed_behaviors=FALSE}.
}
  \item{smooth_parameters}{
Logical.  If TRUE, when performing smoothing, resample the parameters theta as well.
}
  \item{reg_dt}{
Length in seconds of each regular interval.
}
  \item{max_int_wo_obs}{
When simulating, the maximum number of intervals of length \code{reg_dt} without observations for a given shark that we will simulate.  If this is exceeded, algorithm will wait until next observation and start from there.
Default is NULL, meaning it will be set to \code{maxStep}, and thus the algorithm will continue simulating without 
stopping, regardless of when the next observation is.
}
  \item{resamp_full_hist}{
Logical.  If TRUE, resample the full particle history, not just all particle times since the last observation, each time resampling occurs.
}
  \item{compare_with_known}{
Logical.  If TRUE, provide a known regular-step dataset from which \code{d} is a irregularly-sampled subset, for comparison with particle predictions.
}
  \item{known_trans_prob}{
If \code{nstates = 2}, a matrix of row 2 where each column is the behavior transition probabilities between each opposing behavior, in each region.  
}
  \item{known_foraging_prob}{
If \code{nstates = 2}, a matrix of the foraging probabilities for each region. 
}
  \item{known_regular_step_ds}{
If \code{compare_with_known = TRUE}, the dataset of the original regular-step trajectories. Note: this dataset needs to have 
column \code{date_as_sec} (date in seconds) and time gap \code{reg_dt} be the same as the set of regular-step intervals
that the EKF is trying to estimate movement at.  Otherwise, the simulated movement locations and the true ones will not
correspond.
}
  \item{update_eachstep}{
Logical.  If TRUE, for regular steps without observations, update the movement parameters based on the simulated movements.  If FALSE, parameters are
only updated based on the simulated movements when a new observation occurs; this means the simulated movements are drawn using the parameter values learned 
since the last observation.
}
  \item{update_params_for_obs_only}{
Logical.  If TRUE, the particle movement parameters are updated based on simulated movement only at intervals with observed locations.  
If FALSE, particle movement in intermediate steps that are simulated will be used to update as well.
If TRUE, then \code{update_eachstep = FALSE}, meaning that parameter updates will be done only for (and at) the steps that represent observations.
If FALSE, then \code{update_eachstep} can be either TRUE or FALSE, but simulated steps will be used to update, but 
\code{update_eachstep} controls the timing of the the update; if TRUE, it happens one step at a time, and if FALSE, a batch update is done
at observations for the set of simulated steps. 
}
  \item{output_plot}{
Logical.  If TRUE, a set of diagnostic plots will be printed to a file in \code{output_dir}.  Otherwise, it 
will be output to the plotting console.
}
  \item{loc_pred_plot_conf}{
Numeric.  Confidence level of ellipse for location prediction error to plot in step-wise diagnostics.
}
  \item{pdf_prefix}{
String prefix for output PDF filename, if \code{output_plot = TRUE}.  Filename will be the prefix followed by a timestamp.
}
  \item{output_dir}{
Directory for output PDF of diagnostic plots.
}
  \item{verbose}{
Integer, one of 0,1,2,3. Control of verbosity of printouts during simulation.  3 means show both printouts and plots; 2 means show plots only; 
1 means show printouts only; 0 means show no plots or prinouts.  Final plotting will be done regardless.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use

Many of the returned values are the same as in \code{\link{EKF_1d_interp_joint}}.  The ones that differ are listed below. 

\item{centroids}{Input centroids of spatial regions.}
\item{nregions}{Number of unique regions, as determined by \code{centroids}}.
\item{tau_pars}{Posterior inverse gamma distribution parameters for the turn angle variance.}
\item{cov_err_hist}{Overall history of location estimate error draws.}
\item{param_draws}{Posterior sampled valued of mean of log-velocity and turn.}
\item{variance_draws}{Posterior sampled valued of variance of log-velocity and turn.}
\item{trans_mean_byregion}{Posterior estimates of mean behavior switching probabilities from \code{region_trans_draws}.}
\item{region_counts}{Array of total number of simulated regular-step intervals that shark begin movement in each spatial region.  A proxy for the 
total amount of time spent in each region.}
\item{euclidean_estimate_true_from_obs}{Estimates of true locations by Euclidean and Bezier cubic spline interpolation from observations}
\item{error_euclidean_estimate_true_from_obs}{Euclidean error from \code{euclidean_estimate_true_from_obs} compared to true locations from \code{known_regular_step_ds}.}

The following inputted parameters are returned:
\item{area_map}{}


%% ...
}
\references{
Ackerman, Samuel.  "A Probabilistic Characterization of Shark Movement Using Location Tracking Data."  Temple University doctoral thesis, 2018.  \url{https://digital.library.temple.edu/digital/collection/p245801coll10/id/499150}

Carvalho, Carlos M., Johannes, Michael S., Lopes, Hedibert F., and Nicholas G. Polson.  "Particle learning and smoothing."  Statistical Science, 2010.
}
\author{
Samuel Ackerman
}
\note{
See \code{\link{sim_trajectory_joint}} for a full example of usage.
Video explanation of EKF state-space model by author: \url{https://youtu.be/SgyhRVUn77k}

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ts }% use one of  RShowDoc("KEYWORDS")
\keyword{ models }% __ONLY ONE__ keyword per line
\keyword{ spatial }

