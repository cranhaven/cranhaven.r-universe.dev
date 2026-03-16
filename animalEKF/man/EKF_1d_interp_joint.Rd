\name{EKF_1d_interp_joint}
\alias{EKF_1d_interp_joint}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extended Kalman Filter (EKF) for 1-D movement with interpolation
}
\description{
Extended Kalman Filter (EKF) for 1-D movement with interpolation
}
\usage{
EKF_1d_interp_joint(d, npart=100, sigma_pars, 
                    alpha0_pars=list(mu0=c(5, 9), V0=c(0.25, 0.25)),
                    Errvar0=rep(list(5), 2), Errvar_df=c(20, 20), 
                    Particle_errvar0, Particle_err_df=20, delaysample=1,
                    dirichlet_init=c(10,3,3,8), maxStep=NULL, 
                    state_favor=c(1,1), nstates=2,
                    lowvarsample=FALSE, time_radius=60*30, spat_radius=300,
                    min_num_neibs=10, interact=TRUE, 
                    interact_pars=list(mu0=0, precision0=2,
                    known_precision=2),
                    neff_sample=1, time_dep_trans=FALSE, 
                    time_dep_trans_init=dirichlet_init, smoothing=FALSE, 
                    fix_smoothed_behaviors=TRUE, smooth_parameters=TRUE,
                    reg_dt=120, max_int_wo_obs=NULL,
                    resamp_full_hist=TRUE, compare_with_known=FALSE,
                    known_trans_prob=NULL, known_foraging_prob=NULL,
                    known_regular_step_ds=NULL, update_eachstep=FALSE,
                    update_params_for_obs_only=FALSE,
                    output_plot=TRUE, loc_pred_plot_conf=0.5,
                    output_dir=getwd(), pdf_prefix="EKF_1D", verbose=3)

					
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{
Dataset of observations, with required variable columns: tag, X, velocity, date_as_sec, time_to_next, state.guess2, prev.guess2.
}
  \item{npart}{
Number of particles to be used in simulation.
}
  \item{sigma_pars}{
Vector of inverse-gamma parameters for sigma^2 (logV variance).  Two elements for each state.  The inverse gamma parameters are specified in pairs.
}
 \item{alpha0_pars}{
List of initial values of mean velocity (mu) and degrees of freedom (V), one for each behavioral state.
}
  \item{Errvar0}{
List of prior 1x1 covariance matrices for predicting y from x, one for each behavioral state.
}
  \item{Errvar_df}{
Vector of degrees of freedom of \code{Errvar0} covariance matrices.
}
  \item{Particle_errvar0}{
Prior 1x1 covariance matrix for predicting x_t from x_[t-1].
}
  \item{Particle_err_df}{
Degree of freedom of \code{Particle_errvar0}.
}
  \item{dirichlet_init}{
List of 4-element vectors specifying Dirichlet parameters for transition matrices for each region.  Will be replicated to equal number of regions.
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
  \item{lowvarsample}{
Logical.  If TRUE, use low-variance sampling when resampling particles to ensure particles are resampled proportionately to weight.  Otherwise there is some sampling variance when drawing random samples.
The setting applies to smoothing as well.
}
  \item{time_radius}{
Time in seconds to consider for spatial neighbors (1-D interval on either side).
}
  \item{spat_radius}{
Radius (half of interval length) in meters of spatial neighborhood.
}
  \item{min_num_neibs}{
Minimum number of time and spatial radius observations that need to exist to constitute a neighborhood.
}
  \item{interact}{
Logical.  If TRUE, simulate interaction parameters of neighborhood.   If \code{nstates}=1, automatically set to FALSE.
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
Logical.  If TRUE, when performing smoothing, keep behavior modes fixed for each particle history from what was originally predicted duruing filtering, 
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
If \code{nstates}=2, a matrix of row 2 where each column is the behavior transition probabilities between each opposing behavior, in each region.  
}
  \item{known_foraging_prob}{
If \code{nstates}=2, a matrix of the foraging probabilities for each region. 
}
  \item{known_regular_step_ds}{
If \code{compare_with_known}=TRUE, the dataset of the original regular-step trajectories. Note: this dataset needs to have 
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
If TRUE, then \code{update_eachstep=FALSE}, meaning that parameter updates will be done only for (and at) the steps that represent observations.
If FALSE, then \code{update_eachstep} can be either TRUE or FALSE, but simulated steps will be used to update, but 
\code{update_eachstep} controls the timing of the the update; if TRUE, it happens one step at a time, and if FALSE, a batch update is done
at observations for the set of simulated steps. 
}
  \item{output_plot}{
Logical.  If TRUE, a set of diagnostic plots will be printed to a file in \code{output_dir}.  Otherwise, it 
will be output to the plotting console.
}
  \item{loc_pred_plot_conf}{
Numeric.  Confidence level of confidence interval for location prediction error to plot in step-wise diagnostics.
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
\item{d}{Input dataset as \code{data.frame}}
\item{N}{Number of regular steps of length \code{reg_dt} needed to cover the observed range of time.}
\item{t_reg}{Vector of times of regular step \code{reg_dt}.}
\item{nsharks}{Number of sharks in output data.}
\item{shark_names}{Names of sharks in output data.}
\item{shark_valid_steps}{List of regular-step intervals that each shark has simulated particle movement for.}
\item{shark_intervals}{List of regular-step intervals that each shark has observations for.}
\item{first_intervals}{List of regular-step intervals that begin each shark's segments of simulated particle movement.
If observed gaps are larger than \code{max_int_wo_obs}, the shark's trajectory will be simulated as two or more separate segments.}
\item{included_intervals}{Unique list of regular-step intervals with simulated movement for any shark.}
\item{mu}{Array of estimated values of mean log-velocity for normal inverse-gamma conjugate distribution}
\item{XY_errvar}{Estimated matrix and degrees of freedom of estimated location error covariance, for each behavior.}
\item{sigma_pars}{Posterior inverse gamma distribution parameters for the velocity (or, for 2-D, log-velocity) variance.}
\item{Xpart_history}{Overall history of estimated movement values.}
\item{param_draws}{Posterior sampled values of mean of velocity (or, for 2-D, log-velocity).}
\item{variance_draws}{Posterior sampled value of variance of velocity (or, for 2-D, log-velocity).}
\item{eff_size_hist}{History of effective sample sizes in simulations.}
\item{agree_table}{Table of observed agreement between particle predictions of behavior and those observed, overall and by behavior, if \code{nstates > 1}.}
\item{states}{Observed vector of behavioral states.}
\item{state_counts}{Array of total number of simulated regular-step intervals in each behavioral state.}
\item{lambda_matrix}{History of particle predicted values of lambda, the behavior variable.}
\item{lambda_matrix_beforesamp}{Same as \code{lambda_matrix}, except the history before each time has not been resampled according to the particle
resampling weights.  For \code{lambda_matrix}, \code{Xpart_history}, and other estimated outputs, the entire particle history is resampled.}
\item{resample_history}{Fraction of unique particles that are resampled at each regular step over the history.}
\item{transition_mat}{Estimated transition probability matrix parameters for Dirichlet distribution.  If \code{nstates==1}, is meaningless.}
\item{error_beforesamp}{For each regular step \code{i} with an observation, the quantiles of summed prediction errors before each round of resampling, across history.}
\item{error_beforesamp_quantiles}{Quantiles of \code{error_beforesamp_allpart} across history.}
\item{error_final_allpart}{For each regular step \code{i} with an observation, the sum of prediction errors for any observations in that interval (final after resampling).}
\item{error_final_quantiles}{Quantiles of \code{error_final_allpart} across history.}
\item{error_true_allpart}{If \code{compare_with_known == TRUE}, for each regular step \code{i}, the sum of prediction errors for any true locations in that interval.}
\item{error_true_quantiles}{If \code{compare_with_known == TRUE}, quantiles of \code{error_final_true_allpart} across history.}

The following inputted parameters are returned :
\item{npart}{}
\item{nstates}{}
\item{state_favor}{}
\item{known_regular_step_ds}{}
\item{known_foraging_prob}{}
\item{neff_sample}{}
\item{resamp_full_hist}{}
\item{time_dep_trans}{}
\item{interact}{}
\item{spat_radius}{}
\item{time_radius}{}
\item{lowvarsample}{}
\item{update_eachstep}{}
\item{update_params_for_obs_only}{}

The following are returned if \code{nstates > 1}:
\item{trans_counts}{Array of total number of simulated regular-step intervals with transitions between each possible pair of behaviors.}
\item{trans_mean}{Posterior estimates of mean behavior switching probabilities from \code{region_trans_draws}.}
\item{region_foraging_draws}{Posterior estimate of probability of foraging (lambda=0) from behavior switching probabilities.}
\item{region_trans_draws}{Posterior draws of behavior switching probabilities from \code{transition_mat}. For 2-D, this is separately by region, if
there are multiple regions.}

In addition, the following are returned if \code{compare_with_known = TRUE}:
\item{error_final_true_allpart}{Errors from estimating true locations from particle locations (at the same times).}
\item{error_final_true_quantiles}{Quantiles of \code{error_final_true_allpart} across history.}
\item{euclidean_estimate_true_from_obs}{Estimates of true locations by Euclidean interpolation from observations}
\item{error_euclidean_estimate_true_from_obs}{Euclidean error from \code{euclidean_estimate_true_from_obs} compared to true locations from \code{known_regular_step_ds}.}

In addition, the following are returned if \code{interact = TRUE}:
\item{spatial_interact_pars}{Estimated parameters for sharks' tendency to be influenced by other neighboring sharks in determining behavior.}
\item{interact_mu_draws}{Posterior sampled values of interaction mu parameter.}
\item{interact_intensity_draw}{Posterior sampled values of interaction tendency multiplier, at different proportions of neighboring sharks with second behavior type.}
\item{spatial_interact_mu_history}{History of simulated values of interaction mu.}
\item{spatial_interact_intensity_history}{History of simulated values of interaction tendency multiplier.}

The following are returned if \code{smoothing = TRUE}:
\item{Xpart_history_smoothed}{Resampled values of \code{Xpart_history} by reverse smoothing resampling (see Carvalho et al).}
\item{error_smoothed_allpart}{For each regular step \code{i}, the sum of prediction errors for smoothed particles for any observations in that interval.}
\item{error_smoothed_quantiles}{Quantiles of \code{error_smoothed_allpart} across history.}

In addition, if \code{smooth_parameters = TRUE}:
\item{param_draws_smoothed}{Posterior sampled values of mean of velocity (or, for 2-D, log-velocity) after resampling by smoothing.}
\item{variance_draws_smoothed}{Posterior sampled values of variance of velocity (or, for 2-D, log-velocity) after resampling by smoothing.}
\item{transition_mat_smoothed}{Estimated transition probability matrix parameters for Dirichlet distribution after resampling by smoothing.}

In addition, if \code{smooth_parameters = TRUE} and \code{interact = TRUE}:
\item{spatial_interact_pars_smoothed}{Estimated parameters for sharks' tendency to be influenced by other neighboring sharks in determining behavior, after resampling by smoothing.}
\item{interact_mu_draws_smoothed}{Posterior sampled values of interaction mu parameter, after resampling by smoothing.}
\item{interact_intensity_draw_smoothed}{Posterior sampled values of interaction tendency multiplier, at different proportions of neighboring sharks with second behavior type, after resampling by smoothing.}

In addition to smoothing, if \code{compare_with_known = TRUE}:
\item{error_smoothed_true_allpart}{For each regular step \code{i}, the sum of prediction errors for smoothed particles for any observations in that interval.}
\item{error_smoothed_true_quantiles}{Quantiles of \code{error_smoothed_true_allpart} across history.}


In addition to smoothing, if \code{smoothing = TRUE} but \code{fix_smoothed_behaviors = FALSE} (smoothed behaviors allowed to change from filtering):
\item{mu_smoothed}{Corresponding version of \code{mu} after resampling by smoothing.}
\item{sigma_pars_smoothed}{Corresponding version of \code{sigma_pars} after resampling by smoothing.}
\item{agree_table_smoothed}{Corresponding version of \code{agree_table} for smoothed states \code{lambda_matrix_smoothed}.}



	
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
