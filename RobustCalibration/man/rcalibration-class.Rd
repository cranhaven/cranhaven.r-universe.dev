\name{rcalibration-class}
\docType{class}
\alias{rcalibration-class}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Robust Calibration class }
\description{S4 class for Robust rcalibration with or without the specification of the discrepancy model.}

\section{Objects from the Class}{
Objects of this class are created and initialized with the function \code{\link{rcalibration}} that computes the calculations needed for setting up the calibration and prediction.}

\section{Slots}{
	 \describe{
    \item{\code{p_x}:}{Object of class \code{integer}. The dimension of the observed inputs.}
    \item{\code{p_theta}:}{Object of class \code{integer}. The  calibration parameters.}
    \item{\code{num_obs}:}{Object of class \code{integer}. The number of experimental observations.}
    \item{\code{input}:}{Object of class \code{matrix} with dimension n x p_x. The design of experiments.}
    \item{\code{output}:}{Object of class \code{vector} with dimension n x 1. The vector of the experimental observations.}
    \item{\code{X}:}{Object of class \code{matrix} of with dimension n x q. The mean/trend discrepancy basis function.}
        \item{\code{have_trend}:}{Object of class \code{bool} to specify whether the mean/trend discrepancy is zero. "TRUE" means it has zero mean discrepancy and "FALSE"" means the mean discrepancy is not zero.}

    \item{\code{q}:}{Object of class \code{integer}. The number of basis functions of the mean/trend discrepancy.}
    \item{\code{R0}:}{Object of class \code{list} of matrices where the j-th matrix is an absolute difference matrix of the j-th input vector.}
    \item{\code{kernel_type}:}{A \code{character} to specify the type of kernel to use.}
    \item{\code{alpha}:}{Object of class \code{vector}. Each element is the parameter for the roughness for each input coordinate in the kernel.}
    \item{\code{theta_range}:}{A \code{matrix} for the range of the calibration parameters.}
    \item{\code{lambda_z}:}{Object of class \code{vector} about how close the math model to the reality in squared distance when the S-GaSP model is used for modeling the discrepancy.}
    \item{\code{S}:}{Object of class \code{integer} about how many posterior samples to run.}
    \item{\code{S_0}:}{Object of class \code{integer} about the number of burn-in samples.}
    \item{\code{prior_par}:}{Object of class \code{vector} about prior parameters.}
    \item{\code{output_weights}:}{Object of class \code{vector} about the weights of the experimental data.}
    \item{\code{sd_proposal}:}{Object of class \code{vector} about the standard deviation of the proposal distribution.}
    \item{\code{discrepancy_type}:}{Object of class \code{character} about the discrepancy. If it is 'no-discrepancy', it means no discrepancy function. If it is 'GaSP', it means the GaSP model for the discrepancy function. If it is 'S-GaSP', it means the S-GaSP model for the discrepancy function.}

    \item{\code{simul_type}:}{Object of class \code{integer} about the math model/simulator. If the simul_type is 0, it means we use RobustGaSP R package to build an emulator for emulation. If the simul_type is 1, it means the function of the math model is given by the user. When simul_type is 2 or 3, the mathematical model is the geophyiscal model for Kilauea Volcano.  If the simul_type is 2, it means it is for the ascending mode InSAR data; if the simul_type is 3, it means it is for the descending mode InSAR data.}
        \item{\code{emulator_rgasp}:}{An S4 class of \code{rgasp} from the RobustGaSP package.}
                \item{\code{emulator_ppgasp}:}{An S4 class of \code{ppgasp} from the RobustGaSP package.}
    \item{\code{post_sample}:}{Object of class \code{matrix} for the posterior samples after burn-in.}
    \item{\code{post_value}:}{Object of class \code{vector} for the posterior values after burn-in.}
    \item{\code{accept_S}:}{Object of class \code{vector} for the number of proposed samples of the calibation parameters are accepted in MCMC. The first value is the number of proposed calibration parameters  are accepted in MCMC. The second value is  the number of proposed range and nugget parameters  are accepted.}
        \item{\code{count_boundary}:}{Object of class \code{vector} for the number of proposed samples of the calibation parameters are outside the range and they are rejected directly.}
               \item{\code{have_replicates}:}{Object of class \code{bool} for having repeated experiments (replicates) or not.}
           \item{\code{num_replicates}:}{Object of class \code{vector} for the number of replicates at each observable input.}
       \item{\code{thinning}:}{Object of class \code{integer} for the ratio between the number of posterior samples and the number of  samples to be recorded.}
       \item{\code{S_2_f}:}{Object of class \code{numeric} for the variance of the field observations.}
       \item{\code{num_obs_all}:}{Object of class \code{integer} for the total number of field observations.}
       \item{\code{method}:}{Object of class \code{character} for posterior sampling or maximum likelihood estimation.}
       \item{\code{initial_values}:}{Object of class \code{matrix} for initial starts of kernel parameters in maximum likelihood estimation.}
      \item{\code{param_est}:}{Object of class \code{vector} for estimated range and nugget parameter in parameter estimation.}
            \item{\code{opt_value}:}{Object of class \code{numeric} for optimized likelihood or loss function.}
            \item{\code{emulator_type}:}{Object of class \code{character} for the type of emulator. 'rgasp' means scalar-valued emulator and 'ppgasp' means vectorized emulator.}
            \item{\code{loc_index_emulator}:}{Object of class \code{vector} for location index to output in the ppgasp emulator  for computer models with vectorized output.}
            



    }
  }


\section{Methods}{
  \describe{
    \item{show}{Prints the main slots of the object. }
    \item{predict}{See \code{\link[=predict.rcalibration]{predict}}.}
    \item{predict_separable_2dim}{See \code{\link[=predict_separable_2dim]{predict_separable_2dim}}.}

	 }
}


\references{

A. O'Hagan and M. C. Kennedy (2001), \emph{Bayesian calibration of computer models}, \emph{Journal of the Royal Statistical Society: Series B (Statistical Methodology}, \bold{63}, 425-464.


M. Gu (2016), \emph{Robust Uncertainty Quantification and Scalable Computation for Computer Models with Massive Output}, Ph.D. thesis., Duke University.

M. Gu and L. Wang (2017) \emph{Scaled Gaussian Stochastic Process for Computer Model Calibration and Prediction}. arXiv preprint arXiv:1707.08215.

M. Gu (2018) \emph{Jointly Robust Prior for Gaussian Stochastic Process in Emulation, Calibration and Variable Selection
}. arXiv preprint arXiv:1804.09329.

}

\author{
\packageAuthor{RobustCalibration}

Maintainer: \packageMaintainer{RobustCalibration}
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{rcalibration}} for more details about how to create a \code{rcalibration} object.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
