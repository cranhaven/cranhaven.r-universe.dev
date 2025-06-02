\name{rcalibration_MS-class}
\docType{class}
\alias{rcalibration_MS-class}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Robust Calibration for multiple sources class }
\description{S4 class for multiple sources Robust rcalibration with or without the specification of the discrepancy model.}

\section{Objects from the Class}{
Objects of this class are created and initialized with the function \code{\link{rcalibration_MS}} that computes the prediction after calibrating the mathematical models from multiple sources.}

%
\section{Slots}{
	 \describe{
	  \item{\code{num_sources}:}{Object of class \code{integer}. The number of sources.}

    \item{\code{p_x}:}{Object of class \code{vector}. Each element is the dimension of the observed inputs in each source.}
    \item{\code{p_theta}:}{Object of class \code{integer}. The number of calibration parameters.}
    \item{\code{num_obs}:}{Object of class \code{vector}.Each element is the number of experimental observations of each source.}
    \item{\code{index_theta}:}{Object of class \code{list}. The each element is a \code{vector} of the index of calibration parameters (theta) contained in each source. }
    \item{\code{input}:}{Object of class \code{list}. Each element is a \code{matrix} of the design of experiments in each source with dimension \code{n_i x p_{x,i}}, for i=1,...,num_sources.}
    \item{\code{output}:}{Object of class \code{list}. Each element is a \code{vector} of the experimental observations in each source with dimension n_i x 1, for i=1,...,num_sources.}
    \item{\code{X}:}{Object of class \code{list}. Each element is a \code{matrix} of the mean/trend discrepancy basis function in each source with dimension n_i x q_i,  for i=1,...,num_sources.}
        \item{\code{have_trend}:}{Object of class \code{vector}. Each element is a  \code{bool} to specify whether the mean/trend discrepancy is zero in each source. "TRUE" means it has zero mean discrepancy and "FALSE"" means the mean discrepancy is not zero.}

    \item{\code{q}:}{Object of class \code{vector}.  Each element is \code{integer} of the number of basis functions of the mean/trend discrepancy in each source. }
    \item{\code{R0}:}{Object of class \code{list}. Each element is a list of matrices where the j-th matrix is an absolute difference matrix of the j-th input vector in each source.}
    \item{\code{kernel_type}:}{Object of class \code{vector}. Each element is a \code{character} to specify the type of kernel to use in each source.}
    \item{\code{alpha}:}{Object of class \code{list}. Each element is a \code{vector} of parameters for the roughness parameters in the kernel in each source.}
    \item{\code{theta_range}:}{A \code{matrix} for the range of the calibration parameters.}
    \item{\code{lambda_z}:}{Object of class \code{vector}.  Each element is a \code{numeric} value about how close the math model to the reality in squared distance when the S-GaSP model is used for modeling the discrepancy in each source.}
    \item{\code{S}:}{Object of class \code{integer} about how many posterior samples to run.}
    \item{\code{S_0}:}{Object of class \code{integer} about the number of burn-in samples.}
    \item{\code{prior_par}:}{Object of class \code{list}. Each element is a \code{vector} about prior parameters.}
    \item{\code{output_weights}:}{Object of class \code{list}. Each element is a \code{vector} about the weights of the experimental data.}
    \item{\code{sd_proposal_theta}:}{Object of class \code{vector} about the standard deviation of the proposal distribution for the calibration parameters.}
        \item{\code{sd_proposal_cov_par}:}{Object of class \code{list}. Each element is a \code{vector} about the standard deviation of the proposal distribution for the calibration parameters in each source.}

    \item{\code{discrepancy_type}:}{Object of class \code{vector}.  Each element is a \code{character} about the type of the discrepancy in each source. If it is 'no-discrepancy', it means no discrepancy function. If it is 'GaSP', it means the GaSP model for the discrepancy function. If it is 'S-GaSP', it means the S-GaSP model for the discrepancy function.}

    \item{\code{simul_type}:}{Object of class \code{vector}. Each element is an \code{integer} about the math model/simulator. If the simul_type is 0, it means we use RobustGaSP R package to build an emulator for emulation. If the simul_type is 1, it means the function of the math model is given by the user. When simul_type is 2 or 3, the mathematical model is the geophyiscal model for Kilauea Volcano.  If the simul_type is 2, it means it is for the ascending mode InSAR data; if the simul_type is 3, it means it is for the descending mode InSAR data.}
    \item{\code{emulator_rgasp}:}{Object of class \code{list}.  Each element is an S4 class of \code{rgasp} from the RobustGaSP package in each source.}
        \item{\code{emulator_ppgasp}:}{Object of class \code{list}.  Each element is an S4 class of \code{ppgasp} from the RobustGaSP package in each source.}
        \item{\code{post_theta}:}{Object of class \code{matrix} for the posterior samples of the calibration parameters after burn-in.}
    \item{\code{post_individual_par}:}{Object of class \code{list}. Each element is a \code{matrix} for the posterior samples after burn-in in each source.}
    \item{\code{post_value}:}{Object of class \code{vector} for the posterior values after burn-in.}
        \item{\code{accept_S_theta}:}{Object of class \code{numerical} for the number of proposed samples of the calibration parameters are accepted in MCMC.}
    \item{\code{accept_S_beta}:}{Object of class \code{vector} for the number of proposed samples of the range and nugget parameters in each source are accepted in MCMC.}
        \item{\code{count_boundary}:}{Object of class \code{vector} for the number of proposed samples of the calibation parameters are outside the range and they are rejected directly.}
       \item{\code{have_measurement_bias_recorded}:}{Object of class \code{bool} for whether measurement bias will be recorded or not.}
       \item{\code{measurement_bias}:}{Object of class \code{bool} for whether measurement bias exists or not.}
              \item{\code{post_delta}:}{Object of class \code{matrix} of samples of model discrepancy.}

       
       \item{\code{post_measurement_bias}:}{Object of class \code{list} of samples of measurement_bias if measurement bias is chosen to be recorded.}
       \item{\code{thinning}:}{Object of class \code{integer} for the ratio between the number of posterior samples and the number of  samples to be recorded.}
            \item{\code{emulator_type}:}{Object of class \code{vector} for the type of emulator for each source of data. 'rgasp' means scalar-valued emulator and 'ppgasp' means vectorized emulator.}
            \item{\code{loc_index_emulator}:}{Object of class \code{list} for location index to output in ghe ppgasp emulator  for computer models with vectorized output.}


    }
  }


\section{Methods}{
  \describe{
%    \item{show}{Prints the main slots of the object. }
    \item{predict_MS}{See \code{\link[=predict_MS.rcalibration_MS]{predict_MS}}.}
%    \item{predict_discrepancy_separable_2dim}{See \code{\link[=predict_discrepancy_separable_2dim]{predict_discrepancy_separable_2dim}}.}
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

\seealso{ \code{\link{rcalibration_MS}} for more details about how to create a \code{rcalibration_MS} object.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
