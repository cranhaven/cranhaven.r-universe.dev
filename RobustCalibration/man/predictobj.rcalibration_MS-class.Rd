\name{predictobj.rcalibration_MS-class}
\alias{predictobj.rcalibration_MS-class}
\alias{predictobj.rcalibration_MS}
\docType{class}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Predictive results for the Robust Calibration class }
\description{S4 class for prediction after Robust rcalibration for multiple sources.}

\section{Objects from the Class}{
Objects of this class are created and initialized with the function \code{\link{predict_MS}} that computes the prediction and the uncertainty quantification.}

\section{Slots}{
	 \describe{
    \item{\code{mean}:}{object of class \code{list}. Each element is a \code{vector} of the predictive mean at testing inputs combing the mathematical model and discrepancy function for each source.}
    \item{\code{math_model_mean}:}{object of class  \code{list}. Each element is a \code{vector} of the predictive mean at testing inputs using only the mathematical model (and the trend if specified).}
    \item{\code{math_model_mean_no_trend}:}{object of class  \code{list}. Each element is a \code{vector} of the predictive mean at testing inputs using only the mathematical model without the trend for each source.}
    \item{\code{interval}:}{object of class \code{list}. Each element is a \code{matrix} of  the upper and lower predictive credible interval. If interval_data is TRUE in the \code{\link{predict_MS}}, the experimental noise is included for computing the predictive credible interval. }

    }
  }




\references{

A. O'Hagan and M. C. Kennedy (2001), \emph{Bayesian calibration of computer models}, \emph{Journal of the Royal Statistical Society: Series B (Statistical Methodology}, \bold{63}, 425-464.


M. Gu (2016), \emph{Robust Uncertainty Quantification and Scalable Computation for Computer Models with Massive Output}, Ph.D. thesis., Duke University.

M. Gu and L. Wang (2017) \emph{Scaled Gaussian Stochastic Process for Computer Model Calibration and Prediction}. arXiv preprint arXiv:1707.08215.

}

\author{
\packageAuthor{RobustCalibration}

Maintainer: \packageMaintainer{RobustCalibration}
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{predict_MS}} for more details about how to do prediction for a \code{rcalibration_MS} object.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
