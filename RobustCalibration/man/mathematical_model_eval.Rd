\name{mathematical_model_eval}
\alias{mathematical_model_eval}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Evaluation of the mathmatical model at given observed inputs and calibration parameters. }
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function evaluates the mathematical model at given observed inputs and calibration parameters.
}
\usage{
mathematical_model_eval(input,theta,simul_type, emulator,
emulator_type,loc_index_emulator,math_model)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
%%     ~~Describe \code{param} here~~
a matrix of the observed inputs.
}
  \item{theta}{
%%     ~~Describe \code{param} here~~
a vector of calibration parameters.
}
  \item{simul_type}{
  tpye of math model. If the simul_type is 0, it means we use the RobustGaSP R package to emulate the math model. If the simul_type is 1, it means the function of the math model is given by the user. When simul_type is 2 or 3, the mathematical model is the geophyiscal model for Kilauea Volcano.  If the simul_type is 2, it means it is for the ascending mode InSAR data; if the simul_type is 3, it means it is for the descending mode InSAR data.
  }
  \item{emulator}{
   an S4 class of rgasp model from the RobustGaSP R Package.
}
  \item{emulator_type}{
   a character to specify the type of emulator.  'rgasp' is for computer models with scalar-valued output and 'ppgasp'  for computer models with vectorized output.
}


  \item{loc_index_emulator}{
   a vector of the location index from the ppgasp emulator to output. Only useful for vectorized output computer model emulated by the ppgasp emulator.
}


  \item{math_model}{
  a function for the math model to be calibrated.
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
a vector of outputs from the math model or its emulator at given observed inputs and calibration parameters. }
\references{

A. O'Hagan and M. C. Kennedy (2001), \emph{Bayesian calibration of computer models}, \emph{Journal of the Royal Statistical Society: Series B (Statistical Methodology}, \bold{63}, 425-464.

K. R. Anderson and M. P. Poland (2016), \emph{Bayesian estimation of magma supply, storage, and eroption rates using a multiphysical volcano model: Kilauea volcano, 2000-2012.}. \emph{Eath and Planetary Science Letters}, \bold{447}, 161-171.

K. R. Anderson and M. P. Poland (2017), \emph{Abundant carbon in the mantle beneath Hawaii}. \emph{Nature Geoscience}, \bold{10}, 704-708.


}
\author{
%%  ~~who you are~~
\packageAuthor{RobustCalibration}

Maintainer: \packageMaintainer{RobustCalibration}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
% \code{\link{log_marginal_lik}}.
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")

\keyword{internal}