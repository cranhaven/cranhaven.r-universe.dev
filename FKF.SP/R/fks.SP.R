#' Fast Kalman Smoother through Sequential Processing
#'
#'@description
#'\loadmathjax
#'The Kalman smoother is a backwards algorithm that is run after the Kalman filter
#'that allows the user to refine estimates of previous states to produce "smoothed" estimates of state variables.
#'This function performs the "Kalman smoother" algorithm using sequential processing, an approach that can substantially improve
#'processing time over the traditional Kalman filtering/smoothing algorithms. The primary application of Kalman smoothing is in
#'conjunction with expectation-maximization to estimate the parameters of a state space model. This function is called after running \code{\link[FKF.SP]{fkf.SP}}.
#'\code{fks.SP} wraps the C-function \code{fks_SP} which relies upon the linear algebra subroutines of BLAS (Basic Linear Algebra Subprograms).
#'
#'@param FKF.SP_obj  An S3-object of class "fkf.SP", returned by \code{\link[FKF.SP]{fkf.SP}} when using the argument \code{verbose = TRUE}.
#'
#'@details
#'
#'\code{fks.SP} is typically called after the \code{\link[FKF.SP]{fkf.SP}} function to calculate "smoothed" estimates of state variables and their corresponding variances. Smoothed estimates are used
#'when utilizing expectation-maximization (EM) to efficiently estimate the parameters of a state space model.
#'
#'\bold{Sequential Processing Kalman smoother solution}:
#'
#'The \code{fks.SP} function uses the solution to the Kalman smoother through sequential processing provided in the textbook of Durbin and Koopman (2001).
#'
#'Given a state space model has been filtered through the sequential processing Kalman filter algorithm described in \code{fkf.SP}, the smoother can be reformulated for the univariate series:
#'
#'\mjdeqn{y_t'=(y_{(1,1)},y_{(1,2)},\cdots,y_{(1,p_{1})},y_{(2,1)},\cdots,y_{(t,p_{t})})}{y_t'=(y_(1,1),y_(1,2),...y_(1,p_1),y_(2,1),...,y_(t,p_t))}
#'
#'The sequential processing Kalman smoother approach iterates backwards through both observations and time, i.e.: \mjeqn{i=p_{t}, \cdots, 1}{i=p[t], ... ,1} and \mjeqn{t=n,\cdots,1}{t=n, ... ,1},
#'where \mjeqn{p_{t}}{p_t} is the number of observations at time \mjeqn{t}{t} and \mjeqn{n}{n} is the total number of observations.
#'
#'The initialisations are:
#'
#'\mjdeqn{r_{(n,p_{n})} = 0}{r(n,p_n)=0}
#'\mjdeqn{N_{(n,p_{n})}=0}{N(n,p_n)=0}
#'
#'Then, \mjeqn{r}{r} and \mjeqn{N}{N} are recursively calculated through:
#'
#'\mjdeqn{L_{t,i} = I_{m} - K_{t,i} Z_{t,i}}{L(t,i) = I_m - K(t,i) Z(t,i)}
#'
#'\mjdeqn{r_{(t,i-1)} = Z_{t,i}' F_{t,i}^{-1} v_{t,i} + L_{t,i}' r_{t,i}}{r(t,i-1) = Z(t,i)' F(t,i)^-1 v(t,i) + L(t,i)' r(t,i)}
#'
#'\mjdeqn{N_{t,i-1} = Z_{t,i}' F_{t,i}^{-1} Z_{t,i} + L_{t,i}' N_{t,i} L_{t,i}}{N(t,i-1) = Z(t,i)' F(t,i)^-1 Z(t,i) + L(t,i)' N(t,i) L(t,i)}
#'
#'\mjdeqn{r_{t-1,p_{t}} = T_{t-1}' r_{t,0}}{r(t-1,p_t) = T(t-1)' r(t,0)}
#'
#'\mjdeqn{N_{t-1,p_{t}} = T_{t-1}' N_{t,0} T_{t-1}}{N(t-1,p_t) = T(t-1)' N(t,0) T(t-1)}
#'
#'for \mjeqn{i=p_{t},\cdots,1}{i=p_t, ..., 1} and \mjeqn{t=n,\cdots,1}{t=n, ..., 1}
#'
#'The equations for \mjeqn{r_{t-1},p_{t}}{r(t-1,p_t)} and \mjeqn{N_{t-1,p_{t}}}{N(t-1,p_t)} do not apply for \mjeqn{t=1}{t=1}
#'
#'Under this formulation, the values for \mjeqn{r_{t,0}}{r(t,0)} and \mjeqn{N_{t,0}}{N(t,0)} are the same as the values for the smoothing quantities of \mjeqn{r_{t-1}}{r(t-1)} and
#'\mjeqn{N_{t-1}}{N(t-1)} of the standard smoothing equations, respectively.
#'
#'The standard smoothing equations for \mjeqn{\hat{a_{t}}}{ahat(t)} and \mjeqn{V_t}{V(t)} are used:
#'
#'\mjdeqn{\hat{a_{t}} = a_{t} + P_{t} r_{t-1}}{ahat(t) = a(t) + P(t) r(t-1)}
#'
#'\mjdeqn{V_t = P_t - P_t N_{t-1} P_t}{V(t) = P(t) - P(t) N(t-1) P(t)}
#'
#'Where:
#'
#'\mjdeqn{a_{t}=a_{t,1}}{a(t) = a(t,1)}
#'
#'\mjdeqn{P_{t} = P_{t,1}}{P(t) = P(t,1)}
#'
#'In the equations above, \mjeqn{r_{t,i}}{r(t,i)} is an \mjeqn{m \times 1}{m X 1} vector, \mjeqn{I_{m}}{I_m} is an \mjeqn{m \times m}{m X m} identity matrix,
#'\mjeqn{K_{t,i}}{K(t,i)} is an \mjeqn{m \times 1}{m X 1} column vector, \mjeqn{Z_{t,i}}{Z(t,i)} is a \mjeqn{1 \times m}{1 X m} row vector, and both \mjeqn{F_{t,i}^{-1}}{F(t,i)^-1} and
#'\mjeqn{v_{t,i}}{v(t,i)} are scalars. The reduced dimensionality of many of the variables in this formulation compared to traditional Kalman smoothing can result in increased computational efficiency.
#'
#'Finally, in the formulation described above, \mjeqn{a_{t}}{a(t)} and \mjeqn{P_{t}}{P(t)} correspond to the values of \code{att} and \code{ptt} returned from the \code{fkf.SP} function, respectively.
#'
#'
#'@return
#'An S3-object of class \code{fks.SP}, which is a list with the following elements:
#'
#' \tabular{rl}{
#'   \code{ahatt} \tab  A \eqn{m \times n}{m * n}-matrix containing the
#'   smoothed state variables, i.e. \code{ahatt[,t]} = \mjeqn{a_{t|n}}{a(t|n)}\cr
#'   \code{Vt} \tab  A \eqn{m \times m \times n}{m * m * n}-array
#'   containing the variances of \code{ahatt}, i.e. \code{Vt[,,t]} = \mjeqn{P_{t|n}}{P(t|n)}
#'}
#'@references
#'
#'Aspinall, T. W., Harris, G., Gepp, A., Kelly, S., Southam, C., and Vanstone, B. (2022). \emph{The Estimation of Commodity Pricing Models with Applications in Capital Investments}. \href{https://research.bond.edu.au/en/studentTheses/the-estimation-of-commodity-pricing-models-with-applications-in-c}{Available Online}.
#'
#'
#'Durbin, James, and Siem Jan Koopman (2001). \emph{Time series analysis by state space methods.} Oxford university press.
#'
#'@examples
#'### Perform Kalman Filtering and Smoothing through sequential processing:
#'#Nile's annual flow:
#'yt <- Nile
#'
#'# Incomplete Nile Data - two NA's are present:
#'yt[c(3, 10)] <- NA
#'
## Set constant parameters:
#'dt <- ct <- matrix(0)
#'Zt <- Tt <- matrix(1)
#'a0 <- yt[1]   # Estimation of the first year flow
#'P0 <- matrix(100)       # Variance of 'a0'
#'
#'# Parameter estimation - maximum likelihood estimation:
#'# Unknown parameters initial estimates:
#'GGt <- HHt <- var(yt, na.rm = TRUE) * .5
#'HHt = matrix(HHt)
#'GGt = matrix(GGt)
#'yt = rbind(yt)
#'# Filter through the Kalman filter - sequential processing:
#'Nile_filtered <- fkf.SP(HHt = matrix(HHt), GGt = matrix(GGt), a0 = a0, P0 = P0, dt = dt, ct = ct,
#'                   Zt = Zt, Tt = Tt, yt = rbind(yt), verbose = TRUE)
#'# Smooth filtered values through the Kalman smoother - sequential processing:
#'Smoothed_Estimates <- fks.SP(Nile_filtered)
#'
#'@export
fks.SP <- function (FKF.SP_obj) {
  if (!class(FKF.SP_obj)[1] %in% c('fkf.SP', 'fkfs.SP')) stop('Input must be an object of class FKF.SP')
  return(.Call("fks_SP",
               FKF.SP_obj$Tt,
               FKF.SP_obj$Zt,
               FKF.SP_obj$yt,
               FKF.SP_obj$vt,
               FKF.SP_obj$Kt,
               FKF.SP_obj$Ftinv,
               FKF.SP_obj$att,
               FKF.SP_obj$Ptt, PACKAGE = "FKF.SP"))
}
