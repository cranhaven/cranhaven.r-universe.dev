\newcommand{\hr}{\cr___________________________________________________________\cr}
\name{solve}
\alias{solve}
\title{Solves an ode-system}
\usage{
  solve(
    ode,
    parameter,
    reltol,
    abstol,
    states,
    solvertype,
    own_error_fct,
    own_spline_fct,
    own_jac_fct,
    verbose
  )
}
\arguments{
  \item{ode}{the ode-system for which the parameter should be optimized.}
  \item{parameter}{a data.frame containing the parameters.}
  \item{reltol}{a number defining the relative tolerance used by the ode-solver. The default value is 1e-06}
  \item{abstol}{a vector containing the absolute tolerance(s) for each state used by the ode-solver. The default value is 1e-08}
  \item{states}{a data.frame containing the predetermined course of the states. \cr The data.frame is used to extract the initial values of the states. \cr
    Furthermore, the ode-solver returns \emph{in silico} values of the states at the timepoints which has to be defined in the first column}
  \item{solvertype}{a string defining the type of solver which should be used "bdf" or "adams" are the possible values. The default value is "bdf". \cr
    "bdf" is an abbreviation for Backward Differentiation Formulas. "adams" is an abbreviation for the Adams-Moulton algorithm}
  \item{own_error_fct}{An optional function to calculate the error between \emph{in silico} value and the specified value in the data.frame states. The default error calculation is specified in the section notes.}
  \item{own_spline_fct}{An optional function to interpolate the values for variable parameters. The default function is a CatmullRome spline interpolation.}
  \item{own_jac_fct}{An optional function which returns the jacobian function. Furthermore it is possible to calculate the jacobian using the R package dfdr. If this is desired "dfdr" has to be passed as argument. If nothing is passed the jacobian matrix is numerically calculated.}
  \item{verbose}{A logical value defining whether the output during compilation should be shown or not. The default value is FALSE}
}
\description{
  Solves an ode equation and calculate an error based on the difference on a user-defined state-data.frame.
}
\details{

  \strong{The ode system:}
  \hr{}
  The ode system is an R function which accepts four arguments and returns one.
  \enumerate{
    \item the first argument is \strong{t} which defines the (time-) point of then independent variable at which the ode-system is evaluated.
    \item the second argument is a vector called \strong{y} which defines the current states at timepoint \strong{t}
    \item the third argument is a vector called \strong{ydot} which should be filled with the derivative (left hand side) of the ode-system. It has already the correct length! \strong{This vector has to be returned.}
    \item the last argument is called \strong{parameter} and is a vector containing the current parameter-set which is tested by the optimization algorithm. \cr
    \strong{If the parameters can change over time. The already interpolated value is passed to the ode-system.} \cr
  }

  \verb{
    # theoretical Example: The parameter 'a' can change over time whereas 'b' is constant over time.
    parameter_set <- data.frame(
      time = c(0, 10, 20, 30, 40),
      a = c(1, 2, 3, 4, 5),
      b = c(1, NA, NA, NA, NA, NA))
    t <- 5
    # Interpolation would result in 1.5 for parameter 'a'
    parameter <- c(1.5, 1) # 'a', 'b'
    y <- 1
    ydot <- vector(length(1))
    ode(t, y, ydot, parameter)
  }

  \itemize{
    \item The function returns \emph{ydot}. It is only necessary to fill the vector \strong{ydot}. Check the package \emph{ast2ast} for more details how this works.
    \item The R function is translated to a C++ function using the package \emph{ast2ast}, see also \href{https://CRAN.R-project.org/package=ast2ast}{ast2ast on CRAN} and \code{\link[ast2ast:translate]{ast2ast::translate()}}. Therefore, if you are calling 'solve' for the first time the function has to be compiled. This can require a bit of time.
  }
  \strong{The parameters:}
  \hr{}
  The lower and upper boundaries are defined as a data.frame that contains 'time' as the first column. \cr
  The subsequent columns contain the information of the parameter. \cr
  \cr
  \verb{
    # Here some examples
    # all parameters are constant over the entire integration_time
    example1 <- data.frame(
      time = 0,
      a = 0.4,
      b = 1.1,
      c = 0.1,
      d = 0.4)

    # The parameter a, b, and c are constant whereas the parameter d can change over time
    example2 <- data.frame(
      time = c(0, 5, 10, 15),
      a = c(0.4, NA, NA, NA),
      b = c(1.1, NA, NA, NA),
      c = c(0.1, NA, NA, NA),
      d = c(0.4, 0.5, 0.3, 0.4))

    # The parameter a, b are constant
    # whereas parameter c and d can change over time.
    # However, d is not known for all points of c
    example3 <- data.frame(
      time = c(0, 5, 10, 15, 20, 25),
      a = c(1.1, NA, NA, NA, NA, NA),
      b = c(0.1, NA, NA, NA, NA, NA),
      c = c(0.2, 0.2, 0, 0, 0, 0),
      d = c(0, 0.1, 0.2, 1, NA, NA))
  }
  \strong{The states data.frame: }
  \hr{}
  The states are defined as a data.frame that contains the 'time' as the first column.
  The subsequent columns are the individual states. \cr
  \cr
  \verb{
    # Here some examples
    # Only the initial values are defined.
    example1 <- data.frame(
      time = seq(0, 100, 0.5),
      prey = c(10, rep(NA, 200)),
      predator = c(10, rep(NA, 200)))

    # All values are defined at each timepoint
    example2 <- data.frame(
      time =  seq(0, 100, 0.5),
      prey =  c(10, runif(200)),
      predator =  c(10, runif(200)) )

    # Only the values for prey are known and are used during optimization
    example3 <- data.frame(
      time =  seq(0, 100, 0.5),
      prey =  c(10, runif(200)),
      predator =  c(10, rep(NA, 200)) )

  }

  \strong{solvertype: }
  \hr{}
  For solving the ode system the SUNDIALS Software is used check the \href{https://computing.llnl.gov/projects/sundials}{Sundials homepage} for more informations.
  The solver-type which is used during optimization: “bdf“, “adams“. bdf is an abbreviation for Backward Differentiation Formulas and adams means Adams-Moulton.
  All solvers are used in the NORMAL-Step method in a for-loop using the time-points defined in the first column of the 'states' data.frame.
  The bdf solver use the SUNLinSol_Dense as linear solver.

  \strong{own_error_fct: }
\hr{}
The error function calculates the error at one of the possible time-points. Moreover, the function expects three numerical scalars as arguments. The first one is the number of data-points at which the error is calculated. The second argument describes the \emph{in silico} value at one specific time-point. The third argument is the input of the user at the specific time-point which should be matched. \cr
Here is one example shown using the sum of squares as an alternative error function.
\verb{
error_fct <- function(num_points, insilico, measured) {
  ret = (insilico - measured)^2
  return(ret/num_points)
}
}

\strong{own_spline_fct: }
\hr{}
The spline function is called, directly before the ode-system is evaluated. However, the function is only called for non-constant parameters. See example Nr.2 and Nr.3 parameter \emph{d} as described above. The results of the spline function is then stored in the vector \strong{parameter} which is passed to the ode-system. The function expects three arguments:
\itemize{
  \item The time-point at which the function is evaluated
  \item a vector containing the time-points for which parameters are defined
  \item a vector containing the parameters at the respective time-points
}
The function has to return a scalar value.See the example above for a linear interpolation:
\verb{
linear_interpolation <- function(t, time_vec, par_vec) {
  left = 0
  left_time = 0
  right = 0
  right_time = 0
  for(i in 1:length(time_vec)) {

    if(t == time_vec[i]) {
      return(par_vec[i])
    }

    if(t < time_vec[i]) {
      left = par_vec[i - 1]
      right = par_vec[i]
      left_time = time_vec[i - 1]
      right_time = time_vec[i]
      break
    }
  }

  timespan = right_time - left_time
  m = (right - left) / timespan
  ret = left + m*(t - left_time)
  return(ret)
}
}
Mentionable, is that it hasn't to be a interpolation function. See the example above:
\verb{
fct <- function(t, time_vec, par_vec) {
  ret = 0
  for(i in par_vec) {
    ret = ret + i
  }
  return(ret)
}
}


\strong{own_jac_fct: }
\hr{}
The jacobian function expects 5 arguments.
\enumerate{
  \item the first argument is \strong{t} which defines the (time-) point of then independent variable at which the ode-system is evaluated.
  \item the second argument is a vector called \strong{y} which defines the current states at timepoint \strong{t}
  \item the third argument is a vector called \strong{ydot} which should be filled with the derivative (left hand side) of the ode-system. It has already the correct length! Please do not return the vector.
  \item the foruth argument is a matrix called \strong{J} which should be filled with the respective derivatives of \strong{ydot}. The matrix has already the correct dimensions. \strong{This matrix has to be returned.}
  \item the last argument is called \strong{parameter} and is a vector containing the current parameter-set which is tested by the optimization algorithm. \cr
  \strong{If the parameters can change over time. The already interpolated value is passed to the ode-system.} \cr
}


}
\value{
  A list is returned which contains two elements. The first one is the error of the best particle.
  The other element is a data.frame containing the \emph{in silico} states returned from the ode-solver using the parameter-set passed by the user..
}
\note{
  \itemize{
    \item The error between the defined states and the \emph{in silico} states is the absolute difference normalised using the true state.
  }
}
\examples{
  \donttest{
# Solve an ode-system
ode <- function(t, y, ydot, parameter) {
  a_db = at(parameter, 1)
  b_db = at(parameter, 2)
  c_db = at(parameter, 3)
  d_db = at(parameter, 4)
  predator_db = at(y,1)
  prey_db = at(y, 2)
  ydot[1] = predator_db*prey_db*c_db - predator_db*d_db
  ydot[2] = prey_db*a_db - prey_db*predator_db*b_db
  return(ydot)
}
path <- system.file("examples", package = "paropt")
states <- read.table(paste(path,"/states_LV.txt", sep = ""), header = TRUE)
parameter <- data.frame(time = 0, a = 1.1, b = 0.4, c = 0.1, d = 0.4)
res <- paropt::solve(ode,
                     parameter = parameter,
                     reltol = 1e-06, abstol = c(1e-08, 1e-08),
                     states = states, verbose = FALSE)


 # solving with own error, spline and jacobian function

 jac <- function(t, y, ydot, J, parameter) {
  a_db = at(parameter, 1)
  b_db = at(parameter, 2)
  c_db = at(parameter, 3)
  d_db = at(parameter, 4)
  predator_db = at(y,1)
  prey_db = at(y, 2)

  J[1, 1] = prey_db*c_db - d_db
  J[2, 1] = - prey_db*b_db
  J[1, 2] = predator_db*c_db
  J[2, 2] = a_db - predator_db*b_db

  return(J)
}

error_fct <- function(c, a, b) {
  ret = (a - b)^2
  return(ret)
}

spline_fct <- function(t, time_vec, par_vec) {
  ret = 0
  for(i in par_vec) {
    ret = ret + i
  }
  return(ret)
}

path <- system.file("examples", package = "paropt")
states <- read.table(paste(path,"/states_LV.txt", sep = ""), header = TRUE)
parameter <- data.frame(time = 0, a = 1.1, b = 0.4, c = 0.1, d = 0.4)
res <- paropt::solve(ode,
                     parameter = parameter,
                     reltol = 1e-06, abstol = c(1e-08, 1e-08),
                     states = states, verbose = FALSE)

res <- paropt::solve(ode,
                        parameter = parameter,
                        reltol = 1e-06, abstol = c(1e-08, 1e-08),
                        states = states, verbose = FALSE,
                        own_error_fct = error_fct,
                        own_spline_fct = spline_fct,
                        own_jac_fct = jac)


  }
}
\seealso{
\code{\link{optimize}()},
\code{\link[ast2ast:translate]{ast2ast::translate()}}
}
