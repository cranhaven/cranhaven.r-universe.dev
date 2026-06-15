#' Value American-style options through least-squares Monte Carlo (LSM) simulation
#'
#' @description
#'
#' Given a set of state variables and associated payoffs simulated through Monte Carlo simulation, solve for the value of an American-style call or put option through
#' the least-squares Monte Carlo simulation method.
#'
#' @param state_variables \code{matrix} or \code{array}. The simulated states of the underlying stochastic variables. The first dimension corresponds to the simulated values
#' of the state variables at each discrete observation point. The second dimension corresponds to each individual simulation of the state variable. The third dimension
#' corresponds to each state variable considered.
#' @param payoff \code{matrix} The payoff at each observation point resulting from exercise into the underlying asset. The first dimension corresponds to the simulated values
#' of the state variables at each discrete observation point. The second dimension corresponds to each individual simulation of the state variable.
#' @param K the exercise price of the American-style option
#' @param dt Constant, discrete time step of simulated observations
#' @param rf The annual risk-free interest rate
#' @param call \code{logical} Is the American-style option a call or put option?
#' @param orthogonal \code{character}. The orthogonal polynomial used to develop basis functions that estimate the continuation value in the LSM simulation method.
#' \code{orthogonal} arguments available are: "Power", "Laguerre", "Jacobi", "Legendre", "Chebyshev", "Hermite". See \bold{details}.
#' @param degree The number of orthogonal polynomials used in the least-squares fit. See \bold{details}.
#' @param cross_product \code{logical}. Should a cross product of state variables be considered? Relevant only when the number of state variables
#' is greater than one.
#' @param verbose \code{logical}. Should additional information be output? See \bold{values}.
#'
#' @details
#'
#'The \code{LSM_american_option} function provides an implementation of the least-squares Monte Carlo (LSM) simulation approach to numerically approximate
#'the value of American-style options (options with early exercise opportunities). The function provides flexibility in the stochastic process followed by the underlying asset, with simulated values
#'of stochastic processes provided within the \code{state_variables} argument. It also provides flexibility in the payoffs of the option, allowing for vanilla as well as more exotic options to be considered.
#'\code{LSM_american_option} also provides analysis into the exercise timing and probability of early exercise of the option.
#'
#'\bold{Least-Squares Monte Carlo Simulation:}
#'
#'The least-squares Monte Carlo (LSM) simulation method is a numeric approach first presented by Longstaff and Schwartz (2001) that
#'approximates the value of options with early exercise opportunities. The LSM simulation method is considered one of the most efficient
#'methods of valuing American-style options due to its flexibility and computational efficiency. The approach can feature multiple
#'stochastically evolving underlying uncertainties, following both standard and exotic stochastic processes.
#'
#'The LSM method first approximates stochastic variables through a stochastic process to develop cross-sectional information,
#'then directly estimates the continuation value of in-the-money simulation paths by "(regressing) the ex-post realized payoffs from
#'continuation on functions of the values of the state variables" (Longstaff and Schwartz, 2001).
#'
#'The 'LSM_american_option' function at each discrete time period, for each simulated price path, compares the payoff that results from immediate exercise of
#'the option with the expected value of continuing to hold the option for subsequent periods. The payoff of immediate exercise is provided in the \code{payoff} argument and
#'could take several different meanings depending upon the type of American-style option being valued (e.g. the current stock price, the maximum price between multiple assets, etc.).
#'
#'The immediate profit resulting from exercise of the option is dependent upon the type of option being calculated. The profit of price path \eqn{i} and time \eqn{t}
#'is given by:
#'
#'When \code{call = TRUE}:
#'\deqn{profit_{(t,i)} = max(payoff_{(t,i)} - K, 0)}{profit[t,i] = max(payoff[t,i] - K, 0)}
#'
#'When \code{call = FALSE}:
#'\deqn{profit_{(t,i)} = max(K - payoff_{(t,i)}, 0)}{profit[t,i] = max(K - payoff[t,i], 0)}
#'
#'\bold{Orthogonal Polynomials:}
#'
#'To improve the accuracy of estimation of continuation values, the economic values in each period are regressed on a linear
#'combination of a set of basis functions of the stochastic variables. These estimated regression parameters and the simulated
#'stochastic variables are then used to calculate the estimator for the expected economic values.
#'
#'Longstaff and Schwartz (2001) state that as the conditional expectation of the continuation value belongs to a Hilbert space,
#'it can be represented by a combination of orthogonal basis functions. Increasing the number of stochastic state variables
#'therefore increases the number of required basis functions exponentially. The orthogonal polynomials available in the
#'\code{LSMRealOptions} package are: Laguerre, Jacobi, Legendre (spherical), Hermite (probabilistic), Chebyshev (of the first kind).
#'The simple powers of state variables is further available. Explicit expressions of each of these orthogonal polynomials are
#'available within the textbook of Abramowitz and Stegun (1965).
#'
#' @return
#'
#'The 'LSM_american_option' function by default returns a \code{numeric} object corresponding to the calculated value of the American-style option.
#'
#'When \code{verbose = T}, 6 objects are returned within a \code{list} class object. The objects returned are:
#'
#'\tabular{ll}{
#'
#' \code{Value} \tab The calculated option value. \cr
#' \code{Standard Error} \tab The standard error of the option value. \cr
#' \code{Expected Timing} \tab The expected time of early exercise. \cr
#' \code{Expected Timing SE} \tab The standard error of the expected time of early exercise. \cr
#' \code{Exercise Probability} \tab The probability of early exercise of the option being exercised. \cr
#' \code{Cumulative Exercise Probability} \tab \code{vector}. The cumulative probability of option exercise at each discrete observation point \cr
#' }
#'
#' @references
#'
#' Abramowitz, M., and I. A. Stegun, (1965). Handbook of mathematical functions with formulas, graphs, and mathematical tables. Courier Corporation.
#'
#' Longstaff, F. A., and E. S. Schwartz, (2001). "Valuing American options by simulation: a simple least-squares approach." The review of financial
#' studies, 14(1), 113-147.
#'
#' @examples
#'
#'# Price a vanilla American put option on an asset that follows
#'# Geometric Brownian Motion
#'
#'## Step 1 - simulate stock prices:
#'stock_prices <- GBM_simulate(n = 100, t = 1, mu = 0.05,
#'                          sigma = 0.2, S0 = 100, dt = 1/2)
#'
#'## Step 2 - Value the American put option:
#'option_value <- LSM_american_option(state_variables = stock_prices,
#'                                  payoff = stock_prices,
#'                                  K = 100,
#'                                  dt = 1/2,
#'                                  rf = 0.05)
#' @export
LSM_american_option <- function(state_variables, payoff, K, dt, rf,
                               call = FALSE, orthogonal = "Power", degree = 2, cross_product = TRUE,
                               verbose = FALSE){

  if(anyNA(state_variables)) stop("NA's have been specified within 'state_variables'!")

  ###FUNCTION DEFINITIONS:
  #------------------------------------------------------------------------------
  #Continuous compounding discounting
  discount = function(r, t = 1) return(exp(-r*t))

  combination = function(n, k){factorial(n) / (factorial(n-k)*factorial(k))}

  #####orthogonal Polynomial Functions:
  orthogonal_polynomials = c("Laguerre", "Jacobi", "Legendre", "Chebyshev", "Hermite")

  c_m <- function(n, m, orthogonal, alpha_value, beta_value = 0){
    if(orthogonal == "Laguerre")  return((-1)^m * combination(n, n-m) * (1/factorial(m)))
    if(orthogonal == "Jacobi")    return(combination(n + alpha_value, m) * combination(n + beta_value, n - m))
    if(orthogonal == "Legendre")  return((-1)^m * combination(n, m) * combination(2*n - 2*m, n))
    if(orthogonal == "Chebyshev") return((-1)^m * factorial(n - m - 1) / (factorial(m) * factorial(n - 2*m)))
    if(orthogonal == "Hermite")   return((-1)^m / (factorial(m) * (2^m) * factorial(n - 2*m)))}

  g_m_x <- function(n, m, x, orthogonal){
    if(orthogonal == "Laguerre") return(x^m)
    if(orthogonal == "Jacobi")   return((x-1)^(n-m) * (x+1)^m)
    if(orthogonal == "Legendre" || orthogonal == "Hermite") return(x^(n-2*m))
    if(orthogonal == "Chebyshev") return((2*x) ^ (n-2*m))}

  orthogonal_weight <- function(n, x, orthogonal, alpha_value = 0, beta_value = 0){

    ##If the orthogonal is "Power" then we'll just return the power:
    if(orthogonal == "Power") return(x^n)

    #If not, we're return an orthogonal polynomial
    orthogonal_polynomials <- c("Laguerre", "Jacobi", "Legendre", "Chebyshev", "Hermite")
    index <- which(orthogonal_polynomials == orthogonal)

    N <- c(rep(n,2), rep(n/2, 3))[index]
    if(N%%1 != 0) sprintf("warning: orthogonal weight rounded down from %i to %i", n, n-1)
    N <- floor(N)
    d_n <- c(1, rep(1/(2^n), 2), n/2, factorial(n))[index]

    output <- 0
    for(m in 0:N) output = output + c_m(n,m, orthogonal, alpha_value, beta_value) * g_m_x(n, m, x, orthogonal)

    return(d_n * output)
  }

  ###Estimated Continuation Values:
  continuation_value_calc <- function(ITM, t, continuation_value, state_variables_t, orthogonal, degree, cross_product){

    #We only consider the invest / delay investment decision for price paths that are in the money (ie. positive NPV):
    ITM_length <- length(ITM)

    #Only regress paths In the money (ITM):
    if(ITM_length>0){

      if(is.vector(state_variables_t)){
        state_variables_t_ITM <- matrix(state_variables_t[ITM])
        state_variables.ncol <- 1
      } else {
        state_variables.ncol <- ncol(state_variables_t)
        state_variables_t_ITM <- matrix(state_variables_t[ITM,], ncol = state_variables.ncol)
      }

      regression.matrix <- matrix(0, nrow = ITM_length, ncol = (1 + state_variables.ncol*degree + ifelse(cross_product, factorial(state_variables.ncol - 1),0)))
      index <- state_variables.ncol+1
      regression.matrix[,1:index] <- c(continuation_value[ITM], state_variables_t_ITM)
      index <- index + 1

      ##The Independent_variables includes the actual values as well as their polynomial/orthogonal weights:
      Regression_data <- data.frame(matrix(0, nrow = ITM_length,
                                           ncol = (state_variables.ncol*degree + ifelse(cross_product, factorial(state_variables.ncol - 1),0))), state_variables_t_ITM,
                                    dependent_variable = continuation_value[ITM])

      index <- 1
      #Basis Functions:
      for(i in 1:state_variables.ncol){
        for(n in 1:degree){
          Regression_data[,index] <- orthogonal_weight(n, state_variables_t_ITM[,i], orthogonal)
          index <- index + 1
        }
      }

      #Cross Product of state vectors:
      if(cross_product && state_variables.ncol>1){
        for(i in 1:(state_variables.ncol-1)){
          for(j in (i + 1):state_variables.ncol){
            Regression_data[,index] <- state_variables_t_ITM[,i] * state_variables_t_ITM[,j]
            index <- index + 1
          }}
      }
      ##Finally, perform the Linear Regression
      LSM_regression <- stats::lm(dependent_variable~., Regression_data)

      #if(any(is.na(LSM_regression$coefficients))) print("There are NA's present in the (complex) regression!")
      ##Assign fitted values
      continuation_value[ITM] <- LSM_regression$fitted.values
    }
    return(continuation_value)
  }



  if(length(K) > 1 && length(K) != G) stop("length of object 'K' does not equal 1 or number of columns of 'state_variables'!")
  if(nrow(state_variables) != nrow(payoff)) stop("Dimensions of object 'state_variables' does not match 'payoff'!")

  if(any(class(state_variables) == "matrix")) state_variables <- array(state_variables, dim = c(nrow(state_variables), ncol(state_variables), 1))

  if(!(orthogonal %in% c("Power", "Laguerre", "Jacobi", "Legendre", "Chebyshev", "Hermite"))){
    stop("Invalid orthogonal argument! 'orthogonal' must be Power, Laguerre, Jacobi, Legendre, Chebyshev or Hermite")
  }

  # Nominal Interest Rate:
  r <- rf * dt

  # Number of discrete time periods:
  nperiods <- dim(state_variables)[1]
  # Number of simulations:
  G <- dim(state_variables)[2]


  ####BEGIN LSM SIMULATION

  # Profit: value of immediate exercise.
  profit <- matrix(0, nrow = nperiods, ncol = G)

  # Continuation value: expected value of waiting to exercise. Compared with immediate profit to make exercise decisions through dynamic programming.
  continuation_value <- rep(0, G)

  #American option value of project, given that you can either delay or exercise:
  AOV <- rep(0, G)

  ##Optimal period of exercise is the earliest time that exercise is triggered. If no exercise, an NA is returned:
  exercise_timing <- rep(NA, G)

  ###Backwards induction loop
  for(t in nperiods:1){
    #t is representative of the index for time, but in reality the actual time period you're in is (t-1).

    ## STEP ONE:
    ### Forward foresight (high bias) - the Immediate payoff of exercising:
    if(call){
      profit[t,] <- pmax(payoff[t,] - K, 0)
    } else {
      profit[t,] <- pmax(K - payoff[t,], 0)
    }


    ## STEP TWO:

    ### Estimated continuation values:
    if(t < nperiods){

      #We only consider the invest / delay investment decision for price paths that are in the money (ie. positive NPV):
      ITM <- which(profit[t,] > 0)

      #Only regress paths In the money (ITM):
      if(length(ITM)>0){

      continuation_value <- continuation_value_calc(ITM = ITM,
                                               t = t,
                                               continuation_value = AOV  * discount(r),
                                               state_variables_t = state_variables[t,,],
                                               orthogonal = orthogonal,
                                               degree = degree,
                                               cross_product = cross_product)
      } else {
        continuation_value <- AOV  * discount(r)
      }
    }

    ## STEP THREE:

    ### Dynamic programming:
    exercise <- profit[t,] > continuation_value

    ## Discount existing values if not exercising
    AOV[!exercise] <- AOV[!exercise] * discount(r)
    ## Receive immediate profit if exercising
    AOV[exercise] <- profit[t,exercise]

    ##Was the option exercised?
    exercise_timing[exercise] <- t

    ###Re-iterate:
  }
  ###End backwards induction


  #Calculate project value: discount payoffs
  option_values <- rep(0, G)

  # American option value: discounting payoffs back to time zero, averaging over all paths.

  exercised_paths <- !is.na(exercise_timing)
  exercise_period <- exercise_timing[exercised_paths]
  itm_paths <- which(exercised_paths)
  option_values[exercised_paths] <- profit[nperiods * (itm_paths-1) + exercise_period] * discount(r, exercise_period)

  # option value:
  ROV <- mean(option_values)

  if(!verbose)  return(ROV)

  # Verbose Outputs:
  exercise_time <- (exercise_period-1)*dt

  return(list(
    ## Option value
    `Value` = ROV,
    ## Option value standard error
    `Standard Error` = sqrt(stats::var(option_values) / G),
    ## expected exercise time
    `Expected Timing` = mean(exercise_time),
    ## exercise time standard error
    `Expected Timing SE` = sqrt(stats::var(exercise_time) / G),
    ## exercise prob.
    `Exercise Probability` = length(itm_paths) / G,
    ## cumulative exercise prob.
    `Cumulative Exercise Probability` = cumsum(table(c(exercise_time, (0:(nperiods-1))*dt)) - 1) / G
))
}
