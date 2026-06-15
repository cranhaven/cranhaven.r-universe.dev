
#' Value operationally flexible capital investment projects through least-squares Monte Carlo (LSM) simulation:
#' @description
#'
#' Given a set of state variables and associated net cash flows for an investment project simulated through Monte Carlo simulation,
#' solve for the real option value of a capital investment project that has the flexibility to temporarily suspend or permanently abandon operations
#' through the least-squares Monte Carlo simulation method.
#'
#' @param state_variables \code{matrix} or \code{array}. The simulated states of the underlying stochastic variables. The first dimension corresponds to the simulated values
#' of the state variables at each discrete observation point. The second dimension corresponds to each individual simulation of the state variable. The third dimension
#' corresponds to each state variable considered.
#' @param NCF The net cash flows resulting from operating the underlying capital investment project for one discrete time step at the current simulated values of the state variables.
#' Each column corresponds to a simulated price path of underlying stochastic variables, and each row the net cash flows at a discrete time point for each simulated path.
#' The dimensions of object 'NCF' must match the first two dimensions of the object passed to the 'state_variables' argument.
#' @param CAPEX \code{numeric} or \code{vector} object. The initial capital investment of the project. This value can be either constant or time dependent. When the 'CAPEX' argument
#' is time dependent, it must be a vector object of length equal to the number of discrete observations of the simulations (i.e. the number of rows of both 'state_variables' and 'NCF')
#' @param dt Constant, discrete time step of simulated observations
#' @param rf The annual risk-free interest rate
#' @param construction An integer corresponding to the number of periods of construction of the underlying asset. The construction time represents the time between
#' the initial capital expenditure and when net cash flows are accrued, representing the construction time required of the investment project.
#' @param orthogonal \code{character}. The orthogonal polynomial used to develop basis functions that estimate the continuation value in the LSM simulation method.
#' \code{orthogonal} arguments available are: "Power", "Laguerre", "Jacobi", "Legendre", "Chebyshev", "Hermite". See \bold{details}.
#' @param degree The number of orthogonal polynomials used in the least-squares fit. See \bold{details}.
#' @param cross_product \code{logical}. Should a cross product of state variables be considered? Relevant only when the number of state variables
#' is greater than one
#' @param suspend_CAPEX The cash flow resulting from suspending an operational project
#' @param suspend_OPEX the ongoing cash flow resulting from being suspended for one discrete time period
#' @param resume_CAPEX the cash flow resulting from resuming a suspended project
#' @param abandon_CAPEX the terminal cash flow resulting from permanently abandoning a project
#' @param save_states \code{logical}. Should the operational state of each simulated price path be returned? See \bold{values}.
#' @param verbose \code{logical}. Should additional information be output? See \bold{values}.
#' @param debugging \code{logical} Should additional simulation information be output? See \bold{values}.
#'
#' @details
#'The \code{LSM_real_option_OF} function provides an implementation of the least-squares Monte Carlo (LSM) simulation approach to numerically approximate
#'the value of capital investment projects considering the flexibility of timing of investment and operational states under stochastically evolving uncertainty. The function provides flexibility in the stochastic behavior of the underlying uncertainty, with simulated values
#'of state variables provided within the \code{state_variables} argument. Multiple stochastically evolving uncertainties can also be considered. The \code{LSM_real_option_OF} function also provides analysis into the
#'expected investment timing, probability, and operating modes of the project.
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
#'\bold{Real Options Analysis}
#'
#'Real options analysis of investment projects considers the value of the option to delay investment in a project until
#'underlying, stochastically evolving uncertainty has revealed itself. Real options analysis treats investment into capital investment projects as an optimal stopping problem, optimizing the timing of investment that maximizes the payoffs of investment under underlying stochastic uncertainty.
#'Real options analysis is also capable of evaluating the critical value of underlying state variables at which immediate investment into a project is optimal. See Dixit and Pindyck (1994) for more details of real options analysis.
#'
#'The \code{LSM_real_option_OF} function considers the option to invest into a capital investment project within a finite forecasting horizon.
#'Investment into the project results in accruing all future net cash flows (NCF) until the end of the forecasting horizon at the cost of the capital expenditure (CAPEX).
#'
#'The primary difference between the \code{LSM_real_option} and \code{LSM_american_option} function is the way in which they evaluate the payoff of
#'exercise of the American-style option. The \code{LSM_american_option} function considers the payoff of exercise to be a one time payoff (i.e. buying or selling the security in a vanilla call or put option) corresponding to the respective payoff argument.
#'The \code{LSM_real_option_OF} function instead, at each discrete time period, for each simulated price path, compares the sum of all remaining discounted cash flows that are accrued following immediate investment into
#'a project to the end of the forecasting horizon with the expected value of delaying investment. This is is known as the 'running present value' (RPV) of the project, which is the discretised present value of all
#'future cash flows of the project. The RPV of a project increases as the size of the discrete time step decreases, highlighting the need
#'for small discrete time steps to accurately value investment projects. This is due to the discounting effect of discounting larger
#'cash flows over greater time periods compared to smaller cash flows being discounted more frequently.
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
#'\code{LSM_real_options} package are: Laguerre, Jacobi, Legendre (spherical), Hermite (probabilistic), Chebyshev (of the first kind).
#'The simple powers of state variables is further available. Explicit expressions of each of these orthogonal polynomials are
#'available within the textbook of Abramowitz and Stegun (1965).
#'
#' \bold{Operational Flexibility:}
#'
#' Operational flexibility is adjustment made to a project in response to, or anticipation of, positive or negative economic outcomes
#' that occur after a project has begun. Operational flexibility has the ability to mitigate the effect of poor timing of initial
#' investment by maximizing gains and minimizing losses. Operational flexibility that reduces losses in response to negative economic
#' conditions (e.g. output prices falling to levels where operations are no longer profitable) reduce the irreversibility of initial
#' investment into a project. This is particularly valuable for investment projects in highly competitive business operating conditions,
#' high initial investment costs, long residual lifetimes of operations, high levels of volatility in underlying assets and net present
#' values near zero.
#'
#' The operational flexibility supported by the \code{LSM_real_option_OF} function is the ability to temporarily suspend and permanently
#' abandon an operational investment project. The ability to temporarily suspend and permanently abandon operations was first presented
#' in the prolific work of Brennan and Schwartz (1985), whom solved for the trigger values of suspension, resumption and abandonment of
#' a finite natural resource investment under natural resource price uncertainty. The work of Cortazar, Gravet and Urzua (2008) solved
#' this optimal switching problem using the LSM simulation method.
#'
#' Operational flexibilities are path dependent, compound and mutually exclusive real options. Under consideration of operational
#' flexibility, there are several different "states" or operating modes available for an investment project. These are:
#'
#' * Not yet invested
#' * Operational
#' * Suspended
#' * Abandoned
#'
#' With the "Under construction" state further considered when construction time is also considered.
#'
#' Generally, there is a cost of switching between operating modes (for example, switching from the "not yet invested" state into the
#' "operational" state incurs the cost of the initial capital investment, 'CAPEX'). The costs incurred in temporarily suspending,
#' resuming, or abandoning a project could be attributed to firing/hiring project employees, loss in perishable products and general cost
#' of moving a project into a 'suspended' state. Operational expenditures of a suspended project could include maintenance to inactive
#' machinery, preventing weathering of project sites, ongoing rent or utility payments, etc. Finally, the abandonment expenditure could
#' be negative, representing a 'scrap' value of the project that is returned as project assets are sold on abandonment of the project.
#'
#' Solving for the value of an investment project with operational flexibility is classified as an optimal switching or optimal control
#' model. The objective is to optimize the value of the project by taking the optimal set of decisions for the project operating mode
#' at each time point under uncertainty.
#'
#' The 'LSM_real_option_OF' function solves for the optimal switching problem at each discrete time point throughout the backward induction
#' process. For each operating mode, the immediate payoff of switching to a different operating mode is compared against the discounted 'continuation' value
#' of staying in the current operating mode. These continuation values are directly estimated through least-squares regression of all simulated price paths (rather than just 'in-the-money' price paths, as is
#' considered in the value of the option to delay the initial investment decision).
#'
#' The 'ROV' value of the investment project is then calculated by considering the option to invest into the 'operational' state. Investment into the project is optimal when the value of the 'operational' state is
#' greater than the expected continuation value.
#'
#' The introduction of operational flexibility to the LSM simulation method greatly increases the level of computational complexity and thus the
#' \code{LSM_real_option_OF} function is considerably slower than the \code{LSM_real_option} function.
#'
#' @return
#'
#'The \code{LSM_real_option_OF} function by default returns a \code{list} object containing 3 objects, with further objects returned within this list when the
#' \code{save_states}, \code{verbose} and \code{debugging} arguments are set to \code{TRUE}.
#'
#' The objects returned are:
#' \tabular{ll}{
#'
#' \code{ROV} \tab Real option value. The value of an investment project when actively managing the investment and appropriately exercising options \cr
#' \code{NPV + OF} \tab Net present Value + operational flexibility. The value immediate investment into an investment project when actively managing and appropriately exercising operational flexibility \cr
#' \code{NPV} \tab  Net present value. The value immediate investment into an investment project \cr
#' \code{WOV} \tab  Waiting option value. The value of the option to delay initial investment into an investment project\cr
#' }
#'
#' When \code{save_states = TRUE}, an additional object is returned:
#' \tabular{ll}{
#'
#' \code{Project States} \tab  A \code{data.frame} object detailing the proportion of simulated price paths in each operating state at each discrete time point\cr
#' }
#'
#'
#'When \code{verbose = T}, an additional 7 objects are returned within the \code{list} class object, providing further analysis
#'into the capital investment project:
#' \tabular{ll}{
#' \code{ROV SE} \tab  The standard error of 'ROV'. \cr
#' \code{NPV SE} \tab  The standard error of 'NPV'. \cr
#' \code{WOV SE} \tab  The standard error of 'WOV'. \cr
#' \code{Expected Timing} \tab  The expected timing of investment, in years. \cr
#' \code{Expected Timing SE} \tab  The standard error of the expected timing of investment. \cr
#' \code{Investment Prob} \tab  The probability of investment within the forecasting horizon. \cr
#' \code{Cumulative Investment Prob} \tab The cumulative probability of investment at each discrete time point over the forecasting horizon. \cr
#' }
#'
#'When \code{debugging = T}, an additional 5 objects are returned within the \code{list} class object.
#'These objects provide information about the values of individual simulated price paths:
#' \tabular{ll}{
#' \code{Investment Period} \tab The time of investment of invested price paths. Price paths that did not trigger investment are represented as \code{NA} \cr
#' \code{Project Value} \tab The calculated project value at time zero for each simulated price path. The 'ROV' is equal to the mean of this vector. \cr
#' \code{Immediate Profit} \tab The profit resulting from immediate investment for each discrete time period and for all simulated price paths  \cr
#' \code{Running Present Value} \tab  The present value of all future cash flows of an investment project for each discrete time period and for all simulated price paths\cr
#' \code{Path Project States} \tab The current project state  for each discrete time period and for all simulated price paths \cr
#' }
#'
#'@references
#'
#' Abramowitz, M., and I. A. Stegun, (1965). Handbook of mathematical functions with formulas, graphs, and mathematical tables. Courier Corporation.
#'
#' Brennan, M. J., and E. S. Schwartz, (1985). Evaluating natural resource investments. Journal of business, 135-157.
#'
#' Dixit, A. K., and R. S. Pindyck, (1994). Investment under uncertainty. Princeton university press.
#'
#' Longstaff, F. A., and E. S. Schwartz, (2001). Valuing American options by simulation: a simple least-squares approach. The review of financial studies, 14(1), 113-147.
#'
#' Cortazar, G., M. Gravet, and J. Urzua, (2008). The valuation of multidimensional American real options using the LSM simulation method. Computers & Operations Research, 35(1), 113-129.
#'
#' @examples
#'
#'# Example: Value a capital investment project where the revenues follow a
#'# Geometric Brownian Motion stochastic process:
#'
#'## Step 1 - Simulate asset prices:
#'asset_prices <- GBM_simulate(n = 100, t = 10, mu = 0.05,
#'                          sigma = 0.2, S0 = 100, dt = 1/2)
#'
#'## Step 2 - Perform Real Option Analysis (ROA):
#'ROA <- LSM_real_option_OF(state_variables = asset_prices,
#'                      NCF = asset_prices - 100,
#'                      CAPEX = 1000,
#'                      dt = 1/2,
#'                      rf = 0.05,
#'                      suspend_CAPEX = 100,
#'                      suspend_OPEX = 10,
#'                      resume_CAPEX = 100,
#'                      abandon_CAPEX = 0
#'                      )
#'
#' @export
LSM_real_option_OF <- function(state_variables, NCF, CAPEX, dt, rf, construction = 0,
                           orthogonal = "Laguerre", degree = 9,
                           cross_product = TRUE,
                           suspend_CAPEX = NULL, suspend_OPEX = NULL, resume_CAPEX = NULL, abandon_CAPEX = NULL, save_states = FALSE,
                           verbose = FALSE, debugging = FALSE){

  if(anyNA(state_variables)) stop("NA's have been specified within 'state_variables'!")

  ###FUNCTION DEFINITIONS:
  #------------------------------------------------------------------------------
  #Continuous compounding discounting
  discount <- function(r, t = 1) return(exp(-r*t))

  combination <- function(n, k){factorial(n) / (factorial(n-k)*factorial(k))}

  #####orthogonal Polynomial Functions:
  orthogonal_polynomials <- c("Laguerre", "Jacobi", "Legendre", "Chebyshev", "Hermite")

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
  continuation_value_calc <- function(profit, t, continuation_value, state_variables_t, orthogonal, degree, cross_product, moneyness){

    #We only consider the invest / delay investment decision for price paths that are in the money (ie. positive NPV):
    if(moneyness){
      ITM <- which(profit > 0)
      ITM_length <- length(ITM)
    } else {
      ITM <- TRUE
      ITM_length <- length(continuation_value)
    }


    #Only regress paths In the money (ITM):
    if(ITM_length>0){

      if(is.vector(state_variables_t)){
        state_variables_t_ITM <- matrix(state_variables_t[ITM])
        state_variables.ncol <- 1
      } else {
        state_variables.ncol <- ncol(state_variables_t)
        state_variables_t_ITM <- matrix(state_variables_t[ITM,], ncol = state_variables.ncol)
      }

      regression_matrix <- matrix(0, nrow = ITM_length, ncol = (1 + state_variables.ncol*degree + ifelse(cross_product, factorial(state_variables.ncol - 1),0)))
      index <- state_variables.ncol+1

      regression_matrix[,1:index] <- c(continuation_value[ITM], state_variables_t_ITM)
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
      if(cross_product && state_variables.ncol > 1){
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
  #------------------------------------------------------------------------------

  #Nominal Interest Rate:
  r <- rf * dt

  #Number of discrete time periods:
  nperiods <- dim(state_variables)[1]
  #Number of simulations:
  G <- dim(state_variables)[2]

  ## Error Catching:
  if(length(CAPEX) > 1 && length(CAPEX) != nperiods) stop("length of object 'CAPEX' doe not equal 1 or dim(state_variables)[2]!")
  if(construction %% 1 > 0) stop("object 'construction' must be type 'integer'!")
  if(nrow(state_variables) != nrow(NCF)) stop("Dimensions of object 'state_variables' does not match 'NCF'!")

  if(any(class(state_variables) == "matrix")) state_variables <- array(state_variables, dim = c(nrow(state_variables), ncol(state_variables), 1))

  if(!(orthogonal %in% c("Power", "Laguerre", "Jacobi", "Legendre", "Chebyshev", "Hermite"))){
    stop("Invalid orthogonal argument! 'orthogonal' must be Power, Laguerre, Jacobi, Legendre, Chebyshev or Hermite")
  }

  ## If there's only 1 state variable, we cannot perform cross products
  if(dim(state_variables)[3] == 1) cross_product <- FALSE

  if(save_states || debugging) verbose <- TRUE

  ## OF error catching:
  if(!(!any(c(is.null(suspend_CAPEX), is.null(suspend_OPEX), is.null(resume_CAPEX))) &&
       !all(c(is.null(suspend_CAPEX), is.null(suspend_OPEX), is.null(resume_CAPEX))))) stop("arguments 'suspend_CAPEX', 'resume_CAPEX' and 'suspend_OPEX' must all be specified!")

  ## If they are set to null (i.e. they're not being considered, make the cost of switching be insurmountably high)
  if(is.null(suspend_CAPEX)) suspend_CAPEX <- - 1e20 else suspend_CAPEX <- - suspend_CAPEX
  if(is.null(suspend_OPEX))  suspend_OPEX  <- - 1e20 else suspend_OPEX  <- - suspend_OPEX
  if(is.null(resume_CAPEX))  resume_CAPEX  <- - 1e20 else resume_CAPEX  <- - resume_CAPEX
  if(is.null(abandon_CAPEX)) abandon_CAPEX <- - 1e20 else abandon_CAPEX <- - abandon_CAPEX

  ####BEGIN LSM SIMULATION

  #Running Present Value is the PV of all future NCF from that time point.
  RPV_t <- matrix(0, nrow = nperiods, ncol = G)

  #Immediate Profit is the NPV of the project conditional. You expend the NINV costs, wait the construction time, obtain the RPV at the time point that you become operational.
  Immediate_profit  <- matrix(0, nrow = nperiods, ncol = G)

  #Expected value of waiting to invest. Compared with Immediate Profit to make investment decisions through dynamic programming.
  Investment_continuation_value <- rep(0, G)

  #Option value of project, given that you can either delay investment or invest into an operational project:
  X_t <- rep(0, G)

  ##Optimal timing of investment is the earliest time that investment is triggered. If no investment is triggered, an NA is returned:
  Investment_timing <- rep(NA, G)

  #If we're at last time period, there's no time to make any changes, and the payoff would just be operating:
  if(construction == 0) Immediate_profit[nperiods,] <- NCF[nperiods,] - CAPEX[ifelse(length(CAPEX)>1, nperiods, 1)]

  # Operational Flexibility:

  ## Value of being operational:
  V_t <- NCF[nperiods,]

  ##Value of being suspended and the RPV of being suspended:
  RPV.Suspended_t <- W_t <- rep(suspend_OPEX * discount(r), G)

  ## Project states? (computationally intensive)
  if(save_states){
    ##Optimal future project states given that you are operational:
    State.operational <- matrix("", nrow = nperiods, ncol = G)
    State.operational[nperiods,] <- "Operational"
    ##Optimal future project states given that you are suspended:
    State.suspended <- matrix("", nrow = nperiods, ncol = G)
    State.suspended[nperiods,] <- "Suspended"
  }

  ###Backwards induction loop
  for(t in nperiods:1){

    ## Can investment occur in this time period ?
    Investment_opportunity <- (nperiods - t) >= construction

    #t is representative of the index for time, but in reality the actual time period you're in is (t-1).

    if(t < nperiods){

      ## Operational Flexibility:

      ### Estimated continuation value of being in the "Operational" state:
      V_t <- continuation_value_calc(profit = 0,
                         t = t,
                         continuation_value = V_t  * discount(r),
                         state_variables_t = state_variables[t,,],
                         orthogonal = orthogonal,
                         degree = degree,
                         cross_product = cross_product,
                         ## All price paths are considered
                         moneyness = FALSE)

      ### Estimated continuation value of being in the "Suspended" state:
      W_t <- continuation_value_calc(profit = 0,
                         t = t,
                         continuation_value = W_t  * discount(r),
                         state_variables_t = state_variables[t,,],
                         orthogonal = orthogonal,
                         degree = degree,
                         cross_product = cross_product,
                         ## All price paths are considered
                         moneyness = FALSE)

      #Cash Flows that result from Particular path dependent decisions:
      Remain_open     <-  NCF[t,] + V_t
      Remain_closed   <-  suspend_OPEX  + W_t
      Closed_to_open  <-  Remain_open   + resume_CAPEX
      Open_to_closed  <-  Remain_closed + suspend_CAPEX

      ##Dynamic Programming:
      #Value of the operational project:
      V_t <- pmax(Open_to_closed, Remain_open, abandon_CAPEX)
      #Value of the suspended project:
      W_t <- pmax(Closed_to_open, Remain_closed, abandon_CAPEX)

      ###Cash Flows that result from the real options exercised at each path:

      stay_open   <- V_t == Remain_open
      go_closed   <- V_t == Open_to_closed
      bail_out    <- V_t == abandon_CAPEX
      open_up     <- W_t == Closed_to_open
      stay_closed <- W_t == Remain_closed
      bail_bail   <- W_t == abandon_CAPEX

      RPV_t[t,stay_open]    <- NCF[t,stay_open] + RPV_t[t+1,stay_open] * discount(r)
      RPV_t[t,go_closed]    <- suspend_OPEX  + suspend_CAPEX  + RPV.Suspended_t[go_closed]  * discount(r)
      RPV_t[t,bail_out]     <- abandon_CAPEX

      RPV.Suspended_t[stay_closed] <- suspend_OPEX + RPV.Suspended_t[stay_closed] * discount(r)
      RPV.Suspended_t[open_up]     <- NCF[t,open_up] + resume_CAPEX  + RPV_t [t+1,open_up] * discount(r)
      RPV.Suspended_t[bail_bail]   <- abandon_CAPEX

      ## Project states:
      if(save_states){

        ##No state change:
        State.operational[t, stay_open]  <- "Operational"
        State.suspended  [t,stay_closed] <- "Suspended"

        ##State Change:
        State.operational[t, go_closed] <- "Suspended"
        State.suspended  [t,open_up]    <- "Operational"

        ##Adjustment of future states based upon state change:
        State.operational[(t+1):nperiods, go_closed]  <- State.operational[(t+1):nperiods, go_closed]
        State.suspended  [(t+1):nperiods, open_up]    <- State.operational[(t+1):nperiods, open_up]

        State.operational[t:nperiods, bail_out]  <- "Abandoned"
        State.suspended  [t:nperiods,bail_bail]  <- "Abandoned"
      }


      #Forward foresight (high bias) - the Immediate payoff of investing into an operational project (considering possible construction time):
      if(Investment_opportunity) Immediate_profit[t,] <- (RPV_t[t+construction,] * discount(r, construction)) - CAPEX[ifelse(length(CAPEX)>1, t,1)]

      ###Estimated Continuation Values:
      Investment_continuation_value <- continuation_value_calc(profit = Immediate_profit[t,],
                                                         t = t,
                                                         continuation_value = X_t  * discount(r),
                                                         state_variables_t = state_variables[t,,],
                                                         orthogonal = orthogonal,
                                                         degree = degree,
                                                         cross_product = cross_product,
                                                         moneyness = TRUE)

    }

    # ##Dynamic Programming:
    # #Now, the optimal decision at this time period is to either invest, and receive the
    # immediate profit subtract investment capital, delay your investment
    # because there's value in waiting, or just because there's no value at all.
    Invest <- Immediate_profit[t,] > Investment_continuation_value

    X_t[!Invest] <- X_t[!Invest] * discount(r)
    X_t[Invest]  <- Immediate_profit[t,Invest]

    ##Was investment triggered?
    if(Investment_opportunity) Investment_timing[Invest] <- t

    ###Re-iterate:
  }
  ###End Backwards Induction

  #Now that we know the optimal investment timing, we can calculate project value by discounting back from that time point.
  projects_value <- rep(0, G)

  # ##The American option can now be valued by discounted each cash flow in the option cash flow matrix back to time zero, averaging over all paths.

  invested_paths <- !is.na(Investment_timing)
  #The column index represents the time period plus one, thus the optimal timing is given by:
  Optimal_Timing <- Investment_timing[invested_paths] - 1
  itm_paths <- which(invested_paths)
  projects_value[invested_paths] <- Immediate_profit[nperiods * (itm_paths-1) + Investment_timing[invested_paths]] * discount(r, Optimal_Timing)

  ##The project value is the mean of all of the potential price paths:
  ROV <- mean(projects_value)
  NPV <- mean(Immediate_profit[1,])
  WOV <- ROV - NPV

  ## The pure expected NPV of the project:
  if(construction > 0){
    E_NPV <-mean(colSums(NCF[-(1:construction),] * exp(-r * 0:(nperiods - 1))[-(1:construction)])) * discount(r) - CAPEX[1]
  } else {
    E_NPV <-mean(colSums(NCF * exp(-r * 0:(nperiods - 1)))) * discount(r)  - CAPEX[1]
  }

  output.list <- list(ROV = ROV, `NPV OF` = NPV, NPV = E_NPV, WOV = WOV)

  if(!verbose)  return(output.list)

  ## Verbose - Return additional values:

  ##Calculate simulation Standard Errors:
  NPV_SE <- sqrt(stats::var(Immediate_profit[1,]) / G)
  WOV_SE <- sqrt(stats::var(projects_value - Immediate_profit[1,]) / G)
  ROV_SE <- sqrt(stats::var(projects_value) / G)


  # Other Analysis:
  Optimal_Timing <- Optimal_Timing*dt
  Investment_Prob <- length(Optimal_Timing) / length(Investment_timing)
  ##Cumulative investment probability
  Cum_Investment_Prob <- cumsum(table(c(Optimal_Timing, (0:(nperiods-1))*dt)) - 1) / G
  Expected_Investment_time  <- mean(Optimal_Timing)
  se_Expected_Investment_time <- sqrt(stats::var(Optimal_Timing) / G)

  output.list <- c(output.list, list( `ROV SE` = ROV_SE,
                                      `NPV SE` = NPV_SE,
                                      `WOV SE` = WOV_SE,
                                      `Expected Timing` = Expected_Investment_time,
                                      `Expected Timing SE` = se_Expected_Investment_time,
                                      `Investment Prob` = Investment_Prob,
                                      `Cumulative Investment Prob` = Cum_Investment_Prob))

  ## Project states:
  if(save_states){

    States.table <- matrix(0, nrow = nperiods, ncol = 5)
    colnames(States.table) <- c("Yet to Invest", "Under Construction", "Operational", "Suspended", "Abandoned")

    thetimings <- Investment_timing
    thetimings[!invested_paths] <- nperiods+1

    for(i in 1:nperiods){
      State.operational[i, i < thetimings] <- "Yet to Invest"
      if(construction > 0) State.operational[i,i >= thetimings & i < (thetimings + construction)] <- "Under Construction"
      table <- table(State.operational[i,])
      States.table[i,names(table)] <- table
    }

    States.table <- States.table[,colSums(States.table)>0] / G

    output.list <- c(output.list, list(`Project States` = States.table))

  }

  if(!debugging) return(output.list)

  ## Debugging - Return full matrices (memory intensive):

  output.list <- c(output.list, list( `Investment Period` = (Investment_timing-1)*dt,
                                      `Project Value` = projects_value,
                                      `Immediate Profit` = Immediate_profit,
                                      `Running Present Value` = RPV_t))

  if(save_states) output.list <- c(output.list, list(`Path Project States` = State.operational))


  return(output.list)

}

