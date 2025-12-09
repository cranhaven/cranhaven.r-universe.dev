#' Basic structural blocks
#'
#' Creates the basic structure for a dlm block with desired order.
#'
#' @param ... Named values for the planning matrix.
#' @param order integer: The order of the structure. Must be positive
#' @param name character: An optional argument providing the name for this block. Can be useful to identify the models with meaningful labels, also, the name used will be used in some auxiliary functions.
#' @param D array, matrix, vector or scalar: The values for the discount factors associated with the latent states at each time. If D is an array, its dimensions should be n x n x t, where n is the order of the polynomial block and t is the length of the outcomes. If D is a matrix, its dimensions should be n x n and the same discount matrix will be used in all observations. If D is a vector, it should have size t and it is interpreted as the discount factor at each observed time (same discount for all variable). If D is a scalar, the same discount will be used for all latent states at all times.
#' @param h matrix, vector or scalar: A drift to be add after the temporal evolution (can be interpreted as the mean of the random noise at each time). If a matrix, its dimension should be n x t, where n is the number of latent states (i.e., the order) and t is the length of the series. If a vector, it should have size t, and each value will be applied to the first latent state (the one which affects the linear predictors) in their respective time. If a scalar, the passed value will be used for the first latent state at each time.
#' @param H array, matrix, vector or scalar: The values for the covariance matrix for the noise factor at each time. If H is an array, its dimensions should be n x n x t, where n is the order of the polynomial block and t is the length of the series. If H is a matrix, its dimensions should be n x n and its values will be used for each time. If H is a vector or scalar, a discount factor matrix will be created as a diagonal matrix with the values of H in the diagonal.
#' @param a1 vector or scalar: The prior mean for the latent states associated with this block at time 1. If a1 is a vector, its dimension should be equal to the order of the polynomial block. If a1 is a scalar, its value will be used for all latent states.
#' @param R1 matrix, vector or scalar: The prior covariance matrix for the latent states associated with this block at time 1. If R1 is a matrix, its dimensions should be n x n. If R1 is a vector or scalar, a covariance matrix will be created as a diagonal matrix with the values of R1 in the diagonal.
#' @param monitoring vector: A vector of flags indicating which variables should be monitored (if automated monitoring is used). Its size should be n. The default is that only the first order component of this structure should be monitored.
#'
#' @keywords internal
base_block <- function(..., order, name,
                       D, h, H,
                       a1, R1,
                       monitoring) {
  if (any(D > 1 | D < 0) & is.numeric(D)) {
    stop("Error: The discount factor D must be a value between 0 and 1 (included).")
  }
  if (any(D == 0)) {
    warning("Some value of D are equal to 0. Those values will be treated as 1.")
  }
  values <- list(...)
  vars <- names(values)
  if (any(vars == "")) {
    stop("Error: One or more linear predictors are unnamed. Please, name all arguments.")
  }
  if (any(vars == "const")) {
    stop("Error: Cannot create a linear predictor named 'const' (reserved name). Choose another label.")
  }
  var.len <- sapply(values, length)
  k <- length(values)
  t <- max(var.len)
  if (any(var.len != t & var.len > 1)) {
    stop(paste0("Error: Outcomes have mismatching lengths. Expected 1 or ", t, ", got: ", paste(var.len, collapse = ", "), "."))
  }

  if (length(D) == 1) {
    D.base <- diag(order)
    diag(D.base) <- D
    D <- array(D.base, c(order, order, t))
  } else if (is.vector(D)) {
    if (length(D) == order) {
      placeholder <- diag(order)
      diag(placeholder) <- D
      D <- array(placeholder, c(order, order, t))
    } else {
      D <- sapply(D, function(x) {
        diag(order) * x
      }, simplify = "array") |> array(dim = c(order, order, length(D)))
    }
  } else if (is.matrix(D)) {
    D <- array(D, c(dim(D)[1], dim(D)[2], t))
  } else if (length(dim(D)) == 3 & dim(D)[3] == 1) {
    D <- array(D, c(dim(D)[1], dim(D)[2], t))
  }
  t <- if (t == 1) {
    dim(D)[3]
  } else {
    t
  }

  if (length(dim(D)) > 3 | any(dim(D)[1:2] != order) | (dim(D)[3] != t & t > 1)) {
    stop(paste0("Error: Invalid shape for D. Expected ", order, "x", order, "x", t, ". Got ", paste(dim(D), collapse = "x"), "."))
  }

  if (length(H) == 1) {
    pre.H <- diag(order)
    diag(pre.H) <- H
    H <- array(pre.H, c(order, order, t))
  } else if (is.vector(H)) {
    if (length(H) == order) {
      placeholder <- diag(order)
      diag(placeholder) <- H
      H <- array(placeholder, c(order, order, t))
    } else {
      H.vals <- H
      pre.H <- diag(order)
      H <- array(0, c(order, order, length(H.vals)))
      for (i in 1:length(H.vals)) {
        diag(pre.H) <- H.vals[i]
        H[, , i] <- pre.H
      }
    }
  } else if (is.matrix(H)) {
    H <- array(H, c(dim(H)[1], dim(H)[2], t))
  }
  t <- if (t == 1) {
    dim(H)[3]
  } else {
    t
  }

  if (t == 1 & is.null(dim(h)) & length(h) != order) {
    t <- length(h)
  }
  if (!(length(h) %in% c(1, order, t, order * t))) {
    stop(paste0("Error: Invalid shape for h. Expected ", order, "x", t, ". Got ", paste(if (is.null(dim(h))) {
      length(h)
    } else {
      dim(h)
    }, collapse = "x"), "."))
  }
  h <- matrix(h, order, t, byrow = (length(h) != order))

  D <- array(D, c(order, order, t))
  H <- array(H, c(order, order, t))

  if (length(dim(H)) > 3 | any(dim(H)[1:2] != order) | (dim(H)[3] != t & t > 1)) {
    stop(paste0("Error: Invalid shape for H. Expected ", order, "x", order, "x", t, ". Got ", paste(dim(H), collapse = "x"), "."))
  }

  FF <- array(0, c(order, k, t), dimnames = list(NULL, vars, NULL))
  FF.labs <- matrix("const", order, k, dimnames = list(NULL, vars))
  count.regr <- 0
  for (i in 1:k) {
    name.var <- vars[i]
    if (typeof(values[[name.var]]) == "character") {
      FF[1, i, ] <- NA
      FF.labs[1, i] <- values[[name.var]]
      if (values[[name.var]] == "const") {
        stop("Error: Predictor value is equal to 'const', but 'const' is a reserved name. Choose another label.")
      }
    } else {
      if (any(is.infinite(values[[name.var]]))) {
        stop("Error: Cannot have a infinite valued covariate.")
      }
      values[[name.var]] <- ifelse(is.na(values[[name.var]]), 0, values[[name.var]])
      FF[1, i, ] <- values[[name.var]]
      if (any(values[[name.var]] != max(values[[name.var]]))) {
        count.regr <- count.regr + 1
        FF.labs[1, i] <- "Covariate"
      }
    }
  }

  not.observed.flag <- is.na(FF) & (array(FF.labs, c(order, k, t)) == "const")
  D[, , apply(not.observed.flag, 3, any)] <- 1
  H[, , apply(not.observed.flag, 3, any)] <- 0
  FF <- ifelse(not.observed.flag, 0, FF)

  G <- diag(order)
  if (length(a1) == 1) {
    a1 <- rep(a1, order)
  }
  if (length(R1) == 1 | is.vector(R1)) {
    pre.R1 <- diag(order)
    diag(pre.R1) <- R1
    R1 <- pre.R1
  }


  if (length(dim(R1)) > 2) {
    stop(paste0("Error: R1 must be a matrix, but it has ", length(dim(R1)), " dimensions."))
  }
  if (any(dim(R1) != order)) {
    stop(paste0("Error: R1 must have dimensions ", order, "x", order, ". Got ", dim(R1)[1], "x", dim(R1)[2], "."))
  }

  var.labs <- 1:order

  if (length(monitoring) != order) {
    stop(paste0("Error: monitoring size should be equal to the number of latent states. Expected ", order, ", got ", length(monitoring), "."))
  }

  var.names <- list()
  var.names[[name]] <- var.labs
  block <- list(
    "FF" = FF,
    "FF.labs" = FF.labs,
    "G" = array(G, c(order, order, t)),
    "G.labs" = matrix("const", order, order),
    "G.idx" = matrix(NA, order, order),
    "D" = D,
    "h" = h,
    "H" = H,
    "a1" = a1,
    "R1" = R1,
    "var.names" = var.names,
    "order" = order,
    "period" = 1,
    "n" = order,
    "t" = t,
    "k" = k,
    "pred.names" = vars,
    "monitoring" = monitoring,
    "interventions" = list(),
    "type" = "Basic",
    "scale" = rep(1, order),
    "direction" = diag(order)
  )
  class(block) <- "dlm_block"
  block$status <- check.block.status(block)
  return(block)
}



#' Structural blocks for polynomial trends and regressions
#'
#' Creates the structure for a polynomial block with desired order.
#'
#' @param ... Named values for the planning matrix.
#' @param X Vector or scalar: An argument providing the values of the covariate X_t.
#' @param order Positive integer: The order of the polynomial structure.
#' @param name String: An optional argument providing the name for this block. Can be useful to identify the models with meaningful labels, also, the name used will be used in some auxiliary functions.
#' @param D Array, Matrix, vector or scalar: The values for the discount factors associated with the latent states at each time. If D is an array, its dimensions should be n x n x t, where n is the order of the polynomial block and t is the length of the outcomes. If D is a matrix, its dimensions should be n x n and the same discount matrix will be used in all observations. If D is a vector, it should have size t and it is interpreted as the discount factor at each observed time (same discount for all variable). If D is a scalar, the same discount will be used for all latent states at all times.
#' @param h Matrix, vector or scalar: A drift to be add after the temporal evolution (can be interpreted as the mean of the random noise at each time). If a matrix, its dimension should be n x t, where n is the number of latent states (i.e., the order) and t is the length of the series. If a vector, it should have size t, and each value will be applied to the first latent state (the one which affects the linear predictors) in their respective time. If a scalar, the passed value will be used for the first latent state at each time.
#' @param H Array, Matrix, vector or scalar: The values for the covariance matrix for the noise factor at each time. If H is an array, its dimensions should be n x n x t, where n is the order of the polynomial block and t is the length of the series. If H is a matrix, its dimensions should be n x n and its values will be used for each time. If H is a vector or scalar, a discount factor matrix will be created as a diagonal matrix with the values of H in the diagonal.
#' @param a1 Vector or scalar: The prior mean for the latent states associated with this block at time 1. If a1 is a vector, its dimension should be equal to the order of the polynomial block. If a1 is a scalar, its value will be used for all latent states.
#' @param R1 Matrix, vector or scalar: The prior covariance matrix for the latent states associated with this block at time 1. If R1 is a matrix, its dimensions should be n x n. If R1 is a vector or scalar, a covariance matrix will be created as a diagonal matrix with the values of R1 in the diagonal.
#' @param monitoring Vector: A vector of flags indicating which variables should be monitored (if automated monitoring is used). Its size should be n. The default is that only the first order component of this structure should be monitored.
#'
#' @return A dlm_block object containing the following values:
#' \itemize{
#'    \item FF Array: A 3D-array containing the regression matrix for each time. Its dimension should be n x k x t, where n is the number of latent states, k is the number of linear predictors in the model and t is the time series length.
#'    \item FF.labs Matrix: A n x k character matrix describing the type of value of each element of FF.
#'    \item G Matrix: A 3D-array containing the evolution matrix for each time. Its dimension should be n x n x t, where n is the number of latent states and t is the time series length.
#'    \item G.labs Matrix: A n x n character matrix describing the type of value of each element of G.
#'    \item G.idx Matrix: A n x n character matrix containing the index each element of G.
#'    \item D Array: A 3D-array containing the discount factor matrix for each time. Its dimension should be n x n x t, where n is the number of latent states and t is the time series length.
#'    \item h Matrix: The mean for the random noise of the temporal evolution. Its dimension should be n x t.
#'    \item H Array: A 3D-array containing the covariance matrix of the noise for each time. Its dimension should be the same as D.
#'    \item a1 Vector: The prior mean for the latent vector.
#'    \item R1 Matrix: The prior covariance matrix for the latent vector.
#'    \item var.names list: A list containing the variables indexes by their name.
#'    \item order Positive integer: Same as argument.
#'    \item n Positive integer: The number of latent states associated with this block (same value as order).
#'    \item t Positive integer: The number of time steps associated with this block. If 1, the block is compatible with blocks of any time length, but if t is greater than 1, this block can only be used with blocks of the same time length.
#'    \item k Positive integer: The number of outcomes associated with this block. This block can only be used with blocks with the same outcome length.
#'    \item pred.names Vector: The name of the linear predictors associated with this block.
#'    \item monitoring Vector: Same as argument.
#'    \item type Character: The type of block (polynomial).
#' }
#'
#' @export
#' @examples
#' # Creating a first order structure for a model with 2 outcomes.
#' # One block is created for each outcome
#' # with each block being associated with only one of the outcomes.
#' level.1 <- polynomial_block(alpha1 = 1, order = 1)
#' level.2 <- polynomial_block(alpha2 = 1, order = 1)
#'
#' # Creating a block with shared effect between the outcomes
#' level.3 <- polynomial_block(alpha1 = 1, alpha2 = 1, order = 2)
#'
#' @details
#'
#' For the ..., D, H, a1 and R1 arguments, the user may set one or more of its values as a string.
#' By doing so, the user will leave the block partially undefined.
#' The user must then pass the undefined parameter values as named arguments to the \code{\link{fit_model}} function. Also, multiple values can be passed, allowing for a sensitivity analysis for the value of this parameter.
#'
#' For the details about the implementation see \insertCite{ArtigoPacote;textual}{kDGLM}.
#'
#' For the details about polynomial trend in the context of DLM's, see \insertCite{WestHarr-DLM;textual}{kDGLM}, chapter 7.
#'
#' For the details about dynamic regression models in the context of DLM's, see \insertCite{WestHarr-DLM;textual}{kDGLM}, chapters 6 and 9.
#'
#' @seealso \code{\link{fit_model}}
#' @family auxiliary functions for structural blocks
#'
#' @rdname polynomial_block
#'
#' @references
#'    \insertAllCited{}
polynomial_block <- function(..., order = 1, name = "Var.Poly",
                             D = 1, h = 0, H = 0,
                             a1 = 0, R1 = c(9, rep(1, order - 1)),
                             monitoring = c(TRUE, rep(FALSE, order - 1))) {
  if (length(H) == 1) {
    H <- c(H, rep(0, order - 1))
  }
  if (length(h) == 1) {
    h <- c(h, rep(0, order - 1))
  }

  block <- base_block(...,
    order = order, name = name,
    D = D, h = h, H = H,
    a1 = a1, R1 = R1,
    monitoring = monitoring
  )

  if (order == 2) {
    block$G[1, 2, ] <- 1
  } else if (order > 2) {
    for (i in 1:block$t) {
      diag(block$G[1:(order - 1), 2:order, i]) <- 1
    }
  }

  var.labs <- 1:order
  names(var.labs)[1] <- "Level"
  if (order > 1) {
    names(var.labs)[2] <- "Slope"
  }
  if (order >= 3) {
    names(var.labs)[3] <- "Curvature"
  }
  if (order > 3) {
    char.len <- floor(log10(order)) + 1
    names(var.labs)[4:order] <- paste0("Ord.", formatC(3:(order - 1), width = char.len, flag = "0"))
  }
  block$var.names <- list()
  block$var.names[[name]] <- var.labs
  return(block)
}


#' Structural blocks for seasonal trends and regressions
#'
#' Creates the structure for a harmonic block with desired periodicity.
#'
#'
#' @param ... Named values for the planning matrix.
#' @param X Vector or scalar: An argument providing the values of the covariate X_t.
#' @param period Positive integer: The size of the harmonic cycle.
#' @param order Positive integer: The order of the harmonic structure.
#' @param name String: An optional argument providing the name for this block. Can be useful to identify the models with meaningful labels, also, the name used will be used in some auxiliary functions.
#' @param D Array, Matrix, vector or scalar: The values for the discount factors associated with the latent states at each time. If D is an array, its dimensions should be (2n) x (2n) x t, where n is the order of the harmonic block and t is the length of the outcomes. If D is a matrix, its dimensions should be (2n) x (2n) and the same discount matrix will be used in all observations. If D is a vector, it should have size t and it is interpreted as the discount factor at each observed time (same discount for all variable). If D is a scalar, the same discount will be used for all latent states at all times.
#' @param h Matrix, vector or scalar: A drift to be add after the temporal evolution (can be interpreted as the mean of the random noise at each time). If a matrix, its dimension should be (2n) x t, where n is the order of the harmonic_block and t is the length of the series. If a vector, it should have size t, and each value will be applied to the first latent state (the one which affects the linear predictors) in their respective time. If a scalar, the passed value will be used for the first latent state at each time.
#' @param H Array, Matrix, vector or scalar: The values for the covariance matrix for the noise factor at each time. If H is an array, its dimensions should be (2n) x (2n) x t, where n is the order of the harmonic block and t is the length of the series. If H is a matrix, its dimensions should be (2n) x (2n) and its values will be used for each time. If H is a vector or scalar, a discount factor matrix will be created as a diagonal matrix with the values of H in the diagonal.
#' @param a1 Vector or scalar: The prior mean for the latent states associated with this block at time 1. If a1 is a vector, its dimension should be equal to two times the order of the harmonic block. If a1 is a scalar, its value will be used for all latent states.
#' @param R1 Matrix, vector or scalar: The prior covariance matrix for the latent states associated with this block at time 1. If R1 is a matrix, its dimensions should be (2n) x (2n). If R1 is a vector or scalar, a covariance matrix will be created as a diagonal matrix with the values of R1 in the diagonal.
#' @param monitoring Vector: A vector of flags indicating which variables should be monitored (if automated monitoring is used). Its size should be 2n. The default is that only the first order component of this structure should be monitored.
#'
#' @return A dlm_block object containing the following values:
#' \itemize{
#'    \item FF Array: A 3D-array containing the regression matrix for each time. Its dimension should be n x k x t, where n is the number of latent states, k is the number of linear predictors in the model and t is the time series length.
#'    \item FF.labs Matrix: A n x k character matrix describing the type of value of each element of FF.
#'    \item G Matrix: A 3D-array containing the evolution matrix for each time. Its dimension should be n x n x t, where n is the number of latent states and t is the time series length.
#'    \item G.labs Matrix: A n x n character matrix describing the type of value of each element of G.
#'    \item G.idx Matrix: A n x n character matrix containing the index each element of G.
#'    \item D Array: A 3D-array containing the discount factor matrix for each time. Its dimension should be n x n x t, where n is the number of latent states and t is the time series length.
#'    \item h Matrix: The mean for the random noise of the temporal evolution. Its dimension should be n x t.
#'    \item H Array: A 3D-array containing the covariance matrix of the noise for each time. Its dimension should be the same as D.
#'    \item a1 Vector: The prior mean for the latent vector.
#'    \item R1 Matrix: The prior covariance matrix for the latent vector.
#'    \item var.names list: A list containing the variables indexes by their name.
#'    \item period Positive integer: Same as argument.
#'    \item n Positive integer: The number of latent states associated with this block (2).
#'    \item t Positive integer: The number of time steps associated with this block. If 1, the block is compatible with blocks of any time length, but if t is greater than 1, this block can only be used with blocks of the same time length.
#'    \item k Positive integer: The number of outcomes associated with this block. This block can only be used with blocks with the same outcome length.
#'    \item pred.names Vector: The name of the linear predictors associated with this block.
#'    \item monitoring Vector: Same as argument.
#'    \item type Character: The type of block (Harmonic).
#' }
#'
#' @export
#' @examples
#' # Creating seasonal structure for a model with 2 outcomes.
#' # One block is created for each outcome
#' # with each block being associated with only one of the outcomes.
#' season.1 <- harmonic_block(alpha1 = 1, period = 3)
#' season.2 <- harmonic_block(alpha2 = 1, period = 6)
#'
#' # Creating a block with shared effect between the outcomes
#' season.3 <- harmonic_block(alpha = 1, alpha2 = 1, period = 12)
#'
#' @details
#'
#' For the ..., D, H, a1 and R1 arguments, the user may set one or more of its values as a string.
#' By doing so, the user will leave the block partially undefined.
#' The user must then pass the undefined parameter values as named arguments to the \code{\link{fit_model}} function. Also, multiple values can be passed, allowing for a sensitivity analysis for the value of this parameter.
#'
#' For the details about the implementation see \insertCite{ArtigoPacote;textual}{kDGLM}.
#'
#' For the details about the modelling of seasonal trends using harmonics in the context of DLM's, see \insertCite{WestHarr-DLM;textual}{kDGLM}, chapter 8.
#'
#' For the details about dynamic regression models in the context of DLM's, see \insertCite{WestHarr-DLM;textual}{kDGLM}, chapters 6 and 9.
#'
#' @seealso \code{\link{fit_model}}
#'
#' @family auxiliary functions for structural blocks
#'
#' @rdname harmonic_block
#'
#' @references
#'    \insertAllCited{}
harmonic_block <- function(..., period, order = 1, name = "Var.Sazo",
                           D = 1, h = 0, H = 0,
                           a1 = 0, R1 = 4,
                           monitoring = rep(FALSE, order * 2)) {
  if (order > period / 2 & order > 1) {
    stop("Error: order should be lesser or equal to period/2.")
  }
  w <- 2 * pi / period

  block <- base_block(..., order = 2 * order, name = name, D = D, h = h, H = H, a1 = a1, R1 = R1, monitoring = monitoring)
  G <- matrix(0, 2 * order, 2 * order)
  char.len <- floor(log10(order)) + 1

  G[1:2, 1:2] <- matrix(c(cos(w), -sin(w), sin(w), cos(w)), 2, 2)
  if (order == 1) {
    names(block$var.names[[name]]) <- c("Main", "Aux")
  } else {
    names(block$var.names[[name]])[1:2] <- c(paste0("Main.", formatC(1, width = char.len, flag = "0")), paste0("Aux.", formatC(1, width = char.len, flag = "0")))
    for (i in 2:order) {
      index <- 2 * i - 1
      names(block$var.names[[name]])[index:(index + 1)] <- c(
        paste0("Main.", formatC(i, width = char.len, flag = "0")),
        paste0("Aux.", formatC(i, width = char.len, flag = "0"))
      )

      block$FF[index, , ] <- block$FF[1, , ]
      block$FF.labs[index, ] <- block$FF.labs[1, ]
      G[index:(index + 1), index:(index + 1)] <- matrix(c(cos(w * i), -sin(w * i), sin(w * i), cos(w * i)), 2, 2)
    }
  }

  block$G <- array(G, c(2 * order, 2 * order, block$t))
  block$order <- order
  block$period <- period
  block$type <- "Harmonic"
  return(block)
}

#' Structural blocks for free-form seasonal trends and regressions
#'
#' Creates the structure for a free-form seasonal (FFS) block with desired periodicity.
#'
#'
#' @param ... Named values for the planning matrix.
#' @param X Vector or scalar: An argument providing the values of the covariate X_t.
#' @param sum.zero Bool: If true, all latent states will add to 0 and will have a correlated temporal evolution. If false, the first observation is considered the base line level and the states will represent the deviation from the baseline.
#' @param period Positive integer: The size of the seasonal cycle. This block has one latent state for each element of the cycle, such that the number of latent states n is equal to the period.
#' @param name String: An optional argument providing the name for this block. Can be useful to identify the models with meaningful labels, also, the name used will be used in some auxiliary functions.
#' @param D Vector or scalar: The values for the discount factors associated with the first latent state (the current effect) at each time. If D is a vector, it should have size t and it is interpreted as the discount factor at each observed time. If D is a scalar, the same discount will be used at all times.
#' @param h Vector or scalar: A drift to be add after the temporal evolution (can be interpreted as the mean of the random noise at each time). If a vector, it should have size t, and each value will be applied to the first latent state (the one which affects the linear predictors) in their respective time. If a scalar, the passed value will be used for the first latent state at each time.
#' @param H Vector or scalar: The values for the covariance matrix for the noise factor at each time. If a vector, it should have size t, and each value will  represent the variance of the temporal evolution at each time. If a scalar, the passed value will be used for the first latent state at each time.
#' @param a1 Vector or scalar: The prior mean for the latent states associated with this block at time 1. If a1 is a vector, its dimension should be equal to the period of the FFS block. If a1 is a scalar, its value will be used for all latent states.
#' @param R1 Matrix, vector or scalar: The prior covariance matrix for the latent states associated with this block at time 1. If R1 is a matrix, its dimensions should be period x period. If R1 is a vector or scalar, a covariance matrix will be created as a diagonal matrix with the values of R1 in the diagonal.
#' @param monitoring Bool: A indicator if the first latent state should be monitored (if automated monitoring is used).
#'
#' @return A dlm_block object containing the following values:
#' \itemize{
#'    \item FF Array: A 3D-array containing the regression matrix for each time. Its dimension should be n x k x t, where n is the number of latent states, k is the number of linear predictors in the model and t is the time series length.
#'    \item FF.labs Matrix: A n x k character matrix describing the type of value of each element of FF.
#'    \item G Matrix: A 3D-array containing the evolution matrix for each time. Its dimension should be n x n x t, where n is the number of latent states and t is the time series length.
#'    \item G.labs Matrix: A n x n character matrix describing the type of value of each element of G.
#'    \item G.idx Matrix: A n x n character matrix containing the index each element of G.
#'    \item D Array: A 3D-array containing the discount factor matrix for each time. Its dimension should be n x n x t, where n is the number of latent states and t is the time series length.
#'    \item h Matrix: The mean for the random noise of the temporal evolution. Its dimension should be n x t.
#'    \item H Array: A 3D-array containing the covariance matrix of the noise for each time. Its dimension should be the same as D.
#'    \item a1 Vector: The prior mean for the latent vector.
#'    \item R1 Matrix: The prior covariance matrix for the latent vector.
#'    \item var.names list: A list containing the variables indexes by their name.
#'    \item period Positive integer: Same as argument.
#'    \item n Positive integer: The number of latent states associated with this block (2).
#'    \item t Positive integer: The number of time steps associated with this block. If 1, the block is compatible with blocks of any time length, but if t is greater than 1, this block can only be used with blocks of the same time length.
#'    \item k Positive integer: The number of outcomes associated with this block. This block can only be used with blocks with the same outcome length.
#'    \item pred.names Vector: The name of the linear predictors associated with this block.
#'    \item monitoring Vector: Same as argument.
#'    \item type Character: The type of block (Harmonic).
#' }
#'
#' @export
#' @examples
#' # Creating a first order structure for a model with 2 outcomes.
#' # One block is created for each outcome
#' # with each block being associated with only one of the outcomes.
#' season.1 <- ffs_block(alpha1 = 1, period = 12)
#' season.2 <- ffs_block(alpha2 = 1, period = 12)
#'
#' # Creating a block with shared effect between the outcomes
#' season.3 <- ffs_block(alpha1 = 1, alpha2 = 1, period = 12)
#'
#' @details
#'
#' For the ..., D, H, a1 and R1 arguments, the user may set one or more of its values as a string.
#' By doing so, the user will leave the block partially undefined.
#' The user must then pass the undefined parameter values as named arguments to the \code{\link{fit_model}} function. Also, multiple values can be passed, allowing for a sensitivity analysis for the value of this parameter.
#'
#' For the details about the implementation see \insertCite{ArtigoPacote;textual}{kDGLM}.
#'
#' For the details about the free-form seasonal trends in the context of DLM's, see \insertCite{WestHarr-DLM;textual}{kDGLM}, chapter 8.
#'
#' For the details about dynamic regression models in the context of DLM's, see \insertCite{WestHarr-DLM;textual}{kDGLM}, chapters 6 and 9.
#'
#' @seealso \code{\link{fit_model}}
#'
#' @family auxiliary functions for structural blocks
#'
#' @rdname ffs_block
#'
#' @references
#'    \insertAllCited{}
ffs_block <- function(..., period, sum.zero = FALSE, name = "Var.FFS",
                      D = 1, h = 0, H = 0,
                      a1 = 0, R1 = 4,
                      monitoring = FALSE) {
  if (period == 1) {
    stop("The seasonal cycle must have at least size 2.")
  }
  if (length(a1) == 1) {
    a1 <- rep(a1, period)
  }
  if (length(R1) == 1) {
    R1 <- diag(period) * R1
  }

  block.state <- base_block(..., order = 1, name = name, D = D, h = h, H = H, a1 = 0, R1 = 1, monitoring = monitoring)

  dummy.var <- list()
  dummy.var[[names(list(...))[1]]] <- rep(0, block.state$t)

  block.aux <-
    do.call(
      function(...) {
        base_block(..., order = period - 1, name = name, a1 = 0, R1 = 1, D = 1, h = 0, H = 0, monitoring = rep(FALSE, period - 1))
      },
      dummy.var
    )

  block <- (block.state + block.aux)
  block$a1 <- a1
  block$R1 <- R1
  if (sum.zero) {
    block$D[, , ] <- D
    block <- block |> zero_sum_prior()
  } else {
    A <- diag(period)
    A[, 1] <- A[, 1] - 1
    R1 <- diag(7) * 9
    block$a1 <- A %*% block$a1
    block$R1 <- A %*% block$R1 %*% t(A)
  }

  G <- matrix(0, period, period)
  G[-period, -1] <- diag(period - 1)
  G[period, 1] <- 1
  char.len <- floor(log10(period)) + 1

  block$var.names <- list()
  block$var.names[[name]] <- 1:period
  names(block$var.names[[name]]) <- paste0("Lag.", formatC(0:(period - 1), width = char.len, flag = "0"))

  block$G <- array(G, c(period, period, block$t))
  block$period <- period
  block$type <- "Free-from seasonality"
  return(block)
}

#' Structural blocks for regressions
#'
#' Creates a block for a (dynamic) regression for a covariate X_t.
#'
#' @param ... Named values for the planning matrix.
#' @param X Vector or scalar: An argument providing the values of the covariate X_t.
#' @param max.lag Non-negative integer: An optional argument providing the maximum lag for the explanatory variables. If a positive value is provided, this block will create additional latent states to measure the lagged effect of X_t up until the given value. See \insertCite{WestHarr-DLM;textual}{kDGLM}, subsection 9.2.2 item (3).
#' @param zero.fill boolean: A Boolean indicating if the block should fill the initial delay values with 0's. If TRUE and max.lag is positive, the block assumes that X_t=0 for all t<1. If FALSE, the block assumes the user will provide X_t for all t, such that X_t will have size t+propagation_size
#' @param name String: An optional argument providing the name for this block. Can be useful to identify the models with meaningful labels, also, the name used will be used in some auxiliary functions.
#' @param D Array, Matrix, vector or scalar: The values for the discount factors at each time. If D is a array, its dimensions should be n x n x t, where n is the order of the polynomial block and t is the length of the outcomes. If D is a matrix, its dimensions should be n x n and its values will be used for each time. If D is a vector or scalar, a discount factor matrix will be created as a diagonal matrix with the values of D in the diagonal.
#' @param h Matrix, vector or scalar: A drift to be add after the temporal evolution (can be interpreted as the mean of the random noise at each time). If a matrix, its dimension should be 2 x t, where t is the length of the series. If a vector, it should have size t, and each value will be applied to the first latent state (the one which affects the linear predictors) in their respective time. If a scalar, the passed value will be used for the first latent state at each time.
#' @param H Array, Matrix, vector or scalar: The values for the covariance matrix for the noise factor at each time. If H is a array, its dimensions should be n x n x t, where n is the order of the polynomial block and t is the length of the outcomes. If H is a matrix, its dimensions should be n x n and its values will be used for each time. If H is a vector or scalar, a discount factor matrix will be created as a diagonal matrix with the values of H in the diagonal.
#' @param a1 Vector or scalar: The prior mean for the latent states associated with this block at time 1. If a1 is a vector, its dimension should be equal to the order of the polynomial block. If a1 is a scalar, its value will be used for all latent states.
#' @param R1 Matrix, vector or scalar: The prior covariance matrix for the latent states associated with this block at time 1. If R1 is a matrix, its dimensions should be n x n. If R1 is a vector or scalar, a covariance matrix will be created as a diagonal matrix with the values of R1 in the diagonal.
#' @param monitoring Vector: A vector of flags indicating which variables should be monitored (if automated monitoring is used). Its size should be n. The default is that no variable should be monitored.
#'
#' @return A dlm_block object containing the following values:
#' \itemize{
#'    \item FF Array: A 3D-array containing the regression matrix for each time. Its dimension should be n x k x t, where n is the number of latent states, k is the number of linear predictors in the model and t is the time series length.
#'    \item FF.labs Matrix: A n x k character matrix describing the type of value of each element of FF.
#'    \item G Matrix: A 3D-array containing the evolution matrix for each time. Its dimension should be n x n x t, where n is the number of latent states and t is the time series length.
#'    \item G.labs Matrix: A n x n character matrix describing the type of value of each element of G.
#'    \item G.idx Matrix: A n x n character matrix containing the index each element of G.
#'    \item D Array: A 3D-array containing the discount factor matrix for each time. Its dimension should be n x n x t, where n is the number of latent states and t is the time series length.
#'    \item h Matrix: The mean for the random noise of the temporal evolution. Its dimension should be n x t.
#'    \item H Array: A 3D-array containing the covariance matrix of the noise for each time. Its dimension should be the same as D.
#'    \item a1 Vector: The prior mean for the latent vector.
#'    \item R1 Matrix: The prior covariance matrix for the latent vector.
#'    \item var.names list: A list containing the variables indexes by their name.
#'    \item max.lag Positive integer: Same as argument.
#'    \item n Positive integer: The number of latent states associated with this block (2).
#'    \item t Positive integer: The number of time steps associated with this block. If 1, the block is compatible with blocks of any time length, but if t is greater than 1, this block can only be used with blocks of the same time length.
#'    \item k Positive integer: The number of outcomes associated with this block. This block can only be used with blocks with the same outcome length.
#'    \item pred.names Vector: The name of the linear predictors associated with this block.
#'    \item monitoring Vector: Same as argument.
#'    \item type Character: The type of block (Harmonic).
#' }
#'
#' @export
#' @examples
#'
#' structure <- (
#'   polynomial_block(p = 1, order = 2, D = 0.95) +
#'     harmonic_block(p = 1, period = 12, D = 0.95) +
#'     regression_block(p = chickenPox$date >= as.Date("2013-09-01"))
#'   # Vaccine was introduced in September of 2013
#' ) * 4
#'
#' outcome <- Multinom(p = structure$pred.names, data = chickenPox[, c(2, 3, 4, 6, 5)])
#' fitted.data <- fit_model(structure, chickenPox = outcome)
#' summary(fitted.data)
#' plot(coef(fitted.data), plot.pkg = "base")
#'
#' @details
#'
#' For the ..., D, H, a1 and R1 arguments, the user may set one or more of its values as a string.
#' By doing so, the user will leave the block partially undefined.
#' The user must then pass the undefined parameter values as named arguments to the \code{\link{fit_model}} function. Also, multiple values can be passed, allowing for a sensitivity analysis for the value of this parameter.
#'
#' For the details about the implementation see \insertCite{ArtigoPacote;textual}{kDGLM}.
#'
#' For the details about dynamic regression models in the context of DLM's, see \insertCite{WestHarr-DLM;textual}{kDGLM}, chapters 6 and 9.
#'
#' @seealso \code{\link{fit_model}}
#'
#' @rdname regression_block
#' @family auxiliary functions for structural blocks
#'
#'
#' @references
#'    \insertAllCited{}
regression_block <- function(..., max.lag = 0, zero.fill = TRUE, name = "Var.Reg",
                             D = 1, h = 0, H = 0,
                             a1 = 0, R1 = 9,
                             monitoring = rep(FALSE, max.lag + 1)) {
  order <- max.lag + 1
  block <- base_block(..., order = order, name = name, D = D, h = h, H = H, a1 = a1, R1 = R1, monitoring = monitoring)

  G <- diag(max.lag + 1)
  block$G <- array(G, c(order, order, block$t))
  block$order <- NULL
  block$max.lag <- max.lag
  block$zero.fill <- zero.fill
  block$type <- "Regression"
  char.len <- floor(log10(order)) + 1

  names(block$var.names[[name]]) <- paste0("Lag.", formatC(0:(order - 1), width = char.len, flag = "0"))

  t <- block$t
  if (max.lag > 0) {
    for (i in 1:max.lag) {
      block$FF[i + 1, , -1] <- block$FF[i, , -t]
    }
  }
  if (!zero.fill & t > 1) {
    block$FF <- block$FF[, , (max.lag + 1):t, drop = FALSE]
    block$G <- block$G[, , (max.lag + 1):t, drop = FALSE]
    block$D <- block$D[, , (max.lag + 1):t, drop = FALSE]
    block$h <- block$h[, (max.lag + 1):t, drop = FALSE]
    block$H <- block$H[, , (max.lag + 1):t, drop = FALSE]
    block$t <- t - max.lag
  }

  return(block)
}

#' Structural blocks for auto regressive trends and regressions
#'
#' Creates the structure for a Auto Regressive (AR) block (see \insertCite{WestHarr-DLM;textual}{kDGLM}, chapter 9) with desired order.
#' As the package suppose that the structure of the model is linear, a linearization is applied to the evolution equation, as described in \insertCite{WestHarr-DLM;textual}{kDGLM}, chapter 13.
#' This block also supports Transfer Functions, being necessary to specify the associated pulse when calling the TF_block function (see arg.).
#'
#' @param ... Named values for the planning matrix.
#' @param X Vector or scalar: An argument providing the values of the covariate X_t.
#' @param order Positive integer: The order of the AR block.
#' @param noise.var Non-negative scalar: The variance of the white noise added to the latent state.
#' @param noise.disc Vector or scalar: The value for the discount factor associated with the current latent state. If noise.disc is a vector, it should have size t and it is interpreted as the discount factor at each observed time. If D is a scalar, the same discount will be used for all observation.
#' @param pulse Vector or scalar: An optional argument providing the values for the pulse for a Transfer Function. Default is 0 (no Transfer Function).
#' @param X Vector or scalar: An argument providing the values for the pulse for a Transfer Function.
#' @param name String: An optional argument providing the name for this block. Can be useful to identify the models with meaningful labels, also, the name used will be used in some auxiliary functions.
#' @param AR.support String: Either "constrained" or "free" (default). If AR.support is "constrained", then the AR coefficients will be forced to be on the interval (-1,1), otherwise, the coefficients will be unrestricted. Beware that, under no restriction on the coefficients, there is no guarantee that the estimated coefficients will imply in a stationary process, furthermore, if the order of the AR block is greater than 1. As such the restriction of the coefficients support is only available for AR blocks with order equal to 1.
#' @param h Vector or scalar: A drift to be add in the states after the temporal evolution (can be interpreted as the mean of the random noise at each time). If a vector, it should have size t, and each value will be applied in their respective time. If a scalar, the passed value will be used for all observations.
#' @param a1 Vector or scalar: The prior mean for the states associated with this block at time 1. If a1 is a vector, its dimension should be equal to the order of the AR block. If a1 is a scalar, its value will be used for all coefficients.
#' @param R1 Matrix, vector or scalar: The prior covariance matrix for the states associated with this block at time 1. If R1 is a matrix, its dimensions should be n x n, where n is the order of the AR block. If R1 is a vector or scalar, a covariance matrix will be created as a diagonal matrix with the values of R1 in the diagonal.
#' @param monitoring bool: A flag indicating if the latent state should be monitored (if automated monitoring is used). The default is TRUE.
#' @param multi.states bool: If FALSE (default) a single latent state will be created affecting all linear predictor and being affected by all pulses. If TRUE, each linear predictor will have its own latent state, but all latent states will share the same AR coefficients and all pulse effects (each state will have its own pulse though).
#' @param D.coef Array, Matrix, vector or scalar: The values for the discount factors associated with the AR coefficients at each time. If D.coef is an array, its dimensions should be n x n x t, where n is the order of the AR block and t is the length of the outcomes. If D.coef is a matrix, its dimensions should be n x n and the same discount matrix will be used in all observations. If D.coef is a vector, it should have size t and it is interpreted as the discount factor at each observed time (same discount for all variable). If D.coef is a scalar, the same discount will be used for all AR coefficients at all times.
#' @param h.coef Matrix, vector or scalar: A drift to be add in the AR coefficients after the temporal evolution (can be interpreted as the mean of the random noise at each time). If a matrix, its dimension should be n x t, where n is the order of the AR block and t is the length of the series. If a scalar, the passed value will be used for all coefficients at each time.
#' @param H.coef Array, Matrix, vector or scalar: The values for the covariance matrix for the noise factor associated with the AR coefficients at each time. If H.coef is a array, its dimensions should be n x n x t, where n is the order of the AR block and t is the length of the outcomes. If H.coef is a matrix, its dimensions should be n x n and its values will be used for each time. If H.coef is a vector or scalar, a discount factor matrix will be created as a diagonal matrix with the values of H.coef in the diagonal.
#' @param a1.coef Vector or scalar: The prior mean for the AR coefficients associated with this block at time 1. If a1.coef is a vector, its dimension should be equal to the order of the AR block. If a1.coef is a scalar, its value will be used for all coefficients. If the coefficients are restricted to the interval (-1,1), the a1.coef is interpreted as the mean for atanh(rho), where rho is the AR coefficient.
#' @param R1.coef Matrix, vector or scalar: The prior covariance matrix for the coefficients associated with this block at time 1. If R1.coef is a matrix, its dimensions should be n x n, where n is the order of the AR block. If R1.coef is a vector or scalar, a covariance matrix will be created as a diagonal matrix with the values of R1.coef in the diagonal. If the coefficients are restricted to the interval (-1,1), the R1.coef is interpreted as the covariance matrix for atanh(rho), where rho is the AR coefficient.
#' @param monitoring.coef Vector: A vector of flags indicating which AR coefficients should be monitored (if automated monitoring is used). Its size should be n, where n is the order of the AR block. The default is that no coefficient should be monitored.
#' @param D.pulse Array, Matrix, vector or scalar: The values for the discount factors associated with the pulse coefficients at each time. If D.pulse is an array, its dimensions should be n x n x t, where n is the number of pulses and t is the length of the outcomes. If D.pulse is a matrix, its dimensions should be n x n and the same discount matrix will be used in all observations. If D.pulse is a vector, it should have size t and it is interpreted as the discount factor at each observed time (same discount for all variable). If D is a scalar, the same discount will be used for all pulse coefficients at all times.
#' @param h.pulse Matrix, vector or scalar: A drift to be add in the pulse effect after the temporal evolution (can be interpreted as the mean of the random noise at each time). If a matrix, its dimension should be n x t, where n is the number of pulses and t is the length of the series. If a scalar, the passed value will be used for all latent state at each time.
#' @param H.pulse Array, Matrix, vector or scalar: The values for the covariance matrix for the noise factor associated with pulse coefficients at each time. If H.pulse is an array, its dimensions should be n x n x t, where n is the number of pulses and t is the length of the outcomes. If H.pulse is a matrix, its dimensions should be n x n and its values will be used for each time. If H.pulse is a vector or scalar, a covariance matrix will be created as a diagonal matrix with the values of H.pulse in the diagonal.
#' @param a1.pulse Vector or scalar: The prior mean for the coefficients associated with the pulses at time 1. If a1.pulse is a vector, its dimension should be equal to the number of pulses. If a1.pulse is a scalar, its value will be used for all coefficients.
#' @param R1.pulse Matrix, vector or scalar: The prior covariance matrix for the coefficients associated with the pulses at time 1. If R1.pulse is a matrix, its dimensions should be n x n, where n is the number of pulses. If R1.pulse is a vector or scalar, a covariance matrix will be created as a diagonal matrix with the values of R1.pulse in the diagonal.
#' @param monitoring.pulse Vector: A vector of flags indicating which pulse coefficients should be monitored (if automated monitoring is used). Its size should be n, where n is the number of pulses. The default is that no pulse coefficient should be monitored.
#'
#' @return A dlm_block object containing the following values:
#' \itemize{
#'    \item FF Array: A 3D-array containing the regression matrix for each time. Its dimension should be n x k x t, where n is the number of latent states, k is the number of linear predictors in the model and t is the time series length.
#'    \item FF.labs Matrix: A n x k character matrix describing the type of value of each element of FF.
#'    \item G Matrix: A 3D-array containing the evolution matrix for each time. Its dimension should be n x n x t, where n is the number of latent states and t is the time series length.
#'    \item G.labs Matrix: A n x n character matrix describing the type of value of each element of G.
#'    \item G.idx Matrix: A n x n character matrix containing the index each element of G.
#'    \item D Array: A 3D-array containing the discount factor matrix for each time. Its dimension should be n x n x t, where n is the number of latent states and t is the time series length.
#'    \item H Array: A 3D-array containing the covariance matrix of the noise for each time. Its dimension should be the same as D.
#'    \item a1 Vector: The prior mean for the latent vector.
#'    \item R1 Matrix: The prior covariance matrix for the latent vector.
#'    \item var.names list: A list containing the variables indexes by their name.
#'    \item order Positive integer: Same as argument.
#'    \item n Positive integer: The number of latent states associated with this block (2).
#'    \item t Positive integer: The number of time steps associated with this block. If 1, the block is compatible with blocks of any time length, but if t is greater than 1, this block can only be used with blocks of the same time length.
#'    \item k Positive integer: The number of outcomes associated with this block. This block can only be used with blocks with the same outcome length.
#'    \item pred.names Vector: The name of the linear predictors associated with this block.
#'    \item monitoring Vector: The combination of monitoring, monitoring and monitoring.pulse.
#'    \item type Character: The type of block (AR).
#'    \item AR.support Character: Same as argument.
#' }
#'
#' @export
#' @examples
#'
#' #### AR block ####
#' TF_block(mu = 1, order = 2, noise.disc = 0.9)
#'
#' #### Transfer function ####
#' TF_block(mu = 1, pulse = beaver1$activ, order = 1, noise.disc = 0.9)
#'
#' @details
#'
#' For the ..., noise.var, noise.disc, D, H, a1, R1, a1, R1, a1.pulse, R1.pulse, D.pulse, h.pulse, H.pulse arguments, the user may set one or more of its values as a string.
#' By doing so, the user will leave the block partially undefined.
#' The user must then pass the undefined parameter values as named arguments to the \code{\link{fit_model}} function. Also, multiple values can be passed, allowing for a sensitivity analysis for the value of this parameter.
#'
#' For the details about the implementation see \insertCite{ArtigoPacote;textual}{kDGLM}.
#'
#' For the details about Auto regressive models in the context of DLM's, see \insertCite{WestHarr-DLM;textual}{kDGLM}, chapter 9.
#'
#' For the details about the linearization of non-linear evolution equations in the context of DLM's, see \insertCite{WestHarr-DLM;textual}{kDGLM}, chapter 13.
#'
#' For the details about dynamic regression models in the context of DLM's, see \insertCite{WestHarr-DLM;textual}{kDGLM}, chapters 6 and 9.
#'
#' @seealso \code{\link{fit_model}}
#'
#' @family auxiliary functions for structural blocks
#'
#' @rdname tf_block
#'
#' @references
#'    \insertAllCited{}
TF_block <- function(..., order, noise.var = NULL, noise.disc = NULL, pulse = 0, name = "Var.AR", AR.support = "free",
                     h = 0, a1 = 0, R1 = 4, monitoring = TRUE,
                     multi.states = FALSE,
                     D.coef = 1, h.coef = 0, H.coef = 0, a1.coef = c(1, rep(0, order - 1)), R1.coef = c(1, rep(0.25, order - 1)), monitoring.coef = rep(FALSE, order),
                     D.pulse = 1, h.pulse = 0, H.pulse = 0, a1.pulse = 0, R1.pulse = 4, monitoring.pulse = FALSE) {
  if (AR.support == "constrained" & order > 1) {
    warning("Restrictions on the AR coefficients are only available for first order models. Ignoring AR.support argument.")
    AR.support <- "free"
  }

  if (is.null(noise.var) & is.null(noise.disc)) {
    stop("Error: Must specify at least one of noise.var or noise.disc.")
  }

  noise.var <- if.null(noise.var, 0)
  noise.disc <- if.null(noise.disc, 1)

  ts <- c(length(noise.var), length(noise.disc), length(h))
  t <- max(ts)
  if (any(!(ts %in% c(1, t)))) {
    stop(paste0("Error: Incompatible length between noise.var, noise.disc and h. Expected values to be 1 or equal, got", ts, ".", collapse = ", "))
  }

  if (multi.states) {
    order.states <- length(list(...))
    if (order.states == 1) {
      stop("multi.states is TRUE, but only one linear predictor was provided.")
    }
  } else {
    order.states <- 1
  }

  h.placeholder <- h
  h <- matrix(0, order * order.states, t)
  h[1, ] <- h.placeholder
  H <- array(0, c(order * order.states, order * order.states, t))
  H[1, 1, ] <- noise.var
  D <- array(1, c(order * order.states, order * order.states, t))
  D[1, 1, ] <- noise.disc
  block.state <-
    base_block(..., order = order * order.states, name = paste0(name, ".State"), a1 = a1, R1 = R1, D = D, h = h, H = H, monitoring = rep(c(monitoring, rep(FALSE, order - 1)), order.states))

  if (multi.states) {
    block.state$FF[(1:order.states - 1) * order + 1, , ] <-
      apply(block.state$FF[1, , , drop = FALSE], 3, function(x) {
        diag(c(x), nrow = order.states)
      }) |>
      array(c(order.states, order.states, dim(block.state$FF)[3]))
  }

  dummy.var <- list()
  dummy.var[[names(list(...))[1]]] <- rep(0, block.state$t)

  block.coeff <-
    do.call(
      function(...) {
        base_block(..., order = order, name = paste0(name, ".Coef"), a1 = a1.coef, R1 = R1.coef, D = D.coef, h = h.coef, H = H.coef, monitoring = monitoring.coef)
      },
      dummy.var
    )

  block <- block.state + block.coeff
  names(block$var.names[[1]]) <- paste0(sapply(block$pred.names, function(x) {
    rep(x, order)
  }), ".", rep(paste0("Lag.", 0:(order - 1)), order.states))

  names(block$var.names[[2]]) <- paste0("Lag.", 0:(order - 1))


  block$a1 <- block$a1
  block$R1 <- block$R1
  block$D <- block$D
  block$h <- block$h
  block$H <- block$H

  k <- block$k
  n.param <- (order.states + 1) * order
  G <- matrix(0, n.param, n.param)
  G.labs <- matrix("const", n.param, n.param)
  G.idx <- matrix(NA, n.param, n.param)
  for (i in 1:order.states) {
    i <- (i - 1) * order + 1
    G[i, i + (1:order - 1)] <- NA
    G.labs[i, i + (1:order - 1)] <- tolower(AR.support)
    G.idx[i, i + (1:order - 1)] <- order.states * order + (1:order)
    if (order > 1) {
      G[i + (2:order - 1), i + (2:order - 2)] <- diag(order - 1)
    }
  }
  G[order.states * order + (1:order), order.states * order + (1:order)] <- diag(order)

  block$G <- array(G, c(n.param, n.param, block$t))
  block$G.labs <- G.labs
  block$G.idx <- G.idx
  block$order <- order
  block$type <- "AR"
  block$AR.support <- AR.support

  if (any(pulse != 0)) {
    pulse <- as.matrix(pulse)
    if (multi.states) {
      if (if.null(dim(pulse)[2], 1) != order.states) {
        stop("Number of linear predictors is different from the number of pulses.")
      }
      k <- 1
    } else {
      k <- if.null(dim(pulse)[2], 1)
    }
    t <- if.null(dim(pulse)[1], length(pulse))
    dummy.var <- list()
    dummy.var[[names(list(...))[1]]] <- rep(0, t)
    if (length(h.pulse) > 1 & length(dim(h.pulse)) < 2) {
      h.pulse <- matrix(h.pulse, order, length(h.pulse))
    }
    if (length(monitoring.pulse) == 1) {
      monitoring.pulse <- rep(FALSE, k)
    }
    block.pulse <-
      do.call(
        function(...) {
          base_block(..., order = k, name = paste0(name, ".Pulse.effect"), a1 = a1.pulse, R1 = R1.pulse, D = D.pulse, h = h.pulse, H = H.pulse, monitoring = monitoring.pulse)
        },
        dummy.var
      )

    names(block.pulse$var.names[[paste0(name, ".Pulse.effect")]]) <- paste0(1:k)

    block.pulse$G <- diag(k)
    block <- block + block.pulse
    if (multi.states) {
      pulse <- t(matrix(pulse, block$t, order.states))
      for (i in 1:order.states) {
        idx <- 1 + (i - 1) * order
        block$G[idx, n.param + 1, ] <- pulse[i, ]
        block$G.labs[idx, n.param + 1] <- "Pulse"
      }
    } else {
      block$G[1, (n.param + 1):(n.param + k), ] <- t(matrix(pulse, block$t, k))
      block$G.labs[1, (n.param + 1):(n.param + k)] <- "Pulse"
    }
  }
  return(block)
}

#' noise_block
#'
#' Creates the structure for a Noise block. This block represents an independent random noise that should be added to the linear predictor.
#' The variance of the noise cannot be formally estimated, as such we use a discount strategy similar to that of \insertCite{WestHarr-DLM;textual}{kDGLM} to specify it.
#'
#' @param ... Named values for the planning matrix.
#' @param X Vector or scalar: An argument providing the values of the covariate X_t.
#' @param name String: An optional argument providing the name for this block. Can be useful to identify the models with meaningful labels, also, the name used will be used in some auxiliary functions.
#' @param D scalar or vector: A sequence of values specifying the desired discount factor for each time. It should have length 1 or t, where t is the size of the series. If both D and H are specified, the value of D is ignored.
#' @param R1 scalar: The prior variance of the noise.
#' @param H scalar: The variance of the noise. If both D and H are specified, the value of D is ignored.
#'
#' @return A dlm_block object containing the following values:
#' \itemize{
#'    \item FF Array: A 3D-array containing the regression matrix for each time. Its dimension should be n x k x t, where n is the number of latent states, k is the number of linear predictors in the model and t is the time series length.
#'    \item FF.labs Matrix: A n x k character matrix describing the type of value of each element of FF.
#'    \item G Matrix: A 3D-array containing the evolution matrix for each time. Its dimension should be n x n x t, where n is the number of latent states and t is the time series length.
#'    \item G.labs Matrix: A n x n character matrix describing the type of value of each element of G.
#'    \item D Array: A 3D-array containing the discount factor matrix for each time. Its dimension should be n x n x t, where n is the number of latent states and t is the time series length.
#'    \item H Array: A 3D-array containing the covariance matrix of the noise for each time. Its dimension should be the same as D.
#'    \item a1 Vector: The prior mean for the latent vector.
#'    \item R1 Matrix: The prior covariance matrix for the latent vector.
#'    \item var.names list: A list containing the variables indexes by their name.
#'    \item order Positive integer: Same as argument.
#'    \item n Positive integer: The number of latent states associated with this block (2).
#'    \item t Positive integer: The number of time steps associated with this block. If 1, the block is compatible with blocks of any time length, but if t is greater than 1, this block can only be used with blocks of the same time length.
#'    \item k Positive integer: The number of outcomes associated with this block. This block can only be used with blocks with the same outcome length.
#'    \item pred.names Vector: The name of the linear predictors associated with this block.
#'    \item monitoring Vector: The combination of monitoring, monitoring and monitoring.pulse.
#'    \item type Character: The type of block (Noise).
#' }
#'
#' @export
#' @examples
#'
#' noise_block(mu = 1, D = 0.99, R1 = 1e-2)
#'
#' @details
#'
#' For the details about the implementation see \insertCite{ArtigoPacote;textual}{kDGLM}.
#'
#' For the details about dynamic regression models in the context of DLMs, see \insertCite{WestHarr-DLM;textual}{kDGLM}, chapters 6 and 9.
#'
#' @seealso \code{\link{fit_model}}
#'
#' @rdname noise_block
#'
#' @family auxiliary functions for structural blocks
#'
#' @references
#'    \insertAllCited{}
noise_block <- function(..., name = "Noise",
                        D = 0.99, R1 = 0.1,
                        H = 0) {
  labs <- "noise.disc"
  if (any(H > 0) | is.character(H)) {
    if (D > 1) {
      warning("H is non-zero, ignoring the discount factor.")
    }
    D <- 1
    R1 <- H
    labs <- "noise"
  }
  block <- base_block(...,
    order = 1, name = name,
    D = D, h = 0, H = H,
    a1 = 0, R1 = R1,
    monitoring = FALSE
  )

  block$G[, , ] <- 0
  block$G.labs[, ] <- labs

  var.labs <- c(1)
  names(var.labs) <- "Var"
  block$var.names <- list()
  block$var.names[[name]] <- var.labs
  block$type <- "Noise"
  return(block)
}

#' Auxiliary function for block superposition
#'
#' An auxiliary function for block superposition.
#'
#' @param ... dlm_block: A sequence of block to be combine.
#'
#' @return The combined blocks as a dlm_block.
#' @export
#'
#' @examples
#'
#' # Long way
#' level.1 <- polynomial_block(alpha1 = 1, order = 1)
#' level.2 <- polynomial_block(alpha2 = 1, order = 2)
#' season.2 <- harmonic_block(alpha2 = 1, period = 20)
#'
#' final.block <- block_superpos(level.1, level.2, season.2)
#'
#' # Short way
#' final.block <- polynomial_block(alpha1 = 1, order = 1) +
#'   polynomial_block(alpha2 = 1, order = 2) +
#'   harmonic_block(alpha2 = 1, period = 20)
#'
#' @details
#' Additional details can be found in \insertCite{WestHarr-DLM;textual}{kDGLM}, section 6.2.
#'
#'
#' @family auxiliary functions for structural blocks
#'
#' @references
#'    \insertAllCited{}
block_superpos <- function(...) {
  blocks <- list(...)
  if (length(blocks) == 1) {
    return(blocks[[1]])
  }
  for (block in blocks) {
    if (!inherits(block, "dlm_block")) {
      stop(paste0("Error: Expected all arguments to be dlm_block's, but got a ", class(block), "."))
    }
  }

  n <- 0
  t <- 1
  k <- 1

  var.names <- list()
  names <- c()
  pred.names <- c()
  for (block in blocks) {
    ref.names <- block$var.names
    count.labs <- 0
    for (name in names(ref.names)) {
      count.labs <- count.labs + 1
      names <- append(names, name)
      var.names <- append(var.names, list(ref.names[[count.labs]] + n))
    }
    pred.names <- c(pred.names, block$pred.names)
    if (block$t > 1) {
      if (block$t != t & t > 1) {
        stop(paste("Error: Blocks should have same length or length equal 1. Got", block$t, "and", t))
      }
      t <- block$t
    }
    n <- n + block$n
  }
  names(var.names) <- names

  pred.names <- sort(unique(pred.names))
  k <- length(pred.names)

  FF <- array(0, c(n, k, t), dimnames = list(NULL, pred.names, NULL))
  FF.labs <- matrix("const", n, k, dimnames = list(NULL, pred.names))
  G <- array(0, c(n, n, t))
  G.labs <- matrix("const", n, n)
  G.idx <- matrix(NA, n, n)
  D <- array(0, c(n, n, t))
  h <- matrix(0, n, t)
  H <- array(0, c(n, n, t))
  a1 <- c()
  R1 <- matrix(0, n, n)
  period <- 1
  position <- 1
  monitoring <- c()
  scale <- c()
  direction <- diag(0, n, n)
  interventions <- list()
  status <- "defined"
  for (block in blocks) {
    k_i <- length(block$pred.names)
    current.range <- position:(position + block$n - 1)
    FF[current.range, pred.names %in% block$pred.names, ] <- block$FF[, (1:k_i)[order(block$pred.names)], ]
    FF.labs[current.range, pred.names %in% block$pred.names] <- block$FF.labs[, (1:k_i)[order(block$pred.names)]]

    G[current.range, current.range, ] <- block$G
    G.labs[current.range, current.range] <- block$G.labs
    G.idx[current.range, current.range] <- block$G.idx + position - 1
    D[current.range, current.range, ] <- block$D
    h[current.range, ] <- block$h
    H[current.range, current.range, ] <- block$H
    a1 <- c(a1, block$a1)
    R1[current.range, current.range] <- block$R1
    monitoring <- append(monitoring, block$monitoring)
    scale <- c(scale, block$scale)
    direction[current.range, current.range] <- block$direction
    if (length(block$interventions) > 0) {
      for (i in 1:length(block$interventions)) {
        block$interventions[[i]]$var.index <- block$interventions[[i]]$var.index + position - 1
      }
    }
    position <- position + block$n
    period <- lcm(c(period, if.null(block$period, 1)))
    interventions <- append(interventions, block$interventions)
  }
  block <- list(
    "FF" = FF,
    "FF.labs" = FF.labs,
    "G" = G,
    "G.labs" = G.labs,
    "G.idx" = G.idx,
    "D" = D,
    "h" = h,
    "H" = H,
    "a1" = a1,
    "R1" = R1,
    "n" = n,
    "t" = t,
    "k" = k,
    "status" = status,
    "period" = period,
    "var.names" = var.names,
    "name" = names,
    "pred.names" = pred.names,
    "monitoring" = monitoring,
    "interventions" = interventions,
    "type" = "Mixed",
    "scale" = scale,
    "direction" = direction
  )
  class(block) <- "dlm_block"
  block$status <- check.block.status(block)
  return(block)
}

#' Auxiliary function to replicate blocks
#'
#' An auxiliary function to replicate blocks.
#'
#' @param block dlm_block: A block to be replicated
#' @param k Integer: The number of blocks to generate.
#'
#' @return The combined replicated blocks as a dlm_block.
#' @export
#'
#' @examples
#' # Long way
#' level <- polynomial_block(alpha = 1, order = 1)
#'
#' final.block <- block_mult(level, 5)
#'
#' # Short way
#' final.block <- 5 * polynomial_block(alpha = 1, order = 1)
#'
#' @family auxiliary functions for structural blocks
block_mult <- function(block, k) {
  block.list <- list()
  size.total <- floor(log10(k)) + 1
  if (k > 1) {
    for (i in 1:k) {
      size.i <- floor(log10(i)) + 1
      block.clone <- block
      block.clone$pred.names <- paste0(block$pred.names, ".", paste0(rep("0", size.total - size.i), collapse = ""), i)
      block.list <- append(block.list, list(block.clone))
    }
    block <- do.call(block_superpos, block.list)
  }
  return(block)
}

#' block_rename
#'
#' @param block A dlm_block object.
#' @param pred.names A vector of string with names for each linear predictor in block.
#'
#' @return A dlm_block with the linear predictors renamed to the values passed in names.
#' @export
#'
#' @examples
#'
#' base.block <- polynomial_block(
#'   eta = 1,
#'   order = 1,
#'   name = "Poly",
#'   D = 0.95
#' )
#'
#' final.block <- block_rename(2 * base.block, c("mu", "sigma"))
#'
#' @family auxiliary functions for structural blocks
block_rename <- function(block, pred.names) {
  if (!inherits(block, "dlm_block")) {
    stop("Error: The block argument is not a dlm_block object.")
  }
  if (length(pred.names) != length(block$pred.names)) {
    stop(paste0("Error: The number of names provided does not match the number of linear predictor in the block. Expected ", length(block$pred.names), ", got ", length(names), "."))
  }
  if (length(pred.names) != length(unique(pred.names))) {
    stop(paste0("Error: Repeated names are not allowed."))
  }

  block$pred.names <- pred.names
  colnames(block$FF) <- pred.names
  block$status <- check.block.status(block)
  return(block)
}

#' An auxiliary function for model intervention
#'
#' This function adds timely modifications to a dlm_block, such that in the specified time the model will override the usual value of the each variable to the value chosen by the user.
#'
#' @param block dlm_block: The block to add the intervention.
#' @param time Vector: A sequence of integers indicating the time of the intervention.
#' @param var.index Vector: A sequence of integers indicating which variables should be modified in the intervention.
#' @param FF Array: A n x k x t array with the modified FF to be used during the intervention, where n is the length of var.index, k is the number of linear predictors in the block and t is the size of time (can be omitted if time is a scalar).
#' @param D Array: A n x n x t array with the modified D to be used during the intervention, where n is the length of var.index and t is the size of time (can be omitted if time is a scalar).
#' @param h matrix: A n x t matrix with the modified h to be used during the intervention, where n is the length of var.index and t is the size of time (can be omitted if time is a scalar).
#' @param H Array: A n x n x t array with the modified H to be used during the intervention, where n is the length of var.index and t is the size of time (can be omitted if time is a scalar).
#' @param G Array: A n x n x t array with the modified G to be used during the intervention, where n is the length of var.index and t is the size of time (can be omitted if time is a scalar).
#'
#' @return A dlm_block with the added intervention.
#' @export
#'
#' @examples
#'
#' data <- c(AirPassengers)
#' # Adding an artificial change, so that we can make an intervention on the data at that point
#' # Obviously, one should NOT change their own data.
#' data[60:144] <- data[60:144] + 500
#'
#' level <- polynomial_block(rate = 1, order = 2, D = 0.95)
#' season <- harmonic_block(rate = 1, order = 2, period = 12, D = 0.975)
#'
#' # Reducing the discount factor so that the model can capture the expected change.
#' level <- level |> intervention(time = 60, H = 1, var.index = 1)
#' # Comment the line above to see the fit without the intervention
#'
#' outcome <- Poisson(lambda = "rate", data = data)
#'
#' fitted.data <- fit_model(level, season,
#'   AirPassengers = outcome
#' )
#'
#' plot(fitted.data, plot.pkg = "base")
#'
#' @family auxiliary functions for structural blocks
intervention <- function(block, time, var.index = 1:block$n, FF = NULL, D = NULL, h = NULL, H = NULL, G = NULL) {
  block$interventions <- append(
    block$interventions,
    list(list(times = time, var.index = var.index, pred.names = block$pred.names, FF = FF, D = D, h = h, H = H, G = G))
  )
  block
}

#' Specify method for dlm blocks
#'
#' Sets the values of undefined parameters in a block to those passed by the user.
#'
#' @param x dlm_block: A undefined dlm_block object from which the undefined parameters shall be substituted.
#' @param ... A set of named values for each unknown parameter.
#'
#' @return The initual block, but with the undefined parameters set to the chosen values.
#' @export
#'
#' @examples
#'
#' season <- harmonic_block(rate = 1, period = 12, D = "D.sazo") |>
#'   specify(D.sazo = 0.975)
#'
#' @family auxiliary functions for structural blocks
specify.dlm_block <- function(x, ...) {
  var.value <- list(...)
  for (name in names(var.value)) {
    value <- var.value[[name]]
    x$FF[array(x$FF.labs == name, c(x$n, x$k, x$t))] <- value
    x$FF.labs[x$FF.labs == name] <- "Covariate"
    x$D[x$D == name] <- value
    x$H[x$H == name] <- value
    x$a1[x$a1 == name] <- value
    x$R1[x$R1 == name] <- value
    x$G[x$G == name] <- value
    x$scale[x$scale == name] <- value
  }

  FF <- array(as.numeric(x$FF), c(x$n, x$k, x$t), dimnames = list(NULL, x$pred.names, NULL))
  D <- array(as.numeric(x$D), c(x$n, x$n, x$t))
  H <- array(as.numeric(x$H), c(x$n, x$n, x$t))
  a1 <- as.numeric(x$a1)
  R1 <- matrix(as.numeric(x$R1), x$n, x$n)
  G <- array(as.numeric(x$G), c(x$n, x$n, x$t))
  scale <- as.numeric(x$scale)
  if (all(!is.na(FF))) {
    x$FF <- FF
  }
  if (all(!is.na(D))) {
    x$D <- D
  }
  if (all(!is.na(H))) {
    x$H <- H
  }
  if (all(!is.na(a1))) {
    x$a1 <- a1
  }
  if (all(!is.na(R1))) {
    x$R1 <- R1
    if (all(!is.na(scale))) {
      x$scale <- scale
      scale.mat <- x$direction %*% diag(scale, ncol = x$n, nrow = x$n) %*% t(x$direction)
      x$R1 <- scale.mat %*% R1 %*% t(scale.mat)
    }
  }
  if (all(!is.na(G) | array(x$G.labs != "const", c(x$n, x$n, x$t)))) {
    x$G <- G
  }
  x$status <- check.block.status(x)
  return(x)
}

#' @rdname harmonic_block
#' @export
har <- function(period, order = 1, D = 0.98, a1 = 0, R1 = 4, name = "Var.Sazo", X = 1) {
  harmonic_block(mu = X, period = period, order = order, D = D, a1 = a1, R1 = R1, name = name)
}
#' @rdname ffs_block
#' @export
ffs <- function(period, D = 0.95, a1 = 0, R1 = 9, name = "Var.FFS", X = 1) {
  harmonic_block(mu = X, period = period, D = D, a1 = a1, R1 = R1, name = name)
}
#' @rdname regression_block
#' @export
reg <- function(X, max.lag = 0, zero.fill = TRUE, D = 0.95, a1 = 0, R1 = 9, name = "Var.Reg") {
  regression_block(mu = X, max.lag = max.lag, zero.fill = zero.fill, D = D, a1 = a1, R1 = R1, name = name)
}
#' @rdname polynomial_block
#' @export
pol <- function(order = 1, D = 0.95, a1 = 0, R1 = 9, name = "Var.Poly", X = 1) {
  polynomial_block(mu = X, order = order, D = D, a1 = a1, R1 = R1, name = name)
}
#' @rdname tf_block
#' @export
AR <- function(order = 1, noise.var = NULL, noise.disc = NULL, a1 = 0, R1 = 9, a1.coef = c(1, rep(0, order - 1)), R1.coef = c(1, rep(0.25, order - 1)), name = "Var.AR", X = 1) {
  TF_block(mu = X, order = order, noise.var = noise.var, noise.disc = noise.disc, a1 = a1, R1 = R1, a1.coef = a1.coef, R1.coef = R1.coef, name = name)
}
#' @rdname tf_block
#' @export
TF <- function(pulse, order = 1, noise.var = NULL, noise.disc = NULL,
               a1 = 0, R1 = 9,
               a1.coef = c(1, rep(0, order - 1)), R1.coef = c(1, rep(0.25, order - 1)),
               a1.pulse = 0, R1.pulse = 4, name = "Var.AR", X = 1) {
  TF_block(
    mu = X, order = order, noise.var = noise.var, noise.disc = noise.disc, a1 = a1, R1 = R1, a1.coef = a1.coef, R1.coef = R1.coef,
    pulse = pulse, a1.pulse = a1.pulse, R1.pulse = R1.pulse, name = name
  )
}
#' @rdname noise_block
#' @export
noise <- function(name = "Noise", D = 0.99, R1 = 0.1, H = 0, X = 1) {
  noise_block(mu = X, D = D, R1 = R1, H = H, name = name)
}
