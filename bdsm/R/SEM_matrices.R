regressor_names <- function(df, timestamp_col, entity_col, dep_var_col) {
  df %>%
    dplyr::select(
      ! c({{ timestamp_col }}, {{ entity_col }}, {{ dep_var_col }})
    ) %>% colnames()
}

determine_min_timestamps <- function(df, timestamp_col) {
  timestamps <- dplyr::select(df, {{ timestamp_col }})
  timestamp_0 <- min(timestamps)
  timestamp_1 <- min(timestamps[timestamps != timestamp_0])
  list(timestamp_0 = timestamp_0, timestamp_1 = timestamp_1)
}

#' Matrix with dependent variable data for SEM representation
#'
#' Create matrix which contains dependent variable data used in the Simultaneous
#' Equations Model (SEM) representation on the left hand side of the equations.
#' The matrix contains the data for time periods greater than or equal to the
#' second lowest time stamp. The matrix is then used to compute likelihood for
#' SEM analysis.
#'
#' @param df Data frame with data for the SEM analysis.
#' @param timestamp_col Column which determines time periods. For now only
#' natural numbers can be used as timestamps
#' @param entity_col Column which determines entities (e.g. countries, people)
#' @param dep_var_col Column with dependent variable
#'
#' @return
#' Matrix of size N x T where N is the number of entities considered and T is
#' the number of periods greater than or equal to the second lowest time stamp.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   entities = rep(1:4, 5),
#'   times = rep(seq(1960, 2000, 10), each = 4),
#'   dep_var = stats::rnorm(20), a = stats::rnorm(20), b = stats::rnorm(20)
#' )
#' SEM_dep_var_matrix(df, times, entities, dep_var)
SEM_dep_var_matrix <- function(df, timestamp_col, entity_col, dep_var_col) {
  min_timestamps <-
    determine_min_timestamps(df = df, timestamp_col = {{ timestamp_col }})
  timestamp_1 <- min_timestamps$timestamp_1

  df %>% dplyr::filter({{ timestamp_col }} >= timestamp_1) %>%
    dplyr::select({{ timestamp_col }}, {{ entity_col }}, {{ dep_var_col }}) %>%
    tidyr::pivot_wider(names_from = {{ timestamp_col }},
                       values_from = {{ dep_var_col }}) %>%
    dplyr::select(!{{ entity_col }}) %>% as.matrix()
}

#' Matrix with regressors data for SEM representation
#'
#' Create matrix which contains regressors data used in the Simultaneous
#' Equations Model (SEM) representation on the left hand side of the equations.
#' The matrix contains regressors data for time periods greater than or equal to
#' the second lowest time stamp. The matrix is then used to compute likelihood
#' for SEM analysis.
#'
#' @param df Data frame with data for the SEM analysis.
#' @param timestamp_col Column which determines time periods. For now only
#' natural numbers can be used as timestamps
#' @param entity_col Column which determines entities (e.g. countries, people)
#' @param dep_var_col Column with dependent variable
#'
#' @return
#' Matrix of size N x (T-1)*k where N is the number of entities considered, T is
#' the number of periods greater than or equal to the second lowest time stamp
#' and k is the number of chosen regressors. If there are no regressors returns
#' \code{NULL}.
#' @export
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   entities = rep(1:4, 5),
#'   times = rep(seq(1960, 2000, 10), each = 4),
#'   dep_var = stats::rnorm(20), a = stats::rnorm(20), b = stats::rnorm(20)
#' )
#' SEM_regressors_matrix(df, times, entities, dep_var)
SEM_regressors_matrix <- function(df, timestamp_col, entity_col, dep_var_col) {
  regressors <- df %>%
    regressor_names(timestamp_col = {{ timestamp_col }},
                    entity_col = {{ entity_col }},
                    dep_var_col = {{ dep_var_col }})

  min_timestamps <-
    determine_min_timestamps(df = df, timestamp_col = {{ timestamp_col }})
  timestamp_1 <- min_timestamps$timestamp_1

  df <- df %>%
    dplyr::select({{ timestamp_col }}, {{ entity_col }}, {{ regressors }})

  if (length(colnames(df)) == 2) NULL else {
    . <- NULL
    df %>% dplyr::filter({{ timestamp_col }} > timestamp_1) %>%
      tidyr::pivot_wider(
        names_from = {{ timestamp_col }},
        values_from = !{{ entity_col }} & !{{ timestamp_col }}
      ) %>%
      dplyr::select(!{{ entity_col }}) %>%
      dplyr::select(order(as.numeric(gsub("[^0-9]+", "", colnames(.))))) %>%
      as.matrix()
  }
}

#' Matrix with exogenous variables for SEM representation
#'
#' Create matrix which contains exogenous variables used in the Simultaneous
#' Equations Model (SEM) representation. Currently these are: dependent variable
#' from the lowest time stamp and regressors from the second lowest time stamp.
#' The matrix is then used to compute likelihood for SEM analysis.
#'
#' @param df Data frame with data for the SEM analysis.
#' @param timestamp_col Column which determines time periods. For now only
#' natural numbers can be used as timestamps
#' @param entity_col Column which determines entities (e.g. countries, people)
#' @param dep_var_col Column with dependent variable
#'
#' @return
#' Matrix of size N x k+1 where N is the number of entities considered and k is
#' the number of chosen regressors
#' @export
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   entities = rep(1:4, 5),
#'   times = rep(seq(1960, 2000, 10), each = 4),
#'   dep_var = stats::rnorm(20), a = stats::rnorm(20), b = stats::rnorm(20)
#' )
#' exogenous_matrix(df, times, entities, dep_var)
exogenous_matrix <- function(df, timestamp_col, entity_col, dep_var_col) {
  regressors <- df %>%
    regressor_names(timestamp_col = {{ timestamp_col }},
                    entity_col = {{ entity_col }},
                    dep_var_col = {{ dep_var_col }})

  min_timestamps <-
    determine_min_timestamps(df = df, timestamp_col = {{ timestamp_col }})
  timestamp_1 <- min_timestamps$timestamp_1
  timestep <- timestamp_1 - min_timestamps$timestamp_0

  df_with_lagged_col <- df %>%
    dplyr::select({{ entity_col }}, {{ timestamp_col }}, {{ dep_var_col }}) %>%
    dplyr::filter({{ timestamp_col }} == (timestamp_1 - timestep)) %>%
    dplyr::mutate("{{timestamp_col}}" := {{ timestamp_col }} + timestep)

  df %>%
    dplyr::filter({{ timestamp_col }} == timestamp_1) %>%
    dplyr::select(!{{ dep_var_col }}) %>%
    dplyr::left_join(df_with_lagged_col,
              by = dplyr::join_by(
                {{ timestamp_col }} == {{ timestamp_col }},
                {{ entity_col }} == {{ entity_col }}
              )) %>%
    dplyr::select({{ dep_var_col }}, {{ regressors }}) %>% as.matrix()
}

#' Residual Maker Matrix
#'
#' Create residual maker matrix from a given matrix \code{m}. See article about
#' \href{https://en.wikipedia.org/wiki/Projection_matrix}{projection matrix} on
#' the Wikipedia.
#'
#' @param m Matrix
#'
#' @return
#' M x M matrix where M is the number of rows in the \code{m} matrix.
#' @export
#'
#' @examples
#' residual_maker_matrix(matrix(c(1,2,3,4), nrow = 2))
residual_maker_matrix <- function(m) {
  proj_matrix <- m%*%solve(crossprod(m))%*%t(m)
  diag(nrow(m)) - proj_matrix
}

#' Coefficients matrix for SEM representation
#'
#' Create coefficients matrix for Simultaneous Equations Model (SEM)
#' representation.
#'
#' @param alpha numeric
#' @param periods_n integer
#' @param beta numeric vector. Default is c() for no regressors case.
#'
#' @return List with two matrices B11 and B12
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' SEM_B_matrix(3, 4, 4:6)
SEM_B_matrix <- function(alpha, periods_n, beta = c()) {
  alpha_matrix <- diag(rep(-alpha, periods_n-1))
  B11 <- diag(periods_n)
  B11[2:periods_n, 1:(periods_n - 1)] <-
    B11[2:periods_n, 1:(periods_n - 1)] + alpha_matrix

  B12 <- if (length(beta) != 0) {
    regressors_n <- length(beta)
    n_cols <- regressors_n*(periods_n - 1)
    beta_row <- function(row_ind, beta, n_cols, regressors_n) {
      n_zeros_front <- (row_ind-1)*regressors_n
      c(rep(0, n_zeros_front), -beta,
        rep(0, n_cols - n_zeros_front - regressors_n))
    }
    beta_matrix <- 1:(periods_n-1) %>%
      sapply(beta_row, beta = beta,
             n_cols = n_cols, regressors_n = regressors_n) %>% t()
    rbind(optimbase::zeros(1, n_cols),
          beta_matrix)
  } else {
    NULL
  }
  list(B11, B12)
}

#' Coefficients matrix for initial conditions
#'
#' Create matrix for Simultaneous Equations Model (SEM)
#' representation with coefficients placed next to initial values
#' of regressors, dependent variable and country-specific time-invariant
#' variables.
#'
#' @param alpha numeric
#' @param phi_0 numeric
#' @param periods_n numeric
#' @param beta numeric vector. Default is c() for no regressors case.
#' @param phi_1 numeric vector. Default is c() for no regressors case.
#'
#' @return matrix
#' @export
#'
#' @examples
#' alpha <- 9
#' phi_0 <- 19
#' beta <- 11:15
#' phi_1 <- 21:25
#' periods_n <- 4
#' SEM_C_matrix(alpha, phi_0, periods_n, beta, phi_1)
SEM_C_matrix <- function(alpha, phi_0,  periods_n, beta = c(), phi_1 = c()) {
  C1 <- matrix(rep(phi_0, periods_n))
  C1[1, 1] <- C1[1, 1] + alpha
  if (length(beta) != 0) {
    col2 <- matrix(rep(phi_1, periods_n), periods_n, byrow = TRUE)
    col2[1, 1:length(beta)] <-
      col2[1, 1:length(beta)] + beta
    C1 <- cbind(C1, col2)
  }
  C1
}

#' Matrix with psi parameters for SEM representation
#'
#' @param psis double vector with psi parameter values
#' @param timestamps_n number of time stamps (e.g. years)
#' @param features_n number of features (e.g. population size, investment rate)
#'
#' @return
#' A matrix with \code{timestamps_n} rows and
#' \code{(timestamps_n - 1) * feature_n} columns. Psis are filled in row by row
#' in a block manner, i.e. blocks of size \code{feature_n} are placed next to
#' each other
#'
#' @export
#'
#' @examples
#' SEM_psi_matrix(1:30, 4, 5)
SEM_psi_matrix <- function(psis, timestamps_n, features_n) {
  matrix_row_n <- timestamps_n
  psi_matrix_row <- function(row_ind) {
    psi_start_ind_in_row <- row_ind * (row_ind - 1) * features_n / 2 +
      (row_ind - 1) * (timestamps_n - row_ind) * features_n + 1
    psi_end_ind_in_row <- psi_start_ind_in_row +
      (timestamps_n - row_ind) * features_n - 1
    if(row_ind == 1) {
      psis[psi_start_ind_in_row:psi_end_ind_in_row]
    } else if (row_ind == matrix_row_n) {
      optimbase::zeros(1, (row_ind - 1)*features_n)
    } else {
      c(optimbase::zeros(1, (row_ind - 1)*features_n),
        psis[psi_start_ind_in_row:psi_end_ind_in_row])
    }
  }
  t(sapply(1:matrix_row_n, psi_matrix_row))
}

#' Covariance matrix for SEM representation
#'
#' Create covariance matrix for Simultaneous Equations Model (SEM)
#' representation. Only the part necessary to compute concentrated likelihood
#' function is computed (cf. Appendix in the Moral-Benito paper)
#'
#' @param err_var numeric
#' @param dep_vars numeric vector
#' @param phis numeric vector
#' @param psis numeric vector
#'
#' @return List with two matrices Sigma11 and Sigma12
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' err_var <- 1
#' dep_vars <- c(2, 2, 2, 2)
#' phis <- c(10, 10, 20, 20, 30, 30)
#' psis <- c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112)
#' SEM_sigma_matrix(err_var, dep_vars, phis, psis)
SEM_sigma_matrix <- function(err_var, dep_vars, phis = c(), psis = c()) {
  periods_n <- length(dep_vars)

  O11 <- err_var^2*optimbase::ones(periods_n, periods_n) +
    diag(dep_vars^2)

  O12 <- if (length(phis) != 0) {
    regressors_n <- length(phis)/(periods_n - 1)

    phi_matrix <- matrix(rep(phis, periods_n), nrow = periods_n, byrow = TRUE)
    psi_matrix <- SEM_psi_matrix(psis = psis, timestamps_n = periods_n,
                                 features_n = regressors_n)

    phi_matrix + psi_matrix
  } else {
    NULL
  }

  list(O11, O12)
}
