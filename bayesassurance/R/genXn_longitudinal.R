#' Design Matrix Generator in Longitudinal Setting
#'
#' Constructs design matrix using inputs that correspond
#' to a balanced longitudinal study design.
#' Used for power and sample size analysis in the Bayesian setting.
#' @param ids vector of unique subject ids, usually of length 2
#' for study design purposes.
#' @param from start time of repeated measures for
#' each subject
#' @param to end time of repeated measures for each
#' subject
#' @param num_repeated_measures desired length of the repeated measures 
#' sequence. Should be a non-negative number, will be rounded up if fractional.
#' @param poly_degree degree of polynomial in longitudinal model, set to 1 by 
#' default.
#' @return Xn: a design matrix that can be used to assess the
#' Bayesian assurance through Monte Carlo sampling using
#' functions presented in this package.
#' @seealso \code{\link{gen_Xn}}
#'
#' @examples 
#' ## Example 1
#' ## We pass in a vector of subject IDs and specify the start and end
#' ## timepoints along with the desired length of the sequence.
#' ## The resulting design matrix contains vectors of
#' ## ones with lengths that correspond to the number of repeated
#' ## measures for each unique subject.
#'
#' ids <- c(1,2,3,4)
#' gen_Xn_longitudinal(ids, from = 1, to = 10, num_repeated_measures = 4)
#'
#' ## Example 2
#' ## If we wish to fit a longitudinal model of a higher degree (e.g. 
#' ## parabolic, cubic), we need to adjust the `poly_degree` variable
#'
#' # parabolic
#' ids <- c(1,2,3,4)
#' gen_Xn_longitudinal(ids, from = 1, to = 10, num_repeated_measures = 4,
#' poly_degree = 2)
#'
#' # cubic
#' ids <- c(1,2,3,4)
#' gen_Xn_longitudinal(ids, from = 1, to = 10, num_repeated_measures = 4,
#' poly_degree = 3)
#' @export
#'
gen_Xn_longitudinal <- function(ids, from, to, num_repeated_measures, 
                                poly_degree = 1){

  # create dataframe d given inputs
  d1 <- sort(rep(ids, num_repeated_measures))
  d2 <- seq(from = from, to = to, length.out = num_repeated_measures)
  d <- as.data.frame(cbind(d1, d2))

  p <- length(ids)
  n <- as.data.frame(table(d[,1]))$Freq
  t <- d[,2]
  Xn_1 <- matrix(0, nrow=nrow(d), ncol=p)
  Xn_2 <- matrix(0, nrow=nrow(d), ncol=p)

  # manages intercepts of design matrix
  for(i in 1:p){
    ones <- rep(1, n[i])
    if(i == 1){
      Xn_1[1:n[i], i] <- ones
    }else{
      row_begin <- sum(n[1:i-1])
      row_end <- sum(n[1:i])
      Xn_1[(row_begin+1):(row_end), i] <- ones
    }
  }


  # manages coefficients of design matrix
  for(i in 1:p){
    if(i == 1){
      Xn_2[1:n[i], i] <- t[1:n[i]]
    }else{
      row_begin <- sum(n[1:i-1])
      row_end <- sum(n[1:i])
      Xn_2[(row_begin+1):(row_end), i] <- t[(row_begin+1):(row_end)]
    }
  }

  # adds additional columns depending on whether user's model
  # is of higher degree (e.g. quadratic, cubic)
  extra_cols <- function(poly_degree, x){
    result <- x^poly_degree
    return(result)
  }

  if(poly_degree > 1){
    poly_vals <- 2:poly_degree
    columns_to_add <- lapply(poly_vals, extra_cols, x = Xn_2)
    Xn_3 <- cbind.data.frame(columns_to_add)
    Xn <- as.matrix(cbind(Xn_1, Xn_2, Xn_3))
  }else{
    Xn <- as.matrix(cbind(Xn_1, Xn_2))
  }

  return(Xn)
}
