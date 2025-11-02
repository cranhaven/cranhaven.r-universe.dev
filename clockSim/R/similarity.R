# Similarity metrics - comparing two trajectory

#' Trajectory similarity: variable normalization
#' 
#' Performs per-state normalization by dividing state values by: range (max-min) 
#' or sd (standard deviation). none means no normalization is performed.
#'
#' @param res_mat Trajectory matrix, first column must be time while others are states.
#' @param method State normalization method, one of "none", "range", "sd".
#' @param ref_stats Optional list containing pre-computed normalization statistics 
#'                  (e.g., from a previous call to `compute_normalize`). Must include:
#'                  - For `method = "range"`: `mins` and `maxs` vectors
#'                  - For `method = "sd"`: `means` and `sds` vectors
#'
#' @returns Normalized trajectory matrix with attribute `"ref_stats"` containing 
#'          normalization parameters.
#' @export
#'
#' @examples
#' mat <- cbind(time = 1:10, matrix(runif(30, 0, 10), ncol = 3))
#' 
#' # Range normalization with automatic stat computation
#' norm_mat1 <- compute_normalize(mat, "range")
#' 
#' # Reuse stats for another matrix
#' mat2 <- cbind(time = 1:10, matrix(runif(30, 5, 15), ncol = 3))
#' norm_mat2 <- compute_normalize(mat2, "range", attr(norm_mat1, "ref_stats"))
compute_normalize <-
  function(res_mat,
           method = c("none", "range", "sd"),
           ref_stats = NULL) {
    
    # Match method argument
    method <- match.arg(method)
    if (method == "none") return(res_mat)
    
    time_col <- res_mat[, 1, drop = FALSE]
    states <- res_mat[, -1, drop = FALSE]
    
    # Compute or use provided normalization stats
    #   Handles edge cases: when range/sd == 0
    if (is.null(ref_stats)) {
      if (method == "range") {
        ref_stats <- list(mins = matrixStats::colMins(states),
                          maxs = matrixStats::colMaxs(states))
        ref_stats$maxs[ref_stats$maxs == ref_stats$mins] <-
          ref_stats$mins[ref_stats$maxs == ref_stats$mins] + 1
      } else if (method == "sd") {
        ref_stats <- list(means = matrixStats::colMeans2(states),
                          sds = matrixStats::colSds(states))
        ref_stats$sds[ref_stats$sds == 0] <- 1
      }
    }
    
    # Apply normalization
    if (method == "range") {
      states_norm <- (states - rep(ref_stats$mins, each = nrow(states))) /
        rep(ref_stats$maxs - ref_stats$mins, each = nrow(states))
    } else if (method == "sd") {
      states_norm <- (states - rep(ref_stats$means, each = nrow(states))) /
        rep(ref_stats$sds, each = nrow(states))
    }
    
    result <- cbind(time_col, states_norm)
    attr(result, "ref_stats") <- ref_stats
    result
  }



#' Trajectory similarity: validate consistency of result matrix pair
#' 
#' Verify that the two matrices share the same time values and the same states.
#'
#' @param res_mat1 Trajectory matrix 1, first column must be time while others are states.
#' @param res_mat2 Trajectory matrix 2, first column must be time while others are states.
#'
#' @examples
#' # Internal only; error if check fails.
.compute_validatePair <- function(res_mat1, res_mat2){
  # Input validation
  if (!isTRUE(all.equal(res_mat1[, 1], res_mat2[, 1])))
    rlang::abort("Time columns must be identical")
  if (!all(colnames(res_mat1) == colnames(res_mat2)))
    rlang::abort("States must match")
}




#' Trajectory similarity: Root Mean Squared Error RMSE
#' 
#' RMSE is used to characterize how *absolute* values of two trajectory 
#' \eqn{\vec{X}(t), \vec{Y}(t)} agree with each other. There are 
#' normalized and default versions:
#' 
#' Default (value scales with variable \eqn{i}):
#' 
#' \eqn{\mbox{RMSE}_i=\sqrt{\frac{\Sigma(X_i(t^j)-Y_i(t^j))^2}{N}}}
#' 
#' Normalized (value normalized by range for each variable \eqn{i}):
#' 
#' \eqn{\mbox{NRMSE}_i=\frac{\mbox{RMSE}}{\mbox{spread}}} (spread is customizable)
#' 
#' For normalization, spread is always computed ONLY from `res_mat1`. `res_mat2` 
#' is normalized using spread computed from `mat1` to still retain the property 
#' that RMSE allows comparing *absolute* values of two trajectories.
#' 
#' In this case, normalization is used to prevent numerically large states from 
#' dominating RMSE results, giving a more thorough comparison of trajectories.
#'
#' @param res_mat1 Trajectory matrix 1, first column must be time while others are states.
#' @param res_mat2 Trajectory matrix 2, first column must be time while others are states.
#' @param normalize Normalization method, one of "none", "range", "sd".
#'
#' @returns RMSE of the two trajectory, vector with length = number of input states.
#' @export
#'
#' @examples
#' # Perfect agreement (zero error)
#' mat1 <- cbind(time = 1:3, state1 = c(1, 2, 3), state2 = c(4, 5, 6))
#' mat2 <- mat1
#' compute_rmse(mat1, mat2)
#' 
#' # Simple error case
#' #   state1 = sqrt(mean(c(0,0,0.5)^2)) = 0.29
#' #   state2 = sqrt(mean(c(0,0.2,0)^2)) = 0.12
#' mat3 <- cbind(time = 1:3, state1 = c(1, 2, 3.5), state2 = c(4, 5.2, 6))
#' compute_rmse(mat1, mat3)
#' 
#' # Normalized example (NRMSE = RMSE / spread)
#' #   state1 = state2 = 1/20 = 0.05
#' mat4 <- cbind(time = 1:3, state1 = c(10, 20, 30), state2 = c(40, 50, 60))
#' mat5 <- cbind(time = 1:3, state1 = c(11, 21, 31), state2 = c(41, 51, 61))
#' compute_rmse(mat4, mat5, "range")  # c(1/20, 1/20) = c(0.05, 0.05)
compute_rmse <- function(res_mat1, res_mat2, normalize = "none"){
  
  # Validate consistency
  .compute_validatePair(res_mat1, res_mat2)
  
  # Normalize
  norm1 <- compute_normalize(res_mat1, method = normalize)
  ref_stats <- attr(norm1, "ref_stats")
  norm2 <- compute_normalize(res_mat2, method = normalize, ref_stats)
  states1 <- norm1[, -1, drop = FALSE]
  states2 <- norm2[, -1, drop = FALSE]
  
  # Compute RMSE
  sqrt(matrixStats::colMeans2((states1 - states2)^2))
}




#' Trajectory similarity: Cosine Similarity
#'
#' Cosine Similarity is used to characterize how *state direction* of two 
#' trajectory \eqn{\vec{X}(t), \vec{Y}(t)} agree with each other. 
#' It computes cosine of the angle between the two states at each time.
#' 
#' Formula:
#' \eqn{\mbox{CS}(t^j)=\frac{\Sigma_i X_i(t^j)Y_i(t^j)}{\left\Vert X_i(t^j) \right\Vert \left\Vert Y_i(t^j) \right\Vert}}
#' 
#' @param res_mat1 Trajectory matrix 1, first column must be time while others are states.
#' @param res_mat2 Trajectory matrix 2, first column must be time while others are states.
#'
#' @returns Cosine similarity of the two trajectory, vector with length = number of time steps.
#' @export
#'
#' @examples
#' # Perfect alignment (cos=1)
#' mat1 <- cbind(time = 1:3, state1 = 1:3, state2 = 4:6)
#' mat2 <- cbind(time = 1:3, state1 = 2:4, state2 = 5:7)
#' compute_cosine(mat1, mat2)  # c(1, 1, 1)
#' 
#' # Orthogonal vectors (cos=0)
#' mat3 <- cbind(time = 1:2, state1 = c(1, 0), state2 = c(0, 1))
#' mat4 <- cbind(time = 1:2, state1 = c(0, 1), state2 = c(1, 0))
#' compute_cosine(mat3, mat4)  # c(0, 0)
#' 
#' # Opposite direction (cos=-1)
#' mat5 <- cbind(time = 1:3, state1 = 1:3)
#' mat6 <- cbind(time = 1:3, state1 = -1:-3)
#' compute_cosine(mat5, mat6)  # c(-1, -1, -1)
compute_cosine <- function(res_mat1, res_mat2){
  
  # Validate consistency
  .compute_validatePair(res_mat1, res_mat2)
  
  states1 <- res_mat1[, -1, drop = FALSE]
  states2 <- res_mat2[, -1, drop = FALSE]
  
  # Vectorized cosine similarity calculation
  dot_products <- matrixStats::rowSums2(states1 * states2)
  norm1 <- matrixStats::rowSums2(states1^2)
  norm2 <- matrixStats::rowSums2(states2^2)
  
  cosine <- dot_products / sqrt(norm1 * norm2)
  cosine[is.nan(cosine)] <- NA  # Handle zero vectors
  
  cosine
}