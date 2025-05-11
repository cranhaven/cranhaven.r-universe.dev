#' Automatically generate objects used to describe the structure of the overlapping group lasso penalty
#'
#' @description
#' Automatically generate objects used to describe the structure of the overlapping group lasso penalty The output is then used by \code{\link{sox}()} and \code{\link{sox_cv}()}.
#' 
#' @param group_list A list containing the indices of the group members.
#' @noRd
#' @examples 
#' # p = 9 Variables:
#' ## 1: A1
#' ## 2: A2
#' ## 3: C1
#' ## 4: C2
#' ## 5: B
#' ## 6: A1B
#' ## 7: A2B
#' ## 8: C1B
#' ## 9: C2B
#' 
#' # G = 5 Overlapping groups:
#' ## g1: A1, A2, A1B, A2B
#' ## g2: B, A1B, A2B, C1B, C2B
#' ## g3: A1B, A2B
#' ## g4: C1, C2, C1B, C2B
#' ## g5: C1B, C2B
#' 
#' overlapping.groups <- list(c(1, 2, 6, 7),
#'                            c(5, 6, 7, 8, 9),
#'                            c(6, 7),
#'                            c(3, 4, 8, 9),
#'                            c(8, 9))
#'                            
#' pars.overlapping <- overlap_structure(overlapping.groups)
#'
#' str(pars.overlapping)
#'                 
#' @return A list of objects describing the group structure.
#'   \item{groups}{Required by \code{\link{sox}()} and \code{\link{sox_cv}()} to describe the relationship between the \eqn{G} \code{overlapping} groups. A \eqn{G * G} integer matrix whose \eqn{(i,j)} entry is \code{1} if and only if \eqn{i\neq j} and \eqn{g_i} is a child group (subset) of \eqn{g_j}, and is \code{0} otherwise.}
#'   \item{groups_var}{Required by \code{\link{sox}()} and \code{\link{sox_cv}()} to describe the relationship between the \eqn{G} \code{overlapping} groups and the \eqn{p} variables. A \eqn{p * G} integer matrix whose \eqn{(i,j)} entry is \code{1} if and only if variable \eqn{i} is in group \eqn{g_j}, but not in any child group of \eqn{g_j}, and is \code{0} otherwise. }
#'   \item{group_weights}{Required by \code{\link{sox}()} and \code{\link{sox_cv}()} to specify the group-specific penalty weights. The penalty weight for each group is equal to the square root of the group size.}

overlap_structure <- function(group_list) {
  
  n_groups <- length(group_list)
  
  # group_list <- lapply(group_list, sort)
  
  # reorder <- order(sapply(group_list, FUN = utils::head, n = 1))
  # 
  # group_list <- group_list[reorder]
  
  vars <- sort(unique(unlist(group_list)))
  n_vars <- length(vars)
  
  gv <- matrix(0L, nrow = n_vars, ncol = n_groups)
  
  for (i in 1:n_vars) {
    var <- vars[i]
    for (j in 1:n_groups) {
      if (var %in% group_list[[j]]) gv[i, j] <- 1L
    }
  }
  
  g <- matrix(0L, nrow = n_groups, ncol = n_groups)
  
  for (i in 1:(n_groups - 1)) {
    for (j in (i + 1):n_groups) {
      gi <- group_list[[i]]
      gj <- group_list[[j]]
      gij <- intersect(gi, gj)
      if (identical(gi, gij)) {
        g[i, j] <- 1L
        for (var in gi) {
          var_id <- which(var == vars)
          gv[var_id, j] <- 0L
        }
      }
      if (identical(gj, gij)) {
        g[j, i] <- 1L
        for (var in gj) {
          var_id <- which(var == vars)
          gv[var_id, i] <- 0L
        }
      }
    }
  }
  
  w <- sqrt(sapply(group_list, length, simplify = TRUE))
  
  return(list(groups = g,
              groups_var = gv,
              group_weights = w))
  
}




# group.list <- list(0:3, 3:6, 6:9, 0:5, 6:8)
# overlap_structure(group_list = group.list)
