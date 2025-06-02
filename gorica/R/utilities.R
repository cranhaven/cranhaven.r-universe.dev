with_env <- function(fun, envir=parent.frame(), ...) {
  stopifnot(is.function(fun))
  environment(fun) <- envir
  do.call(fun, list(...))
}

utils::globalVariables(c("cl", "x", "Sigma", "comparison", "hypothesis"))

create_res <- function(){
  assign("Goricares", list(
    fit = NULL,
    call = cl,
    model = x,
    estimates = x,
    Sigma = Sigma,
    comparison = comparison
  ), envir = parent.env())
}

# drop_empty_hyps <- function(){
#   #browser()
#   null_hyps <- sapply(hypothesis$hyp_mat, is.null)
#   if(any(null_hyps)){
#     which_null <- which(null_hyps)
#     remove_these <- as.vector(sapply(which_null, seq_tuples, tuples = 2))
#     hypothesis$hyp_mat[which_null] <<- NULL
#     hypothesis$n_constraints <<- hypothesis$n_constraints[-remove_these]
#     hypothesis$n_ec <<- hypothesis$n_ec[-which_null]
#     hypothesis$original_hypothesis <<- hypothesis$original_hypothesis[-which_null]
#   }
# }

# hypothesis_remove_nulls <- function(which_par){
#   R <- do.call(rbind, hypothesis$hyp_mat)
#   if(raw_probs){
#     remove_par <- max(which(x != 0))
#     R <- sweep(R, MARGIN = 1, as.vector(R[, remove_par]))
#   }
#   rhs <- R[, ncol(R), drop = FALSE]
#   R <- R[, -ncol(R), drop = FALSE]
#
#   # Delete eta(s) without variation (which are now all 0; which gives eta_adj);
#   x <<- x[-which_par]
#   # delete corresponding rows and columns in Sigma (which gives Sigma_adj)
#   Sigma <<- Sigma[-which_par, -which_par]
#
#   # and the corresponding columns from the restriction matrix Rm, which leads to Rmadj
#   R_adj <- R[, -which_par, drop = FALSE]
#
#   # and do the check to see if you need to adjust the rhs (which then gives rhs_adj)
#   null_rows <- which(apply(R_adj, 1, function(i){all(i == 0)}))
#   if(length(null_rows) > 0){
#       if(any(rhs[null_rows] > 0)){
#         qadj <- which(apply(R[null_rows, , drop = FALSE], 2, function(j){any(j != 0)}))
#
#         # Fix below
#         G <- diag(1,ncol(hypothesis$hyp_mat[[1]])-1)[null_rows,]
#         H <- rep(0,length(null_rows))
#         q <- ldei(E=R, F=rhs, G = G, H = H)$X
#         q[qadj] <- 0
#         # Overwrite rhs
#         rhs <- R%*%q
#
#       }
#   }
#   new_hypmat <- cbind(R_adj, rhs)
#   lengths <- c(0, sapply(hypothesis$hyp_mat, nrow))
#   hypothesis$hyp_mat <<- lapply(1:length(hypothesis$hyp_mat), function(mat_num){
#     new_hypmat[(sum(lengths[1:mat_num])+1):sum(lengths[1:(mat_num+1)]), ]
#   })
#
# }

# hypothesis_sums_to_one <- function(which_par){
#   hypothesis$hyp_mat <<- lapply(hypothesis$hyp_mat, function(R){
#     sweep(R[, -which_par, drop = FALSE], MARGIN = 1, as.vector(R[, which_par]))
#   })
#   # Drop parameters not in hypothesis
#   x <<- x[-which_par]
#   Sigma <<- Sigma[-which_par, -which_par]
# }


seq_tuples <- function(from = 1, to = from, tuples = 1){
  (((from-1)*tuples)+1):(to*tuples)
}


pos_definite <- function(x, tolerance = 1e-08){
  tryCatch({
    (dim(x)[1] == dim(x)[2]) & all(abs(eigen(x, only.values = TRUE)$values) > tolerance)
  }, error = function(e){
    FALSE
  })
}
