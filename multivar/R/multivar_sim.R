#' Simulate multivar data.
#'
#' @param k Integer. The number of individuals (or datasets) to be generated.
#' @param d Integer. The number of variables per dataset. For now this will be constant across individuals. 
#' @param n Integer. The time series length. 
#' @param prop_fill_com Numeric. The proportion of nonzero paths in the common transition matrix.
#' @param prop_fill_ind Numeric. The proportion of nonzero unique (not in the common transition matrix or transition matrix of other individuals) paths in each individual transition matrix.
#' @param ub Numeric. The lower bound for individual elements of the transition matrices.
#' @param lb Numeric. The upper bound for individual elements of the transition matrices.
#' @param sigma Matrix. The (population) innovation covariance matrix.
#' @param unique_overlap Logical. Default is FALSE. Whether the unique portion should be completely unique (no overlap) or randomly chosen.
#' @param mat_common Matrix. A common effects transition matrix (if known).
#' @param mat_unique List. A list of unique effects transition matrix (if known).
#' @param mat_total List. A list of total effects transition matrix (if known).
#' @param diag Logical. Default is FALSE. Should diagonal elements be filled first for common elements.
#' @keywords var multivar simulate
#' @examples
#' k <- 3
#' d <- 10
#' n <- 20
#' prop_fill_com <- .1
#' prop_fill_ind <- .05
#' lb <- 0.1
#' ub <- 0.5
#' sigma <- diag(d)
#' data <- multivar_sim(k, d, n, prop_fill_com, prop_fill_ind, lb, ub,sigma)$data
#' @export
multivar_sim <- function(
  k, 
  d, 
  n, 
  prop_fill_com, 
  prop_fill_ind, 
  lb, 
  ub,
  sigma, 
  unique_overlap = FALSE, 
  mat_common = NULL, 
  mat_unique = NULL, 
  mat_total = NULL, 
  diag = FALSE){
  

  if (is.null(mat_common) & is.null(mat_total)){
  
    d.fun                 <- function(n) runif(n,lb,ub)
    densities             <- c(prop_fill_com, rep(prop_fill_ind,k))        
    n_elem_prop_fill_com  <- round(d^2*prop_fill_com) 
    n_elem_prop_fill_ind  <- round(d^2*prop_fill_ind) 
    n_elem_total_all_subj <- n_elem_prop_fill_ind*k
    n_elem_total_all      <- n_elem_total_all_subj + n_elem_prop_fill_com
    
    if(!unique_overlap & n_elem_total_all_subj > (d^2 - n_elem_prop_fill_com)){
      stop(paste0("multivar ERROR: The arguments prop_fill_com and prop_fill_ind are not well specified."))
    }
    
    max_eigs <- TRUE
    
    while(max_eigs){
      if(diag){
        vec_A             <- c(matrix(c(1:d^2),d,d))    # indices for elements of A
        vec_A_diag        <- diag(matrix(c(1:d^2),d,d)) # indices for diagonal elements of A
        vec_A_nondiag     <- setdiff(vec_A,vec_A_diag)  # indices for non-diagonal elements of A
        permuted_elements <- c(vec_A_diag,sample(vec_A_nondiag))
      } else {
        permuted_elements <- sample(c(1:d^2))
      }
      
      if(n_elem_prop_fill_com == 0 & n_elem_prop_fill_ind != 0){
        
        idx_grp <- 0
        if(!unique_overlap){
          sub_elements <- permuted_elements[(1):(n_elem_total_all_subj)]
          idx_sub <- split(sub_elements, ceiling(seq_along(sub_elements)/n_elem_prop_fill_ind))
        } else {
          sub_elements <- permuted_elements[(1):length(permuted_elements)]
          idx_sub <- lapply(1:k, function(v){sample(sub_elements,size = n_elem_prop_fill_ind)})
        }
        idx_all <- append(list(idx_grp),unname(idx_sub))
        mats <- replicate(k+1, matrix(0,d,d), simplify = FALSE)
        mats <- lapply(seq_along(mats), function(i){
          if(i == 1){
            mats[[i]]
          } else {
            mats[[i]][idx_all[[i]]] <- runif(length(idx_all[[i]]), lb, ub)
            mats[[i]]
          }
        })
        
      } else if (n_elem_prop_fill_com != 0 & n_elem_prop_fill_ind == 0){
        
        idx_grp <- permuted_elements[1:n_elem_prop_fill_com]
        idx_all <- list(idx_grp)
        mats <- replicate(k+1, matrix(0,d,d), simplify = FALSE)
        mats[[1]][idx_all[[1]]]<- d.fun(length(idx_all[[1]]))
        mats <- lapply(seq_along(mats), function(i){
          if(i == 1){
            mats[[i]]
          } else {
            mats[[i]] <-  mats[[1]]
            mats[[i]]
          }
        })
        
      } else if (n_elem_prop_fill_com != 0 & n_elem_prop_fill_ind != 0){
        
        idx_grp <- permuted_elements[1:n_elem_prop_fill_com]
        if(!unique_overlap){
          sub_elements <- permuted_elements[(n_elem_prop_fill_com+1):(n_elem_prop_fill_com+n_elem_total_all_subj)]
          idx_sub <- split(sub_elements, ceiling(seq_along(sub_elements)/n_elem_prop_fill_ind))
        } else {
          sub_elements <- permuted_elements[(n_elem_prop_fill_com+1):length(permuted_elements)]
          idx_sub <- lapply(1:k, function(v){sample(sub_elements,size = n_elem_prop_fill_ind)})
        }
        idx_all <- append(list(idx_grp),unname(idx_sub))
        mats <- replicate(k+1, matrix(0,d,d), simplify = FALSE)
        mats[[1]][idx_all[[1]]]<- d.fun(length(idx_all[[1]]))
        mats <- lapply(seq_along(mats), function(i){
          if(i == 1){
            mats[[i]]
          } else {
            mats[[i]][idx_all[[i]]] <- runif(length(idx_all[[i]]), lb, ub)
            mats[[i]] <- mats[[i]] + mats[[1]]
            mats[[i]]
          }
        })
        
      } else {
         stop(paste0("multivar ERROR: One of the arguments prop_fill_com and prop_fill_ind must be nonzero."))
      }

      max_eigs <- any(unlist(lapply(mats[-1], function(x){max(abs(eigen(x)$values))})) > .95)
    }
    
    data <- lapply(mats[-1], function(x) {var_sim(n, x, sigma)})
    data <- lapply(data, function(df){colnames(df) <- paste0("V",1:ncol(df)); df})
    
    mat_list <- list(
      mat_com        = mats[[1]],
      mat_ind_unique = lapply(1:k, function(i){mats[[i+1]] - mats[[1]]}),
      mat_ind_final  = mats[-1],
      data = data
    )
  
    
  } else {
    
    if(is.null(mat_total)){
      mat_total <- lapply(1:length(mat_unique), function(i){mat_common + mat_unique[[i]]})
    }
    
    data <- lapply(mat_total, function(x) {var_sim(n, x, sigma)})
    data <- lapply(data, function(df){colnames(df) <- paste0("V",1:ncol(df)); df})
    
    mat_list <- list(
      mat_com        = mat_common,
      mat_ind_unique = mat_unique,
      mat_ind_final  = mat_total,
      data = data
    )
    
    
  }
  
  return(mat_list)
  
}

