fit_canonical_var <- function(A, p = 1, type = "none"){

    fit <- vars::VAR(A, p = p, type = type)
    
    transition_mat <- as.matrix(do.call("rbind",lapply(seq_along(colnames(A)), function(x) {
      fit$varresult[[x]]$coefficients
    })))
    colnames(transition_mat) <-  rownames(transition_mat) <- colnames(A)
    
    transition_mat_pval <- as.matrix(do.call("rbind",lapply(seq_along(colnames(A)), function(x) {
       coef(fit)[[x]][,"Pr(>|t|)"]
    })))
    colnames(transition_mat_pval) <-  rownames(transition_mat_pval) <- colnames(A)
    
    transition_mat_thresh <- as.matrix(do.call("rbind",lapply(seq_along(colnames(A)), function(x) {
      coef(fit)[[x]][,"Pr(>|t|)"] < 0.05
    })))
    colnames(transition_mat_thresh) <-  rownames(transition_mat_thresh) <- colnames(A)
    
    transition_mat_sigonly <- matrix(0, nrow(transition_mat), ncol(transition_mat))
    transition_mat_sigonly[transition_mat_thresh] <- transition_mat[transition_mat_thresh]
    colnames(transition_mat_sigonly) <-  rownames(transition_mat_sigonly) <- colnames(A)
    
    return(list(
      transition_mat = transition_mat,
      transition_mat_sigonly = transition_mat_sigonly
    ))
        

}