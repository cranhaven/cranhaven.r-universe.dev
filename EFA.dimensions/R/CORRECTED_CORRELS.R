



CORRECTED_CORRELS <- function(cormat, Ncases = 100, alphas, 
                              systematic=FALSE, Nperms = 1000, verbose=TRUE) {
  
  Nvars <- nrow(cormat)
  
  if (ncol(cormat) != nrow(cormat))
    message('\nThe entered correlation matrix is not square. Expect problems.')
  
  if (ncol(cormat) != length(alphas)) {
    message('\nThe number of variables in the correlation matrix is not equal')
    message('to the number of values in alphas. Expect problems.')
  }
  
  corxd <- matrix(NA, Nvars, Nvars)
  
  cor_both <- cormat
  
  correls_orig_vector <- correls_corxd_vector <- comp_noms <- comp_alphas <- c()
  
  for (lupe1 in 1:(Nvars-1)) {
    for (lupe2 in (lupe1+1):(ncol(cormat))) {
      
      corxd[lupe1, lupe2] <- cormat[lupe1, lupe2] / sqrt( alphas[lupe1] * alphas[lupe2] )
      
      corxd[lupe2, lupe1] <- corxd[lupe1, lupe2]
      
      cor_both[lupe1, lupe2] <- corxd[lupe1, lupe2]
      
      correls_orig_vector <- c(correls_orig_vector, cormat[lupe1, lupe2])
      
      correls_corxd_vector <- c(correls_corxd_vector, corxd[lupe1, lupe2])
      
      comp_noms <- rbind(comp_noms, cbind(rownames(cormat)[lupe1], rownames(cormat)[lupe2]))
      
      comp_alphas <- rbind(comp_alphas, cbind(alphas[lupe1], alphas[lupe2]))
    }
  }
  diag(corxd) <- 1
  rownames(corxd) <- colnames(corxd) <- colnames(cormat)
  
  resids <- cormat - corxd
  
  
  # input data and results in row format
  data_in_rows <- data.frame(V1=comp_noms[,1], alpha_1=comp_alphas[,1], 
                             V2=comp_noms[,2], alpha_2=comp_alphas[,2], 
                             r_orig=correls_orig_vector, 
                             r_corrected=round(correls_corxd_vector,2), 
                             difference=round((correls_orig_vector - correls_corxd_vector),2))
  
  if (Nvars > 2) {
    
    cor_spearman <- cor.test(correls_orig_vector, correls_corxd_vector, 
                             method = "spearman", alternative = "greater")$estimate
    cor_kendall <- cor.test(correls_orig_vector, correls_corxd_vector, 
                            method = "kendall", alternative = "greater")$estimate
    
    # permutation tests for Spearman's rho & Kendall's tau
    rhos <- taus <- c()
    
    keeper_pos <- which(lower.tri(cormat))
    keeper_pos <- setdiff(keeper_pos, which(corxd == 0))
    
    cormat_orig_vector <- cormat[keeper_pos]
    
    num_permutations <- choose(Nvars, Nvars) * factorial(Nvars)
    
    if (!systematic & (num_permutations <= Nperms)) {
      
      systematic <- TRUE
      
      message('\nNotice:')
      message('The number of possible permutations for ', Nvars, ' variables ')
      message('is less than Nperms (', Nperms, '). Systematic matrix permutations')   
      message('will therefore be conducted instead of random data permutations.')
    }
    
    if (systematic) {
      
      cat('\nThe number of permutations for ', Nvars, ' variables = ', num_permutations, '\n')
      
      if (num_permutations > 1000000) {
        message('\nNotice:')
        message('The number of systematic matrix permutations is over one million.')
        message('Either wait for the analyses to run, or Stop and switch to')
        message('systematic = FALSE, for random data permutations.')
      }
      
      all_combins <- perm(c(1:Nvars))
      
      for (lupe in 1:nrow(all_combins)) {
        
        shuffled_corxd_mat <- corxd[all_combins[lupe,], all_combins[lupe,]]
        
        shuffled_corxd_vec <- shuffled_corxd_mat[keeper_pos] 
        
        cor_spearman_perm <- cor.test(cormat_orig_vector, shuffled_corxd_vec,
                                      method = "spearman")$estimate
        
        cor_kendall_perm  <- cor.test(cormat_orig_vector, shuffled_corxd_vec,
                                      method = "kendall")$estimate
        
        rhos <- c(rhos, cor_spearman_perm)
        taus <- c(taus, cor_kendall_perm)
      }
    }
    
    if (!systematic) {
      
      for (lupe in 1:Nperms) {
        
        samp_pos <- sample.int(Nvars)
        
        shuffled_corxd_mat <- corxd[samp_pos, samp_pos]
        
        shuffled_corxd_vec <- shuffled_corxd_mat[keeper_pos] 
        
        cor_spearman_perm <- cor.test(cormat_orig_vector, shuffled_corxd_vec,
                                      method = "spearman")$estimate
        
        cor_kendall_perm  <- cor.test(cormat_orig_vector, shuffled_corxd_vec,
                                      method = "kendall")$estimate
        
        rhos <- c(rhos, cor_spearman_perm)
        taus <- c(taus, cor_kendall_perm)
      }
    }
    
    # Monte Carlo p  http://www.ncbi.nlm.nih.gov/pmc/articles/PMC379178/
    num_rand_rhos_gteq_rho <- sum(rhos >= cor_spearman)
    perm_p_rho <- (num_rand_rhos_gteq_rho + 1) / (length(rhos) + 1)
    
    num_rand_taus_gteq_tau <- sum(taus >= cor_kendall)
    perm_p_tau <- (num_rand_taus_gteq_tau + 1) / (length(taus) + 1)
  }
  
  # the Steiger test
  p1 <- psych::cortest.normal(R1 = cormat, R2 = corxd, n1 = Ncases, n2 = Ncases, fisher = TRUE) 
  
  # the Jennrich test
  p3 <- psych::cortest.jennrich(R1 = cormat, R2 = corxd, n1 = Ncases, n2 = Ncases)
  
  
  output <- list(cormat=cormat, alphas=alphas, corxd=corxd, resids=resids, 
                 data_in_rows=data_in_rows,
                 cor_spearman=cor_spearman, cor_kendall=cor_kendall,
                 Steiger_test=p1, Jennrich_test=p3)
  
  
  if (verbose) {
    
    cat('\nThe number of cases for the input correlations = ', Ncases)
    
    cat('\n\nThe input correlations:\n\n')
    print(round(cormat,2),2)
    
    cat('\nThe input alphas:\n\n')
    names(alphas) <- rownames(cormat)
    print(alphas)
    
    cat('\nThe corrected-for-attenuation correlations:\n\n')
    print(round(corxd,2))
    
    cat('\nCorrected-for-attenuation correlations above the diagonal,')
    cat('\nuncorrected correlations below the diagonal:\n\n')
    print(round(cor_both,2))
    
    cat('\nThe residuals correlation matrix:\n\n')
    print(round(resids,2), print.gap=4)
    
    cat('\nInput data and results in row format:\n\n')
    colnames(data_in_rows)[c(1,3)] <- c('','')
    colnames(data_in_rows)[c(2,4)] <- c('alpha','alpha')
    print(data_in_rows, print.gap=3)
    
    if (Nvars > 2) {
      
      cat('\nRank correlations between the original & corrected correlations,')
      cat('\nwith permutation tests of significance:\n') 
      cat("\n   Spearman's rho = ", round(cor_spearman,2), '    p = ', round(perm_p_rho,6))
      cat("\n   Kendall's tau  = ", round(cor_kendall,2),  '    p = ', round(perm_p_tau,6))
      
      if (systematic)
        cat('\n\nThe permutation tests were based on systematic matrix permutations.')
      
      if (!systematic) {
        cat('\n\nThe permutation tests were based on random matrix permutations.')
        cat('The number of random matrix permutations (Nperms) = ', Nperms)
      }
    }
    
    cat('\n\nTests of whether the corrected and uncorrected correlation matrices are equal:\n')
    
    cat('\n   Steiger test:   Chisq = ', round(p1$chi,2), '   df = ', 
            round(p1$df,2), '   p = ', round(p1$prob,6), 
            '   RMSEA = ', round(p1$RMSEA,2), '   SRMR = ', round(p1$SRMR,2))
    
    cat('\n   Jennrich test:  Chisq = ', round(p3$chi2,2), '   p = ', round(p3$prob,6), '\n')
  }
  
  return(invisible(output))
}


# from Google AI  note: does not work with return(invisible(X)), & which is not necess
perm <- function(v) {
  n <- length(v)
  if (n == 1) v
  else {
    X <- NULL
    for (i in 1:n) {
      X <- rbind(X, cbind(v[i], perm(v[-i])))
    }
    X
  }
}

