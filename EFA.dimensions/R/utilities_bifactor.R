


bifactor_engine <- function(loadings, cormat = NULL,  # phi = NULL, 
                            corkind = 'pearson', Ncases = NULL, 
                            bifactor_kind, 
                            schmid_options = list(extraction = 'minres', 
                                                  rotation = 'oblimin', 
                                                  N_group_factors = 3),
                            delta = .01, min_loading = .2) { 
  
  # just one bifactorkind
  if (length(bifactor_kind) > 1) {
    cat('\nbifactor_kind can have only one value. It has been changed to ', bifactor_kind[1])
    bifactor_kind <- bifactor_kind[1]
  }
  
  structureBIF <- phiBIF <- varexplBIF <-
    omega_total <- omega_hierl <- 
    Calpha.z <-
    ECV <- ARPB <- FD <- PUC <- coef_H <- rmsr <- rmsr_gen <- 
    rmsr_psych <- var_partit_mat <- NULL
  
  
  # when loadings are already bifactor loadings
  if (bifactor_kind == 'none')  loadingsBIF <- loadings 
  
  if (bifactor_kind == 'bifactorT')   # orthogonal
    
    loadingsBIF <- GPArotation::bifactorT(loadings)$loadings
  
  if (bifactor_kind == 'bifactorQ') {  # oblique
    
    outp <- GPArotation::bifactorQ(loadings)
    loadingsBIF <- outp$loadings
    phiBIF <- outp$Phi
    structureBIF <- loadingsBIF %*% phiBIF
  }
  
  if (bifactor_kind == 'bigeominT')    # orthogonal
    
    loadingsBIF <- GPArotation::bigeominT(loadings, delta=delta)$loadings
  
  if (bifactor_kind == 'bigeominQ') {  # oblique
    
    outp <- GPArotation::bigeominQ(loadings, delta=delta)
    loadingsBIF <- outp$loadings
    phiBIF <- outp$Phi
    structureBIF <- loadingsBIF %*% phiBIF
  }
  
  if (bifactor_kind == 'SL') {
    
    # Nfactors for bifactor analyses:
    # SL, SLiD, & DSL all do their own factor extractions in which Nfactors = N_group_factors
    # & the general factor is later added to the Nfactors loading matrix in these cases.
    
    schmid_outp <- suppressMessages(
      psych::schmid(model = cormat, 
                    nfactors = schmid_options$N_group_factors, 
                    fm = schmid_options$extraction, 
                    rotate = schmid_options$rotation, 
                    digits=2, n.obs=Ncases, option="equal",
                    Phi=NULL, covar=FALSE, two.ok=FALSE))  #, plot=FALSE)
      
      loadings_SL <- schmid_outp$sl
      
      loadingsBIF <- loadings_SL[, -which(colnames(loadings_SL) %in% c('h2','u2','p2','com'))]
  }
  
  
  if (bifactor_kind == 'SLiD') {
    
    # SLiD_rotation - 2021 Garcia-Garzon - On Omega Hierarchical Estimation - 
    # A Comparison of Exploratory Bi-Factor Analysis Algorithms
    
    loadings_SLiD <- suppressMessages(
      slid_rotation(data = cormat, 
                    n_factors = schmid_options$N_group_factors, 
                    fm = schmid_options$extraction, 
                    rotate = schmid_options$rotation, 
                    max_iter = 100, tol = 1e-5)$bifactor_loadings)
    
    loadingsBIF <- loadings_SLiD
  }
  
  
  if (bifactor_kind == 'DSL') {  
    
    # Direct Schmid Leiman -- psych
    directSl_outp <- suppressMessages(
      psych::directSl(cormat, 
                      nfactors = schmid_options$N_group_factors, 
                      fm = schmid_options$extraction, 
                      rotate = schmid_options$rotation, cut=.3))  #$direct  # $f$loadings
    
    loadings_DSL <- unclass(directSl_outp$direct)
    
    loadingsBIF <- loadings_DSL
  }
  
  colnames(loadingsBIF) <- 
    c('General', c(paste('Group', 1:(ncol(loadingsBIF) - 1), sep=' ')))
  
  
  # item_stats for orthogonal loadings
  if (!bifactor_kind %in% c('bifactorQ', 'bigeominQ')) {
    
    loadingsBIF_sqd <- loadingsBIF^2
    
    h2 <- rowSums(loadingsBIF_sqd)
    
    u2 <- 1 - h2
    
    IECV <- loadingsBIF_sqd[,1] / h2
  }
  
  # item_stats for oblique (pattern) loadings
  if (bifactor_kind %in% c('bifactorQ', 'bigeominQ')) {
    
    # structureBIF <- loadingsBIF %*% phiBIF
    
    pattern_structure <- loadingsBIF * structureBIF
    
    h2 <- rowSums(pattern_structure)
    
    u2 <- 1 - h2
    
    IECV <- pattern_structure[,1] / h2
  }
  
  item_stats <- cbind(h2, u2, IECV)
  colnames(item_stats)[1:2] <- c('Communalities', 'Uniquenesses')
  
  
  # omega
  omega_total <- omega_t(loadingsBIF, min_loading = min_loading) 
  omega_hierl <- omega_h(loadingsBIF, min_loading = min_loading)
  # omega_total <- omega_t(loadingsBIF, cormat) 
  # omega_hierl <- omega_h(loadingsBIF, cormat)
  
  # standardized alpha
  if (!is.null(cormat))  Calpha.z <- Cronbach.alpha.z(cormat)

  # # ECV - Explained Common Variance of the (first) general factor
  # eigenvalues <- colSums(loadingsBIF^2) 
  # ECV <- eigenvalues[1] / sum(eigenvalues)
  
  # ECV - Explained Common Variance of the general & specific factors
  # ECV - general factor
  # 2023 Dueber, Toland - A Bifactor Approach to Subscore Assessment  p 224 formula 3
  ECV <- ( sum(loadingsBIF[,1]^2) / ( sum(loadingsBIF[,1]^2) + 
                                        sum( colSums(loadingsBIF[,2:ncol(loadingsBIF)]^2) ) ) )
  # ECV - Explained Common Variance for the specific factors
  # 2023 Dueber, Toland - A Bifactor Approach to Subscore Assessment  p 224 formula 6
  for (lupe in 2:ncol(loadingsBIF)) {
    group_TF <- abs(loadingsBIF[,lupe]) >= min_loading
    dum <- cbind( loadingsBIF[group_TF,1], loadingsBIF[group_TF,lupe])
    # 2023 Dueber, Toland - A Bifactor Approach to Subscore Assessment  p 224 formula 4
    ECV <- c(ECV, ( sum(dum[,2]^2) / ( sum(dum[,1]^2) + sum(dum[,2]^2)) ) )
  }
  # print( ECV )
  # bifactorIndices(Lambda)
  
  
  # # IECV - Explained Common Variance of the (first) general factor for each item
  # loadingsBIF_sqd <- loadingsBIF^2
  # IECV <- loadingsBIF_sqd[,1] / rowSums(loadingsBIF_sqd)
  
  
  # PUC    not using, it is based on counts of 0-value loadings, as in CFA
  min_loading <- .1
  # the number of items on each factor that are >= min_loading
  N_items_ge_min <- colSums( abs(loadingsBIF) >= min_loading)
  Nitems <- nrow(loadingsBIF)
  N_correls_tot <- (Nitems*(Nitems-1) / 2)
  N_correls_contaminated <- sum(N_items_ge_min * (N_items_ge_min - 1) / 2) -
    N_correls_tot
  PUC <- 1 - N_correls_contaminated / N_correls_tot
  
  
  # ARPB -- Average Relative Parameter Bias
  if (!is.null(cormat)) {
    loadingsBIF_gen <- loadingsBIF[,1]
    loadingsBIF_unid <- EFA.dimensions::EFA(data=cormat, 
                                            extraction = schmid_options$extraction,  
                                            corkind=corkind, Ncases=Ncases, Nfactors = 1, 
                                            rotation='none', verbose=FALSE)$loadingsNOROT
    ARPB <- mean(abs((loadingsBIF_unid - loadingsBIF_gen) / loadingsBIF_gen))
  }
  
  
  # FD -- factor determinacy index
  # phi is a matrix of factor intercorrelations. For the bifactor model,
  # this matrix always will have ones on the diagonal and zeros elsewhere
  phiBIF <- diag(1, nrow = ncol(loadingsBIF))
  cormat_reprod <- reproduced_R(loadingsBIF)
  FD <- sqrt(diag(phiBIF %*% t(loadingsBIF) %*% solve(cormat_reprod) %*% loadingsBIF %*% phiBIF))
  
  
  # coefficient H -- using the general factor loadings
  coef_H <- coefficient_H(loadings=loadingsBIF)
  # H <-  1/(1+1/(colSums(loadingsBIF^2/(1-loadingsBIF^2))))
  
  
  # rmsr
  if (!is.null(cormat)) {
    
    cormat_reproduced <- reproduced_R(loadingsBIF)
    
    # rmsr for the factor model (general + group factors)
    rmsr <- RMSR_boc(cormat, cormat_reproduced)
    # residuals <- cormat - cormat_reproduced
    # residuals.upper <- as.matrix(residuals[upper.tri(residuals, diag = FALSE)])
    # rmsr <- sqrt(mean(residuals.upper^2)) # rmr is perhaps the more common term for this stat
    
    # rmsr for the general factor-only model
    rmsr_gen <- RMSR_boc(cormat, reproduced_R(loadingsBIF[,1, drop=FALSE]))
    
    # rmsr - psych
    residuals <- cormat - cormat_reproduced
    rstar.off <- sum(residuals^2)/2
    n <- nrow(loadingsBIF)  #number of variables
    rmsr_psych <- sqrt(rstar.off/(n*(n-1)))  #this is the empirical rmsea
  }
  
  
  # variance partitioning -- from "GPA3bifactor - Bernaards.pdf"
  loadingsBIF_gen    <- loadingsBIF[, 1]
  loadingsBIF_group  <- loadingsBIF[, -1]
  theta <- 1 - rowSums(loadingsBIF^2)
  denom <- sum(loadingsBIF_gen)^2 + sum(loadingsBIF_group^2) + sum(theta)
  
  var_partit_mat <- sum(loadingsBIF_gen)^2 / denom
  for (j in 1:ncol(loadingsBIF_group)) 
    var_partit_mat <- rbind(var_partit_mat, (sum(loadingsBIF_group[, j]^2) / denom) )
  var_partit_mat <- rbind(var_partit_mat, (sum(theta) / denom) )
  
  for (lupe in 1:nrow(var_partit_mat)) {
    
    if (lupe == 1) rownames(var_partit_mat)[1] <- 'General factor   '
    
    if (lupe > 1 & lupe < nrow(var_partit_mat))
      rownames(var_partit_mat)[lupe] <- paste('Group factor', (lupe-1), '  ')
    
    if (lupe == nrow(var_partit_mat)) 
      rownames(var_partit_mat)[lupe] <- 'Measurement error'
  }
  
  if (!is.null(cormat))
  varexplBIF <- VarianceExplained(eigenvalues = eigen(cormat)$values, 
                                       loadingsROT = loadingsBIF, phi = phiBIF)
  
  
  output <- list(omega_total = omega_total, omega_hierl = omega_hierl,
                 Calpha.z = Calpha.z,
                 ECV = ECV, ARPB = ARPB, FD = FD, coef_H = coef_H, PUC = PUC,
                 rmsr = rmsr, rmsr_gen = rmsr_gen, rmsr_psych = rmsr_psych, 
                 var_partit_mat = var_partit_mat, 
                 loadingsBIF = loadingsBIF, structureBIF = structureBIF, 
                 phiBIF = phiBIF, varexplBIF = varexplBIF, 
                 item_stats = item_stats, bifactor_kind = bifactor_kind,
                 min_loading = min_loading)
  
  return(invisible(output))
}




show_bifactor_stats <- function(bifactor_output) {
  
  # cat('\n\nbifactor_kind = ', bifactor_output$bifactor_kind, '\n')
  
  cat('\n\nBifactor Model Statistics:')	
  
  cat('\n\n   Omega total =', round(bifactor_output$omega_total[1],2),
      '\n\n   Omega hierarchical =', round(bifactor_output$omega_hierl[1],2) )
  
  if (!is.null(bifactor_output$Calpha.z))
    cat('\n\n   Cronbach alpha =', round(bifactor_output$Calpha.z,2))
  
  if (!is.null(bifactor_output$ARPB))
    cat('\n\n   Average relative parameter bias (ARPB) =', round(bifactor_output$ARPB,2))
  
  if (!is.null(bifactor_output$PUC))
    cat('\n\n   Percent of uncontaminated correlations (PUC) =', round(bifactor_output$PUC,2))
  
  if (!is.null(bifactor_output$rmsr))
    cat('\n\n   Root mean square of the residuals using the general & group factors =', round(bifactor_output$rmsr,2))
  
  if (!is.null(bifactor_output$rmsr_gen))
    cat('\n\n   Root mean square of the residuals using only the general factor =', round(bifactor_output$rmsr_gen,2))

    
  cat('\n\nmin_loading =', bifactor_output$min_loading, 
      ' (which is important for PUC and for omega and ECV group factor statistics)\n')

    
  cat('\n\n\nBifactor Factor Statistics:\n')	
  factor_stats <- rbind(bifactor_output$omega_total, bifactor_output$omega_hierl, 
                        bifactor_output$ECV, bifactor_output$FD, bifactor_output$coef_H)
  rownames(factor_stats) <- c('Omega total', 'Omega hierarchical',
                              'Explained Common Variance', 'Factor Determinacy', 'coefficient H')
  colnames(factor_stats) <- c('General', c(paste('Group', 1:(ncol(bifactor_output$loadings) - 1), sep=' ')))
  # print(round(factor_stats, 2), print.gap=3)
  writeLines(paste0("   ", capture.output(print(round(factor_stats,2), print.gap=3))))
  
  
  cat('\n\nBifactor Item Statistics:\n\n')	
  print(round(bifactor_output$item_stats,3), print.gap=4)
  
  
  # cat('\n\nBifactor Partitions of the Total Score Variance:\n')	
  # var_partit_mat <- round(bifactor_output$var_partit_mat, 3)
  # for (lupe in 1:nrow(var_partit_mat)) 
  #   cat("\n   ", rownames(var_partit_mat)[lupe], '  ', var_partit_mat[lupe,])
  # 
  # cat('\n\n    The variance partition results indicate that ', var_partit_mat[1,]*100,
  #     '% of the total score', sep = '')
  # cat('\n    variance could be attributable to the general factor, with the group')
  # cat('\n    factors together contributing ',
  #     sum( c(var_partit_mat[2:(nrow(var_partit_mat)-1),]))*100,
  #     '% and measurement error ', var_partit_mat[nrow(var_partit_mat),]*100, '%.', sep = '')
  
  #  The variance explained by each factor may be informative: the general factor accounts for
  # approximately 56.3% of item variance, while the two group factors account for 6.5% and 7.9%
  # respectively, for a total of 70.7%. 
  
  if (!is.null(bifactor_output$varexplBIF)) {
    cat('\n\n\nEigenvalues and Proportions of Total Variance Explained:\n')
    cat('\n               Initial            Bifactor\n')  
    print(bifactor_output$varexplBIF, print.gap=2)
  }
  
}  





# coefficient H is a measurement of construct replicability and is
# defined as the extent to which a set of items represents a latent
# variable (Hancock & Mueller, 2001)
coefficient_H <- function(loadings=NULL, cormat=NULL, corkind='pearson',
                          Ncases=NULL, extraction=NULL) {
  
  if (is.null(loadings)) {
    
    efa_output <- EFA.dimensions::EFA(data=cormat, extraction = extraction, 
                                      corkind=corkind, Ncases=Ncases, Nfactors = 1, 
                                      rotation='none', verbose=FALSE)
    
    loadings <- efa_output$loadingsNOROT
    # errors <- 1 - efa_output$communalities
  }
  
  # 2017 McNeish p 418
  proportions <- (loadings^2) / (1 - loadings^2)
  if (is.vector(proportions)) dim(proportions) <- c(length(proportions), 1)
  # sum_of_props <- sum(proportions)
  sum_of_props <- colSums(proportions)
  coef_H <- sum_of_props / (1 + sum_of_props)
  
  # # reliable package approach
  # Lambda <- loadings
  # Psi <- diag(as.vector(errors))
  # numerator <- crossprod(Lambda * 1 / diag(Psi), Lambda)
  # H <- unname( numerator / (1 + numerator) )
  
  # H <-  1/(1+1/(colSums(loadings^2/(1-loadings^2))))
  
  return(invisible(coef_H))
}




omega_t <- function(loadings, min_loading = .2) {
  
  loadings_group <- loadings[, -1]
  
  theta <- 1 - rowSums(loadings^2)  # model-implied unique variances
  
  # for the general factor
  outp <- sum(colSums(loadings)^2) / (sum(colSums(loadings)^2) + sum(theta))
  
  # for the group factors
  for (lupe in 1:ncol(loadings_group)) {
    
    group_TF <- abs(loadings_group[,lupe]) >= min_loading
    
    dum <- cbind( loadings[group_TF,1], loadings_group[group_TF,lupe])
    
    # 2023 Dueber, Toland - A Bifactor Approach to Subscore Assessment  p 224 formula 4
    outp <- c(outp, 
              ( sum(dum[,1])^2 + sum(dum[,2])^2 ) / 
                ( sum(dum[,1])^2 + sum(dum[,2])^2 + sum((theta*group_TF)) ) )
  }
  return(invisible(outp))
}



omega_h <- function(loadings, min_loading = .2) {

  loadings_group <- loadings[, -1]

  theta <- 1 - rowSums(loadings^2)  # model-implied unique variances
  
  # for the general factor
  outp <- sum(loadings[,1])^2 / (sum(colSums(loadings)^2) + sum(theta))
  
  # for the group factors
  for (lupe in 1:ncol(loadings_group)) {
    
    group_TF <- abs(loadings_group[,lupe]) >= min_loading
    
    dum <- cbind( loadings[group_TF,1], loadings_group[group_TF,lupe])

    # 2023 Dueber, Toland - A Bifactor Approach to Subscore Assessment  p 224 formula 5
    outp <- c(outp, 
              ( sum(dum[,2])^2 / 
                ( sum(dum[,1])^2 + sum(dum[,2])^2 + sum(theta*group_TF)) ) )
  }
  return(invisible(outp))
}





# AI Overview for  r code for Schmid-Leiman Iterative Bi-factor Difference- based Target Rotation Algorithm
# 
# The Schmid-Leiman Iterative Bi-factor Difference-based Target Rotation Algorithm (SLiD) 
# is a state-of-the-art exploratory factor analysis method used to establish an 
# empirical, factor-specific cutoff for defining target matrices based on loading 
# differences.Because this algorithm relies on evaluating the largest drop 
# (one-lagged difference) in sorted squared loadings, you can implement the 
# full SLiD procedure in R using the base framework, psych package, and GPArotation 
# package. Complete R Implementation of SLiDCopy and execute this complete, 
# self-contained R script. It defines the SLiD algorithm function and applies it to 
# a sample datase.

# Algorithm Breakdown
# 
# Initial Alignment: The function utilizes psych::schmid to generate the foundational 
# hierarchical structure from first-order oblique factors.
# 
# Empirical Cutoff Rule: Instead of enforcing arbitrary cross-loading thresholds 
# (e.g., 0.10 or 0.20), SLiD dynamically evaluates individual factor vectors. 
# It maps out the sharpest mathematical drop in sorted squared loadings to 
# differentiate true indicator items from background noise.
# 
# Partially Specified Target Rotation: Using the computed GPArotation::pstQ system, 
# non-salient paths are held to zero while dominant indicators and cross-loadings 
# are left unconstrained (NA), resulting in a cleaner bi-factor structure.
# 
# To customize the script for your workflow, let me know:
#   Are your factors expected to be orthogonal or oblique?
#   What is the sample size and number of items in your dataset?
#   Are you planning to compare this against standard Direct Schmid-Leiman (DSL) or CFA implementations?

# ?? Google AI got the code from 
# "2019 - Improving Bi-Factor Exploratory Modeling Empirical Target Rotation Based on Loading Differences" ??


# -------------------------------------------------------------------------
# R Implementation: Schmid-Leiman Iterative Difference-Based Target Rotation (SLiD)
# -------------------------------------------------------------------------

#' SLiD: Schmid-Leiman Iterative Bi-factor Difference-based Target Rotation
#' 
#' @param data A matrix or dataframe of observed variables, or a correlation matrix.
#' @param n_factors Number of group/specific factors to extract.
#' @param max_iter Maximum number of outer target matrix update iterations.
#' @param tol Convergence tolerance (change in target matrix elements).
#'
slid_rotation <- function(data, n_factors = 3, max_iter = 100, tol = 1e-5,
                          fm = 'minres', rotate='oblimin') {
  
  # Ensure we have a correlation matrix
  if (is.matrix(data) && all(diag(data) == 1) && isSymmetric(data)) {
    R <- data
  } else {
    R <- cor(data, use = "pairwise.complete.obs")
  }
  
  # Step 1: Compute initial baseline Schmid-Leiman solution
  # This serves as the starting point for group factors
  sl_init <- psych::schmid(R, nfactors = n_factors, 
                           fm = fm, rotate=rotate, plot=FALSE)
  
  # Extract initial group factor loadings (exclude general factor column)
  # psych::schmid returns general factor in column 1, group factors in subsequent columns
  group_loadings <- sl_init$sl[, 2:(n_factors + 1)]
  p <- nrow(group_loadings)
  
  # Initialize the Target matrix (NA means unrestricted/free parameter)
  target_matrix <- matrix(NA, nrow = p, ncol = n_factors)
  old_target <- target_matrix
  
  # Main SLiD Iterative Loop
  for (iter in 1:max_iter) {
    
    # Calculate empirical cutoffs for each factor based on one-lagged differences
    for (j in 1:n_factors) {
      loadings_j <- group_loadings[, j]
      sq_loadings <- loadings_j^2
      
      # Sort squared loadings in descending order
      sorted_idx <- order(sq_loadings, decreasing = TRUE)
      sorted_sq <- sq_loadings[sorted_idx]
      
      # Compute first prominent one-lagged difference
      differences <- -diff(sorted_sq)
      
      if (length(differences) > 0) {
        max_diff_idx <- which.max(differences)
        # Cutoff is the average threshold separating major and minor loads
        cutoff_val <- sqrt(sorted_sq[max_diff_idx + 1])
      } else {
        cutoff_val <- 0.10 # Fallback default
      }
      
      # Construct target entries for factor j
      # Values below the empirical difference cutoff are targeted to 0
      target_matrix[, j] <- ifelse(abs(loadings_j) <= cutoff_val, 0, NA)
    }
    
    # Handle absolute strictness check for convergence
    if (iter > 1 && all(is.na(target_matrix) == is.na(old_target)) && 
        all(target_matrix[!is.na(target_matrix)] == old_target[!is.na(old_target)])) {
      # cat("SLiD Converged successfully at iteration:", iter, "\n")
      break
    }
    
    old_target <- target_matrix
    
    # Apply Partially Specified Procrustes/Target Rotation (pstQ or pstT) via GPArotation
    # We rotate the group factor space conditionally based on our target matrix
    tryCatch({
      rotated_solution <- GPArotation::pstQ(group_loadings, target = target_matrix)
      group_loadings <- rotated_solution$loadings
    }, error = function(e) {
      # warning("Target rotation matrix issue encountered during iteration ", iter, ". Using last stable state.")
    })
  }
  
  # Construct final Bifactor Matrix output layout
  # Re-attach general factor from baseline for complete structural representation
  final_bifactor_matrix <- cbind(sl_init$sl[, 1, drop = FALSE], group_loadings)
  colnames(final_bifactor_matrix) <- c("General", paste0("Group_", 1:n_factors))
  
  return(list(
    bifactor_loadings = final_bifactor_matrix,
    final_target = target_matrix,
    iterations = iter
  ))
}





