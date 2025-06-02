# Sets up alpha estimation
index.setup <- function(mf, Cmat = NULL, bvec = NULL, control)
{
  # Extract index variables
  mt <- attr(mf, "terms")
  index_interp <- mf[attr(mt, "specials")$g]
  # Labels for smoothing step
  index_labels <- make.names(sapply(index_interp, attr, "label"), unique = TRUE)
  # Extract response
  y <- stats::model.response(mf)
  attr(y, "varname") <- all.vars(mt)[1]
  # Extract weights
  w <- mf$`(weights)`
  if (is.null(w)) w <- rep(1, NROW(y))
  # Organizing variables in indices as a matrix with index positions   
  p <- length(index_interp)
  pvec <- sapply(index_interp, attr, "nterms")
  index <- rep(1:p, pvec)
  Xind <- do.call(cbind, index_interp)
  names(index) <- rep(index_labels, pvec)
  # Number of variables and indices
  ptot <- sum(sapply(index_interp, ncol))
  # Check Cmat
  if (!is.null(Cmat)){
    if (is.vector(Cmat)) Cmat <- t(as.matrix(Cmat))
    if (ncol(Cmat) != ptot){
      stop("Inconsistent number of columns in Cmat")
    }
  } else {
    Cmat <- matrix(nrow = 0, ncol = ptot)
  }
  if (is.null(bvec)) bvec <- 0
  bvec <- rep_len(bvec, nrow(Cmat))
  # Add Cmat provided in index specific specifications
  gCmat <- as.matrix(Matrix::bdiag(lapply(index_interp, attr, "Cmat")))
  Cmat <- rbind(Cmat, gCmat)
  # Add bvec
  bvec <- c(bvec, unlist(lapply(index_interp, attr, "bvec"), use.names = FALSE))
  # Check irreducibility
  if (nrow(Cmat) > 1){
    chkc <- check_cmat(Cmat)
    if (length(chkc) > 0 && control$check.Cmat){
      Cmat <- Cmat[-chkc,]
      bvec <- bvec[-chkc]
      warning(paste0("Redundant constraints were removed: ", 
        paste(chkc, collapse = ", ")))
    }
    
  } else if (nrow(Cmat) == 0) { # Check identifiability
  warning(paste0("The constraint matrix is empty ", 
    "and the model might not be identifiable. ", 
    "It is recommended to leave at least a sign constraint for one ",
    "coefficient per index."))
  }
  # Return
  return(list(y = y, x = Xind, index = index, w = w, Cmat = Cmat, bvec = bvec)) 
}