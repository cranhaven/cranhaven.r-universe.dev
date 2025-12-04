distShell <-
function(DATA, FUNC, diag=FALSE, upper=FALSE, ...) {
  # Distance matrix computation using a user-defined distance measure.
  #
  # Args:
  #   DATA: a matrix-like object with variables in COLUMNS, cases in ROWS.
  #   FUNC(x1, x2, ...): the distance function; takes two vector arguments,
  #     returns a single scalar distance measure.
  #   diag, upper: logical values indicating whether the diagonal or upper
  #     triangle of the distance matrix should be printed by print.dist.
  #   ...: additional arguments passed to FUNC.
  #
  # Returns:
  #   An object of class 'dist', ie. the lower triangle of the distance 
  #     matrix stored by columns in a vector. 

  call <- match.call()
# Convert to a matrix, do sanity checks.
  DAT <- as.matrix(DATA)
  if(!is.numeric(DAT))
    stop("Argument DAT must be numeric.")
  if(any(dim(DAT) < 2))
    stop("DAT must have at least 2 columns and 2 rows.")
  cases <- dim(DAT)[1]       # number of cases, ie. sites, samples, quadrats,...

# Set up the output matrix:
  OP <- matrix(NA, nrow=cases, ncol=cases)  # the output matrix
  rownames(OP) <- colnames(OP) <- rownames(DAT)

# Calculate the index for each pair of cases; the i loop does rows, the
#  j loop cols.
  for(i in 1:(cases-1))
    for(j in (i+1):cases)
      OP[i,j] <- FUNC(DAT[i,], DAT[j,],...)

# Return the result as an object of type 'dist' with an additional
# attribute giving the call (important as a record of the FUNC used).
  tmp <- as.dist(t(OP), diag=diag, upper=upper)
  attr(tmp, "call") <- call
  return(tmp)
}
