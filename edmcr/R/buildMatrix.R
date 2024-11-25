buildMatrix <- function(UnknownEntries, KnownEntries,Index,NRows){
    
  KnownEntries <- KnownEntries^2
  
  Matrix <- matrix(rep(-1,NRows^2),nrow=NRows)
  Matrix[Index] <- UnknownEntries
  Matrix[lower.tri(Matrix)][which(Matrix[lower.tri(Matrix)] == -1)] <- KnownEntries
  Matrix[upper.tri(Matrix)] <- t(Matrix)[upper.tri(Matrix)]
  diag(Matrix) <- 0
  
  return(Matrix)
  
}