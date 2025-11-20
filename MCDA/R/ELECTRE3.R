Concordance <- function(scores, q, p, w ){
  
  ## feasability checks for "scores"
  if (!is.matrix(scores)) 
    stop("The performance scores table must be provided as a matrix")
  
  numOptions = nrow(scores);
  numCriteria= ncol(scores);
  ## if weights are not provided, then assign equal weight to all criteria
  if(is.null(w) || !is.vector(w))
    w = rep(1.0/numCriteria, numCriteria)
  
  ## dimensions of scores should be consistent with q,p,v,w
  if(numCriteria != length(q))
    stop("The indifference thresholds q dimensions mismatch with the table of scores")
  if(numCriteria != length(p))
    stop("The preference thresholds p dimensions mismatch with the table of scores")
  if(numCriteria != length(w))
    stop("The weights vector w dimensions mismatch with the table of scores")
  
  ## partial concordance Cx = p-(b-a) / (p-q)
  Cx <- array(NA, c(numOptions,numOptions,numCriteria))
  for(k in 1:numCriteria){
    for(j in 1:numOptions){
      for(i in 1:numOptions){
        Cx[i,j,k] <- (p[k] - (scores[j,k]-scores[i,k]) )/(p[k]-q[k])
      }
    }
  }
  ## clip the values above 1 and below 0
  for(k in 1:numCriteria){
    for(j in 1:numOptions){
      for(i in 1:numOptions){
        Cx[i,j,k] <- ifelse(Cx[i,j,k]>1, 1, ifelse(Cx[i,j,k]<0, 0,Cx[i,j,k]) ) 
      }
    }
  }
  ## overall concordance
  concordance <- array(NA, c(numOptions,numOptions))
  for(i in 1:numOptions){
    for(j in 1:numOptions){
      concordance[i,j] <- 0
      for(k in 1:numCriteria){
        concordance[i,j] = concordance[i,j] + (w[k]*Cx[i,j,k])
      }
    }
  }
  
  return(concordance)
  
}

Discordance <- function(scores, p, v){
  
  ## feasability checks for "scores"
  if (!is.matrix(scores)) 
    stop("The performance scores table must be provided as a matrix")
  
  numOptions = nrow(scores);
  numCriteria= ncol(scores);
  
  ## dimensions of scores should be consistent with q,p,v,w
  if(numCriteria != length(p))
    stop("The preference thresholds p dimensions mismatch with the table of scores")
  if(numCriteria != length(v))
    stop("The veto thresholds v dimensions mismatch with the table of scores")
  
  
  ## partial discordance Dx = (b-a)-p / (v-p)
  Dx <- array(NA, c(numOptions,numOptions,numCriteria))
  for(k in 1:numCriteria){
    for(j in 1:numOptions){
      for(i in 1:numOptions){
        Dx[i,j,k] = ((scores[j,k]-scores[i,k]) - p[k] ) / (v[k] - p[k])
      }
    }
  }
  ## clip the values above 1 and below 0
  for(k in 1:numCriteria){
    for(j in 1:numOptions){
      for(i in 1:numOptions){
        Dx[i,j,k] <- ifelse(Dx[i,j,k]>1, 1, ifelse(Dx[i,j,k]<0, 0,Dx[i,j,k]) ) 
      }
    }
  }
  ## overall discordance (we do not aggregate discordances)
  discordance <- Dx
  return(discordance)
  
}

Credibility <- function(scores, concordance, discordance ){
  
  numOptions = nrow(scores);
  numCriteria= ncol(scores);
  
  ## credibility to combine concordance and discordances
  credibility <- array(NA, c(numOptions,numOptions))
  for(j in 1:numOptions){
    for(i in 1:numOptions){
      credibility[i,j] <- concordance[i,j]
      for(k in 1:numCriteria){
        if(discordance[i,j,k]>concordance[i,j]){
          credibility[i,j] <- credibility[i,j]*(1-discordance[i,j,k])/(1-concordance[i,j]) 
        }
      }
    }
  }
  
  return(credibility)
  
}


Distillation <- function(credibility, threshold=0.3){
  
  N <- nrow(credibility)
  highest <- max( as.vector(credibility) )
  cutoff <- highest - threshold
  
  dominance <- array(NA, c(N,N))
  for(i in 1:N){
    for(j in 1:N){
      dominance[i,j] <- ifelse(credibility[i,j]>cutoff, 1, 0)
    }
  }
  
  RowSum <- rep(0,N)
  ColSum <- rep(0,N)
  for(i in 1:N){
    for(j in 1:N){
      RowSum[i] <- RowSum[i] + dominance[i,j]
      ColSum[i] <- ColSum[i] + dominance[j,i]
    }
  }
  scoring <- rep(NA,N)
  for(i in 1:N){
    scoring[i] <- RowSum[i] - ColSum[i]
  }
  
  outputs <- list(dominance = dominance, scoring = scoring )
  return(outputs)
  
}



#' ELimination Et Choice Translating REality - ELECTRE-III
#' 
#' ELECTRE (ELimination Et Choice Translating REality) is an outranking method
#' proposed by Bernard Roy and his colleagues at SEMA consultancy company. This
#' is the implementation of ELECTRE-III.
#' 
#' 
#' @param scores Matrix or data frame containing the performance table. Each
#' column corresponds to a criterion, and each row to an alternative.
#' @param q Vector containing the indifference thresholds. The elements are
#' named according to the IDs of the criteria.
#' @param p Vector containing the preference threshold on each of the criteria.
#' The elements are named according to the IDs of the criteria.
#' @param v Vector containing the veto thresholds for each criterion. The
#' elements are named according to the IDs of the criteria.
#' @param w Vector containing the weights of criteria. The elements are named
#' according to the IDs of the criteria.
#' @return The function returns the Concordance, Discordance, Credibility,
#' Dominance, and Scoring tables.
#' @references Roy, Bernard (1968). "Classement et choix en présence de points
#' de vue multiples (la méthode ELECTRE)". La Revue d'Informatique et de
#' Recherche Opérationelle (RIRO) (8): 57–75.
#' @examples
#' 
#' library(MCDA)
#' scores <- matrix( c(-0.2,-2.3,-2.4,-1,3,9,10,7), 
#'                   nrow = 4, 
#'                   dimnames = list(
#'                     c("School-A","School-B","School-C", "School-D"), 
#'                     c("Location","Quality")) )
#' 
#' q <- c( 0.2, 1)
#' p <- c(   1, 2)
#' v <- c( 3.5, 4)
#' w <- c(0.25, 0.75)
#' 
#' res <- ELECTRE3(scores, q, p, v, w)
#' print(res)
#' 
#' 
#' @export ELECTRE3
ELECTRE3 <- function(scores, q, p, v, w ){
  
  ## feasability checks for "scores"
  if (!is.matrix(scores)) 
    stop("The performance scores table must be provided as a matrix")
  
  numOptions = nrow(scores);
  numCriteria= ncol(scores);
  ## if weights are not provided, then assign equal weight to all criteria
  if(is.null(w) || !is.vector(w))
    w = rep(1.0/numCriteria, numCriteria)
  
  ## dimensions of scores should be consistent with q,p,v,w
  if(numCriteria != length(q))
    stop("The indifference thresholds q dimensions mismatch with the table of scores")
  if(numCriteria != length(p))
    stop("The preference thresholds p dimensions mismatch with the table of scores")
  if(numCriteria != length(v))
    stop("The veto thresholds v dimensions mismatch with the table of scores")
  if(numCriteria != length(w))
    stop("The weights vector w dimensions mismatch with the table of scores")
  
  concordance <- Concordance(scores, p=p, q=q, w=w)
  discordance <- Discordance(scores, p=p, v=v)  
  credibility <- Credibility(scores, concordance=concordance, discordance=discordance)
  res <- Distillation(credibility)
  
  outputs <- list(concordance = concordance, 
                  discordance = discordance, 
                  credibility = credibility,
                  dominance = res$dominance,
                  scoring = res$scoring)
  return(outputs)
  
}
