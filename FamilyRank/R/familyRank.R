familyRank <-
function(scores, graph, d = .5, 
                        n.rank = min(length(scores),1000), 
                        n.families=min(n.rank, 1000), 
                        tol = 1e-3){
  # make sure d is between 0 and 1
  if(d>1 | d<0){stop("Damping factor must be between 0 and 1.")}
  
  # make sure graph has 3 cols
  if(ncol(graph)!=3){stop("Graph must have 3 columns.")}
  
  # make sure graph is numeric
  graph[,1] <- as.numeric(as.character(graph[,1]))
  graph[,2] <- as.numeric(as.character(graph[,2]))
  graph[,3] <- as.numeric(as.character(graph[,3]))
  
  nas <- is.na(graph)
  if(any(nas)){
    rows.to.remove <- which(apply(nas, 1, function(row)any(is.na(row))))
    graph <- graph[-nas,]
    if(nrow(graph)==0){stop("Graph must be numeric")}else{
      warning("Removing non-numeric rows from graph.")
    }
  }
  
  # make sure scores are numeric
  if(!is.numeric(scores)){stop("Scores must be numeric.")}
  
  # make sure all scores are between 0 and 1
  if(any(scores < 0)){scores <- scores + abs(min(scores))}
  if(any(scores > 1)){scores <- scores/max(scores)}
  
  # make sure all edge weights are between 0 and 1
  if(any(graph[,3] < 0)){graph[,3] <- graph[,3] + abs(min(graph[,3]))}
  if(any(graph[,3] > 1)){graph[,3] <- graph[,3]/max(graph[,3])}
  
  # set # of families to build
  n <- length(scores)
  if(n.families > min(n, n.rank)){
    n.families <- min(n.families, n, n.rank)
    warn <- ifelse(which.min(c(n, n.rank))==1, "Setting n.families to score length", "Setting n.families to n.rank")
    warning(warn)
  }
  
  # get features to rank
  lst <- indexFeats(scores, graph, n.rank)
  if(is.null(lst)) return(scores)
  
  # get feats to initiate families
  feats.fam <- lst$loc.map$new.loc[1:n.families]
  scores.fam <- lst$scores.w[1:n.families]
  
  # create matrices to store features and scores 
  feat.mat <- score.mat <- matrix(0, nrow = n, ncol = n.families)
  feat.mat[1,] <- feats.fam
  score.mat[1,] <- scores.fam
  
  # Initialize Score Vector
  family.rank.scores <- rep(0, length(scores))
  
  # Get Family Rank Scores
  mats <- .Call("_FamilyRank_grow", n = lst$n.rank, f = n.families, d = d, graph = as.matrix(lst$graph.w), scores = lst$scores.w, 
               feat_mat = feat.mat, score_mat = score.mat, tol = tol, 
               weight_mat = as.matrix(lst$scores.w), selected = rep(1, lst$n.rank),
               PACKAGE = "FamilyRank")
  feat.mat <- mats[, 2:(n.families+1)]
  score.mat <- mats[, (n.families+2):(1+2*n.families)]
  family.rank.scores[lst$loc.map$original.loc] <- mats[,1]
  
  return(family.rank.scores)
}
