indexFeats <-
function(scores, graph, n.rank=NULL){
  # make sure graph has 3 cols
  if(ncol(graph)!=3){stop("Graph must have 3 columns.")}
  
  # set n.rank
  n <- length(scores)
  if(is.null(n.rank)) n.rank <- min(1000, n)
  n.rank <- min(n, n.rank)
  
  # make sure all scores are between 0 and 1
  if(any(scores < 0)){scores <- scores + abs(min(scores))}
  if(any(scores > 1)){scores <- scores/max(scores)}
  
  # make sure all edge weights are between 0 and 1
  if(any(graph[,3] < 0)){graph[,3] <- graph[,3] + abs(min(graph[,3]))}
  if(any(graph[,3] > 1)){graph[,3] <- graph[,3]/max(graph[,3])}
  
  # select feats to rank
  feats.w <- order(scores, decreasing = T)[1:n.rank]
  scores.w <- sort(scores, decreasing = T)[1:n.rank]
  
  # get sub graph for ranking
  graph.feats <- graph[which(graph[,1] %in% feats.w & graph[,2] %in% feats.w),]
  if(length(graph.feats)==0){return(NULL)}
  if(is.null(dim(graph.feats))){
    graph.feats <- t(as.matrix(graph.feats))
  }
  
  # re-number from 1 to n.rank
  loc.map <- data.frame(original.loc = feats.w, new.loc = 1:n.rank)
  graph.w = cbind(plyr::mapvalues(graph.feats[,1], loc.map[,1], loc.map[,2],  warn_missing = F),
                  plyr::mapvalues(graph.feats[,2], loc.map[,1], loc.map[,2], warn_missing = F),
                  graph.feats[,3])
  
  return(list(graph.w = graph.w, scores.w = scores.w, loc.map = loc.map, n.rank = n.rank))
}
