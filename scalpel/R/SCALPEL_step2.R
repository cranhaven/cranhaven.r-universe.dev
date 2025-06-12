#The following functions are needed to perform Step 2 of SCALPEL

#function to calculate dissimilarity matrix based on
# weighted average of 1 - cosine distance for spatial components and
# 1 - cosine similarity (i.e. uncentered correlation) of univariate temporal components
#columns of Azero should contain candidate spatial components from image segmentation
calcDissimilarity = function(Azero, Y, weight) {
  #calculate spatial similiarity
  numer = crossprod(Azero)
  norms = Matrix(sqrt(diag(numer)), nrow=1)
  space = numer / crossprod(norms)

  #now calculate temporal similarity
  #first, calculate average fluorescence for each component over time
  Atilde =  t(t(Azero) / colSums(Azero))
  avgFluor = t(crossprod(Atilde, Y))
  numer = crossprod(avgFluor)
  norms = Matrix(sqrt(diag(numer)), nrow=1)
  time = numer / crossprod(norms)

  #following equals weight*(1-space) + (1-weight)*(1-time)
  return(1 - weight*space + (weight-1)*time)
}

#function to cluster candidate spatial components using hierarchical clustering
#specify cutoff for clustering
#instead of using prototype for representative spatial component, we use
#the component with the smallest median dissimilarity from others
clusterComponentsHelper = function(Azero, Y, videoHeight, cutoff=0.18, weight=0.2) {

  output = list()
  videoWidth = nrow(Azero)/videoHeight
  ASp = Matrix(Azero, sparse=T)

  #calculate dissimilarity matrix
  d = stats::as.dist(calcDissimilarity(Azero=ASp, Y=Y, weight=weight))

  #perform prototype clustering
  hc = protoclust::protoclust(d)
  cut = protoclust::protocut(hc, h=cutoff)
  output$clusterID = cut$cl
  nclusters = max(output$clusterID)

  #figure out representative components
  repComps = rep(NA, nclusters)
  for (clusterID in 1:nclusters) {
    inCluster = which(cut$cl==clusterID)
    if (length(inCluster)>2) {
      clusterDist = as.matrix(d)[inCluster,inCluster]
      diag(clusterDist) = NA
      medianDist = apply(clusterDist, 1, stats::median, na.rm=T)
      #choose the first one if multiple tied for minimum
      repComps[clusterID] = inCluster[which(medianDist==min(medianDist))][1]
    } else if (length(inCluster)==2) {
      #if only two components are clustered together, choose the first one
      repComps[clusterID] = inCluster[1]
    } else repComps[clusterID] = inCluster
  }
  output$repComps = repComps
  output$tree = hc

  return(output)
}

#same as clusterComponentsHelper, except that if Azero has >6000 columns then
#cluster groups of size floor(ncol(Azero)/3000) first, followed by clustering the representatives
#and finally recalculating the final representative using all candidates in the cluster
clusterComponents = function(Azero, Y, videoHeight, cutoff=0.18, weight=0.2, maxSizeToCluster=3000, fullCluster=FALSE) {

  output = list(); output$tree = NULL
  videoWidth = nrow(Azero)/videoHeight
  ASp = Matrix(Azero, sparse=T)

  #randomly break components into floor(ncol(Azero)/maxSizeToCluster) groups
  #unless fullCluster==TRUE, then only have one group
  set.seed(100)
  if (fullCluster==FALSE) {
    groups = sample(rep(1:max(floor(ncol(Azero)/maxSizeToCluster),1), len=ncol(Azero)))
  } else groups = rep(1, len=ncol(Azero))

  #cluster assiginments initially
  prelimClusters = rep(NA, ncol(Azero))
  #index for representative component
  ArepIndex = 1
  #vector to store indices of representative components
  ArepComps = Matrix(0, nrow=1, ncol=200000, sparse=T)

  #do hierarchical clustering for each group
  for (set in 1:max(groups)) {

    if (max(groups)!=1) {
      message("Finding preliminary clusters: ",set," of ",max(groups))
    } else message("Finding clusters")
    cols = which(groups==set)
    if (length(cols)>1) {
      out = clusterComponentsHelper(Azero=ASp[which(rowSums(ASp[,cols,drop=F])!=0),cols,drop=F],
                                    Y=Y[which(rowSums(ASp[,cols,drop=F])!=0),],
                                    videoHeight=videoHeight, cutoff=cutoff, weight=weight)

      #save tree if only one group (max(groups)==1)
      if (max(groups)==1) output$tree = out$tree

      prelimClusters[cols] = out$clusterID + max(c(prelimClusters, 0), na.rm = T)
      ArepComps[,ArepIndex:(ArepIndex+length(out$repComps)-1)] = cols[out$repComps]
      ArepIndex = ArepIndex + length(out$repComps)
    } else {
      prelimClusters[cols] = 1 + max(c(prelimClusters, 0), na.rm = T)
      ArepComps[,ArepIndex] = cols
      ArepIndex = ArepIndex + 1
    }
  }

  ArepComps = ArepComps[1,1:(ArepIndex-1)]

  #now cluster the representative components (if more than one group)
  if (max(groups)>1) {
    message("Finding final clusters")
    cols = ArepComps
    outRep = clusterComponentsHelper(Azero=ASp[which(rowSums(ASp[,cols,drop=F])!=0),cols],
                                     Y=Y[which(rowSums(ASp[,cols,drop=F])!=0),],
                                     videoHeight=videoHeight, cutoff=cutoff, weight=weight)
    output$clusterID = outRep$clusterID[prelimClusters]

    #calculate the representative components
    nclusters = max(output$clusterID)
    repComps = rep(NA, nclusters)
    for (clusterID in 1:nclusters) {
      message("Finding representatives: ", clusterID, " of ", nclusters)
      inCluster = which(output$clusterID==clusterID)
      if (length(inCluster)>2) {
        if (length(inCluster)>3000) inCluster = sample(inCluster, 3000)
        clusterDist = as.matrix(calcDissimilarity(Azero=ASp[which(rowSums(ASp[,inCluster,drop=F])!=0),inCluster], Y=Y[which(rowSums(ASp[,inCluster,drop=F])!=0),], weight=weight))
        diag(clusterDist) = NA
        medianDist = apply(clusterDist, 1, stats::median, na.rm=T)
        #choose the first one if multiple tied for minimum
        repComps[clusterID] = inCluster[which(medianDist==min(medianDist))][1]
      } else if (length(inCluster)==2) {
        #if only two components are clustered together, choose the first one
        repComps[clusterID] = inCluster[1]
      } else repComps[clusterID] = inCluster
    }
    output$repComps = repComps
  } else {
    #don't need to recluster if we started with a single group (max(groups)==1)
    output$clusterID = prelimClusters
    output$repComps = ArepComps
  }

  return(output)
}

#the following function finds sets of overlapping neurons
#A should be a binary matrix indicating whether a pixel (rows) is in a certain neuron (columns)
#returns a vector indicating which set each neuron is in
findSets = function(A) {
  adj = crossprod(A)
  adjBinary = (adj!=0)
  rm(adj)
  graph = igraph::graph.adjacency(adjBinary, mode="undirected")
  set = igraph::clusters(graph)$membership
  return(set)
}
