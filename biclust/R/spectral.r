spectral=function(x,normalization="log", numberOfEigenvalues=6, 
                  minr=2, minc=2, withinVar=1, n_clusters = NULL, n_best = 3) {
  A=x
  n=dim(A)[1]
  m=dim(A)[2]
  discardFirstEigenvalue=T
  
  #1) Normalization------
  if(normalization=="log") {
    # if(min(A)<1)  A= A+1+(abs(min(A)))
    if (min(A)<1) A = A+(1-min(A))
    K=logt(A)
    discardFirstEigenvalue=F } else {
      
      if(numberOfEigenvalues==1) {
      warning("Only 1 eigenvalue selected with a normalization that gives first eigenvalue as background. Increasing to 2 eigenvalues")
      numberOfEigenvalues=2;
      } 
      
      Ab=A+abs(min(A))
      if(normalization=="irrc") K = irrc(Ab) else if(normalization=="bistochastization") K = bistochastization(Ab)
  }
  
  #2) SVD Decomposition
  desc=svd(K)
  
  
  #3) Vector processing: partitive clustering and reordering
  #  result=postprocess(desc,maxeigen=numberOfEigenvalues,minCG=max(2,floor(n/400)),
  #          maxCG=min(n/minr,100),minCE=max(2,floor(m/20)),maxCE=m/minc)
  result=postprocess(desc,maxeigen=numberOfEigenvalues,minCG=2,
                     maxCG=min(n/minr,100),minCE=2,maxCE=m/minc, n_clusters = n_clusters, 
                     discardFirstEigenvalue = discardFirstEigenvalue)
  
  # 4) Projection and partioning
  if (length(n_clusters) == 1) {n_clusters = rep(n_clusters, 2)}
  
  n_row_clusters = n_clusters[1]
  n_column_clusters = n_clusters[2]
  
  if (n_best > ncol(result$u)) {n_best = ncol(result$u)}
  
  projected_u = t(K) %*% result$u[,1:n_best, drop = FALSE] # rows as sample, column partition
  projected_v = K %*% result$v[,1:n_best, drop = FALSE] # rows as sample, row partition
  
  
  if (is.null(n_clusters)) {
    
    # row_partitioning = kmeans(projected_v, centers = min(result$numgenes[1:n_best]), 
    #                           nstart = 10, iter.max = 500)
    # column_partitioning = kmeans(projected_u, centers = min(result$numexpr[1:n_best]), 
    #                              nstart = 10, iter.max = 500)

    row_partitioning=iterativeKmeans(projected_v,minimum=2,maximum=max(result$numgenes[1:n_best]), choice=0.5)
    column_partitioning=iterativeKmeans(projected_u,minimum=2,maximum=max(result$numexpr[1:n_best]),choice=0.5)
    
  } else {
    row_partitioning = kmeans(projected_v, centers = n_row_clusters, 
                              nstart = 10, iter.max = 500)
    column_partitioning = kmeans(projected_u, centers = n_column_clusters, 
                                 nstart = 10, iter.max = 500)
    
  }
  
  row_labels = row_partitioning$cluster
  column_labels = column_partitioning$cluster
  
  
  # 5) Taking biclusters of all possible eigenvector combinations and filtering non-relevant biclusters
  
  rowxnumber = NULL
  colxnumber = NULL
  
  selected_clusters = rep(FALSE, 
                          max(row_labels)*max(column_labels))
  k=0
  for (i in 1:max(row_labels)) {
    for (j in 1:max(column_labels)) {
      k = k + 1

      
      Bicluster = A[row_labels == i,
                    column_labels == j, drop = FALSE]


      quality_bicluster = withinVar(Bicluster)

      selected_clusters[k] = all(dim(Bicluster)[1] > minr &
                                   dim(Bicluster)[2] > minc & quality_bicluster < withinVar)
      
      if (selected_clusters[k]) {
        rowxnumber = cbind(rowxnumber, row_labels == i)
        colxnumber = cbind(colxnumber, column_labels == j)
      }
      
    }
  }

  if(all(!selected_clusters)) {
    warning("No biclusters found")

    return(BiclustResult(as.list(match.call()),matrix(NA,1,1),matrix(NA,1,1),0,
                         list(row_labels=row_labels, column_labels=column_labels)))
  }
  

  return(BiclustResult(as.list(match.call()),rowxnumber,t(colxnumber),
                       sum(selected_clusters), list(row_labels=row_labels,
                                                      column_labels=column_labels)))


}
