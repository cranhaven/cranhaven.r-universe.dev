# Postprocessing of eigenvectors as described in Kluger2003:
#
# Kluger, Y.; Basri, R.; Chang, J.T. & Gerstein, M., 
# Spectral Biclustering of Microarray Data: Coclustering Genes and Conditions
# Genome Research 2003. 
#
# It basically apply iterative k means clustering to all possible combinations
# of eigenvector of SVD analysis.
#
# dec - SVD decomposition of normalized matrix as returned by svd()
# maxeigen - maximum number of eigenvectors of dec processed. Default 6.
# minCG - minimum number of clusters in which eigengenes will be clustered. Default 2.
# maxCG - maximum number of clusters in which eigengenes will be clustered
# minCE - minimum number of clusters in which eigenexpressions will be clustered. Default 2.
# maxCE - maximum number of clusters in which eigenexpressions will be clustered
#
# Authors: Sami Leon (2021), Rodrigo Santamaria (2007)
#
postprocess=function(dec, maxeigen=6, minCG=2, maxCG, minCE=2, maxCE, n_clusters, discardFirstEigenvalue)
{
  
  if (length(n_clusters) == 1) {n_clusters = rep(n_clusters, 2)}
  n_row_clusters = n_clusters[1]
  n_column_clusters = n_clusters[2]
  
  if (discardFirstEigenvalue) {
    dec$u = dec$u[, -1]
    dec$v = dec$v[, -1]
    
  }
  

  
  u=dec$u #u are eigengenes, nxc (c=min(n,m), freedom degrees)
  v=dec$v #v are eigenarrays, mxc
  n=dim(u)[2]
  m=dim(v)[2]
  

  dev=NULL
  max=min(n,m)
  if(maxeigen>max)
  {
    warning("Number of eigenvectors required exceeds degrees of freedom")
    maxeigen=max
  }
  
  u = u[, 1:maxeigen, drop = FALSE]
  v = v[, 1:maxeigen, drop = FALSE]

  dev$numgenes = 1:maxeigen
  dev$numexpr = 1:maxeigen
  
  dev$u_withinss = NULL
  dev$v_withinss = NULL

  

  
  #Determine the best cluster decomposing by k means
  for (j in  1:maxeigen) {
    
    if (is.null(n_clusters)) {

      tresu=iterativeKmeans(u[,j],minimum=minCG,maximum=maxCG, choice=0.5)
      
    } else {
      tresu=kmeans(u[,j], centers = n_row_clusters, 
                   nstart = 10, iter.max = 500)
    }
    
    
    dev$numgenes[j]=max(tresu$cluster)
    
    dev$u_withinss[j] = sum(tresu$withinss)
    
  }
  
  
  for(j in 1:maxeigen) {
    
    #The same for eigenarrays
    
    if (is.null(n_clusters)) {

      tresu=iterativeKmeans(v[,j],minimum=minCE,maximum=maxCE,choice=0.5)
    } else {
      tresu=kmeans(v[,j], centers = n_column_clusters, 
                   nstart = 10, iter.max = 500)
    }
    
    dev$numexpr[j]=max(tresu$cluster)
    
    dev$v_withinss[j] = sum(tresu$withinss)
  }
  
  # Best piecewise
  dev$u=u[,order(dev$u_withinss), drop = FALSE]
  dev$v=v[,order(dev$v_withinss), drop = FALSE]
  
  dev$numgenes = dev$numgenes[order(dev$u_withinss)]
  dev$numexpr = dev$numexpr[order(dev$v_withinss)]
  
  dev
}


# ------------------- WITHIN VAR -------------------------
# Within Variation of a matrix, by rows.
# Computes the row mean and then the euclidean distance of each row to the mean.
# The lower this value is, the higher row homogeneity of the bicluster
# returns: the mean of the distances

withinVar=function(x) {
  n = nrow(x)
  m = ncol(x)
  within=0
  
  if(n==1)#Just one row
  {
    within=0
  }
  else    
  {
    if(m==1)  #Just one column
    {
      centroid=mean(x)
      distances=sqrt(sum((x-centroid)^2))
      within=sum(distances)/n
    }
    else  #More than one row or column
    {
      centroid=apply(x,2,mean)
      distances=sqrt(apply(t(centroid-t(x))^2,1,sum))
      within=sum(distances)/n
    }
  }
  within
}

