#Get the coordinates of the waypoints of the principal path
compute_spathial <- function(X, boundary_ids, NC){
  ### Initialize waypoints
  print("Initializing waypoints...")
  X <- as.matrix(X)
  N<-nrow(X)
  M <- NC+2
  XX <- matrix(rep(matrixStats::rowSums2(X*X), M), N, M, byrow=F)
  waypoint_ids<-initMedoids(X, XX, NC, 'kpp', boundary_ids)

  waypoint_ids<-c(boundary_ids[1],waypoint_ids,boundary_ids[2])
  init_W<-X[waypoint_ids,]

  ### Annealing with rkm
  print("Computing paths...")
  s_span<-pracma::logspace(5,-5)
  s_span<-c(s_span,0)

  models<-list()
   pb <- utils::txtProgressBar(min = 0, max = length(s_span), style = 3)
   for(i in 1:length(s_span)){
     s<-s_span[i]
     W<-rkm(X,XX, init_W,s,plot_ax=FALSE)
     init_W<-W
     models[[as.character(s)]]<-W
     utils::setTxtProgressBar(pb, i)
   }
  close(pb)

  print("Path selection...")
  W_dst_var <- rkm_MS_pathvar(models, s_span, X)
  s_elb_id <- find_elbow(cbind(s_span, W_dst_var))
  ppath <- models[[s_elb_id]]
  return(ppath)
}

#Regularized K-means for principal path, MINIMIZER.
rkm <- function(X, XX, init_W, s, plot_ax=FALSE){
  X<-as.matrix(X)

  # Extract useful info from args
  N<-nrow(X)
  d<-ncol(X)
  NC<-nrow(init_W)-2

  # Construct boundary matrix
  boundary<-init_W[c(1,nrow(init_W)),]
  B<-matrix(0,nrow=NC,ncol=d)
  B[1,]<-as.numeric(boundary[1,])
  B[nrow(B),]<-as.numeric(boundary[2,])
  rownames(B)<-rownames(init_W)[2:(nrow(init_W)-1)]

  # Construct regularizer hessian
  diag1<-diag(NC)
  diag2<-diag(NC)
  diag(diag2)<-(-0.5)
  diag2<-cbind(rep(0,NC),diag2[,1:(ncol(diag2)-1)])
  diag3<-diag(NC)
  diag(diag3)<-(-0.5)
  diag3<-rbind(rep(0,NC),diag3[1:(nrow(diag3)-1),])
  AW<-diag1+diag2+diag3
  rownames(AW)<-colnames(AW)<-rownames(B)

  # Compute initial labels
  XW_dst <- dist(as.matrix(X), as.matrix(init_W), XX)

  XW_dst<-XW_dst^2
  u<-colnames(XW_dst)[apply(XW_dst,1,which.min)]

  ### Iterate the minimizer
  names_of_rows<-rownames(B)
  converged<-FALSE
  it<-0
  while(!converged){
    it<-it+1

    # Compute Cardinality
    W_card<-stats::setNames(rep(0,NC+2),rownames(init_W))
    tu<-table(u)
    W_card[names(tu)]<-tu

    ### Compute Centroid Matrix
    C<-matrix(NA,nrow=NC,ncol=d)
    rownames(C)<-names_of_rows
    colnames(C)<-colnames(X)
    # Speed up with a matrixStats solution
    for (i in 1:nrow(C)) {
      indexes <- which(u == names_of_rows[i])
      C[i, ] <-  matrixStats::colSums2(X, rows = indexes)
    }

    # Construct K-means Heassian
    AX<-diag(W_card[2:(length(W_card)-1)])

    ### Update waypoints
    # Compute the (Moore-Penrose) pseudo-inverse of the multiplied Hessians
    pseudo<-MASS::ginv(AX+s*AW)
    csb<-C+0.5*s*B
    W<-pseudo%*%csb
    rownames(W)<-rownames(B)
    colnames(W)<-colnames(X)
    W<-rbind(boundary[1,],W,boundary[2,])
    rownames(W)<-rownames(init_W)

    # Compute new labels
    XW_dst <- dist(as.matrix(X), as.matrix(W), XX)

    XW_dst<-XW_dst^2
    u_new<-colnames(XW_dst)[apply(XW_dst,1,which.min)]

    # Check for convergence
    converged<-all(u_new==u)
    u<-u_new
  }
  return(W)
}

#Regularized K-means for principal path: prefiltering
rkm_prefilter <- function(X, boundary_ids, Nf=200, k=5, p=1000, T=0.1){
  X <- as.matrix(X)
  N <- nrow(X)
  d <- ncol(X)
  n <- Nf - 2
  M <- Nf
  XX <- matrix(rep(matrixStats::rowSums2(X*X), M), N, M, byrow=F)

  # Pick Nf medoids with k-means++ and compute pairwise distance matrix
  med_ids<-initMedoids(X,XX,n,init_type="kpp",boundary_ids)
  med_ids<-c(boundary_ids[1],med_ids,boundary_ids[2])
  medmed_dst<-as.matrix(stats::dist(X[med_ids,],method="euclidean"))^2

  # Build k-nearest-neighbor penalized matrix
  knn_ids<-t(apply(medmed_dst,1,order))
  medmed_dst_p<-medmed_dst*p
  for (i in 1:Nf){
    for(j in 1:k){
      k<-knn_ids[i,j]
      medmed_dst_p[i,k]<-medmed_dst[i,k]
      medmed_dst_p[k,i]<-medmed_dst[k,i]
    }
  }
  medmed_dst_p[1,Nf]<-0
  medmed_dst_p[Nf,1]<-0

  # Find shortest path using dijkstra
  g<-igraph::graph.adjacency(medmed_dst_p, weighted=TRUE)
  path<-igraph::shortest_paths(g,from=1,to=ncol(medmed_dst_p))
  path<-names(path$vpath[[1]])

  # Filter out medoids too close to the shortest path
  T<-T*mean(medmed_dst)
  to_filter_ids<-c()
  for(i in path){
    to_filter_ids<-c(names(which(medmed_dst[i,]<T,useNames=TRUE)),to_filter_ids)
  }
  to_filter_ids<-unique(setdiff(to_filter_ids,path))
  to_keep_ids<-setdiff(rownames(medmed_dst),to_filter_ids)
  Xmed_dst<-dist(X, X[to_keep_ids,], XX)
  Xmed_dst<-Xmed_dst^2
  rownames(Xmed_dst)<-rownames(X)
  colnames(Xmed_dst)<-to_keep_ids

  # Select minimum distance medoids
  u<-colnames(Xmed_dst)[apply(Xmed_dst,1,which.min)]
  filter_mask<-stats::setNames(rep(FALSE,N),rownames(X))
  filter_mask[u%in%path]<-TRUE

  boundary_ids_filtered<-boundary_ids

  # Return ouput
  outlist<-list(
    filter_mask=filter_mask,
    boundary_ids_filtered=boundary_ids_filtered
  )
  return(outlist)
}

#Regularized K-menans for principal path, MODEL SELECTION, variance on waypoints interdistance.
rkm_MS_pathvar <- function(models, s_span, X){
  W_dst_var <- array(data = 0.0, dim = length(models))
  for(i in (1:length(models))){
    W = models[[i]]
    W=as.matrix(W)
    W_diff <- W[2:nrow(W),] - W[1:(nrow(W)-1),]
    W_dst=apply(W_diff, 1, function(x) norm(as.matrix(x),"F"))
    W_dst_var[i] = pvar(W_dst)
  }
  return(W_dst_var)
}

pvar <- function(x) {
  sum((x - mean(x))**2) / length(x)
}
