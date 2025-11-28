tsne.defaults <- list(
  dims = 2,
  perplexity = 30,
  theta = 0.5,
  max_iter = 1000,
  verbose = getOption("verbose", FALSE),
  Y_init = NULL,
  momentum = 0.5,
  final_momentum = 0.8,
  eta = 200,
  exaggeration_factor = 12,
  num_threads = 1
)
class(tsne.defaults) <- "tsne.config"


umap.defaults <- list(
           n_neighbors= 15,
          n_components= 2,
                metric= "euclidean",
              n_epochs= 200,
                 input= "data",
                  init= "spectral",
              min_dist= 0.1,
      set_op_mix_ratio= 1,
    local_connectivity= 1,
             bandwidth= 1,
                 alpha= 1,
                 gamma= 1,
  negative_sample_rate= 5,
                     a= NA,
                     b= NA,
                spread= 1,
          random_state= NA,
       transform_state= NA,
                   knn= NA,
           knn_repeats= 1,
               verbose= FALSE,
       umap_learn_args= NA,
            n_threads = NULL,
        n_sgd_threads = 0
)
class(umap.defaults) <- "umap.config"



MDS.defaults <- list(
  dims = 2
)
class(MDS.defaults) <- "MDS.config"

print.tsne.config <- function(x, ...) {
  if (!is(x, "tsne.config")) {
    stop("x is not a tsne configuration object")
  }
  
  # produce a string of form "  z:  " of total length width
  padspaces <- function(z, width=24) {
    padleft <- max(0, width-nchar(z)-2)
    paste(c(rep(" ", padleft), z, ": "), collapse="")
  }
  
  message("t-SNE configuration parameters")
  primitives <- c("numeric", "integer", "character", "logical")
  vapply(names(x), function(z) {
    zval <- x[[z]]
    if (sum(class(zval) %in% primitives)) {
      message(padspaces(z), paste(zval, collapse=" "))
    } else {
      message(padspaces(z), "[", paste(class(zval), collapse=","), "]")
    }
    z
  }, character(1))
  
  invisible(x)
}


print.MDS.config <- function(x, ...) {
  if (!is(x, "MDS.config")) {
    stop("x is not a MDS configuration object")
  }
  
  # produce a string of form "  z:  " of total length width
  padspaces <- function(z, width=24) {
    padleft <- max(0, width-nchar(z)-2)
    paste(c(rep(" ", padleft), z, ": "), collapse="")
  }
  
  message("MDS configuration parameters")
  primitives <- c("numeric", "integer", "character", "logical")
  vapply(names(x), function(z) {
    zval <- x[[z]]
    if (sum(class(zval) %in% primitives)) {
      message(padspaces(z), paste(zval, collapse=" "))
    } else {
      message(padspaces(z), "[", paste(class(zval), collapse=","), "]")
    }
    z
  }, character(1))
  
  invisible(x)
}

kabsch <- function(pm, qm) {
  pm_dims <- dim(pm)
  if (!all(dim(qm) == pm_dims)) {
    stop(call. = TRUE, "Point sets must have the same dimensions")
  }
  # The rotation matrix will have (ncol - 1) leading ones in the diagonal
  diag_ones <- rep(1, pm_dims[2] - 1)
  
  # center the points
  pm <- scale(pm, center = TRUE, scale = FALSE)
  qm <- scale(qm, center = TRUE, scale = FALSE)
  
  am <- crossprod(pm, qm)
  
  svd_res <- svd(am)
  # use the sign of the determinant to ensure a right-hand coordinate system
  d <- determinant(tcrossprod(svd_res$v, svd_res$u))$sign
  dm <- diag(c(diag_ones, d))
  
  # rotation matrix
  um <- svd_res$v %*% tcrossprod(dm, svd_res$u)
  
  # Rotate and then translate to the original centroid location of qm
  sweep(t(tcrossprod(um, pm)), 2, -attr(qm, "scaled:center"))
}



pca = function(x,...){
  res=prcomp(x,...)
  ss=sprintf("%.1f",summary(res)$importance[2,]*100)
  res$txt = paste(names(summary(res)$importance[2,])," (",ss,"%)",sep="")
  colnames(res$x)=res$txt
  res
}


quality_control = function(data_row,data_col,spatial_row=NULL,FUN,data=NULL,f.par.pls){
  matchFUN = pmatch(FUN[1], c("fastpls","simpls"))
  if (is.na(matchFUN)) 
    stop("The method to be considered must be  \"fastpls\", \"simpls\".")
  if (!is.null(spatial_row)){
    if (spatial_row!=data_row) 
      stop("The number of spatial coordinates and number of entries do not match.")    

  } 

  if (f.par.pls > data_col) {
    message("The number of components selected for PLS-DA is too high and it will be automatically reduced to ", data_col)
    f.par.pls = data_col
  }

  
  return(list(matchFUN=matchFUN,f.par.pls=f.par.pls))
}


                              
KODAMA.matrix =
function (data,                       # Dataset
          spatial = NULL,             # In spatial are conteined the spatial coordinates of each entries
          samples = NULL,
          M = 100, Tcycle = 20, 
          FUN = c("fastpls","simpls"), 
          ncomp = min(c(50,ncol(data))),
          W = NULL, metrics="euclidean",
          constrain = NULL, fix = NULL,  landmarks = 10000,  
          splitting = ifelse(nrow(data) < 40000, 100, 300), 
          spatial.resolution = 0.3 , 
          simm_dissimilarity_matrix=FALSE,
         seed=1234) 
{
  epsilon = 0.05
  set.seed(seed)
  
  f.par.pls = ncomp
  neighbors = round(min(c(landmarks, nrow(data)/3),500)) + 1
  if (sum(is.na(data)) > 0) {
    stop("Missing values are present")
  } 
  data = as.matrix(data)
  nsample = nrow(data)
  nvariable = ncol(data)
  nsample_spatial= nrow(spatial)

  writeLines("Calculating Network")
  knn_Rnanoflann = Rnanoflann::nn(data, data, neighbors +1, method=metrics)
  knn_Rnanoflann$distances = knn_Rnanoflann$distance[,-1]
  knn_Rnanoflann$indices = knn_Rnanoflann$indices[,-1]
  
  
  if (is.null(spatial)) {
    spatial_flag = FALSE
  }  else {
    spatial_flag = TRUE
    writeLines("\nCalculating Spatial Network")
    knn_Rnanoflann_spatial = Rnanoflann::nn(spatial, spatial, neighbors, method=metrics)

    aa=colMeans(abs(spatial[knn_Rnanoflann_spatial$indices[,1],]-spatial[knn_Rnanoflann_spatial$indices[,20],]))*3
     
    if(!is.null(samples)){
      samples_names=names(table(samples))          
      if(length(samples_names)>1){
        ma=0
        for (j in 1:length(samples_names)) {
          sel <- samples_names[j] == samples
          spatial[sel, 1]=spatial[sel, 1]+ma
          ran=range(spatial[sel, 1])
          ma=ran[2]+ dist(ran)[1]*0.5
        }
      }
    }
  }
  if (is.null(fix)) 
    fix = rep(FALSE, nsample)
  if (is.null(constrain)) 
    constrain = 1:nsample
  is.na.constrain=is.na(constrain)
  if(any(is.na.constrain)){
    constrain=as.numeric(as.factor(constrain))
    constrain[is.na.constrain]=max(constrain,na.rm = TRUE)+(1:length(constrain[is.na.constrain]))
  }

     
  
  if(nsample<=landmarks){
    landmarks=ceiling(nsample*0.75)
  } 

  nspatialclusters=round(landmarks*spatial.resolution)
  
  QC=quality_control(data_row = nsample,
                     data_col = nvariable,
                     spatial_row = nsample_spatial,
                     FUN = FUN,
                     data = data,
                     f.par.pls = f.par.pls)
  matchFUN=QC$matchFUN

  f.par.pls=QC$f.par.pls



  res = matrix(nrow = M, ncol = nsample)
  res_constrain = matrix(nrow = M, ncol =nsample)

  vect_acc = matrix(NA, nrow = M, ncol = Tcycle)
  accu = NULL


  pb <- txtProgressBar(min = 1, max = M, style = 1)

  for (k in 1:M) {
    setTxtProgressBar(pb, k)
    set.seed(seed+k)
    
    # The landmarks samples are chosen in a way to cover all different profile
    # The data are divided in a number of clusters equal to the number of landmarks
    # A landpoint is chosen randomly from each cluster

    landpoints=NULL
    clust = as.numeric(kmeans(data, landmarks)$cluster)
    for (ii in 1:landmarks) {
      www = which(clust == ii)
      lwww=length(www)
      landpoints = c(landpoints,www[sample.int(lwww, 1, FALSE, NULL)])
    }

    # Variables are splitted in two where 
    # X variables are the variables for cross-validation accuracy maximizzation
    # T variables are the variable for the projections
    
    Tdata = data[-landpoints, , drop = FALSE]
    Xdata = data[landpoints, , drop = FALSE]
    Tfix = fix[-landpoints]
    Xfix = fix[landpoints]
    whF = which(!Xfix)
    whT = which(Xfix)
    Xspatial = spatial[landpoints, , drop = FALSE]

 

    if (spatial_flag) {
      delta=as.numeric(unlist(tapply(aa,1:length(aa),function(x) runif(nsample,-x,x))))
      spatialclusters=as.numeric(kmeans(spatial+delta, nspatialclusters)$cluster)
      ta_const=table(spatialclusters)
      ta_const=ta_const[ta_const>1]
      sel_cluster_1=spatialclusters %in% as.numeric(names(ta_const))
      if(sum(!sel_cluster_1)>0){
        spatialclusters[!sel_cluster_1]=spatialclusters[sel_cluster_1][Rnanoflann::nn(spatial[sel_cluster_1,],spatial[!sel_cluster_1,,drop=FALSE],1)$indices]
      }        
      constrain_clean=NULL
      for(ic in 1:max(constrain)){
        sel_ic=ic==constrain
        constrain_clean[sel_ic]=as.numeric(names(which.max(table(spatialclusters[sel_ic]))))
      }                                 
    }else{
      constrain_clean=constrain
    }



        Xconstrain = as.numeric(as.factor(constrain_clean[landpoints]))
        if(!is.null(W)){
          SV_startingvector = W[landpoints]
          unw = unique(SV_startingvector)
          unw = unw[-which(is.na(unw))]
          ghg = is.na(SV_startingvector)
          SV_startingvector[ghg] = as.numeric(as.factor(SV_startingvector[ghg])) + length(unw)
          
          #################  tab = apply(table(SV_startingvector,Xconstrain), 2,  which.max)
          ################   XW = as.numeric(as.factor(tab[as.character(Xconstrain)]))
          XW=NULL
          for(ic in 1:max(Xconstrain)){
            XW[ic==Xconstrain]=as.numeric(names(which.max(table(SV_startingvector[ic==Xconstrain]))))
          }
          
          
        }else{
          if (landmarks<200) {
            XW = Xconstrain
          } else {
            clust = as.numeric(kmeans(Xdata, splitting)$cluster)
            #############   tab = apply(table(clust, Xconstrain), 2, which.max)
            #############   XW = as.numeric(as.factor(tab[as.character(Xconstrain)]))
            
            XW=NULL
            for(ic in 1:max(Xconstrain)){
              XW[ic==Xconstrain]=as.numeric(names(which.max(table(clust[ic==Xconstrain]))))
            }
            
            
          }
        }
        
        
        clbest = XW
        options(warn = -1)
        yatta = 0
        attr(yatta, "class") = "try-error"
        while (!is.null(attr(yatta, "class"))) {
          yatta = try(core_cpp(Xdata, Tdata, clbest, Tcycle, FUN, 
                               f.par.pls,
                               Xconstrain, Xfix), silent = FALSE)
          
        }
        options(warn = 0)
        res_k=rep(NA,nsample)
        if (is.list(yatta)) {
          clbest = as.vector(yatta$clbest)
          accu = yatta$accbest
          yatta$vect_acc = as.vector(yatta$vect_acc)
          yatta$vect_acc[yatta$vect_acc == -1] = NA
          vect_acc[k, ] = yatta$vect_acc
          
          yatta$vect_proj = as.vector(yatta$vect_proj)
          
          if(!is.null(W))
            yatta$vect_proj[Tfix] = W[-landpoints][Tfix]
          
          temp=rep(NA,nsample)
          res_k[landpoints] = clbest
          res_k[-landpoints] = yatta$vect_proj
          
          
          ###########        tab = apply(table(res_k, constrain_clean), 2, which.max)
          ###########        res_k = as.numeric(as.factor(tab[as.character(constrain_clean)]))
          
          res_k_temp=NULL
          for(ic in 1:max(constrain_clean)){
            res_k_temp[ic==constrain_clean]=as.numeric(names(which.max(table(res_k[ic==constrain_clean]))))
          }
          res_k=res_k_temp 
          
          
        }
        
        res[k,]=res_k
        res_constrain[k,]=constrain_clean                             
                                     
                                     
        
  }
  
  close(pb)

    print("Calculation of dissimilarity matrix...")
    

    pb <- txtProgressBar(min = 1, max = nrow(data), style = 1)
    for (k in 1:nrow(data)) {
    setTxtProgressBar(pb, k)
    
        
        
        
        knn_indices=knn_Rnanoflann$indices[k,]
        knn_distances=knn_Rnanoflann$distances[k,]
        
        
        
        mean_knn_distances=mean(knn_distances)                             
        for (j_tsne in 1:neighbors) {
          
          kod_tsne = mean(res[, k] == res[, knn_indices[j_tsne]], na.rm = TRUE)
          knn_distances[j_tsne] = (1+knn_distances[j_tsne])/(kod_tsne^2)
          
        }
        
        
        oo_tsne = order(knn_distances)
        knn_distances = knn_distances[oo_tsne]
        knn_indices = knn_indices[oo_tsne]
        
      
      knn_Rnanoflann$indices[k,]=knn_indices
      knn_Rnanoflann$distances[k,] =knn_distances
      
    }
    
    close(pb)



dissimilarity=NULL
  ma=NULL
  if(simm_dissimilarity_matrix){
    ma = matrix(0, ncol = nsample, nrow = nsample)
    for(k in 1:M){
      uni = unique(res[k,])
      nun = length(uni)
      res_k=res[k,]
      for (ii in 1:nun) 
        ma[res[k,] == uni[ii], res_k ==  uni[ii]] = ma[res_k == uni[ii], res_k == uni[ii]] + 1
    }
    ma = ma/M
    Edist = as.matrix(dist(data))
    ma[ma < epsilon] = 0

#  Entropy calculation
#    y = ma
#    diag(y) = NA
#    yy = as.numeric(y)
#    yy = yy[!is.na(yy)]
#    yy = yy/sum(yy)
#    H = -sum(ifelse(yy > 0, yy * log(yy), 0))
    
    mam = (1/ma) * Edist
  #  mam[is.na(mam)] <- .Machine$double.xmax
  #  mam[is.infinite(mam) & mam > 0] <- .Machine$double.xmax
    mam = floyd(mam)
  #  mam[mam == .Machine$double.xmax] <- NA
    prox = Edist/mam
    diag(prox) = 1
    prox[is.na(prox)] = 0
    maxvalue = max(mam, na.rm = TRUE)
    mam[is.na(mam)] = maxvalue

    dissimilarity = mam
  }

                                     
    
    knn_Rnanoflann$neighbors = neighbors
    return(list(dissimilarity = dissimilarity,acc = accu, proximity = ma, 
                v = vect_acc, res = res, 
                knn_Rnanoflann = knn_Rnanoflann, 
                data = data,
                res_constrain=res_constrain))
    
  }

                            



                              
KODAMA.visualization=function(kk,method=c("UMAP","t-SNE","MDS"),config=NULL){
  
  mat=c("UMAP","t-SNE","MDS")[pmatch(method,c(c("UMAP","t-SNE","MDS")))[1]]

  if(mat=="t-SNE"){ 
    if(is.null(config)){
      config = tsne.defaults
    }
    if(config$perplexity>(floor(nrow(kk$data)/3)-1)){
      stop("Perplexity is too large for the number of samples")
    }

    ntsne=min(c(round(config$perplexity)*3,nrow(kk$data)-1,ncol(kk$knn_Rnanoflann$indices)))

    if(is.null(config$stop_lying_iter)){
      config$stop_lying_iter = ifelse(is.null(config$Y_init), 250L, 0L)
    }

    if(is.null(config$mom_switch_iter)){
      config$mom_switch_iter = ifelse(is.null(config$Y_init), 250L, 0L)
    }
    res_tsne=Rtsne_neighbors(kk$knn_Rnanoflann$indices[,1:ntsne],kk$knn_Rnanoflann$distances[,1:ntsne],
                             dims = config$dims,
                             perplexity = config$perplexity,
                             theta = config$theta,
                             max_iter = config$max_iter,
                             verbose = config$verbose,
                             Y_init = config$Y_init,
                             stop_lying_iter = config$stop_lying_iter,
                             mom_switch_iter = config$mom_switch_iter,
                             momentum = config$momentum,
                             final_momentum = config$final_momentum,
                             eta = config$eta,
                             exaggeration_factor = config$exaggeration_factor,
                             num_threads = config$num_threads)
  dimensions=res_tsne$Y
  #res_tsne=within(res_tsne, rm(Y))
  res_tsne=res_tsne[names(res_tsne)!="Y"]
    colnames(dimensions)[1:config$dims] = paste ("Dimension", 1:config$dims)
    rownames(dimensions)=rownames(kk$data)
    
  }
  if(mat=="MDS"){ 
    if(is.null(config)){
      config = MDS.defaults
    }
    dimensions=cmdscale(kk$dissimilarity,k=config$dims)
    colnames(dimensions)[1:config$dims] = paste ("Dimension", 1:config$dims)
    rownames(dimensions)=rownames(kk$data)
  }
  if(mat=="UMAP"){ 
    if(is.null(config)){
      config = umap.defaults
    }
    numap=min(c(round(config$n_neighbors)*3,nrow(kk$data)-1,ncol(kk$knn_Rnanoflann$indices)))

    
    u=umap.knn(kk$knn_Rnanoflann$indices[,1:numap],kk$knn_Rnanoflann$distances[,1:numap])
    u$distances[u$distances==Inf]=max(u$distances[u$distances!=Inf])
    config$knn=u
   
    dimensions = umap(kk$data,knn=u,config=config,n_sgd_threads=config$n_sgd_threads,n_threads=config$n_threads)$layout
    colnames(dimensions)[1:config$n_components] = paste ("Dimension", 1:config$n_components)
    rownames(dimensions)=rownames(kk$data)
  }
  dimensions 
}





  
# This function performs a permutation test to assess association between the 
# KODAMA output and any additional related parameters such as clinical metadata.


#k.test = 
#  function (data, labels, n = 100) 
#  {
#    data = as.matrix(data)
#    compmax = min(dim(data))
#    option = 2 - as.numeric(is.factor(labels))
#    
#    w_R2Y = pls.double.cv(data, labels, 1:nrow(data),compmax = 2,perm.test = FALSE,times = 1,runn=1)$medianR2Y
#    
#    v_R2Y = NULL
#    for (i in 1:n) {
#      ss = sample(1:nrow(data))
#      v_R2Y[i] = pls.double.cv(data, labels[ss], 1:nrow(data),compmax = 2,perm.test = FALSE,times = 1,runn=1)$medianR2Y
#    }
#    pval = sum(v_R2Y>w_R2Y)/n
#    pval
#  }




# This function can be used to extract the variable ranking.

#loads = function (model,method=c("loadings","kruskal.test")) 
#{
#  mat=pmatch(method,c("loadings","kruskal.test"))[1]
#  nn = nrow(model$res)
#  for (i in 1:nn) {
#    clu = model$res[i, ]
#    na.clu = !is.na(clu)
#    clu = clu[na.clu]
#    clu=as.numeric(as.factor(clu))
#    red.out = matrix(ncol = ncol(model$data), nrow = nn)
#    if (length(unique(clu)) > 1) {
#      if(mat==1)
#         red.out[i, ] = as.numeric(pls.kodama(Xtrain = model$data[na.clu,], 
#                                              Xtest  = model$data[na.clu,], 
#                                              as.factor(clu), ncomp = 1)$P[, 1])
#      if(mat==2)
#         red.out[i, ] = apply(model$data,2,function(x) -log(kruskal.test(x[na.clu],as.factor(clu))$p.value))
#    }
#  }
#  colMeans(abs(red.out), na.rm = TRUE)
#}




mcplot = function (model){
  A=model$v
  A[,1]=0
  plot(A[1,],type="l",xlim=c(1,ncol(model$v)),ylim=c(0,1),xlab="Numer of interatation",ylab="Accuracy")
  for(i in 1:nrow(A))
      points(A[i,],type="l")
}


                              

core_cpp <- function(x, 
                     xTdata=NULL,
                     clbest, 
                     Tcycle=20, 
                     FUN=c("fastpls","simpls"), 
                     f.par.pls = 5,
                     constrain=NULL, 
                     fix=NULL) {


    QC=quality_control(data_row = nrow(x),
                     data_col = ncol(x),
                     FUN = FUN,
                     f.par.pls = f.par.pls)
  
  matchFUN=QC$matchFUN
  f.par.pls=QC$f.par.pls
  
  if (is.null(constrain)) 
    constrain = 1:length(clbest)
  
  if (is.null(fix)) 
    fix = rep(FALSE, length(clbest))
  if(is.null(xTdata)){
    xTdata=matrix(1,ncol=1,nrow=1)
    proj=1
  }else{
    proj=2
  }
# shake=FALSE
  out=corecpp(x, xTdata,clbest, Tcycle, matchFUN, f.par.pls, constrain, fix, FALSE,proj)
  return(out)
}





