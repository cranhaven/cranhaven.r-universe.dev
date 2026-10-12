repfdr_clusters <- function(pdf.binned.z, binned.z.mat,clusters, non.null = c('replication','meta-analysis'),
                   Pi.previous.result=NULL, control = em.control(),clustering.ldr.report = NULL,clustering.verbose = F)
{

  if(!(non.null %in% c('replication','meta-analysis'))){stop('for Cluster Analysis only replication and meta-analysis are allowd, (no option user defined)')}
  
  nr_studies = dim(pdf.binned.z)[1] #total number of studies in analysis
  n_association_status = dim(pdf.binned.z)[3] #association status used, 2 or 3
  n_bins = dim(pdf.binned.z)[2] #number of bins in discretization
  
  Pi_list = list() # list of pi results from running repfdr in each cluster
  nr_clusters = max(clusters) # number of clusters
  
  # we now check that the vector of cluster partitions is legal:
  CLUSTERS_STOP_MSG = "Argument Clusters must be a vector of integers, covering all values between 1 and the chosen number of clusters. Use NULL for single cluster analysis."
  if(sum(clusters<1 )>0){stop(CLUSTERS_STOP_MSG)}
  for(i in 1:length(clusters)){if(!is.integer(clusters[i])){stop(CLUSTERS_STOP_MSG)}}
  for(i in 1:nr_clusters){if(sum(clusters ==i)<1){stop(CLUSTERS_STOP_MSG)}}
  
  #holders for the current cluster parameters, when doing the per cluster repfdr
  current_pdf.binned.z = NULL 
  current_binned.z.mat = NULL
  current_Pi.previous.result = NULL
  
  # these are lists of the parameters and results for the per cluster repfdrs
  cluster.ind.list  = list()
  pdf.binned.z.list = list()
  pdf.binned.z.list.index0 = list()
  pdf.binned.z.list.index1 = list()
  pdf.binned.z.list.index2 = list()
  
  binned.z.mat.list = list()
  repfdr.res.list   = list()
  repfdr.mat.list = list()
  repfdr.Pi.list = list()
  repfdr.Pi.list.NA.corrected = list()
  
  #actual iteration over the clusters
  for(i in 1:nr_clusters){
    #getting cluster parameters
    cluster_ind = which(clusters == i)
    cluster.ind.list [[ i ]] = cluster_ind
    current_Pi.previous.result = Pi.previous.result[cluster_ind]
    if(length(cluster_ind)>1){
      current_pdf.binned.z = pdf.binned.z[cluster_ind,,]
      current_binned.z.mat = binned.z.mat[,cluster_ind]
    }else{
      current_pdf.binned.z = array(pdf.binned.z[cluster_ind,,],dim = c(1,dim(pdf.binned.z[cluster_ind,,])))
      current_binned.z.mat = matrix(binned.z.mat[,cluster_ind],ncol = 1) 
    }
    
    pdf.binned.z.list[[i]] = current_pdf.binned.z
    pdf.binned.z.list.index0 [[ i ]] = matrix(current_pdf.binned.z[,,1],ncol = dim(current_pdf.binned.z)[2] ,nrow = length(cluster_ind))
    pdf.binned.z.list.index1 [[ i ]] = matrix(current_pdf.binned.z[,,2],ncol = dim(current_pdf.binned.z)[2] ,nrow = length(cluster_ind))
    if(n_association_status==3){
      pdf.binned.z.list.index2 [[ i ]] = matrix(current_pdf.binned.z[,,3],ncol = dim(current_pdf.binned.z)[2] ,nrow = length(cluster_ind))  
    }
    binned.z.mat.list[[i]] = current_binned.z.mat
    
    if(clustering.verbose){cat(paste0("repfdr cluster :",i,"\n"))}
    
    repfdr.res.list[[i]] = repfdr::repfdr(current_pdf.binned.z,
                                          current_binned.z.mat,
                                          non.null[1],
                                          Pi.previous.result = current_Pi.previous.result,
                                          control = control)
    
    if(clustering.verbose){cat(paste0("\n"))}
    
    repfdr.mat.list[[i]]   =  repfdr.res.list[[i]]$mat
    repfdr.Pi.list[[i]]    =  repfdr.res.list[[i]]$Pi
    
    #handling NAs and NaNs
    repfdr.Pi.list.NA.corrected[[i]] = repfdr.Pi.list[[i]]
    repfdr.Pi.list.NA.corrected[[i]][is.na(repfdr.Pi.list.NA.corrected[[i]])] = 0
    pdf.binned.z.list.index0 [[ i ]][is.na(pdf.binned.z.list.index0 [[ i ]])] = 0
    pdf.binned.z.list.index1 [[ i ]][is.na(pdf.binned.z.list.index1 [[ i ]])] = 0
    if(n_association_status == 3){
      pdf.binned.z.list.index2 [[ i ]][is.na(pdf.binned.z.list.index2 [[ i ]])] = 0  
    }
    
    
  }
  
  
  Rcpp_res = NULL
  non.null.trans = NULL
  non.null.u=2
  
  #number of rows in ldr matrix
  lfdr_mat_rows = choose(3+nr_studies-1,3-1)
  if(n_association_status == 2){
    lfdr_mat_rows = choose(2+nr_studies-1,2-1)
  }
  
  #thresholding on number of non null hypothesis for the aggregated local fdr
  if(non.null == 'replication'){non.null.trans=0 ; non.null.u = 2}
  if(non.null == 'meta-analysis'){non.null.trans=1 ; non.null.u = 1}
  
  #ldr reports
  
  ldr_report_code = 0
  lfdr_ncol = 1
  if(!is.null(clustering.ldr.report)){
    if(clustering.ldr.report[1] == "ALL"){
      ldr_report_code = 1
      lfdr_ncol = (dim(binned.z.mat)[1])
    }else{
      ldr_report_code = 2
      lfdr_ncol =  length(clustering.ldr.report)
    }
  }
  
  lfdr_mat = matrix(NA,nrow = lfdr_mat_rows,ncol = lfdr_ncol)
  
  fdr_vec = rep(NA,(dim(binned.z.mat)[1]))
  Fdr_vec = rep(NA,(dim(binned.z.mat)[1]))
  
  #we now iterate over SNPs and aggregate the results
  for(i in 1:(dim(binned.z.mat)[1])){
    #index of the current SNP
    current_SNP=as.integer(i)
    if(clustering.verbose){
      if(i%%round((dim(binned.z.mat)[1])/100) == 1)
      cat(paste0('Doing SNP: ',current_SNP,'\n\r')) 
    }
    
    #performing the per SNP aggregation of lfdr
    i_is_last = (i==dim(binned.z.mat)[1]) #PI is computed only for the last i
    Rcpp_res = rcpp_main(Sizes = c(nr_studies,n_bins,n_association_status,
                                            nr_clusters,non.null.trans,non.null.u,current_SNP,0,1*i_is_last), #0 is for the debug value
                                  pdf.binned.z.list.index0,
                                  pdf.binned.z.list.index1,
                                  pdf.binned.z.list.index2,
                                  binned.z.mat.list,
                                  cluster.ind.list,
                                  repfdr.Pi.list.NA.corrected
    )
    
    #under this formulation, do we really need a different analysis for meta & rep?
    if(non.null == 'replication'){
      if(n_association_status == 2)
        h1_rows = which(Rcpp_res[[1]][,2] >= non.null.u)
      if(n_association_status == 3)
        h1_rows = which(Rcpp_res[[1]][,1] >= non.null.u | Rcpp_res[[1]][,3] >= non.null.u)  
    }
    if(non.null == 'meta-analysis'){
      if(n_association_status == 2)
        h1_rows = which(Rcpp_res[[1]][,2] >= non.null.u)
      if(n_association_status == 3)
        h1_rows = which(Rcpp_res[[1]][,1] >= non.null.u | Rcpp_res[[1]][,3] >= non.null.u) 
    }
    
    lfdr = (Rcpp_res[[2]]) / sum(Rcpp_res[[2]]) #computing the aggregated local fdr
    if(ldr_report_code>0){
      if(ldr_report_code == 1){
        lfdr_mat[,i] = lfdr    
      }else if(ldr_report_code == 2){
        col_to_report = which(clustering.ldr.report == i)
        if(length(col_to_report)>0){
          lfdr_mat[,col_to_report[1]] = lfdr
        }
      }
    }
    
    fdr = sum(lfdr[-h1_rows])
    fdr_vec[i] = fdr
  }
  
  o <- order(fdr_vec)
  ro <- order(o)
  Fdr_vec <- (cumsum(fdr_vec[o])/(1:length(fdr_vec)))[ro]
  
  ret = list(repfdr.mat.percluster = repfdr.mat.list,
             repfdr.Pi.percluster = repfdr.Pi.list,
             mat = data.frame(fdr = fdr_vec,Fdr = Fdr_vec))
  
  #add col names to association values (0,1) or (-1,0,1)
  comb_mat = Rcpp_res[[1]]
  if(n_association_status == 2){
    comb_mat = comb_mat[,-c(3)]
    colnames(comb_mat) = c("H:0","H:1")
  }
  if(n_association_status == 3){
    colnames(comb_mat) = c("H:-1","H:0","H:1")
  }
  
  #handle ldr reporting
  if(ldr_report_code>0){
    # add col names to SNP LFDRs
    if(ldr_report_code == 1){
      colnames(lfdr_mat) = paste0("SNP ", 1:ncol(lfdr_mat))
    }else if(ldr_report_code == 2){
      colnames(lfdr_mat) = paste0("SNP ", clustering.ldr.report)
    }
    ldr = cbind(comb_mat,lfdr_mat)
    ret$ldr = ldr 
  }
  PI = cbind(comb_mat,Rcpp_res[[4]])
  colnames(PI) = c(colnames(comb_mat),'PI')
  ret$Pi =PI
  return (ret)
}


