repfdr <- function(pdf.binned.z, binned.z.mat, non.null = c('replication','meta-analysis','user.defined'),
                   non.null.rows = NULL, Pi.previous.result=NULL, control = em.control(),clusters = NULL,clusters.ldr.report=NULL, clusters.verbose=T)
{
  #we handle the case of clusterd results in a seperate function
  if(!is.null(clusters)){
    return(repfdr_clusters(pdf.binned.z = pdf.binned.z,
                                    binned.z.mat = binned.z.mat,
                                    clusters = clusters,
                                    non.null = non.null,
                                    Pi.previous.result = Pi.previous.result,
                                    control = control,
                                    clustering.ldr.report = clusters.ldr.report,
                                    clustering.verbose = clusters.verbose))
  }
  
  nr_studies = dim(pdf.binned.z)[1]
  
  #we check which studies are available for computation
  kept_studies = rep(T,nr_studies)
  for(i in 1:nr_studies){
    if(dim(pdf.binned.z)[3]==2){
      kept_studies[i]   = !(is.na(pdf.binned.z[i,1,2]))      
    }else if(dim(pdf.binned.z)[3]==3){
      kept_studies[i]   = !(is.na(pdf.binned.z[i,1,3]))
    }

  }
  if(length(which(kept_studies))==0){
    stop("Must have at least one study with estimated fraction of nulls below 1.")
  }
  #we take only the studies needed
  pdf.binned.z.original = pdf.binned.z
  binned.z.mat.original = binned.z.mat
  
  if(sum(kept_studies)>1){
    pdf.binned.z = pdf.binned.z[which(kept_studies),,]
    binned.z.mat = binned.z.mat[,which(kept_studies)]
  }else{
    pdf.binned.z = array(pdf.binned.z[kept_studies,,],dim = c(1,dim(pdf.binned.z[kept_studies,,])))
    binned.z.mat = matrix(binned.z.mat[,kept_studies],ncol = 1) 
  }
 
  
  #we use PI from a previous result
  if (is.null(Pi.previous.result)){
    Pi <- piem(pdf.binned.z, binned.z.mat, control)$last.iteration
  } else {
    Pi <- Pi.previous.result
  }
  
  #matrix of all combinations of (-1,0,1) or (0,1) for the different studies
  H  <- Pi[,-dim(Pi)[2],drop=FALSE]
  
  #which rows are defined as h0 in H
  h0 <- switch(match.arg(non.null), replication = which(apply(H,1,function(y){ sum(y==1)<=1 & sum(y==-1)<=1 })),
               "meta-analysis" = which(rowSums(abs(H))==0),
               user.defined = (1:dim(H)[1])[-non.null.rows])
  
  if(non.null=='user.defined' & is.null(non.null.rows))
    stop("'user.defined' is selected but rows are not specified.")
  
  if(non.null!='user.defined' & !is.null(non.null.rows))
    warning(sprintf("%s is selected, supplied rows are ignored.",non.null))
  

  maximal_Hrows = (dim(pdf.binned.z)[3])^(nr_studies) # We cannot check against dim(H)[1], as we might have removed studies,
  #therefore the number of rows of the final H is given by the choose expression
  if(length(non.null.rows) >= maximal_Hrows)
    stop("Number of selected configurations is larger than possible.")
  
  #computing the local fdr, per -1/0/1 combination of the H matrix, for chunks of SNPs
  chunksize <- 20000
  if (dim(binned.z.mat)[1] <= chunksize)
  {
    chunkbegin <- 1
    chunkend   <- dim(binned.z.mat)[1]
  }
  else
  {
    chunkbegin <- c(1,seq(from = chunksize,to = dim(binned.z.mat)[1],by = chunksize))
    chunkend   <- c(chunkbegin[-1]-1,dim(binned.z.mat)[1])
  }
  
  #putting NAs in studies that were removed
  #rows which of non null H values for studies removed of probabilty NA
  if(sum(kept_studies)!=nr_studies){
    
    order_by = hconfigs(nr_studies,dim(pdf.binned.z)[3])
    H = order_by
    h0 <- switch(match.arg(non.null), replication = which(apply(order_by,1,function(y){ sum(y==1)<=1 & sum(y==-1)<=1 })),
                 "meta-analysis" = which(rowSums(abs(order_by))==0),
                 user.defined = (1:dim(order_by)[1])[-non.null.rows])
    cols_taken = which(kept_studies)
    cols_not_taken = which(!kept_studies)
    current_result_pointer = 1
    Pi_new = matrix(NA,nrow(order_by),nr_studies+1)
    Pi_new[,1: nr_studies] = order_by
    titles=rep(NA,nr_studies+1)
    for(i in 1:nrow(Pi_new)){
      values_taken = Pi_new[i,cols_taken]
      values_not_taken = Pi_new[i,cols_not_taken]
      if(sum(values_not_taken!=rep(0,length(values_not_taken)))==0){
        which_row_to_take_from_result = which_row(Pi_new[i,cols_taken],Pi)
        #cat("row to take: ",which_row_to_take_from_result,'\n')
        Pi_new[i,nr_studies+1] =  Pi[which_row_to_take_from_result , ncol(Pi)] #take value 
      }else{
        Pi_new[i,nr_studies+1] = NA
      }
    }
    for(i in 1:nr_studies){
      titles[i] = paste0('Study ',i)
    }
    titles[nr_studies + 1] = 'Pi'
    Pi_new[is.na(Pi_new)]=0 #nas are caused by a dropped study, being non null in a row
    Pi = Pi_new
    colnames(Pi) = titles
  }
  
  pdf.binned.z.original[is.na(pdf.binned.z.original)] = 0
  fdr <- NULL
  for (b in 1:length(chunkbegin)) {
    fh <- ldr(pdf.binned.z.original, binned.z.mat.original[chunkbegin[b]:chunkend[b],,drop=FALSE],Pi = Pi, h.vecs = h0)
    if (dim(fh)[1]==1)
      fdr <- c(fdr,fh[,-(1:dim(H)[2])])  
    else
      fdr <- c(fdr,colSums(fh[,-(1:dim(H)[2]),drop=FALSE]))  
  }
  
  #computing the local Fdr
  o <- order(fdr)
  ro <- order(o)
  Fdr <- (cumsum(fdr[o])/(1:length(fdr)))[ro]
  
  
  return (list(mat = cbind(fdr = fdr, Fdr = Fdr) ,Pi = Pi))
}

#function to find index of row in table_to_look_in
which_row = function(row,table_to_look_in){
  #we assume there is only one
  p = length(row)
  flag_vec = rep(T,nrow(table_to_look_in))
  for(i in 1:p){
    flag_vec = flag_vec & (table_to_look_in[,i] == row[i])
  }
  return(which(flag_vec))
}

piem <- function(pdf.binned.z, binned.z.mat, control = em.control())
{
  inputchk(pdf.binned.z, binned.z.mat)
  
  n.studies <- dim(pdf.binned.z)[1]
  n.bins <- dim(pdf.binned.z)[2]
  n.association.status=dim(pdf.binned.z)[3]
  
  H <- hconfigs(n.studies,n.association.status)
  
  if (n.association.status == 2)
  {
    pbz <- array(c(array(0,dim=c(n.studies,n.bins,1)),pdf.binned.z),
                 dim=c(n.studies,n.bins,3))
  }
  
  if (n.association.status == 3)
    pbz <- pdf.binned.z
    
  if (is.null(control$pi.initial)) {
    control$pi.initial <- rep(0.1 / (dim(H)[1] - 1), dim(H)[1])
    zer.idx <- apply((H == 0), 1, all)
    control$pi.initial[zer.idx] <- 0.9
  }
  
  h     <- apply(H + 1, 2, as.integer)
  bzm <- apply(binned.z.mat - 1, 2, as.integer)
  
  out <- .Call('REM', h, pi.initial = control$pi.initial, pbz, bzm, max.iter = control$max.iter,
               tol = control$tol, nr.threads = control$nr.threads, verbose = control$verbose)
  EMi   <- out[1, control$max.iter + 1]
  out <- out[, 1:EMi,drop=FALSE]
  if (EMi == control$max.iter) {
    warning('EM did not converge within tolerance after maximum number of iterations specified.', call. = FALSE)
  } else {
    if(control$verbose)
      cat("Converged after",EMi,"EM iterations.")
  }
  return(list(all.iterations=cbind(H,out),
              last.iteration=cbind(H,Pi=out[,dim(out)[2]])))
}  

ldr <- function(pdf.binned.z, binned.z.mat ,Pi, h.vecs = NULL)
{
  if (is.vector(binned.z.mat)) #when only one test is selected
    binned.z.mat <- matrix(binned.z.mat,nrow=1,ncol=length(binned.z.mat))
  
  inputchk(pdf.binned.z, binned.z.mat)
  
  n.studies <- dim(pdf.binned.z)[1]
  n.bins <- dim(pdf.binned.z)[2]
  n.association.status <- dim(pdf.binned.z)[3]
  
  if (n.association.status == 2)
  {
    pbz <- array(c(array(0,dim=c(n.studies,n.bins,1)),pdf.binned.z),
                 dim=c(n.studies,n.bins,3))
  }
  
  if (n.association.status == 3)
    pbz <- pdf.binned.z
    
  H  <- Pi[,-dim(Pi)[2],drop=FALSE]
  nrowH <- dim(H)[1]
  ncolH <- dim(H)[2]
  
  if(ncolH!=n.studies)
    stop('First dimension of pdf.binned.z do not match the number of studies indicated by Pi.')
  
  nrowbz <- dim(binned.z.mat)[1]
    
  if (is.null(h.vecs))
    h.vecs = 1:dim(H)[1] else
      if (max(h.vecs) > nrowH)
        stop("Number of configurations to report is larger than possible.")
  
  fzh <- matrix(nrow = nrowH, ncol = nrowbz)
  pbz[is.na(pbz)]=0
  for(i in 1:nrowH)
  {
    term <- vector(length = nrowbz)
    term[] <- 1
    for (j in 1:ncolH)
      term <- term*pbz[j,binned.z.mat[,j],H[i,j]+2]
    fzh[i,] <- term
  }
  
  Pi.NA.to.ZERO = Pi[,dim(Pi)[2]]
  Pi.NA.to.ZERO[is.na(Pi.NA.to.ZERO)] = 0
  Pih0     <- Pi.NA.to.ZERO
  Pih0[is.na(Pih0)] = 0
  Pih0[-h.vecs] <- 0
  
  fh   <- matrix(t(t(fzh * Pih0) / as.numeric(Pi.NA.to.ZERO %*% fzh ))[h.vecs,],
                 nrow=length(h.vecs),ncol=dim(binned.z.mat)[1])
  colnames(fh) <- rownames(binned.z.mat)
  return(cbind(H[h.vecs,,drop=FALSE],fh))
}

hconfigs <- function(n.studies,n.association.status = 3,studies.names=NULL)
{
  if (n.association.status == 3) Hstates <- c(0,-1,1)
  if (n.association.status == 2) Hstates <- c(0,1) 
  
  H <- as.matrix(expand.grid(replicate(n.studies,Hstates,FALSE)))
  
  if (is.null(studies.names))
    colnames(H) <- sprintf("Study %i",1:n.studies) else
    colnames(H) <- studies.names
  return(H)
}

inputchk <- function(pdf.binned.z, binned.z.mat){
  
  # pdf.binned.z dimension restriction
  if (!(dim(pdf.binned.z)[3] %in% c(2,3)))
    stop('dim(pdf.binned.z)[3] should be equal to 2 or 3')
  
  # binned.z.mat value restriction (not NA or NaN):
  if (any(is.na(binned.z.mat)) | any(!is.finite(binned.z.mat)))
      stop('binned.z.mat should contain positive integers only.')
  
  # binned.z.mat value restriction (positive integer):
  if (any(!is.int(binned.z.mat)) | !any(as.logical(binned.z.mat)))
    stop('binned.z.mat should contain positive integers only.')
  
  # binned.z.mat value restriction (maximum bin number):
  if (max(binned.z.mat) > dim(pdf.binned.z)[2])
    stop('bin number(s) in \'binned.z.mat\' is out of bin\'s range specified in \'pdf.binned.z.\'')
  
  # binned.z.mat & pdf.binned.z march in number of studies:
  if (dim(binned.z.mat)[2] != dim(pdf.binned.z)[1])
    stop('number of studies in pdf.binned.z is different than in binned.z.mat')
  
  # pdf.binned.z value restriction (0<=p<=1):
  if (any(pdf.binned.z[!is.na(pdf.binned.z)]<0 & pdf.binned.z[!is.na(pdf.binned.z)]>1))
    stop('pdf.binned.z should contain values between 0 and 1.')
  
  return(NULL)
}

is.int <- function(x, tol = .Machine$double.eps^0.5)  (x - round(x)) < tol

em.control <- function(pi.initial = NULL, max.iter = 1e4, tol = 1e-12, nr.threads = 0, verbose = TRUE)
{
  if (!is.null(pi.initial)) if (any(pi.initial<0 | pi.initial>1))
    stop("pi.initial values should be between 0 and 1.")

  if (!is.int(max.iter))
    stop('max.iter should be integer.')
  
  if (!is.int(nr.threads))
    stop('nr.threads should be integer.')
  
  if (tol <= 0)
    stop('tol should be positive number.')
  
  max.iter <- as.integer(max.iter)
  nr.threads = as.integer(nr.threads)
  verbose = as.integer(verbose)
  return (list(pi.initial = pi.initial, max.iter = max.iter, tol = tol, nr.threads = nr.threads, verbose = verbose))
}