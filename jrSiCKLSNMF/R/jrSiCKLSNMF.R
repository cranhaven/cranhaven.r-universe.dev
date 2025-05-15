NULL

#' @title The SickleJr class
#' @description Defines the SickleJr class for use with jrSiCKLSNMF. This object
#' contains all of the information required for analysis using jrSiCKLSNMF. This includes
#' count matrices, normalized matrices, graph Laplacians, hyperparameters, diagnostic plots,
#' and plots of cell clusters.
#' @slot count.matrices A list containing all of the quality controlled count matrices.
#' Note that these count matrices should not use all features and should
#' only include features that appear in at a minimum 10 cells.
#' @slot normalized.count.matrices A list that holds the normalized count matrices
#' @slot graph.laplacian.list A list of the graph Laplacians to be used for graph regularization
#' @slot rowRegularization A string that indicates the type of row regularization to
#' use. Types include "None" and "L2Norm"
#' @slot diffFunc A string that holds the name of the function used to measure the discrepancy between
#' data matrix X and WH for each modality; can be \code{"klp"} for the Poisson Kullback-Leibler divergence
#' or \code{"fr"} for the Frobenius norm
#' @slot lambdaWlist A list of lambda values to use as the hyperparameters for the
#' corresponding \eqn{\mathbf{W}^v} in the \eqn{v^{\text{th}}} modality
#' @slot lambdaH A numeric value corresponding to the hyperparameter of the sparsity constraint on \eqn{\mathbf{H}}
#' @slot Wlist A list of the generated \eqn{\mathbf{W}^v} matrices, one for each modality
#' @slot H The shared \eqn{\mathbf{H}} matrix
#' @slot WHinitials A list that if, when using \code{\link{PlotLossvsLatentFactors}}, all of the cells are used to calculate
#' the initial values, stores these initial generated matrices; can be used
#' as initializations when running \code{\link{RunjrSiCKLSNMF}} to save time
#' @slot lossCalcSubsample A vector that holds the cell indices on which \code{\link{PlotLossvsLatentFactors}} was calculated
#' @slot latent.factor.elbow.values A data frame that holds the relevant information to plot the latent factor elbow plot
#' @slot minibatch Indicator variable that states whether the algorithm should use mini-batch updates.
#' @slot clusterdiagnostics List of the cluster diagnostic results for the SickleJr object. Includes diagnostic plots from \code{\link[factoextra]{fviz_nbclust}} and
#' and diagnostics from \code{\link[clValid]{clValid}}
#' @slot clusters List of results of different clustering methods performed on the SickleJr object
#' @slot metadata List of metadata
#' @slot loss Vector of the value for the loss function
#' @slot umap List of different UMAP-based dimension reductions using \code{\link[umap]{umap}}
#' @slot plots Holds various \code{\link[ggplot2]{ggplot}} results for easy access of diagnostics and cluster visualizations
#' @returns An object of class SickleJr
#' @export
SickleJr<-methods::setClass(
  "SickleJr",
  slots=c(
    count.matrices="list",
    normalized.count.matrices="list",
    graph.laplacian.list="list",
    rowRegularization="character",
    diffFunc="character",
    lambdaWlist="list",
    lambdaH="numeric",
    Wlist="list",
    H="matrix",
    WHinitials="list",
    lossCalcSubsample="vector",
    latent.factor.elbow.values="data.frame",
    minibatch="logical",
    clusterdiagnostics="list",
    clusters="list",
    metadata="list",
    loss="vector",
    umap="list",
    plots="list"
  )
)

#' @title Create an object of class SickleJr
#' @name CreateSickleJr
#' @description Using a list of sparse count matrices, create an object of class SickleJr
#' and specify the names of these count matrices.
#' @param count.matrices A list of quality-controlled count matrices with pre-filtered features where each modality corresponds to each matrix in the
#' list
#' @param names Optional parameter with names for the count matrices in vector format
#' @returns An object of class SickleJr with sparse count matrices added to the \code{count.matrices} slot
#' @examples
#' ExampleSickleJr<-CreateSickleJr(SimData$Xmatrices)
CreateSickleJr<-function(count.matrices,names=NULL){
  if(length(names)!=length(count.matrices)){
    warning("\n Length of names does not match length of list of count matrices. Names of count matrices will remain unchanged.\n")
    names<-NULL
  }
  if(!is.null(names)){
    names(count.matrices)<-names
  }
  #Force matrix to be stored in sparse format.
  count.matrices<-lapply(count.matrices, function(x){if(!.is.sparseMatrix(x)){
    x<-.sparsify(x)
  }else{x}})
  object<-methods::new(Class="SickleJr",count.matrices=count.matrices)
  return(object)
}

#' @title Build KNN graphs and generate their graph Laplacians
#' @description Generate graph Laplacians for graph regularization of
#' jrSiCKLSNMF from the list of raw count matrices using a KNN graph. Note that this
#' is only appropriate when the number of features is considerably greater
#' than the number of cells in all modalities. If this is not the case, please use
#' \code{\link{BuildSNNGraphLaplacians}} or any other method of graph
#' construction that does not rely on the Euclidean distance and store the graph Laplacians for
#' each modality as a list in the \code{graph.laplacian.list} slot.
#' @name BuildKNNGraphLaplacians
#' @param SickleJr An object of class SickleJr
#' @param k Number of KNN neighbors to calculate; by default, is set to 20
#' @returns An object of class SickleJr with a list of graph Laplacians in sparse matrix format
#' added to the \code{graph.laplacian.list} slot
#' @references
#' \insertRef{Lun2016}{jrSiCKLSNMF}
#' @examples
#' SimSickleJrSmall<-BuildKNNGraphLaplacians(SimSickleJrSmall)
#'@export
BuildKNNGraphLaplacians<-function(SickleJr,k=20){
    counts<-SickleJr@count.matrices
    SickleJr@graph.laplacian.list<-lapply(counts,function(x){
    laplacian_matrix(buildKNNGraph(x,transposed=TRUE,k=k))})
  return(SickleJr)
}

#' @title Build SNN graphs and generate their graph Laplacians
#' @description Generate graph Laplacians for graph regularization of
#' jrSiCKLSNMF from the list of raw count matrices using an SNN graph. SNN is more robust to
#' situations where the number of cells outnumbers the number of features. Uses
#' the \code{scran} package's \code{BuildSNNGraph} function \insertCite{Lun2016}{jrSiCKLSNMF}
#' @name BuildSNNGraphLaplacians
#' @param SickleJr An object of class SickleJr
#' @param k Number of KNN neighbors to calculate SNN graph; defaults to 20
#' @returns An object of class SickleJr with list of graph Laplacians in sparse
#' matrix format added to its \code{graph.laplacian.list} slot
#' @references
#' \insertRef{Lun2016}{jrSiCKLSNMF}
#' @examples
#' SimSickleJrSmall<-BuildSNNGraphLaplacians(SimSickleJrSmall)
#'@export
BuildSNNGraphLaplacians<-function(SickleJr,k=20){
    counts<-SickleJr@count.matrices
    SickleJr@graph.laplacian.list<-lapply(counts,function(x){
    laplacian_matrix(buildSNNGraph(x,transposed=TRUE,k=k))})
  return(SickleJr)
}



#' @title Normalize the count matrices and set whether to use the Poisson KL divergence
#' or the Frobenius norm
#' @description Normalize the count data within each modality. The default
#' normalization, which should be used when using the KL divergence, is median
#' library size normalization \insertCite{Zheng2017,Elyanow2020}{jrSiCKLSNMF}. To perform median library size normalization,
#' each count within a cell is divided by its library size (i.e. the counts within a column are divided by the
#' column sum). Then, all values are multiplied by the median library size
#' (i.e. the median column sum). To use the Frobenius norm, set \code{frob=TRUE} to log\eqn{(x+1)}
#' normalize your count data and use a desired \code{scaleFactor}.
#' You may also use a different form of normalization and store these results
#' in the \code{normalized.count.matrices} slot.
#' @name NormalizeCountMatrices
#' @param SickleJr An object of class SickleJr
#' @param diffFunc A string set to "klp" when using the Poisson KL divergence
#'or to "fr" when using the Frobenius norm: default is KL divergence; this also determines
#'the type of normalization
#' @param scaleFactor A single numeric value (if using the same scale factor for each modality)
#' or a list of numeric values to use (if using different scale factors in different modalities)
#' as scale factors for the log\eqn{(x+1)} normalization when \code{diffFunc="fr"}
#' @returns An object of class SickleJr with a list of sparse, normalized data matrices added to its \code{normalized.count.matrices} slot
#' @references
#' \insertRef{Elyanow2020}{jrSiCKLSNMF}
#'
#' \insertRef{Zheng2017}{jrSiCKLSNMF}
#' @examples
#' SimSickleJrSmall<-NormalizeCountMatrices(SimSickleJrSmall)
#' SimSickleJrSmall<-NormalizeCountMatrices(SimSickleJrSmall, diffFunc="fr",scaleFactor=1e6)
#'@export

NormalizeCountMatrices<-function(SickleJr,diffFunc="klp",scaleFactor=NULL){
  Xmatrixlist<-SickleJr@count.matrices
  SickleJr@diffFunc<-diffFunc
  frob=FALSE
  if(diffFunc=="fr"){
    frob=TRUE
  }
  NormalizedXmatrices<-Xmatrixlist
  medianlibsize<-list()
  scaleFactorlist<-list()
  if(!frob){
    medianlibsize<-lapply(Xmatrixlist,function(x) {median(unlist(
      lapply(.listCols(x),function(y) sum(y))))})
    for(i in 1:length(Xmatrixlist)){
      normalizeXmatrixlist<-.listCols(Xmatrixlist[[i]])
      normalizeXmatrixlist<-lapply(normalizeXmatrixlist,function(x){x/sum(x)*medianlibsize[[i]]})
      NormalizedXmatrices[[i]]@x<-unlist(normalizeXmatrixlist)
    }
  } else{
    if(is.list(scaleFactor)){
      if(length(scaleFactor)==length(Xmatrixlist)){
        scaleFactorlist=scaleFactor
      } else{
        if(length(scaleFactor)==1){
          warning(paste0("\n List contains only 1 scale factor. Setting all scale factors to ",scaleFactor[[1]],".\n"))
          for(i in 1:length(Xmatrixlist)){
            scaleFactorlist[[i]]=scaleFactor[[1]]
          }
        } else{
          stop("\n Length of scale factor list not equal to number of assays\n")
        }
      }
    } else if(is.null(scaleFactor)){
      warning("\n No scale factor given. Setting to 1e6 \n")
      for(i in 1:length(Xmatrixlist)){
        scaleFactorlist[[i]]=1e6
      }
    } else if (is.numeric(scaleFactor)){
      warning(paste0("\n Only one scale factor specified. Setting all to ",scaleFactor,".\n"))
      for(i in 1:length(Xmatrixlist)){
        scaleFactorlist[[i]]=scaleFactor
      }
    } else{
      stop("\n Scale factor is not numeric or a list of proper length.\n")
    }
    for(i in 1:length(Xmatrixlist)){
      normalizeXmatrixlist<-.listCols(Xmatrixlist[[i]])
      scaleFactori=scaleFactorlist[[i]]
      normalizeXmatrixlist<-lapply(normalizeXmatrixlist,function(x){x/sum(x)*scaleFactori})
      normalizeXmatrixlist<-lapply(normalizeXmatrixlist,function(x){log(x+1)})
      NormalizedXmatrices[[i]]@x<-unlist(normalizeXmatrixlist)
    }
  }
  SickleJr@normalized.count.matrices<-NormalizedXmatrices
  return(SickleJr)
}


#' @title Set lambda values and type of row regularization for an object of class SickleJr
#' @description Provide the values for the graph regularization \eqn{\lambda_{\textbf{W}^v}}
#' for each modality as a list, provide the value for the sparsity constraint \eqn{\lambda_{\mathbf{H}}},
#' and select whether to use L2 norm regularization.
#' @name SetLambdasandRowReg
#' @param SickleJr An object of class SickleJr
#' @param lambdaWlist A list of graph regularization constraints for the \eqn{\mathbf{W}^v} matrices:
#' defaults to 2 modalities with the RNA modality constraint equal to 10 and the ATAC modality constraint equal to 50
#' @param lambdaH A numeric holding the sparsity constraint on \eqn{\mathbf{H}}: defaults to 500.
#' @param rowReg A string that is equal to \code{"None"} for no constraints on the rows of \eqn{\mathbf{H}} and \code{"L2Norm"}
#' to set the L2 norms of the rows of \eqn{\mathbf{H}} to be equal to 1: defaults to "None"
#' @returns An object of class SickleJr with the lambda hyperparameter values added to its \code{lambdaWlist} and \code{lambdaH} slots and with the
#' row regularization value added to its \code{rowRegularization} slot
#' @examples
#' SimSickleJrSmall<-SetLambdasandRowReg(SimSickleJrSmall,
#' lambdaWlist=list(10,50),lambdaH=500,rowReg="None")
#' SimSickleJrSmall<-SetLambdasandRowReg(SimSickleJrSmall,
#' lambdaWlist=list(3,15),lambdaH=0,rowReg="L2Norm")
#' @export
#'
SetLambdasandRowReg<-function(SickleJr,lambdaWlist=list(10,50),lambdaH=500,rowReg="None"){
  SickleJr@lambdaWlist<-lambdaWlist
  SickleJr@lambdaH<-lambdaH
  SickleJr@rowRegularization<-rowReg
  return(SickleJr)
}

#' @title Initialize the \eqn{\mathbf{W}} matrices in each modality and the shared \eqn{\mathbf{H}} matrix
#' @description Create the \eqn{\mathbf{W}^v} matrices and \eqn{\mathbf{H}} matrix via non-negative double singular
#' value decomposition (NNDSVD) (Boutsidis and Gallopoulus 2008; Gaujoux and Seoighe 2010)
#' or randomization. For randomization, the algorithm runs for 10 rounds
#' for the desired number of random initializations and picks the \eqn{\mathbf{W}^v} matrices and \eqn{\mathbf{H}} matrix with
#' the lowest achieved loss.
#' @name GenerateWmatricesandHmatrix
#' @param SickleJr An object of class SickleJr
#' @param d Number of latent factors to use: defaults to 10
#' @param random Boolean indicating whether to use random initialization (\code{TRUE}) or NNDSVD (\code{FALSE}): default is NNDSVD
#' @param numberReps Number of random initializations to use: default is 5
#' @param seed Random seed for reproducibility of random initializations
#' @param minibatch Indicates whether or not to use the mini-batch algorithm
#' @param batchsize Size of batches for mini-batch NMF
#' @param random_W_updates Indicates whether to only update each \eqn{\mathbf{W}^v} once per round of
#' \eqn{\mathbf{H}} updates; only appropriate for mini-batch algorithms
#' @param subsample A vector of values to use for subsampling; only appropriate
#' when determining proper values for d.
#' @param usesvd Indicates whether to use \code{R}'s singular value decomposition function
#' svd (TRUE) or irlba (FALSE), default is \code{FALSE}; use irlba for larger datasets
#' to increase performance
#' @returns SickleJr An object of class SickleJr with the \eqn{\mathbf{W}^v} matrices and \eqn{\mathbf{H}} matrix added.
#' @references
#' \insertRef{Boutsidis2008}{jrSiCKLSNMF}
#'
#' \insertRef{NMFinR}{jrSiCKLSNMF}
#' @export
#' @examples
#' SimSickleJrSmall<-SetLambdasandRowReg(SimSickleJrSmall,
#' lambdaWlist=list(10,50),lambdaH=500,rowReg="None")
#' SimSickleJrSmall<-GenerateWmatricesandHmatrix(SimSickleJrSmall,d=5,usesvd=TRUE)
#'
GenerateWmatricesandHmatrix<-function(SickleJr,d=10,random=FALSE,
                                      numberReps=100,seed=5,minibatch=FALSE,batchsize=-1,
                                      random_W_updates=FALSE,subsample=1:dim(SickleJr@count.matrices[[1]])[2],
                                      usesvd=FALSE){
  Hconcatmat<-NULL
  NormalizedXmatrices<-SickleJr@normalized.count.matrices
  if(length(subsample)<dim(SickleJr@normalized.count.matrices[[1]])[2]){
    for(i in 1:length(SickleJr@normalized.count.matrices)){
      NormalizedXmatrices[[i]]<-SickleJr@normalized.count.matrices[[i]][,subsample]
    }
  }
  rowReg=SickleJr@rowRegularization

  if(!random){
    svdlist<-lapply(NormalizedXmatrices,function(x) .nndsvd(A=x,k=d,flag=1,svd=usesvd))
    WandMeanlist<-lapply(svdlist,function(x) list(W=x$W,meanW=mean(apply(x$W,2,function(y) sum(y)))))
    SickleJr@Wlist<-lapply(WandMeanlist,function(x) apply(x$W,2,function(y) y/sum(y))*x$meanW)
    bigXmatrix<-do.call(rbind,NormalizedXmatrices)
    svdH<-.nndsvd(A=bigXmatrix,k=d,flag=1,svd=usesvd)
    Hconcatmat<-t(svdH$H)
    if(rowReg=="L2Norm"){
      norms<-apply(Hconcatmat,2,function(x)sqrt(sum(x^2)))
      for(i in 1:dim(Hconcatmat)[[2]]){
        Hconcatmat[,i]=Hconcatmat[,i]/norms[i]
      }
    } else if(rowReg=="L1Norm"){
      Hconcatmat<-apply(Hconcatmat,2,function(x)x/sum(x))
    } else{
      meanHconcat<-mean(apply(Hconcatmat,2,function(x) sum(x)))
      Hconcatmat<-apply(Hconcatmat,2,function(x) x/sum(x))*meanHconcat
    }
    SickleJr@H<-Hconcatmat
  }else{
    output<-rep(0,numberReps)
    for(i in 1:numberReps){

    set.seed(seed+i)
    Unormvals<-lapply(NormalizedXmatrices,
                      function(x) {list(normvals=median(unlist(.listCols(x))),
                      dimW=dim(x)[1])})
    Hvals<-mean(unlist(lapply(Unormvals,function(x) x$normvals)))
    Wvals<-lapply(Unormvals,function(x) {x$normvals<-x$normvals/(Hvals*sqrt(d))
                                          return(x)})
    Hvals<-Hvals/sqrt(d)
    Hdim<-dim(NormalizedXmatrices[[1]])[2]
    Wnew<-lapply(Wvals,function(x) matrix(runif(d*x$dimW,min=0,max=x$normvals),nrow = x$dimW,ncol=d))
    Hconcatmat<-matrix(runif(Hdim*d,min=0,max=Hvals),nrow=Hdim,ncol=d)
    if(rowReg=="L2Norm"){
      norms<-apply(Hconcatmat,2,function(x)sqrt(sum(x^2)))
      for(i in 1:dim(Hconcatmat)[[2]]){
        Hconcatmat[,i]=Hconcatmat[,i]/norms[i]
      }
    } else if(rowReg=="L1Norm"){
      Hconcatmat<-apply(Hconcatmat,2,function(x)x/sum(x))
    } else{
      meanHconcat<-mean(apply(Hconcatmat,2,function(x) sum(x)))
      Hconcatmat<-apply(Hconcatmat,2,function(x) x/sum(x))*meanHconcat
    }
    if(i==1){
      old.W=copy(Wnew)
      old.H=copy(Hconcatmat)
    }
    SickleJr@H<-Hconcatmat
    SickleJr@Wlist<-Wnew
    rm(Hconcatmat)
    rm(Wnew)
    SickleJr<-RunjrSiCKLSNMF(SickleJr,rounds=5,minibatch=minibatch,random_W_updates=random_W_updates,
                   batchsize=batchsize,seed=seed,display_progress = FALSE,suppress_warnings = TRUE)
    output[i]=tail(SickleJr@loss$Loss,1)
    if(which.min(output[which(output>0)])!=i){
      SickleJr@H<-copy(old.H)
      SickleJr@Wlist=copy(old.W)
    }else{
      old.H=copy(SickleJr@H)
      old.W=copy(SickleJr@Wlist)
    }
    }
  }
  return(SickleJr)
}

#' @title Create plots to help determine the number of latent factors
#' @description Generate plots of the lowest achieved loss after a
#' pre-specified number of iterations (default 100) for each latent factor
#' (defaults to 2:20). This operates similarly to a scree plot, so please select
#' a number of latent factors that corresponds to the elbow of the plot.
#' This method is not appropriate for larger sets of data (more than 1000 cells)
#' @name PlotLossvsLatentFactors
#' @param SickleJr An object of class SickleJr
#' @param rounds Number of rounds to use: defaults to 100; this process is time consuming,
#' so a high number of rounds is not recommended
#' @param differr Tolerance for the percentage update in the likelihood: for these plots,
#' this defaults to \eqn{1e-4}
#' @param d_vector Vector of \eqn{d} values to test: default is 2 to 20
#' @param parallel Boolean indicating whether to use parallel computation
#' @param nCores Number of desired cores; defaults to the number of cores of the current machine minus 1 for convenience
#' @param subsampsize Size of the random subsample (defaults to \code{NULL}, which means all cells will be used); using a random subsample decreases computation time but sacrifices accuracy
#' @param minibatch Boolean indicating whether to use the mini-batch algorithm: default is \code{FALSE}
#' @param random Boolean indicating whether to use random initialization to generate the \eqn{\mathbf{W}^v} matrices and \eqn{\mathbf{H}} matrix:
#' defaults to \code{FALSE}
#' @param random_W_updates Boolean parameter for mini-batch algorithm; if \code{TRUE}, only updates \eqn{\mathbf{W}^v} once per epoch on the
#' penultimate subset of \eqn{\mathbf{H}}; otherwise updates \eqn{\mathbf{W}^v} after every update of the subset of \eqn{\mathbf{H}}
#' @param seed Number representing the random seed
#' @param batchsize Desired batch size; do not use if using a subsample
#' @param lossonsubset Boolean indicating whether to calculate the loss on a subset rather than the full dataset; speeds up computation for larger datasets
#' @param losssubsetsize Number of cells to use for the loss subset; default is total number of cells
#' @returns An object of class SickleJr with a list of initialized \eqn{\mathbf{W}^v} matrices and an \eqn{\mathbf{H}} matrix
#' for each latent factor \eqn{d\in\{1,...,D\}} added to the \code{WHinitials} slot, a data frame holding relevant
#' values for plotting the elbow plot added to the \code{latent.factor.elbow.values} slot, diagnostic plots of the loss vs. the number of latent factors added to the \code{plots}
#' slot, and the cell indices used to calculate the loss on the subsample added to the \code{lossCalcSubSample} slot
#' @references
#' \insertRef{ggplot2}{jrSiCKLSNMF}
#' @export
#' @examples
#' SimSickleJrSmall@latent.factor.elbow.values<-data.frame(NULL,NULL)
#' SimSickleJrSmall<-PlotLossvsLatentFactors(SimSickleJrSmall,d_vector=c(2:5),
#' rounds=5,parallel=FALSE)
#' #Next, we commute 2 of these in parallel.
#' \dontrun{
#' SimSickleJrSmall<-PlotLossvsLatentFactors(SimSickleJrSmall,
#' d_vector=c(6:7),rounds=5,parallel=TRUE,nCores=2)}

PlotLossvsLatentFactors<-function(SickleJr,rounds=100,differr=1e-4,d_vector=c(2:20),
                                  parallel=FALSE,nCores=detectCores()-1,subsampsize=NULL,
                                  minibatch=FALSE,random=FALSE,random_W_updates=FALSE,seed=NULL,batchsize=-1,
                                  lossonsubset=FALSE,losssubsetsize=dim(SickleJr@count.matrices[[1]])[2]){
  latentfactors<-SickleJr@latent.factor.elbow.values$latent_factor_number
  if(any(latentfactors%in%d_vector)){
    alreadycalced<-dput(latentfactors[which(latentfactors%in%d_vector)])
      message(paste(alreadycalced,
            "has already been calculated. Skipping calculation for this value"))
    d_vector<-d_vector[-which(d_vector%in%latentfactors)]
    emptyvec<-is_empty(d_vector)
    if(emptyvec){
      latent_factor_number<-SickleJr@latent.factor.elbow.values$latent_factor_number
      Loss<-SickleJr@latent.factor.elbow.values$Loss
      elbowvals<-data.frame(latent_factor_number=latent_factor_number,Loss=Loss)
      s=""
      if(rounds>1){
        s="s"
      }
      plotvals<-ggplot(elbowvals,aes(x=latent_factor_number,y=Loss))+geom_line()+geom_point()+theme_bw()+ggtitle(paste0("Plot of Number of Latent Factors vs Loss after ",rounds," Iteration",s))+xlab("Number of Latent Factors")
      print(plotvals)
      stop("No uncalculated values for d. Plotting loss values for latent factors and exiting...\n")
    }
  }
  if(is.numeric(seed)){
    set.seed(seed)
  }
  initsamp=NULL
  if(!lossonsubset){
    initsamp=1:losssubsetsize
  }else{
    initsamp<-sample(1:dim(SickleJr@normalized.count.matrices[[1]])[2],losssubsetsize,replace=FALSE)
  }
  if(is.numeric(subsampsize)&(minibatch)){
    stop("It is not appropriate to use both the minibatch algorithm and a subsample.
         Please select only one.\n")
  }
  samp<-NULL
  if(is.null(subsampsize)){
    subsampsize=dim(SickleJr@count.matrices[[1]])[2]
    message("\n Subsample not specified. Setting to number of cells. \n")
    samp<-1:subsampsize
  } else  if(subsampsize>=dim(SickleJr@count.matrices[[1]])[2]){
    warning("\n Subsample size is greater than or equal to number of cells. Using all cells.\n")
    subsampsize=dim(SickleJr@count.matrices[[1]])[2]
    samp=1:dim(SickleJr@count.matrices[[1]])[2]
  } else if (!is.numeric(subsampsize)){
    stop("\n Subsample size is not numeric. Exiting...")
  }else {
    samp=sample(1:dim(SickleJr@count.matrices[[1]])[2],subsampsize,replace=FALSE)
  }
  if(random_W_updates & !minibatch){
    warning("\n Random W updates are only appropriate for minibatch algorithms. Setting random_W_updates to FALSE.\n" )
    random_W_updates=FALSE
  }
  if(batchsize>dim(SickleJr@count.matrices[[1]])[2]){
    warning("\n Batch size larger than number of cells. Will not use the minibatch algorithm \n")
    batchsize=-1
    minibatch=FALSE
  }
  AdjL=lapply(SickleJr@graph.laplacian.list,function(x) {x@x[which(x@x>0)]=0
  return(-x)})
  DL=lapply(SickleJr@graph.laplacian.list,function(x) {x@x[which(x@x<0)]=0
  return(x)})
  SickleJrSub<-list()
  for(i in 1:length(SickleJr@normalized.count.matrices)){
    SickleJrSub[[i]]<-SickleJr@normalized.count.matrices[[i]][,samp]
  }
  if(!minibatch){
    initsamp=1:dim(SickleJrSub[[1]])[2]
  }
  if(losssubsetsize<dim(SickleJrSub[[1]])[2]){
    initsamp<-sample(1:dim(SickleJrSub[[1]])[2],batchsize,replace=FALSE)
    #prepare initsamp for c++
    initsamp<-initsamp-1
  }else{initsamp=1:dim(SickleJrSub[[1]])[2]-1}
  WHinitials<-NULL
  lossvals<-NULL
  if(!parallel){
    cl=NULL} else{
      cl<-makeCluster(nCores)
      clusterExport(cl,varlist=c("SickleJr","d_vector","random","samp","AdjL","DL","differr","rounds","initsamp","SickleJrSub"),envir = environment())
      clusterEvalQ(cl,library("jrSiCKLSNMF"))
    }
  message("\n Calculating initial WH matrices... \n")
  WHinitials<-pblapply(X=d_vector,FUN=function(x) {newvals=GenerateWmatricesandHmatrix(SickleJr,d=x,random=random,subsample=samp)
  list(d=x,Wlist=newvals@Wlist,H=newvals@H)},cl=cl)
  message("\n Running jrSiCKLSNMF... \n")
  lossvals<-pblapply(X=WHinitials,FUN=function(x){
      vals<-jrSiCKLSNMF(datamatL=SickleJrSub,
                      WL=x[["Wlist"]],
                      H=x[["H"]],
                      AdjL=AdjL,
                      DL=DL,
                      lambdaWL=SickleJr@lambdaWlist,
                      lambdaH=SickleJr@lambdaH,
                      Hconstraint = toString(SickleJr@rowRegularization),
                      differr=differr,
                      display_progress=FALSE,
                      rounds=rounds,
                      diffFunc = toString(SickleJr@diffFunc),
                      minrounds=rounds,
                      suppress_warnings=TRUE,
                      initsamp = 1:dim(SickleJrSub[[1]])[2])
    return(vals[length(vals)])},cl=cl)
   if(parallel) {
     stopCluster(cl)
   }
    newlikelihoodvalues<-NULL
    for(i in 1:length(lossvals)){
      newlikelihoodvalues<-rbind(newlikelihoodvalues,min(lossvals[[i]]$Loss))
    }
    oldvals<-SickleJr@latent.factor.elbow.values
    SickleJr@latent.factor.elbow.values<-rbind(oldvals,data.frame(latent_factor_number=d_vector,Loss=newlikelihoodvalues))
    elbowvals=SickleJr@latent.factor.elbow.values
    plotvals<-ggplot(elbowvals,aes(x=latent_factor_number,y=Loss))+geom_line()+geom_point()+theme_bw()+ggtitle(paste0("Plot of Number of Latent Factors vs Loss after ",rounds," Iterations"))+xlab("Number of Latent Factors")
    print(plotvals)
    SickleJr@plots[["LossvsLatentFactors"]]<-plotvals
    SickleJr@WHinitials<-WHinitials
    SickleJr@lossCalcSubsample<-samp
  return(SickleJr)
}


#' @title Create elbow plots of the singular values derived from IRLBA
#' to determine D for large datasets
#' @description This generates v+1 plots, where v is the number of data modalities, of the approximate
#' singular values generated by IRLBA.There is one plot for each modality and then a
#' final plot that concatenates all of the modalities together. Choose the largest elbow
#' value among the three plots.
#' @param SickleJr An object of class SickleJr
#' @param d Number of desired factors; it is important to select a number that
#' allows you to see a clear elbow: defaults to 50.
#' @returns An object of class SickleJr with plots for IRLBA diagnostics added to its \code{plots} slot
#' @references
#' \insertRef{irlba}{jrSiCKLSNMF}
#' @export
#' @examples
#' SimSickleJrSmall<-DetermineDFromIRLBA(SimSickleJrSmall,d=5)
DetermineDFromIRLBA<-function(SickleJr,d=50){

  modalitylabels<-names(SickleJr@normalized.count.matrices)
  modalitylabels<-c(modalitylabels,"Concatenated")
  concatmat<-do.call("rbind",SickleJr@normalized.count.matrices)
  normcounts<-SickleJr@normalized.count.matrices
  normcounts[[length(normcounts)+1]]<-concatmat
  message("Calculating IRLBA for all data matrices")
  IRLBAlist<-pblapply(normcounts,function(x) irlba(x,nv=d))
  message("Preparing data for plotting")
  Singular_values=NULL
  Index=NULL
  dataframelist<-pblapply(IRLBAlist,function(x){Index=c(1:length(x$d))
    Singular_values=x$d
    data.frame(Index,Singular_values)})
  for(i in 1:length(dataframelist)){
    dataframelist[[i]]<-cbind(dataframelist[[i]],Name=rep(modalitylabels[i],length(dataframelist[[i]][,1])))
  }
  message("Generating Plots")
  ggplots<-pblapply(dataframelist,function(x) ggplot<-ggplot(x,aes(Index,Singular_values))+geom_point()+geom_line()+ggtitle(paste0("Index vs. Singular Value for ",x$Name[1]," Data"))+theme_bw()+ylab("Singular Value")+xlab("Index of Singular Value"))
  lapply(ggplots,FUN=function(x) print(x))
  SickleJr@plots[["DiagnosticIRLBA"]]<-ggplots
  return(SickleJr)
}

#' @title Set \eqn{\mathbf{W}} matrices and \eqn{\mathbf{H}} matrix from pre-calculated values
#' @description
#' Use values calculated in the step to determine number of latent factors in the initial
#' steps for the jrSiCKLSNMF algorithm. If only a subset was calculated, this produces an error.
#' In this case, please use \code{\link{GenerateWmatricesandHmatrix}} to generate new
#' \eqn{\mathbf{W}^v} matrices and a new \eqn{\mathbf{H}} matrix.
#' @param SickleJr An object of class SickleJr
#' @param d The number of desired latent factors
#' @returns An object of class SickleJr with the \code{Wlist} slot and the \code{H} slot filled from pre-calculated values.
#' @examples SimSickleJrSmall<-SetWandHfromWHinitials(SimSickleJrSmall,d=5)
#' @export
#'
SetWandHfromWHinitials<-function(SickleJr,d){
  numcells<-dim(SickleJr@normalized.count.matrices[[1]])[2]
  numcellsinit<-dim(SickleJr@WHinitials[[1]][["H"]])[1]
  latent_factor_number=SickleJr@latent.factor.elbow.values$latent_factor_number
  if(!(d%in%latent_factor_number)){
    stop("\n Initial values for W and H and have not been calculated for the specified number of latent factors. Exiting...\n")
  }
  whichd<-which(unlist(lapply(SickleJr@WHinitials,function(x) x[["d"]]==d)))
  listwh<-copy(SickleJr@WHinitials[[whichd]])

  if(numcells!=numcellsinit){
    stop("\n Number of total cells differs from number of cells used in calculation for the initial plot. Use 'GenerateWMatricesandHmatrix' to generate the initial matrices. \n")
  } else{
    SickleJr@Wlist<-SickleJr@WHinitials[[whichd]][["Wlist"]]
    SickleJr@H<-SickleJr@WHinitials[[whichd]][["H"]]}
  return(SickleJr)
}

#' @title Run jrSiCKLSNMF on an object of class SickleJr
#' @description Wrapper function to run jrSiCKLSNMF on an object of class SickleJr. Performs jrSiCKLSNMF on
#' the given SickleJr
#' @name RunjrSiCKLSNMF
#' @param SickleJr An object of class SickleJr
#' @param rounds Number of rounds: defaults to 2000
#' @param differr Tolerance for percentage change in loss between updates: defaults to 1e-6
#' @param display_progress Boolean indicating whether to display the progress bar for jrSiCKLSNMF
#' @param lossonsubset Boolean indicating whether to use a subset to calculate the loss function
#' rather than the whole dataset
#' @param losssubsetsize Size of the subset of data on which to calculate the loss
#' @param minibatch Boolean indicating whether to use mini-batch updates
#' @param batchsize Size of batch for mini-batch updates
#' @param random_W_updates Boolean indicating whether or not to use random_W_updates updates
#' (i.e. only update \eqn{\mathbf{W}^v} once per mini-batch epoch)
#' @param seed Number specifying desired random seed
#' @param minrounds Minimum number of rounds: most helpful for the mini-batch algorithm
#' @param suppress_warnings Boolean indicating whether to suppress warnings
#' @param subsample A numeric used primarily when finding an appropriate number of
#' latent factors: defaults to total number of cells
#' @returns An object of class SickleJr with updated \eqn{\mathbf{W}^v} matrices, updated \eqn{\mathbf{H}} matrix, and a vector of values for
#' the loss function added to the \code{Wlist}, \code{H}, and \code{loss} slots, respectively
#' @references
#' \insertRef{Cai2008}{jrSiCKLSNMF}
#'
#' \insertRef{jnmf2009}{jrSiCKLSNMF}
#'
#' \insertRef{Eddelbuettel2011}{jrSiCKLSNMF}
#'
#' \insertRef{Eddelbuettel2014}{jrSiCKLSNMF}
#'
#' \insertRef{Elyanow2020}{jrSiCKLSNMF}
#'
#' \insertRef{halfbakednmf}{jrSiCKLSNMF}
#'
#' \insertRef{Lee1999}{jrSiCKLSNMF}
#'
#' \insertRef{Liu2013}{jrSiCKLSNMF}
#'
#' @examples SimSickleJrSmall<-RunjrSiCKLSNMF(SimSickleJrSmall,rounds=5)
#' @export
RunjrSiCKLSNMF<-function(SickleJr,rounds=30000,differr=1e-6,
                         display_progress=TRUE,lossonsubset=FALSE,losssubsetsize=dim(SickleJr@H)[1],
                         minibatch=FALSE,batchsize=1000,random_W_updates=FALSE,seed=NULL,minrounds=200,
                         suppress_warnings=FALSE,subsample=1:dim(SickleJr@normalized.count.matrices[[1]])[2]){
  SickleJr@minibatch<-FALSE
  if(minibatch){
    SickleJr@minibatch<-TRUE
  }
  if(is.numeric(seed)){
    set.seed(seed)
  }
  initsamp=NULL
  if(!lossonsubset){
    if(losssubsetsize!=dim(SickleJr@H)[1]){
      warning("\n Using a subset to calculate the loss was set to FALSE while the size of the subset for the loss was not equal to the number of cells. Setting the loss subset to the total number of cells... \n")
    }
    losssubsetsize=dim(SickleJr@H)[1]
  }
  if(losssubsetsize<dim(SickleJr@H)[1]){
    initsamp<-sample(1:dim(SickleJr@normalized.count.matrices[[1]])[2],losssubsetsize,replace=FALSE)
    #prepare initsamp for c++
    initsamp<-initsamp-1
  }else{initsamp=1:dim(SickleJr@H)[1]-1}
  AdjL=lapply(SickleJr@graph.laplacian.list,function(x) {x@x[which(x@x>0)]=0
                                                          return(-x)})
  DL=lapply(SickleJr@graph.laplacian.list,function(x) {x@x[which(x@x<0)]=0
                                                        return(x)})
  usemats<-lapply(SickleJr@normalized.count.matrices,function(x) x[,subsample])
  outputloss<-jrSiCKLSNMF(datamatL=usemats,
                          WL=SickleJr@Wlist,H=SickleJr@H,
                          AdjL=AdjL,
                          DL=DL,
                          lambdaWL=SickleJr@lambdaWlist,
                          lambdaH=SickleJr@lambdaH,
                          Hconstraint=toString(SickleJr@rowRegularization),
                          differr=differr,
                          display_progress = display_progress,
                          rounds=rounds,
                          diffFunc=SickleJr@diffFunc,
                          minibatch=minibatch,
                          batchsize=batchsize,
                          initsamp=initsamp,
                          random_W_updates=random_W_updates,
                          minrounds=minrounds,
                          suppress_warnings=suppress_warnings)
  SickleJr@loss<-outputloss
  return(SickleJr)
}

#' @title Plot a diagnostic plot for the mini-batch algorithm
#' @description To ensure sufficient convergence of the loss for jrSiCKLSNMF with mini-batch updates, we
#' plot the loss vs the number of iterations for the mini-batch algorithm.
#' After a certain number of iterations, the loss should appear to oscillate
#' around a value. Before continuing with downstream analyses, please ensure that
#' the loss exhibits this sort of behavior. For the mini-batch algorithm, it is not
#' possible to use the convergence criteria used for the batch version of the
#' algorithm.
#' @param SickleJr An object of class SickleJr
#' @returns An object of class SickleJr with mini-batch diagnostic plots added to the \code{plots} slot.
#' @examples SimSickleJrSmall<-MinibatchDiagnosticPlot(SimSickleJrSmall)
#' @export

MinibatchDiagnosticPlot<-function(SickleJr){
  if(!SickleJr@minibatch){
    warning("\n This is not a mini-batch algorithm, so this plot won't give you particularly helpful diagnostics.")
  }
  niterations<-4:length(SickleJr@loss$Loss)
  loss<-SickleJr@loss$Loss[-c(1:3)]
  lossdata<-data.frame(niterations=niterations,loss=loss)
  g<-ggplot(lossdata,aes(x=niterations,y=loss))+geom_line()+theme_bw()+ggtitle("Plot of Loss vs. Number of Iterations")+xlab("Number of Iterations")+ylab("Loss")
  print(g)
  SickleJr@plots[["Mini-batch_Diagnostics"]]<-g
}

#' @title Perform clustering diagnostics
#' @description
#' A wrapper for the \code{\link[clValid]{clValid}} and \code{\link[factoextra]{fviz_nbclust}} functions to perform clustering diagnostics
#' @param SickleJr An object of class SickleJr
#' @param numclusts A vector of integers indicating the number of clusters to test
#' @param clusteringmethod String holding the clustering method: defaults to k-means; since the other methods
#' are not implemented in jrSiCKLSNMF, it is recommended to use k-means.
#' @param diagnosticmethods Vector of strings indicating which methods to plot. Defaults to all three of the available: wss, silhouette, and gap_stat
#' @param clValidvalidation String containing validation method to use for \code{clValid}. Defaults to internal.
#' @param createDiagnosticplots Boolean indicating whether to create diagnostic plots for cluster size
#' @param runclValidDiagnostics Boolean indicating whether to calculate the diagnostics from \code{clValid}
#' @param printPlots Boolean indicating whether to print the diagnostic plots
#' @param printclValid Boolean indicating whether to print the diagnostic results from \code{clValid}
#' @param subset Boolean indicating whether to calculate the diagnostics on a subset of the data rather
#' than on the whole dataset.
#' @param subsetsize Numeric value indicating size of the subset
#' @param seed Numeric value holding the random seed
#' @references
#' \insertRef{clValid}{jrSiCKLSNMF}
#'
#' \insertRef{factoextra}{jrSiCKLSNMF}
#' @returns An object of class SickleJr with cluster diagnostics added to its \code{clusterdiagnostics} slot
#' @examples #Since these data are too small, the clValid diagnostics do not run
#' #properly. See the vignette for an example with the clValid diagnostics
#' SimSickleJrSmall<-DetermineClusters(SimSickleJrSmall,numclusts=2:5,runclValidDiagnostics=FALSE)
#' @export
DetermineClusters<-function(SickleJr,numclusts=2:20,clusteringmethod="kmeans",
                            diagnosticmethods=c("wss","silhouette","gap_stat"),
                            clValidvalidation="internal",createDiagnosticplots=TRUE,
                            runclValidDiagnostics=TRUE,printPlots=TRUE,
                            printclValid=TRUE,
                            subset=FALSE,subsetsize=1000,
                            seed=NULL){

  ingraph<-FALSE
  inclValid<-FALSE
  if((clusteringmethod%in% c("kmeans","clara","fanny","dbscan","Mclust","HCPC","hkmeans"))&
     !(clusteringmethod%in% c("hierarchical","kmeans","diana","fanny","som","model","sota","pam","clara","agnes"))){
    warning("\n This clustering method is not available in clValid. Only graphs will be printed and saved in the clusterdiagnostics slot.\n")
    ingraph<-TRUE
    }else if(!(clusteringmethod%in% c("kmeans","clara","fanny","dbscan","Mclust","HCPC","hkmeans"))&
           (clusteringmethod%in% c("hierarchical","kmeans","diana","fanny","som","model","sota","pam","clara","agnes"))){
      warning("\n This clustering method is not available in factoextra. Only information from clValid will be printed and saved in the clusterdiagnostics slot.\n")
      inclValid<-TRUE
  }else if ((clusteringmethod%in% c("kmeans","clara","fanny","dbscan","Mclust","HCPC","hkmeans"))&
            (clusteringmethod%in% c("hierarchical","kmeans","diana","fanny","som","model","sota","pam","clara","agnes"))){
    ingraph<-TRUE
    inclValid<-TRUE
  }else{
    stop("\n Valid clustering method not selected. Please select a valid clustering method. Options include:\n 'kmeans','clara','fanny','dbscan','Mclust',HCPC','hkmeans','hierarchical','diana','som','model','pam',and 'agnes'.")
  }
  ggplotlist<-list()
  rownames(SickleJr@H)<-colnames(SickleJr@count.matrices[[1]])
  clValidobj<-NULL
  Hrun<-NULL
  if(subset&subsetsize>dim(SickleJr@H)[1]){
    warning("Size of subset is greater than the number of cells. Using full dataset")
    subset=FALSE
  }
  if(subset){
    if(is.numeric(seed)){
      set.seed(seed)
    }
    subset<-sample(1:dim(SickleJr@H)[1],subsetsize,replace = FALSE)
    Hrun<-SickleJr@H[subset,]
  }else{
    Hrun<-SickleJr@H
  }
  if(!createDiagnosticplots){
    ingraph=FALSE
  }
  if(ingraph){
    ggplotlist<-lapply(diagnosticmethods,function(x) fviz_nbclust(Hrun,match.fun(clusteringmethod),method=x,k.max=max(numclusts)))
    if(printPlots){
      lapply(ggplotlist,function(x) print(x))
    }
  }
  if(!runclValidDiagnostics){
    inclValid<-FALSE
  }
  if(inclValid){
    clValidobj<-clValid(Hrun,clMethods=clusteringmethod,validation=clValidvalidation,nClust=numclusts)
    if(printclValid){
      print(summary(clValidobj))
    }
  }
  SickleJr@clusterdiagnostics<-list(DiagnosticPlots=ggplotlist,clValidResults=clValidobj)
  return(SickleJr)
}

#' @title Cluster the \eqn{\mathbf{H}} matrix
#' @description Perform k-means, spectral clustering, clustering based off of the
#' index of the maximum latent factor, or Louvain community detection on the \eqn{\mathbf{H}} matrix.
#' Defaults to k-means.
#' @name ClusterSickleJr
#' @param SickleJr An object of class SickleJr
#' @param numclusts Number of clusters; can be NULL when method is "max" or "louvain"
#' @param method String holding the clustering method: can choose "kmeans" for
#' k-means clustering, "spectral" for spectral clustering, "louvain" for Louvain
#' community detection or "max" for clustering based on the maximum row value; note that
#' "max" is only appropriate for jrSiCKLSNMF with L2 norm row regularization
#' @param neighbors Number indicating the number of neighbors to use to generate the
#' graphs for spectral clustering and Louvain community detection: both of these
#' methods require the construction of a graph first (here we use KNN);
#' defaults to 20 and unused when the clustering method equal to "kmeans" or "max"
#' @param louvainres Numeric containing the resolution parameter for Louvain
#' community detection; unused for all other methods
#' @examples SimSickleJrSmall<-ClusterSickleJr(SimSickleJrSmall,3)
#' SimSickleJrSmall<-ClusterSickleJr(SimSickleJrSmall,method="louvain",neighbors=5)
#' SimSickleJrSmall<-ClusterSickleJr(SimSickleJrSmall,method="spectral",neighbors=5,numclusts=3)
#' #DO NOT DO THIS FOR REAL DATA; this is just to illustrate max clustering
#' SimSickleJrSmall<-SetLambdasandRowReg(SimSickleJrSmall,rowReg="L2Norm")
#' SimSickleJrSmall<-ClusterSickleJr(SimSickleJrSmall,method="max")
#' @references
#' \insertRef{louvain}{jrSiCKLSNMF}
#'
#' \insertRef{Lun2016}{jrSiCKLSNMF}
#'
#' \insertRef{Ng2001}{jrSiCKLSNMF}
#'
#' \insertRef{Schliep2016}{jrSiCKLSNMF}
#'
#' \insertRef{maxcluster}{jrSiCKLSNMF}
#' @returns SickleJr- an object of class SickleJr with added clustering information
#' @export
ClusterSickleJr<-function(SickleJr,numclusts,method="kmeans",neighbors=20,louvainres=0.3){
  clust<-NULL
  if(method=="kmeans"){
   clust<-kmeans(SickleJr@H,centers=numclusts,nstart=1000)$cluster
  }else if(method=="spectral"){
    clust<-specClust(SickleJr@H,centers=numclusts,nn=neighbors)$cluster
  }else if(method=="louvain"){
    knngraph<-buildKNNGraph(SickleJr@H,transposed=TRUE,k=neighbors)
    newvals<-cluster_louvain(knngraph,resolution = louvainres)
    clust<-newvals$membership
  }else if(method=="max"){
    if(SickleJr@rowRegularization!="L2Norm"){
      stop("\n Clustering based off the maximum latent factor per observation is not appropriate for H matrices that do not utilize the 'L2Norm' constraint\n")
    }
    clust<-apply(SickleJr@H,1,function(x) which.max(x))
    }else {print("Please enter 'kmeans' for k-means clustering, 'spectral' for spectral clustering,
                'louvain' for Louvain community detection or 'max' to cluster based on
                the maximum latent factor for each observation. Please note that 'max'
                is only appropriate for the L2Norm-based variant.")}
  SickleJr@clusters[[method]]<-clust
  return(SickleJr)
}


#' @title Calculate the UMAP for an object of class SickleJr
#' @description Perform UMAP on the \eqn{\mathbf{H}} matrix alone (default) or within a modality by
#' using UMAP on the \eqn{W^vH} corresponding to modality \eqn{v}.
#' @name CalculateUMAPSickleJr
#' @param SickleJr An object of class SickleJr
#' @param umap.settings Optional settings for the \code{\link[umap]{umap}}; defaults to \code{\link[umap]{umap.defaults}}
#' @param modality A number corresponding to the desired modality; if set, will perform UMAP on
#' \eqn{\mathbf{W}^{\mathtt{modality}}}%*%t(\eqn{\mathbf{H}}) rather than
#' on \eqn{\mathbf{H}} alone; not recommended for datasets of more than 1000 cells
#' @returns An object of class SickleJr with UMAP output based on the \eqn{\mathbf{H}} matrix alone or within a modality added to its \code{umap} slot
#' @references
#' \insertRef{UMAP}{jrSiCKLSNMF}
#' @examples
#' #Since this example has only 10 observations,
#' #we need to modify the number of neighbors from the default of 15
#' umap.settings=umap::umap.defaults
#' umap.settings$n_neighbors=2
#' SimSickleJrSmall<-CalculateUMAPSickleJr(SimSickleJrSmall,
#' umap.settings=umap.settings)
#' SimSickleJrSmall<-CalculateUMAPSickleJr(SimSickleJrSmall,
#' umap.settings=umap.settings,modality=1)
#' SimSickleJrSmall<-CalculateUMAPSickleJr(SimSickleJrSmall,
#' umap.settings=umap.settings,modality=2)
#' @export
CalculateUMAPSickleJr<-function(SickleJr,umap.settings=umap::umap.defaults,modality=NULL){
  if(!is.null(modality)){
    WH<-t(SickleJr@Wlist[[modality]]%*%t(SickleJr@H))
    WHname<-paste0("W",modality,"H")
    SickleJr@umap[[WHname]]<-umap::umap(WH,config=umap.settings)
  }else{
    SickleJr@umap[["H"]]<-umap::umap(SickleJr@H,config=umap.settings)
  }
  return(SickleJr)
}

#' @title Add metadata to an object of class SickleJr
#' @description Add any type of metadata to an object of class SickleJr. Metadata
#' are stored in list format under the name specified in \code{metadataname} of each node in slot \code{metadata}.
#' @name AddSickleJrMetadata
#' @param SickleJr An object of class SickleJr holding at least one count matrix of omics data
#' @param metadata Metadata to add to the SickleJr object; there are no restrictions on type
#' @param metadataname A string input that indicates the desired name for the added
#' metadata.
#' @returns An object of class SickleJr with added metadata
#' @examples SimSickleJrSmall<-AddSickleJrMetadata(SimSickleJrSmall,
#' SimData$cell_type,"cell_types_full_data")
#' @export
AddSickleJrMetadata<-function(SickleJr,metadata,metadataname){
  SickleJr@metadata[[metadataname]]<-metadata
  return(SickleJr)
}

#' @title Generate UMAP plots for an object of class SickleJr
#' @description Plot the first and second dimensions of a UMAP dimension reduction
#' and color either by clustering results or metadata.
#' @name PlotSickleJrUMAP
#' @param SickleJr An object of class SickleJr
#' @param umap.modality String corresponding to the name of the UMAP of interest: defaults to \code{"H"}
#' @param cluster String input that indicates which cluster to color by: defaults to \code{"kmeans"}
#' @param title String input for optional \code{\link[ggplot2:ggplot]{ggplot}} plot title
#' @param colorbymetadata Name of metadata column if coloring by metadata
#' @param legendname String input that to allow specification of a different legend name
#' @returns An object of class SickleJr with plots added to the \code{plots} slot
#' @references
#' \insertRef{ggplot2}{jrSiCKLSNMF}
#' @examples SimSickleJrSmall<-PlotSickleJrUMAP(SimSickleJrSmall,
#' title="K-Means Example")
#' SimSickleJrSmall<-PlotSickleJrUMAP(SimSickleJrSmall,umap.modality=1)
#' @export
PlotSickleJrUMAP<-function(SickleJr,umap.modality="H",cluster="kmeans",title="",colorbymetadata=NULL,legendname=NULL){
  if(is.null(colorbymetadata)){
    color=SickleJr@clusters[[cluster]]
    if(is.null(legendname)){
      legendname=paste(cluster, "cluster")
      }

  }else{
    color=SickleJr@metadata[[colorbymetadata]]
    if(is.null(legendname)){legendname=colorbymetadata}
    cluster=colorbymetadata
  }
  UMAP1=SickleJr@umap[[umap.modality]]$layout[,1]
  UMAP2=SickleJr@umap[[umap.modality]]$layout[,2]
  umapvals<-data.frame(UMAP1=UMAP1,UMAP2=UMAP2,cluster=color)
  plots<-ggplot(umapvals,aes(x=UMAP1,y=UMAP2,color=factor(color)))+geom_point()+theme_bw()+
    ggtitle(title)+guides(color=guide_legend(title=legendname))
  SickleJr@plots[[paste0(umap.modality,":",cluster)]]<-plots
  print(plots)
  return(SickleJr)
}
