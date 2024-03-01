#'Constructing object of class bnInfo
#'
#'This function constructs an object of class bnInfo which is needed for Bayesian network
#'based clustering; see function \link{bnclustOmics}. In this object the names and types of omics data are stored as well
#'as maappings containing the correspondance between gene names in each omic type and gene names used
#'in blacklist and edge penalization matrices in the clustering step. These mappings are helpful for constructing
#'such matrices. For example, transcriptome data often includes ensemble IDs and mutation data includes gene names. If we want to
#'penalize all interactions which are not found in a specific interactions database, we need to pass an interaction list
#'this list usually includes gene names and not ensemble IDs. Mappings pass the information needed to assign the edges between any IDs of gene X
#'the specified penalization factor. If some omics types already have the same ID as in interaction list, corresponding mappings can be skipped.
#'
#'@param omicdata a list of matrices containing data, rows are observations, columns are variables (the order should be as following binary->ordinal->continuous)
#'@param types a vector of characters equal in length to the number of provided omic matrices, "b" binary, "o" ordinal, "c" continuous
#'@param omics a vector of omic names, must be the same as names of elements in omicdata, otherwise names of omicdata will be overwritten
#'@param mappings mappings containing a gene symbol for each omic type, rownames have to contain column names of the parameter 'omicdata'; column "gene" must be present; if NULL for a certain omic type, than gene  name will be taken from the column name of the corresponding matrix.
#'@param attachtype when TRUE .O will be attached to each variable name, where O is omic name (see parameter 'omics'); when FALSE (default) .O is only attached to duplicated names
#'@return an object of class bnInfo
#'@examples
#'#with mappings
#'bnnames<-bnInfo(toydata,c("b","o","c","c","c"),c("M","CN","T","P","PP"),mappings)
#'#no mappings
#'bnnames<-bnInfo(simdata,c("b","c"),c("M","T"))
#'@export
bnInfo<-function(omicdata,types,omics,mappings=NULL,attachtype=FALSE) {
  res<-list()
  samplenames<-Reduce('rbind',lapply(omicdata,rownames))
  cond<-all(apply(samplenames,2,function(x)length(unique(x))==1))
  if(!cond) stop("sample names (row names) must be the same between omic types!")

  bin_ind<-which(types=="b")
  ord_ind<-which(types=="o")
  cont_ind<-which(types=="c")
  if(!all(bin_ind<ord_ind) | !all(bin_ind<cont_ind) |!all(ord_ind<cont_ind)) stop("omic types have to be ordered! (b->o->c)")
  if(!all(unlist(lapply(omicdata,function(x)!is.null(x))))) stop("emtpy omicdata entries are now allowed")
  if(length(bin_ind>0)) nb<-sum(unlist(lapply(omicdata[bin_ind],ncol))) else nb<-0
  if(length(ord_ind>0)) no<-sum(unlist(lapply(omicdata[ord_ind],ncol))) else no<-0
  if(length(cont_ind>0)) nc<-sum(unlist(lapply(omicdata[cont_ind],ncol))) else stop("continuous types must be present!")

  res$nb<-nb
  res$no<-no
  res$nc<-nc
  res$totn<-nb+no+nc
  res$types<-types
  res$omics<-omics

  allnames<-lapply(omicdata,colnames)
  ns<-unlist(lapply(allnames,length))
  allnamesone<-unlist(allnames)
  alltypesone<-rep(omics,ns)


  omicranges<-list()
  for(i in 1:length(omics)) {
    omicranges[[omics[i]]]<-which(alltypesone==omics[i])
  }
  res$omicranges<-omicranges

  alltypesoneind<-rep(1:length(omics),ns)
  allnamesonebn<-allnamesone

  if(attachtype) {
    for(i in 1:length(allnamesonebn)) {
      allnamesonebn[i]<-paste(allnamesone[i],".",omics[alltypesoneind[i]],sep="")
    }
  } else {
    dups<-which(duplicated(allnamesone))
    for(i in dups) {
      allnamesonebn[i]<-paste(allnamesone[i],".",omics[alltypesoneind[i]],sep="")
    }
  }




  for(i in 1:length(omics)) {
    allnames[[i]]<-cbind(allnames[[i]],allnamesonebn[omicranges[[omics[i]]]]) #double check
    if(!is.null(mappings[[omics[i]]])) {
      allnames[[i]]<-cbind(allnames[[i]],mappings[[omics[i]]][allnames[[i]][,1],"gene"])
    } else {
      allnames[[i]]<-cbind(allnames[[i]],allnames[[i]][,1])
    }
    colnames(allnames[[i]])<-c("name","bnname","gene")
    rownames(allnames[[i]])<-allnames[[i]][,"bnname"]
    names(allnames)[i]<-omics[i]
  }
  res$ns<-ns
  res$allnames<-allnames
  #res$allnamesone<-allnamesone
  res$allnamesonebn<-allnamesonebn
  class(res)<-"bnInfo"
  return(res)
}
