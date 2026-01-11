#'Calculate numbers of sharing alleles each pair at each locus
#'@details This function calculates the numbers of shared alleles between each pair of individuals for a dataset.
#'@usage AlleleShare(df,sep,replacement=FALSE)
#'@importFrom utils combn
#'@param df a dataframe of genotype data with rownames of sample ID and column names of markers.
#'@param replacement a logical variable. If it is TRUE, the pairs are sampled with replacement; if FALSE (default), the pairs are sampled without replacement.
#'@param sep allele separator in the imported genotype data. Note: when using the special character like "|", remember to protect it as "\\|"(default).
#'@return a dataframe of numbers of shared alleles. Each row denotes each pair; Each column denotes each locus.
#'@export
#'@examples
#'df <- data.frame(SNP1=c("A|A","T|T","A|T","A|T"),
#'                 STR1=c("12|12","13|14","13|13","14|15"))
#'AlleleShare(df,"\\|",replacement=FALSE)
#'

AlleleShare <- function(df,sep="\\|",replacement=FALSE){
  n <- nrow(df)
  m <- ncol(df)
  if (replacement){
    d<-combn(n,2,simplify = TRUE)
    b1 <- df[d[1,],]
    b2 <- df[d[2,],]
  }else{
    d1<-sample(n,n/2,replace = FALSE)
    d2<-sample(setdiff(c(1:n),d1))
    b1<-df[d1,]
    b2<-df[d2[1:(n/2)],]
  }

  a11<-matrix(sapply(strsplit(as.matrix(b1),sep),"[",1),nrow = nrow(b1),ncol=m,byrow = F)
  a12<-matrix(sapply(strsplit(as.matrix(b1),sep),"[",2),nrow = nrow(b1),ncol=m,byrow = F)
  a21<-matrix(sapply(strsplit(as.matrix(b2),sep),"[",1),nrow = nrow(b2),ncol=m,byrow = F)
  a22<-matrix(sapply(strsplit(as.matrix(b2),sep),"[",2),nrow = nrow(b2),ncol=m,byrow = F)

  MAX <- data.frame(t(data.frame(a11 =as.vector(a11),a12=as.vector(a12),a21=as.vector(a21),a22=as.vector(a22))))

  AS <- function(x){
    a11 <- x[1]
    a12 <- x[2]
    a21 <- x[3]
    a22 <- x[4]
    ind1<-sort(c(a11,a12))
    ind2<-sort(c(a21,a22))
    if (identical(ind1,ind2)){
      as <- 2
    }else{
      as <- length(intersect(ind1,ind2))
    }
    return(as)
  }

  output <-matrix(sapply(MAX, AS),nrow = nrow(b1),ncol = m)
  colnames(output) <- colnames(df)
  pair <-rbind(rownames(b1),rownames(b2))
  rownames(output)<-sapply(data.frame(pair),function(x){paste(x[1],x[2])})
  return(data.frame(output))
}
