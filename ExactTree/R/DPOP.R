DPOP<-function(Y,k,defMeasure=NULL, Prior=NULL, LossM=NULL, ClassSizes=NULL, verbose = TRUE){
# % DP for rank-order constrained partitioning n objects described by Y, into k groups
# % using the heterogeneity measure indicated by measure
# % H(2:k) returns the overall heterogeneity values for the partition into 2..k clusters
# % where H(1) returns the overall heterogeneity
# % P returns all partitions, for 2 up to k clusters
# % I returns the sort order, on which the splits are defined
if (is.null(defMeasure)<3){
  defMeasure <- 0
}


n<-NROW(Y)
m<-NCOL(Y)

if(m>1){
  warning('The function only works for one dimensional dependent variable Y')
  #print('only for one dimensional dependent variable Y')
  return()
}


#measure<-globalenv()$measure
#Heap<-globalenv()$Heap
#SplitHeap<-globalenv()$SplitHeap

#tic;
#cpu1=cputime;
time<-proc.time()

measure<-defMeasure
O<-sort(Y)
I<-order(Y)
Heap<-rep(0,n)
SplitHeap<-matrix(0, nrow = n, ncol= (k-2) )
H<-c()
#Global Heterogeneity
H[1]<-GetHeterogeneity(Y=O, measure=measure, Prior=Prior, LossM=LossM, ClassSizes=ClassSizes)

#Compute all heterogeneity values for ordered subsets of Y (=O)
Hetero<-matrix(0, nrow = n, ncol=n)
if(measure==0){ #speedup for minimizing SS
  SO<-c(0,cumsum(O))
  SSO<-c(0,cumsum(O^2))

  for(s in 1:(n-1)){ #%split s= split between object s and s+1
    for(ss in (s+1):n){
      ni<-(ss+1-s)
      Hetero[s,ss]<-(SSO[ss+1]-SSO[s])-((SO[ss+1]-SO[s])^2/ni) #raw score formula using cum sums
    }
    Heap[s]<-Hetero[1,s]
  }
}else if(measure==1000){ #measure ==0 vectorized no additional speedup in MATLAB 7
  SO<-c(0,cumsum(O))
  SSO<-c(0,cumsum(O^2))
  for(s in 1:(n-1)){ #%split s= split between object s and s+1
    Hetero[s,(s+1):n]<-(SSO[(s+2):(n+1)]-SSO[s])-((SO[(s+2):(n+1)]-SO[s])^2/(2:(1+n-s)))
    Heap[s]<-Hetero[1,s]
  }
}else{
  for(s in 1:(n-1)){ #%split s= split between object s and s+1
    for(ss in (s+1):n){
      Hetero[s,ss]=GetHeterogeneity(Y=O[s:ss], measure=measure, Prior=Prior, LossM=LossM, ClassSizes=ClassSizes)
      #Hetero(s,ss)=sum((O(s:ss)-mean(O(s:ss))).^2);
    }
    Heap[s]<-Hetero[1,s]
  }
}

#cpu2=cputime;
time2<-proc.time()
#%disp(['time for first phase ',num2str(cpu2-cpu1)]);
#%toc

#%Do all, but last stage (stage=ki)
for(ki in 2:(k-1)){
  for(e in seq(n,ki,-1)){
    minH<-Heap[ki-1]+Hetero[ki,e]
    minS<-ki-1
    for(s in ki:(e-1)){ #split s= split between object s and s+1
      newH<-Heap[s]+Hetero[s+1,e]
      if(newH<minH){
        minH<-newH
        minS<-s
      }
    }
    Heap[e]<-minH
    SplitHeap[e,ki-1]<-minS
  }
  H[ki]<-Heap[n]
}

#%Do Last Stage
minH<-Heap[k-1]+Hetero[k,n]
minS<-k-1
for(s in k:(n-1)){ #%split s= split between object s and s+1
  newH<-Heap[s]+Hetero[s+1,n]
  if(newH<minH){
    minH<-newH
    minS<-s
  }
}
H[k]<-minH

time3<-proc.time()-time #cpu2=cputime;
if(verbose){
  print(paste0('elapsed time ', time3[3]))
  print(paste0('cpu time ', time3[2]))
}

#%Get optimal partitions
P<- matrix(0,nrow = k, ncol = (k-1))
for(ki in 2:(k-1)){
  P[ki,]<-ResolveSplits(n,ki,P[ki,1:(k-1)],SplitHeap)
}
P[k,k-1]<-minS
P[k,]<-ResolveSplits(minS,k-1,P[k,1:(k-1)],SplitHeap)

return(list(H=H,P=P,I=I, measure=measure, Heap=Heap))

}


ResolveSplits<-function(n,k,SP,SplitHeap){
#%Resolve optimal partition from split heap
#global SplitHeap;
#SplitHeap<-globalenv()$SplitHeap
SP[k-1]<-SplitHeap[n,k-1]
if(k>2){
  SP<-ResolveSplits(SP[k-1],k-1,SP,SplitHeap)
}

return(SP)
}
