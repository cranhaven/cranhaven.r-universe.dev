CombineTrees2<-function(H1,TT1,H2,TT2,maxSize,FirstSplit){
#%and enforce split
H<-rep(2.3E10,maxSize)
Tl<-list() #=cell(3,1);
TT<-list() #cell(maxSize,1);
availSize<- dim(H1)[1]

for(ms in 2:maxSize){
# %msm=ms-min(maxSize,availSize);
# %H1
# %H1(1+msm:ms-(1+msm))
# %H2
# %H2(ms-(1+msm):-1:1+msm)
# %HN=H1(1+msm:ms-(1+msm))+H2(ms-msm:-1:msm);
  HN<-H1[1:(ms-1)]+H2[seq(from=ms-1,to=1,by=-1)]

  mH<-min(HN)
  mi<-which.min(HN)

  if(mH<H[ms]){
    H[ms:maxSize]<-mH
    Tl[[1]]<-FirstSplit
    Tl[[2]]<-TT1[mi]
    Tl[[3]]<-TT2[ms-mi]
    TT[[ms]]<-Tl
  }
}

H[1]<-0

return(H=H,TT=TT)

}
