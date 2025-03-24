#' Creates a string with the cues for each frequency band and segment seperated by "_"
#' @param WAVE A Wave object (see \link{tuneR}). Currently it is implemented for use with 16kHz sampling rate.
#' @param IntensitySteps Number of steps that the intensity gets compressed to. Default is 5.
#' @param Smooth A parameter for using the kernel smooth function provied by the package zoo.
#' @return A string containing the coding. Each band and part is seperated by "_"
#' @import zoo
#' @import tuneR
#' @import seewave
#' @examples \dontrun{
#'          
#'          library(tuneR)
#'          library(seewave)
#'          Wave=readWave("MyWaveFile.wav")
#'          if(Wave@samp.rate!=16000){
#'          Wave=resamp(Wave,f=Wave@samp.rate,g=16000,output="Wave")
#'          }
#'          Cues=makeCues(Wave,IntensitySteps=5,Smooth=800)
#'          
#'          }
#' @export
#' @author Denis Arnold

makeCues<-function(WAVE,IntensitySteps=5,Smooth=800){

  CODED="tooShort"
  if(length(WAVE)>800){

    ps=powspec(WAVE@left,sr=WAVE@samp.rate,wintime=0.005,steptime=0.005)
    SPEC=log(audspec(ps,sr=WAVE@samp.rate,fbtype="mel")$aspec+1)
    #SPEC=ceiling((SPEC-min(SPEC))/sd(SPEC))
    SPEC=ceiling((SPEC-min(SPEC))*(IntensitySteps/abs(range(SPEC)[2]-range(SPEC)[1])))
    BOUNDARY=getBoundary(WAVE,Smooth)/80
    CODED=""

    if(length(BOUNDARY)>0){
      PARTS=data.frame(start=c(1,ceiling(BOUNDARY)),stop=c(ceiling(BOUNDARY),dim(SPEC)[2]))
      for(i in 1:dim(PARTS)[1]){CODED=paste(CODED,CODE(SPEC[,(PARTS$start[i]:PARTS$stop[i])],i),sep="_")}
    }
    else{
      CODED=CODE(SPEC,1)
    }
  }
  return(gsub("^_","",CODED))
}


#' Helper function for makeCues that splits the signal based on the envelope of the signal
#' @param Wave A Wave object (see tuneR)
#' @param smooth A parameter for using the kernel smooth function provied by the package zoo.
#' @return A vector with the sample numbers of the boundaries.
#' @importFrom stats kernel
#' @examples 
#'        \dontrun{
#'        library(tuneR)
#'        Wave=readWave("MyWaveFile.wav")
#'        Boundaries=getBoundary(Wave,800)
#'        }
#' @export
#' @author Denis Arnold

getBoundary<-function(Wave,smooth=800){
  if(length(Wave)>(1000+(2*smooth))){
    ENV=env(Wave,ksmooth=kernel("daniell",smooth),plot=F)
    ENVzoo=as.zoo(ENV)
    MIN=rollapply(ENVzoo,1000, function(x) which.min(x)==500)
    INDEX=index(MIN[coredata(MIN)])
    return(ceiling(INDEX*(length(Wave)/length(ENV))))}
}

#' Helper function for makeCues
#' @export
#' @param SPEC Spectrum representation made in makeCues()
#' @param num Number of the part
#' @return A string containing the coding. Each band is seperated by "_".
#' @export
#' @importFrom stats median
#' @author Denis Arnold

CODE<-function(SPEC,num){

  LINES=dim(SPEC)[1]
  END=dim(SPEC)[2]

  OUT=vector(mode="character",length=LINES)

  for(i in 1:LINES){

    OUT[i]=paste("b",i,"start",SPEC[i,1],"median",median(SPEC[i,]),"min",min(SPEC[i,]),"max",max(SPEC[i,]),"end",SPEC[i,END],"part",num,sep="")
  }

  return(paste(OUT,collapse="_"))
}

