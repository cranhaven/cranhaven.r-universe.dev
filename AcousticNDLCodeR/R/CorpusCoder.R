#' Codes a corpus for use with NDL with vector of wavefile names and a vector of TextGrid names provided
#' @param Waves Vector with names (and full path to if not in wd) of the wave files.
#' @param Annotations Vector with names (and full path to if not in wd) of the TextGrid files.
#' @param AnnotationType Type of annotation files. Suported formats are praat TextGrids (set to "TextGrid") and ESPS/Wavesurfer (set to "ESPS") files.
#' @param TierName Name of the tier in the TextGrid to be used.
#' @param Dismiss Regular expression for Outcomes that should be removed. Uses grep.
#'        E.g. "<|>" would remove <noise>,<xxx>, etc. Default is NULL.
#' @param Encoding Encoding of the annotation file. It is assumed, that all annotation files have the same encoding.
#' @param Fast Switches between a fast and a robust TextGrid parser.
#'        For Fast no "\\n" or "\\t" may be in the transcription. Default is FALSE.
#' @param Cores Number of cores that the function may use. Default is 1.
#' @param IntensitySteps Number of steps that the intensity gets compressed to. Default is 5
#' @param Smooth A parameter for using the kernel smooth function provied by the package zoo.
#' @return A data.frame with $Cues and $Outcomes for use with ndl or ndl2.
#' @examples 
#'        \dontrun{
#'        # assuming the corpus contains wave files and praat textgrids
#'            
#'          setwd(~/Data/MyCorpus) # assuming everything is in one place
#'            
#'          #assuming you have one wav for each annotation
#'            
#'          Waves=list.files(pattern="*.wav",recursive=T)
#'          Annotations=list.files(pattern="*.TextGrids",recursive=T) # see above
#'            
#'          # Lets assume the annotation is in UTF-8 and you want everything from a tier called words
#'          # Lets assume tha you want to dismiss everything in <|>
#'          # Lets assume that have 4 cores available
#'          # Lets assume that you want the defaut settings for the parameters
#'            
#'          Data=CorpusCoderCorpusCoder(Waves, Annotations, AnnotationType = "TextGrid",
#'          TierName = "words", Dismiss = "<|>", Encoding, Fast = F, Cores = 4, 
#'          IntensitySteps = 5, Smooth = 800)
#'          
#'        }
#' @import tuneR
#' @import seewave
#' @import parallel
#' @export
#' @author Denis Arnold

CorpusCoder=function(Waves,Annotations,AnnotationType=c("TextGrid","ESPS"),TierName=NULL,Dismiss=NULL,Encoding,Fast=F,Cores=1,IntensitySteps,Smooth){

  WaveHandling=function(val,IntensitySteps,Smooth){

    start=Part$start[val]
    end=Part$end[val]
    Cues=makeCues(Wave[(start*16000):(end*16000)],IntensitySteps,Smooth)
    return(Cues)
  }

  WholeData=data.frame(Outcomes=character(),
                       start=numeric(),
                       end=numeric(),
                       file=character(),
                       Cues=character(),
                       stringsAsFactors=F)

  if(length(Waves)!=length(Annotations)){
    stop("Length of lists does not match!")
  }

  for(i in 1:length(Waves)){

    if(AnnotationType=="ESPS"){
      Part=readESPSAnnotation(Annotations[i],Encoding)  
    }else{
      if(Fast){
        TG=readTextGridFast(Annotations[i],Encoding)
      }else{
        TG=readTextGridRobust(Annotations[i],Encoding)
      }
      if(!(TierName%in%TG[[1]])){
        stop(paste0("TierName ",TierName," is not prestent in TextGrid:", Annotations[i]))
      }
      Part=TG[[which(TG[[1]]==TierName)+1]]
      if(length(Part$Outcomes)<2) next
    }  
    Part$File=Waves[i]
    Part$Prev=c("<P>",Part$Outcomes[1:(length(Part$Outcomes)-1)])
    if(!is.null(Dismiss)){
      if(length(grep(Dismiss,Part$Outcomes))>0){
        Part=Part[-grep(Dismiss,Part$Outcomes),]
      }
    }
    if(length(Part$Outcomes)==0) next
    Wave=readWave(Waves[i])
    if(Wave@samp.rate!=16000){
      if(Wave@samp.rate<16000){
        warning("Sampling rate below 16 kHz!")
      }
      Wave=resamp(Wave,f=Wave@samp.rate,g=16000,output="Wave")
    }

    X=mclapply(1:dim(Part)[1],WaveHandling,IntensitySteps,Smooth,mc.cores=Cores)
    Part$Cues=unlist(X)
    WholeData=rbind(WholeData,Part)

  }

  return(WholeData)
}

