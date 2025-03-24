#' Reads a TextGrid made with praat and returns a list with a vector of all tier names and a data.frame for each tier.
#' @param File Name (with full path, if not in wd) of the TextGrid
#' @param Encoding Encoding of the TextGrid. Typically encodings are "ACSII","UTF-8" or "UTF-16"
#' @details This method has sometimes problems with certain sequences like "\\n" in the annotation file. 
#' If the method fails, try readTextGridRobust()
#' @return A list containing a vectors with the names and data.frames for each tier in the TextGrid.
#' @examples 
#'        \dontrun{
#'        # Assume that NameOfTextGrid is encoded in "UTF-8"
#'        Data=readTextGridFast("NameOfTextGrid","UTF-8")
#'
#'        }
#' @export
#' @author Denis Arnold

readTextGridFast<-function(File,Encoding){
  
  File=file(File,encoding=Encoding)
  Data=readLines(File,-1)
  close(File)
  
  names=gsub("^[[:space:]]+name\\ =\\ |\"|[\\ ]+","",Data[grep("name\ =",Data)])
  numberOfTiers=length(names)
  TierBorder=c(grep("IntervalTier|TextTier",Data),length(Data))
  TierType=gsub("[[:space:]]+|class|\\=|\"|-","",Data[grep("IntervalTier|TextTier",Data)])
  
  for(i in 1:numberOfTiers){
    if(TierType[i]=="IntervalTier"){
      Part=Data[TierBorder[i]:TierBorder[i+1]]
      Part=Part[-(1:5)]
      Part=gsub("^[[:space:]]+((text)|(xmin)|(xmax))\\ =\\ |\"|[\\ ]+$","",Part)
      Part=gsub("[\\ ]+$","",Part)
      if(length(grep("class\ =\ (IntervalTier)|(TextTier)",Part))>0){
        Part=Part[-grep("class\ =\ (IntervalTier)|(TextTier)",Part)]
      }      
      if(length(grep("item\ \\[[0-9]+\\]",Part))>0){
        Part=Part[-grep("item\ \\[[0-9]+\\]",Part)]
      }
      PartDataFrame=data.frame(Outcomes=Part[seq(4,length(Part),4)],
                               start=as.numeric(Part[seq(2,length(Part),4)]),
                               end=as.numeric(Part[seq(3,length(Part),4)]),
                               stringsAsFactors=F)
    }
    else{
      Part=Data[TierBorder[i]:TierBorder[i+1]]
      Part=Part[-(1:5)]
      Part=gsub("^[[:space:]]+((mark)|(number))\\ =\\ |\"|[\\ ]+$","",Part)
      Part=gsub("[\\ ]+$","",Part)
      if(length(grep("class\ =\ (IntervalTier)|(TextTier)",Part))>0){
        Part=Part[-grep("class\ =\ (IntervalTier)|(TextTier)",Part)]
      }
      if(length(grep("item\ \\[[0-9]+\\]",Part))>0){
        Part=Part[-grep("item\ \\[[0-9]+\\]",Part)]
      }
      PartDataFrame=data.frame(	Outcomes=Part[seq(3,length(Part),3)],
                                point=as.numeric(Part[seq(2,length(Part),3)]),
                                stringsAsFactors=F)
    }
    assign(names[i],PartDataFrame)
  }
  
  
  NewData=vector("list",length(names)+1)
  NewData[[1]]=names
  
  for(i in 2:length(NewData)){
    NewData[[i]]=get(names[i-1])
  }
  
  return(NewData)
  
}

#' Reads a TextGrid made with praat and returns a list with a vector of all tier names and a data.frame for each tier
#' @param File Name (with full path, if not in wd) of the TextGrid
#' @param Encoding Encoding of the TextGrid. Typically encodings are "ACSII","UTF-8" or "UTF-16"
#' @importFrom utils read.csv
#' @return A list containing a vectors with the names and data.frames for each tier in the TextGrid.
#' @examples 
#'        \dontrun{
#'        # Assume that NameOfTextGrid is encoded in "UTF-8"
#'        Data=readTextGridRobust("NameOfTextGrid","UTF-8")
#'
#'        }
#' @export
#' @author Denis Arnold

readTextGridRobust<-function(File,Encoding){
  
  Data=read.csv(file(File,encoding=Encoding),stringsAsFactors=F,header=F)$V1
  names=gsub("^[[:space:]]+name\\ =\\ |\"|[\\ ]+","",Data[grep("name\ =",Data)])
  numberOfTiers=length(names)
  TierBorder=c(grep("IntervalTier|TextTier",Data),length(Data)+1)
  TierType=gsub("[[:space:]]+|class|\\=|\"|-","",Data[grep("IntervalTier|TextTier",Data)])
  
  for(i in 1:numberOfTiers){
    if(TierType[i]=="IntervalTier"){
      Part=Data[TierBorder[i]:(TierBorder[i+1]-1)]
      Part=Part[-(1:5)]
      Part=gsub("^[[:space:]]+((text)|(xmin)|(xmax))\\ =\\ |\"|[\\ ]+$","",Part)
      Part=gsub("[\\ ]+$","",Part)
      PartDataFrame=data.frame(Outcomes=character(),start=numeric(),end=numeric(),stringsAsFactors=F)
      for(j in 1:(length(Part)/4)){
        PartDataFrame=rbind(PartDataFrame,
                            data.frame(Outcomes=Part[(j*4)],
                                       start=as.numeric(Part[(j*4)-2]),
                                       end=as.numeric(Part[(j*4)-1]),
                                       stringsAsFactors=F),
                            stringsAsFactors=F)
      }
      assign(names[i],PartDataFrame)
    }
    else{
      Part=Data[TierBorder[i]:(TierBorder[i+1]-1)]
      Part=Part[-(1:5)]
      Part=gsub("^[[:space:]]+((mark)|(number))\\ =\\ |\"|[\\ ]+$","",Part)
      Part=gsub("[\\ ]+$","",Part)
      PartDataFrame=data.frame(Outcomes="",point=0,stringsAsFactors=F)
      for(j in 1:(length(Part)/3)){
        PartDataFrame=rbind(PartDataFrame,
                            data.frame(Outcomes=Part[(j*3)],
                                       point=as.numeric(Part[(j*3)-1]),stringsAsFactors=F),
                            stringsAsFactors=F)
      }
      assign(names[i],PartDataFrame)
      
    }
  }
  NewData=vector("list",length(names)+1)
  NewData[[1]]=names
  
  for(i in 2:length(NewData)){
    NewData[[i]]=get(names[i-1])
  }
  
  return(NewData)
  
}

#' Reads a ESPS/Old Wavesurfer style annotation file and returns a data.frame with times and lables
#' @param File Name (with full path, if not in wd) of the annotation file
#' @param Encoding Encoding of the annotation file. Typically encodings are "ACSII","UTF-8" or "UTF-16"
#' @return A data.frame with $Output for the lable $start and $end time of the lable.
#' @examples 
#'        \dontrun{
#'        # Assume that NameOfAnnotation is encoded in "UTF-8"
#'        Data=readESPSAnnotation("NameOfTextGrid","UTF-8")
#'        }
#' @export
#' @author Denis Arnold

readESPSAnnotation<-function(File,Encoding){
  File=file(File,encoding=Encoding)
  Data=readLines(File,-1)
  close(File)
  
  r=grep("^#",Data)
  if(r>0){
    Data=Data[-(1:r)]
  }
  Data=unlist(strsplit(Data,"\ [0-9]+\ "))
  End=as.numeric(Data[seq(1,length(Data),2)])
  DataFrame=data.frame( Outcomes=Data[seq(2,length(Data),2)],
                        start=as.numeric(c(0,End[-length(End)])),
                        end=as.numeric(End),
                        stringsAsFactors=F)
  
  return(DataFrame)
} 


#' Reads a New Wavesurfer style annotation file and returns a data.frame with times and lables
#' @param File Name (with full path, if not in wd) of the annotation file
#' @param Encoding Encoding of the annotation file. Typically encodings are "ACSII","UTF-8" or "UTF-16"
#' @return A data.frame with $Output for the lable $start and $end time of the lable.
#' @examples 
#'        \dontrun{
#'        # Assume that NameOfAnnotation is encoded in "UTF-8"
#'        Data=readWavesurfer("NameOfTextGrid","UTF-8")
#'        }
#' @export
#' @author Denis Arnold

readWavesurfer<-function(File,Encoding){
  File=file(File,encoding=Encoding)
  Data=readLines(File,-1)
  close(File)
  Data=gsub("^[\ ]+","",Data)
  Data=unlist(strsplit(Data,"\ "))
  End=as.numeric(Data[seq(1,length(Data),3)])
  DataFrame=data.frame( Outcomes=Data[seq(3,length(Data),3)],
                        start=as.numeric(c(0,End[-length(End)])),
                        end=as.numeric(End),
                        stringsAsFactors=F)
  return(DataFrame)
}
