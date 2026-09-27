#' File Fetcher
#'
#' Download Protein Alignment and Accessory Files
#' @param Loci HLA Loci to be fetched. Limited Loci available.
#' @note This function is for internal BIGDAWG use only.
GetFiles <- function(Loci) {
  #downloads *_prot.txt alignment files
  #downloads hla_nom_p.txt file

  # Get P-Groups Files
  download.file("https://raw.githubusercontent.com/ANHIG/IMGTHLA/Latest/wmda/hla_nom_p.txt",destfile="hla_nom_p.txt",method="libcurl")

  # Get Release Version
  df <- readLines(con="hla_nom_p.txt", n=3)
  RD <- unlist(strsplit(df[2],split=" "))[3]
  RV <- paste(unlist(strsplit(df[3],split=" "))[3:4],collapse=" ")
  write.table(c(RD,RV),file="Release.txt",quote=F,col.names=F,row.names=F)

  # Get Locus Based Alignments
  for(i in 1:length(Loci)) {
    Locus <- Loci[i]
    FileName <- paste(Locus,"_prot.txt",sep="")
    URL <- paste("https://raw.githubusercontent.com/ANHIG/IMGTHLA/Latest/alignments/",FileName,sep="")
    download.file(URL,destfile = FileName,method="libcurl")
  }

}

#' HLA P group File Formatter
#'
#' Format the hla_nom_p.txt read table object for a specific locus.
#' @param x P group object from read.table command.
#' @param Locus Locus to be filtered on.
#' @note This function is for internal BIGDAWG use only.
PgrpFormat <- function(x,Locus) {

  # Identify for Locus ... change necessary if DRB
  x.sub <- x[which(x[,1]==Locus),]
  rownames(x.sub) <- NULL
  x.sub[,2] <- sapply(x.sub[,2],function(i) paste(paste(Locus,"*",unlist(strsplit(i,"/")),sep=""),collapse="/"))
  colnames(x.sub) <- c("Locus","Allele","P.Group")

  #Expand
  x.list <- list()
  for(i in 1:nrow(x.sub)) {
    if(grepl("/",x.sub[i,'Allele'],fixed=T)) {
      tmp <- unlist(strsplit(x.sub[i,'Allele'],"/"))
      tmp <- cbind(rep(Locus,length(tmp)),tmp,rep(x.sub[i,'P.Group'],length(tmp)))
      colnames(tmp) <- colnames(x.sub)
      x.list[[i]] <- tmp
    } else {
      x.list[[i]] <- x.sub[i,]
    }
  }
  x.list <- do.call(rbind,x.list)
  x.list <- x.list[order(x.list[,'Allele']),]
  rownames(x.list) <- NULL
  colnames(x.list) <- c("Locus","Allele","P.Group")
  return(x.list)
}

#' HLA P group Finder
#'
#' Identify P group for a given allele if exists.
#' @param x Allele of interest.
#' @param y Formatted P groups.
#' @note This function is for internal BIGDAWG use only.
PgrpExtract <- function(x,y) {
  getRow <- grep(x,y[,'Allele'],fixed=T)
  if(length(getRow)>=1) {
    if(length(getRow)>1) { getRow <- getRow[which(sapply(as.character(y[getRow,'Allele']),nchar)==nchar(x))] }
    return(as.character(y[getRow,'P.Group']))
  } else { return("") }
}

#' Protein Exon Alignment Formatter
#'
#' Dynamically creates an alignmnet of Allele exons for Analysis.
#' @param Locus Locus alignment to be formatted.
#' @param RefTab Reference exon protein information for alignment formatting.
#' @note This function is for internal BIGDAWG use only.
ExonPtnAlign.Create <- function(Locus,RefTab) {

  #########################################################################
  # Need to remove if DRB split into single locus files
  if(grepl("DRB",Locus)) { Locus.get <- "DRB" } else { Locus.get <- Locus }
  #########################################################################

  AlignMatrix <- NULL; rm(AlignMatrix)

  #Read in P-Groups
  Pgrps <- read.table("hla_nom_p.txt",fill=T,header=F,sep=";",stringsAsFactors=F,strip.white=T,colClasses="character")
  Pgrps[,1] <- gsub("\\*","",Pgrps[,1])
  Pgrps <- PgrpFormat(Pgrps,Locus)

  #Read in Alignment
  Name <- paste0(Locus.get,"_prot.txt")
  Align <- read.table(Name,fill=T,header=F,sep="\t",stringsAsFactors=F,strip.white=T,colClasses="character")

  #Trim
  Align <- as.matrix(Align[-nrow(Align),]) #Remove Footer

  #Begin Formatting
  Align <- as.matrix(Align[-grep("\\|",Align[,1]),1]) #Remove Pipes
  Align[,1] <- sapply(Align[,1],FUN=sub,pattern=" ",replacement="~")
  Align[,1] <- sapply(Align[,1],FUN=gsub,pattern=" ",replacement="")
  Align <- strsplit(Align[,1],"~")
  Align <- as.matrix(do.call(rbind,Align))

  #Adjust rows to blank where Sequence column == Allele Name
  Align[which(Align[,1]==Align[,2]),2] <- ""

  # Remove Prot Numbering Headers
  Align <- Align[-which(Align[,1]=="Prot"),]

  # Get Unique Alleles
  Alleles <- unique(Align[,1])

  # Loop Through and Build Alignment Block
  Block <- list()
  for(i in Alleles) {
    getRows <- which(Align[,1]==i)
    Block[[i]] <- paste(Align[getRows,2],collapse="")
  }
  Block <- cbind(Alleles,do.call(rbind,Block))

  #Fill end gaps with * to make char lengths even
  Block.len <- max(as.numeric(sapply(Block[,2],FUN=nchar)))
  for( i in 1:nrow(Block) ) {
    Block.miss <- Block.len - nchar(Block[i,2])
    if( Block.miss > 0 ) {
      Block[i,2] <- paste0(as.character(Block[i,2]), paste(rep(".",Block.miss),collapse=""))
    }
  }; rm(i)

  #Split Allele name into separate Locus and Allele, Send Back to Align object
  AlignAlleles <- do.call(rbind,strsplit(Block[,1],"[*]"))
  AlignAlleles <- cbind(AlignAlleles,apply(AlignAlleles,MARGIN=c(1,2),FUN=GetField,Res=2)[,2])
  rownames(AlignAlleles) <- NULL

  Align <- cbind(AlignAlleles,Block)
  colnames(Align) <- c("Locus","Allele","Trimmed","FullName","Sequence")

  #Split Sub Alignment into composite elements and Extract relevant positions
  Align.split <- strsplit(Align[,'Sequence'],"")
  Align.split <- do.call(rbind,Align.split)
  AlignMatrix <- cbind(Align[,1:4],Align.split)
  rownames(AlignMatrix) <- NULL

  #Ensure Reference in Row 1
  RefAllele <- paste(RefTab[which(RefTab[,'Locus']==Locus),'Reference.Locus'],
                     RefTab[which(RefTab[,'Locus']==Locus),'Reference.Allele'],
                     sep="*")
  if( !AlignMatrix[1,'FullName']==RefAllele ) {

    Align.tmp <- rbind(AlignMatrix[which(AlignMatrix[1,'Allele']==RefAllele),],
                       AlignMatrix[-which(AlignMatrix[1,'Allele']==RefAllele),])
    AlignMatrix <- Align.tmp
    rm(Align.tmp)

  }

  #Save Reference Row
  RefSeq <- AlignMatrix[1,]

  #Ensure Locus Specific Rows
  AlignMatrix <- AlignMatrix[which(AlignMatrix[,'Locus']==Locus),]

  #Rebind Reference if removed (only for DRB3, DRB4, and DRB5)
  if( !AlignMatrix[1,'FullName']==RefAllele ) { AlignMatrix <- rbind(RefSeq,AlignMatrix) }

  #Remove columns with no amino acids positions (only for DRB3, DRB4, and DRB5)
  #Count occurence of "." and compare to nrow of AlignMatrix
  rmCol <- which( apply(AlignMatrix,MARGIN=2, FUN=function(x) length(which(x=="."))) == nrow(AlignMatrix) )
  if( length(rmCol)>0 ) { AlignMatrix <- AlignMatrix[,-rmCol] }

  #Propagate Consensus Positions
  for( i in 5:ncol(AlignMatrix) ) {
    x <- AlignMatrix[,i]
    x[which(x=="-")] <- x[1]
    AlignMatrix[,i] <- x
  }

  #Rename amino acid positions based on reference numbering
  #Deletions are named according to the preceding position with a .1,.2,etc.
  RefStart <- as.numeric(RefTab[which(RefTab[,'Locus']==Locus),'Reference.Start'])
  RefArray <- AlignMatrix[1,5:ncol(AlignMatrix)]
  Names <- NULL ; RefPos <- RefStart
  for(i in 1:length(RefArray) ) {

    if(RefArray[i]==".") {
      Names <- c(Names, paste0("Pos.",RefPos-1,".",Iteration) )
      Iteration = Iteration + 1
    } else {
      Iteration=1
      Names <- c(Names, paste0("Pos.",RefPos))
      RefPos <- RefPos + 1
      if (RefPos==0) { RefPos <- 1 }
    }

  }
  colnames(AlignMatrix)[5:ncol(AlignMatrix)] <- Names
  rownames(AlignMatrix) <- NULL

  #Add Absent Allele (Absence due to lack of allele and not lack of typing information)
  AlignMatrix <- rbind(c(Locus,"00:00:00:00","00:00",paste(Locus,"*00:00:00:00",sep=""),rep("^",ncol(AlignMatrix)-4)),
                       AlignMatrix)

  #Assign P groups
  AlignMatrix <- cbind(AlignMatrix, sapply(AlignMatrix[,'FullName'],PgrpExtract,y=Pgrps) )
  colnames(AlignMatrix)[ncol(AlignMatrix)] <- "P.group"

  #Tally Unknowns as separate column
  AlignMatrix <- cbind(AlignMatrix,
                       apply(AlignMatrix[,5:(ncol(AlignMatrix)-1)],MARGIN=1,FUN=function(x) length(which(unlist(grep("*",x,fixed=T))>0))) )
  colnames(AlignMatrix)[ncol(AlignMatrix)] <- "Unknowns"

  #Tally Null Positions as separate column
  AlignMatrix <- cbind(AlignMatrix,
                       apply(AlignMatrix[,5:(ncol(AlignMatrix)-2)],MARGIN=1,FUN=function(x) length(which(unlist(grep("-",x,fixed=T))>0))) )
  colnames(AlignMatrix)[ncol(AlignMatrix)] <- "NullPositions"

  #Tally InDels
  AlignMatrix <- cbind(AlignMatrix,
                       apply(AlignMatrix[,5:(ncol(AlignMatrix)-3)],MARGIN=1,FUN=function(x) length(which(unlist(grep(".",x,fixed=T))>0))) )
  colnames(AlignMatrix)[ncol(AlignMatrix)] <- "InDels"

  rownames(AlignMatrix) <- NULL
  FileName <- paste("ExonPtnAlign_",Locus,".obj",sep="")

  save(AlignMatrix,file=FileName)

}

#' Alignment Object Creator
#'
#' Create Object for Exon Protein Alignments.
#' @param Loci Loci to be bundled.
#' @param Release IMGT/HLA database release version.
#' @param RefTab Data of reference exons used for protein alignment creation.
#' @note This function is for internal BIGDAWG use only.
AlignObj.Create <- function(Loci,Release,RefTab) {

  AlignMatrix <- NULL; rm(AlignMatrix)

  ExonPtnList <- list()
  for(i in 1:length(Loci)) {
    Locus <- Loci[i]
    FileName <- paste("ExonPtnAlign_",Locus,".obj",sep="")
    load(FileName) #Loads AlignMatrix
    ExonPtnList[[Locus]] <- AlignMatrix
  }

  ExonPtnList[['Release.Version']] <- as.character(Release[2,])
  ExonPtnList[['Release.Date']] <- as.character(Release[1,])
  ExonPtnList[['RefExons']] <- RefTab
  save(ExonPtnList,file="ExonPtnAlign.obj")

}

#' Updated Alignment Object Creator
#'
#' Synthesize Object for Exon Protein Alignments.
#' @param Loci Loci to be bundled.
#' @param Release IMGT/HLA database release version.
#' @param RefTab Data of reference exons used for protein alignment creation.
#' @note This function is for internal BIGDAWG use only.
AlignObj.Update <- function(Loci,Release,RefTab) {

  AlignMatrix <- NULL; rm(AlignMatrix)

  UpdatePtnList <- list()
  for(i in 1:length(Loci)) {
    Locus <- Loci[i]
    FileName <- paste("ExonPtnAlign_",Locus,".obj",sep="")
    load(FileName) #Loads AlignMatrix
    UpdatePtnList[[Locus]] <- AlignMatrix
  }
  UpdatePtnList[['Release.Version']] <- as.character(Release[2,])
  UpdatePtnList[['Release.Date']] <- as.character(Release[1,])
  UpdatePtnList[['RefExons']] <- RefTab
  save(UpdatePtnList,file="UpdatePtnAlign.RData")

}
