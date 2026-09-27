#' Check Input Parameters
#'
#' Check input parameters for invalid entries.
#' @param HLA Logical indicating whether data is HLA class I/II genotyping data only.
#' @param Loci.Set Input list defining which loci to use for analyses (combinations permitted).
#' @param Exon Numeric Exon(s) for targeted amino acid analysis.
#' @param All.Pairwise Logical indicating whether all pairwise loci should be analyzed in haplotype analysis.
#' @param Trim Logical indicating if HLA alleles should be trimmed to a set resolution.
#' @param Res Numeric setting what desired resolution to trim HLA alleles.
#' @param EVS.rm Logical indicating if expression variant suffixes should be removed.
#' @param Missing Numeric setting allowable missing data for running analysis (may use "ignore").
#' @param Cores.Lim Integer setting the number of cores accessible to BIGDAWG (Windows limit is 1 core).
#' @param Return Logical Should analysis results be returned as list.
#' @param Output Logical Should analysis results be written to output directory.
#' @param Merge.Output Logical Should analysis results be merged into a single file for easy access.
#' @param Verbose Logical Should a summary of each analysis be displayed in console.
#' @note This function is for internal use only.
Check.Params <- function (HLA,Loci.Set,Exon,All.Pairwise,Trim,Res,EVS.rm,Missing,Cores.Lim,Return,Output,Merge.Output,Verbose) {

  # Logicals: HLA=TRUE, All.Pairwise=FALSE, EVS.rm=FALSE, Trim=FALSE, Return=FALSE, Merge.FALSE, Verbose=TRUE, TRUE,
  # Numerics: Res=2, Missing=2, Cores.Lim=1L
  # Untested: Data, Results.Dir, Run.Tests, Loci.Set

  if( is.na(as.logical(HLA)) ) { Err.Log(FALSE,"P.Error","HLA") ; stop("Analysis Stopped.",call.=FALSE) }
  if( !missing(Loci.Set) && !is.list(Loci.Set) ) { Err.Log(FALSE,"P.Error","Loci.Set") ; stop("Analysis Stopped.",call.=FALSE) }
  if( !missing(Exon) && !is.numeric(Exon) ) { Err.Log(FALSE,"P.Error","Exon") ; stop("Analysis Stopped.",call.=FALSE) }
  if( !is.logical(All.Pairwise) ) { Err.Log(FALSE,"P.Error","All.Pairwise") ; stop("Analysis Stopped.",call.=FALSE) }
  if( !is.logical(EVS.rm) ) { Err.Log(FALSE,"P.Error","EVS.rm") ; stop("Analysis Stopped.",call.=FALSE) }
  if( !is.logical(Trim) ) { Err.Log(FALSE,"P.Error","Trim") ; stop("Analysis Stopped.",call.=FALSE) }
  if( !is.logical(Return) ) { Err.Log(FALSE,"P.Error","Return") ; stop("Analysis Stopped.",call.=FALSE) }
  if( !is.logical(Merge.Output) ) { Err.Log(FALSE,"P.Error","Merge.Output") ; stop("Analysis Stopped.",call.=FALSE) }
  if( !is.logical(Verbose) ) { Err.Log(FALSE,"P.Error","Verbose") ; stop("Analysis Stopped.",call.=FALSE) }
  if( !is.logical(Output) ) { Err.Log(FALSE,"P.Error","Output") ; stop("Analysis Stopped.",call.=FALSE) }
  if( !is.numeric(Res) ) { Err.Log(FALSE,"P.Error","Res") ; stop("Analysis Stopped.",call.=FALSE) }
  if( !is.numeric(Cores.Lim) && !is.integer(Cores.Lim) ) { Err.Log(FALSE,"P.Error","Cores.Lim") ; stop("Analysis Stopped.",call.=FALSE) }
  if( !is.numeric(Missing) ) { if(Missing!="ignore") { Err.Log(FALSE,"P.Error","Missing") ; stop("Analysis Stopped.",call.=FALSE) } }

}

#' Check Input Parameters for GLS conversion
#'
#' Check input parameters for invalid entries.
#' @param Convert String Direction for conversion.
#' @param File.Output String Type of output.
#' @param System String Genetic system (HLA or KIR) of the data being converted
#' @param HZY.Red Logical Reduction of homozygote genotypes to single allele.
#' @param DRB345.Check Logical Check DR haplotypes for consistency and flag unusual haplotypes.
#' @param Cores.Lim Integer How many cores can be used.
#' @note This function is for internal use only.
Check.Params.GLS <- function (Convert,File.Output,System,HZY.Red,DRB345.Check,Cores.Lim) {

  if( is.na(match(Convert,c("GL2Tab","Tab2GL"))) ) { Err.Log(FALSE,"P.Error","Convert") ; stop("Analysis Stopped.",call.=FALSE) }
  if( is.na(match(File.Output,c("R","txt","csv","pypop"))) ) { Err.Log(FALSE,"P.Error","File.Output") ; stop("Analysis Stopped.",call.=FALSE) }
  if( is.na(match(System,c("HLA","KIR"))) ) { Err.Log(FALSE,"P.Error","System") ; stop("Analysis Stopped.",call.=FALSE) }
  if( !is.logical(HZY.Red) ) { Err.Log(FALSE,"P.Error","HZY.Red") ; stop("Analysis Stopped.",call.=FALSE) }
  if( !is.logical(DRB345.Check) ) { Err.Log(FALSE,"P.Error","DRB345.Check") ; stop("Analysis Stopped.",call.=FALSE) }
  if( !is.numeric(Cores.Lim) || !is.integer(Cores.Lim) ) { Err.Log(FALSE,"P.Error","Cores.Lim") ; stop("Analysis Stopped.",call.=FALSE) }

}

#' Check Cores Parameters
#'
#' Check cores limitation for OS compatibility
#' @param Cores.Lim Integer How many cores can be used.
#' @param Output Logical Should analysis results be written to output directory.
Check.Cores <- function(Cores.Lim,Output) {
  if ( Cores.Lim!=1L ) {
    Cores.Max <- as.integer( floor( parallel::detectCores() * 0.9) )
    if(Sys.info()['sysname']=="Windows" && as.numeric(Cores.Lim)>1) {
      Err.Log(Output,"Windows.Cores") ; stop("Analysis Stopped.",call. = F)
    } else if( Cores.Lim > Cores.Max ) { Cores <- Cores.Max
    } else { Cores <- Cores.Lim }
  } else { Cores <- Cores.Lim }
  return(Cores)
}

#' HLA Formatting Check for Amino Acid Analysis
#'
#' Checks data to see if HLA data is properly formatted .
#' @param x All columns of HLA genotyping data.
#' @note This function is for internal BIGDAWG use only.
CheckHLA <- function(x) {
  #Return TRUE if properly formatted HLA

  # temporary reassignment for test
  x[is.na(x)] <- "00:00" # NA cells
  x[x=="^"] <- "00:00" # absent cells
  x[x==""] <- "00:00" # empty cells

  # test for colon delimiters
  test <- apply(x,MARGIN=c(1,2),FUN=function(z) length(unlist(strsplit(as.character(z),split=":"))))
  test <- apply(test,MARGIN=2,FUN=min)
  Flag <- as.logical(min(test)==2)

  return(Flag)
}

#' HLA Loci Legitimacy Check for Amino Acid Analysis
#'
#' Checks available loci against data to ensure complete overlap.
#' @param x Loci available in exon protein list alignment object.
#' @param y Unique column names
#' @note This function is for internal BIGDAWG use only.
CheckLoci <- function(x,y) {
  #Returns TRUE if absent locus(loci) encountered
  #x=Loci available in ExonPtnList
  #y=Loci.Set from data

  Output <- list()

  y <- unique(unlist(y))
  y <- gsub("HLA-","",y)

  Flag <- ( !sum(y %in% x) == length(y) )
  Output[['Flag']] <- Flag
  if(Flag) {
    Output[['Loci']] <- paste(y[!y %in% x],collapse=",")
  } else {
    Output[['Loci']] <- NA
  }

  return(Output)
}

#' HLA Allele Legitimacy Check for Amino Acid Analysis
#'
#' Checks available alleles against data to ensure complete overlap.
#' @param x Exon protein list alignment object.
#' @param y Genotypes from data file
#' @note This function is for internal BIGDAWG use only.
CheckAlleles <- function(x,y) {

  # Returns TRUE if unknown allele(s) encountered
  # Checks at 2 levels of resolution: Full, 3-Field, 2-Field, 1-Field

  # Define Loci
  Loci <- unique( gsub("\\.1|\\.2|\\_1|\\_2","",colnames(y)) )

  Output <- list()
  for(i in Loci ) {

    # Database Alleles
    x.locus <- x[[i]][,'Allele']
    x.locus[] <- sapply(x.locus, FUN = gsub, pattern="[[:alpha:]]", replacement="")

    # Current Data Alleles
    y.locus <- y[,grep(i,colnames(y))]
    y.locus <- unique(c(y.locus[,1],y.locus[,2]))
    y.locus[] <- sapply(y.locus, FUN = gsub, pattern="[[:alpha:]]", replacement="")
    y.locus <- na.omit(y.locus)
    y.locus <- y.locus[y.locus!="^"]
    y.locus <- y.locus[y.locus!=""]


    # Check Each Allele against Database at defined resolution
    Resolution <- c("Full",3,2,1) ; r = 1

    repeat{

      Res <- Resolution[r]
      #cat(r,":",Res,"\n")

      if( Res=="Full" ) {
        y.locus.sub <- y.locus ; x.locus.sub <- x.locus
      } else {
        y.locus.sub <- sapply(y.locus,GetField,Res=Res)
        x.locus.sub <- sapply(x.locus,GetField,Res=Res)
      }

      A.check <- y.locus.sub %in% x.locus.sub

      if( sum(A.check)==length(y.locus.sub) ) { A.Flag <- FALSE ; break } else { r <- r + 1 ; A.Flag <- TRUE }
      if( r > length(Resolution) ) { break }

    }

    if(A.Flag) { Alleles <- y.locus[!A.check] ; Alleles <- paste(Alleles,collapse=",") }

    Output[[i]] <- list( Flag = ifelse(A.Flag,TRUE,FALSE),
                         Alleles = ifelse(A.Flag,Alleles,"") )

  }

  Flags <- unlist(lapply(Output,"[","Flag")) ; Alleles <-lapply(Output,"[","Alleles")
  if( sum(Flags)>0 ) {

    getFlags <- which(Flags==TRUE)
    Alleles.Flagged <- sapply(getFlags,FUN= function(z) paste(Loci[z], unlist(Alleles[[z]]) , sep="*" )  )

    Out <- list( Flag =  TRUE,
                 Alleles =  Alleles.Flagged )
  } else {
    Out <- list( Flag=FALSE ,
                 Alleles="" )
  }

  return(Out)

}

#' Data Summary Function
#'
#' Summary function for sample population within data file.
#' @param Tab Loci available in exon protein list alignment object.
#' @param All.ColNames Column names from genotype data.
#' @param rescall HLA resolution set for analysis.
#' @param HLA HLA BIGDAWG argument passed to function
#' @param Verbose Summary display carryover from BIGDAWG function.
#' @param Output Data output carryover form BIGDAWG function
#' @note This function is for internal BIGDAWG use only.
PreCheck <- function(Tab,All.ColNames,rescall,HLA,Verbose,Output) {

  Grp0 <- which(Tab[,2]==0)
  Grp1 <- which(Tab[,2]==1)
  nGrp0 <- length(Tab[Grp0,2])
  nGrp1 <- length(Tab[Grp1,2])

  if(min(nGrp0,nGrp1)==0) {
    Err.Log(Output,"Case.Con")
    stop("Analysis Stopped.",call. = F)
  }

  Loci <- as.list(unique(All.ColNames[3:length(All.ColNames)]))
  nLoci <- length(Loci)
  GTYPE <- Tab[,3:ncol(Tab)]
  colnames(GTYPE) <- All.ColNames[3:length(All.ColNames)]
  nGTYPE <- unlist(lapply(Loci,function(x) length(unique(unlist(GTYPE[,which(colnames(GTYPE)==x)])))))
  Grp0un <- unlist(lapply(Loci,function(x) length(unique(unlist(GTYPE[Grp0,which(colnames(GTYPE)==x)])))))
  Grp1un <- unlist(lapply(Loci,function(x) length(unique(unlist(GTYPE[Grp1,which(colnames(GTYPE)==x)])))))

  nMissing <- unlist(lapply(Loci,function(x) sum(is.na(GTYPE[,which(colnames(GTYPE)==x)]))))
  Grp0miss <- unlist(lapply(Loci,function(x) sum(is.na(GTYPE[Grp0,which(colnames(GTYPE)==x)]))))
  Grp1miss <- unlist(lapply(Loci,function(x) sum(is.na(GTYPE[Grp1,which(colnames(GTYPE)==x)]))))

  if(Verbose) {
    cat("  Sample Summary\n")
    cat("    Sample Size (n):",nrow(Tab),"\n")
    cat("    ...Number of Controls/Cases:",paste(paste(nGrp0,nGrp1,sep="/"),collapse=", "),"\n")
    cat("    Allele Count (2n):",nrow(Tab)*2,"\n")
    cat("    Total loci in file:",nLoci,"\n")
    cat("    Unique loci:",paste(Loci,collapse=", "),"\n")
    cat("    Unique alleles per locus:",paste(nGTYPE,collapse=", "),"\n")
    cat("    ...Unique in Controls/Cases:",paste(paste(Grp0un,Grp1un,sep="/"),collapse=", "),"\n")
    cat("    Missing alleles per locus:",paste(nMissing,collapse=", "),"\n")
    cat("    ...Missing in Controls/Cases:",paste(paste(Grp0miss,Grp1miss,sep="/"),collapse=", "),"\n")
    cat("\n")
  }

  if(HLA) {

    Grp0res <- max(unlist(lapply(Loci,function(x) max(unlist(lapply(strsplit(unlist(GTYPE[Grp0,which(colnames(GTYPE)==x)]),split=":"),length))))))
    Grp1res <- max(unlist(lapply(Loci,function(x) max(unlist(lapply(strsplit(unlist(GTYPE[Grp1,which(colnames(GTYPE)==x)]),split=":"),length))))))

    if(max(Grp0res,Grp1res)>4) {
      Err.Log(Output,"High.Res")
      stop("Analysis Stopped.",call. = F)
    }

    if(Verbose){
      cat("  Observed Allele Resolution\n")
      cat("    Max Resolution Controls:",paste(Grp0res,"-Field",sep=""),"\n")
      cat("    Max Resolution Cases:",paste(Grp1res,"-Field",sep=""),"\n")
      cat("    Defined Resolution:",rescall,"\n")
      if(Grp0res!=Grp1res){ cat("  ***** Warning. \n") }
      if(Grp0res!=Grp1res){ cat("  ***** There may exist a Case-Control field resolution imbalance.\n") }
      if(Grp0res!=Grp1res){ cat("  ***** Considering trimming to",paste(min(Grp0res,Grp1res),"-Field resolution.",sep=""),"\n") }
      cat("\n")
    }
  }

  if(HLA) {
    Out <- list(Sample.Size=nrow(Tab),
                   No.Controls=nGrp0,
                   No.Cases=nGrp1,
                   Allele.Count=nrow(Tab)*2,
                   Total.Loci=nLoci,
                   Loci=paste(Loci,collapse=", "),
                   AllelePerLocus=paste(nGTYPE,collapse=", "),
                   MissingPerLocus=paste(nMissing,collapse=", "),
                   MaxResGrp0=paste(Grp0res,"-Field",sep=""),
                   MaxResGrp1=paste(Grp1res,"-Field",sep=""),
                   Suggested.Res=paste(min(Grp0res,Grp1res),"-Field",sep=""),
                   SetRes=rescall)
  } else {
    Out <- list(Sample.Size=nrow(Tab),
                   No.Controls=nGrp0,
                   No.Cases=nGrp1,
                   Allele.Count=nrow(Tab)*2,
                   Total.Loci=nLoci,
                   Loci=paste(Loci,collapse=", "),
                   AllelePerLocus=paste(nGTYPE,collapse=", "),
                   MissingPerLocus=paste(nMissing,collapse=", "),
                   SetRes=rescall)
  }

  return(do.call(rbind,Out))

}

#' Check Data Structure
#'
#' Check data structure for successful conversion.
#' @param Data String Type of output.
#' @param System Character Genetic system HLA or KIR
#' @param Convert String Direction for conversion.
#' @note This function is for internal use only.
Check.Data <- function (Data,System,Convert) {

  if(Convert=="Tab2GL") {

    # Check for column formatting consistency
    if( ncol(Data) < 3 ) { Err.Log(FALSE,"Table.Col") ; stop("Analysis Stopped.",call.=F) }

    # Check for GL string field delimiters Presence
    if ( sum(grepl("\\+",Data[,ncol(Data)])) > 0 || sum(grepl("\\^",Data[,ncol(Data)])) > 0 || sum(grepl("\\|",Data[,ncol(Data)])) > 0 ) {
      Err.Log(FALSE,"Tab.Format") ; stop("Analysis Stopped.",call.=F)
    }

    # Check for repeating column names
    colnames(Data) <- sapply(colnames(Data),FUN=gsub,pattern="\\.1|\\.2|\\_1|\\_2",replacement="")
    DataCol <- which(table(colnames(Data))==2)
    if( length(DataCol)==0 ) { Err.Log(FALSE,"Table.Pairs") ; stop("Analysis Stopped.",call.=F) }

  }

  if(Convert=="GL2Tab") {

    LastCol <- ncol(Data)

    #Check for System Name in GL String
    test <- na.omit(Data[,LastCol])
    test <- test[-which(sapply(test,nchar)==0)]
    if(length(grep(System,test))!=length(test)) {
      Err.Log(FALSE,"GL.Format") ; stop("Analysis Stopped.",call.=F)
    }


    # Check for GL string field delimiters Absence
    if ( sum(grepl("\\+",Data[,LastCol])) == 0 && sum(grepl("\\^",Data[,LastCol])) == 0 && sum(grepl("\\|",Data[,LastCol])) == 0 ) {
      Err.Log(FALSE,"GL.Format") ; stop("Analysis Stopped.",call.=F)
    }

    # Check for ambiguous data at genotype "|"
    if( sum(grepl("\\|",Data[,LastCol]))>0 ) {
      Check.Rows <- paste(grep("\\|",Data[,LastCol]),collapse=",")
      Err.Log(FALSE,"GTYPE.Amb",Check.Rows) ; stop("Analysis Stopped.",call.=F) }
  }

}

#' GL String Locus Check
#'
#' Check GL string for loci appearing in multiple gene fields.
#' @param x GL String to check against
#' @param Loci Loci to check
#' @note This function is for internal use only.
CheckString.Locus <- function(x,Loci) {

  Calls <- sapply(x,FUN=function(x) strsplit(x,"\\+"))
  Calls.loci <- lapply(Calls,FUN=function(x) unlist(lapply(strsplit(x,"\\*"),"[",1)))

  Calls.loci.1 <- unlist(lapply(Calls.loci,"[",1))
  Calls.loci.1 <- colSums(sapply(Loci, FUN = function(z) Calls.loci.1 %in% z))

  Calls.loci.2 <- as.character(unlist(lapply(Calls.loci,"[",2)))
  Calls.loci.2 <- colSums(sapply(Loci, FUN = function(z) Calls.loci.2 %in% z))

  test.CS <- colSums(rbind(Calls.loci.1,Calls.loci.2))
  if( max(test.CS)>2 ) {

    Loci.Err <- paste(Loci[which(test.CS>2)],collapse=",")
    GLS <- paste(x,collapse="^")
    Err.Log(FALSE,"Locus.MultiField",GLS,Loci.Err)
    stop("Analysis Stopped.",call.=FALSE)

  }

  return("ok")

}

#' GL String Allele Check
#'
#' GL String check for allele ambiguity formatting
#' @param x  GL String to check against
#' @note This function is for internal use only.
CheckString.Allele <- function(x) {

  x <- as.character(x)

  if(grepl("/",x)) {
    tmp <- strsplit(unlist(strsplit(x,"/")),"\\*")
    tmp.len <- length(unique(lapply(tmp,length)))
    if( tmp.len > 1 ) {
      Err.Log(FALSE,"Allele.Amb.Format",x)
      stop("Analysis Stopped.",call.=FALSE)
    }
  }

  return("ok")

}

#' Function to Check Release Versions
#'
#' This updates the protein aligment used in checking HLA loci and alleles as well as in the amino acid analysis.
#' @param Package Logical to check for BIGDAWG package versions
#' @param Alignment Logical to check the IMGT/HLA database version for the alignment bundled with BIGDAWG.
#' @param Output Should any error be written to a file
#' @note Requires active internet connection.
CheckRelease <- function(Package=T,Alignment=T,Output=F) {

  if( !inherits(try(XML::readHTMLTable("http://cran.r-project.org/web/packages/BIGDAWG/index.html",header=F),silent=T),"try-error") ) {

    if(Package) {

      CranR <- as.character(XML::readHTMLTable("http://cran.r-project.org/web/packages/BIGDAWG/index.html",header=F)[[1]][1,2])
      GitHubR <- read.table("https://raw.githubusercontent.com/IgDAWG/BIGDAWG/master/DESCRIPTION",sep="\t",stringsAsFactors=F,nrows=4)
      GitHubR <- unlist(strsplit(GitHubR[4,],split=" "))[2]
      CurrR <- as.character(packageVersion('BIGDAWG') )

    }

    if(Alignment) {

      # Get IMGT Release Version

      # release_version not updated consistently
      #download.file("ftp://ftp.ebi.ac.uk/pub/databases/ipd/imgt/hla/release_version.txt",destfile="release_version.txt",method="libcurl")
      #Release <- read.table("release_version.txt",comment.char="",sep="\t")
      #Release <- apply(Release,MARGIN=1,FUN= function(x) gsub(": ",":",x))
      #RV.current <- unlist(strsplit(Release[3],split=":"))[2]

      URL=file("ftp://ftp.ebi.ac.uk/pub/databases/ipd/imgt/hla/Allele_status.txt",method=getOption("url.method", "libcurl"))
      df <- read.table(URL,sep="\t",nrows=3,comment.char="")
      df.v <- unlist(strsplit(df[3,],split=" "))
      RV.current <- paste(df.v[3:4],collapse=" ")

      # Get BIGDAWG
      UPL <- paste(path.package('BIGDAWG'),"/data/UpdatePtnAlign.RData",sep="")
      UpdatePtnList <- NULL ; rm(UpdatePtnList)
      if( file.exists(UPL) ) {
        load(UPL)
        EPL <- UpdatePtnList
        rm(UpdatePtnList,UPL)
        UPL.flag=T
      } else {
        EPL <- ExonPtnList
        UPL.flag=F }

      RV.BIGDAWG <- EPL$Release.Version

    }

    cat("\n")
    if(Package) { cat("BIGDAWG Package Versions:\n","Installed Version: ",CurrR,"\n CRAN Release Version: ",CranR,"\n Developmental version: ",GitHubR,"\n") }
    if(Package & Alignment) { cat("\n") }
    if(Alignment) {
      if(UPL.flag) {
        cat("IMGT/HLA Versions:\n","IMGT/HLA Version: ",RV.current,"\n BIGDAWG version (from update): ",RV.BIGDAWG,"\n")
      } else {
        cat("IMGT/HLA Versions:\n","IMGT/HLA Version: ",RV.current,"\n BIGDAWG version: ",RV.BIGDAWG,"\n")
      }
    }
    cat("\n")

  } else {

    Err.Log(Output,"No.Internet")
    stop("Analysis stopped.",call.=F)

  }

}
