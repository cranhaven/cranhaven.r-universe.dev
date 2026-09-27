#' Genotype List String to Tabular Data Conversion
#'
#' Expands GL strings to columns of adjacent locus pairs.
#' @param df Data frame containing GL strings
#' @param System Character Genetic system HLA or KIR
#' @param HZY.Red Logical Should homozygote genotypes be a single allele for non-DRB345.
#' @param Abs.Fill Logical Should absent loci special designations be used
#' @param Cores Integer How many cores can be used.
#' @note This function is for internal use only.
Tab2GL.wrapper <- function(df,System,HZY.Red,Abs.Fill,Cores) {
  
  # Define data locus columns assuming Locus columns come in pairs
  colnames(df) <- sapply(colnames(df),FUN=gsub,pattern="\\.1|\\.2|\\_1|\\_2",replacement="")
  DataCol <- which(colnames(df) %in% names(which(table(colnames(df))==2))==TRUE)
  MiscCol <- setdiff(1:ncol(df),DataCol)

  # Check for identical rows of miscellanous information from non-data columns. Ambiguous Data Flag.
  Misc.tmp <- apply(df[,MiscCol],MARGIN=1,FUN=paste,collapse=":")
  if( length(which(table(Misc.tmp)>1))>0 ) {
    Err.Log(FALSE,"Table.Amb") ; stop("Conversion stopped.",call.=F)
  }; rm(Misc.tmp)
  
  # Remove empty data rows
  rmRows <- which(rowSums(apply(df[,DataCol],MARGIN=c(1,2),FUN=nchar))==0)
  if( length(rmRows)!=0 ) { df <- df[-rmRows,] ; rownames(df) <- NULL }
  
  # Pre-format data to SystemLoci*Allele if necessary
  if( sum(grepl(System,colnames(df)[DataCol]))==0 ) { colnames(df)[DataCol] <- paste(System,colnames(df)[DataCol],sep="") }
  for(i in DataCol) {
    if(sum(grepl(colnames(df)[i],df[,i]))==0) {
      df[,i] <- sapply(df[,i], FUN = Append.System, df.name=colnames(df)[i] )
    }
  }

  # Pad Absent Calls for DRBx?
  if( System=="HLA-") {
    getCol <- grep("DRB3|DRB4|DRB5",colnames(df))
    if(length(getCol)>0) {
      if(Abs.Fill) {
        df[,getCol] <- sapply(getCol,FUN=function(i) Filler(df[,i], colnames(df)[i], Type="Fill"))
      } else {
        df[,getCol] <- sapply(getCol,FUN=function(i) Filler(df[,i], Type="Remove"))
      }
    }
  }

  # Run Conversion
  df.list <- lapply(seq(1,nrow(df)), FUN=function(k) df[k,DataCol])
  GL <- parallel::mclapply(df.list,FUN=Tab2GL.Sub,System=System,HZY.Red=HZY.Red,mc.cores=Cores)
  GL <- do.call(rbind,GL)

  if(ncol(GL)==1) { colnames(GL) <- "GL.String" } else if(ncol(GL)==2) { colnames(GL) <- c("GL.String","DR.HapFlag") }
  GL <- cbind(df[,MiscCol],GL)

  return(GL)

}

#' Genotype List String Condenser
#'
#' Condenses column of loci into a GL string using "^"
#' @param x Row of loci to condense
#' @param System Character Genetic system HLA or KIR
#' @param HZY.Red Logical Should homozygote genotypes be a single allele for non-DRB345.
#' @note This function is for internal use only.
Tab2GL.Sub <- function(x,System,HZY.Red) {

  # Identify Loci in data and for HLA-DRB1 expected DRB345 Loci
  x <- x[which(x!="")]
  colnames(x) <- sapply(colnames(x),FUN=gsub,pattern="\\.1|\\.2|\\_1|\\_2",replacement="")
  Loci <- unique(colnames(x))
  if(System=="HLA-") {
    if( sum(grepl("DRB1",Loci))>0 ) { Loci <- c(Loci,DRB345.Exp(x[grep("DRB1",colnames(x))])) }
    if( sum(grepl("\\^",Loci))>0 ) { Loci <- Loci[-grep("\\^",Loci)] }
    Loci <- unique(Loci)
  }

  # Append Locus to ambiguous Allele/Allele calls
  x[] <- sapply(x,Format.Allele,Type="on")

  # Condense Alleles (+)
  GLS <- lapply(Loci,Tab2GL.Loci,Genotype=x,System=System,HZY.Red=HZY.Red)
  GLS <- do.call(rbind,GLS)
  GLS[,1] <- sapply(GLS[,1],FUN=gsub,pattern="NA+NA|\\+NA|NA\\+",replacement="")
  GLS[GLS=="OK"] <- ""

  # Condense Chromosomes (^)
  Out <- paste(as.character(na.omit(GLS[,1])),collapse="^")
  Flag <- paste(GLS[which(GLS[,2]!=""),2],collapse="")

  Out <- c(Out,Flag)

}

#' Locus Condenser for Tab2GL
#'
#' Condenses alleles calls of a single locus string using "+"
#' @param Locus Locus to condense
#' @param Genotype Row of loci to condense
#' @param System Character Genetic system HLA or KIR
#' @param HZY.Red Logical Should homozygote genotypes be a single allele for non-DRB345.
#' @note This function is for internal use only.
Tab2GL.Loci <- function(Locus,Genotype,System,HZY.Red) {

  Alleles <- Genotype[grep(Locus,Genotype)]

  if(System=="HLA-") {
    if(Locus=="HLA-DRB3" || Locus=="HLA-DRB4" || Locus=="HLA-DRB5") {
      if( sum(grepl("DRB1",Genotype))>0 ) {
        # Assumptions for DRB345
        DRB.GTYPE <- DRB345.Check.Zygosity(Locus, Genotype[grep("DRB",Genotype)] )
        DRB.GTYPE[1,grepl("\\^",DRB.GTYPE)] <- NA
        Alleles <- c(DRB.GTYPE[,'Locus_1'],DRB.GTYPE[,'Locus_2'])
        # for inconsistent DR haplotypes
        DRB345.Flag <- DRB.GTYPE[,'Flag']
      } else if( sum(grepl(grep("DRB",Genotype)))>0 ) {
        # DRB345 but no DRB1 (ZYgosity Check Not Determined)
        DRB345.Flag <- "DRB345_ND"
      } # fi no DRB1 but DRB345
    } else { DRB345.Flag <- NULL } # fi DRB345
  } # fi HLA


  if( sum(is.na(Alleles))==0 && HZY.Red && Alleles[1]==Alleles[2] ) {

    # Homozygous Reduction
    GLS <- Alleles[1]

  } else {

    # Remove NA Strings and Collapse
    Alleles <- Alleles[!is.na(Alleles)]
    GLS <- paste(Alleles,collapse="+")

  }

  if(System=="HLA-") {
    DR.HapFlag <- ifelse(!is.null(DRB345.Flag), paste(unlist(DRB345.Flag),collapse=",") , "")
    Out <- c(GLS,DR.HapFlag)
  } else {
    Out <- GLS
  }

  return(Out)

}
