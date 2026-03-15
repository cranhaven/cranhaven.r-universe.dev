#' Prepare imported data
#'
#' Prepare imported data for processing, checks, and analysis.
#' @param Tab Genotypes dataframe.
#' @note This function is for internal BIGDAWG use only.
prepData <- function(Tab) {
  
  Tab[] <- lapply(Tab, as.character)
  
  colnames(Tab) <- gsub( "HLA-","",colnames(Tab) )
  colnames(Tab) <- gsub( "\\.1|\\.2|\\_1|\\_2","",colnames(Tab) )
  colnames(Tab) <- toupper(colnames(Tab))
  colnames(Tab) <- gsub( "DRB3.4.5|DRB3/4/5","DRB345",colnames(Tab) )
  rownames(Tab) <- NULL
  Tab <- rmABstrings(Tab)
  
  return(Tab)
  
}

#' Replace absent allele strings
#'
#' Replaces allowable absent allele strings with ^ symbol.
#' @param df Genotypes dataframe.
#' @note This function is for internal BIGDAWG use only.
rmABstrings <- function(df) {
  df[] <- apply(df, MARGIN=c(1,2), FUN=function(x) gsub("ABSENT|Absent|absent|Abs|ABS|ab|Ab|AB","^",x) )
  df[df=="00"] <- "^"
  df[df=="00:00"] <- "^"
  df[df=="00:00:00"] <- "^"
  df[df=="00:00:00:00"] <- "^"
  return(df)
}

#' Replace or Fill 00:00 allele strings
#'
#' Replaces or Fills absent allele strings.
#' @param x Genotype
#' @param Locus Locus column to adjust.
#' @param Type String specifying whether to pad ('Fill') or leave blank ('Remove') absent calls
#' @note This function is for internal use only.
Filler <- function(x,Locus=NULL,Type) {
  
  if (Type=="Fill") {
    which(x=="")
    Locus <- gsub("_1|_2","",Locus)
    x[which(x=="")] <- paste(Locus,"*00:00",sep="")
    
  }
  
  if (Type=="Remove") {
    
    x[] <- sapply(x, FUN=function(x) gsub("ABSENT|Absent|absent|Abs|ABS|ab|Ab|AB","",x) )
    x[grep("\\*00",x)] <- ""
    
  }
  
  if (Type=="Sub") {
    
    x[] <- sapply(x, FUN=function(x) gsub("ABSENT|Absent|absent|Abs|ABS|ab|Ab|AB","^",x) )
    x[grep("\\*00",x)] <- "^"
    
  }
  
  return(x)
  
}

#' Removes System and Locus from Alleles
#'
#' Removes the System and Locus designations for alleles calls in GL2Tab
#' @param x Allele
#' @note This function is for internal use only.
Stripper <- function(x) {
  
  if( grepl("\\*",x) ) {
    if( is.na(x) ) { Fix <- x
      } else if ( x!="" ) { Fix <- unlist(strsplit(x,"\\*"))[2]
      } else { Fix <- x }
  } else { return (x) }
  
  return(Fix)
  
}

#' Expression Variant Suffix Removal
#'
#' Removes expression variant suffixes from HLA alleles in the exon protein alignment object.
#' @param Locus Locus to be filtered against.
#' @param EPList Exon Protein Alignment Object
#' @note This function is for internal BIGDAWG use only.
EVSremoval <- function(Locus,EPList) {
  if(Locus=='Release') { 
    tmp <- EPList[[Locus]]
    return(tmp)
  } else if(Locus=='RefExons') {
    tmp <- EPList[[Locus]]
    return(tmp)
  } else {
    tmp <- EPList[[Locus]]
    tmp[,'Trimmed'] <- sapply(tmp[,'Trimmed'],gsub,pattern="[[:alpha:]]",replacement="")
    return(tmp)
  }
}

#' HLA trimming function
#'
#' Trim a properly formatted HLA allele to desired number of fields.
#' @param x HLA allele.
#' @param Res Resolution desired.
#' @note This function is for internal BIGDAWG use only.
GetField <- function(x,Res) {
  Tmp <- unlist(strsplit(as.character(x),":"))
  if (length(Tmp)<2) {
    return(x)
  } else if (Res==1) {
    return(Tmp[1])
  } else if (Res > 1) {
    Out <- paste(Tmp[1:Res],collapse=":")
    return(Out)
  }
}

#' Haplotype missing Allele summary function
#'
#' Summary function for identifying missing alleles in a matrix of genotypes.
#' @param geno Matrix of genotypes.
#' @param miss.val Vector of codes for allele missing values.
#' @note This function is for internal BIGDAWG use only and is ported from haplo.stats.
summaryGeno.2 <- function (geno, miss.val = 0) {
  # Ported from R package haplo.stats v 1.7.7
  # Authors: Sinnwell JP, Schaid DJ
  # URL: https://cran.r-project.org/web/packages/haplo.stats/index.html
  n.loci <- ncol(geno)/2
  nr <- nrow(geno)
  geno <- haplo.stats::setupGeno(geno, miss.val)
  loc0 <- numeric(nr)
  loc1 <- numeric(nr)
  loc2 <- numeric(nr)
  for (i in 1:nr) {
    first.indx <- seq(1, (2 * n.loci - 1), by = 2)
    miss.one <- is.na(geno[i, first.indx]) | is.na(geno[i, 
                                                        first.indx + 1])
    miss.two <- is.na(geno[i, first.indx]) & is.na(geno[i, 
                                                        first.indx + 1])
    loc2[i] <- sum(miss.two)
    loc1[i] <- sum(miss.one - miss.two)
    loc0[i] <- sum(!miss.one)
  }
  tbl <- data.frame(missing0 = loc0, missing1 = loc1, missing2 = loc2)
  return(tbl)
}

#' Data Object Merge and Output
#'
#' Whole data set table construction of per haplotype for odds ratio, confidence intervals, and pvalues
#' @param BD.out Output of analysis as list.
#' @param Run Tests that are to be run as defined by Run.Tests.
#' @param OutDir Output directory defined by Results.Dir or default.
#' @note This function is for internal BIGDAWG use only.
MergeData_Output <- function(BD.out,Run,OutDir) {
  
  FM.out <- data.frame(Analysis=character(),
                       Locus=character(),
                       Allele=character(),
                       Group.0=numeric(),
                       Group.1=numeric())
  
  CN.out <- data.frame(Analysis=character(),
                       Locus=character(),
                       Allele=character(),
                       Group.0=numeric(),
                       Group.1=numeric())
  
  OR.out <- data.frame(Analysis=character(),
                       Locus=character(),
                       Allele=character(),
                       OR=numeric(),
                       CI.Lower=numeric(),
                       CI.Upper=numeric(),
                       p.value=numeric(),
                       sig=character())
  
  CS.out <- data.frame(Analysis=character(),
                       Locus=character(),
                       x.square=numeric(),
                       df=numeric(),
                       p.value=numeric(),
                       sig=character())
  
  for(i in Run) {
    
    switch(i,
           H= { TestName <- "Haplotype" },
           L= { TestName <- "Locus" },
           A= { TestName <- "AminoAcid" } )
    
    Test <- BD.out[[i]]
    
    for(k in 1:length(Test)) {
      
      Test.sub <- Test[[k]]
      
      #Frequencies
      tmp <- Test.sub$freq
      if(i=="A") { Allele <- paste(tmp[,'Position'],tmp[,'Residue'],sep="::") }
      switch(i,
             H = { tmp <- cbind(rep(TestName,nrow(tmp)),rep(colnames(tmp)[1],nrow(tmp)),tmp) },
             L = { tmp <- cbind(rep(TestName,nrow(tmp)),tmp) },
             A = { tmp <- cbind(rep(TestName,nrow(tmp)),tmp[,'Locus'],Allele,tmp[,c('Group.0','Group.1')]) })
      colnames(tmp) <- c("Analysis","Locus","Allele","Group.0","Group.1")
      FM.out <- rbind(tmp,FM.out) ; rm(tmp)
      
      #Counts
      tmp <- Test.sub$table
      if(i=="A") { Allele <- paste(tmp[,'Position'],tmp[,'Residue'],sep="::") }
      switch(i,
             H = { tmp <- cbind(rep(TestName,nrow(tmp)),rep(colnames(tmp)[1],nrow(tmp)),tmp) },
             L = { tmp <- cbind(rep(TestName,nrow(tmp)),tmp) },
             A = { tmp <- cbind(rep(TestName,nrow(tmp)),tmp[,'Locus'],Allele,tmp[,c('Group.0','Group.1')]) })
      colnames(tmp) <- c("Analysis","Locus","Allele","Group.0","Group.1")
      CN.out <- rbind(tmp,CN.out) ; rm(tmp)
      
      #Odds Ratios
      tmp <- Test.sub$OR
      if(i=="A") { Allele <- paste(tmp[,'Position'],tmp[,'Residue'],sep="::") }
      switch(i,
             H = { tmp <- cbind(rep(TestName,nrow(tmp)),rep(colnames(tmp)[1],nrow(tmp)),tmp) },
             L = { tmp <- cbind(rep(TestName,nrow(tmp)),tmp) },
             A = { tmp <- cbind(rep(TestName,nrow(tmp)),tmp[,'Locus'],Allele,tmp[,c("OR","CI.lower","CI.upper","p.value","sig")]) })
      colnames(tmp) <- c("Analysis","Locus","Allele","OR","CI.Lower","CI.Upper","p.value","sig")
      OR.out <- rbind(tmp,OR.out) ; rm(tmp)
      
      #ChiSq
      tmp <- Test.sub$chisq
      if(i=="A") { Locus <- paste(tmp[,'Locus'],tmp[,'Position'],sep="::") }
      switch(i,
             H = { tmp <- cbind(rep(TestName,nrow(tmp)), rep(names(Test)[k],nrow(tmp)), tmp) },
             L = { tmp <- cbind(rep(TestName,nrow(tmp)), tmp) },
             A = { tmp <- cbind(rep(TestName,nrow(tmp)), Locus, tmp[,c('X.square','df','p.value','sig')] ) } )
      colnames(tmp)[1:2] <- c("Analysis","Locus")
      rownames(tmp) <- NULL
      CS.out <- rbind(tmp,CS.out); rm(tmp)
      
    }; rm(k)
    
  }; rm(i)
  
  setwd(OutDir)
  
  # Remove redundant entries
  # Especially relevant to multi-set runs
  FM.out <- unique(FM.out)
  CN.out <- unique(CN.out)
  CS.out <- unique(CS.out)
  
  OR.out <- apply(OR.out,MARGIN=c(1,2),as.character)
  OR.out <- unique(OR.out)
  
  write.table(FM.out,file="Merged_Frequencies.txt",sep="\t",col.names=T,row.names=F,quote=F)
  write.table(CN.out,file="Merged_Counts.txt",sep="\t",col.names=T,row.names=F,quote=F)
  write.table(CS.out,file="Merged_ChiSq.txt",sep="\t",col.names=T,row.names=F,quote=F)
  write.table(OR.out,file="Merged_OddsRatio.txt",sep="\t",col.names=T,row.names=F,quote=F)
  
}

#' File Name Extraction
#'
#' Function to extract file path.
#' @param x File name.
#' @note This function is for internal use only.
getFileName <- function(x) {
  
  tmpDir <- dirname(x)
  tmpName <- basename(x)
  
  if(basename(x)==x) {
    outName <- paste("Converted_",x,sep="")
  } else {
    outName <- paste(tmpDir,"/Converted_",tmpName,sep="")
  }
  
  outName <- gsub(".txt","",outName)
  return(outName)
  
}

#' Build Output Matrix for GL2Tab Conversion
#'
#' Initializes output matrix format for GL2Tab conversion
#' @param System Character Genetic system HLA- or KIR
#' @param Loci The loci for header names
#' @note This function is for internal use only.
Build.Matrix <- function(System,Loci) {
  
  Loci.Grp <- rep(Loci,each=2)
  
  if(System=="HLA-") {
    Out <- mat.or.vec(nr=1,nc=length(Loci.Grp)+1) ; colnames(Out) <- c(Loci.Grp,"DR.HapFlag")
  } else {
    Out <- mat.or.vec(nr=1,nc=length(Loci.Grp)) ; colnames(Out) <- Loci.Grp
  }
  colnames(Out)[seq(1,length(Loci.Grp),by=2)] <- paste(Loci,"_1",sep="")
  colnames(Out)[seq(2,length(Loci.Grp),by=2)] <- paste(Loci,"_2",sep="")
  
  return(Out)
  
}

#' Tabular Data Locus Format Tool
#'
#' Correctly orders the expanded GL string
#' @param x Single row of converted GL string
#' @param Order Single row data frame for mapping converted GL strings
#' @note This function is for internal use only.
Format.Tab <- function(x,Order) {
  
  Order[,match(colnames(x),colnames(Order))] <- x
  return(Order)
  
}

#' Ambiguous Alleles Locus Name Formatting
#'
#' Remove or Append Locus name from/to allele in an ambiguous allele string
#' @param x Allele String
#' @param Type String specifying whether to strip ('off') or append ('on') locus prefix
#' @note This function is for internal use only.
Format.Allele <- function(x,Type) {
  
  if(Type=="off") {
    if(grepl("/",x)) {
      tmp <- strsplit(unlist(strsplit(x,"/")),"\\*")
      Fix <- paste(unlist(lapply(tmp,"[",1)[1]),
                   paste(unlist(lapply(tmp,"[",2)),collapse="/"),
                   sep="*")
    } else {  Fix <- x }
  }
  
  if(Type=="on"){
    if(grepl("/",x)) {
      Locus <- unlist(strsplit(x,"\\*"))[1]
      Fix <- paste(
        paste(
          Locus,unlist(strsplit(unlist(strsplit(x,"\\*"))[2],"/"))
          ,sep="*")
        ,collapse="/")
    } else { Fix <- x }
  }
  
  return(Fix)
}

#' Append Genetic System Locus Designation to Allele String
#'
#' Adds genetic system (HLA/KIR) to each allele name
#' @param x Vector Column genotypes to append
#' @param df.name String SystemLocus name for each allele.
#' @note This function is for internal use only.
Append.System <- function(x,df.name) {
  
  getAllele <- which(x!="")
  x[getAllele] <- paste(df.name,x[getAllele],sep="*")
  return(x)
  
}
