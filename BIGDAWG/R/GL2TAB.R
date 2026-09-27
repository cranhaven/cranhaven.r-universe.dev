#' Genotype List String to Tabular Data Conversion
#'
#' Expands GL strings to columns of adjacent locus pairs.
#' @param df Data frame containing GL strings
#' @param System Character Genetic system HLA or KIR
#' @param Strip.Prefix Logical Should System/Locus prefixes be stripped from table data.
#' @param Abs.Fill Logical Should absent loci special designations be used.
#' @param Cores Integer How many cores can be used
#' @note This function is for internal use only
GL2Tab.wrapper <- function(df,System,Strip.Prefix,Abs.Fill,Cores) {

  # Data column
  LastCol <- ncol(df)
  MiscCol <- seq(1,ncol(df)-1)

  # Remove empty data rows
  df <- na.omit(df)
  rmRows <- which(nchar(df[,LastCol])==0)
  if( length(rmRows)!=0 ) { df <- df[-rmRows,] }
  
  # Run Conversion
  df.list <- strsplit(df[,LastCol],"\\^")
  Tab <- parallel::mclapply(df.list,FUN=GL2Tab.Sub,System=System,mc.cores=Cores)
    Loci <- sort(unique(gsub("_1|_2","",unlist(lapply(Tab,colnames)))))
    if( sum(grepl('DR.HapFlag',Loci)) > 0 ) { Loci <- Loci[-grep('DR.HapFlag',Loci)] }
    Order <- Build.Matrix(System,Loci)

  Tab <- parallel::mclapply(Tab,FUN=Format.Tab,Order=Order,mc.cores=Cores)
    Tab <- do.call(rbind,Tab)
    Tab[Tab==0] <- ""
    Tab[grepl("\\^",Tab)] <- ""

  # Pad Absent Calls for DRBx?
  if( System=="HLA-") {
    getCol <- grep("DRB3|DRB4|DRB5",colnames(Tab))
    if(length(getCol)>0) {
      if(Abs.Fill) {
        Tab[,getCol] <- sapply(getCol,FUN=function(i) Filler(Tab[,i], colnames(Tab)[i], Type="Fill"))
      } else {
        Tab[,getCol] <- sapply(getCol,FUN=function(i) Filler(Tab[,i], Type="Remove"))
      }
    }
  }

  # Strip Prefixes?
  if(Strip.Prefix) { Tab[,seq(1,ncol(Tab)-1)] <- apply(Tab[,seq(1,ncol(Tab)-1)],MARGIN=c(1,2),FUN=Stripper) }

  # Final Table with Misc Information Appended
  Tab <- cbind(df[,MiscCol],Tab)

  return(Tab)

}

#' Genotype List String Expander
#'
#' Expands GL string into a table of adjacent loci
#' @param x Character GL string to expand
#' @param System Character Genetic system HLA or KIR
#' @note This function is for internal use only.
GL2Tab.Sub <- function(x,System) {

  # Break GL String and Remove Any Absent Call Type Strings (00:00)
  Calls <- unlist(sapply(x,FUN=function(x) strsplit(x,"\\+"))) ; names(Calls) <- NULL

  # Check GL String For Locus*Allele/Locus*Allele Ambiguity Formatting
  invisible(sapply(Calls,CheckString.Allele))

  # Collapse Ambiguous allele names to remove locus prefix
  if( sum(grepl("/",Calls)>0 ) ) {
    Calls <- unlist(lapply(Calls,Format.Allele,Type="off"))
  }

  # Get loci and initialize table
  Loci <- unique(unlist(lapply(strsplit(Calls,"\\*"),"[",1)))
  if(System=="HLA-") {
    if( sum(grepl("DRB1",Loci))>0 ) { Loci <- c(Loci,DRB345.Exp(Calls[grep("DRB1",Calls)])) }
    Loci <- unique(Loci)
  }
  Tab <- Build.Matrix(System,Loci)

  # Check GL String For Locus^Gene Field Consistency
  invisible(CheckString.Locus(x,Loci))

  # Populate table
  tmp <- lapply(Loci,GL2Tab.Loci,Genotype=Calls,System=System)
  tmp.calls <- lapply( seq(length(tmp)), FUN = function(i) cbind(tmp[[i]]['Locus_1'], tmp[[i]]['Locus_2']) )
  tmp.calls <- do.call(cbind, tmp.calls)

  DR.HapFlag <- unlist(lapply(tmp,'[','DR.HapFlag'))
  DR.HapFlag <-paste(DR.HapFlag[which(DR.HapFlag!="")],collapse=",")

  Tab[1,] <- cbind(tmp.calls,DR.HapFlag)

  return(Tab)

}

#' Locus Ordering for GL2Tab
#'
#' Orders Locus Calls
#' @param Locus Locus to condense
#' @param Genotype Row of loci to condense
#' @param System Character Genetic system HLA or KIR
#' @note This function is for internal use only.
GL2Tab.Loci <- function(Locus,Genotype,System) {

  Alleles <- Genotype[grep(Locus,Genotype)]

  if( length(Alleles) == 1 && System=="HLA-" ) {
   if( Locus!="HLA-DRB3" || Locus!="HLA-DRB4" || Locus!="HLA-DRB5" ) {
    # Homozygous Assumption for HLA non-DRB345
    Alleles <- c(Alleles,Alleles)
   }
  } else if ( length(Alleles) == 1 ) {
    Alleles <- c(Alleles,"")
  } else if ( length(Alleles) == 0 ) {
    Alleles <- c("","")
  }
  names(Alleles) <- c('Locus_1','Locus_2')

  if(System=="HLA-") {
    if(Locus=="HLA-DRB3" || Locus=="HLA-DRB4" || Locus=="HLA-DRB5") {
      if( sum(grepl("DRB1",Genotype))>0 ) {
        # Assumptions for DRB345
        DRB.GTYPE <- DRB345.Check.Zygosity(Locus,Genotype[grep("DRB",Genotype)])
        DRB.GTYPE[1,grepl("\\^",DRB.GTYPE)] <- ""
        Alleles[] <- c(DRB.GTYPE[,'Locus_1'],DRB.GTYPE[,'Locus_2'])
        # for inconsistent DR haplotypes
        DRB345.Flag <- DRB.GTYPE[,'Flag']
      } else {
        # No DRB1 but DBR345 (ZYgosity Check Not Determined)
        DRB345.Flag <- "DRB345_ND"
      } # fi DRB1
     } else { DRB345.Flag <- NULL } # fi DRB345
    } # fi HLA

  if(System=="HLA-") {
    DR.HapFlag <- ifelse(!is.null(DRB345.Flag), paste(unlist(DRB345.Flag),collapse=",") , "")
    Out <- c(Alleles,DR.HapFlag)
    names(Out)[length(Out)] <- "DR.HapFlag"
  } else {
    Out <- Alleles
  }

 return(Out)

}
