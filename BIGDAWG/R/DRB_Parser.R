#' DRB345 Column Processing
#'
#' Separates DRB345 column pair into separate columns for each locus
#' @param Tab Data frame of sampleIDs, phenotypes, and genotypes
#' @note This function is for internal BIGDAWG use only.
DRB345.parser <- function(Tab) {
  #Tab Dataset Data-frame
  
  getCol <- grep("DRB345",colnames(Tab))
  df <- matrix(data="^",nrow=nrow(Tab),ncol=6)
  colnames(df) <- c("DRB3","DRB3.1","DRB4","DRB4.1","DRB5","DRB5.1")
  tmp.1 <- sapply(Tab[,getCol[1]],FUN=GetField,Res=1) ; tmp.2 <- sapply(Tab[,getCol[2]],FUN=GetField,Res=1)
  
  tmp <- list()
  # DRB3
  tmp[[1]] <- unlist(grep("DRB3",Tab[,getCol[1]])) ; tmp[[2]] <- unlist(grep("DRB3",Tab[,getCol[2]]))
  df[tmp[[1]],1] <- Tab[tmp[[1]],getCol[1]] ; df[tmp[[2]],2] <- Tab[tmp[[2]],getCol[2]]
  df[setdiff(1:nrow(df),tmp[[1]]),1] <- "DRB3*^" ; df[setdiff(1:nrow(df),tmp[[2]]),2] <- "DRB3*^"
  df[which(tmp.1=="00"),1] <- paste("DRB3*",Tab[which(tmp.1=="00"),getCol[1]],sep="")
  df[which(tmp.2=="00"),2] <- paste("DRB3*",Tab[which(tmp.2=="00"),getCol[2]],sep="")
  
  tmp <- list()
  # DRB4
  tmp[[1]] <- unlist(grep("DRB4",Tab[,getCol[1]])) ; tmp[[2]] <- unlist(grep("DRB4",Tab[,getCol[2]]))
  df[tmp[[1]],3] <- Tab[tmp[[1]],getCol[1]] ; df[tmp[[2]],4] <- Tab[tmp[[2]],getCol[2]]
  df[setdiff(1:nrow(df),tmp[[1]]),3] <- "DRB4*^" ; df[setdiff(1:nrow(df),tmp[[2]]),4] <- "DRB4*^"
  df[which(tmp.1=="00"),3] <- paste("DRB4*",Tab[which(tmp.1=="00"),getCol[1]],sep="")
  df[which(tmp.2=="00"),4] <- paste("DRB4*",Tab[which(tmp.2=="00"),getCol[2]],sep="")
  
  tmp <- list()
  # DRB5
  tmp[[1]] <- unlist(grep("DRB5",Tab[,getCol[1]])) ; tmp[[2]] <- unlist(grep("DRB5",Tab[,getCol[2]]))
  df[tmp[[1]],5] <- Tab[tmp[[1]],getCol[1]] ; df[tmp[[2]],6] <- Tab[tmp[[2]],getCol[2]]
  df[setdiff(1:nrow(df),tmp[[1]]),5] <- "DRB5*^" ; df[setdiff(1:nrow(df),tmp[[2]]),6] <- "DRB5*^"
  df[which(tmp.1=="00"),5] <- paste("DRB5*",Tab[which(tmp.1=="00"),getCol[1]],sep="")
  df[which(tmp.2=="00"),6] <- paste("DRB5*",Tab[which(tmp.2=="00"),getCol[2]],sep="")
  
  # NA's
  df[is.na(Tab[,getCol[1]]),] <- NA ; df[is.na(Tab[,getCol[2]]),] <- NA
  
  Tab.sub <- Tab[,-getCol]
  Tab <- cbind(Tab.sub,df)
  
  return(Tab)
  
}

#' DRB345 haplotype zygosity wrapper
#'
#' Checks DR haplotypes for correct zygosity and flags unanticipated haplotypes
#' @param Genotype Row of data set data frame following DRB345 parsing
#' @param Loci.DR DRBx Loci of interest to test for consistency
#' @note This function is for internal use only.
DRB345.Check.Wrapper <- function(Genotype,Loci.DR) {
  
  # Set non-DRB1 Loci
  Loci.DR <- Loci.DR[-grep("DRB1",Loci.DR)]
  
  # Substitute ^ for 00:00
  Genotype[] <- sapply(Genotype,as.character)
  if( sum(grepl("\\^",Genotype)) > 0 ) { Genotype[] <- gsub("\\^","00:00",Genotype) ; Fill.Flag <- T } else { Fill.Flag <- F }
  
  # Apply Function to each DRBx Locus
  tmp <- lapply(Loci.DR,FUN=DRB345.Check.Zygosity,Genotype=Genotype)
  tmp.calls <- lapply( seq(length(tmp)), FUN = function(i) cbind(tmp[[i]]['Locus_1'], tmp[[i]]['Locus_2']) )
  Genotype[,!grepl("DRB1",colnames(Genotype))] <- do.call(cbind, tmp.calls)
  if( Fill.Flag ) { Genotype[] <- gsub("00:00","^",Genotype) }
  
  DR.HapFlag <- unlist(lapply(tmp,'[','Flag'))
  DR.HapFlag <-paste(DR.HapFlag[which(DR.HapFlag!="")],collapse=",")
  
  Genotype <- cbind(Genotype,DR.HapFlag)
  return(Genotype)
  
  
}

#' DRB345 haplotype zygosity checker single locus
#'
#' Checks DR haplotypes for correct zygosity and flags unanticipated haplotypes for a single DRBx
#' @param Locus Locus of interest to test for consistency
#' @param Genotype Row of data set data frame following DRB345 parsing
#' @note This function is for internal use only.
DRB345.Check.Zygosity <- function(Locus,Genotype) {
  
  # Remove Abs Strings
  Genotype <- Filler(Genotype,Type="Remove") ; Genotype <- Genotype[which(Genotype!="")] ;  Genotype <- Genotype[which(Genotype!="")]
  
  DR.out <- data.frame(Locus_1=character(), Locus_2=character(), DR.HapFlag=character(), stringsAsFactors=F)
  Abs <- paste(Locus,"*00:00",sep="")
  
  DR.Locus <- gsub("HLA-","",Locus) ; DR.Calls <- gsub("HLA-","",Genotype)
  DR.Calls <- sapply(DR.Calls,FUN=GetField,Res=1) # get 1 Field Resolution for Genotype Calls
  names(DR.Calls) <- NULL ; Flag <- NULL
  
  #DRB1 - get expected DRB3/4/5 genotypes
  DR345.Exp.Calls <- DRB345.Exp(DR.Calls[grep("DRB1",DR.Calls)])
  
  #DRB345 Check
  getDRB345 <- grep(DR.Locus,DR.Calls) ; DR.obs <- length(getDRB345) ; DR.exp <- sum(grepl(DR.Locus,DR345.Exp.Calls))
  
  # Assign Genotypes
  if( DR.obs != DR.exp ) {
    
    # Inconsistent Genotype Possibilities
    if ( DR.obs==0 && DR.exp>=1 ) {
      # Missing Allele
      DR.out[1, 'Locus_1'] <- Abs ; DR.out[1, 'Locus_2'] <- Abs ; DR.out[1, 'Flag'] <- paste(Locus,"_M",sep="")
    } else if ( DR.obs >=1 && DR.exp==0 ) {
      # Extra Allele
      DR.out[1, 'Locus_1'] <- Genotype[getDRB345[1]] ; DR.out[1, 'Locus_2'] <- Abs ; DR.out[1, 'Flag'] <- paste(Locus,"_P",sep="")
    } else if( DR.obs==1 && DR.exp==2 ) {
      # Presumed Homozygote Missing Allele
      DR.out[1, 'Locus_1'] <- Genotype[getDRB345[1]] ; DR.out[1, 'Locus_2'] <- Genotype[getDRB345[1]] ; DR.out[1, 'Flag'] <- ""
    } else if( DR.obs==2 && DR.exp==1 ) {
      
      if( Genotype[getDRB345[1]] == Genotype[getDRB345[2]] ) {
        # Extra Allele ... False Homozygote Assumption
        DR.out[1, 'Locus_1'] <- Genotype[getDRB345[1]] ; DR.out[1, 'Locus_2'] <- Abs ; DR.out[1, 'Flag'] <- ""
      } else {
        # Extra Allele Present
        DR.out[1, 'Locus_1'] <- Genotype[getDRB345[1]] ; DR.out[1, 'Locus_2'] <-Genotype[getDRB345[2]] ; DR.out[1, 'Flag'] <- paste(Locus,"_P",sep="")
      }
      
    }
    
  } else {
    
    DR.out[1, 'Flag'] <- ""
    
    # Consistent Genotype
    if(  DR.obs==0 ) {
      DR.out[1, 'Locus_1'] <-Abs ; DR.out[1, 'Locus_2'] <- Abs
    } else if( DR.obs==1 ) {
      DR.out[1, 'Locus_1'] <- Genotype[getDRB345[1]] ; DR.out[1, 'Locus_2'] <- Abs
    } else if ( DR.obs==2 ) {
      DR.out[1, 'Locus_1'] <- Genotype[getDRB345[1]] ; DR.out[1, 'Locus_2'] <- Genotype[getDRB345[2]]
    }
    
  }
  
  # Return Result
  return(DR.out)
  
}

#' DRB345 Expected
#'
#' Checks DRB1 Genotype and Returns Expected DR345 Loci
#' @param DRB1.Genotype DRB1 Subject Genotypes
#' @note This function is for internal use only.
DRB345.Exp <- function(DRB1.Genotype) {
  
  #Checks for and fixes certain DRB345 errors that are consistent with known DR haplotypes
  Rules <- list("DRB1*01"="","DRB1*10"="","DRB1*08"="",
                "DRB1*03"="DRB3","DRB1*11"="DRB3","DRB1*12"="DRB3","DRB1*13"="DRB3","DRB1*14"="DRB3",
                "DRB1*04"="DRB4","DRB1*07"="DRB4","DRB1*09"="DRB4",
                "DRB1*15"="DRB5","DRB1*16"="DRB5")
  
  DRB1.Genotype <- gsub("HLA-","",DRB1.Genotype)
  DRB1.Genotype <- sapply(DRB1.Genotype,FUN=GetField,Res=1)
  
  # Allele 1
  DRB1.1 <- DRB1.Genotype[1]
  DR.Gtype <- as.character(Rules[DRB1.1])
  
  # Allele 2
  if(length(DRB1.Genotype)==1) {
    #Consider Homozygote
    DRB1.2 <-  DRB1.Genotype[1]
  } else {
    DRB1.2 <-  DRB1.Genotype[2]
  }
  DR.Gtype <- c(DR.Gtype,as.character(Rules[DRB1.2]))
  DR.Gtype <- DR.Gtype[which(DR.Gtype!="")]
  
  if(length(DR.Gtype)>0) { DR.Gtype <- paste("HLA-",DR.Gtype,sep="") }
  return(DR.Gtype)
  
}

