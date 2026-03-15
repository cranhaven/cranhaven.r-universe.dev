#' BIGDAWG Main Wrapper Function
#'
#' This is the main wrapper function for each analysis.
#' @param Data Name of the genotype data file.
#' @param HLA Logical Indicating whether data is HLA class I/II genotyping data only.
#' @param Run.Tests Specifics which tests to run.
#' @param Loci.Set Input list defining which loci to use for analyses (combinations permitted).
#' @param Exon Numeric Exon(s) for targeted amino acid analysis.
#' @param All.Pairwise Logical indicating whether all pairwise loci should be analyzed in haplotype analysis.
#' @param Trim Logical indicating if HLA alleles should be trimmed to a set resolution.
#' @param Res Numeric setting what desired resolution to trim HLA alleles.
#' @param EVS.rm Logical indicating if expression variant suffixes should be removed.
#' @param Missing Numeric setting allowable missing data for running analysis (may use "ignore").
#' @param Strict.Bin Logical specify if strict rare cell binning should be used in ChiSq test.
#' @param Cores.Lim Integer setting the number of cores accessible to BIGDAWG (Windows limit is 1 core).
#' @param Results.Dir Optional, string of full path directory name for BIGDAWG output.
#' @param Return Logical Should analysis results be returned as list.
#' @param Output Logical Should analysis results be written to output directory.
#' @param Merge.Output Logical Should analysis results be merged into a single file for easy access.
#' @param Verbose Logical Should a summary of each analysis be displayed in console.
#' @examples
#' \dontrun{
#' ### The following examples use the synthetic data set bundled with BIGDAWG
#'
#' # Haplotype analysis with no missing genotypes for two loci sets
#' # Significant haplotype association with phenotype
#' # BIGDAWG(Data="HLA_data", Run.Tests="H", Missing=0, Loci.Set=list(c("DRB1","DQB1")))
#'
#' # Hardy-Weinberg and Locus analysis ignoring missing data
#' # Significant locus associations with phenotype at all but DQB1
#' # BIGDAWG(Data="HLA_data", Run.Tests="L", Missing="ignore")
#'
#' # Hardy-Weinberg analysis trimming data to 2-Field resolution with no output to files (console only)
#' # Significant locus deviation at DQB1
#' BIGDAWG(Data="HLA_data", Run.Tests="HWE", Trim=TRUE, Res=2, Output=FALSE)
#' }
BIGDAWG <- function(Data, HLA=TRUE, Run.Tests, Loci.Set, Exon, All.Pairwise=FALSE, Trim=FALSE, Res=2, EVS.rm=FALSE, Missing=2, Strict.Bin=FALSE, Cores.Lim=1L, Results.Dir, Return=FALSE, Output=TRUE, Merge.Output=FALSE, Verbose=TRUE) {

  options(warn=-1)

  MainDir <- getwd()
  on.exit(setwd(MainDir), add = TRUE)

  # CHECK PARAMETERS
  if( missing(Data) ) { Err.Log("P.Missing","Data") ; stop("Analysis Stopped.",call.=FALSE) }
  HLA <- as.logical(HLA)
  Check.Params(HLA, Loci.Set, Exon, All.Pairwise, Trim, Res, EVS.rm, Missing, Cores.Lim, Return, Output, Merge.Output, Verbose)

  # MULTICORE LIMITATIONS
  Cores <- Check.Cores(Cores.Lim,Output)

  cat(rep("=",40))
  cat("\n          BIGDAWG: Bridging ImmunoGenomic Data Analysis Workflow Gaps\n")
  cat(rep("=",40),"\n")
  cat("\n>>>>>>>>>>>>>>>>>>>>>>>>> BEGIN Analysis <<<<<<<<<<<<<<<<<<<<<<<<<\n\n")

  # Define Output object
  BD.out <- list()

# ===================================================================================================================================== ####
# Read in Data ________________________________________________________________________________________________________________________ ####

  NAstrings=c("NA","","****","-","na","Na")

  if(is.character(Data)) {

    if (Data=="HLA_data") {

      # Using internal synthetic set
      Tab <- BIGDAWG::HLA_data
      Data.Flag <- "Internal Synthetic Data Set"

    } else {

      # Read in data file entered as string
      if(!file.exists(Data)) { Err.Log(Output,"Bad.Filename", Data) ; stop("Analysis stopped.",call.=F) }
      Tab <- read.table(Data, header = T, sep="\t", stringsAsFactors = F, na.strings=NAstrings, fill=T, comment.char = "#", strip.white=T, blank.lines.skip=T, colClasses="character")
      Data.Flag <- Data

    }

  } else {

    # Using R object
    Tab <- Data
    Data.Flag <- deparse(substitute(Data))

    # Convert Empty Cells to NA
    for ( i in 3:ncol(Tab) ) {
      putCell <- which( sapply( Tab[,i], nchar )==0 )
      if( length(putCell) > 0 ) { Tab[putCell,i] <- NA }
    }

  }

  # Declare Data Input Parameter
  cat("Data Input:",Data.Flag,"\n\n\n")

  # Convert GLS data
  if( ncol(Tab)==3 && !HLA ) { Err.Log(Output,"notHLA.GLS") }
  if( ncol(Tab)==3 && HLA ) {
    cat("Converting Gene List Strings to Tabular Format...\n\n")
    Tab <- GLSconvert(Tab,Convert="GL2Tab",System="HLA",File.Output="R",Strip.Prefix=T,Abs.Fill=T,Cores.Lim=Cores)
  }

  # Prep Data for processing and checks
  Tab <- prepData(Tab)

  # Define and Change to the required output directory
  if (Output) {
    if(missing(Results.Dir)) {
      OutDir <- paste(MainDir,"/output ",format(Sys.time(), "%d%m%y %H%M%S"),sep="")
      dir.create(OutDir)
    } else {
      OutDir <- Results.Dir
    }
  }
  if(Output) { setwd(OutDir) }

# ===================================================================================================================================== ####
# Data Processing and Sanity Checks ___________________________________________________________________________________________________ ####

  cat(">>>> DATA PROCESSING AND CHECKS.\n")

  #### General processing and checks for all data

  # Define Data Columns
  Data.Col <- seq(3,ncol(Tab))

  # RUN TESTS DEFINITIONS
  if ( missing(Run.Tests) ) { Run <- c("HWE","H","L","A") } else { Run <- Run.Tests }
  if(!HLA) {
    if("A" %in% Run) {
      cat("Not HLA data. Skipping Amino Acid Analysis.\n")
      Run <- Run[-which(Run=="A")]
    }
  }

  # BAD DATA DEFINITIONS - No 1's or 0's
  if( length(which(Tab[,Data.Col]==0))>0 || length(which(Tab[,Data.Col]==0))>1 ) {
    Err.Log(Output,"Bad.Data")
    stop("Analysis Stopped.",call. = F)
  }

  # MISSING DATA
  if(Missing == "ignore") {
    cat("Ignoring any missing data.\n")
    Err.Log(Output,"Ignore.Missing")
    rows.rm <- NULL
  } else {
    if (Missing > 2) {
      if ("H" %in% Run) { Err.Log(Output,"Big.Missing") }
    }
    cat("Removing any missing data. This will affect Hardy-Weinberg Equilibrium test.\n")
    geno.desc <- summaryGeno.2(Tab[,Data.Col], miss.val=NAstrings)
    test <- geno.desc[,2] + 2*geno.desc[,3]
    rows.rm <- which(test > Missing)
    if( length(rows.rm) > 0 ) {
      rows.rm <- which(test > Missing)
      ID.rm <- Tab[rows.rm,1]
      Tab <- Tab[-rows.rm,]
      if(Output) { write.table(ID.rm, file="Removed_SampleIDs.txt", sep="\t", row.names=F, col.names=F, quote=F) }
      rm(ID.rm)
    }
    rm(geno.desc,test)
    if(nrow(Tab)==0) { Err.Log(Output,"TooMany.Missing") ; stop("Analysis Stopped.",call. = F) }
  }

  # MULTIPLE SETS AND ANALYSIS DUPLICATION
  if(!missing(Loci.Set)) {
    if( length(Loci.Set)>1 && (All.Pairwise | "L" %in% Run | "A" %in% Run ) ) { Err.Log(Output,"MultipleSets") }
  }

  # DATA MERGE AND NUMBER OF LOCI
  if(Output && Merge.Output && All.Pairwise) {
    if(ncol(Tab)>52) { Err.Log(Output,"AllPairwise.Merge") }
  }

  ##### HLA specific checks

  #Check for the updated ExonPtnList 'UpdatePtnList' and use if found.
  UpdatePtnList <- NULL
  UPL <- paste(path.package('BIGDAWG'),"/data/UpdatePtnAlign.RData",sep="")
  if( file.exists(UPL) ) {
    load(UPL)
    EPL <- UpdatePtnList
    rm(UpdatePtnList)
    UPL.flag=T
  } else {
    rm(UpdatePtnList,UPL)
    EPL <- BIGDAWG::ExonPtnList
    UPL.flag=F }

  if(Trim & !HLA) { Err.Log(Output,"NotHLA.Trim") }
  if(EVS.rm & !HLA) { Err.Log(Output,"NotHLA.EVS.rm") }
  if(!HLA) { DRBFLAG <- NULL }  else { DRB345.test <- length(grep("DRB345",colnames(Tab)))>0 }

  if(HLA) {

    if(Trim | EVS.rm | "A" %in% Run | DRB345.test ) { cat("Running HLA specific check functions...\n") }

    # Check Locus*Allele Formatting across all loci
    CheckCol <- sum( unlist( apply(Tab[,Data.Col], MARGIN=c(1,2), FUN = function(x) grepl("\\*",na.omit(x))) ) )
    TotalCol <-  ( dim(Tab[,Data.Col])[1] * dim(Tab[,Data.Col])[2] ) - ( length(which(Tab[,Data.Col]=="^")) + sum(is.na(Tab[,Data.Col])) )

    if( CheckCol>0 && CheckCol!=TotalCol ) {
      Err.Log(Output,"Bad.Format.HLA")
      stop("Analysis Stopped.",call. = F)
    }

    # Separate DRB345 if exists as single column pair and check zygosity
    if(DRB345.test) {

      cat("Processing DRB345 column data.\n")

      DRBFLAG <- T

      # Expand DRB3/4/5 to separate column pairs
      Tab <- DRB345.parser(Tab)
      colnames(Tab) <- sapply(colnames(Tab),FUN=gsub,pattern="\\.1",replacement="")

      # Redefine Data Columns
      Data.Col <- seq(3,ncol(Tab))

      # Define DR Loci to Process
      getCol <- grep("DRB",colnames(Tab))
      Loci.DR <- unique(colnames(Tab)[getCol])

      # Process Loci
      Tab.list <- lapply(seq_len(nrow(Tab)), FUN=function(z) Tab[z,getCol])
      Tab.tmp <- mclapply(Tab.list,FUN=DRB345.Check.Wrapper,Loci.DR=Loci.DR,mc.cores=Cores)
      Tab.tmp <- do.call(rbind,Tab.tmp)
      Tab[,getCol] <- Tab.tmp[,grep("DRB",colnames(Tab.tmp))]
      Tab <- cbind(Tab,Tab.tmp[,'DR.HapFlag']) ; colnames(Tab)[ncol(Tab)] <- "DR.HapFlag"

      #Identify DR345 flagged haplotypes and Write to File
      DR.Flags <- Tab[which(Tab[,'DR.HapFlag']!=""),c(1,2,getCol,ncol(Tab))] ; row.names(DR.Flags) <- NULL

      if(Output) {
        if(!is.null(DR.Flags)) {
          Err.Log(Output,"Bad.DRB345.hap") ; cat("\n")
          write.table(DR.Flags,file="Flagged_DRB345_Haplotypes.txt",sep="\t",quote=F,row.names=F,col.names=T)
        }
      }

      cat("\n")
    } else { DRBFLAG <- F }

    # Separate locus and allele names if data is formatted as Loci*Allele
    Tab[,Data.Col] <- apply(Tab[,Data.Col],MARGIN=c(1,2),FUN=Stripper)

    # Sanity Check for Resolution if Trim="T" and Trim Data
    if(Trim & CheckHLA(Tab[,Data.Col])) {
      cat("--Trimming Data.\n")
      #Tab.untrim <- Tab
      Tab[,Data.Col] <- apply(Tab[,Data.Col],MARGIN=c(1,2),GetField,Res=Res)
      rownames(Tab) <- NULL
    } else if (Trim) {
      Err.Log(Output,"Bad.Format.Trim")
      stop("Analysis Stopped.",call. = F)
    }

    # Sanity Check for Expression Variant Suffix Stripping
    if(EVS.rm & CheckHLA(Tab[,Data.Col])) {
      cat("--Stripping Expression Variants Suffixes.\n")
      Tab[,Data.Col] <- apply(Tab[,Data.Col],MARGIN=c(1,2),gsub,pattern="[[:alpha:]]",replacement="")
      EVS.loci <- as.list(names(EPL))
      EPL <- lapply(EVS.loci,EVSremoval,EPList=EPL)
      names(EPL) <- EVS.loci ; rm(EVS.loci)
    } else if (EVS.rm) {
      Err.Log(Output,"Bad.Format.EVS")
      stop("Analysis Stopped.",call. = F)
    }

    # Sanity Check for Amino Acid Test Feasibility
    if ("A" %in% Run) {

      cat("Running Amino Acid Analysis specific checks functions...\n")

      Release <- EPL$Release.Version

      # Sanity Check for Known HLA loci in Bundled Database Release
      cat(paste("--Checking loci against database version",Release,".\n",sep=""))
      test <- CheckLoci(names(EPL),unique(colnames(Tab)[Data.Col]))
      if( test$Flag  ) { Err.Log(Output,"Bad.Locus.HLA",test$Loci) ; stop("Analysis stopped.",call. = F) }

      # Sanity Check for Known HLA alleles in Bundled Database Release
      cat(paste("--Checking alleles against database version",Release,".\n",sep=""))
      test <- CheckAlleles(EPL, Tab[,Data.Col])
      if( test$Flag ) { Err.Log(Output,"Bad.Allele.HLA",test$Alleles) ; stop("Analysis stopped.",call. = F) }

      # Sanity Check for Analysis and HLA Allele Resolution (MUST perform THIS STEP AFTER TRIM!!!!)
      if(Res<2 | !CheckHLA(Tab[,Data.Col]))  {
        Err.Log(Output,"Low.Res")
        cat("You have opted to run the amino acid analysis.\n")
        stop("Analysis stopped.",call. = F)
      }

    } # End A if statement

  } # End HLA if statement and HLA specific functionalities

  # LOCI SET COLUMN DEFINITIONS
  # This section MUST follow DRB345 processing (above) on the chance that DRB345 is formatted as single column
  # and DRB3, DRB4, or DRB5 is defined in Loci.Set.
  if(missing(Loci.Set)) {
    Set <- list(Data.Col)
  } else {
    Loci.Set <- lapply(Loci.Set,FUN=function(x) sapply(x,toupper))
    Set <- lapply(Loci.Set,FUN=function(x) seq(1,ncol(Tab))[colnames(Tab) %in% x])
  }

  # LOCUS SET DEFINED DOES NOT EXIST IN DATA
  if(!missing(Loci.Set)) {
    Loci.Set <- unique(unlist(Loci.Set))
    Loci.Data <- colnames(Tab)[Data.Col]
    if ( sum(Loci.Set %in% Loci.Data) != length(Loci.Set) ) { Err.Log(Output,"PhantomSets") ; stop("Analysis Stopped.",call. = F) }
  }


# ===================================================================================================================================== ####
# Case-Control Summary ________________________________________________________________________________________________________________ ####

cat("\n>>>> CASE - CONTROL SUMMARY STATISTICS\n")
#cat(paste(rep("_",50),collapse=""),"\n")
if (Trim) { rescall <- paste(Res,"-Field",sep="") } else { rescall <- "Not Defined" }
Check <- PreCheck(Tab,colnames(Tab),rescall,HLA,Verbose,Output)
if(Output) { write.table(Check,file="Data_Summary.txt",sep=": ",col.names=F,row.names=T,quote=F); rm(Check,rescall) }


# ===================================================================================================================================== ####
# Write to Parameter File _____________________________________________________________________________________________________________ ####

  if(Output) {

    if(HLA && !is.null(DRBFLAG)) { DRB345.tmp <- DRBFLAG } else { DRB345.tmp <- NULL }
    if(HLA) { Trim.tmp <- Trim } else { Trim.tmp <- NULL }
    if(HLA && Trim) { Res.tmp <- Res } else { Res.tmp <- NULL }
    if(HLA) { EVS.rm.tmp <- EVS.rm } else { EVS.rm.tmp <- NULL }
    if( !missing(Exon) ) { Exon.tmp <- paste(unique(unlist(Exon)),collapse=",") } else { Exon.tmp <- NULL }

    Params.Run <- list(Time = format(Sys.time(), "%a %b %d %X %Y"),
                       BD.Version = as.character(packageVersion("BIGDAWG")),
                       Cores.Used = Cores,
                       File = Data.Flag,
                       Output.Results = Output,
                       Merge = Merge.Output,
                       Return.Object = Return,
                       Display.Results = Verbose,
                       HLA.Data = HLA,
                       Exon = Exon.tmp,
                       DRB345.Parsed = DRB345.tmp,
                       Tests = paste(Run,collapse=","),
                       All.Pairwise = All.Pairwise,
                       Trim = Trim.tmp,
                       Resolution = Res.tmp,
                       Suffix.Stripping = EVS.rm.tmp,
                       Missing.Allowed = Missing,
                       Strict.Binning = Strict.Bin,
                       Samples.Removed = length(rows.rm))

    Params.Run <- do.call(rbind,Params.Run)
    write.table(Params.Run,file="Run_Parameters.txt",sep=": ", row.names=T, col.names=F, quote=F)
  }

# ===================================================================================================================================== ####
# Hardy Weignberg Equilibrium 'HWE' ___________________________________________________________________________________________________ ####

  if ("HWE" %in% Run) {

    cat("\n>>>> STARTING HARDY-WEINBERG ANALYSIS...\n")
    #cat(paste(rep("_",50),collapse=""),"\n")
    if(HLA && Trim) {
      cat("HWE performed at user defined resolution.\n")
    } else if (HLA) {
      cat("HWE performed at maximum available resolution.\n")
    }

    HWE <- HWE.wrapper(Tab,Output,Verbose)
    BD.out[['HWE']] <- HWE
    rm(HWE)

  } #END HARDY-WEINBERG

# ===================================================================================================================================== ####
# Set Loop Begin (loop through each defined locus/loci set) ___________________________________________________________________________ ####

  if ( sum( c("H","L","A") %in% Run ) > 0 ) {

    cat("\n>>>>>>>>>>>>>>>>>>>>>>>>> Begin Locus Sets <<<<<<<<<<<<<<<<<<<<<<<<<\n\n")
    if(length(Set)==1) {
      cat("Your analysis has 1 set to analyze.\n")
    } else {
      cat(paste("Your analysis has ", length(Set), " sets to analyze.", sep=""),"\n")
    }

    for(k in 1:length(Set)) {
      cat("\n")
      cat(paste(rep(">",35),collapse=""),"Running Set",k,"\n")

      cols <- Set[[k]]
      Tabsub <- Tab[,c(1,2,cols)]

      #Set Specific Global Variables
      SID <- Tabsub[,1] # sample IDs
      genos <- Tabsub[,3:ncol(Tabsub)] # genotypes
      genos[genos==""] <- NA
      grp <- Tabsub[, 2] # phenotype
      #nGrp0 <- length(which(grp==0))*2 #nalleles
      #nGrp1 <- length(which(grp==1))*2 #nalleles
      loci <- unique(gsub(".1","",colnames(genos),fixed=T)) # name of loci
      loci.ColNames <- gsub(".1","",colnames(genos),fixed=T) # column names
      nloci <- as.numeric(length(loci)) # number of loci
      SetName <- paste('Set',k,sep="")

      if(HLA==T) { genos[genos=='^'] <- "00:00" }

      if(Output) {

        OutSetDir <- paste(OutDir,"/Set",k,sep="")
        dir.create(OutSetDir)
        setwd(OutSetDir)

        Params.set <- list(Set = paste("Set",k),
                           Loci.Run = paste(loci,collapse=",")
                           )

        Params.set <- do.call(rbind,Params.set)
        write.table(Params.set,file="Set_Parameters.txt",sep=": ", row.names=T, col.names=F, quote=F)
      }

      SAFE <- c(ls(),"SAFE")

# ===================================================================================================================================== ####
# Haplotype Analysis 'H' ______________________________________________________________________________________________________________ ####

      if ("H" %in% Run) {

        #cat(paste(rep("_",50),collapse="","\n"))

        # Sanity check for set length and All.Pairwise=T
        if (nloci<2) {
          Err.Log(Output,"Loci.No")
          stop("Analysis Stopped.", call. = F)
        } else if (All.Pairwise & nloci<=2)  {
          Err.Log(Output,"Loci.No.AP")
          stop("Analysis Stopped.", call. = F) }

        Haps.list <- H.MC.wrapper(SID,Tabsub,loci,loci.ColNames,genos,grp,All.Pairwise,Strict.Bin,Output,Verbose,Cores)

        if(All.Pairwise) {
          if(length(BD.out[['H']])>0) { BD.out[['H']] <- c(BD.out[['H']],Haps.list) } else { BD.out[['H']] <- Haps.list }
        } else {
          BD.out[['H']][[SetName]] <- Haps.list
        }

        rm(list=ls()[!(ls() %in% SAFE)])

      } #END HAPLOTYPE

# ===================================================================================================================================== ####
# Locus Level 'L' _____________________________________________________________________________________________________________________ ####

      if ("L" %in% Run) {

        #cat(paste(rep("_",50),collapse=""))

        L.list <- L.wrapper(nloci,loci,loci.ColNames,genos,grp,Strict.Bin,Output,Verbose)
        BD.out[['L']][[SetName]] <- list(binned=L.list[['AB']],
                                         freq=L.list[['AF']],
                                         OR=L.list[['OR']],
                                         chisq=L.list[['CS']],
                                         table=L.list[['FB']])

        rm(list=ls()[!(ls() %in% SAFE)])

      } #END LOCUS

# ===================================================================================================================================== ####
# Amino Acid Level 'A' ________________________________________________________________________________________________________________ ####

      if(HLA) {
        if ("A" %in% Run) {

          #cat(paste(rep("_",50),collapse=""))

          if(UPL.flag) { cat("Using updated protein exon alignments for amino acid analysis.\n") }

          A.list <- A.wrapper(loci,loci.ColNames,genos,grp,Exon,EPL,Cores,Strict.Bin,Output,Verbose)

          if(Output) {
            ## write to file
            write.table(Release, file = "Set_Parameters.txt", sep="\t", row.names = F, col.names=F, quote = F, append=T)
          }

          BD.out[['A']][[SetName]] <- list(log=A.list[['AL']],
                                           binned=A.list[['AB']],
                                           freq=A.list[['AF']],
                                           OR=A.list[['OR']],
                                           chisq=A.list[['CS']],
                                           table=A.list[['FB']])

          rm(list=ls()[!(ls() %in% SAFE)])

        } #END AMINO ACID
      }#END if(HLA)

# ===================================================================================================================================== ####
# End Analyses ________________________________________________________________________________________________________________________ ####

    }; rm(k)

 }# END SET LOOP

  if(Output) {

    if(Merge.Output) {

      cat("\nMerging data files ...\n")
      if("HWE" %in% Run) { Run <- Run[-which(Run=="HWE")] }
      if( length(Run)>=1 ) { MergeData_Output(BD.out,Run,OutDir) }

    }

  }

# ===================================================================================================================================== ####
  cat("\n>>>>>>>>>>>>>>>>>>>>>>>>>> End Analysis <<<<<<<<<<<<<<<<<<<<<<<<<<\n")


  if(Output) { setwd(OutDir); save(BD.out, file="Analysis.RData") }
  options(warn=0)

  if(Return) { return(BD.out) }

}# END FUNCTION
