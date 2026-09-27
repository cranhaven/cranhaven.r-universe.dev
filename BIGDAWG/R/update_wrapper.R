#' Update function for protein aligment upon new IMGT HLA data release
#'
#' This updates the protein aligment used in checking HLA loci and alleles as well as in the amino acid analysis.
#' @param Restore Logical specifying if the original alignment file be restored.
#' @param Force Logical specifiying if update should be forced.
#' @param Output Logical indicating if error reporting should be written to file.
UpdateRelease <- function(Force=F,Restore=F,Output=F) {


  if( !inherits(try(XML::readHTMLTable("http://cran.r-project.org/web/packages/BIGDAWG/index.html",header=F),silent=T),"try-error") ) {

    MainDir <- getwd()
    on.exit(setwd(MainDir), add = TRUE)

    getDir <- path.package('BIGDAWG')
    putDir <- paste(getDir,"/data",sep="")
    if(!dir.exists(putDir)) { dir.create(putDir) }

    if(!Restore) {

      #Check current version against BIGDAWG version
      if(!Force) {

        setwd(putDir)

        # Get IMGT Release Version
        invisible(download.file("ftp://ftp.ebi.ac.uk/pub/databases/ipd/imgt/hla/version_report.txt",destfile="release_version.txt",method="libcurl"))
        Release <- read.table("version_report.txt",comment.char="",sep="\t")
        RV.current <- Release[2]
        file.remove("version_report.txt")

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

        cat("Versions:\n","IMGT/HLA current: ",RV.current,"\n BIGDAWG version: ",RV.BIGDAWG,"\n")
        if(grepl(RV.current,RV.BIGDAWG)) { Flag <- T } else { Flag <- F }

      } else {

        Flag <- F

      }# End if() for setting Flag

      #Run Update if Flag = T
      if(Flag) {

        cat("\nYour database seems up to date. Use Force = T to force the update.")

      } else {

        # For creating UpdatePtnAlign.RData object
        # Define download directory
          setwd(putDir)
          Safe <- dir()
          Safe <- c(Safe[!grepl(".txt",Safe)],"UpdatePtnAlign.RData")

        #STEP 1: Define Loci and Read in Reference Exon Map Files
          Loci <- c("A","B","C","DPA1","DPB1","DQA1","DQB1","DRB1","DRB3","DRB4","DRB5")

          #Currently DRB1, DRB3, DRB4, DRB5 aligments in single file
          #Remove if split into individual files
          Loci.get <- c("A","B","C","DPA1","DPB1","DQA1","DQB1","DRB")

          #Exon Info
          RefTab <- BIGDAWG::ExonPtnList$RefExons

        #STEP 2: Download protein alignments and other ancillary files
          cat("Updating reference object for the amino acid analysis.\n")
          cat("Downloading alignment files from the IMGT/HLA.\n")
          GetFiles(Loci.get)
          Release <- read.table('Release.txt',sep="\t") # created during GetFiles download

        #STEP 3: Format alignments for exons of interest
          cat("Formatting alignment files.\n")
          for(i in 1:length(Loci)) { Locus <- Loci[i] ; ExonPtnAlign.Create(Locus,RefTab) }

        #STEP 4: Create ExonPtnAlign list object for BIGDAWG package
          AlignObj.Update(Loci,Release,RefTab)

        #STEP 5: Clean up
          cat("Cleaning up.\n")
          invisible(file.remove(dir()[which(dir() %in% Safe!=T)]))

          cat("Updated.\n")
      }

    } else if (Restore) {

      setwd(putDir)
      if(!file.exists('UpdatePtnAlign.RData')) { stop("No prior update to restore.", call.= F) }
      cat("Restoring original alignment reference object for amino acid analysis.\n")
      invisible(file.remove('UpdatePtnAlign.RData'))
      cat("Restored.\n")

    }

  } else {

    Err.Log(Output,"No.Internet")
    stop("Update stopped.",call.=F)

  }

}
