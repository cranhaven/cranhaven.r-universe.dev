#' Haplotype Wrapper for Multicore
#'
#' Wrapper for main H function
#' @param SID Character vector of subject IDs.
#' @param Tabsub Data frame of genotype calls for set being analyzed.
#' @param loci Character vector of unique loci being analyzed.
#' @param loci.ColNames Character vector of genos column names.
#' @param genos The genotype columns of the loci set being analyzed.
#' @param grp Case/Control or Phenotype groupings.
#' @param All.Pairwise Haplotype argument carryover from main BIGDAWG function
#' @param Strict.Bin Logical specify if strict rare cell binning should be used in ChiSq test
#' @param Output Data return carryover from main BIGDAWG function
#' @param Verbose Summary display carryover from main BIGDAWG function
#' @param Cores Cores carryover from main BIGDAWG function
#' @note This function is for internal BIGDAWG use only.
H.MC.wrapper <- function(SID,Tabsub,loci,loci.ColNames,genos,grp,All.Pairwise,Strict.Bin,Output,Verbose,Cores) {

  cat(">>>> STARTING HAPLOTYPE ANALYSIS...","\n")

  # Define Pairwise Combinations to Run When Selected
  if(All.Pairwise) {

    # Define Combinations
    Combos <- t(combn(length(loci),2))
    Combos <- lapply(seq_len(nrow(Combos)),FUN=function(x) Combos[x,])

    cat("\nYou have opted to run all pairwise combinations for the haplotype analysis.\n")
    cat("There are", length(Combos), "possible locus combinations to run.\n" )

    # Define Pairwise Sets
    HAPsets <- parallel::mclapply(Combos,FUN=buildHAPsets,genos=genos,loci=loci,loci.ColNames=loci.ColNames,mc.cores=Cores)
    HAPnames <- unlist(parallel::mclapply(Combos,FUN=buildHAPnames,loci=loci,mc.cores=Cores))
    names(HAPsets) <- HAPnames

  } else {

    HAPsets <- list()
    HAPsets[[paste(loci,collapse='~')]] <- genos

  }

  # Run H
  #Start <- Sys.time()
  H.list <- parallel::mclapply(HAPsets,H.MC,grp=grp,Strict.Bin=Strict.Bin,Verbose=Verbose,mc.cores=Cores)
  #Sys.time() - Start

  if(All.Pairwise) {

    #chisq
    tmp.cs <- parallel::mclapply(H.list,"[[","chisq",mc.cores=Cores)
    tmp.cs <- do.call(rbind,tmp.cs)
    tmp.cs <- cbind(rownames(tmp.cs),tmp.cs)
    colnames(tmp.cs)[1] <- "Pairwise.Loci"
    rownames(tmp.cs) <- NULL

  }

  if(Output) {

      # Gathering List Elements
      H.out <- list()

      if(All.Pairwise) {

        #Frequencies
          tmp <- parallel::mclapply(H.list,"[[","freq",mc.cores=Cores)
          nrow.tmp <- lapply(tmp,nrow)
          haps.tmp <- rep(names(nrow.tmp),nrow.tmp)
          tmp <- cbind(haps.tmp,do.call(rbind,tmp))
          colnames(tmp)[1:2] <- c("Pairwise.Loci","Haplotype")
          H.out[['freq']] <- tmp

        #binned
          tmp <- parallel::mclapply(H.list,"[[","binned",mc.cores=Cores)
          nrow.tmp <- lapply(tmp,nrow)
          haps.tmp <- rep(names(nrow.tmp),nrow.tmp)
          tmp <- cbind(haps.tmp,do.call(rbind,tmp))
          colnames(tmp)[1:2] <- c("Pairwise.Loci","Haplotype")
          H.out[['binned']] <- tmp

        #OR
          tmp <- parallel::mclapply(H.list,"[[","OR",mc.cores=Cores)
          nrow.tmp <- lapply(tmp,nrow)
          haps.tmp <- rep(names(nrow.tmp),nrow.tmp)
          tmp <- cbind(haps.tmp,do.call(rbind,tmp))
          colnames(tmp)[1:2] <- c("Pairwise.Loci","Haplotype")
          H.out[['OR']] <- tmp

        #chisq
          H.out[['chisq']] <- tmp.cs

        #table
          tmp <- parallel::mclapply(H.list,"[[","table",mc.cores=Cores)
          nrow.tmp <- lapply(tmp,nrow)
          haps.tmp <- rep(names(nrow.tmp),nrow.tmp)
          tmp <- cbind(haps.tmp,do.call(rbind,tmp))
          colnames(tmp)[1:2] <- c("Pairwise.Loci","Haplotype")
          H.out[['table']] <- tmp

        #Haplotypes
          tmp <- parallel::mclapply(H.list,"[[","Haplotypes",mc.cores=Cores)
          nrow.tmp <- lapply(tmp,nrow)
          haps.tmp <- rep(names(nrow.tmp),nrow.tmp)
          tmp <- cbind(SID,haps.tmp,do.call(rbind,tmp))
          colnames(tmp) <- c("SAMPLE.ID","Pairwise.Loci","Haplotype.1","Haplotype.2")
          H.out[['Haplotypes']] <- tmp

      } else {

        #Frequencies-binned-OR-chisq-table
          H.out[['freq']] <- H.list[[1]][['freq']]
          H.out[['binned']] <- H.list[[1]][['binned']]
          H.out[['OR']] <- H.list[[1]][['OR']]
          H.out[['chisq']] <- H.list[[1]][['chisq']]
          H.out[['table']] <- H.list[[1]][['table']]

        #Haplotyeps
          tmp <- H.list[[1]][['Haplotypes']]
          tmp <- cbind(SID,tmp)
          colnames(tmp)[1] <- "SAMPLE.ID"
          H.out[['Haplotypes']] <- tmp

      }


      ## write to file
      write.table(H.out[['freq']], "haplotype_freqs.txt", sep="\t", quote = F, row.names=F, col.names=T)
      write.table(H.out[['binned']], "haplotype_binned.txt", sep="\t", quote = F, row.names=F, col.names=T)
      write.table(H.out[['OR']], "haplotype_OR.txt", sep="\t", quote = F, row.names=F, col.names=T)
      write.table(H.out[['chisq']], "haplotype_chisq.txt", sep="\t", row.names = F, quote = F)
      write.table(H.out[['table']], "haplotype_table.txt", sep="\t", row.names = F, quote = F)
      write.table(H.out[['Haplotypes']], "haplotype_HapsbySubject.txt", sep="\t", row.names = F, quote = F)

  }

  if(All.Pairwise) {

    PairSetName <- paste("Pairwise.Set",seq_len(length(H.list)),sep="")

    # Hapset Legend File
    if(Output) {
      tmp <- cbind(PairSetName,names(HAPsets))
      colnames(tmp) <- c("Set_Name","Loci")
      write.table(tmp,file="haplotype_PairwiseSets.txt",sep="\t",quote=F,col.names=T,row.names=F)
    }

    names(H.list) <- PairSetName

  } else { H.list <- H.list[[1]] }

  cat("> HAPLOTYPE ANALYSIS COMPLETED","\n")
    if(Verbose) {
      if(All.Pairwise) { overall.chisq <- tmp.cs } else { overall.chisq <- H.list[['chisq']] }
      print(as.data.frame(overall.chisq), row.names=F)
      cat("\n")
    }

  return(H.list)

}
