
# Jason Sinnwell
# Mayo Division of Biostatistics
# 3/2007
  
# make a function that takes a single haplotype block simulated from snap that
# 1) compares phased haplotype frequencies from snap to those estimated from haplo.em
# 2) compare posterior probabilities estimated from haplo.em to the actual phased haplotype pair

# As input, take one block of phased haplotypes, which is one column from SNaP that looks like:
#                                    V1 
# 1  2,1,2,2,1,1,1,2.1,2,1,1,1,1,2,2,2,1 
# 2  1,2,2,1,2,2,1,1.1,1,2,1,2,2,1,2,2,1 
# 3  1,1,1,2,1,1,1,2.2,2,2,1,2,2,1,2,2,1


checkPhase <- function(snapfile, blocknum=1, miss.val=c(0,NA), em.control=haplo.em.control()) {
  
  ## Splus:genetics and R:haplo.stats required for haplo.em
  ## slocal and rlocal required for strsplit()
   dat <- read.table(snapfile, sep=" ", stringsAsFactors=FALSE)
   hap1 <- hap2 <- charhap1 <- charhap2 <- NULL
   nsubj <- nrow(dat)

   for(k in 1:nsubj) {
     blockhap1 <- strsplit(dat[k,1],split="\\.")[[1]][blocknum]
     blockhap2 <-strsplit(dat[k,2],split="\\.")[[1]][blocknum]
     
     hapvec1 <- as.numeric(strsplit(blockhap1, split=',')[[1]])
     hapvec2 <- as.numeric(strsplit(blockhap2, split=',')[[1]])
     hap1 <- rbind(hap1, hapvec1)
     hap2 <- rbind(hap2, hapvec2)
     charhap1 <- c(charhap1, paste(hapvec1,collapse=""))
     charhap2 <- c(charhap2, paste(hapvec2,collapse=""))
   }
   nloc <- ncol(hap1)
   hapgeno <- cbind(hap1,hap2)
   col.indx <- as.vector(matrix(c(1:(2*nloc)), nrow=2,byrow=TRUE))
   geno <- hapgeno[,col.indx]

   # run haplo.em, estimated hap freq
   em.block <- haplo.em(geno, control=em.control, miss.val=miss.val)

   # construct data.frame of hap-pairs estimated by haplo.em for each
   # subject, with post prob of hap-pair, merge with snap haplotype pairs
   hap1sorted <- ifelse(em.block$hap1code < em.block$hap2code, em.block$hap1code, em.block$hap2code)
   hap2sorted <- ifelse(em.block$hap1code < em.block$hap2code, em.block$hap2code, em.block$hap1code)
   emhaps <- data.frame(subj=em.block$subj.id,
                       emhap1=apply(em.block$haplotype[hap1sorted,],1, paste, collapse=""),
                       emhap2=apply(em.block$haplotype[hap2sorted,], 1, paste, collapse=""),
                       post=em.block$post)

   snaphaps <- data.frame(subj=1:nsubj, snaphap1=charhap1, snaphap2=charhap2)

   merge.em.snap <- merge(emhaps, snaphaps, all.x=TRUE, by="subj")

  # compute snap hap freqs, join with freqs estimated from haplo.em
   haps.freq <- table(c(charhap1, charhap2))/(2*nsubj)
   
   em.charhaps <- apply(em.block$haplotype, 1, paste, collapse="")
   indx.snap.to.em <- match(names(haps.freq),em.charhaps)
   freq.frame <- data.frame(haplotype=em.charhaps, em.freq=em.block$hap.prob, snap.freq=0)
   freq.frame$snap.freq[indx.snap.to.em] <- haps.freq

   return(list(merge.em.snap, freq.frame))
   
}


# simulate an effect based on a person's haplotypes
# simulate an additive, dominant, or recessive effect

simHapEff <- function(snapfile, blocknum=1, causehap,
                      haplo.effect="additive", pen=.5, spurious=.01) {

  effcode=match(haplo.effect, c("additive", "dominant", "recessive"))  
  
  dat <- read.table(snapfile, sep=" ",stringsAsFactors=FALSE)
  hap1 <- hap2 <- charhap1 <- charhap2 <- NULL
  nsubj <- nrow(dat)
  
  for(k in 1:nsubj) {

    blockhap1 <- strsplit(dat[k,1],split="\\.")[[1]][blocknum]
    blockhap2 <-strsplit(dat[k,2],split="\\.")[[1]][blocknum]
    
    hapvec1 <- as.numeric(strsplit(blockhap1, split=',')[[1]])
    hapvec2 <- as.numeric(strsplit(blockhap2, split=',')[[1]]) 

    charhap1 <- c(charhap1, paste(hapvec1,collapse=""))
    charhap2 <- c(charhap2, paste(hapvec2,collapse=""))
  }

  if(nchar(causehap) != nchar(charhap1[1])) stop("causehap not correct length")

  count.causehap <- 1*(charhap1==causehap) + 1*(charhap2==causehap)

  bin.resp <- ifelse(count.causehap==0,
                     1*(runif(nsubj) < spurious),
                     ifelse(count.causehap==1,
                            1*(runif(nsubj) < if(effcode<3) pen else spurious),
                            if(effcode==1) 1*((runif(nsubj) < pen) | (runif(nsubj) < pen)) else 1*(runif(nsubj) < pen)))

  return(bin.resp)
}


simLocEff <- function(snapfile, blocknum, locnum, allele, effect="additive", pen=.5, spur=.01) {

  effcode=match(haplo.effect, c("additive", "dominant", "recessive"))  
  
  dat <- read.table(snapfile, sep=" ", stringsAsFactors=FALSE)
  hap1 <- hap2 <- allele1 <- allele2 <- NULL
  nsubj <- nrow(dat.in)
  
  for(k in 1:nsubj) {  
     
    blockhap1 <- strsplit(dat[k,1],split="\\.")[[1]][blocknum]
    blockhap2 <- strsplit(dat[k,2],split="\\.")[[1]][blocknum]
    hapvec1 <- as.numeric(strsplit(blockhap1, split=',')[[1]])
    hapvec2 <- as.numeric(strsplit(blockhap2, split=',')[[1]])

    allele1 <- c(allele1, paste(hapvec1,collapse=""))
    allele2 <- c(allele2, paste(hapvec2,collapse=""))
  }

  count.allele <- 1*(allele1==allele) + 1*(allele2==allele)

  bin.resp <- ifelse(count.allele==0,
                     1*(runif(nsubj) < spurious),
                     ifelse(count.allele==1,
                            1*(runif(nsubj) < if(effcode<3) pen else spurious),
                            if(effcode==1) 1*((runif(nsubj) < pen) | (runif(nsubj) < pen)) else 1*(runif(nsubj) < pen)))
  
  return(bin.resp)
}


simLoc2Eff <- function(snapfile, locnum, effect="additive", pen=.5, spur=.01) {

  return(affected)
}

