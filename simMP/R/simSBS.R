
####### main #############
simSBS <- function(nSigs = NULL, nGenomes = NULL, refGenome = NULL,
                   similarity = 0.6, noise = 0,
                   presetSigs = NULL, chrs = NULL, nMutPerGenome = NULL,
                   sigPrevalence = NULL, chrDistribution = NULL, parallel = TRUE, saveDir = './') {

  # for temp file name
  tempStamp = as.character(unclass(Sys.time()))
  dir.create(file.path(saveDir, tempStamp))
  if (file.exists(file.path(saveDir, 'parallel.log'))) {
    unlink(file.path(saveDir, 'parallel.log'))
  }

  ####### ckeck arguments
  if (is.null(nSigs)) {
    stop('Please set nSigs')
  }else if (nSigs < 2) {
    stop('The number of mutational processes is suggested to be >= 2.')
  }
  if (is.null(nGenomes)) {
    stop('Please set nGenomes')
  }
  if (is.null(refGenome)) {
    stop('Please set refGenome')
  }
  if (similarity < 0.4) {
    warning('Don\'t set the similarity too low. The simulation may fail.')
  }
  nBases = 3
  if (nBases == 3 || nBases == 5) {
    nMotifs = (4 ^ (nBases - 1)) * 6
  }else{
    stop('nBases should only be 3 or 5.')
  }

  if (noise < 0 || noise > 1) {
    stop("The proportion of noise should between 0 and 1 included.")
  }

  if (is.null(chrs)) {
    CHRS = paste('chr', c(1:22, 'X', 'Y'), sep = '')
  }else {
    CHRS = paste('chr', chrs, sep = '')
  }

  # num mutations each genome assigned
  if (is.null(nMutPerGenome)) {
    numAcrossGenomes = read.table('./data/mutDistriWGS.txt')
    numAcrossGenomes = c(t(numAcrossGenomes))
    nMutPerGenome = sample(numAcrossGenomes, nGenomes, replace = TRUE)
  }else if (length(nMutPerGenome) != nGenomes) {
    stop("The length of nMutPerGenome should equal nGenome.")
  }

  # mutational processes prevalence across tumors
  if (is.null(sigPrevalence)) {
    if (nSigs > 21) {
      stop("If known prevalences are used, total processes, i.e. nSigs, should be <= 21.")
    }
    # use 21 known sigs prevalence, doi:10.1038/nature12477
    sigPrevalence = c(0.724, 0.144, 0.099, 0.121, 0.144, 0.026,
                      0.05, 0.02, 0.006, 0.005, 0.006, 0.014, 0.022,
                      0.001, 0.005, 0.011, 0.018, 0.022, 0.002, 0.005,
                      0.003)
    sigPrevalence = sort(sigPrevalence, decreasing = TRUE)
  } else if (length(sigPrevalence) != nSigs) {
    stop('You are using custom prevalencies. It should be a numerical vector and its length should equal nSigs.')
  }

  # num of mutations across all chromosomes, chr1 to chrY
  if (is.null(chrDistribution)) {
    # calculate from num per Mb to chromosome distribution
    # based on length, such before having other reasonable info
    # (1, 2, 3, ..., X, Y), GRCh37/hg19
    chrSize = as.numeric(GenomeInfoDb::seqlengths(refGenome)[CHRS])
    # add name attributes, for indexing. Others will also inherate this attribute.
    names(chrSize) = CHRS
    chrDistribution = chrSize / sum(chrSize)
  }else if(sum(chrDistribution) != 1) {
    stop("Ivalid distribution of chromosomes. Sum of values of chrDistribution should be 1.")
  }else if(length(chrDistribution) != length(CHRS)) {
    stop("The length of chrDistribution doesn't equal the length of chrs.")
  }

  if (!dir.exists(file.path(getwd(), saveDir))) {
    stop("Output directory ", saveDir, " doesn't exist. Please make sure a directory relative to the current working directory is given.")
  }

  # generate signatures
  MOTIFS = constructMotifs()
  if (is.null(presetSigs)) {
    simSigs = getSimSigs(nSigs, similarity, nMotifs)

    write.table(simSigs, file.path(saveDir, 'res_processes.txt'),
                quote = FALSE, sep = '\t', row.names = MOTIFS[, 1], col.names = paste('process', seq(nSigs), sep = '_'))
  }else{
    if (nSigs != ncol(presetSigs)){
      stop('presetSigs is given, nSigs should equal the number of processes in presetSigs.')
    }
    simSigs = presetSigs
  }


  ######## end checking input arguments


  # affected genomes of each sig/process
  targetGenomes = round(sigPrevalence[1:nSigs] * nGenomes)
  targetGenomes = as.list(targetGenomes)
  ## indexes of affected genomes of each sig/process
  targetGenomes = lapply(targetGenomes, function(j) {sample(nGenomes, j, replace = FALSE)})

  # create simulated genomes one by one
  ## parallel if possible
  #library(doParallel)
  if (parallel) {
    nCPUcores = detectCores()
    if (nCPUcores < 3) {
      registerDoSEQ()
    }else{
      cl = makeCluster(nCPUcores - 1)
      registerDoParallel(cl)
    }
  }
  g = NULL
  resParallel = foreach(g = 1:nGenomes, .packages = c(refGenome@pkgname)) %dopar% {
    mutCount = matrix(0, nMotifs, 1)
    rownames(mutCount) = MOTIFS[, 1]
    sigsContribution = matrix(0, nSigs, 1)

    cat("Simulating genome", g, '...\n', file = file.path(saveDir, 'parallel.log'), append = TRUE)
    totalMut = nMutPerGenome[g]
    operativeSigIdx = sapply(targetGenomes, function(x, ig) ifelse(ig %in% x, TRUE, FALSE), g)
    operativeSigIdx = seq(nSigs)[operativeSigIdx]
    if (length(operativeSigIdx) > 0) {
      sigExposure = runif(length(operativeSigIdx))
      sigExposure = round(totalMut * (sigExposure / sum(sigExposure)))
      sigsContribution[operativeSigIdx, 1] = sigExposure
      # iterate all sigs in the genome
      for(sigIdxIdx in 1:length(operativeSigIdx)) {
        theSig = simSigs[, operativeSigIdx[sigIdxIdx]]
        numPerChr = round(sigExposure[sigIdxIdx] * chrDistribution)
        # average noise of the genome to each signature
        noisePerChr = round(nMutPerGenome[g] * noise / length(sigExposure) * chrDistribution)
        # the sig impacts all chromosomes
        for (iChr in CHRS) {
          # no mutation in this genome throws error when make GRanges
          mutThisChr = numPerChr[iChr] + noisePerChr[iChr]
          if (mutThisChr < 1) {
            next()
          }
          # beware of chr boundary
          mutPos = sample(chrSize[iChr] - 1, mutThisChr)
          mutPos = ifelse(mutPos > 1, mutPos, 2)
          # reference contexts
          infoGR = GenomicRanges::GRanges(
            seqnames = iChr,
            ranges = IRanges::IRanges(start = mutPos, width = 1)
          )
          ctxRanges = GenomicRanges::resize(infoGR, 3, fix = 'center')
          refCtx = BSgenome::getSeq(refGenome, ctxRanges)
          # unify to C and T
          if (TRUE) {
            unifyIdx = which(as.character(XVector::subseq(refCtx, start = 2, end = 2)) %in% c("A", "G"))
            refCtx[unifyIdx] = Biostrings::reverseComplement(refCtx[unifyIdx])
          }
          # start mutating
          for (mii in 1:length(refCtx)) {
            idx = which(MOTIFS[, 2] == as.character(refCtx[mii]))
            while(length(idx) == 0) {
              # ref sequences like NNN, N.., ..N, .N., cannot mutate, re-sample
              reGetMutPos = sample(chrSize[iChr] - 1, 1)
              mutPos[mii] = ifelse(reGetMutPos > 1, reGetMutPos, 2)
              reGetRef = BSgenome::getSeq(refGenome, iChr,
                                          start = reGetMutPos - 1,
                                          width = 3)
              # unify to C and T
              if (TRUE) {
                if (as.character(XVector::subseq(reGetRef, start = 2, end = 2)) %in% c("A", "G")) {
                  reGetRef = Biostrings::reverseComplement(reGetRef)
                }
              }
              idx = which(MOTIFS[, 2] == as.character(reGetRef))
            }
            if (mii > numPerChr[iChr]) {
              # this is noise on this chr of this genome
              ## random mutation
              mutTo = sample(MOTIFS[idx, 1], 1)
            }else{
              mutTo = sample(MOTIFS[idx, 1], 1, replace = TRUE, prob = (theSig[idx] / sum(theSig[idx])))
            }
            # construct final mutation count matrix gradually
            mutCount[mutTo, 1] = mutCount[mutTo, 1] + 1
            #refCtx[mii] = Biostrings::DNAStringSet('TTT')
            # save tsv mutation position file
            cat(paste('genome', g, sep = '_'), '\t',
                iChr, '\t', mutPos[mii], '\t', mutPos[mii], '\t',
                paste(strsplit(mutTo, '')[[1]][c(3, 5)], collapse = '\t'), '\n',
                file = file.path(saveDir, tempStamp, paste('genome_', g, '.tmp', sep = '')),
                sep = '', append = TRUE)
          }
        }
      }
    }
    return(list(
      mutCount,
      sigsContribution
    ))
  }
  ## end parallele
  if (parallel) {
    if (getDoParName() != 'doSEQ') {
      registerDoSEQ()
      stopCluster(cl)
    }
  }
  ## collect parallel result
  counts = matrix(0, nMotifs, nGenomes)
  contributions = matrix(0, nSigs, nGenomes)
  for (gg in 1:nGenomes) {
    counts[, gg] = resParallel[[gg]][[1]]
    contributions[, gg] = resParallel[[gg]][[2]]
  }
  # clean up
  write.table(counts, file.path(saveDir, 'res_mutationCounts.txt'),
              sep = '\t', quote = FALSE, row.names = MOTIFS[, 1],
              col.names = paste('genome', seq(nGenomes), sep = '_'))
  write.table(contributions, file.path(saveDir, 'res_processContribution.txt'),
              sep = '\t', quote = FALSE, row.names = paste('process', seq(nSigs), sep = '_'),
              col.names = paste('genome', seq(nGenomes), sep = '_'))
  tempDirFiles = list.files(path = file.path(saveDir, tempStamp), full.names = TRUE)
  allMutations = do.call("rbind",lapply(tempDirFiles,
                                        FUN = function(j){read.table(j, sep="\t", stringsAsFactors = FALSE)}))
  write.table(allMutations, file.path(saveDir, 'res_mutation.tsv'), sep = '\t', quote = FALSE,
              row.names = FALSE, col.names = c('genome', 'chr', 'start', 'end', 'ref', 'alt'))
  unlink(file.path(saveDir, tempStamp), recursive = TRUE)
  # Since all result has been written to files, the returned value is useless
  #return(list(
   # M = counts,
    #E = contributions
  #))
  return(1)
}

###### other useful functions

# generate simulated mutational signatures
getSimSigs <- function(nSigs, similarity, nMotifs) {
  # W made with gamma distribution, lamda = 1:nSigs
  distInSigs = matrix(0, nSigs, nSigs)
  # dist in signs must > 0.6, thus similarity in sigs < 0.4
  while (min(distInSigs[lower.tri(distInSigs)]) <= 1 - similarity) {
    W = matrix(0, nMotifs, nSigs)
    for (i in 1:nSigs) {
      W[, i] = rgamma(nMotifs, shape = 1/i)
    }
    W = t(t(W) / colSums(W))
    distInSigs = squareform(pdist(t(W)), ncol(W))
  }
  return(W)
}


# pairwise distance between pairs of objects (rows) in m-by-n data matrix X
pdist <- function(X, distance = NULL) {
  # X is m-by-n
  m = nrow(X)
  dst = m * (m-1) / 2
  d = 1
  for (i in 1:(m - 1)) {
    for (j in (i+1):m) {
      y1 = X[i, ]
      y2 = X[j, ]
      if (is.null(distance)) {
        similarity = sum(pmin(y1, y2)) / sum(pmax(y1, y2)) # generalized jaccrd similarity
      }else{
        similarity = y1 %*% y2 / sqrt(y1 %*% y1 * y2 %*% y2) # cos(theta) similarity between y1 and y2
      }
      dist_y1y2 = 1 - similarity # dissimilarity is termed distance here
      dst[d] = dist_y1y2
      d = d + 1
    }
  }
  return(dst)
}

# square, symmetric matrix, from vector
squareform <- function(x, dimMatrix) {
  symMatrix = diag(0, dimMatrix, dimMatrix)
  symMatrix[lower.tri(symMatrix)] = x
  symMatrix[upper.tri(symMatrix)] = t(symMatrix)[upper.tri(symMatrix)]
  return(symMatrix)
}

# make trinucleotides
constructMotifs <- function(k = 3) {
  ref = alt = NULL
  alteration = expand.grid(ref = Biostrings::DNA_BASES, alt = Biostrings::DNA_BASES)
  alteration = subset(alteration, ref != alt & ref %in% c("C", "T"))
  alteration1 = sort(paste(alteration$ref, alteration$alt, sep = ">"))
  alteration2 = sort(as.character(alteration$ref))
  if (k == 3) {
    ### p is 5', s is 3'
    motifs1 = expand.grid(s = Biostrings::DNA_BASES, p = Biostrings::DNA_BASES, a = alteration1)
    motifs1 = sprintf("%s[%s]%s", motifs1$p, motifs1$a, motifs1$s)
    motifs2 = expand.grid(s = Biostrings::DNA_BASES, p = Biostrings::DNA_BASES, a = alteration2)
    motifs2 = sprintf("%s%s%s", motifs2$p, motifs2$a, motifs2$s)
  }
  return(data.frame(motifs1, motifs2,
                    mut = sort(rep(c('C>A', 'C>G', 'C>T', 'T>A', 'T>C', 'T>G'), 16)),
                    stringsAsFactors = FALSE))
}

