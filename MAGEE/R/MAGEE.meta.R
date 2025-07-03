MAGEE.meta <- function(meta.files.prefix, n.files = rep(1, length(meta.files.prefix)), cohort.group.idx = NULL, group.file, group.file.sep = "\t", E.match=NULL, MAF.range = c(1e-7, 0.5), MAF.weights.beta = c(1, 25), miss.cutoff = 1, method = "davies", tests = "JF", use.minor.allele = FALSE)
{
  if(.Platform$endian!="little") stop("Error: platform must be little endian.")
  n.cohort <- length(meta.files.prefix)
  if(length(n.files) != n.cohort) stop("Error: numbers of cohorts specified in meta.files.prefix and n.files do not match.")
  if(!is.null(cohort.group.idx)) {
    if(length(cohort.group.idx) != n.cohort) stop("Error: numbers of cohorts specified in meta.files.prefix and cohort.group.idx do not match.")
    cohort.group.idx <- as.numeric(factor(cohort.group.idx))
    n.cohort.groups <- length(unique(cohort.group.idx))
  }
  if(any(!tests %in% c("MV", "MF", "IV", "IF", "JV", "JF", "JD"))) stop("Error: \"tests\" should only include \"MV\" for the main effect variance component test, \"MF\" for the main effect combined test of the burden and variance component tests using Fisher\'s method, \"IV\" for the interaction variance component test, \"IF\" for the interaction combined test of the burden and variance component tests using Fisher\'s method, \"JV\" for the joint variance component test for main effect and interaction, \"JF\" for the joint combined test of the burden and variance component tests for main effect and interaction using Fisher\'s method, or \"JD\" for the joint combined test of the burden and variance component tests for main effect and interaction using double Fisher\'s method.")
  MV <- "MV" %in% tests
  MF <- "MF" %in% tests
  IV <- "IV" %in% tests
  IF <- "IF" %in% tests
  JV <- "JV" %in% tests
  JF <- "JF" %in% tests
  JD <- "JD" %in% tests
  group.info <- try(fread(group.file, header = FALSE, data.table = FALSE, col.names = c("group", "chr", "pos", "ref", "alt", "weight"), colClasses = c("character","character","integer","character","character","numeric"), sep = group.file.sep), silent = TRUE)
  if (inherits(group.info, "try-error")) {
    stop("Error: cannot read group.file!")
  }
  group.info <- group.info[!duplicated(paste(group.info$group, group.info$chr, group.info$pos, group.info$ref, group.info$alt, sep = ":")), ]
  groups <- unique(group.info$group)
  n.groups <- length(groups)
  group.info$group.idx <- as.numeric(factor(group.info$group))
  group.info <- group.info[order(group.info$group.idx, group.info$chr, group.info$pos), ]
  group.idx.end <- findInterval(1:n.groups, group.info$group.idx)
  group.idx.start <- c(1, group.idx.end[-n.groups] + 1)
  variant.id <- paste(group.info$group, group.info$chr, group.info$pos, group.info$ref, group.info$alt, sep = ":")
  group.idx.ends <- group.idx.starts <- scores <- cons <- vector("list", n.cohort)
  for(i in 1:n.cohort) { # Read the scores for each study from each core
    tmp.scores <- NULL
    for(j in 1:n.files[i]) { # n.files[i] is the number of cores for the i-th study
      tmp <- try(fread(paste0(meta.files.prefix[i], ".score.", j), header = TRUE, data.table = FALSE))
      if (inherits(tmp,"try-error")) {
        stop(paste0("Error: cannot read ", meta.files.prefix[i], ".score.", j, "!"))
      }
      tmp <- tmp[,c("group", "chr", "pos", "ref", "alt", "N", "missrate", "altfreq", "G.SCORE", paste("K.SCORE.", 1:(ncol(tmp)-9), sep=""))] # n.E = ncol(tmp)-9, number of environmental factor to test
      tmp$idx <- match(paste(tmp$group, tmp$chr, tmp$pos, tmp$ref, tmp$alt, sep = ":"), variant.id) # find the index of the variants from the score file in the group file
      if(any(is.na(tmp$idx))) { # if any variant from the score file is not in the group file, report error and stop the job
        tmp.dups <- which(is.na(tmp$idx))
        message("In ", paste0(meta.files.prefix[i], ".score.", j), ", the following variants were not present in group.file:")
        message("group: ", paste(tmp$group[tmp.dups], collapse = ", "))
        message("chr: ", paste(tmp$chr[tmp.dups], collapse = ", "))
        message("pos: ", paste(tmp$pos[tmp.dups], collapse = ", "))
        message("ref: ", paste(tmp$ref[tmp.dups], collapse = ", "))
        message("alt: ", paste(tmp$alt[tmp.dups], collapse = ", "))
        stop("Error: meta files possibly not generated using this group.file!")
      }
      tmp$file <- j
      tmp.scores <- rbind(tmp.scores, tmp) # rbind the scores for the same study from different cores
      rm(tmp)
    }
    if(any(sort(tmp.scores$idx)!=tmp.scores$idx)) { # for the same study, the score file from each core has to be from MAGEE() analysis using the same group file, the order of variants can't be mixed 
      message("In some ", paste(meta.files.prefix[i], collapse = ", "), " score files, the order of group and variants is not the same as in the group-sorted group.file.") # meta.files.prefix[i] shows which study has problem
      stop("Error: meta files possibly not generated using this group.file!")
    }
    group.idx.ends[[i]] <- findInterval(1:n.groups, group.info$group.idx[tmp.scores$idx]) # for different studies, the score files may have diffferent variants (some variants may be removed for some study in MAGEE() analysis)
    group.idx.starts[[i]] <- c(1, group.idx.ends[[i]][-n.groups] + 1) # the start and end point for each group from different study may vary
    scores[[i]] <- tmp.scores # save all the scores from study i in to a list 
    rm(tmp.scores)
    cons[[i]] <- file(paste0(meta.files.prefix[i], ".cov.1"), "rb")
  }
  n.variants <- rep(0,n.groups)
  if(MV | JV) MV.pval <- rep(NA, n.groups)
  if(IV | JV) IV.pval <- rep(NA, n.groups)
  if(JV) JV.pval <- rep(NA, n.groups)
  if(MF | JF | JD) MF.pval <- rep(NA, n.groups)
  if(IF | JF | JD) IF.pval <- rep(NA, n.groups)
  if(JF) JF.pval <- rep(NA, n.groups)
  if(JD) JD.pval <- rep(NA, n.groups)
  current.lines <- current.cons <- s.count <- rep(1, n.cohort)
  for(i in 1:n.groups) {
    tmp.idx <- group.idx.start[i]:group.idx.end[i]
    tmp.group.info <- group.info[tmp.idx, , drop = FALSE]
    U.list <- V.list <- KPG.list <- KPK.list <- vector("list", n.cohort)
    variant.indices <- tmp.N <- tmp.Nmiss <- tmp.AC <- c()
    for(j in 1:n.cohort) { # define scores and cov matrix for group i for each study 
      if(group.idx.starts[[j]][i] <= group.idx.ends[[j]][i]) { # the start and end point for group i in study j 
        tmp.n.p <- group.idx.ends[[j]][i]-group.idx.starts[[j]][i]+1
        U.list[[j]] <- scores[[j]][current.lines[j]:(current.lines[j]+tmp.n.p-1), , drop = FALSE] # the U.list has both scores for G & scores for K
        ### read n.E and interaction (names) and decide the binary file name (if there are multiple binary files for study j)
        if(all(U.list[[j]]$file==current.cons[j]+1)) { # for group i in study j, 
          close(cons[[j]]) # close the current binary file for study j
          current.cons[j] <- current.cons[j]+1
          cons[[j]] <- file(paste0(meta.files.prefix[j], ".cov.", current.cons[j]), "rb") # decide the next binary file name for study j
          n.E <- readBin(cons[[j]], what = "numeric", n = 1, size = 4) # There are n.E and interaction at the beginning for every binary file from different cores
          interaction <- readBin(cons[[j]], what = "character", n = n.E, size = 4)
        } else if(any(U.list[[j]]$file!=current.cons[j])) {
          message("For test group ", i, ", cohort ", j, "has incorrect indices for binary var files.")
          stop("Error: check your individual score files!")
        } else if (s.count[j] == 1) { # Read n.E and interaction only from the first group
          n.E <- readBin(cons[[j]], what = "numeric", n = 1, size = 4)
          interaction <- readBin(cons[[j]], what = "character", n = n.E, size = 4)
          s.count[j] <- s.count[j] +1
        }
        tmp.n.d <- tmp.n.p*(1+n.E) # = n.variants + n.variants * n.E
        tmp.COV <- matrix(0, tmp.n.d, tmp.n.d)
        tmp.COV[lower.tri(tmp.COV, diag = TRUE)] <- readBin(cons[[j]], what = "numeric", n = (1+tmp.n.d)*tmp.n.d/2, size = 4)
        tmp.COV[upper.tri(tmp.COV)] <- t(tmp.COV)[upper.tri(tmp.COV)]
        V.list[[j]] <- as.matrix(tmp.COV[1:tmp.n.p, 1:tmp.n.p])
        KPG.list[[j]] <- as.matrix(tmp.COV[(tmp.n.p+1):tmp.n.d, 1:tmp.n.p])
        KPK.list[[j]] <- as.matrix(tmp.COV[(tmp.n.p+1):tmp.n.d, (tmp.n.p+1):tmp.n.d])
        rm(tmp.COV)
        if (!is.null(E.match)) {
          odr<-rep(0, n.E)
          for (k in 1:n.E) {
            odr[k] <- which(interaction %in% E.match[[k]])
          }
          KPG.list[[j]] <- KPG.list[[j]][odr,]
          KPK.list[[j]] <- KPK.list[[j]][odr, odr]
        }
        current.lines[j] <- current.lines[j]+tmp.n.p
        variant.indices <- c(variant.indices, U.list[[j]]$idx)
        tmp.N <- c(tmp.N, U.list[[j]]$N)
        tmp.Nmiss <- c(tmp.Nmiss, U.list[[j]]$N * U.list[[j]]$missrate/(1-U.list[[j]]$missrate))
        tmp.AC <- c(tmp.AC, 2*U.list[[j]]$N*U.list[[j]]$altfreq)
      }
    }
    if(length(variant.indices) == 0) next
    tmp.variant.indices <- variant.indices
    variant.indices <- sort(unique(variant.indices))
    N <- sapply(variant.indices, function(x) sum(tmp.N[tmp.variant.indices==x])) # N is a vector for the number of counts for each variant, names(N) is the variant index
    Nmiss <- sapply(variant.indices, function(x) sum(tmp.Nmiss[tmp.variant.indices==x])) # Nmiss is a vector for the number of missed variant
    AF <- sapply(variant.indices, function(x) sum(tmp.AC[tmp.variant.indices==x]))/2/N 
    include <- (Nmiss/(N+Nmiss) <= miss.cutoff & ((AF >= MAF.range[1] & AF <= MAF.range[2]) | (AF >= 1-MAF.range[2] & AF <= 1-MAF.range[1])))
    rm(tmp.N, tmp.Nmiss, tmp.AC, tmp.variant.indices, N, Nmiss)
    if(sum(include) == 0) next
    variant.indices <- variant.indices[include]
    n.p <- length(variant.indices)
    n.variants[i] <- n.p
    U <- if(!is.null(cohort.group.idx)) rep(0, n.cohort.groups*n.p) else rep(0, n.p)
    SK <- if(!is.null(cohort.group.idx)) rep(0, n.cohort.groups*n.p*n.E) else rep(0, n.p*n.E)
    V <- if(!is.null(cohort.group.idx)) matrix(0, n.cohort.groups*n.p, n.cohort.groups*n.p) else matrix(0, n.p, n.p)
    KPG <- if(!is.null(cohort.group.idx)) matrix(0, n.cohort.groups*n.p*n.E, n.cohort.groups*n.p) else matrix(0, n.p*n.E, n.p)
    KPK <- if(!is.null(cohort.group.idx)) matrix(0, n.cohort.groups*n.p*n.E, n.cohort.groups*n.p*n.E) else matrix(0, n.p*n.E, n.p*n.E)
    for(j in 1:n.cohort) { # combine score and cov across studies
      if(!is.null(U.list[[j]]) & !is.null(V.list[[j]]) & !is.null(KPG.list[[j]]) & !is.null(KPK.list[[j]])) {
        IDX <- match(U.list[[j]]$idx, variant.indices) # index for genetic score in the bigger vector U and its cov in the bigger matrix V
        if(sum(!is.na(IDX)) == 0) next
        IDX2 <- which(!is.na(IDX)) # index for genetic score in the smaller vector U.list[[j]]$G.SCORE and its cov in the smaller matrix V.list[[j]]
        IDX3 <- unlist(sapply(1:n.E, function(x) IDX+(dim(U.list[[j]])[1]*(x-1)), simplify = FALSE)) # index for GEI score in the bigger vector SK its cov in the bigger matrix KPG & KPK
        IDX4 <- which(!is.na(IDX3)) # index for GEI score in the smaller vector for study j and its cov in the smaller matrix KPG.list[[j]] and KPK.list[[j]]
        IDX <- IDX[IDX2]
        IDX3 <- IDX3[IDX4]
        if(!is.null(cohort.group.idx)) { # IDX2 annd IDX4 for the smaller vector and matrix don't need to change
          IDX <- IDX+n.p*(cohort.group.idx[j]-1)
          IDX3 <- IDX3+n.p*n.E*(cohort.group.idx[j]-1)
        }
        U[IDX] <- U[IDX]+U.list[[j]]$G.SCORE[IDX2]
        SK[IDX3] <- SK[IDX3]+as.vector(as.matrix(U.list[[j]][, paste("K.SCORE.", 1:n.E, sep="")]))[IDX4]
        V[IDX, IDX] <- V[IDX,IDX]+V.list[[j]][IDX2,IDX2]
        KPG[IDX3,IDX] <- KPG[IDX3,IDX]+KPG.list[[j]][IDX4,IDX2]
        KPK[IDX3,IDX3] <- KPK[IDX3,IDX3]+KPK.list[[j]][IDX4,IDX4]
      }
    }
    tmp.weight <- tmp.group.info$weight[match(variant.indices, tmp.idx)]
    if(use.minor.allele) tmp.weight[AF[include] > 0.5] <- -tmp.weight[AF[include] > 0.5]
    tmp.weight <- tmp.weight * MAF.weights.beta.fun(AF[include], MAF.weights.beta[1], MAF.weights.beta[2])
    if(!is.null(cohort.group.idx)) tmp.weight <- rep(tmp.weight, n.cohort.groups)
    V_i <- MASS::ginv(V)
    if(IV | IF | JV | JF | JD) {
      IV.U <- SK-tcrossprod(tcrossprod(KPG,V_i),t(U))
      IV.V <- KPK - tcrossprod(tcrossprod(KPG,V_i),KPG)
    }
    U <- U*tmp.weight
    V <- t(V*tmp.weight)*tmp.weight
    if(IV | IF | JV | JF | JD) {
      IV.U <- IV.U*rep(tmp.weight, n.E)
      IV.V <- t(IV.V*rep(tmp.weight, n.E))*rep(tmp.weight, n.E)
    }
    if(MV | JV) MV.pval[i] <- tryCatch(.quad_pval(U = U, V = V, method = method), error = function(e) { NA })
    if(IV | JV) IV.pval[i] <- tryCatch(.quad_pval(U = IV.U, V = IV.V, method = method), error = function(e) { NA })
    if(JV) JV.pval[i] <- tryCatch(fisher_pval(c(MV.pval[i], IV.pval[i])), error = function(e) { MV.pval[i] })
    if(MF | JF | JD) {
      MF.BU <- sum(U)
      MF.BV <- sum(V)
      MF.Bp <- pchisq(MF.BU^2/MF.BV,df=1,lower.tail=FALSE)
      V.rowSums <- rowSums(V)
      MF.U <- U - V.rowSums * MF.BU / MF.BV
      MF.V <- V - tcrossprod(V.rowSums) / MF.BV
      if(MF.BV == 0 | mean(abs(MF.V)) < sqrt(.Machine$double.eps)) MF.p <- NA
      else MF.p <- tryCatch(.quad_pval(U = MF.U, V = MF.V, method = method), error = function(e) { NA })
      MF.pval[i] <- tryCatch(fisher_pval(c(MF.Bp, MF.p)), error = function(e) { MF.Bp })
    }
    if(IF | JF | JD) {
      IF.BU <- sum(IV.U)
      IF.BV <- sum(IV.V)
      IF.Bp <- pchisq(IF.BU^2/IF.BV,df=1,lower.tail=FALSE)
      IV.V.rowSums <- rowSums(IV.V)
      IF.U <- IV.U - IV.V.rowSums * IF.BU / IF.BV
      IF.V <- IV.V - tcrossprod(IV.V.rowSums) / IF.BV
      if(IF.BV == 0 | mean(abs(IF.V)) < sqrt(.Machine$double.eps)) IF.p <- NA
      else IF.p <- tryCatch(.quad_pval(U = IF.U, V = IF.V, method = method), error = function(e) { NA })
      IF.pval[i] <- tryCatch(fisher_pval(c(IF.Bp, IF.p)), error = function(e) { IF.Bp })
    }
    if(JF) JF.pval[i] <- tryCatch(fisher_pval(c(MF.Bp, MF.p, IF.Bp, IF.p)), error = function(e) { MF.Bp })
    if(JD) JD.pval[i] <- tryCatch(fisher_pval(c(MF.pval[i], IF.pval[i])), error = function(e) { MF.pval[i] })
  }
  for(i in 1:n.cohort) close(cons[[i]])
  out <- data.frame(group=unique(group.info$group), n.variants=n.variants)
  if(MV | JV) out$MV.pval <- MV.pval
  if(MF | JF | JD) out$MF.pval <- MF.pval
  if(IV | JV) out$IV.pval <- IV.pval
  if(IF | JF | JD) out$IF.pval <- IF.pval
  if(JV) out$JV.pval <- JV.pval
  if(JF) out$JF.pval <- JF.pval
  if(JD) out$JD.pval <- JD.pval
  return(out[match(groups, out$group),])
}

