MAGEE <- function(null.obj, interaction, geno.file, group.file, group.file.sep = "\t", bgen.samplefile = NULL, interaction.covariates = NULL, meta.file.prefix = NULL, MAF.range = c(1e-7, 0.5), AF.strata.range = c(0, 1), MAF.weights.beta = c(1, 25), miss.cutoff = 1, missing.method = "impute2mean", method = "davies", tests = "JF", use.minor.allele = FALSE, auto.flip = FALSE, Garbage.Collection = FALSE, is.dosage = FALSE, ncores = 1){
  if(Sys.info()["sysname"] == "Windows" && ncores > 1) {
    warning("The package doMC is not available on Windows... Switching to single thread...", call. = FALSE)
    ncores <- 1
  }
  if(!grepl("\\.gds$|\\.bgen$", geno.file[1])) stop("Error: only .gds and .bgen format is supported in geno.file!")
  if(!inherits(null.obj,  c("glmmkin", "glmmkin.multi"))) stop("Error: null.obj must be a class glmmkin or glmmkin.multi object!")
  n.pheno <- null.obj$n.pheno
  missing.method <- try(match.arg(missing.method, c("impute2mean", "impute2zero")))
  if(inherits(missing.method, "try-error")) stop("Error: \"missing.method\" must be \"impute2mean\" or \"impute2zero\".")
  if(any(!tests %in% c("MV", "MF", "IV", "IF", "JV", "JF", "JD"))) stop("Error: \"tests\" should only include \"MV\" for the main effect variance component test, \"MF\" for the main effect combined test of the burden and variance component tests using Fisher\'s method, \"IV\" for the interaction variance component test, \"IF\" for the interaction combined test of the burden and variance component tests using Fisher\'s method, \"JV\" for the joint variance component test for main effect and interaction, \"JF\" for the joint combined test of the burden and variance component tests for main effect and interaction using Fisher\'s method, or \"JD\" for the joint combined test of the burden and variance component tests for main effect and interaction using double Fisher\'s method.")
  MV <- "MV" %in% tests
  MF <- "MF" %in% tests
  IV <- "IV" %in% tests
  IF <- "IF" %in% tests
  JV <- "JV" %in% tests
  JF <- "JF" %in% tests
  JD <- "JD" %in% tests
  if(!inherits(interaction, c("integer", "numeric", "character"))) stop("Error: \"interaction\" should be an integer, numeric, or character vector.")
  residuals <- null.obj$scaled.residuals
  n <- length(unique(null.obj$id_include))
  qi <- length(interaction.covariates) # number of covariates with interaction effects but we don't test
  ei <- length(interaction) # number of covariates with interaction effects that we want to test
  if(inherits(interaction,"character")) {
    if (is.null(interaction.covariates)) {
      if (!all(interaction %in% colnames(null.obj$X))) {stop("there are interactions not in column name of covariate matrix.")}
      E <- as.matrix(null.obj$X[,interaction])
    } else {
      if (any(interaction.covariates %in% interaction)) {stop("there are interaction.covariates also specified as interaction.")}
      interaction <- c(interaction.covariates, interaction)
      if (!all(interaction %in% colnames(null.obj$X))) {stop("there are interaction or interaction.covariates not in column name of covariate matrix.")}
      E <- as.matrix(null.obj$X[,interaction])
    }
  } else {
    if (is.null(interaction.covariates)) {
      E <- as.matrix(null.obj$X[,interaction+1])
    } else {
      if (any(interaction.covariates %in% interaction)) {stop("there are interaction.covariates also specified as interaction.")}
      interaction <- c(interaction.covariates, interaction)
      E <- as.matrix(null.obj$X[,interaction+1])
    }
  }
  interaction <- as.character(interaction)
  n.E <- as.numeric(dim(E)[2]) # n.E = qi + ei
  if(grepl("\\.gds$", geno.file[1])){
    if (!inherits(geno.file, "SeqVarGDSClass")) {
      gds <- SeqArray::seqOpen(geno.file) 
    } else {
      gds <- geno.file
    }
    sample.id <- SeqArray::seqGetData(gds, "sample.id")
    if(any(is.na(match(null.obj$id_include, sample.id)))) warning("Check your data... Some individuals in null.obj$id_include are missing in sample.id of geno.file!", call. = FALSE)
    sample.id <- sample.id[sample.id %in% null.obj$id_include]
    if(length(sample.id) == 0) stop("Error: null.obj$id_include does not match sample.id in geno.file!")
    if(any(duplicated(null.obj$id_include))) {
      match.id <- null.obj$id_include %in% sample.id
      null.obj$id_include <- null.obj$id_include[match.id]
      J <- t(sparseMatrix(i=1:length(null.obj$id_include), j=match(null.obj$id_include, unique(null.obj$id_include)[match(sample.id, unique(null.obj$id_include))]), x=1))
    } else match.id <- match(sample.id, null.obj$id_include)
    E <- as.matrix(E[match.id, , drop = FALSE])
    E <- scale(E, scale = FALSE)
    if(inherits(null.obj, "glmmkin.multi")) {
      residuals <- residuals[match.id, , drop = FALSE]
      match.id <- rep(match.id, n.pheno) + rep((0:(n.pheno-1))*n, each = length(match.id))
    } else {
      residuals <- residuals[match.id]
    }
    if(!is.null(null.obj$P)) {
      null.obj$P <- null.obj$P[match.id, match.id]
    } else {
      null.obj$Sigma_iX <- null.obj$Sigma_iX[match.id, , drop = FALSE]
      null.obj$Sigma_i <- null.obj$Sigma_i[match.id, match.id]
    }
    strata <- apply(E, 1, paste, collapse = ":")
    strata <- if(length(unique(strata))>length(strata)/100) NULL else as.numeric(as.factor(strata)) 
    if(!is.null(strata)) strata.list <- lapply(unique(strata), function(x) which(strata==x)) 
    variant.idx <- SeqArray::seqGetData(gds, "variant.id")
    chr <- SeqArray::seqGetData(gds, "chromosome")
    pos <- SeqArray::seqGetData(gds, "position")
    alleles.list <- strsplit(SeqArray::seqGetData(gds, "allele"), ",")
    ref <- unlist(lapply(alleles.list, function(x) x[1]))
    alt <- unlist(lapply(alleles.list, function(x) paste(x[-1], collapse=",")))
    rm(alleles.list); gc()
    if (!inherits(geno.file, "SeqVarGDSClass")) {
      SeqArray::seqClose(gds)
    }
    variant.id <- paste(chr, pos, ref, alt, sep = ":")
    rm(chr, pos, ref, alt); gc()
    group.info <- try(fread(group.file, header = FALSE, data.table = FALSE, col.names = c("group", "chr", "pos", "ref", "alt", "weight"), colClasses = c("character","character","integer","character","character","numeric"), sep = group.file.sep), silent = TRUE)
    if (inherits(group.info, "try-error")) {
      stop("Error: cannot read group.file!")
    }
    variant.id1 <- paste(group.info$chr, group.info$pos, group.info$ref, group.info$alt, sep = ":")
    is.duplicated <- duplicated(paste(group.info$group, variant.id1, sep = ":"))
    group.info <- group.info[!is.duplicated, ]
    variant.id1 <- variant.id1[!is.duplicated]
    rm(is.duplicated)
    variant.idx1 <- variant.idx[match(variant.id1, variant.id)]
    group.info$variant.idx <- variant.idx1
    group.info$flip <- 0
    if(auto.flip) {
      message("Automatic allele flipping enabled...\nVariants matching alt/ref but not ref/alt alleles will also be included, with flipped effects")
      variant.id2 <- paste(group.info$chr, group.info$pos, group.info$alt, group.info$ref, sep = ":")
      variant.idx2 <- variant.idx[match(variant.id2, variant.id)]
      if(any(!is.na(variant.idx1) & !is.na(variant.idx2))) {
        tmp.dups <- which(!is.na(variant.idx1) & !is.na(variant.idx2))
        message("The following ambiguous variants were found:")
        message(paste(variant.id1[tmp.dups], collapse = ", "))
        warning("Both variants with alleles ref/alt and alt/ref were present at the same position and coding should be double checked!\nFor these variants, only those with alleles ref/alt were used in the analysis...", call. = FALSE)
        variant.idx2[tmp.dups] <- NA
        rm(tmp.dups)
      }
      group.info$flip <- 1*(!is.na(variant.idx2))
      group.info$variant.idx[!is.na(variant.idx2)] <- variant.idx2[!is.na(variant.idx2)]
      rm(variant.id2, variant.idx2)
    }
    rm(variant.id, variant.id1, variant.idx1); gc()
    group.info <- subset(group.info, !is.na(variant.idx))
    groups <- unique(group.info$group)
    n.groups.all <- length(groups)
    group.info$group.idx <- as.numeric(factor(group.info$group))
    group.info <- group.info[order(group.info$group.idx, group.info$variant.idx), ]
    group.idx.end <- findInterval(1:n.groups.all, group.info$group.idx)
    group.idx.start <- c(1, group.idx.end[-n.groups.all] + 1)
    ncores <- min(c(ncores, parallel::detectCores(logical = TRUE)))
    if(ncores > 1) {
      doMC::registerDoMC(cores = ncores)
      n.groups.percore <- (n.groups.all-1) %/% ncores + 1
      n.groups.percore_1 <- n.groups.percore * ncores - n.groups.all
      b <- NULL
      out <- foreach(b=1:ncores, .combine=rbind, .multicombine = TRUE, .inorder=FALSE, .options.multicore = list(preschedule = FALSE, set.seed = FALSE)) %dopar% {
        idx <- if(b <= n.groups.percore_1) ((b-1)*(n.groups.percore-1)+1):(b*(n.groups.percore-1)) else (n.groups.percore_1*(n.groups.percore-1)+(b-n.groups.percore_1-1)*n.groups.percore+1):(n.groups.percore_1*(n.groups.percore-1)+(b-n.groups.percore_1)*n.groups.percore)
        n.groups <- length(idx)
        if (!inherits(geno.file, "SeqVarGDSClass")) {
          gds <- SeqArray::seqOpen(geno.file)
        } else {
          gds <- geno.file
        }
        SeqArray::seqSetFilter(gds, sample.id = sample.id, verbose = FALSE)
        n.variants <- rep(0,n.groups)
        miss.min <- rep(NA,n.groups)
        miss.mean <- rep(NA, n.groups)
        miss.max <- rep(NA, n.groups)
        freq.min <- rep(NA, n.groups)
        freq.mean <- rep(NA, n.groups)
        freq.max <- rep(NA, n.groups)
        freq.strata.min <- rep(NA, n.groups)
        freq.strata.max <- rep(NA, n.groups)
        if(MV | JV) MV.pval <- rep(NA, n.groups)
        if(IV | JV) IV.pval <- rep(NA, n.groups)
        if(JV) JV.pval <- rep(NA, n.groups)
        if(MF | JF | JD) MF.pval <- rep(NA, n.groups)
        if(IF | JF | JD) IF.pval <- rep(NA, n.groups)
        if(JF) JF.pval <- rep(NA, n.groups)
        if(JD) JD.pval <- rep(NA, n.groups)
        if(!is.null(meta.file.prefix)) {
          if(inherits(null.obj, "glmmkin.multi")) stop("Error: meta-analysis not supported yet for multiple phenotypes.")
          if(!is.null(interaction.covariates)) stop("Error: meta-analysis not supported yet for interaction.covariates.")
          if(.Platform$endian!="little") stop("Error: platform must be little endian.")
          meta.file.score <- paste0(meta.file.prefix, ".score.", b)
          meta.file.cov <- paste0(meta.file.prefix, ".cov.", b)
          write.table(t(c("group", "chr", "pos", "ref", "alt", "N", "missrate", "altfreq", "G.SCORE", paste("K.SCORE.", 1:ei, sep=""))), meta.file.score, quote = FALSE, row.names = FALSE, col.names = FALSE)
          meta.file.cov.handle <- file(meta.file.cov, "wb")
          writeBin(as.numeric(ei), meta.file.cov.handle, size = 4)
          writeBin(interaction, meta.file.cov.handle, size = 4)
        }
        for(i in 1:n.groups) {
          tmp.idx <- group.idx.start[idx[i]]:group.idx.end[idx[i]]
          tmp.group.info <- group.info[tmp.idx, , drop = FALSE]
          SeqArray::seqSetFilter(gds, variant.id = tmp.group.info$variant.idx, verbose = FALSE)
          geno <- if(is.dosage) SeqVarTools::imputedDosage(gds, use.names = FALSE) else SeqVarTools::altDosage(gds, use.names = FALSE)
          miss <- colMeans(is.na(geno))
          freq <- colMeans(geno, na.rm = TRUE)/2
          include <- (miss <= miss.cutoff & ((freq >= MAF.range[1] & freq <= MAF.range[2]) | (freq >= 1-MAF.range[2] & freq <= 1-MAF.range[1])))
          if(any(duplicated(null.obj$id_include))) geno <- crossprod(J, geno)
          if(!is.null(strata)) { # E is not continuous
            freq.tmp <- sapply(strata.list, function(x) colMeans(geno[x, , drop = FALSE], na.rm = TRUE)/2) # freq.tmp is a matrix, each column is a strata, and each row is a varirant 
            if (length(dim(freq.tmp)) == 2) freq_strata <- apply(freq.tmp, 1, range) else freq_strata <- as.matrix(range(freq.tmp)) # freq_strata is the range of allele freq across strata.list
            include <- include & !is.na(freq_strata[1,]) & !is.na(freq_strata[2,]) & freq_strata[1,] >= AF.strata.range[1] & freq_strata[2,] <= AF.strata.range[2]
            rm(freq.tmp)
          }
          n.p <- sum(include)
          if(n.p == 0) next
          tmp.group.info <- tmp.group.info[include, , drop = FALSE]
          miss <- miss[include]
          freq <- freq[include]
          geno <- geno[, include, drop = FALSE]
          if(!is.null(strata)) freq_strata <- freq_strata[, include, drop = FALSE]
          N <- nrow(geno) - colSums(is.na(geno))
          if(sum(tmp.group.info$flip) > 0) {
            freq[tmp.group.info$flip==1] <- 1 - freq[tmp.group.info$flip==1]
            geno[, tmp.group.info$flip==1] <- 2 - geno[, tmp.group.info$flip==1]
            if(!is.null(strata)) freq_strata[, tmp.group.info$flip==1] <- 1 - freq_strata[, tmp.group.info$flip==1]
          }
          if(max(miss)>0) {
            miss.idx <- which(is.na(geno))
            geno[miss.idx] <- if(missing.method=="impute2mean") 2*freq[ceiling(miss.idx/nrow(geno))] else 0
          }
          if(IV | IF | JV | JF | JD) {
            K <- do.call(cbind, sapply((1+qi):ncol(E), function(xx) geno*E[,xx], simplify = FALSE), envir = environment())
            SK <- as.vector(crossprod(K,residuals))
          }
          if(!is.null(interaction.covariates) && (IV | IF | JV | JF | JD)) {
            geno <- cbind(geno, do.call(cbind, sapply(1:qi, function(xx) geno*E[,xx], simplify = FALSE), envir = environment()))
          }
          U <- as.vector(crossprod(geno, residuals))
          if(inherits(null.obj, "glmmkin.multi")) {
            geno <- Diagonal(n = n.pheno) %x% geno
            if (IV | IF | JV | JF | JD) K <- Diagonal(n = n.pheno) %x% K
          }
          if(!is.null(null.obj$P)) {
            PG <- crossprod(null.obj$P, geno)
          } else {
            GSigma_iX <- crossprod(geno, null.obj$Sigma_iX)
            PG <- crossprod(null.obj$Sigma_i, geno) - tcrossprod(null.obj$Sigma_iX, tcrossprod(GSigma_iX, null.obj$cov))
          }
          V <- as.matrix(crossprod(geno, PG))
          if(MV | MF | JV | JF | JD) c1 <- rep(1:n.p,n.pheno)+rep((0:(n.pheno-1))*n.p*(1+qi), each=n.p) # index for GPG and row.index for GPC
          if(!is.null(interaction.covariates) && (JV | JF | JD)) {
            c2 <- rep((n.p+1):(n.p*(1+qi)),n.pheno)+rep((0:(n.pheno-1))*n.p*(1+qi), each=n.p*qi) # index for CPC and col.index for GPC
            CPC_i <- try(solve(V[c2,c2]), silent = TRUE)
            if(inherits(CPC_i, "try-error") || any(diag(CPC_i)<0)) CPC_i <- try(MASS::ginv(V[c2,c2]), silent = TRUE)
	    if(inherits(CPC_i, "try-error")) next
            U.adj <- U[c1] - tcrossprod(tcrossprod(V[c1,c2],CPC_i),t(U[c2]))
            V.adj <- V[c1,c1] - tcrossprod(tcrossprod(V[c1,c2],CPC_i),V[c1,c2])
          }
          if(IV | IF | JV | JF | JD) {
            if(!is.null(null.obj$P)) {
              KPK <- crossprod(K,crossprod(null.obj$P,K))
            } else {
              KSigma_iX <- crossprod(K, null.obj$Sigma_iX)
              KPK <- crossprod(K, crossprod(null.obj$Sigma_i, K)) - tcrossprod(KSigma_iX, tcrossprod(KSigma_iX, null.obj$cov))
            }
            V_i <- try(solve(V), silent = TRUE)
            if(inherits(V_i, "try-error") || any(diag(V_i)<0)) V_i <- try(MASS::ginv(V), silent = TRUE)
	    if(inherits(V_i, "try-error")) next
            KPG <- crossprod(K,PG)
            IV.U <- SK - tcrossprod(tcrossprod(KPG,V_i),t(U))
            IV.V <- KPK - tcrossprod(tcrossprod(KPG,V_i),KPG)
          }
          if(!is.null(meta.file.prefix)) {
            score <- cbind(tmp.group.info[,c("group","chr","pos","ref","alt")], N, miss, freq, U, matrix(crossprod(K,residuals), nrow = n.p, ncol = ei))
            # odr <- order(as.numeric(row.names(tmp.group.info)))
            # score <- score[odr, ]
            # odr <- rep(odr, (1+ei))+rep(0:ei*length(odr), each=length(odr))
            # COV <-rbind(cbind(V, t(KPG)), cbind(KPG, KPK))[odr, odr] 
            COV <-rbind(cbind(V, t(KPG)), cbind(KPG, KPK))
            write.table(score, meta.file.score, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
            writeBin(COV[lower.tri(COV, diag = TRUE)], meta.file.cov.handle, size = 4)
          }
          if(use.minor.allele) {
            tmp.group.info$weight[freq > 0.5] <- -tmp.group.info$weight[freq > 0.5]
            if(!is.null(strata)) freq_strata[, freq > 0.5] <- 1 - freq_strata[, freq > 0.5]
            freq[freq > 0.5] <- 1 - freq[freq > 0.5]
          }
          weights <- rep(tmp.group.info$weight * MAF.weights.beta.fun(freq, MAF.weights.beta[1], MAF.weights.beta[2]), n.pheno)
          n.variants[i] <- n.p
          miss.min[i] <- min(miss)
          miss.mean[i] <- mean(miss)
          miss.max[i] <- max(miss)
          freq.min[i] <- min(freq)
          freq.mean[i] <- mean(freq)
          freq.max[i] <- max(freq)
          if(!is.null(strata)) {
            freq.strata.min[i] <- min(freq_strata)
            freq.strata.max[i] <- max(freq_strata)
          }
          if(MV | MF | JV | JF | JD) {
            U <- U[c1]*weights
            V <- t(V[c1,c1]*weights)*weights
          }
          if(!is.null(interaction.covariates) && (JV | JF | JD)) {
            U.adj <- U.adj*weights
            V.adj <- t(V.adj*weights)*weights
          }
          if(IV | IF | JV | JF | JD) {
            IV.U <- IV.U*rep(weights, ei)
            IV.V <- t(IV.V*rep(weights, ei))*rep(weights, ei)
          }
          if(MV | JV) MV.pval[i] <- tryCatch(.quad_pval(U = U, V = V, method = method), error = function(e) { NA })
          if(IV | JV) IV.pval[i] <- tryCatch(.quad_pval(U = IV.U, V = IV.V, method = method), error = function(e) { NA })
          if(JV && is.null(interaction.covariates)) JV.pval[i] <- tryCatch(fisher_pval(c(MV.pval[i], IV.pval[i])), error = function(e) { MV.pval[i] })
          if(JV && !is.null(interaction.covariates)) {
            MV.adj.pval <- tryCatch(.quad_pval(U = U.adj, V = V.adj, method = method), error = function(e) { NA })
            JV.pval[i] <- tryCatch(fisher_pval(c(MV.adj.pval, IV.pval[i])), error = function(e) { MV.adj.pval })
          }
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
          if((JF | JD) && !is.null(interaction.covariates)) {
            MF.BU.adj <- sum(U.adj)
            MF.BV.adj <- sum(V.adj)
            MF.Bp.adj <- pchisq(MF.BU.adj^2/MF.BV.adj,df=1,lower.tail=FALSE)
            V.adj.rowSums <- rowSums(V.adj)
            MF.U.adj <- U.adj  - V.adj.rowSums * MF.BU.adj / MF.BV.adj 
            MF.V.adj  <- V.adj  - tcrossprod(V.adj.rowSums) / MF.BV.adj 
            if(MF.BV.adj == 0 | mean(abs(MF.V.adj)) < sqrt(.Machine$double.eps)) MF.adj.p <- NA
            else MF.adj.p <- tryCatch(.quad_pval(U = MF.U.adj, V = MF.V.adj, method = method), error = function(e) { NA })
            MF.adj.pval <- tryCatch(fisher_pval(c(MF.Bp.adj, MF.adj.p)), error = function(e) { MF.Bp.adj })
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
          if(JF && is.null(interaction.covariates)) JF.pval[i] <- tryCatch(fisher_pval(c(MF.Bp, MF.p, IF.Bp, IF.p)), error = function(e) { MF.Bp })
          if(JF && !is.null(interaction.covariates)) JF.pval[i] <- tryCatch(fisher_pval(c(MF.Bp.adj, MF.adj.p, IF.Bp, IF.p)), error = function(e) { MF.Bp.adj })
          if(JD && is.null(interaction.covariates)) JD.pval[i] <- tryCatch(fisher_pval(c(MF.pval[i], IF.pval[i])), error = function(e) { MF.pval[i] })
          if(JD && !is.null(interaction.covariates)) JD.pval[i] <- tryCatch(fisher_pval(c(MF.adj.pval, IF.pval[i])), error = function(e) { MF.adj.pval })
          rm(geno)
          if(Garbage.Collection) gc()
        }
        SeqArray::seqClose(gds)
        if(!is.null(meta.file.prefix)) close(meta.file.cov.handle)
        tmp.out <- data.frame(group=unique(group.info$group)[idx], n.variants=n.variants, miss.min=miss.min, miss.mean=miss.mean, miss.max=miss.max, freq.min=freq.min, freq.mean=freq.mean, freq.max=freq.max,freq.strata.min=freq.strata.min, freq.strata.max=freq.strata.max)
        if(MV | JV) tmp.out$MV.pval <- MV.pval
        if(MF | JF | JD) tmp.out$MF.pval <- MF.pval
        if(IV | JV) tmp.out$IV.pval <- IV.pval
        if(IF | JF | JD) tmp.out$IF.pval <- IF.pval
        if(JV) tmp.out$JV.pval <- JV.pval
        if(JF) tmp.out$JF.pval <- JF.pval
        if(JD) tmp.out$JD.pval <- JD.pval
        tmp.out
      }
      if (inherits(geno.file, "SeqVarGDSClass")) {
    	SeqArray::seqClose(geno.file)
      }
      return(out)
    } else { # use a single core
      n.groups <- n.groups.all
      if (!inherits(geno.file, "SeqVarGDSClass")) {
        gds <- SeqArray::seqOpen(geno.file)
      }
      SeqArray::seqSetFilter(gds, sample.id = sample.id, verbose = FALSE)
      n.variants <- rep(0,n.groups)
      miss.min <- rep(NA,n.groups)
      miss.mean <- rep(NA, n.groups)
      miss.max <- rep(NA, n.groups)
      freq.min <- rep(NA, n.groups)
      freq.mean <- rep(NA, n.groups)
      freq.max <- rep(NA, n.groups)
      freq.strata.min <- rep(NA, n.groups)
      freq.strata.max <- rep(NA, n.groups)
      if(MV | JV) MV.pval <- rep(NA, n.groups)
      if(IV | JV) IV.pval <- rep(NA, n.groups)
      if(JV) JV.pval <- rep(NA, n.groups)
      if(MF | JF | JD) MF.pval <- rep(NA, n.groups)
      if(IF | JF | JD) IF.pval <- rep(NA, n.groups)
      if(JF) JF.pval <- rep(NA, n.groups)
      if(JD) JD.pval <- rep(NA, n.groups)
      if(!is.null(meta.file.prefix)) {
        if(inherits(null.obj, "glmmkin.multi")) stop("Error: meta-analysis not supported yet for multiple phenotypes.")
        if(!is.null(interaction.covariates)) stop("Error: meta-analysis not supported yet for interaction.covariates.")
        if(.Platform$endian!="little") stop("Error: platform must be little endian.")
        meta.file.score <- paste0(meta.file.prefix, ".score.1")
        meta.file.cov <- paste0(meta.file.prefix, ".cov.1")
        write.table(t(c("group", "chr", "pos", "ref", "alt", "N", "missrate", "altfreq", "G.SCORE", paste("K.SCORE.", 1:ei, sep=""))), meta.file.score, quote = FALSE, row.names = FALSE, col.names = FALSE)
        meta.file.cov.handle <- file(meta.file.cov, "wb")
        writeBin(as.numeric(ei), meta.file.cov.handle, size = 4)
        writeBin(interaction, meta.file.cov.handle, size = 4)
      }
      for(i in 1:n.groups) {
        tmp.idx <- group.idx.start[i]:group.idx.end[i]
        tmp.group.info <- group.info[tmp.idx, , drop = FALSE]
        SeqArray::seqSetFilter(gds, variant.id = tmp.group.info$variant.idx, verbose = FALSE)
        geno <- if(is.dosage) SeqVarTools::imputedDosage(gds, use.names = FALSE) else SeqVarTools::altDosage(gds, use.names = FALSE)
        miss <- colMeans(is.na(geno))
        freq <- colMeans(geno, na.rm = TRUE)/2
        include <- (miss <= miss.cutoff & ((freq >= MAF.range[1] & freq <= MAF.range[2]) | (freq >= 1-MAF.range[2] & freq <= 1-MAF.range[1])))
        if(any(duplicated(null.obj$id_include))) geno <- crossprod(J, geno)
        if(!is.null(strata)) { # E is not continuous
          freq.tmp <- sapply(strata.list, function(x) colMeans(geno[x, , drop = FALSE], na.rm = TRUE)/2) # freq.tmp is a matrix, each column is a strata, and each row is a varirant 
          if (length(dim(freq.tmp)) == 2) freq_strata <- apply(freq.tmp, 1, range) else freq_strata <- as.matrix(range(freq.tmp)) # freq_strata is the range of allele freq across strata.list
          include <- include & !is.na(freq_strata[1,]) & !is.na(freq_strata[2,]) & freq_strata[1,] >= AF.strata.range[1] & freq_strata[2,] <= AF.strata.range[2]
          rm(freq.tmp)
        }
        n.p <- sum(include)
        if(n.p == 0) next
        tmp.group.info <- tmp.group.info[include, , drop = FALSE]
        miss <- miss[include]
        freq <- freq[include]
        geno <- geno[, include, drop = FALSE]
        if(!is.null(strata)) freq_strata <- freq_strata[, include, drop = FALSE]
        N <- nrow(geno) - colSums(is.na(geno))
        if(sum(tmp.group.info$flip) > 0) {
          freq[tmp.group.info$flip==1] <- 1 - freq[tmp.group.info$flip==1]
          geno[, tmp.group.info$flip==1] <- 2 - geno[, tmp.group.info$flip==1]
          if(!is.null(strata)) freq_strata[, tmp.group.info$flip==1] <- 1 - freq_strata[, tmp.group.info$flip==1]
        }
        if(max(miss)>0) {
          miss.idx <- which(is.na(geno))
          geno[miss.idx] <- if(missing.method=="impute2mean") 2*freq[ceiling(miss.idx/nrow(geno))] else 0
        }
        if(IV | IF | JV | JF | JD) {
          K <- do.call(cbind, sapply((1+qi):ncol(E), function(xx) geno*E[,xx], simplify = FALSE), envir = environment())
          SK <- as.vector(crossprod(K,residuals))
        }
        if(!is.null(interaction.covariates) && (IV | IF | JV | JF | JD)) {
          geno <- cbind(geno, do.call(cbind, sapply(1:qi, function(xx) geno*E[,xx], simplify = FALSE), envir = environment()))
        }
        U <- as.vector(crossprod(geno, residuals))
        if(inherits(null.obj, "glmmkin.multi")) {
          geno <- Diagonal(n = n.pheno) %x% geno
          if (IV | IF | JV | JF | JD) K <- Diagonal(n = n.pheno) %x% K
        }
        if(!is.null(null.obj$P)) {
          PG <- crossprod(null.obj$P, geno)
        } else {
          GSigma_iX <- crossprod(geno, null.obj$Sigma_iX)
          PG <- crossprod(null.obj$Sigma_i, geno) - tcrossprod(null.obj$Sigma_iX, tcrossprod(GSigma_iX, null.obj$cov))
        }
        V <- as.matrix(crossprod(geno, PG))
        if(MV | MF | JV | JF | JD) c1 <- rep(1:n.p,n.pheno)+rep((0:(n.pheno-1))*n.p*(1+qi), each=n.p) # index for GPG and row.index for GPC
        if(!is.null(interaction.covariates) && (JV | JF | JD)) {
          c2 <- rep((n.p+1):(n.p*(1+qi)),n.pheno)+rep((0:(n.pheno-1))*n.p*(1+qi), each=n.p*qi) # index for CPC and col.index for GPC
          CPC_i <- try(solve(V[c2,c2]), silent = TRUE)
          if(inherits(CPC_i, "try-error") || any(diag(CPC_i)<0)) CPC_i <- try(MASS::ginv(V[c2,c2]), silent = TRUE)
	  if(inherits(CPC_i, "try-error")) next
          U.adj <- U[c1] - tcrossprod(tcrossprod(V[c1,c2],CPC_i),t(U[c2]))
          V.adj <- V[c1,c1] - tcrossprod(tcrossprod(V[c1,c2],CPC_i),V[c1,c2])
        }
        if(IV | IF | JV | JF | JD) {
          if(!is.null(null.obj$P)) {
            KPK <- crossprod(K,crossprod(null.obj$P,K))
          } else {
            KSigma_iX <- crossprod(K, null.obj$Sigma_iX)
            KPK <- crossprod(K, crossprod(null.obj$Sigma_i, K)) - tcrossprod(KSigma_iX, tcrossprod(KSigma_iX, null.obj$cov))
          }
          V_i <- try(solve(V), silent = TRUE)
          if(inherits(V_i, "try-error") || any(diag(V_i)<0)) V_i <- try(MASS::ginv(V), silent = TRUE)
	  if(inherits(V_i, "try-error")) next
          KPG <- crossprod(K,PG)
          IV.U <- SK - tcrossprod(tcrossprod(KPG,V_i),t(U))
          IV.V <- KPK - tcrossprod(tcrossprod(KPG,V_i),KPG)
        }
        if(!is.null(meta.file.prefix)) {
          score <- cbind(tmp.group.info[,c("group","chr","pos","ref","alt")], N, miss, freq, U, matrix(crossprod(K,residuals), nrow = n.p, ncol = ei))
          # odr <- order(as.numeric(row.names(tmp.group.info)))
          # score <- score[odr, ]
          # odr <- rep(odr, (1+ei))+rep(0:ei*length(odr), each=length(odr))
          # COV <-rbind(cbind(V, t(KPG)), cbind(KPG, KPK))[odr, odr] 
          COV <-rbind(cbind(V, t(KPG)), cbind(KPG, KPK))
          write.table(score, meta.file.score, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
          writeBin(COV[lower.tri(COV, diag = TRUE)], meta.file.cov.handle, size = 4)
        }
        if(use.minor.allele) {
          tmp.group.info$weight[freq > 0.5] <- -tmp.group.info$weight[freq > 0.5]
          if(!is.null(strata)) freq_strata[, freq > 0.5] <- 1 - freq_strata[, freq > 0.5]
          freq[freq > 0.5] <- 1 - freq[freq > 0.5]
        }
        weights <- rep(tmp.group.info$weight * MAF.weights.beta.fun(freq, MAF.weights.beta[1], MAF.weights.beta[2]), n.pheno)
        n.variants[i] <- n.p
        miss.min[i] <- min(miss)
        miss.mean[i] <- mean(miss)
        miss.max[i] <- max(miss)
        freq.min[i] <- min(freq)
        freq.mean[i] <- mean(freq)
        freq.max[i] <- max(freq)
        if(!is.null(strata)) {
          freq.strata.min[i] <- min(freq_strata)
          freq.strata.max[i] <- max(freq_strata)
        }
        if(MV | MF | JV | JF | JD) {
          U <- U[c1]*weights
          V <- t(V[c1,c1]*weights)*weights
        }
        if(!is.null(interaction.covariates) && (JV | JF | JD)) {
          U.adj <- U.adj*weights
          V.adj <- t(V.adj*weights)*weights
        }
        if(IV | IF | JV | JF | JD) {
          IV.U <- IV.U*rep(weights, ei)
          IV.V <- t(IV.V*rep(weights, ei))*rep(weights, ei)
        }
        if(MV | JV) MV.pval[i] <- tryCatch(.quad_pval(U = U, V = V, method = method), error = function(e) { NA })
        if(IV | JV) IV.pval[i] <- tryCatch(.quad_pval(U = IV.U, V = IV.V, method = method), error = function(e) { NA })
        if(JV && is.null(interaction.covariates)) JV.pval[i] <- tryCatch(fisher_pval(c(MV.pval[i], IV.pval[i])), error = function(e) { MV.pval[i] })
        if(JV && !is.null(interaction.covariates)) {
          MV.adj.pval <- tryCatch(.quad_pval(U = U.adj, V = V.adj, method = method), error = function(e) { NA })
          JV.pval[i] <- tryCatch(fisher_pval(c(MV.adj.pval, IV.pval[i])), error = function(e) { MV.adj.pval })
        }
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
        if((JF | JD) && !is.null(interaction.covariates)) {
          MF.BU.adj <- sum(U.adj)
          MF.BV.adj <- sum(V.adj)
          MF.Bp.adj <- pchisq(MF.BU.adj^2/MF.BV.adj,df=1,lower.tail=FALSE)
          V.adj.rowSums <- rowSums(V.adj)
          MF.U.adj <- U.adj  - V.adj.rowSums * MF.BU.adj / MF.BV.adj 
          MF.V.adj  <- V.adj  - tcrossprod(V.adj.rowSums) / MF.BV.adj 
          if(MF.BV.adj == 0 | mean(abs(MF.V.adj)) < sqrt(.Machine$double.eps)) MF.adj.p <- NA
          else MF.adj.p <- tryCatch(.quad_pval(U = MF.U.adj, V = MF.V.adj, method = method), error = function(e) { NA })
          MF.adj.pval <- tryCatch(fisher_pval(c(MF.Bp.adj, MF.adj.p)), error = function(e) { MF.Bp.adj })
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
        if(JF && is.null(interaction.covariates)) JF.pval[i] <- tryCatch(fisher_pval(c(MF.Bp, MF.p, IF.Bp, IF.p)), error = function(e) { MF.Bp })
        if(JF && !is.null(interaction.covariates)) JF.pval[i] <- tryCatch(fisher_pval(c(MF.Bp.adj, MF.adj.p, IF.Bp, IF.p)), error = function(e) { MF.Bp.adj })
        if(JD && is.null(interaction.covariates)) JD.pval[i] <- tryCatch(fisher_pval(c(MF.pval[i], IF.pval[i])), error = function(e) { MF.pval[i] })
        if(JD && !is.null(interaction.covariates)) JD.pval[i] <- tryCatch(fisher_pval(c(MF.adj.pval, IF.pval[i])), error = function(e) { MF.adj.pval })
        rm(geno, K)
        if(Garbage.Collection) gc()
      }
      SeqArray::seqClose(gds)
      if(!is.null(meta.file.prefix)) close(meta.file.cov.handle)
      out <- data.frame(group=unique(group.info$group), n.variants=n.variants, miss.min=miss.min, miss.mean=miss.mean, miss.max=miss.max, freq.min=freq.min, freq.mean=freq.mean, freq.max=freq.max,freq.strata.min=freq.strata.min, freq.strata.max=freq.strata.max)
      if(MV | JV) out$MV.pval <- MV.pval
      if(MF | JF | JD) out$MF.pval <- MF.pval
      if(IV | JV) out$IV.pval <- IV.pval
      if(IF | JF | JD) out$IF.pval <- IF.pval
      if(JV) out$JV.pval <- JV.pval
      if(JF) out$JF.pval <- JF.pval
      if(JD) out$JD.pval <- JD.pval
    }
    return(out[match(groups, out$group),])
  } else if (grepl("\\.bgen$", geno.file)) {
    bgenInfo <- .Call('bgenHeader', geno.file)
    if (bgenInfo$SampleIdFlag == 0) {
      if (is.null(bgen.samplefile)) {
        stop("Error: bgen file does not contain sample identifiers. A .sample file (bgen.samplefile) is needed.")
      }
      sample.id <- fread(bgen.samplefile, header = TRUE, data.table = FALSE)
      if ((nrow(sample.id)-1) != bgenInfo$N){
        stop(paste0("Error: Number of sample identifiers in BGEN sample file (", nrow(sample.id)-1, ") does not match number of samples in BGEN file (", bgenInfo$N,")."))
      }
      sample.id <- sample.id[-1, 2]
    } else {
      sample.id <- bgenInfo$SampleIds
    }
    if(any(is.na(match(null.obj$id_include, sample.id)))) warning("Check your data... Some individuals in null.obj$id_include are missing in sample.id of bgen sample file!", call. = FALSE)
    select <- match(sample.id, unique(null.obj$id_include))
    select[is.na(select)] <- 0
    sample.id <- sample.id[sample.id %in% null.obj$id_include]
    if(length(sample.id) == 0) stop("Error: null.obj$id_include does not match sample.id in geno.file!")
    if(any(duplicated(null.obj$id_include))) {
      match.id <- null.obj$id_include %in% sample.id
      null.obj$id_include <- null.obj$id_include[match.id]
      J <- t(sparseMatrix(i=1:length(null.obj$id_include), j=match(null.obj$id_include, unique(null.obj$id_include)[match(sample.id, unique(null.obj$id_include))]), x=1))
    } else match.id <- match(sample.id, null.obj$id_include)
    E <- as.matrix(E[match.id, , drop = FALSE])
    E <- scale(E, scale = FALSE)
    if(inherits(null.obj, "glmmkin.multi")) {
      residuals <- residuals[match.id, , drop = FALSE]
      match.id <- rep(match.id, n.pheno) + rep((0:(n.pheno-1))*n, each = length(match.id))
    } else {
      residuals <- residuals[match.id]
    }
    if(!is.null(null.obj$P)) {
      null.obj$P <- null.obj$P[match.id, match.id]
    } else {
      null.obj$Sigma_iX <- null.obj$Sigma_iX[match.id, , drop = FALSE]
      null.obj$Sigma_i <- null.obj$Sigma_i[match.id, match.id]
    }
    strata <- apply(E, 1, paste, collapse = ":")
    strata <- if(length(unique(strata))>length(strata)/100) NULL else as.numeric(as.factor(strata))
    if(!is.null(strata)) {
      strata.list <- lapply(unique(strata), function(x) which(strata==x))
    } else {
      strata.list <- NULL
    }
    bgenVariant <- .Call("bgenVariantInfo", geno.file, bgenInfo$offset, bgenInfo$M, bgenInfo$N, bgenInfo$LayoutFlag, bgenInfo$CompressionFlag)
    variant.id <- paste(bgenVariant$VariantInfo$CHR, bgenVariant$VariantInfo$POS, bgenVariant$VariantInfo$A1, bgenVariant$VariantInfo$A2, sep = ":")
    gc()
    variant.idx <- 1:length(variant.id)
    group.info <- try(fread(group.file, header = FALSE, data.table = FALSE, col.names = c("group", "chr", "pos", "ref", "alt", "weight"), colClasses = c("character","character","integer","character","character","numeric"), sep = group.file.sep), silent = TRUE)
    if (inherits(group.info, "try-error")) {
      stop("Error: cannot read group.file!")
    }
    group.info <- group.info[!duplicated(paste(group.info$group, group.info$chr, group.info$pos, group.info$ref, group.info$alt, sep = ":")), ]
    variant.id1 <- paste(group.info$chr, group.info$pos, group.info$ref, group.info$alt, sep = ":")
    variant.idx1 <- variant.idx[match(variant.id1, variant.id)]
    group.info$variant.idx <- variant.idx1
    group.info$flip <- 0
    if(auto.flip) {
      message("Automatic allele flipping enabled...\nVariants matching alt/ref but not ref/alt alleles will also be included, with flipped effects")
      variant.id2 <- paste(group.info$chr, group.info$pos, group.info$alt, group.info$ref, sep = ":")
      variant.idx2 <- variant.idx[match(variant.id2, variant.id)]
      if(any(!is.na(variant.idx1) & !is.na(variant.idx2))) {
        tmp.dups <- which(!is.na(variant.idx1) & !is.na(variant.idx2))
        message("The following ambiguous variants were found:")
        message(paste(variant.id1[tmp.dups], collapse = ", "))
        warning("Both variants with alleles ref/alt and alt/ref were present at the same position and coding should be double checked!\nFor these variants, only those with alleles ref/alt were used in the analysis...", call. = FALSE)
        variant.idx2[tmp.dups] <- NA
        rm(tmp.dups)
      }
      group.info$flip <- 1*(!is.na(variant.idx2))
      group.info$variant.idx[!is.na(variant.idx2)] <- variant.idx2[!is.na(variant.idx2)]
      rm(variant.id2, variant.idx2)
    }
    ptr = bgenVariant$fbytes
    rm(variant.id, variant.id1, variant.idx1, bgenVariant); gc()
    group.info <- subset(group.info, !is.na(variant.idx))
    groups <- unique(group.info$group)
    n.groups.all <- length(groups)
    group.info$group.idx <- as.numeric(factor(group.info$group))
    group.info <- group.info[order(group.info$group.idx, group.info$variant.idx), ]
    group.idx.end <- findInterval(1:n.groups.all, group.info$group.idx)
    group.idx.start <- c(1, group.idx.end[-n.groups.all] + 1)
    if (ncores > n.groups.all) {
      warning("Number of cores (", ncores, ") is greater than number of groups (", n.groups.all, "). Using ", n.groups.all, " instead.", call. = FALSE)
      ncores <- n.groups.all
    }
    ncores <- min(c(ncores, parallel::detectCores(logical = TRUE)))
    if(ncores > 1) {
      doMC::registerDoMC(cores = ncores)
      n.groups.percore <- (n.groups.all-1) %/% ncores + 1
      n.groups.percore_1 <- n.groups.percore * ncores - n.groups.all
      b <- NULL
      out <- foreach(b=1:ncores, .combine=rbind, .multicombine = TRUE, .inorder=FALSE, .options.multicore = list(preschedule = FALSE, set.seed = FALSE)) %dopar% {
        idx <- if(b <= n.groups.percore_1) ((b-1)*(n.groups.percore-1)+1):(b*(n.groups.percore-1)) else (n.groups.percore_1*(n.groups.percore-1)+(b-n.groups.percore_1-1)*n.groups.percore+1):(n.groups.percore_1*(n.groups.percore-1)+(b-n.groups.percore_1)*n.groups.percore)
        n.groups <- length(idx)
        n.variants <- rep(0,n.groups)
        miss.min <- rep(NA,n.groups)
        miss.mean <- rep(NA, n.groups)
        miss.max <- rep(NA, n.groups)
        freq.min <- rep(NA, n.groups)
        freq.mean <- rep(NA, n.groups)
        freq.max <- rep(NA, n.groups)
        freq.strata.min <- rep(NA, n.groups)
        freq.strata.max <- rep(NA, n.groups)
        if(MV | JV) MV.pval <- rep(NA, n.groups)
        if(IV | JV) IV.pval <- rep(NA, n.groups)
        if(JV) JV.pval <- rep(NA, n.groups)
        if(MF | JF | JD) MF.pval <- rep(NA, n.groups)
        if(IF | JF | JD) IF.pval <- rep(NA, n.groups)
        if(JF) JF.pval <- rep(NA, n.groups)
        if(JD) JD.pval <- rep(NA, n.groups)
        if(!is.null(meta.file.prefix)) {
          if(inherits(null.obj, "glmmkin.multi")) stop("Error: meta-analysis not supported yet for multiple phenotypes.")
          if(!is.null(interaction.covariates)) stop("Error: meta-analysis not supported yet for interaction.covariates.")
          if(.Platform$endian!="little") stop("Error: platform must be little endian.")
          meta.file.score <- paste0(meta.file.prefix, ".score.", b)
          meta.file.cov <- paste0(meta.file.prefix, ".cov.", b)
          write.table(t(c("group", "chr", "pos", "ref", "alt", "N", "missrate", "altfreq", "G.SCORE", paste("K.SCORE.", 1:ei, sep=""))), meta.file.score, quote = FALSE, row.names = FALSE, col.names = FALSE)
          meta.file.cov.handle <- file(meta.file.cov, "wb")
          writeBin(as.numeric(ei), meta.file.cov.handle, size = 4)
          writeBin(interaction, meta.file.cov.handle, size = 4)
        }
        for(i in 1:n.groups) {
          tmp.idx <- group.idx.start[idx[i]]:group.idx.end[idx[i]]
          tmp.group.info <- group.info[tmp.idx, , drop = FALSE]
          if (bgenInfo$Layout == 2) {
            geno <- .Call("magee_bgen13", geno.file, tmp.group.info$variant.idx, ptr, select, bgenInfo$Compression, n)
          } else {
            geno <- .Call("magee_bgen11", geno.file, tmp.group.info$variant.idx, ptr, select, bgenInfo$Compression, bgenInfo$N, n)
          }
          miss <- colMeans(is.na(geno))
          freq <- colMeans(geno, na.rm = TRUE)/2
          include <- (miss <= miss.cutoff & ((freq >= MAF.range[1] & freq <= MAF.range[2]) | (freq >= 1-MAF.range[2] & freq <= 1-MAF.range[1])))
          if(any(duplicated(null.obj$id_include))) geno <- crossprod(J, geno)
          if(!is.null(strata)) { # E is not continuous
            freq.tmp <- sapply(strata.list, function(x) colMeans(geno[x, , drop = FALSE], na.rm = TRUE)/2) # freq.tmp is a matrix, each column is a strata, and each row is a varirant 
            if (length(dim(freq.tmp)) == 2) freq_strata <- apply(freq.tmp, 1, range) else freq_strata <- as.matrix(range(freq.tmp)) # freq_strata is the range of allele freq across strata.list
            include <- include & !is.na(freq_strata[1,]) & !is.na(freq_strata[2,]) & freq_strata[1,] >= AF.strata.range[1] & freq_strata[2,] <= AF.strata.range[2]
            rm(freq.tmp)
          }
          n.p <- sum(include)
          if(n.p == 0) next
          tmp.group.info <- tmp.group.info[include, , drop = FALSE]
          miss <- miss[include]
          freq <- freq[include]
          geno <- geno[, include, drop = FALSE]
          if(!is.null(strata)) freq_strata <- freq_strata[, include, drop = FALSE]
          N <- nrow(geno) - colSums(is.na(geno))
          if(sum(tmp.group.info$flip) > 0) {
            freq[tmp.group.info$flip==1] <- 1 - freq[tmp.group.info$flip==1]
            geno[, tmp.group.info$flip==1] <- 2 - geno[, tmp.group.info$flip==1]
            if(!is.null(strata)) freq_strata[, tmp.group.info$flip==1] <- 1 - freq_strata[, tmp.group.info$flip==1]
          }
          if(max(miss)>0) {
            miss.idx <- which(is.na(geno))
            geno[miss.idx] <- if(missing.method=="impute2mean") 2*freq[ceiling(miss.idx/nrow(geno))] else 0
          }
          if(IV | IF | JV | JF | JD) {
            K <- do.call(cbind, sapply((1+qi):ncol(E), function(xx) geno*E[,xx], simplify = FALSE), envir = environment())
            SK <- as.vector(crossprod(K,residuals))
          }
          if(!is.null(interaction.covariates) && (IV | IF | JV | JF | JD)) {
            geno <- cbind(geno, do.call(cbind, sapply(1:qi, function(xx) geno*E[,xx], simplify = FALSE), envir = environment()))
          }
          U <- as.vector(crossprod(geno, residuals))
          if(inherits(null.obj, "glmmkin.multi")) {
            geno <- Diagonal(n = n.pheno) %x% geno
            if (IV | IF | JV | JF | JD) K <- Diagonal(n = n.pheno) %x% K
          }
          if(!is.null(null.obj$P)) {
            PG <- crossprod(null.obj$P, geno)
          } else {
            GSigma_iX <- crossprod(geno, null.obj$Sigma_iX)
            PG <- crossprod(null.obj$Sigma_i, geno) - tcrossprod(null.obj$Sigma_iX, tcrossprod(GSigma_iX, null.obj$cov))
          }
          V <- as.matrix(crossprod(geno, PG))
          if(MV | MF | JV | JF | JD) c1 <- rep(1:n.p,n.pheno)+rep((0:(n.pheno-1))*n.p*(1+qi), each=n.p) # index for GPG and row.index for GPC
          if(!is.null(interaction.covariates) && (JV | JF | JD)) {
            c2 <- rep((n.p+1):(n.p*(1+qi)),n.pheno)+rep((0:(n.pheno-1))*n.p*(1+qi), each=n.p*qi) # index for CPC and col.index for GPC
            CPC_i <- try(solve(V[c2,c2]), silent = TRUE)
            if(inherits(CPC_i, "try-error") || any(diag(CPC_i)<0)) CPC_i <- try(MASS::ginv(V[c2,c2]), silent = TRUE)
            if(inherits(CPC_i, "try-error")) next
            U.adj <- U[c1] - tcrossprod(tcrossprod(V[c1,c2],CPC_i),t(U[c2]))
            V.adj <- V[c1,c1] - tcrossprod(tcrossprod(V[c1,c2],CPC_i),V[c1,c2])
          }
          if(IV | IF | JV | JF | JD) {
            if(!is.null(null.obj$P)) {
              KPK <- crossprod(K,crossprod(null.obj$P,K))
            } else {
              KSigma_iX <- crossprod(K, null.obj$Sigma_iX)
              KPK <- crossprod(K, crossprod(null.obj$Sigma_i, K)) - tcrossprod(KSigma_iX, tcrossprod(KSigma_iX, null.obj$cov))
            }
            V_i <- try(solve(V), silent = TRUE)
            if(inherits(V_i, "try-error") || any(diag(V_i)<0)) V_i <- try(MASS::ginv(V), silent = TRUE)
            if(inherits(V_i, "try-error")) next
            KPG <- crossprod(K,PG)
            IV.U <- SK - tcrossprod(tcrossprod(KPG,V_i),t(U))
            IV.V <- KPK - tcrossprod(tcrossprod(KPG,V_i),KPG)
          }
          if(!is.null(meta.file.prefix)) {
            score <- cbind(tmp.group.info[,c("group","chr","pos","ref","alt")], N, miss, freq, U, matrix(crossprod(K,residuals), nrow = n.p, ncol = ei))
            # odr <- order(as.numeric(row.names(tmp.group.info)))
            # score <- score[odr, ]
            # odr <- rep(odr, (1+ei))+rep(0:ei*length(odr), each=length(odr))
            # COV <-rbind(cbind(V, t(KPG)), cbind(KPG, KPK))[odr, odr] 
            COV <-rbind(cbind(V, t(KPG)), cbind(KPG, KPK))
            write.table(score, meta.file.score, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
            writeBin(COV[lower.tri(COV, diag = TRUE)], meta.file.cov.handle, size = 4)
          }
          if(use.minor.allele) {
            tmp.group.info$weight[freq > 0.5] <- -tmp.group.info$weight[freq > 0.5]
            if(!is.null(strata)) freq_strata[, freq > 0.5] <- 1 - freq_strata[, freq > 0.5]
            freq[freq > 0.5] <- 1 - freq[freq > 0.5]
          }
          weights <- rep(tmp.group.info$weight * MAF.weights.beta.fun(freq, MAF.weights.beta[1], MAF.weights.beta[2]), n.pheno)
          n.variants[i] <- n.p
          miss.min[i] <- min(miss)
          miss.mean[i] <- mean(miss)
          miss.max[i] <- max(miss)
          freq.min[i] <- min(freq)
          freq.mean[i] <- mean(freq)
          freq.max[i] <- max(freq)
          if(!is.null(strata)) {
            freq.strata.min[i] <- min(freq_strata)
            freq.strata.max[i] <- max(freq_strata)
          }
          if(MV | MF | JV | JF | JD) {
            U <- U[c1]*weights
            V <- t(V[c1,c1]*weights)*weights
          }
          if(!is.null(interaction.covariates) && (JV | JF | JD)) {
            U.adj <- U.adj*weights
            V.adj <- t(V.adj*weights)*weights
          }
          if(IV | IF | JV | JF | JD) {
            IV.U <- IV.U*rep(weights, ei)
            IV.V <- t(IV.V*rep(weights, ei))*rep(weights, ei)
          }
          if(MV | JV) MV.pval[i] <- tryCatch(.quad_pval(U = U, V = V, method = method), error = function(e) { NA })
          if(IV | JV) IV.pval[i] <- tryCatch(.quad_pval(U = IV.U, V = IV.V, method = method), error = function(e) { NA })
          if(JV && is.null(interaction.covariates)) JV.pval[i] <- tryCatch(fisher_pval(c(MV.pval[i], IV.pval[i])), error = function(e) { MV.pval[i] })
          if(JV && !is.null(interaction.covariates)) {
            MV.adj.pval <- tryCatch(.quad_pval(U = U.adj, V = V.adj, method = method), error = function(e) { NA })
            JV.pval[i] <- tryCatch(fisher_pval(c(MV.adj.pval, IV.pval[i])), error = function(e) { MV.adj.pval })
          }
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
          if((JF | JD) && !is.null(interaction.covariates)) {
            MF.BU.adj <- sum(U.adj)
            MF.BV.adj <- sum(V.adj)
            MF.Bp.adj <- pchisq(MF.BU.adj^2/MF.BV.adj,df=1,lower.tail=FALSE)
            V.adj.rowSums <- rowSums(V.adj)
            MF.U.adj <- U.adj  - V.adj.rowSums * MF.BU.adj / MF.BV.adj 
            MF.V.adj  <- V.adj  - tcrossprod(V.adj.rowSums) / MF.BV.adj 
            if(MF.BV.adj == 0 | mean(abs(MF.V.adj)) < sqrt(.Machine$double.eps)) MF.adj.p <- NA
            else MF.adj.p <- tryCatch(.quad_pval(U = MF.U.adj, V = MF.V.adj, method = method), error = function(e) { NA })
            MF.adj.pval <- tryCatch(fisher_pval(c(MF.Bp.adj, MF.adj.p)), error = function(e) { MF.Bp.adj })
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
          if(JF && is.null(interaction.covariates)) JF.pval[i] <- tryCatch(fisher_pval(c(MF.Bp, MF.p, IF.Bp, IF.p)), error = function(e) { MF.Bp })
          if(JF && !is.null(interaction.covariates)) JF.pval[i] <- tryCatch(fisher_pval(c(MF.Bp.adj, MF.adj.p, IF.Bp, IF.p)), error = function(e) { MF.Bp.adj })
          if(JD && is.null(interaction.covariates)) JD.pval[i] <- tryCatch(fisher_pval(c(MF.pval[i], IF.pval[i])), error = function(e) { MF.pval[i] })
          if(JD && !is.null(interaction.covariates)) JD.pval[i] <- tryCatch(fisher_pval(c(MF.adj.pval, IF.pval[i])), error = function(e) { MF.adj.pval })
          rm(geno)
          if(Garbage.Collection) gc()
        }
        if(!is.null(meta.file.prefix)) close(meta.file.cov.handle)
        tmp.out <- data.frame(group=unique(group.info$group)[idx], n.variants=n.variants, miss.min=miss.min, miss.mean=miss.mean, miss.max=miss.max, freq.min=freq.min, freq.mean=freq.mean, freq.max=freq.max,freq.strata.min=freq.strata.min, freq.strata.max=freq.strata.max)
        if(MV | JV) tmp.out$MV.pval <- MV.pval
        if(MF | JF | JD) tmp.out$MF.pval <- MF.pval
        if(IV | JV) tmp.out$IV.pval <- IV.pval
        if(IF | JF | JD) tmp.out$IF.pval <- IF.pval
        if(JV) tmp.out$JV.pval <- JV.pval
        if(JF) tmp.out$JF.pval <- JF.pval
        if(JD) tmp.out$JD.pval <- JD.pval
        tmp.out
      }
    } else {
      n.groups   <- n.groups.all
      n.variants <- rep(0,n.groups)
      miss.min   <- rep(NA,n.groups)
      miss.mean  <- rep(NA, n.groups)
      miss.max   <- rep(NA, n.groups)
      freq.min   <- rep(NA, n.groups)
      freq.mean  <- rep(NA, n.groups)
      freq.max   <- rep(NA, n.groups)
      freq.strata.min <- rep(NA, n.groups)
      freq.strata.max <- rep(NA, n.groups)
      if(MV | JV) MV.pval <- rep(NA, n.groups)
      if(IV | JV) IV.pval <- rep(NA, n.groups)
      if(JV) JV.pval      <- rep(NA, n.groups)
      if(MF | JF | JD) MF.pval <- rep(NA, n.groups)
      if(IF | JF | JD) IF.pval <- rep(NA, n.groups)
      if(JF) JF.pval <- rep(NA, n.groups)
      if(JD) JD.pval <- rep(NA, n.groups)
      if(!is.null(meta.file.prefix)) {
        if(inherits(null.obj, "glmmkin.multi")) stop("Error: meta-analysis not supported yet for multiple phenotypes.")
        if(!is.null(interaction.covariates)) stop("Error: meta-analysis not supported yet for interaction.covariates.")
        if(.Platform$endian!="little") stop("Error: platform must be little endian.")
        meta.file.score <- paste0(meta.file.prefix, ".score.1")
        meta.file.cov <- paste0(meta.file.prefix, ".cov.1")
        write.table(t(c("group", "chr", "pos", "ref", "alt", "N", "missrate", "altfreq", "G.SCORE", paste("K.SCORE.", 1:ei, sep=""))), meta.file.score, quote = FALSE, row.names = FALSE, col.names = FALSE)
        meta.file.cov.handle <- file(meta.file.cov, "wb")
        writeBin(as.numeric(ei), meta.file.cov.handle, size = 4)
        writeBin(interaction, meta.file.cov.handle, size = 4)
      }
      for(i in 1:n.groups) {
        tmp.idx <- group.idx.start[i]:group.idx.end[i]
        tmp.group.info <- group.info[tmp.idx, , drop = FALSE]
        if (bgenInfo$Layout == 2) {
          geno <- .Call("magee_bgen13", geno.file, tmp.group.info$variant.idx, ptr, select, bgenInfo$Compression, n)
        } else {
          geno <- .Call("magee_bgen11", geno.file, tmp.group.info$variant.idx, ptr, select, bgenInfo$Compression, bgenInfo$N, n)
        }
        miss <- colMeans(is.na(geno))
        freq <- colMeans(geno, na.rm = TRUE)/2
        include <- (miss <= miss.cutoff & ((freq >= MAF.range[1] & freq <= MAF.range[2]) | (freq >= 1-MAF.range[2] & freq <= 1-MAF.range[1])))
        if(any(duplicated(null.obj$id_include))) geno <- crossprod(J, geno)
        if(!is.null(strata)) { # E is not continuous
          freq.tmp <- sapply(strata.list, function(x) colMeans(geno[x, , drop = FALSE], na.rm = TRUE)/2) # freq.tmp is a matrix, each column is a strata, and each row is a varirant 
          if (length(dim(freq.tmp)) == 2) freq_strata <- apply(freq.tmp, 1, range) else freq_strata <- as.matrix(range(freq.tmp)) # freq_strata is the range of allele freq across strata.list
          include <- include & !is.na(freq_strata[1,]) & !is.na(freq_strata[2,]) & freq_strata[1,] >= AF.strata.range[1] & freq_strata[2,] <= AF.strata.range[2]
          rm(freq.tmp)
        }
        n.p <- sum(include)
        if(n.p == 0) next
        tmp.group.info <- tmp.group.info[include, , drop = FALSE]
        miss <- miss[include]
        freq <- freq[include]
        geno <- geno[, include, drop = FALSE]
        if(!is.null(strata)) freq_strata <- freq_strata[, include, drop = FALSE]
        N <- nrow(geno) - colSums(is.na(geno))
        if(sum(tmp.group.info$flip) > 0) {
          freq[tmp.group.info$flip==1] <- 1 - freq[tmp.group.info$flip==1]
          geno[, tmp.group.info$flip==1] <- 2 - geno[, tmp.group.info$flip==1]
          if(!is.null(strata)) freq_strata[, tmp.group.info$flip==1] <- 1 - freq_strata[, tmp.group.info$flip==1]
        }
        if(max(miss)>0) {
          miss.idx <- which(is.na(geno))
          geno[miss.idx] <- if(missing.method=="impute2mean") 2*freq[ceiling(miss.idx/nrow(geno))] else 0
        }
        if(IV | IF | JV | JF | JD) {
          K <- do.call(cbind, sapply((1+qi):ncol(E), function(xx) geno*E[,xx], simplify = FALSE), envir = environment())
          SK <- as.vector(crossprod(K,residuals))
        }
        if(!is.null(interaction.covariates) && (IV | IF | JV | JF | JD)) {
          geno <- cbind(geno, do.call(cbind, sapply(1:qi, function(xx) geno*E[,xx], simplify = FALSE), envir = environment()))
        }
        U <- as.vector(crossprod(geno, residuals))
        if(inherits(null.obj, "glmmkin.multi")) {
          geno <- Diagonal(n = n.pheno) %x% geno
          if (IV | IF | JV | JF | JD) K <- Diagonal(n = n.pheno) %x% K
        }
        if(!is.null(null.obj$P)) {
          PG <- crossprod(null.obj$P, geno)
        } else {
          GSigma_iX <- crossprod(geno, null.obj$Sigma_iX)
          PG <- crossprod(null.obj$Sigma_i, geno) - tcrossprod(null.obj$Sigma_iX, tcrossprod(GSigma_iX, null.obj$cov))
        }
        V <- as.matrix(crossprod(geno, PG))
        if(MV | MF | JV | JF | JD) c1 <- rep(1:n.p,n.pheno)+rep((0:(n.pheno-1))*n.p*(1+qi), each=n.p) # index for GPG and row.index for GPC
        if(!is.null(interaction.covariates) && (JV | JF | JD)) {
          c2 <- rep((n.p+1):(n.p*(1+qi)),n.pheno)+rep((0:(n.pheno-1))*n.p*(1+qi), each=n.p*qi) # index for CPC and col.index for GPC
          CPC_i <- try(solve(V[c2,c2]), silent = TRUE)
          if(inherits(CPC_i, "try-error") || any(diag(CPC_i)<0)) CPC_i <- try(MASS::ginv(V[c2,c2]), silent = TRUE)
          if(inherits(CPC_i, "try-error")) next
          U.adj <- U[c1] - tcrossprod(tcrossprod(V[c1,c2],CPC_i),t(U[c2]))
          V.adj <- V[c1,c1] - tcrossprod(tcrossprod(V[c1,c2],CPC_i),V[c1,c2])
        }
        if(IV | IF | JV | JF | JD) {
          if(!is.null(null.obj$P)) {
            KPK <- crossprod(K,crossprod(null.obj$P,K))
          } else {
            KSigma_iX <- crossprod(K, null.obj$Sigma_iX)
            KPK <- crossprod(K, crossprod(null.obj$Sigma_i, K)) - tcrossprod(KSigma_iX, tcrossprod(KSigma_iX, null.obj$cov))
          }
          V_i <- try(solve(V), silent = TRUE)
          if(inherits(V_i,"try-error") || any(diag(V_i)<0)) V_i <- try(MASS::ginv(V), silent = TRUE)
          if(inherits(V_i, "try-error")) next
          KPG <- crossprod(K,PG)
          IV.U <- SK - tcrossprod(tcrossprod(KPG,V_i),t(U))
          IV.V <- KPK - tcrossprod(tcrossprod(KPG,V_i),KPG)
        }
        if(!is.null(meta.file.prefix)) {
          score <- cbind(tmp.group.info[,c("group","chr","pos","ref","alt")], N, miss, freq, U, matrix(crossprod(K,residuals), nrow = n.p, ncol = ei))
          # odr <- order(as.numeric(row.names(tmp.group.info)))
          # score <- score[odr, ]
          # odr <- rep(odr, (1+ei))+rep(0:ei*length(odr), each=length(odr))
          # COV <-rbind(cbind(V, t(KPG)), cbind(KPG, KPK))[odr, odr] 
          COV <-rbind(cbind(V, t(KPG)), cbind(KPG, KPK))
          write.table(score, meta.file.score, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
          writeBin(COV[lower.tri(COV, diag = TRUE)], meta.file.cov.handle, size = 4)
        }
        if(use.minor.allele) {
          tmp.group.info$weight[freq > 0.5] <- -tmp.group.info$weight[freq > 0.5]
          if(!is.null(strata)) freq_strata[, freq > 0.5] <- 1 - freq_strata[, freq > 0.5]
          freq[freq > 0.5] <- 1 - freq[freq > 0.5]
        }
        weights <- rep(tmp.group.info$weight * MAF.weights.beta.fun(freq, MAF.weights.beta[1], MAF.weights.beta[2]), n.pheno)
        n.variants[i] <- n.p
        miss.min[i] <- min(miss)
        miss.mean[i] <- mean(miss)
        miss.max[i] <- max(miss)
        freq.min[i] <- min(freq)
        freq.mean[i] <- mean(freq)
        freq.max[i] <- max(freq)
        if(!is.null(strata)) {
          freq.strata.min[i] <- min(freq_strata)
          freq.strata.max[i] <- max(freq_strata)
        }
        if(MV | MF | JV | JF | JD) {
          U <- U[c1]*weights
          V <- t(V[c1,c1]*weights)*weights
        }
        if(!is.null(interaction.covariates) && (JV | JF | JD)) {
          U.adj <- U.adj*weights
          V.adj <- t(V.adj*weights)*weights
        }
        if(IV | IF | JV | JF | JD) {
          IV.U <- IV.U*rep(weights, ei)
          IV.V <- t(IV.V*rep(weights, ei))*rep(weights, ei)
        }
        if(MV | JV) MV.pval[i] <- tryCatch(.quad_pval(U = U, V = V, method = method), error = function(e) { NA })
        if(IV | JV) IV.pval[i] <- tryCatch(.quad_pval(U = IV.U, V = IV.V, method = method), error = function(e) { NA })
        if(JV && is.null(interaction.covariates)) JV.pval[i] <- tryCatch(fisher_pval(c(MV.pval[i], IV.pval[i])), error = function(e) { MV.pval[i] })
        if(JV && !is.null(interaction.covariates)) {
          MV.adj.pval <- tryCatch(.quad_pval(U = U.adj, V = V.adj, method = method), error = function(e) { NA })
          JV.pval[i] <- tryCatch(fisher_pval(c(MV.adj.pval, IV.pval[i])), error = function(e) { MV.adj.pval })
        }
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
        if((JF | JD) && !is.null(interaction.covariates)) {
          MF.BU.adj <- sum(U.adj)
          MF.BV.adj <- sum(V.adj)
          MF.Bp.adj <- pchisq(MF.BU.adj^2/MF.BV.adj,df=1,lower.tail=FALSE)
          V.adj.rowSums <- rowSums(V.adj)
          MF.U.adj <- U.adj  - V.adj.rowSums * MF.BU.adj / MF.BV.adj 
          MF.V.adj  <- V.adj  - tcrossprod(V.adj.rowSums) / MF.BV.adj 
          if(MF.BV.adj == 0 | mean(abs(MF.V.adj)) < sqrt(.Machine$double.eps)) MF.adj.p <- NA
          else MF.adj.p <- tryCatch(.quad_pval(U = MF.U.adj, V = MF.V.adj, method = method), error = function(e) { NA })
          MF.adj.pval <- tryCatch(fisher_pval(c(MF.Bp.adj, MF.adj.p)), error = function(e) { MF.Bp.adj })
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
        if(JF && is.null(interaction.covariates)) JF.pval[i] <- tryCatch(fisher_pval(c(MF.Bp, MF.p, IF.Bp, IF.p)), error = function(e) { MF.Bp })
        if(JF && !is.null(interaction.covariates)) JF.pval[i] <- tryCatch(fisher_pval(c(MF.Bp.adj, MF.adj.p, IF.Bp, IF.p)), error = function(e) { MF.Bp.adj })
        if(JD && is.null(interaction.covariates)) JD.pval[i] <- tryCatch(fisher_pval(c(MF.pval[i], IF.pval[i])), error = function(e) { MF.pval[i] })
        if(JD && !is.null(interaction.covariates)) JD.pval[i] <- tryCatch(fisher_pval(c(MF.adj.pval, IF.pval[i])), error = function(e) { MF.adj.pval })
        rm(geno, K)
        if(Garbage.Collection) gc()
      }
      if(!is.null(meta.file.prefix)) close(meta.file.cov.handle)
      out <- data.frame(group=unique(group.info$group), n.variants=n.variants, miss.min=miss.min, miss.mean=miss.mean, miss.max=miss.max, freq.min=freq.min, freq.mean=freq.mean, freq.max=freq.max,freq.strata.min=freq.strata.min, freq.strata.max=freq.strata.max)
      if(MV | JV) out$MV.pval <- MV.pval
      if(MF | JF | JD) out$MF.pval <- MF.pval
      if(IV | JV) out$IV.pval <- IV.pval
      if(IF | JF | JD) out$IF.pval <- IF.pval
      if(JV) out$JV.pval <- JV.pval
      if(JF) out$JF.pval <- JF.pval
      if(JD) out$JD.pval <- JD.pval
    }
    .Call("clear_exptr", ptr)
    return(out[match(groups, out$group),])
  }
}

.Q_pval <- function(Q, lambda, method = "davies") {
  if(method == "davies") {
    tmp <- try(suppressWarnings(CompQuadForm::davies(q = Q, lambda = lambda, acc = 1e-6)))
    if(inherits(tmp, "try-error") || tmp$ifault > 0 || tmp$Qq <= 1e-5 || tmp$Qq >= 1) method <- "kuonen"
    else return(tmp$Qq)
  }
  if(method == "kuonen") {
    pval <- try(.pKuonen(x = Q, lambda = lambda))
    if(inherits(pval, "try-error") || is.na(pval)) method <- "liu"
    else return(pval)
  }
  if(method == "liu") {
    pval <- try(CompQuadForm::liu(q = Q, lambda = lambda))
    if(inherits(pval, "try-error")) warning("Method \"liu\" failed...\nQ: ", Q, "\nlambda: ", paste(lambda, collapse = ", "), call. = FALSE)
    else return(pval)
  }
  return(NA)
}

.quad_pval <- function(U, V, method = "davies") {
  Q <- sum(U^2)
  lambda <- eigen(V, only.values = TRUE, symmetric=TRUE)$values
  lambda <- lambda[lambda > 0]
  pval <- .Q_pval(Q, lambda, method = method)
  return(pval)
}

.pKuonen<-function (x, lambda, delta = rep(0, length(lambda)), df = rep(1, length(lambda)))
{
  delta <- delta[lambda != 0]
  df <- df[lambda != 0]
  lambda <- lambda[lambda != 0]
  if(length(lambda) != length(delta)) stop("Error: inconsistent length in lambda and delta!")
  if(length(lambda) != length(df)) stop("Error: inconsistent length in lambda and df!")
  if (length(lambda) == 1) {
    return(pchisq(x/lambda, df = df, ncp = delta, lower.tail = FALSE))
  }
  d <- max(lambda)
  lambda <- lambda/d
  x <- x/d
  k0 <- function(zeta) {
    -sum(df * log(1 - 2 * zeta * lambda))/2 + sum((delta * lambda *
                                                     zeta)/(1 - 2 * zeta * lambda))
  }
  kprime0 <- function(zeta) {
    sapply(zeta, function(zz) {
      sum(((delta + df) * lambda)/(1 - 2 * zz * lambda) + 2 * (delta *
                                                                 zz * lambda^2)/(1 - 2 * zz * lambda)^2)
    })
  }
  kpprime0 <- function(zeta) {
    sum((2 * (2 * delta + df) * lambda^2)/(1 - 2 * zeta * lambda)^2 + 8 *
          delta * zeta * lambda^3/(1 - 2 * zeta * lambda)^3)
  }
  if (any(lambda < 0)) {
    lmin <- max(1/(2 * lambda[lambda < 0])) * 0.99999
  }
  else if (x > sum((df+delta)*lambda)) {
    lmin <- -0.01
  }
  else {
    lmin <- -length(lambda)*max(df+delta)/(2 * x)
  }
  lmax <- min(1/(2 * lambda[lambda > 0])) * 0.99999
  hatzeta <- uniroot(function(zeta) kprime0(zeta) - x, lower = lmin,
                     upper = lmax, tol = 1e-08)$root
  w <- sign(hatzeta) * sqrt(2 * (hatzeta * x - k0(hatzeta)))
  v <- hatzeta * sqrt(kpprime0(hatzeta))
  if (abs(hatzeta) < 1e-04)
    NA
  else pnorm(w + log(v/w)/w, lower.tail = FALSE)
}

MAF.weights.beta.fun <- function(freq, beta1, beta2) {
  freq[freq > 0.5] <- 1 - freq[freq > 0.5]
  ifelse(freq <= 0, 0, dbeta(freq, beta1, beta2))
}

fisher_pval <- function(p) {
  is.valid.p <- !is.na(p) & p > 0 & p <= 1
  if(sum(is.valid.p) == 0) return(NA)
  p <- p[is.valid.p]
  pchisq(-2*sum(log(p)), df = 2*length(p), lower.tail = FALSE)
}

MAGEE.prep <- function(null.obj, interaction, geno.file, group.file, interaction.covariates = NULL, group.file.sep = "\t", auto.flip = FALSE)
{
  if(!grepl("\\.gds$", geno.file[1])) stop("Error: currently only .gds format is supported in geno.file for MAGEE.prep!")
  if(!inherits(null.obj, c("glmmkin", "glmmkin.multi"))) stop("Error: null.obj must be a class glmmkin or glmmkin.multi object!")
  n.pheno <- null.obj$n.pheno
  residuals <- null.obj$scaled.residuals
  n <- length(unique(null.obj$id_include))
  
  qi <- length(interaction.covariates) # number of covariates with interaction effects but we don't test
  ei <- length(interaction) # number of covariates with interaction effects that we want to test
  if(inherits(interaction,"character")) {
    if (is.null(interaction.covariates)) {
      if (!all(interaction %in% colnames(null.obj$X))) {stop("there are interactions not in column name of covariate matrix.")}
      E <- as.matrix(null.obj$X[,interaction])
    } else {
      if (any(interaction.covariates %in% interaction)) {stop("there are interaction.covariates also specified as interaction.")}
      interaction <- c(interaction.covariates, interaction)
      if (!all(interaction %in% colnames(null.obj$X))) {stop("there are interaction or interaction.covariates not in column name of covariate matrix.")}
      E <- as.matrix(null.obj$X[,interaction])
    }
  } else {
    if (is.null(interaction.covariates)) {
      E <- as.matrix(null.obj$X[,interaction+1])
    } else {
      if (any(interaction.covariates %in% interaction)) {stop("there are interaction.covariates also specified as interaction.")}
      interaction <- c(interaction.covariates, interaction)
      E <- as.matrix(null.obj$X[,interaction+1])
    }
  }
  interaction <- as.character(interaction)
  n.E <- as.numeric(dim(E)[2]) # n.E = qi + ei
  
  if (!inherits(geno.file, "SeqVarGDSClass")) {
    gds <- SeqArray::seqOpen(geno.file) 
  } else {
    gds <- geno.file
  }
  
  sample.id <- SeqArray::seqGetData(gds, "sample.id")
  if(any(is.na(match(null.obj$id_include, sample.id)))) warning("Check your data... Some individuals in null.obj$id_include are missing in sample.id of geno.file!", call. = FALSE)
  sample.id <- sample.id[sample.id %in% null.obj$id_include]
  if(length(sample.id) == 0) stop("Error: null.obj$id_include does not match sample.id in geno.file!")
  if(any(duplicated(null.obj$id_include))) {
    match.id <- null.obj$id_include %in% sample.id
    null.obj$id_include <- null.obj$id_include[match.id]
    J <- t(sparseMatrix(i=1:length(null.obj$id_include), j=match(null.obj$id_include, unique(null.obj$id_include)[match(sample.id, unique(null.obj$id_include))]), x=1))
  } else {
    match.id <- match(sample.id, null.obj$id_include)
    J <- NULL
  }
  E <- as.matrix(E[match.id, , drop = FALSE])
  E <- scale(E, scale = FALSE)
  if(inherits(null.obj, "glmmkin.multi")) {
    residuals <- residuals[match.id, , drop = FALSE]
    match.id <- rep(match.id, n.pheno) + rep((0:(n.pheno-1))*n, each = length(match.id))
  } else {
    residuals <- residuals[match.id]
  }
  if(!is.null(null.obj$P)) {
    null.obj$P <- null.obj$P[match.id, match.id]
  }else {
    null.obj$Sigma_iX <- null.obj$Sigma_iX[match.id, , drop = FALSE]
    null.obj$Sigma_i <- null.obj$Sigma_i[match.id, match.id]
  }
  strata <- apply(E, 1, paste, collapse = ":")
  strata <- if(length(unique(strata))>length(strata)/100) NULL else as.numeric(as.factor(strata))
  if(!is.null(strata)) {
    strata.list <- lapply(unique(strata), function(x) which(strata==x))
  } else {
    strata.list <- NULL
  }
  variant.idx <- SeqArray::seqGetData(gds, "variant.id")
  chr <- SeqArray::seqGetData(gds, "chromosome")
  pos <- SeqArray::seqGetData(gds, "position")
  alleles.list <- strsplit(SeqArray::seqGetData(gds, "allele"), ",")
  ref <- unlist(lapply(alleles.list, function(x) x[1]))
  alt <- unlist(lapply(alleles.list, function(x) paste(x[-1], collapse=",")))
  rm(alleles.list); gc()
  if (!inherits(geno.file, "SeqVarGDSClass")) {
    SeqArray::seqClose(gds)
  }
  variant.id <- paste(chr, pos, ref, alt, sep = ":")
  rm(chr, pos, ref, alt); gc()
  group.info <- try(fread(group.file, header = FALSE, data.table = FALSE, col.names = c("group", "chr", "pos", "ref", "alt", "weight"), colClasses = c("character","character","integer","character","character","numeric"), sep = group.file.sep), silent = TRUE)
  if (inherits(group.info, "try-error")) {
    stop("Error: cannot read group.file!")
  }
  variant.id1 <- paste(group.info$chr, group.info$pos, group.info$ref, group.info$alt, sep = ":")
  is.duplicated <- duplicated(paste(group.info$group, variant.id1, sep = ":"))
  group.info <- group.info[!is.duplicated, ]
  variant.id1 <- variant.id1[!is.duplicated]
  rm(is.duplicated)
  variant.idx1 <- variant.idx[match(variant.id1, variant.id)]
  group.info$variant.idx <- variant.idx1
  group.info$flip <- 0
  if(auto.flip) {
    message("Automatic allele flipping enabled...\nVariants matching alt/ref but not ref/alt alleles will also be included, with flipped effects")
    variant.id2 <- paste(group.info$chr, group.info$pos, group.info$alt, group.info$ref, sep = ":")
    variant.idx2 <- variant.idx[match(variant.id2, variant.id)]
    if(any(!is.na(variant.idx1) & !is.na(variant.idx2))) {
      tmp.dups <- which(!is.na(variant.idx1) & !is.na(variant.idx2))
      message("The following ambiguous variants were found:")
      message(paste(variant.id1[tmp.dups], collapse = ", ")) 
      warning("Both variants with alleles ref/alt and alt/ref were present at the same position and coding should be double checked!\nFor these variants, only those with alleles ref/alt were used in the analysis...", call. = FALSE)
      variant.idx2[tmp.dups] <- NA
      rm(tmp.dups)
    }
    group.info$flip <- 1*(!is.na(variant.idx2))
    group.info$variant.idx[!is.na(variant.idx2)] <- variant.idx2[!is.na(variant.idx2)]
    rm(variant.id2, variant.idx2)
  }
  rm(variant.id, variant.id1, variant.idx1); gc()
  group.info <- subset(group.info, !is.na(variant.idx))
  groups <- unique(group.info$group)
  n.groups.all <- length(groups)
  group.info$group.idx <- as.numeric(factor(group.info$group))
  group.info <- group.info[order(group.info$group.idx, group.info$variant.idx), ]
  group.idx.end <- findInterval(1:n.groups.all, group.info$group.idx)
  group.idx.start <- c(1, group.idx.end[-n.groups.all] + 1)
  out <- list(null.obj = null.obj, interaction = interaction, E = E, geno.file = geno.file, group.file = group.file, group.file.sep = group.file.sep, J = J, strata = strata, strata.list = strata.list, auto.flip = auto.flip, residuals = residuals, sample.id = sample.id, group.info = group.info, groups = groups, group.idx.start = group.idx.start, group.idx.end = group.idx.end, ei = ei, qi = qi)
  class(out) <- "MAGEE.prep"
  return(out)
}

MAGEE.lowmem <- function(MAGEE.prep.obj, geno.file = NULL, meta.file.prefix = NULL, MAF.range = c(1e-7, 0.5), AF.strata.range = c(0, 1), MAF.weights.beta = c(1, 25), miss.cutoff = 1, missing.method = "impute2mean", method = "davies", tests = "JF", use.minor.allele = FALSE, Garbage.Collection = FALSE, is.dosage = FALSE, ncores = 1)
{
  if(!inherits(MAGEE.prep.obj, "MAGEE.prep")) stop("Error: MAGEE.prep.obj must be a class MAGEE.prep object!")
  is.Windows <- Sys.info()["sysname"] == "Windows"
  if(is.Windows && ncores > 1) {
    warning("The package doMC is not available on Windows... Switching to single thread...", call. = FALSE)
    ncores <- 1
  }
  null.obj <- MAGEE.prep.obj$null.obj
  interaction <- MAGEE.prep.obj$interaction
  E <- MAGEE.prep.obj$E
  if(is.null(geno.file)) {
    geno.file <- MAGEE.prep.obj$geno.file
  }
  residuals <- MAGEE.prep.obj$residuals
  sample.id <- MAGEE.prep.obj$sample.id
  group.info <- MAGEE.prep.obj$group.info
  groups <- MAGEE.prep.obj$groups
  n.groups.all <- length(groups)
  group.idx.start <- MAGEE.prep.obj$group.idx.start
  group.idx.end <- MAGEE.prep.obj$group.idx.end
  J <- MAGEE.prep.obj$J
  strata <- MAGEE.prep.obj$strata
  strata.list <- MAGEE.prep.obj$strata.list
  ei <- MAGEE.prep.obj$ei
  qi <- MAGEE.prep.obj$qi
  
  rm(MAGEE.prep.obj); gc()
  if(!inherits(null.obj, c("glmmkin", "glmmkin.multi"))) stop("Error: null.obj must be a class glmmkin or glmmkin.multi object!")
  n.pheno <- null.obj$n.pheno
  n.E <- as.numeric(dim(E)[2])
  missing.method <- try(match.arg(missing.method, c("impute2mean", "impute2zero")))
  if(inherits(missing.method, "try-error")) stop("Error: \"missing.method\" must be \"impute2mean\" or \"impute2zero\".")
  if(any(!tests %in% c("MV", "MF", "IV", "IF", "JV", "JF", "JD"))) stop("Error: \"tests\" should only include \"MV\" for the main effect variance component test, \"MF\" for the main effect combined test of the burden and variance component tests using Fisher\'s method, \"IV\" for the interaction variance component test, \"IF\" for the interaction combined test of the burden and variance component tests using Fisher\'s method, \"JV\" for the joint variance component test for main effect and interaction, \"JF\" for the joint combined test of the burden and variance component tests for main effect and interaction using Fisher\'s method, or \"JD\" for the joint combined test of the burden and variance component tests for main effect and interaction using double Fisher\'s method.")
  MV <- "MV" %in% tests
  MF <- "MF" %in% tests
  IV <- "IV" %in% tests
  IF <- "IF" %in% tests
  JV <- "JV" %in% tests
  JF <- "JF" %in% tests
  JD <- "JD" %in% tests
  
  if(!grepl("\\.gds$", geno.file[1])) stop("Error: currently only .gds format is supported in geno.file for MAGEE.lowmem!")
  ncores <- min(c(ncores, parallel::detectCores(logical = TRUE)))
  if(ncores > 1) {
    doMC::registerDoMC(cores = ncores)
    n.groups.percore <- (n.groups.all-1) %/% ncores + 1
    n.groups.percore_1 <- n.groups.percore * ncores - n.groups.all
    b <- NULL
    out <- foreach(b=1:ncores, .combine=rbind, .multicombine = TRUE, .inorder=FALSE, .options.multicore = list(preschedule = FALSE, set.seed = FALSE)) %dopar% {
      idx <- if(b <= n.groups.percore_1) ((b-1)*(n.groups.percore-1)+1):(b*(n.groups.percore-1)) else (n.groups.percore_1*(n.groups.percore-1)+(b-n.groups.percore_1-1)*n.groups.percore+1):(n.groups.percore_1*(n.groups.percore-1)+(b-n.groups.percore_1)*n.groups.percore)
      n.groups <- length(idx)
      if (!inherits(geno.file, "SeqVarGDSClass")) {
        gds <- SeqArray::seqOpen(geno.file)
      } else {
        gds <- geno.file
      }
      SeqArray::seqSetFilter(gds, sample.id = sample.id, verbose = FALSE)
      n.variants <- rep(0,n.groups)
      miss.min <- rep(NA,n.groups)
      miss.mean <- rep(NA, n.groups)
      miss.max <- rep(NA, n.groups)
      freq.min <- rep(NA, n.groups)
      freq.mean <- rep(NA, n.groups)
      freq.max <- rep(NA, n.groups)
      freq.strata.min <- rep(NA, n.groups)
      freq.strata.max <- rep(NA, n.groups)
      if(MV | JV) MV.pval <- rep(NA, n.groups)
      if(IV | JV) IV.pval <- rep(NA, n.groups)
      if(JV) JV.pval <- rep(NA, n.groups)
      if(MF | JF | JD) MF.pval <- rep(NA, n.groups)
      if(IF | JF | JD) IF.pval <- rep(NA, n.groups)
      if(JF) JF.pval <- rep(NA, n.groups)
      if(JD) JD.pval <- rep(NA, n.groups)
      if(!is.null(meta.file.prefix)) {
        if(inherits(null.obj, "glmmkin.multi")) stop("Error: meta-analysis not supported yet for multiple phenotypes.")
        if(.Platform$endian!="little") stop("Error: platform must be little endian.")
        meta.file.score <- paste0(meta.file.prefix, ".score.", b)
        meta.file.cov <- paste0(meta.file.prefix, ".cov.", b)
        write.table(t(c("group", "chr", "pos", "ref", "alt", "N", "missrate", "altfreq", "G.SCORE", paste("K.SCORE.", 1:n.E, sep=""))), meta.file.score, quote = FALSE, row.names = FALSE, col.names = FALSE)
        meta.file.cov.handle <- file(meta.file.cov, "wb")
        writeBin(n.E, meta.file.cov.handle, size = 4)
        writeBin(interaction, meta.file.cov.handle, size = 4)
      }
      for(i in 1:n.groups) {
        tmp.idx <- group.idx.start[idx[i]]:group.idx.end[idx[i]]
        tmp.group.info <- group.info[tmp.idx, , drop = FALSE]
        SeqArray::seqSetFilter(gds, variant.id = tmp.group.info$variant.idx, verbose = FALSE)
        geno <- if(is.dosage) SeqVarTools::imputedDosage(gds, use.names = FALSE) else SeqVarTools::altDosage(gds, use.names = FALSE)
        miss <- colMeans(is.na(geno))
        freq <- colMeans(geno, na.rm = TRUE)/2
        include <- (miss <= miss.cutoff & ((freq >= MAF.range[1] & freq <= MAF.range[2]) | (freq >= 1-MAF.range[2] & freq <= 1-MAF.range[1])))
	if(any(duplicated(null.obj$id_include))) geno <- crossprod(J, geno)
	if(!is.null(strata)) { # E is not continuous
          freq.tmp <- sapply(strata.list, function(x) colMeans(geno[x, , drop = FALSE], na.rm = TRUE)/2) # freq.tmp is a matrix, each column is a strata, and each row is a varirant 
          if (length(dim(freq.tmp)) == 2) freq_strata <- apply(freq.tmp, 1, range) else freq_strata <- as.matrix(range(freq.tmp)) # freq_strata is the range of allele freq across strata.list
          include <- include & !is.na(freq_strata[1,]) & !is.na(freq_strata[2,]) & freq_strata[1,] >= AF.strata.range[1] & freq_strata[2,] <= AF.strata.range[2]
          rm(freq.tmp)
        }
        n.p <- sum(include)
        if(n.p == 0) next
        tmp.group.info <- tmp.group.info[include, , drop = FALSE]
        miss <- miss[include]
        freq <- freq[include]
        geno <- geno[, include, drop = FALSE]
        if(!is.null(strata)) freq_strata <- freq_strata[, include, drop = FALSE]
        N <- nrow(geno) - colSums(is.na(geno))
        if(sum(tmp.group.info$flip) > 0) {
          freq[tmp.group.info$flip==1] <- 1 - freq[tmp.group.info$flip==1]
          geno[, tmp.group.info$flip==1] <- 2 - geno[, tmp.group.info$flip==1]
          if(!is.null(strata)) freq_strata[, tmp.group.info$flip==1] <- 1 - freq_strata[, tmp.group.info$flip==1]
        }
        if(max(miss)>0) {
          miss.idx <- which(is.na(geno))
          geno[miss.idx] <- if(missing.method=="impute2mean") 2*freq[ceiling(miss.idx/nrow(geno))] else 0
        }
        if(IV | IF | JV | JF | JD) {
          K <- do.call(cbind, sapply((1+qi):ncol(E), function(xx) geno*E[,xx], simplify = FALSE), envir = environment())
          SK <- as.vector(crossprod(K,residuals))
        }
        if((qi != 0) && (IV | IF | JV | JF | JD)) {
          geno <- cbind(geno, do.call(cbind, sapply(1:qi, function(xx) geno*E[,xx], simplify = FALSE), envir = environment()))
        }
        U <- as.vector(crossprod(geno, residuals))
        if(inherits(null.obj, "glmmkin.multi")) {
          geno <- Diagonal(n = n.pheno) %x% geno
          if (IV | IF | JV | JF | JD) K <- Diagonal(n = n.pheno) %x% K
        }
        if(!is.null(null.obj$P)) {
          PG <- crossprod(null.obj$P, geno)
        } else {
          GSigma_iX <- crossprod(geno, null.obj$Sigma_iX)
          PG <- crossprod(null.obj$Sigma_i, geno) - tcrossprod(null.obj$Sigma_iX, tcrossprod(GSigma_iX, null.obj$cov))
        }
        V <- as.matrix(crossprod(geno, PG))
        if(MV | MF | JV | JF | JD) c1 <- rep(1:n.p,n.pheno)+rep((0:(n.pheno-1))*n.p*(1+qi), each=n.p) # index for GPG and row.index for GPC
        if((qi != 0) && (JV | JF | JD)) {
          c2 <- rep((n.p+1):(n.p*(1+qi)),n.pheno)+rep((0:(n.pheno-1))*n.p*(1+qi), each=n.p*qi) # index for CPC and col.index for GPC
          CPC_i <- try(solve(V[c2,c2]), silent = TRUE)
          if(inherits(CPC_i,"try-error") || any(diag(CPC_i)<0)) CPC_i <- try(MASS::ginv(V[c2,c2]), silent = TRUE)
          if(inherits(CPC_i,"try-error")) next
          U.adj <- U[c1] - tcrossprod(tcrossprod(V[c1,c2],CPC_i),t(U[c2]))
          V.adj <- V[c1,c1] - tcrossprod(tcrossprod(V[c1,c2],CPC_i),V[c1,c2])
        }
        if(IV | IF | JV | JF | JD) {
          if(!is.null(null.obj$P)) {
            KPK <- crossprod(K,crossprod(null.obj$P,K))
          } else {
            KSigma_iX <- crossprod(K, null.obj$Sigma_iX)
            KPK <- crossprod(K, crossprod(null.obj$Sigma_i, K)) - tcrossprod(KSigma_iX, tcrossprod(KSigma_iX, null.obj$cov))
          }
          V_i <- try(solve(V), silent = TRUE)
          if(inherits(V_i, "try-error") || any(diag(V_i)<0)) V_i <- try(MASS::ginv(V), silent = TRUE)
          if(inherits(V_i, "try-error")) next
          KPG <- crossprod(K,PG)
          IV.U <- SK - tcrossprod(tcrossprod(KPG,V_i),t(U))
          IV.V <- KPK - tcrossprod(tcrossprod(KPG,V_i),KPG)
        }
        if(!is.null(meta.file.prefix)) {
          COV <-rbind(cbind(V, t(KPG)), cbind(KPG, KPK))
          write.table(cbind(tmp.group.info[,c("group","chr","pos","ref","alt")], N, miss, freq, U, matrix(crossprod(K,residuals), nrow = n.p, ncol = n.E)), meta.file.score, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
          writeBin(COV[lower.tri(COV, diag = TRUE)], meta.file.cov.handle, size = 4)
        }
        if(use.minor.allele) {
          tmp.group.info$weight[freq > 0.5] <- -tmp.group.info$weight[freq > 0.5]
          if(!is.null(strata)) freq_strata[, freq > 0.5] <- 1 - freq_strata[, freq > 0.5]
          freq[freq > 0.5] <- 1 - freq[freq > 0.5]
        }
        weights <- rep(tmp.group.info$weight * MAF.weights.beta.fun(freq, MAF.weights.beta[1], MAF.weights.beta[2]), n.pheno)
        n.variants[i] <- n.p
        miss.min[i] <- min(miss)
        miss.mean[i] <- mean(miss)
        miss.max[i] <- max(miss)
        freq.min[i] <- min(freq)
        freq.mean[i] <- mean(freq)
        freq.max[i] <- max(freq)
        if(!is.null(strata)) {
          freq.strata.min[i] <- min(freq_strata)
          freq.strata.max[i] <- max(freq_strata)
        }
        if(MV | MF | JV | JF | JD) {
          U <- U[c1]*weights
          V <- t(V[c1,c1]*weights)*weights
        }
        if((qi != 0) && (JV | JF | JD)) {
          U.adj <- U.adj*weights
          V.adj <- t(V.adj*weights)*weights
        }
        if(IV | IF | JV | JF | JD) {
          IV.U <- IV.U*rep(weights, ei)
          IV.V <- t(IV.V*rep(weights, ei))*rep(weights, ei)
        }
        if(MV | JV) MV.pval[i] <- tryCatch(.quad_pval(U = U, V = V, method = method), error = function(e) { NA })
        if(IV | JV) IV.pval[i] <- tryCatch(.quad_pval(U = IV.U, V = IV.V, method = method), error = function(e) { NA })
        if(JV && (qi == 0)) JV.pval[i] <- tryCatch(fisher_pval(c(MV.pval[i], IV.pval[i])), error = function(e) { MV.pval[i] })
        if(JV && (qi != 0)) {
          MV.adj.pval <- tryCatch(.quad_pval(U = U.adj, V = V.adj, method = method), error = function(e) { NA })
          JV.pval[i] <- tryCatch(fisher_pval(c(MV.adj.pval, IV.pval[i])), error = function(e) { MV.adj.pval })
        }
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
        if((JF | JD) && (qi != 0)) {
          MF.BU.adj <- sum(U.adj)
          MF.BV.adj <- sum(V.adj)
          MF.Bp.adj <- pchisq(MF.BU.adj^2/MF.BV.adj,df=1,lower.tail=FALSE)
          V.adj.rowSums <- rowSums(V.adj)
          MF.U.adj <- U.adj  - V.adj.rowSums * MF.BU.adj / MF.BV.adj 
          MF.V.adj  <- V.adj  - tcrossprod(V.adj.rowSums) / MF.BV.adj 
          if(MF.BV.adj == 0 | mean(abs(MF.V.adj)) < sqrt(.Machine$double.eps)) MF.adj.p <- NA
          else MF.adj.p <- tryCatch(.quad_pval(U = MF.U.adj, V = MF.V.adj, method = method), error = function(e) { NA })
          MF.adj.pval <- tryCatch(fisher_pval(c(MF.Bp.adj, MF.adj.p)), error = function(e) { MF.Bp.adj })
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
        if(JF && (qi == 0)) JF.pval[i] <- tryCatch(fisher_pval(c(MF.Bp, MF.p, IF.Bp, IF.p)), error = function(e) { MF.Bp })
        if(JF && (qi != 0)) JF.pval[i] <- tryCatch(fisher_pval(c(MF.Bp.adj, MF.adj.p, IF.Bp, IF.p)), error = function(e) { MF.Bp.adj })
        if(JD && (qi == 0)) JD.pval[i] <- tryCatch(fisher_pval(c(MF.pval[i], IF.pval[i])), error = function(e) { MF.pval[i] })
        if(JD && (qi != 0)) JD.pval[i] <- tryCatch(fisher_pval(c(MF.adj.pval, IF.pval[i])), error = function(e) { MF.adj.pval })
        rm(geno)
        if(Garbage.Collection) gc()
      }
      SeqArray::seqClose(gds)
      if(!is.null(meta.file.prefix)) close(meta.file.cov.handle)
      tmp.out <- data.frame(group=unique(group.info$group)[idx], n.variants=n.variants, miss.min=miss.min, miss.mean=miss.mean, miss.max=miss.max, freq.min=freq.min, freq.mean=freq.mean, freq.max=freq.max,freq.strata.min=freq.strata.min, freq.strata.max=freq.strata.max)
      if(MV | JV) tmp.out$MV.pval <- MV.pval
      if(MF | JF | JD) tmp.out$MF.pval <- MF.pval
      if(IV | JV) tmp.out$IV.pval <- IV.pval
      if(IF | JF | JD) tmp.out$IF.pval <- IF.pval
      if(JV) tmp.out$JV.pval <- JV.pval
      if(JF) tmp.out$JF.pval <- JF.pval
      if(JD) tmp.out$JD.pval <- JD.pval
      tmp.out
    }
    if (inherits(geno.file, "SeqVarGDSClass")) {
      SeqArray::seqClose(geno.file)
    }
    return(out)
  } else { # use a single core
    n.groups <- n.groups.all
    if (!inherits(geno.file, "SeqVarGDSClass")) {
      gds <- SeqArray::seqOpen(geno.file)
    }
    SeqArray::seqSetFilter(gds, sample.id = sample.id, verbose = FALSE)
    n.variants <- rep(0,n.groups)
    miss.min <- rep(NA,n.groups)
    miss.mean <- rep(NA, n.groups)
    miss.max <- rep(NA, n.groups)
    freq.min <- rep(NA, n.groups)
    freq.mean <- rep(NA, n.groups)
    freq.max <- rep(NA, n.groups)
    freq.strata.min <- rep(NA, n.groups)
    freq.strata.max <- rep(NA, n.groups)
    if(MV | JV) MV.pval <- rep(NA, n.groups)
    if(IV | JV) IV.pval <- rep(NA, n.groups)
    if(JV) JV.pval <- rep(NA, n.groups)
    if(MF | JF | JD) MF.pval <- rep(NA, n.groups)
    if(IF | JF | JD) IF.pval <- rep(NA, n.groups)
    if(JF) JF.pval <- rep(NA, n.groups)
    if(JD) JD.pval <- rep(NA, n.groups)
    if(!is.null(meta.file.prefix)) {
      if(inherits(null.obj, "glmmkin.multi")) stop("Error: meta-analysis not supported yet for multiple phenotypes.")
      if(.Platform$endian!="little") stop("Error: platform must be little endian.")
      meta.file.score <- paste0(meta.file.prefix, ".score.1")
      meta.file.cov <- paste0(meta.file.prefix, ".cov.1")
      write.table(t(c("group", "chr", "pos", "ref", "alt", "N", "missrate", "altfreq", "G.SCORE", paste("K.SCORE.", 1:n.E, sep=""))), meta.file.score, quote = FALSE, row.names = FALSE, col.names = FALSE)
      meta.file.cov.handle <- file(meta.file.cov, "wb")
      writeBin(n.E, meta.file.cov.handle, size = 4)
      writeBin(interaction, meta.file.cov.handle, size = 4)
    }
    for(i in 1:n.groups) {
      tmp.idx <- group.idx.start[i]:group.idx.end[i]
      tmp.group.info <- group.info[tmp.idx, , drop = FALSE]
      SeqArray::seqSetFilter(gds, variant.id = tmp.group.info$variant.idx, verbose = FALSE)
      geno <- if(is.dosage) SeqVarTools::imputedDosage(gds, use.names = FALSE) else SeqVarTools::altDosage(gds, use.names = FALSE)
      miss <- colMeans(is.na(geno))
      freq <- colMeans(geno, na.rm = TRUE)/2
      include <- (miss <= miss.cutoff & ((freq >= MAF.range[1] & freq <= MAF.range[2]) | (freq >= 1-MAF.range[2] & freq <= 1-MAF.range[1])))
      if(any(duplicated(null.obj$id_include))) geno <- crossprod(J, geno)
      if(!is.null(strata)) { # E is not continuous
        freq.tmp <- sapply(strata.list, function(x) colMeans(geno[x, , drop = FALSE], na.rm = TRUE)/2) # freq.tmp is a matrix, each column is a strata, and each row is a varirant 
        if (length(dim(freq.tmp)) == 2) freq_strata <- apply(freq.tmp, 1, range) else freq_strata <- as.matrix(range(freq.tmp)) # freq_strata is the range of allele freq across strata.list
        include <- include & !is.na(freq_strata[1,]) & !is.na(freq_strata[2,]) & freq_strata[1,] >= AF.strata.range[1] & freq_strata[2,] <= AF.strata.range[2]
        rm(freq.tmp)
      }
      n.p <- sum(include)
      if(n.p == 0) next
      tmp.group.info <- tmp.group.info[include, , drop = FALSE]
      miss <- miss[include]
      freq <- freq[include]
      geno <- geno[, include, drop = FALSE]
      if(!is.null(strata)) freq_strata <- freq_strata[, include, drop = FALSE]
      N <- nrow(geno) - colSums(is.na(geno))
      if(sum(tmp.group.info$flip) > 0) {
        freq[tmp.group.info$flip==1] <- 1 - freq[tmp.group.info$flip==1]
        geno[, tmp.group.info$flip==1] <- 2 - geno[, tmp.group.info$flip==1]
        if(!is.null(strata)) freq_strata[, tmp.group.info$flip==1] <- 1 - freq_strata[, tmp.group.info$flip==1]
      }
      if(max(miss)>0) {
        miss.idx <- which(is.na(geno))
        geno[miss.idx] <- if(missing.method=="impute2mean") 2*freq[ceiling(miss.idx/nrow(geno))] else 0
      }
      if(IV | IF | JV | JF | JD) {
        K <- do.call(cbind, sapply((1+qi):ncol(E), function(xx) geno*E[,xx], simplify = FALSE), envir = environment())
        SK <- as.vector(crossprod(K,residuals))
      }
      if((qi != 0) && (IV | IF | JV | JF | JD)) {
        geno <- cbind(geno, do.call(cbind, sapply(1:qi, function(xx) geno*E[,xx], simplify = FALSE), envir = environment()))
      }
      U <- as.vector(crossprod(geno, residuals))
      if(inherits(null.obj, "glmmkin.multi")) {
        geno <- Diagonal(n = n.pheno) %x% geno
        if (IV | IF | JV | JF | JD) K <- Diagonal(n = n.pheno) %x% K
      }
      if(!is.null(null.obj$P)) {
        PG <- crossprod(null.obj$P, geno)
      } else {
        GSigma_iX <- crossprod(geno, null.obj$Sigma_iX)
        PG <- crossprod(null.obj$Sigma_i, geno) - tcrossprod(null.obj$Sigma_iX, tcrossprod(GSigma_iX, null.obj$cov))
      }
      V <- as.matrix(crossprod(geno, PG))
      if(MV | MF | JV | JF | JD) c1 <- rep(1:n.p,n.pheno)+rep((0:(n.pheno-1))*n.p*(1+qi), each=n.p) # index for GPG and row.index for GPC
      if((qi != 0) && (JV | JF | JD)) {
        c2 <- rep((n.p+1):(n.p*(1+qi)),n.pheno)+rep((0:(n.pheno-1))*n.p*(1+qi), each=n.p*qi) # index for CPC and col.index for GPC
        CPC_i <- try(solve(V[c2,c2]), silent = TRUE)
        if(inherits(CPC_i, "try-error") || any(diag(CPC_i)<0)) CPC_i <- try(MASS::ginv(V[c2,c2]), silent = TRUE)
        if(inherits(CPC_i, "try-error")) next
        U.adj <- U[c1] - tcrossprod(tcrossprod(V[c1,c2],CPC_i),t(U[c2]))
        V.adj <- V[c1,c1] - tcrossprod(tcrossprod(V[c1,c2],CPC_i),V[c1,c2])
      }
      if(IV | IF | JV | JF | JD) {
        if(!is.null(null.obj$P)) {
          KPK <- crossprod(K,crossprod(null.obj$P,K))
        } else {
          KSigma_iX <- crossprod(K, null.obj$Sigma_iX)
          KPK <- crossprod(K, crossprod(null.obj$Sigma_i, K)) - tcrossprod(KSigma_iX, tcrossprod(KSigma_iX, null.obj$cov))
        }
        V_i <- try(solve(V), silent = TRUE)
        if(inherits(V_i, "try-error") || any(diag(V_i)<0)) V_i <- try(MASS::ginv(V), silent = TRUE)
        if(inherits(V_i,"try-error")) next
        KPG <- crossprod(K,PG)
        IV.U <- SK - tcrossprod(tcrossprod(KPG,V_i),t(U))
        IV.V <- KPK - tcrossprod(tcrossprod(KPG,V_i),KPG)
      }
      if(!is.null(meta.file.prefix)) {
        COV <-rbind(cbind(V, t(KPG)), cbind(KPG, KPK))
        write.table(cbind(tmp.group.info[,c("group","chr","pos","ref","alt")], N, miss, freq, U, matrix(crossprod(K,residuals), nrow = n.p, ncol = n.E)), meta.file.score, quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
        writeBin(COV[lower.tri(COV, diag = TRUE)], meta.file.cov.handle, size = 4)
      }
      if(use.minor.allele) {
        tmp.group.info$weight[freq > 0.5] <- -tmp.group.info$weight[freq > 0.5]
        if(!is.null(strata)) freq_strata[, freq > 0.5] <- 1 - freq_strata[, freq > 0.5]
        freq[freq > 0.5] <- 1 - freq[freq > 0.5]
      }
      weights <- rep(tmp.group.info$weight * MAF.weights.beta.fun(freq, MAF.weights.beta[1], MAF.weights.beta[2]), n.pheno)
      n.variants[i] <- n.p
      miss.min[i] <- min(miss)
      miss.mean[i] <- mean(miss)
      miss.max[i] <- max(miss)
      freq.min[i] <- min(freq)
      freq.mean[i] <- mean(freq)
      freq.max[i] <- max(freq)
      if(!is.null(strata)) {
        freq.strata.min[i] <- min(freq_strata)
        freq.strata.max[i] <- max(freq_strata)
      }
      if(MV | MF | JV | JF | JD) {
        U <- U[c1]*weights
        V <- t(V[c1,c1]*weights)*weights
      }
      if((qi != 0) && (JV | JF | JD)) {
        U.adj <- U.adj*weights
        V.adj <- t(V.adj*weights)*weights
      }
      if(IV | IF | JV | JF | JD) {
        IV.U <- IV.U*rep(weights, ei)
        IV.V <- t(IV.V*rep(weights, ei))*rep(weights, ei)
      }
      if(MV | JV) MV.pval[i] <- tryCatch(.quad_pval(U = U, V = V, method = method), error = function(e) { NA })
      if(IV | JV) IV.pval[i] <- tryCatch(.quad_pval(U = IV.U, V = IV.V, method = method), error = function(e) { NA })
      if(JV && (qi == 0)) JV.pval[i] <- tryCatch(fisher_pval(c(MV.pval[i], IV.pval[i])), error = function(e) { MV.pval[i] })
      if(JV && (qi != 0)) {
        MV.adj.pval <- tryCatch(.quad_pval(U = U.adj, V = V.adj, method = method), error = function(e) { NA })
        JV.pval[i] <- tryCatch(fisher_pval(c(MV.adj.pval, IV.pval[i])), error = function(e) { MV.adj.pval })
      }
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
      if((JF | JD) && (qi != 0)) {
        MF.BU.adj <- sum(U.adj)
        MF.BV.adj <- sum(V.adj)
        MF.Bp.adj <- pchisq(MF.BU.adj^2/MF.BV.adj,df=1,lower.tail=FALSE)
        V.adj.rowSums <- rowSums(V.adj)
        MF.U.adj <- U.adj  - V.adj.rowSums * MF.BU.adj / MF.BV.adj 
        MF.V.adj  <- V.adj  - tcrossprod(V.adj.rowSums) / MF.BV.adj 
        if(MF.BV.adj == 0 | mean(abs(MF.V.adj)) < sqrt(.Machine$double.eps)) MF.adj.p <- NA
        else MF.adj.p <- tryCatch(.quad_pval(U = MF.U.adj, V = MF.V.adj, method = method), error = function(e) { NA })
        MF.adj.pval <- tryCatch(fisher_pval(c(MF.Bp.adj, MF.adj.p)), error = function(e) { MF.Bp.adj })
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
      if(JF && (qi == 0)) JF.pval[i] <- tryCatch(fisher_pval(c(MF.Bp, MF.p, IF.Bp, IF.p)), error = function(e) { MF.Bp })
      if(JF && (qi != 0)) JF.pval[i] <- tryCatch(fisher_pval(c(MF.Bp.adj, MF.adj.p, IF.Bp, IF.p)), error = function(e) { MF.Bp.adj })
      if(JD && (qi == 0)) JD.pval[i] <- tryCatch(fisher_pval(c(MF.pval[i], IF.pval[i])), error = function(e) { MF.pval[i] })
      if(JD && (qi != 0)) JD.pval[i] <- tryCatch(fisher_pval(c(MF.adj.pval, IF.pval[i])), error = function(e) { MF.adj.pval })
      rm(geno, K)
      if(Garbage.Collection) gc()
    }
    SeqArray::seqClose(gds)
    if(!is.null(meta.file.prefix)) close(meta.file.cov.handle)
    out <- data.frame(group=unique(group.info$group), n.variants=n.variants, miss.min=miss.min, miss.mean=miss.mean, miss.max=miss.max, freq.min=freq.min, freq.mean=freq.mean, freq.max=freq.max,freq.strata.min=freq.strata.min, freq.strata.max=freq.strata.max)
    if(MV | JV) out$MV.pval <- MV.pval
    if(MF | JF | JD) out$MF.pval <- MF.pval
    if(IV | JV) out$IV.pval <- IV.pval
    if(IF | JF | JD) out$IF.pval <- IF.pval
    if(JV) out$JV.pval <- JV.pval
    if(JF) out$JF.pval <- JF.pval
    if(JD) out$JD.pval <- JD.pval
  }
  return(out[match(groups, out$group),])
}


