glmm.gei <- function(null.obj, interaction, geno.file, outfile, bgen.samplefile=NULL, interaction.covariates=NULL, meta.output=FALSE, covar.center="interaction.covariates.only", geno.center=TRUE, cat.threshold = 20, MAF.range = c(1e-7, 0.5), MAC.cutoff = 1, miss.cutoff = 1, RSQ.cutoff = 0, missing.method = "impute2mean", nperbatch=100, is.dosage = FALSE, ncores = 1, verbose = FALSE){
  if(Sys.info()["sysname"] == "Windows" && ncores > 1) {
    warning("The package doMC is not available on Windows... Switching to single thread...", call. = FALSE)
    ncores <- 1
  }

  Sys.setenv(MKL_NUM_THREADS = 1)
  if(!grepl("\\.gds$|\\.bgen$", geno.file[1])) stop("Error: only .gds and .bgen format is supported in geno.file!")
  if(!inherits(null.obj, c("glmmkin", "glmmkin.multi"))) stop("Error: null.obj must be a class glmmkin or glmmkin.multi object!")
  if(inherits(null.obj,"glmmkin.multi")) stop("Error: currently null.obj must be a class glmmkin object, glmmkin.multi not yet supported...")
  n.pheno <- null.obj$n.pheno
  covar.center <- try(match.arg(covar.center, c("none", "all", "interaction.covariates.only")))
  if(inherits(covar.center,"try-error")) stop("Error: \"covar.center\" must be one of the following: \"none\", \"all\", \"interaction.covariates.only\".")
  missing.method <- try(match.arg(missing.method, c("impute2mean", "omit")))
  if(inherits(missing.method,"try-error")) stop("Error: \"missing.method\" must be \"impute2mean\" or \"omit\".")
  if(!inherits(interaction, c("integer", "numeric", "character"))) stop("Error: \"interaction\" should be an integer, numeric, or character vector.")
  residuals <- null.obj$scaled.residuals
  qi <- length(interaction.covariates)
  ei <- length(interaction)
  ei1 <- ei+1
  if(inherits(interaction,"character")) {
    if(!is.null(interaction.covariates)) {
      if(any(interaction.covariates %in% interaction)) {stop("there are interaction.covariates also specified as interaction.")}
      interaction <- c(interaction, interaction.covariates)
    }
    if (!all(interaction %in% colnames(null.obj$X))) {stop("there are interactions not in column name of covariate matrix.")}
    E <- as.matrix(null.obj$X[,interaction])
  } else { # indices
    if(!is.null(interaction.covariates)) {
      if(any(interaction.covariates %in% interaction)) {stop("there are interaction.covariates also specified as interaction.")}
      interaction <- c(interaction, interaction.covariates)
    }
    E <- as.matrix(null.obj$X[,interaction+1])
  }

  ncores <- min(c(ncores, parallel::detectCores(logical = TRUE)))
  J <- NULL
  dupeflag<-NULL
  
  
  if(grepl("\\.gds$", geno.file[1])) {
    if (!inherits(geno.file, "SeqVarGDSClass")) {
      gds <- SeqArray::seqOpen(geno.file) 
    } else {
      gds <- geno.file
    }
    sample.id <- SeqArray::seqGetData(gds, "sample.id")
    variant.idx.all <- SeqArray::seqGetData(gds, "variant.id")
    if(any(is.na(match(null.obj$id_include, sample.id)))) warning("Check your data... Some individuals in null.obj$id_include are missing in sample.id of geno.file!", call. = FALSE)
    sample.id <- sample.id[sample.id %in% null.obj$id_include]
    if(length(sample.id) == 0) stop("Error: null.obj$id_include does not match sample.id in geno.file!")
    #J <- NULL
    if(any(duplicated(null.obj$id_include))) {
      match.id <- null.obj$id_include %in% sample.id
      null.obj$id_include <- null.obj$id_include[match.id]
      J <- t(sparseMatrix(i=1:length(null.obj$id_include), j=match(null.obj$id_include, unique(null.obj$id_include)[match(sample.id, unique(null.obj$id_include))]), x=1))
    } else match.id <- match(sample.id, null.obj$id_include)

    E <- as.matrix(E[match.id, , drop = FALSE])
    
    residuals <- residuals[match.id]
    if(!is.null(null.obj$P)) {
      null.obj$P <- null.obj$P[match.id, match.id]
    } else {
      null.obj$Sigma_iX <- null.obj$Sigma_iX[match.id, , drop = FALSE]
      null.obj$Sigma_i <- null.obj$Sigma_i[match.id, match.id]
    }
    
    Ebin<-apply(as.matrix(E[!duplicated(null.obj$id_include),,drop=FALSE]),2,function(x) length(unique(x))<=cat.threshold)
    if(any(Ebin)){
      Ecat<-as.matrix(E[!duplicated(null.obj$id_include),Ebin,drop=FALSE])
      strata <- apply(Ecat, 1, paste, collapse = "_")
      
      uni.strata<-unique(rev(strata))
      uni.strata<-sort(uni.strata)
      cat_inter<-paste(interaction[Ebin], collapse = '_')
      tmp<-apply(as.matrix(uni.strata),1,function(x) paste(x, collapse = '_'))
      tmp1<-paste0(cat_inter,"_",tmp)
      tmp2<-c("N","AF")
      bin_header<-c(apply(as.matrix(tmp1),1, function(x) paste0(tmp2,"_",x)))
    }else {
      bin_header=NULL
    }
    strata <- if(any(Ebin))  as.numeric(as.factor(apply(as.matrix(E[!duplicated(null.obj$id_include),Ebin,drop=FALSE]), 1, paste, collapse = "_"))) else NULL 

    if(covar.center == "all") {
      E <- scale(E, scale = FALSE)
    } else if(covar.center != "none") {
      if(!is.null(interaction.covariates)) {
        E <- cbind(E[,1:ei,drop=F], scale(E[,(1+ei):(qi+ei),drop=F], scale = FALSE))
      }
    }
    E <- cbind(1, E)
    ncolE <- ncol(E)
    if(!is.null(strata)) {
      strata.list <- lapply(sort(unique(strata)), function(x) which(strata==x))
    } else {
      strata.list <- NULL
    }

    
    
    if (!inherits(geno.file, "SeqVarGDSClass")) {
      SeqArray::seqClose(gds)
    }
    p.all <- length(variant.idx.all)
    
    if(ncores > 1) {
      doMC::registerDoMC(cores = ncores)
      p.percore <- (p.all-1) %/% ncores + 1
      n.p.percore_1 <- p.percore * ncores - p.all
      
 
      if (meta.output) {
        interaction2 <- c("G", paste0("G-", interaction))
        cov.header = matrix(paste(rep(paste0("Cov_Beta_", interaction2), each = ncolE), interaction2, sep = "_"), ncolE, ncolE)
        meta.header = c(paste0("Beta_", interaction2), paste0("SE_Beta_", interaction2), cov.header[lower.tri(cov.header)])

        if (is.null(strata.list)) 
        {
          totalCol =  11 + 3*(ei+qi) + ((ei+qi) * ((ei+qi) - 1) / 2)
          }
        else {      
          totalCol =  11 +2*length(unique(strata))+ 3*(ei+qi) + ((ei+qi) * ((ei+qi) - 1) / 2)
          }
      } else {
        interaction2 <- paste0("G-", interaction[1:ei])
        if (ei != 1) {
          cov.header = matrix(paste(rep(paste0("Cov_Beta_", interaction2), each = ei), interaction, sep = "_G-"), ei, ei)
          meta.header = c(paste0("Beta_", interaction2), paste0("SE_Beta_", interaction2), cov.header[lower.tri(cov.header)])
        } else {
          meta.header = c(paste0("Beta_", interaction2), paste0("SE_Beta_", interaction2))
        }
        
        if (is.null(strata.list)) {totalCol = 9 + ei + ei+ ei * (ei - 1) / 2 }
        else {totalCol = 9 +2*length(unique(strata))+ 2*ei+ ei * (ei - 1) / 2 }
      }
      
      if (is.null(strata.list)){
        write.table(t(data.frame(n = c("SNPID","CHR","POS","Non_Effect_Allele","Effect_Allele", "N_Samples", "AF", "MAC", "RSQ", "Beta_Marginal", "SE_Beta_Marginal", meta.header, "P_Value_Marginal",  "P_Value_Interaction", "P_Value_Joint"))), outfile, quote = F, col.names = F, row.names = F, sep="\t")
        
      }else {
        write.table(t(data.frame(n = c("SNPID","CHR","POS","Non_Effect_Allele","Effect_Allele", "N_Samples", "AF", "MAC", "RSQ", bin_header, "Beta_Marginal", "SE_Beta_Marginal", meta.header, "P_Value_Marginal",  "P_Value_Interaction", "P_Value_Joint"))), outfile, quote = F, col.names = F, row.names = F, sep="\t")
      }
      
      foreach(b=1:ncores, .inorder=FALSE, .options.multicore = list(preschedule = FALSE, set.seed = FALSE)) %dopar% {
        file.create(paste0(outfile, "_tmp.", b))
        debug_file <- paste0(outfile, "_tmp.", b, ".err")
        file.create(debug_file)
        variant.idx <- if(b <= n.p.percore_1) variant.idx.all[((b-1)*(p.percore-1)+1):(b*(p.percore-1))] else variant.idx.all[(n.p.percore_1*(p.percore-1)+(b-n.p.percore_1-1)*p.percore+1):(n.p.percore_1*(p.percore-1)+(b-n.p.percore_1)*p.percore)]
        p <- length(variant.idx)
        if (!inherits(geno.file, "SeqVarGDSClass")) {
          gds <- SeqArray::seqOpen(geno.file)
        } else {
          gds <- geno.file
        }
        SeqArray::seqSetFilter(gds, sample.id = sample.id, verbose = FALSE)
        rm(sample.id)
        nbatch.flush <- (p-1) %/% 100000 + 1
        ii <- 0
        for(i in 1:nbatch.flush) {
          gc()
          tmp.variant.idx <- if(i == nbatch.flush) variant.idx[((i-1)*100000+1):p] else variant.idx[((i-1)*100000+1):(i*100000)]
          SeqArray::seqSetFilter(gds, variant.id = tmp.variant.idx, verbose = FALSE)
          MISSRATE <- if(is.dosage) SeqArray::seqApply(gds, "annotation/format/DS", function(xx) mean(is.na(xx)), margin = "by.variant", as.is = "double") else SeqVarTools::missingGenotypeRate(gds, margin = "by.variant")
          AF <- if(is.dosage) SeqArray::seqApply(gds, "annotation/format/DS", mean, margin = "by.variant", as.is = "double", na.rm = TRUE)/2 else 1 - SeqVarTools::alleleFrequency(gds)
	  #MAC <- SeqVarTools::minorAlleleCount(gds)
          #include <- (MISSRATE <= miss.cutoff & MAC >= MAC.cutoff & ((AF >= MAF.range[1] & AF <= MAF.range[2]) | (AF >= 1-MAF.range[2] & AF <= 1-MAF.range[1])))
	  include <- (MISSRATE <= miss.cutoff & ((AF >= MAF.range[1] & AF <= MAF.range[2]) | (AF >= 1-MAF.range[2] & AF <= 1-MAF.range[1])))
          if(sum(include) == 0) {
            next
          }
          ii <- ii + 1
          tmp.variant.idx <- tmp.variant.idx[include]
          tmp.p <- length(tmp.variant.idx)
          SeqArray::seqSetFilter(gds, variant.id = tmp.variant.idx, verbose = FALSE)
          SNP <- SeqArray::seqGetData(gds, "annotation/id")
          SNP[SNP == ""] <- NA
          out <- data.frame(SNP = SNP, CHR = SeqArray::seqGetData(gds, "chromosome"), POS = SeqArray::seqGetData(gds, "position"))
          rm(SNP)
          alleles.list <- strsplit(SeqArray::seqGetData(gds, "allele"), ",")
          out$REF <- unlist(lapply(alleles.list, function(x) x[1]))
          out$ALT <- unlist(lapply(alleles.list, function(x) paste(x[-1], collapse=",")))
          out$MISSRATE <- MISSRATE[include]
          out$AF <- AF[include]
	  #out$MAC <- MAC[include]
          include <- include[include]
          rm(alleles.list)
          tmp_idx <- 1
          tmp.out <- lapply(1:((tmp.p-1) %/% nperbatch + 1), function(j) {
            tmp2.variant.idx <- if(j == (tmp.p-1) %/% nperbatch + 1) tmp.variant.idx[((j-1)*nperbatch+1):tmp.p] else tmp.variant.idx[((j-1)*nperbatch+1):(j*nperbatch)]
            SeqArray::seqSetFilter(gds, variant.id = tmp2.variant.idx, verbose = FALSE)
            geno <- if(is.dosage) SeqVarTools::imputedDosage(gds, use.names = FALSE) else SeqVarTools::altDosage(gds, use.names = FALSE)
            ng <- ncol(geno)
            freq <- colMeans(geno, na.rm = TRUE)/2
            N <- nrow(geno) - colSums(is.na(geno))
	    MAC <- colSums(geno, na.rm = TRUE)
	    MAC[MAC>N] <- (2*N - MAC)[MAC>N]
	    RSQ <- apply(geno, 2, .calc_rsq)
            if(!is.null(strata.list)) { # E is not continuous
              freq.tmp <- sapply(strata.list, function(x) colMeans(geno[x, , drop = FALSE], na.rm = TRUE)/2)
	      if(is.null(ncol(freq.tmp))) freq.tmp <- matrix(freq.tmp, nrow = 1, dimnames = list(NULL, names(freq.tmp)))
              n.tmp <- sapply(strata.list, function(x) colSums(!is.na(geno[x, , drop = FALSE])))
	      if(is.null(ncol(n.tmp))) n.tmp <- matrix(n.tmp, nrow = 1, dimnames = list(NULL, names(n.tmp)))
              freq.tmp.rev<-freq.tmp[,order(ncol(freq.tmp):1),drop=FALSE]
              n.tmp.rev<-n.tmp[,order(ncol(n.tmp):1),drop=FALSE]
              rows.freq_N<-nrow(freq.tmp)
              cols.freq_N<-ncol(freq.tmp)+ncol(n.tmp)
              freq_N<-matrix(NA,nrow =rows.freq_N,ncol=cols.freq_N)
              freq_N[,seq(1,cols.freq_N,2)]<-n.tmp
              freq_N[,seq(2,cols.freq_N,2)]<-freq.tmp
            } else {freq_N<-NA}
 
            miss.idx <- which(is.na(geno))
            if(length(miss.idx)>0) {
              geno[miss.idx] <- if(missing.method == "impute2mean") 2*freq[ceiling(miss.idx/nrow(geno))] else NA
            }
            if(geno.center) geno <- scale(geno, scale = FALSE)
            miss.idx <- which(is.na(geno))
            if(length(miss.idx)>0) { # omit
              geno[miss.idx] <- 0
            }
            if(any(duplicated(null.obj$id_include))) geno <- crossprod(J, geno)
            
            K <- do.call(cbind, sapply(1:ncolE, function(xx) geno*E[,xx], simplify = FALSE), envir = environment())
            
            U <- as.vector(crossprod(geno, residuals))
            if(!is.null(null.obj$P)) {
              PG <- crossprod(null.obj$P, geno)
            } else {
              GSigma_iX <- crossprod(geno, null.obj$Sigma_iX)
              PG <- crossprod(null.obj$Sigma_i, geno) - tcrossprod(null.obj$Sigma_iX, tcrossprod(GSigma_iX, null.obj$cov))
            }
            
            GPG <- as.matrix(crossprod(geno, PG)) * (matrix(1, 1, 1) %x% diag(ng))
            GPG_i <- try(solve(GPG), silent = TRUE)
            if(inherits(GPG_i, "try-error")) GPG_i <- MASS::ginv(GPG)
            V_i <- pmax(0,diag(GPG_i))
            
            BETA.MAIN <- V_i * U
            SE.MAIN   <- sqrt(V_i)
            STAT.MAIN <- BETA.MAIN * U
            PVAL.MAIN <- ifelse(V_i>0, pchisq(STAT.MAIN, df=1, lower.tail=FALSE), NA)
            
            if(!is.null(null.obj$P)) {
              KPK <- crossprod(K,crossprod(null.obj$P,K))
            } else {
              KSigma_iX <- crossprod(K, null.obj$Sigma_iX)
              KPK <- crossprod(K, crossprod(null.obj$Sigma_i, K)) - tcrossprod(KSigma_iX, tcrossprod(KSigma_iX, null.obj$cov))
            }
            KPK <- as.matrix(KPK) * (matrix(1, ncolE, ncolE) %x% diag(ng))
            
            IV.V_i <- try(solve(KPK), silent = TRUE)
            if(inherits(IV.V_i, "try-error")) IV.V_i <- try(MASS::ginv(KPK), silent = TRUE)
            if (inherits(IV.V_i, "try-error")) {
              fix_out <- fix.dgesdd(gds, out, debug_file, null.obj, J, residuals, tmp2.variant.idx, meta.output, geno.center, missing.method, strata.list, ncolE, E, ei, bin_header, meta.header, totalCol, tmp_idx, include, is.dosage)
              tmp_idx <<- fix_out[[1]]
              include <<- fix_out[[2]]
              return(fix_out[[3]])
            }
            diag(IV.V_i) <- pmax(0,diag(IV.V_i))
            IV.U <- (rep(1, ncolE) %x% diag(ng)) * as.vector(crossprod(K,residuals))
            BETA.INT <- crossprod(IV.V_i, IV.U)
            
            ng1   <- ng+1
            ngei1 <- ng*ei1
            
            IV.E_i <- try(solve(IV.V_i[ng1:ngei1, ng1:ngei1]), silent = TRUE)
            if(inherits(IV.E_i,"try-error")) IV.E_i <- try(MASS::ginv(IV.V_i[ng1:ngei1, ng1:ngei1]), silent = TRUE)
            if(inherits(IV.E_i, "try-error")) {
              fix_out <- fix.dgesdd(gds, out, debug_file, null.obj, J, residuals, tmp2.variant.idx, meta.output, geno.center, missing.method, strata.list, ncolE, E, ei, bin_header, meta.header, totalCol, tmp_idx, include, is.dosage)
              tmp_idx <<- fix_out[[1]]
              include <<- fix_out[[2]]
              return(fix_out[[3]])
            }
            STAT.INT   <- diag(crossprod(BETA.INT[ng1:ngei1,], crossprod(IV.E_i, BETA.INT[ng1:ngei1,])))
            
            IV.GE_i <- try(solve(IV.V_i[1:ngei1, 1:ngei1]), silent = TRUE)
            if(inherits(IV.GE_i, "try-error")) IV.GE_i <- try(MASS::ginv(IV.V_i[1:ngei1, 1:ngei1]), silent = TRUE)
            if (inherits(IV.GE_i, "try-error")) {
              fix_out <- fix.dgesdd(gds, out, debug_file, null.obj, J, residuals, tmp2.variant.idx, meta.output, geno.center, missing.method, strata.list, ncolE, E, ei, bin_header, meta.header, totalCol, tmp_idx, include, is.dosage)
              tmp_idx <<- fix_out[[1]]
              include <<- fix_out[[2]]
              return(fix_out[[3]])
            }
            STAT.JOINT <- diag(crossprod(BETA.INT[1:ngei1,], crossprod(IV.GE_i, BETA.INT[1:ngei1,])))
            
            PVAL.INT   <- pchisq(STAT.INT, df=ei, lower.tail=FALSE)
            PVAL.JOINT <- ifelse(is.na(PVAL.MAIN), NA, pchisq(STAT.JOINT, df=1+ei, lower.tail=FALSE))
            
            
            split_mat <- matrix(1:(ncolE*ncolE), ncolE, ncolE)
            if (ng > 1) {
              IV.V_i <- split(IV.V_i, split_mat %x% diag(ng))[-1]
            }else {
              IV.V_i <- split(IV.V_i, split_mat %x% diag(ng))
            }

            tmp_idx <<- tmp_idx + ng
            
            if (!is.null(strata.list)){
              if (meta.output) {
                return(rbind(N, MAC, RSQ, t(freq_N), BETA.MAIN, SE.MAIN, 
                             diag(as.matrix(BETA.INT[1:ng,])), # Beta G;
                             t(do.call(cbind, lapply(2:ncolE, function(x) {diag(as.matrix(BETA.INT[(((x-1)*ng)+1):(ng*x),]))}))), # Beta GxE and then Beta Covariates
                             t(sqrt(do.call(cbind, lapply(seq(1,ncolE*ncolE, ncolE+1), function(x) {IV.V_i[[x]]})))),
                             t(do.call(cbind, lapply(split_mat[lower.tri(split_mat)], function(x) {IV.V_i[[x]]}))),
                             PVAL.MAIN, STAT.INT, PVAL.INT, PVAL.JOINT))
              } else {
                split_mat <- as.matrix(split_mat[2:(ei+1),2:(ei+1)])
                if (length(split_mat) == 1) {
                  return(rbind(N, MAC, RSQ, t(freq_N), BETA.MAIN, SE.MAIN,
                               t(do.call(cbind, lapply(2:(ei+1), function(x) {diag(as.matrix(BETA.INT[(((x-1)*ng)+1):(ng*x),]))}))), # Beta GxE only
                               t(sqrt(do.call(cbind,lapply(diag(split_mat), function(x) {IV.V_i[[x]]})))),   # SE Beta GxE only
                               PVAL.MAIN, STAT.INT, PVAL.INT, PVAL.JOINT))
                } else {
                  return(rbind(N, MAC, RSQ, t(freq_N), BETA.MAIN, SE.MAIN,
                               t(do.call(cbind, lapply(2:(ei+1), function(x) {diag(as.matrix(BETA.INT[(((x-1)*ng)+1):(ng*x),]))}))), # Beta GxE only
                               t(sqrt(do.call(cbind,lapply(diag(split_mat), function(x) {IV.V_i[[x]]})))),   # SE Beta GxE only
                               t(do.call(cbind, lapply(split_mat[lower.tri(split_mat)], function(x) {IV.V_i[[x]]}))),
                               PVAL.MAIN, STAT.INT, PVAL.INT, PVAL.JOINT)) 
                }
              }
            }
            else {
              if (meta.output) {
                return(rbind(N, MAC, RSQ, BETA.MAIN, SE.MAIN, 
                             diag(as.matrix(BETA.INT[1:ng,])), # Beta G;
                             t(do.call(cbind, lapply(2:ncolE, function(x) {diag(as.matrix(BETA.INT[(((x-1)*ng)+1):(ng*x),]))}))), # Beta GxE and then Beta Covariates
                             t(sqrt(do.call(cbind, lapply(seq(1,ncolE*ncolE, ncolE+1), function(x) {IV.V_i[[x]]})))),
                             t(do.call(cbind, lapply(split_mat[lower.tri(split_mat)], function(x) {IV.V_i[[x]]}))),
                             PVAL.MAIN, STAT.INT, PVAL.INT, PVAL.JOINT))
              } else {
                split_mat <- as.matrix(split_mat[2:(ei+1),2:(ei+1)])
                if (length(split_mat) == 1) {
                  return(rbind(N, MAC, RSQ, BETA.MAIN, SE.MAIN,
                               t(do.call(cbind, lapply(2:(ei+1), function(x) {diag(as.matrix(BETA.INT[(((x-1)*ng)+1):(ng*x),]))}))), # Beta GxE only
                               t(sqrt(do.call(cbind,lapply(diag(split_mat), function(x) {IV.V_i[[x]]})))),   # SE Beta GxE only
                               PVAL.MAIN, STAT.INT, PVAL.INT, PVAL.JOINT))
                } else {
                  return(rbind(N, MAC, RSQ, BETA.MAIN, SE.MAIN,
                               t(do.call(cbind, lapply(2:(ei+1), function(x) {diag(as.matrix(BETA.INT[(((x-1)*ng)+1):(ng*x),]))}))), # Beta GxE only
                               t(sqrt(do.call(cbind,lapply(diag(split_mat), function(x) {IV.V_i[[x]]})))),   # SE Beta GxE only
                               t(do.call(cbind, lapply(split_mat[lower.tri(split_mat)], function(x) {IV.V_i[[x]]}))),
                               PVAL.MAIN, STAT.INT, PVAL.INT, PVAL.JOINT)) 
                }
              }
            }

            
          })
          
          if (!is.null(strata.list)){
            if (any(include)) {
              out <- out[include,]
              tmp.out <- matrix(unlist(tmp.out), ncol = totalCol, byrow = TRUE, dimnames = list(NULL, c("N", "MAC", "RSQ", bin_header, "BETA.MARGINAL", "SE.MARGINAL", meta.header, "PVAL.MARGINAL", "STAT.INT", "PVAL.INT", "PVAL.JOINT")))
	      include <- include & tmp.out[,"MAC"] >= MAC.cutoff & tmp.out[,"RSQ"] >= RSQ.cutoff
              out <- cbind(out[include,c("SNP","CHR","POS","REF","ALT")], tmp.out[include,"N", drop = F], out[include,"AF",drop=F], tmp.out[include,c("MAC", "RSQ", bin_header, "BETA.MARGINAL", "SE.MARGINAL", meta.header, "PVAL.MARGINAL", "PVAL.INT", "PVAL.JOINT"), drop = F])
              write.table(out, paste0(outfile, "_tmp.", b), quote=FALSE, row.names=FALSE, col.names=FALSE, sep="\t", append=TRUE, na=".")
            }
          }
          else {
            if (any(include)) {
              out <- out[include,]
              tmp.out <- matrix(unlist(tmp.out), ncol = totalCol, byrow = TRUE, dimnames = list(NULL, c("N", "MAC", "RSQ", "BETA.MARGINAL", "SE.MARGINAL", meta.header, "PVAL.MARGINAL", "STAT.INT", "PVAL.INT", "PVAL.JOINT")))
	      include <- include & tmp.out[,"MAC"] >= MAC.cutoff & tmp.out[,"RSQ"] >= RSQ.cutoff
              out <- cbind(out[include,c("SNP","CHR","POS","REF","ALT")], tmp.out[include,"N", drop = F], out[include,"AF",drop=F], tmp.out[include,c("MAC", "RSQ", "BETA.MARGINAL", "SE.MARGINAL", meta.header, "PVAL.MARGINAL", "PVAL.INT", "PVAL.JOINT"), drop = F])
              write.table(out, paste0(outfile, "_tmp.", b), quote=FALSE, row.names=FALSE, col.names=FALSE, sep="\t", append=TRUE, na=".")
            }
          }
          
          
          rm(tmp.out)
          rm(out)
        }
        SeqArray::seqClose(gds)
      }
      if (inherits(geno.file, "SeqVarGDSClass")) {
        SeqArray::seqClose(geno.file)
      }
      for(b in 1:ncores) {
        system(paste0("cat ", outfile, "_tmp.", b, " >> ", outfile))
        system(paste0("cat ", outfile, "_tmp.", b , ".err", " >> ", outfile, ".err"))
        unlink(paste0(outfile, "_tmp.", b))
        unlink(paste0(outfile, "_tmp.", b , ".err"))
      }
    } else { # use a single core
      variant.idx <- variant.idx.all
      rm(variant.idx.all)
      p <- length(variant.idx)
      if (!inherits(geno.file, "SeqVarGDSClass")) {
        gds <- SeqArray::seqOpen(geno.file)
      }
      SeqArray::seqSetFilter(gds, sample.id = sample.id, verbose = FALSE)
      rm(sample.id)
      nbatch.flush <- (p-1) %/% 100000 + 1
      ii <- 0
      
      if (meta.output) {
        interaction2 <- c("G", paste0("G-", interaction))
        cov.header = matrix(paste(rep(paste0("Cov_Beta_", interaction2), each = ncolE), interaction2, sep = "_"), ncolE, ncolE)
        meta.header = c(paste0("Beta_", interaction2), paste0("SE_Beta_", interaction2), cov.header[lower.tri(cov.header)])
        
        if (is.null(strata.list)){
       
          totalCol =  11 + 3*(ei+qi) + ((ei+qi) * ((ei+qi) - 1) / 2)
        }else 
        {

            totalCol =  11 +2*length(unique(strata))+ 3*(ei+qi) + ((ei+qi) * ((ei+qi) - 1) / 2)
            }
 
      } else {
        interaction2 <- paste0("G-", interaction[1:ei])
        if (ei != 1) {
          cov.header = matrix(paste(rep(paste0("Cov_Beta_", interaction2), each = ei), interaction, sep = "_G-"), ei, ei)
          meta.header = c(paste0("Beta_", interaction2), paste0("SE_Beta_", interaction2), cov.header[lower.tri(cov.header)])
        } else {
          meta.header = c(paste0("Beta_", interaction2), paste0("SE_Beta_", interaction2))
          
        }
        
        if (is.null(strata.list)) {
          totalCol = 9+ ei + ei + ei * (ei - 1) / 2
        } else {totalCol = 9+ ei + ei+ +2*length(unique(strata))+ ei * (ei - 1) / 2} 
      }
      if (is.null(strata.list)){
        write.table(t(data.frame(n = c("SNPID","CHR","POS","Non_Effect_Allele","Effect_Allele", "N_Samples", "AF", "MAC", "RSQ", "Beta_Marginal", "SE_Beta_Marginal", meta.header, "P_Value_Marginal",  "P_Value_Interaction", "P_Value_Joint"))), outfile, quote = F, col.names = F, row.names = F, sep="\t")
        
      } else {
         write.table(t(data.frame(n = c("SNPID","CHR","POS","Non_Effect_Allele","Effect_Allele", "N_Samples", "AF", "MAC", "RSQ", bin_header, "Beta_Marginal", "SE_Beta_Marginal", meta.header, "P_Value_Marginal",  "P_Value_Interaction", "P_Value_Joint"))), outfile, quote = F, col.names = F, row.names = F, sep="\t")
      }
      
      debug_file <- paste0(outfile, ".err")
      file.create(debug_file)
      for(i in 1:nbatch.flush) {
        gc()
        tmp.variant.idx <- if(i == nbatch.flush) variant.idx[((i-1)*100000+1):p] else variant.idx[((i-1)*100000+1):(i*100000)]
        SeqArray::seqSetFilter(gds, variant.id = tmp.variant.idx, verbose = FALSE)
        MISSRATE <- if(is.dosage) SeqArray::seqApply(gds, "annotation/format/DS", function(xx) mean(is.na(xx)), margin = "by.variant", as.is = "double") else SeqVarTools::missingGenotypeRate(gds, margin = "by.variant")
        AF <- if(is.dosage) SeqArray::seqApply(gds, "annotation/format/DS", mean, margin = "by.variant", as.is = "double", na.rm = TRUE)/2 else 1 - SeqVarTools::alleleFrequency(gds)
	#MAC <- SeqVarTools::minorAlleleCount(gds)
        #include <- (MISSRATE <= miss.cutoff & MAC >= MAC.cutoff & ((AF >= MAF.range[1] & AF <= MAF.range[2]) | (AF >= 1-MAF.range[2] & AF <= 1-MAF.range[1])))
        include <- (MISSRATE <= miss.cutoff & ((AF >= MAF.range[1] & AF <= MAF.range[2]) | (AF >= 1-MAF.range[2] & AF <= 1-MAF.range[1])))

        if(sum(include) == 0) {
          next
        }
        ii <- ii + 1
        tmp.variant.idx <- tmp.variant.idx[include]
        tmp.p <- length(tmp.variant.idx)
        SeqArray::seqSetFilter(gds, variant.id = tmp.variant.idx, verbose = FALSE)
        SNP <- SeqArray::seqGetData(gds, "annotation/id")
        SNP[SNP == ""] <- NA
        out <- data.frame(SNP = SNP, CHR = SeqArray::seqGetData(gds, "chromosome"), POS = SeqArray::seqGetData(gds, "position"))
        rm(SNP)
        alleles.list <- strsplit(SeqArray::seqGetData(gds, "allele"), ",")
        out$REF <- unlist(lapply(alleles.list, function(x) x[1]))
        out$ALT <- unlist(lapply(alleles.list, function(x) paste(x[-1], collapse=",")))
        out$MISSRATE <- MISSRATE[include]
        out$AF <- AF[include]
	#out$MAC <- MAC[include]
        include <- include[include]
        rm(alleles.list)
        tmp_idx <- 1
        tmp.out <- lapply(1:((tmp.p-1) %/% nperbatch + 1), function(j) {
          tmp2.variant.idx <- if(j == (tmp.p-1) %/% nperbatch + 1) tmp.variant.idx[((j-1)*nperbatch+1):tmp.p] else tmp.variant.idx[((j-1)*nperbatch+1):(j*nperbatch)]
          SeqArray::seqSetFilter(gds, variant.id = tmp2.variant.idx, verbose = FALSE)
          geno <- SeqVarTools::altDosage(gds, use.names = FALSE)
          ng <- ncol(geno)
          freq <- colMeans(geno, na.rm = TRUE)/2
          N <- nrow(geno) - colSums(is.na(geno))
	  MAC <- colSums(geno, na.rm = TRUE)
	  MAC[MAC>N] <- (2*N - MAC)[MAC>N]
	  RSQ <- apply(geno, 2, .calc_rsq)
          if(!is.null(strata.list)) { # E is not continuous
            freq.tmp <- sapply(strata.list, function(x) colMeans(geno[x, , drop = FALSE], na.rm = TRUE)/2)
	    if(is.null(ncol(freq.tmp))) freq.tmp <- matrix(freq.tmp, nrow = 1, dimnames = list(NULL, names(freq.tmp)))
            n.tmp <- sapply(strata.list, function(x) colSums(!is.na(geno[x, , drop = FALSE])))
	    if(is.null(ncol(n.tmp))) n.tmp <- matrix(n.tmp, nrow = 1, dimnames = list(NULL, names(n.tmp)))
            freq.tmp.rev<-freq.tmp[,order(ncol(freq.tmp):1),drop=FALSE]
            n.tmp.rev<-n.tmp[,order(ncol(n.tmp):1),drop=FALSE]
            #combine the freq.tmp and n.tmp by alterating columns
            rows.freq_N<-nrow(freq.tmp)
            cols.freq_N<-ncol(freq.tmp)+ncol(n.tmp)
            freq_N<-matrix(NA,nrow =rows.freq_N,ncol=cols.freq_N)
            freq_N[,seq(1,cols.freq_N,2)]<-n.tmp
            freq_N[,seq(2,cols.freq_N,2)]<-freq.tmp
          } else {freq_N<-NA}
    
          miss.idx <- which(is.na(geno))
          if(length(miss.idx)>0) {
            geno[miss.idx] <- if(missing.method == "impute2mean") 2*freq[ceiling(miss.idx/nrow(geno))] else NA
          }

          if(geno.center) geno <- scale(geno, scale = FALSE)
          miss.idx <- which(is.na(geno))
          if(length(miss.idx)>0) { # omit
            geno[miss.idx] <- 0
          }
          if(any(duplicated(null.obj$id_include))) geno <- crossprod(J, geno)

          K <- do.call(cbind, sapply(1:ncolE, function(xx) geno*E[,xx], simplify = FALSE), envir = environment())
          U <- as.vector(crossprod(geno, residuals))

          if(!is.null(null.obj$P)) {
            PG <- crossprod(null.obj$P, geno)
          } else {
            GSigma_iX <- crossprod(geno, null.obj$Sigma_iX)
            PG <- crossprod(null.obj$Sigma_i, geno) - tcrossprod(null.obj$Sigma_iX, tcrossprod(GSigma_iX, null.obj$cov))
          }
          
          GPG <- as.matrix(crossprod(geno, PG)) * (matrix(1, 1, 1) %x% diag(ng))
          
          GPG_i <- try(solve(GPG), silent = TRUE)
          if(inherits(GPG_i, "try-error")) GPG_i <- MASS::ginv(GPG)
          V_i <- pmax(0,diag(GPG_i))

          BETA.MAIN <- V_i * U
          
          SE.MAIN   <- sqrt(V_i)
          STAT.MAIN <- BETA.MAIN * U
          PVAL.MAIN <- ifelse(V_i>0, pchisq(STAT.MAIN, df=1, lower.tail=FALSE), NA)

          if(!is.null(null.obj$P)) {
            KPK <- crossprod(K,crossprod(null.obj$P,K))
          } else {
            KSigma_iX <- crossprod(K, null.obj$Sigma_iX)
            KPK <- crossprod(K, crossprod(null.obj$Sigma_i, K)) - tcrossprod(KSigma_iX, tcrossprod(KSigma_iX, null.obj$cov))
          }
   
          KPK <- as.matrix(KPK) * (matrix(1, ncolE, ncolE) %x% diag(ng))

          
          IV.V_i <- try(solve(KPK), silent = TRUE)
     
          if(inherits(IV.V_i, "try-error")) IV.V_i <- try(MASS::ginv(KPK), silent = TRUE)
          if (inherits(IV.V_i, "try-error")) {
            fix_out <- fix.dgesdd(gds, out, debug_file, null.obj, J, residuals, tmp2.variant.idx, meta.output, geno.center, missing.method, strata.list, ncolE, E, ei, bin_header, meta.header, totalCol, tmp_idx, include, is.dosage)
            tmp_idx <<- fix_out[[1]]
            include <<- fix_out[[2]]
            return(fix_out[[3]])
          }
	  diag(IV.V_i) <- pmax(0,diag(IV.V_i))
          IV.U <- (rep(1, ncolE) %x% diag(ng)) * as.vector(crossprod(K,residuals))
          
          BETA.INT <- crossprod(IV.V_i, IV.U)

          
          ng1   <- ng+1
          ngei1 <- ng*ei1
          
          IV.E_i <- try(solve(IV.V_i[ng1:ngei1, ng1:ngei1]), silent = TRUE)
          if(inherits(IV.E_i, "try-error")) IV.E_i <- try(MASS::ginv(IV.V_i[ng1:ngei1, ng1:ngei1]), silent = TRUE)
          if(inherits(IV.E_i, "try-error")) {
            fix_out <- fix.dgesdd(gds, out, debug_file, null.obj, J, residuals, tmp2.variant.idx, meta.output, geno.center, missing.method, strata.list, ncolE, E, ei, bin_header, meta.header, totalCol, tmp_idx, include, is.dosage)
            tmp_idx <<- fix_out[[1]]
            include <<- fix_out[[2]]
            return(fix_out[[3]])
          }
          STAT.INT   <- diag(crossprod(BETA.INT[ng1:ngei1,], crossprod(IV.E_i, BETA.INT[ng1:ngei1,])))
         
          IV.GE_i <- try(solve(IV.V_i[1:ngei1, 1:ngei1]), silent = TRUE)
          if(inherits(IV.GE_i, "try-error")) IV.GE_i <- try(MASS::ginv(IV.V_i[1:ngei1, 1:ngei1]), silent = TRUE)
          if(inherits(IV.GE_i, "try-error")) {
            fix_out <- fix.dgesdd(gds, out, debug_file, null.obj, J, residuals, tmp2.variant.idx, meta.output, geno.center, missing.method, strata.list, ncolE, E, ei, bin_header, meta.header, totalCol, tmp_idx, include, is.dosage)
            tmp_idx <<- fix_out[[1]]
            include <<- fix_out[[2]]
            return(fix_out[[3]])
          }
          STAT.JOINT <- diag(crossprod(BETA.INT[1:ngei1,], crossprod(IV.GE_i, BETA.INT[1:ngei1,])))
          
          PVAL.INT   <- pchisq(STAT.INT, df=ei, lower.tail=FALSE)
          PVAL.JOINT <- ifelse(is.na(PVAL.MAIN), NA, pchisq(STAT.JOINT, df=1+ei, lower.tail=FALSE))
     
          
          split_mat <- matrix(1:(ncolE*ncolE), ncolE, ncolE)
          if (ng > 1) {
            IV.V_i <- split(IV.V_i, split_mat %x% diag(ng))[-1]
          }else {
            IV.V_i <- split(IV.V_i, split_mat %x% diag(ng))
          }
          tmp_idx <<- tmp_idx + ng
          if (!is.null(strata.list)){
            if (meta.output) {
              return(rbind(N, MAC, RSQ, t(freq_N), BETA.MAIN, SE.MAIN, 
                           diag(as.matrix(BETA.INT[1:ng,])), # Beta G;
                           t(do.call(cbind, lapply(2:ncolE, function(x) {diag(as.matrix(BETA.INT[(((x-1)*ng)+1):(ng*x),]))}))), # Beta GxE and then Beta Covariates
                           t(sqrt(do.call(cbind, lapply(seq(1,ncolE*ncolE, ncolE+1), function(x) {IV.V_i[[x]]})))),
                           t(do.call(cbind, lapply(split_mat[lower.tri(split_mat)], function(x) {IV.V_i[[x]]}))),
                           PVAL.MAIN, STAT.INT, PVAL.INT, PVAL.JOINT))
            } else {
              split_mat <- as.matrix(split_mat[2:(ei+1),2:(ei+1)])
              if (length(split_mat) == 1) {
                return(rbind(N, MAC, RSQ, t(freq_N), BETA.MAIN, SE.MAIN,
                             t(do.call(cbind, lapply(2:(ei+1), function(x) {diag(as.matrix(BETA.INT[(((x-1)*ng)+1):(ng*x),]))}))), # Beta GxE only
                             t(sqrt(do.call(cbind,lapply(diag(split_mat), function(x) {IV.V_i[[x]]})))),   # SE Beta GxE only
                             PVAL.MAIN, STAT.INT, PVAL.INT, PVAL.JOINT))
              } else {
                return(rbind(N, MAC, RSQ, t(freq_N), BETA.MAIN, SE.MAIN,
                             t(do.call(cbind, lapply(2:(ei+1), function(x) {diag(as.matrix(BETA.INT[(((x-1)*ng)+1):(ng*x),]))}))), # Beta GxE only
                             t(sqrt(do.call(cbind,lapply(diag(split_mat), function(x) {IV.V_i[[x]]})))),   # SE Beta GxE only
                             t(do.call(cbind, lapply(split_mat[lower.tri(split_mat)], function(x) {IV.V_i[[x]]}))),
                             PVAL.MAIN, STAT.INT, PVAL.INT, PVAL.JOINT)) 
              }
            }
          }else {
            if (meta.output) {
              return(rbind(N, MAC, RSQ, BETA.MAIN, SE.MAIN, 
                           diag(as.matrix(BETA.INT[1:ng,])), # Beta G;
                           t(do.call(cbind, lapply(2:ncolE, function(x) {diag(as.matrix(BETA.INT[(((x-1)*ng)+1):(ng*x),]))}))), # Beta GxE and then Beta Covariates
                           t(sqrt(do.call(cbind, lapply(seq(1,ncolE*ncolE, ncolE+1), function(x) {IV.V_i[[x]]})))),
                           t(do.call(cbind, lapply(split_mat[lower.tri(split_mat)], function(x) {IV.V_i[[x]]}))),
                           PVAL.MAIN, STAT.INT, PVAL.INT, PVAL.JOINT))
            } else {
              split_mat <- as.matrix(split_mat[2:(ei+1),2:(ei+1)])
              if (length(split_mat) == 1) {
                return(rbind(N, MAC, RSQ, BETA.MAIN, SE.MAIN,
                             t(do.call(cbind, lapply(2:(ei+1), function(x) {diag(as.matrix(BETA.INT[(((x-1)*ng)+1):(ng*x),]))}))), # Beta GxE only
                             t(sqrt(do.call(cbind,lapply(diag(split_mat), function(x) {IV.V_i[[x]]})))),   # SE Beta GxE only
                             PVAL.MAIN, STAT.INT, PVAL.INT, PVAL.JOINT))
              } else {
                return(rbind(N, MAC, RSQ, BETA.MAIN, SE.MAIN,
                             t(do.call(cbind, lapply(2:(ei+1), function(x) {diag(as.matrix(BETA.INT[(((x-1)*ng)+1):(ng*x),]))}))), # Beta GxE only
                             t(sqrt(do.call(cbind,lapply(diag(split_mat), function(x) {IV.V_i[[x]]})))),   # SE Beta GxE only
                             t(do.call(cbind, lapply(split_mat[lower.tri(split_mat)], function(x) {IV.V_i[[x]]}))),
                             PVAL.MAIN, STAT.INT, PVAL.INT, PVAL.JOINT)) 
              }
            }
          }
          
        })

        

        if (!is.null(strata.list)){
          if (any(include)) {
            out <- out[include,]
            tmp.out <- matrix(unlist(tmp.out), ncol = totalCol, byrow = TRUE, dimnames = list(NULL, c("N", "MAC", "RSQ", bin_header, "BETA.MARGINAL", "SE.MARGINAL", meta.header, "PVAL.MARGINAL", "STAT.INT", "PVAL.INT", "PVAL.JOINT")))
	    include <- include & tmp.out[,"MAC"] >= MAC.cutoff & tmp.out[,"RSQ"] >= RSQ.cutoff
            out <- cbind(out[include,c("SNP","CHR","POS","REF","ALT")], tmp.out[include,"N", drop = F], out[include,"AF",drop=F], tmp.out[include,c("MAC", "RSQ", bin_header, "BETA.MARGINAL", "SE.MARGINAL", meta.header, "PVAL.MARGINAL",  "PVAL.INT", "PVAL.JOINT"), drop = F])
            write.table(out, outfile, quote=FALSE, row.names=FALSE, col.names=FALSE, sep="\t", append=TRUE, na=".")
          }
        }else {
          if (any(include)) {
            out <- out[include,]
            tmp.out <- matrix(unlist(tmp.out), ncol = totalCol, byrow = TRUE, dimnames = list(NULL, c("N", "MAC", "RSQ", "BETA.MARGINAL", "SE.MARGINAL", meta.header, "PVAL.MARGINAL", "STAT.INT", "PVAL.INT", "PVAL.JOINT")))
	    include <- include & tmp.out[,"MAC"] >= MAC.cutoff & tmp.out[,"RSQ"] >= RSQ.cutoff
            out <- cbind(out[include,c("SNP","CHR","POS","REF","ALT")], tmp.out[include,"N", drop = F], out[include,"AF",drop=F], tmp.out[include,c("MAC", "RSQ", "BETA.MARGINAL", "SE.MARGINAL", meta.header, "PVAL.MARGINAL",  "PVAL.INT", "PVAL.JOINT"), drop = F])
            write.table(out, outfile, quote=FALSE, row.names=FALSE, col.names=FALSE, sep="\t", append=TRUE, na=".")
          }
        }
        
        rm(tmp.out)
        rm(out)
      }
      SeqArray::seqClose(gds)
    }
    if(!verbose) unlink(paste0(outfile, ".err"))
    return(invisible(NULL))
  } else if (grepl("\\.bgen$", geno.file)) {
    
    bgenInfo <- .Call('bgenHeader', geno.file)
    
    if (is.null(bgen.samplefile) && bgenInfo$SampleIdFlag == 0) {
      stop("Error: bgen file does not contain sample identifiers. A .sample file (bgen.samplefile) is needed.")
    }
    if(is.null(bgen.samplefile)) {
      sample.id <- bgenInfo$SampleIds
    } else {
      sample.id <- fread(bgen.samplefile, header = TRUE, data.table = FALSE)
      if ((nrow(sample.id)-1) != bgenInfo$N){
        stop(paste0("Error: Number of sample identifiers in BGEN sample file (", nrow(sample.id)-1, ") does not match number of samples in BGEN file (", bgenInfo$N,")."))
      }
      sample.id <- sample.id[-1, 2]
    }
    
    if(any(is.na(match(null.obj$id_include, sample.id)))) warning("Check your data... Some individuals in null.obj$id_include are missing in sample.id of bgen sample file!", call. = FALSE)

    sample.id_original<-sample.id
    sample.id <- sample.id[sample.id %in% unique(null.obj$id_include)]


    sel<-match(sample.id_original,sample.id)
    sel[is.na(sel)]<-0
    if(length(sample.id) == 0) stop("Error: null.obj$id_include does not match sample.id in geno.file!")
    if(any(duplicated(null.obj$id_include))) {
      match.id <- null.obj$id_include %in% sample.id
      null.obj$id_include <- null.obj$id_include[match.id]
      J <- t(sparseMatrix(i=1:length(null.obj$id_include), j=match(null.obj$id_include, unique(null.obj$id_include)[match(sample.id, unique(null.obj$id_include))]), x=1))
      null.obj$J=J
      dupeflag<-1
    }else match.id <- match(sample.id, null.obj$id_include)

    E <- as.matrix(E[match.id, , drop = FALSE])
    
    Ebin<-apply(as.matrix(E[!duplicated(null.obj$id_include),,drop=FALSE]),2,function(x) length(unique(x))<=cat.threshold)
    if(any(Ebin)){
      Ecat<-as.matrix(E[!duplicated(null.obj$id_include),Ebin,drop=FALSE])
      strata <- apply(Ecat, 1, paste, collapse = "_")
      
      uni.strata<-unique(rev(strata))
      uni.strata<-sort(uni.strata)
      cat_inter<-paste(interaction[Ebin], collapse = '_')
      tmp<-apply(as.matrix(uni.strata),1,function(x) paste(x, collapse = '_'))
      tmp1<-paste0(cat_inter,"_",tmp)
      tmp2<-c("N","AF")
      bin_header<-c(apply(as.matrix(tmp1),1, function(x) paste0(tmp2,"_",x)))
    }else {
      bin_header=NULL
    }
    strata <- if(any(Ebin))  as.numeric(as.factor(apply(as.matrix(E[!duplicated(null.obj$id_include),Ebin,drop=FALSE]), 1, paste, collapse = "_"))) else NULL 
    
    if(!is.null(strata)) {
      strata.list <- lapply(sort(unique(strata)), function(x) which(strata==x))
    } else {
      strata.list <- NULL
    }
    
    if(covar.center == "all") {
      E <- scale(E, scale = FALSE)
    } else if(covar.center != "none") {
      if(!is.null(interaction.covariates)) {
        E <- cbind(E[,1:ei,drop=F], scale(E[,(1+ei):(qi+ei),drop=F], scale = FALSE))
      }
    }
    residuals <- residuals[match.id]
    if(!is.null(null.obj$P)) {
      null.obj$P <- null.obj$P[match.id, match.id]
    } else {
      null.obj$Sigma_iX <- Matrix(null.obj$Sigma_iX[match.id, , drop = FALSE], sparse = TRUE)
      null.obj$Sigma_i <- Matrix(null.obj$Sigma_i[match.id, match.id], sparse = TRUE)
      null.obj$cov <- Matrix(null.obj$cov, sparse = TRUE)
    }
    
   
    if (is.null(interaction.covariates)) {
      null.obj$E <- E

    } else {

      null.obj$E <- E
      null.obj$EC <- E[,(1+ei):(qi+ei), drop = F]

    }
    

    ncolE<- ncol(E)+1
    rm(sample.id, E)
    
    variant.idx.all <- 1:bgenInfo$M
    p.all <- length(variant.idx.all)

    if (ncores > bgenInfo$M) {
      warning("Number of cores (", ncores, ") is greater than number of variants in BGEN files (", bgenInfo$M, "). Using ", bgenInfo$M, " cores instead.", call. = FALSE)
      ncores <- bgenInfo$M
    }
    
    threadInfo <- .Call("getVariantPos", geno.file, bgenInfo$offset, bgenInfo$M, bgenInfo$N, bgenInfo$CompressionFlag, bgenInfo$LayoutFlag, ncores)
    center2 <- ifelse(geno.center, 'c', 'n')
    missing.method <- substr(missing.method, 1, 1)
    
    if (ncores > 1) {
      doMC::registerDoMC(cores = ncores)
      foreach(i=1:ncores) %dopar% {
        if (bgenInfo$LayoutFlag == 2) {
          .Call("glmm_gei_bgen13", is.null(dupeflag), as.numeric(residuals), null.obj, geno.file, paste0(outfile, "_tmp.", i), center2, MAF.range[1], MAF.range[2], MAC.cutoff, miss.cutoff, RSQ.cutoff, missing.method, nperbatch, ei, qi, is.null(null.obj$P),strata.list, sel, threadInfo$begin[i], threadInfo$end[i], threadInfo$pos[i], bgenInfo$N, bgenInfo$CompressionFlag, meta.output)
        } else {
          .Call("glmm_gei_bgen11", is.null(dupeflag),as.numeric(residuals), null.obj, geno.file, paste0(outfile, "_tmp.", i), center2, MAF.range[1], MAF.range[2], MAC.cutoff, miss.cutoff, RSQ.cutoff, missing.method, nperbatch, ei, qi, is.null(null.obj$P),  strata.list, sel, threadInfo$begin[i], threadInfo$end[i], threadInfo$pos[i], bgenInfo$N, bgenInfo$CompressionFlag, meta.output)
        }
      }
      
      outTmp <- file(outfile, "w")
      if (meta.output) {
        interaction2 <- c("G", paste0("G-", interaction))
        cov.header = matrix(paste(rep(paste0("Cov_Beta_", interaction2), each = ncolE), interaction2, sep = "_"), ncolE, ncolE)
        ss.header = c(paste0("Beta_", interaction2), paste0("SE_Beta_", interaction2), cov.header[lower.tri(cov.header)])
	if(is.null(bin_header)) {
          writeLines(paste0("SNPID\tRSID\tCHR\tPOS\tNon_Effect_Allele\tEffect_Allele\tN_Samples\tAF\tMAC\tRSQ\tBeta_Marginal\tSE_Beta_Marginal\t",paste0(ss.header, collapse = "\t"),"\tP_Value_Marginal\tP_Value_Interaction\tP_Value_Joint"), outTmp)
	} else {
          writeLines(paste0("SNPID\tRSID\tCHR\tPOS\tNon_Effect_Allele\tEffect_Allele\tN_Samples\tAF\tMAC\tRSQ\t",paste0(bin_header, collapse = "\t"),"\tBeta_Marginal\tSE_Beta_Marginal\t",paste0(ss.header, collapse = "\t"),"\tP_Value_Marginal\tP_Value_Interaction\tP_Value_Joint"), outTmp)
        }
         } else {
        interaction2 <- paste0("G-", interaction[1:ei])
        if (ei != 1) {
          cov.header = matrix(paste(rep(paste0("Cov_Beta_", interaction2), each = ei), interaction, sep = "_G-"), ei, ei)
          ss.header = c(paste0("Beta_", interaction2), paste0("SE_Beta_", interaction2), cov.header[lower.tri(cov.header)])
        } else {
          ss.header = c(paste0("Beta_", interaction2), paste0("SE_Beta_", interaction2))
        }
	if(is.null(bin_header)) {
          writeLines(paste0("SNPID\tRSID\tCHR\tPOS\tNon_Effect_Allele\tEffect_Allele\tN_Samples\tAF\tMAC\tRSQ\tBeta_Marginal\tSE_Beta_Marginal\t",paste0(ss.header, collapse = "\t"),"\tP_Value_Marginal\tP_Value_Interaction\tP_Value_Joint"), outTmp)
        } else {
          writeLines(paste0("SNPID\tRSID\tCHR\tPOS\tNon_Effect_Allele\tEffect_Allele\tN_Samples\tAF\tMAC\tRSQ\t",paste0(bin_header, collapse = "\t"),"\tBeta_Marginal\tSE_Beta_Marginal\t",paste0(ss.header, collapse = "\t"),"\tP_Value_Marginal\tP_Value_Interaction\tP_Value_Joint"), outTmp)
	}
        }
    
      for(i in 1:ncores){
        inTmp <- readLines(paste0(outfile, "_tmp.", i))
        writeLines(inTmp, outTmp)
        unlink(paste0(outfile, "_tmp.", i))
      }
      close(outTmp)
    } else {
      if (bgenInfo$LayoutFlag == 2) {

        .Call("glmm_gei_bgen13", is.null(dupeflag), as.numeric(residuals), null.obj, geno.file, paste0(outfile, "_tmp"), center2, MAF.range[1], MAF.range[2], MAC.cutoff, miss.cutoff, RSQ.cutoff, missing.method, nperbatch, ei, qi, is.null(null.obj$P),  strata.list, sel, threadInfo$begin[1], threadInfo$end[1], threadInfo$pos[1], bgenInfo$N, bgenInfo$CompressionFlag, meta.output)
        
        } else {
        .Call("glmm_gei_bgen11", is.null(dupeflag), as.numeric(residuals), null.obj, geno.file, paste0(outfile, "_tmp"), center2, MAF.range[1], MAF.range[2], MAC.cutoff, miss.cutoff, RSQ.cutoff, missing.method, nperbatch, ei, qi, is.null(null.obj$P),  strata.list, sel, threadInfo$begin[1], threadInfo$end[1], threadInfo$pos[1], bgenInfo$N, bgenInfo$CompressionFlag, meta.output)
      }
      
      outTmp <- file(outfile, "w")
      
      if (meta.output) {

          interaction2 <- c("G", paste0("G-", interaction))
          cov.header = matrix(paste(rep(paste0("Cov_Beta_", interaction2), each = ncolE), interaction2, sep = "_"), ncolE, ncolE)
          ss.header = c(paste0("Beta_", interaction2), paste0("SE_Beta_", interaction2), cov.header[lower.tri(cov.header)])
	  if(is.null(bin_header)) {
            writeLines(paste0("SNPID\tRSID\tCHR\tPOS\tNon_Effect_Allele\tEffect_Allele\tN_Samples\tAF\tMAC\tRSQ\tBeta_Marginal\tSE_Beta_Marginal\t",paste0(ss.header, collapse = "\t"),"\tP_Value_Marginal\tP_Value_Interaction\tP_Value_Joint"), outTmp)
	  } else {
            writeLines(paste0("SNPID\tRSID\tCHR\tPOS\tNon_Effect_Allele\tEffect_Allele\tN_Samples\tAF\tMAC\tRSQ\t",paste0(bin_header, collapse = "\t"),"\tBeta_Marginal\tSE_Beta_Marginal\t",paste0(ss.header, collapse = "\t"),"\tP_Value_Marginal\tP_Value_Interaction\tP_Value_Joint"), outTmp)
	  }
          } else {
          interaction2 <- paste0("G-", interaction[1:ei])
          if (ei != 1) {
            cov.header = matrix(paste(rep(paste0("Cov_Beta_", interaction2), each = ei), interaction, sep = "_G-"), ei, ei)
            ss.header = c(paste0("Beta_", interaction2), paste0("SE_Beta_", interaction2), cov.header[lower.tri(cov.header)])
          } else {
            ss.header = c(paste0("Beta_", interaction2), paste0("SE_Beta_", interaction2))
          }
	  if(is.null(bin_header)) {
            writeLines(paste0("SNPID\tRSID\tCHR\tPOS\tNon_Effect_Allele\tEffect_Allele\tN_Samples\tAF\tMAC\tRSQ\tBeta_Marginal\tSE_Beta_Marginal\t",paste0(ss.header, collapse = "\t"),"\tP_Value_Marginal\tP_Value_Interaction\tP_Value_Joint"), outTmp)
          } else {
            writeLines(paste0("SNPID\tRSID\tCHR\tPOS\tNon_Effect_Allele\tEffect_Allele\tN_Samples\tAF\tMAC\tRSQ\t",paste0(bin_header, collapse = "\t"),"\tBeta_Marginal\tSE_Beta_Marginal\t",paste0(ss.header, collapse = "\t"),"\tP_Value_Marginal\tP_Value_Interaction\tP_Value_Joint"), outTmp)
	  }
        }
      inTmp <- readLines(paste0(outfile, "_tmp"))
      writeLines(inTmp, outTmp)
      unlink(paste0(outfile, "_tmp"))
      close(outTmp)
    }
    return(invisible(NULL))
  }
}


fix.dgesdd <- function(gds, out, debug_file, null.obj, J, residuals, tmp2.variant.idx, meta.output, geno.center, missing.method, strata.list, ncolE, E, ei, bin_header, meta.header, totalCol, tmp_idx, include, is.dosage) {
  ei1 <- ei+1
  tmp_idx0 <- tmp_idx
  tmp.out <- lapply(tmp2.variant.idx, function(j) {
   
    SeqArray::seqSetFilter(gds, variant.id = j, verbose = FALSE)
    geno <- if(is.dosage) SeqVarTools::imputedDosage(gds, use.names = FALSE) else SeqVarTools::altDosage(gds, use.names = FALSE)
    ng <- ncol(geno)
    freq <- colMeans(geno, na.rm = TRUE)/2
    if(any(duplicated(null.obj$id_include))) geno <- crossprod(J, geno)
    N <- nrow(geno) - colSums(is.na(geno))
    MAC <- colSums(geno, na.rm = TRUE)
    MAC[MAC>N] <- (2*N - MAC)[MAC>N]
    RSQ <- apply(geno, 2, .calc_rsq)
    if(!is.null(strata.list)) { # E is not continuous
      freq.tmp <- sapply(strata.list, function(x) colMeans(geno[x, , drop = FALSE], na.rm = TRUE)/2)
      if(is.null(ncol(freq.tmp))) freq.tmp <- matrix(freq.tmp, nrow = 1, dimnames = list(NULL, names(freq.tmp)))
      n.tmp <- sapply(strata.list, function(x) colSums(!is.na(geno[x, , drop = FALSE])))
      if(is.null(ncol(n.tmp))) n.tmp <- matrix(n.tmp, nrow = 1, dimnames = list(NULL, names(n.tmp)))
      freq.tmp.rev<-freq.tmp[,order(ncol(freq.tmp):1),drop=FALSE]
      n.tmp.rev<-n.tmp[,order(ncol(n.tmp):1),drop=FALSE]
      rows.freq_N<-nrow(freq.tmp)
      cols.freq_N<-ncol(freq.tmp)+ncol(n.tmp)
      freq_N<-matrix(NA,nrow =rows.freq_N,ncol=cols.freq_N)
      freq_N[,seq(1,cols.freq_N,2)]<-n.tmp
      freq_N[,seq(2,cols.freq_N,2)]<-freq.tmp
    } else {freq_N<-NA}
    
    miss.idx <- which(is.na(geno))
    if(length(miss.idx)>0) {
      geno[miss.idx] <- if(missing.method == "impute2mean") 2*freq[ceiling(miss.idx/nrow(geno))] else NA
    }
    if(geno.center) geno <- scale(geno, scale = FALSE)
    miss.idx <- which(is.na(geno))
    if(length(miss.idx)>0) { # omit
      geno[miss.idx] <- 0
    }
            
    K <- do.call(cbind, sapply(1:ncolE, function(xx) geno*E[,xx], simplify = FALSE), envir = environment())

    U <- as.vector(crossprod(geno, residuals))
    if(!is.null(null.obj$P)) {
      PG <- crossprod(null.obj$P, geno)
    } else {
      GSigma_iX <- crossprod(geno, null.obj$Sigma_iX)
      PG <- crossprod(null.obj$Sigma_i, geno) - tcrossprod(null.obj$Sigma_iX, tcrossprod(GSigma_iX, null.obj$cov))
    }

    GPG <- as.matrix(crossprod(geno, PG)) * (matrix(1, 1, 1) %x% diag(ng))
    GPG_i <- try(solve(GPG), silent = TRUE)
    if(inherits(GPG_i, "try-error")) GPG_i <- MASS::ginv(GPG)
    V_i <- pmax(0,diag(GPG_i))
    
    BETA.MAIN <- V_i * U
    SE.MAIN   <- sqrt(V_i)
    STAT.MAIN <- BETA.MAIN * U
    PVAL.MAIN <- ifelse(V_i>0, pchisq(STAT.MAIN, df=1, lower.tail=FALSE), NA)

    if(!is.null(null.obj$P)) {
      KPK <- crossprod(K,crossprod(null.obj$P,K))
    } else {
      KSigma_iX <- crossprod(K, null.obj$Sigma_iX)
      KPK <- crossprod(K, crossprod(null.obj$Sigma_i, K)) - tcrossprod(KSigma_iX, tcrossprod(KSigma_iX, null.obj$cov))
    }
    
    KPK <- as.matrix(KPK) * (matrix(1, ncolE, ncolE) %x% diag(ng))
    IV.V_i <- try(solve(KPK), silent = TRUE)
    if(inherits(IV.V_i, "try-error")) IV.V_i <- try(MASS::ginv(KPK), silent = TRUE)
    if (inherits(IV.V_i, "try-error")) {
      write.table("Variant Info: ", debug_file, row.names = F, col.names = F, quote = F, append = T)
      write.table(out[tmp_idx, ], debug_file, row.names = F, col.names = F, quote = F, append = T)
      write.table("KPK: ", debug_file, row.names = F, col.names = F, quote = F, append = T)
      write.table(KPK, debug_file, row.names = F, col.names = F, quote = F, append = T)
      include[tmp_idx] <<- F
      tmp_idx <<- tmp_idx + 1
      return(NULL)
    }
    diag(IV.V_i) <- pmax(0,diag(IV.V_i))
    IV.U <- (rep(1, ncolE) %x% diag(ng)) * as.vector(crossprod(K,residuals))
    BETA.INT <- crossprod(IV.V_i, IV.U)
    
    ng1   <- ng+1
    ngei1 <- ng*ei1
    
    IV.E_i <- try(solve(IV.V_i[ng1:ngei1, ng1:ngei1]), silent = TRUE)
    if(inherits(IV.E_i, "try-error")) IV.E_i <- try(MASS::ginv(IV.V_i[ng1:ngei1, ng1:ngei1]), silent = TRUE)
    if(inherits(IV.E_i, "try-error")) {
      write.table("Variant Info: ", debug_file, row.names = F, col.names = F, quote = F, append = T)
      write.table(out[tmp_idx, ], debug_file, row.names = F, col.names = F, quote = F, append = T)
      write.table("KPK: ", debug_file, row.names = F, col.names = F, quote = F, append = T)
      write.table(KPK, debug_file, row.names = F, col.names = F, quote = F, append = T)
      write.table("IV.E_i: ", debug_file, row.names = F, col.names = F, quote = F, append = T)
      write.table(IV.V_i[ng1:ngei1, ng1:ngei1], debug_file, row.names = F, col.names = F, quote = F, append = T)
      include[tmp_idx] <<- F
      tmp_idx <<- tmp_idx + 1
      return(NULL)
    }
    STAT.INT   <- diag(crossprod(BETA.INT[ng1:ngei1,], crossprod(IV.E_i, BETA.INT[ng1:ngei1,])))
    
    IV.GE_i <- try(solve(IV.V_i[1:ngei1, 1:ngei1]), silent = TRUE)
    if(inherits(IV.GE_i, "try-error")) IV.GE_i <- try(MASS::ginv(IV.V_i[1:ngei1, 1:ngei1]), silent = TRUE)
    if(inherits(IV.GE_i, "try-error")) {
      write.table("Variant Info: ", debug_file, row.names = F, col.names = F, quote = F, append = T)
      write.table(out[tmp_idx, ], debug_file, row.names = F, col.names = F, quote = F, append = T)
      write.table("KPK: ", debug_file, row.names = F, col.names = F, quote = F, append = T)
      write.table(KPK, debug_file, row.names = F, col.names = F, quote = F, append = T)
      write.table("IV.GE_i: ", debug_file, row.names = F, col.names = F, quote = F, append = T)
      write.table(IV.V_i[1:ngei1, 1:ngei1], debug_file, row.names = F, col.names = F, quote = F, append = T)
      include[tmp_idx] <<- F
      tmp_idx <<- tmp_idx + 1
      return(NULL)
    }
    STAT.JOINT <- diag(crossprod(BETA.INT[1:ngei1,], crossprod(IV.GE_i, BETA.INT[1:ngei1,])))
    
    PVAL.INT   <- pchisq(STAT.INT, df=ei, lower.tail=FALSE)
    PVAL.JOINT <- ifelse(is.na(PVAL.MAIN), NA, pchisq(STAT.JOINT, df=1+ei, lower.tail=FALSE))
    
    
    split_mat <- matrix(1:(ncolE*ncolE), ncolE, ncolE)
    if (ng > 1) {
      IV.V_i <- split(IV.V_i, split_mat %x% diag(ng))[-1]
    }else {
      IV.V_i <- split(IV.V_i, split_mat %x% diag(ng))
    }
    tmp_idx <<- tmp_idx + 1
    if (meta.output) {
      return(rbind(N, MAC, RSQ, t(freq_N), BETA.MAIN, SE.MAIN, 
                   diag(as.matrix(BETA.INT[1:ng,])), # Beta G;
                   t(do.call(cbind, lapply(2:ncolE, function(x) {diag(as.matrix(BETA.INT[(((x-1)*ng)+1):(ng*x),]))}))), # Beta GxE and then Beta Covariates
                   t(sqrt(do.call(cbind, lapply(seq(1,ncolE*ncolE, ncolE+1), function(x) {IV.V_i[[x]]})))),
                   t(do.call(cbind, lapply(split_mat[lower.tri(split_mat)], function(x) {IV.V_i[[x]]}))),
                   PVAL.MAIN, STAT.INT, PVAL.INT, PVAL.JOINT))
    } else {
      split_mat <- as.matrix(split_mat[2:(ei+1),2:(ei+1)])
      if (length(split_mat) == 1) {
        return(rbind(N, MAC, RSQ, t(freq_N), BETA.MAIN, SE.MAIN,
                     t(do.call(cbind, lapply(2:(ei+1), function(x) {diag(as.matrix(BETA.INT[(((x-1)*ng)+1):(ng*x),]))}))), # Beta GxE only
                     t(sqrt(do.call(cbind,lapply(diag(split_mat), function(x) {IV.V_i[[x]]})))),   # SE Beta GxE only
                     PVAL.MAIN, STAT.INT, PVAL.INT, PVAL.JOINT))
      } else {
        return(rbind(N, MAC, RSQ, t(freq_N), BETA.MAIN, SE.MAIN,
                     t(do.call(cbind, lapply(2:(ei+1), function(x) {diag(as.matrix(BETA.INT[(((x-1)*ng)+1):(ng*x),]))}))), # Beta GxE only
                     t(sqrt(do.call(cbind,lapply(diag(split_mat), function(x) {IV.V_i[[x]]})))),   # SE Beta GxE only
                     t(do.call(cbind, lapply(split_mat[lower.tri(split_mat)], function(x) {IV.V_i[[x]]}))),
                     PVAL.MAIN, STAT.INT, PVAL.INT, PVAL.JOINT)) 
      }
    }
    
  })
  if (any(include[tmp_idx0:(tmp_idx-1)])) {
    tmp.out <- matrix(unlist(tmp.out), nrow = totalCol, byrow = F, dimnames = list(c("N", "MAC", "RSQ", bin_header, "BETA.MARGINAL", "SE.MARGINAL", meta.header, "PVAL.MARGINAL", "STAT.INT", "PVAL.INT", "PVAL.JOINT"), NULL))
    return(list(tmp_idx, include, tmp.out))
  } else {
    return(list(tmp_idx, include, NULL))
  }
  
}

.calc_rsq <- function(x) {
  m <- mean(x,na.rm=T)/2
  return(var(x,na.rm=T)/(2*m*(1-m)))
}
