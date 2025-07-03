glmm.gei.meta <- function(files, outfile, interaction, SNPID = rep("SNPID", length(files)), CHR = rep("CHR", length(files)), POS = rep("POS", length(files)), Non_Effect_Allele = rep("Non_Effect_Allele", length(files)), Effect_Allele = rep("Effect_Allele", length(files))) {
  k <- length(files)
  if(length(SNPID) != k) stop("Error: \"SNPID\" must have the same length as \"files\"!")
  if(length(CHR) != k) stop("Error: \"CHR\" must have the same length as \"files\"!")
  if(length(POS) != k) stop("Error: \"POS\" must have the same length as \"files\"!")
  if(length(Non_Effect_Allele) != k) stop("Error: \"Non_Effect_Allele\" must have the same length as \"files\"!")
  if(length(Effect_Allele) != k) stop("Error: \"Effect_Allele\" must have the same length as \"files\"!")
  col.include <- c("Beta_Marginal", "SE_Beta_Marginal", "P_Value_Marginal", "Beta_G", paste0("Beta_G-",interaction), "SE_Beta_G", paste0("SE_Beta_G-", interaction), paste0("Cov_Beta_G_G-", interaction), "P_Value_Interaction", "P_Value_Joint")
  master <- fread(files[1], header=T, data.table = FALSE)[, c(SNPID[1], CHR[1], POS[1],Non_Effect_Allele[1], Effect_Allele[1], "N_Samples", "AF", col.include)]
  names(master)[1:5] <- c("SNPID", "CHR", "POS", "Non_Effect_Allele", "Effect_Allele")
  master <- master[apply(!is.na(master[, col.include]), 1, all),]
  master$SNPID <- paste(master$CHR, master$POS, master$Non_Effect_Allele, master$Effect_Allele, sep = ":")
  master$SNPID2 <- paste(master$CHR, master$POS, master$Effect_Allele, master$Non_Effect_Allele, sep = ":")
  master$SCORE <- master$Beta_Marginal/master$SE_Beta_Marginal^2
  master$VAR <- 1/master$SE_Beta_Marginal^2
  master$intSCORE <- master[,paste0("Beta_G-", interaction)]/master[,paste0("SE_Beta_G-", interaction)]^2
  master$intVAR <- 1/master[,paste0("SE_Beta_G-", interaction)]^2
  master$jointSCORE1 <- (master[,paste0("SE_Beta_G-", interaction)]^2*master$Beta_G-master[,paste0("Cov_Beta_G_G-", interaction)]*master[,paste0("Beta_G-", interaction)])/(master$SE_Beta_G^2*master[,paste0("SE_Beta_G-", interaction)]^2-master[,paste0("Cov_Beta_G_G-", interaction)]^2)
  master$jointSCORE2 <- (master$SE_Beta_G^2*master[,paste0("Beta_G-", interaction)]-master[,paste0("Cov_Beta_G_G-", interaction)]*master$Beta_G)/(master$SE_Beta_G^2*master[,paste0("SE_Beta_G-", interaction)]^2-master[,paste0("Cov_Beta_G_G-", interaction)]^2)
  master$jointVAR11 <- master[,paste0("SE_Beta_G-", interaction)]^2/(master$SE_Beta_G^2*master[,paste0("SE_Beta_G-", interaction)]^2-master[,paste0("Cov_Beta_G_G-", interaction)]^2)
  master$jointVAR12 <- -master[,paste0("Cov_Beta_G_G-", interaction)]/(master$SE_Beta_G^2*master[,paste0("SE_Beta_G-", interaction)]^2-master[,paste0("Cov_Beta_G_G-", interaction)]^2)
  master$jointVAR22 <- master$SE_Beta_G^2/(master$SE_Beta_G^2*master[,paste0("SE_Beta_G-", interaction)]^2-master[,paste0("Cov_Beta_G_G-", interaction)]^2)
  flag <- rep(0, nrow(master))
  if(k > 1) {
    for(i in 2:k) {
      tmp <- fread(files[i], header=T, data.table = FALSE)[, c(SNPID[i], CHR[i],POS[i],Non_Effect_Allele[i], Effect_Allele[i], "N_Samples", "AF", col.include)]
      names(tmp)[1:5] <- c("SNPID", "CHR", "POS", "Non_Effect_Allele", "Effect_Allele")
      tmp <- tmp[apply(!is.na(tmp[, col.include]), 1, all),]
      tmp$SNPID <- paste(tmp$CHR, tmp$POS, tmp$Non_Effect_Allele, tmp$Effect_Allele, sep = ":")
      tmp$SNPID2 <- paste(tmp$CHR, tmp$POS, tmp$Effect_Allele, tmp$Non_Effect_Allele, sep = ":")
      tmp$SCORE <- tmp$Beta_Marginal/tmp$SE_Beta_Marginal^2
      tmp$VAR <- 1/tmp$SE_Beta_Marginal^2
      tmp$intSCORE <- tmp[,paste0("Beta_G-", interaction)]/tmp[,paste0("SE_Beta_G-", interaction)]^2
      tmp$intVAR <- 1/tmp[,paste0("SE_Beta_G-", interaction)]^2
      tmp$jointSCORE1 <- (tmp[,paste0("SE_Beta_G-", interaction)]^2*tmp$Beta_G-tmp[,paste0("Cov_Beta_G_G-", interaction)]*tmp[,paste0("Beta_G-", interaction)])/(tmp$SE_Beta_G^2*tmp[,paste0("SE_Beta_G-", interaction)]^2-tmp[,paste0("Cov_Beta_G_G-", interaction)]^2)
      tmp$jointSCORE2 <- (tmp$SE_Beta_G^2*tmp[,paste0("Beta_G-", interaction)]-tmp[,paste0("Cov_Beta_G_G-", interaction)]*tmp$Beta_G)/(tmp$SE_Beta_G^2*tmp[,paste0("SE_Beta_G-", interaction)]^2-tmp[,paste0("Cov_Beta_G_G-", interaction)]^2)
      tmp$jointVAR11 <- tmp[,paste0("SE_Beta_G-", interaction)]^2/(tmp$SE_Beta_G^2*tmp[,paste0("SE_Beta_G-", interaction)]^2-tmp[,paste0("Cov_Beta_G_G-", interaction)]^2)
      tmp$jointVAR12 <- -tmp[,paste0("Cov_Beta_G_G-", interaction)]/(tmp$SE_Beta_G^2*tmp[,paste0("SE_Beta_G-", interaction)]^2-tmp[,paste0("Cov_Beta_G_G-", interaction)]^2)
      tmp$jointVAR22 <- tmp$SE_Beta_G^2/(tmp$SE_Beta_G^2*tmp[,paste0("SE_Beta_G-", interaction)]^2-tmp[,paste0("Cov_Beta_G_G-", interaction)]^2)
      idx <- tmp$SNPID %in% master$SNPID | tmp$SNPID2 %in% master$SNPID
      if(sum(!idx) > 0) {
        flag <- c(flag, rep(0, sum(!idx)))
        master <- rbind(master, tmp[!idx, ])
      }
      idx2 <- match(tmp$SNPID[idx], master$SNPID)
      idx2[is.na(idx2)] <- match(tmp$SNPID2[idx], master$SNPID)[is.na(idx2)]
      noflip <- master$Non_Effect_Allele[idx2] == tmp$Non_Effect_Allele[idx] & master$Effect_Allele[idx2] == tmp$Effect_Allele[idx]
      flip <- master$Non_Effect_Allele[idx2] == tmp$Effect_Allele[idx] & master$Effect_Allele[idx2] == tmp$Non_Effect_Allele[idx]
      flag[idx2] <- flag[idx2] + as.numeric(!noflip & !flip)
      master$AF[idx2][noflip] <- (master$AF[idx2][noflip]*master$N_Samples[idx2][noflip] + tmp$AF[idx][noflip]*tmp$N_Samples[idx][noflip])/(master$N_Samples[idx2][noflip] + tmp$N_Samples[idx][noflip])
      master$AF[idx2][flip] <- (master$AF[idx2][flip]*master$N_Samples[idx2][flip] + (1 - tmp$AF[idx][flip])*tmp$N_Samples[idx][flip])/(master$N_Samples[idx2][flip] + tmp$N_Samples[idx][flip])
      master$N_Samples[idx2] <- master$N_Samples[idx2] + tmp$N_Samples[idx]
      master$SCORE[idx2][noflip] <- master$SCORE[idx2][noflip] + tmp$SCORE[idx][noflip]
      master$SCORE[idx2][flip] <- master$SCORE[idx2][flip] - tmp$SCORE[idx][flip]
      master$intSCORE[idx2][noflip] <- master$intSCORE[idx2][noflip] + tmp$intSCORE[idx][noflip]
      master$intSCORE[idx2][flip] <- master$intSCORE[idx2][flip] - tmp$intSCORE[idx][flip]
      master$jointSCORE1[idx2][noflip] <- master$jointSCORE1[idx2][noflip] + tmp$jointSCORE1[idx][noflip]
      master$jointSCORE1[idx2][flip] <- master$jointSCORE1[idx2][flip] - tmp$jointSCORE1[idx][flip]
      master$jointSCORE2[idx2][noflip] <- master$jointSCORE2[idx2][noflip] + tmp$jointSCORE2[idx][noflip]
      master$jointSCORE2[idx2][flip] <- master$jointSCORE2[idx2][flip] - tmp$jointSCORE2[idx][flip]
      master$VAR[idx2] <- master$VAR[idx2] + tmp$VAR[idx]
      master$intVAR[idx2] <- master$intVAR[idx2] + tmp$intVAR[idx]
      master$jointVAR11[idx2] <- master$jointVAR11[idx2] + tmp$jointVAR11[idx]
      master$jointVAR12[idx2] <- master$jointVAR12[idx2] + tmp$jointVAR12[idx]
      master$jointVAR22[idx2] <- master$jointVAR22[idx2] + tmp$jointVAR22[idx]
    }
    if(any(flag > 0)) {
      message("The following SNPIDs have been removed due to inconsistent alleles across studies:")
      message(paste(master$SNPID[flag > 0], collapse = ", "))
      master <- subset(master, flag == 0)
    }
    master$Beta_Marginal <- master$SCORE/master$VAR
    master$SE_Beta_Marginal <- 1/sqrt(master$VAR)
    master$P_Value_Marginal <- pchisq(master$SCORE^2/master$VAR, 1, lower.tail=F)
    master$P_Value_Interaction <- pchisq(master$intSCORE^2/master$intVAR, 1, lower.tail=F)
    master$SE_Beta_G <- sqrt(master$jointVAR22/(master$jointVAR11*master$jointVAR22-master$jointVAR12^2))
    master[,paste0("SE_Beta_G-", interaction)] <- sqrt(master$jointVAR11/(master$jointVAR11*master$jointVAR22-master$jointVAR12^2))
    master[,paste0("Cov_Beta_G_G-", interaction)] <- -master$jointVAR12/(master$jointVAR11*master$jointVAR22-master$jointVAR12^2)
    master$Beta_G <- (master$jointVAR22*master$jointSCORE1-master$jointVAR12*master$jointSCORE2)/(master$jointVAR11*master$jointVAR22-master$jointVAR12^2)
    master[,paste0("Beta_G-", interaction)] <- (master$jointVAR11*master$jointSCORE2-master$jointVAR12*master$jointSCORE1)/(master$jointVAR11*master$jointVAR22-master$jointVAR12^2)
    master$P_Value_Joint <- pchisq((master$jointVAR22*master$jointSCORE1^2-2*master$jointVAR12*master$jointSCORE1*master$jointSCORE2+master$jointVAR11*master$jointSCORE2^2)/(master$jointVAR11*master$jointVAR22-master$jointVAR12^2), 2, lower.tail=F)
  }
  master <- master[, c("SNPID", "CHR", "POS", "Non_Effect_Allele", "Effect_Allele", "N_Samples", "AF", col.include)]
  write.table(master, outfile, sep="\t", row.names=F, col.names=T, quote=F)
  invisible(master)
}
