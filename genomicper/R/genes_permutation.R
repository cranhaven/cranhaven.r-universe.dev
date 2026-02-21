genes_permutation <-
function (ordered_alldata = "", pers_ids = "", pathways = "", 
    ntraits = "", nper = 100, threshold = 0.05, seed=10,saveto = "workspace", 
gs_locs = "",envir = "") 
{
    print("Arguments set:")
    print(paste("Ordered dataset: ", substitute(ordered_alldata), 
        sep = ""))
    print(paste("Indexes of Gene Annotations: ", substitute(pers_ids)))
    print(paste("Gene-sets Information and counts: ", substitute(pathways), 
        sep = ""))
    print(paste("Indexes of Traits to Analyse:", as.numeric(ntraits)))
    print(paste("Traits:", colnames(ordered_alldata)[as.numeric(ntraits)]))
    print(paste("Number of permutations: ", nper))
    print(paste("Threshold level: ", threshold))
    print(paste("Permutation Results save to: ", substitute(saveto)))
    if (saveto != "workspace"){
if(saveto != "directory") {
stop("Define where are the results to be saved: \"saveto\"=\"workspace\" OR \"directory\"")
}
    }
    ntraits <- as.numeric(ntraits)
    nper <- as.numeric(nper)
    threshold <- as.numeric(threshold)
set.seed(as.numeric(seed), kind = "Mersenne-Twister")
    temp <- ordered_alldata[, c(1:6, ntraits)]
    ns <- which(pers_ids != "NULL")
    if (length(ns) == 0) {
        stop("No genes mapped to the gene-sets")
    }
    pers_ids <- pers_ids[ns]
    paths_list <- names(pers_ids)
    mx_rs <- dim(ordered_alldata)[1]
    sd <- round(runif(nper, 1, mx_rs))
    rowsf <- dim(gs_locs)[1]
    tname <- NULL
    lab <- NULL
    i <- NULL
    ids <- NULL
    j <- NULL
    k <- NULL
    all_ts <- NULL
    listf <- as.numeric(as.character(gs_locs[, 4]))
    pathways2 <- matrix(data = 0, nrow = length(paths_list), 
        ncol = 4 + length(ntraits))
    pathways2[, 1:4] <- pathways[ns, 1:4]
    pathways <- pathways2
    for (i in 7:length(temp)) {
        temp2 <- temp[, i]
        hyper_mat <- matrix(data = NA, nrow = length(sd) + 1, 
            ncol = length(paths_list))
        trtsnm <- colnames(temp)[i]
        print(trtsnm)
        all_ts <- c(all_ts, trtsnm)
        sig_genes_real <- 0
        for (j in 1:length(sd)) {
            sig_genes <- 0
            path_counts <- matrix(data = 0, nrow = length(paths_list), 
                ncol = 2)
            for (m in 1:length(paths_list)) {
                path_counts[m, 1] <- paths_list[m]
            }
            for (k in 1:length(listf)) {
                obs <- as.numeric(gs_locs[k, 6])
                strt <- as.numeric(gs_locs[k, 5])
                ed <- strt + obs - 1
                if (j == 1) {
                  tscore <- pchisq(-2 * (sum(log(temp[c(strt:ed), 
                    i]))), df = 2 * (obs), lower.tail = F)
                  if (tscore <= threshold) {
                    sig_genes_real <- sig_genes_real + 1
                    for (m in 1:length(paths_list)) {
                      if (k %in% pers_ids[[m]]) {
                        pathways[m, i - 2] <- as.numeric(pathways[m, 
                          i - 2]) + 1
                      }
                    }
                  }
                }
                obs <- as.numeric(gs_locs[k, 6])
                fkstrt <- as.numeric(gs_locs[k, 5]) + sd[j]
                fkend <- fkstrt + obs - 1
                if (fkend <= mx_rs) {
                  tscore <- pchisq(-2 * (sum(log(temp2[c(fkstrt:fkend)]))), 
                    df = 2 * (obs), lower.tail = F)
                  if (tscore <= threshold) {
                    sig_genes <- sig_genes + 1
                    for (m in 1:length(paths_list)) {
                      if (k %in% pers_ids[[m]]) {
                        path_counts[m, 2] <- as.numeric(path_counts[m, 
                          2]) + 1
                      }
                    }
                    next
                  }
                }
                if (fkend > mx_rs) {
                  if (fkstrt >= mx_rs) {
                    pls <- fkstrt - mx_rs
                    nend <- pls + obs - 1
                    if (nend > mx_rs) {
                      rst <- nend - mx_rs
                      ps <- c(pls:mx_rs, 1:rst)
                      tscore <- pchisq(-2 * (sum(log(temp2[ps]))), 
                        df = 2 * (obs), lower.tail = F)
                      if (tscore <= threshold) {
                        sig_genes <- sig_genes + 1
                        for (m in 1:length(paths_list)) {
                          if (k %in% pers_ids[[m]]) {
                            path_counts[m, 2] <- as.numeric(path_counts[m, 
                              2]) + 1
                          }
                        }
                        next
                      }
                    }
                    else {
                      ps <- c(pls:nend)
                      tscore <- pchisq(-2 * (sum(log(temp2[ps]))), 
                        df = 2 * (obs), lower.tail = F)
                      if (tscore <= threshold) {
                        sig_genes <- sig_genes + 1
                        for (m in 1:length(paths_list)) {
                          if (k %in% pers_ids[[m]]) {
                            path_counts[m, 2] <- as.numeric(path_counts[m, 
                              2]) + 1
                          }
                        }
                        next
                      }
                    }
                  }
                  else {
                    rst <- fkend - mx_rs
                    ps <- c(fkstrt:mx_rs, 1:rst)
                    tscore <- pchisq(-2 * (sum(log(temp2[ps]))), 
                      df = 2 * (obs), lower.tail = F)
                    if (tscore <= threshold) {
                      sig_genes <- sig_genes + 1
                      for (m in 1:length(paths_list)) {
                        if (k %in% pers_ids[[m]]) {
                          path_counts[m, 2] <- as.numeric(path_counts[m, 
                            2]) + 1
                        }
                      }
                    }
                  }
                }
            }
            if (j == 1) {
                for (m in 1:length(paths_list)) {
                  hypr_ps <- hyprbg(Sig_in_Paths = as.numeric(pathways[m, 
                    i - 2]), uniSig = sig_genes_real, gns_in_Paths = as.numeric(pathways[m, 
                    3]), universe = rowsf)
                  hyper_mat[j, m] <- hypr_ps
                  hypr_ps <- hyprbg(Sig_in_Paths = as.numeric(path_counts[m, 
                    2]), uniSig = sig_genes, gns_in_Paths = as.numeric(pathways[m, 
                    3]), universe = rowsf)
                  hyper_mat[j + 1, m] <- hypr_ps
                }
            }
            else {
                for (m in 1:length(paths_list)) {
                  hypr_ps <- hyprbg(Sig_in_Paths = as.numeric(path_counts[m, 
                    2]), uniSig = sig_genes, gns_in_Paths = as.numeric(pathways[m, 
                    3]), universe = rowsf)
                  hyper_mat[j + 1, m] <- hypr_ps
                }
            }
        }
        rownames(hyper_mat) <- c("REAL_Pval", 1:length(sd))
        colnames(hyper_mat) <- paths_list
        if (saveto == "directory") {
            write.table(hyper_mat, file = paste("Permus_", trtsnm, 
                ".txt", sep = ""), sep = "\t", row.names = T, 
                col.names = T, quote = F)
        }
        if (saveto == "workspace") {
            assign(paste("Permus_", trtsnm, sep = ""), hyper_mat, 
                envir = envir)
        }
    }
    colnames(pathways) <- c("ID", "GenesInPath", "GenesFound", 
        "SNPsInPath", all_ts)
}
