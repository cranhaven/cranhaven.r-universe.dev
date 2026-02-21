get_results <-
function (res_pattern = "Permus", level = "snp", from = "workspace", 
    threshold = 0.05,envir = "") 
{
    permus_list <- NULL
    y <- "NULL"
    print("Arguments")
    print(paste("Permutations results pattern:", res_pattern))
    print(paste("Permutations results location:", from))
    print(paste("Permutation level:", level))
    print(paste("Threshold:", threshold))
    if (from != "workspace"){
if(from != "directory") {
stop("Argument \"from\" must be defined: \"workspace\" OR \"directory\"")
}
    }
    if (missing(level)) {
        stop("Argument \"level\" must be defined: \"snp\" OR \"gene\"")
    }
    if (from == "workspace") {
        permus_list <- ls(pattern = res_pattern, envir = envir)
        if (length(permus_list) == 0) {
            stop("Permutations not found in workspace")
        }
    }
    if (from == "directory") {
        x <- list.files(pattern = res_pattern)
        if (length(x) == 0) {
            stop("Permutations not found in working directory")
        }
        for (i in list.files(pattern = res_pattern)) {
            mode <- strsplit(i, split = "[/.]")
            mode <- mode[[1]]
            x <- strsplit(mode, split = "[_]")[[1]]
            x <- x[2]
            x <- paste(res_pattern, x, sep = "")
            mode <- mode[1]
            if (x %in% permus_list) {
                z <- which(permus_list == x)
                permus_list[z]
                assign(permus_list[z], cbind(get(permus_list[z],envir=envir), 
                  read.table(i)),envir=envir)
            }
            else {
                assign(mode, read.table(i),envir=envir)
                permus_list <- c(permus_list, mode)
            }
        }
    }
    temp <- get(permus_list[1],envir=envir)
    if (level == "snp") {
        npaths <- length(permus_list)
        ntraits <- dim(temp)[2]
        npermus <- dim(temp)[1] - 3
        id_thre <- round(npermus * threshold) + 1
        count_vs_obs <- matrix(data = 0, nrow = 0, ncol = 5)
        colnames(count_vs_obs) <- c("PathID", "Trait", "Threshold", 
            "RealCount", "Score")
        for (i in 1:npaths) {
            temp <- get(permus_list[i],envir=envir)
            mat <- matrix(data = 0, nrow = ntraits, ncol = 5)
            mat[, 1] <- rep(strsplit(permus_list[i], "_")[[1]][2], 
                ntraits)
            for (j in 1:ntraits) {
                mat[j, 2] <- colnames(temp)[j]
                x <- sort(temp[2:npermus + 1, j], decreasing = TRUE)
                thre <- x[id_thre]
                mat[j, 3] <- thre
if(thre>0){
mat[j, 4] <- temp[1, j]
mat[j, 5] <- temp[npermus + 3, j]
}
if(thre==0){
mat[j, 4] <- temp[1, j]
mat[j, 5] <- 1
}

            }
            count_vs_obs <- rbind(count_vs_obs, mat)
        }
        count_vs_obs <- as.data.frame(count_vs_obs)
        count_vs_obs[, 3] <- as.numeric(as.character(count_vs_obs[, 
            3]))
        count_vs_obs[, 4] <- as.numeric(as.character(count_vs_obs[, 
            4]))
        count_vs_obs[, 5] <- as.numeric(as.character(count_vs_obs[, 
            5]))
        return(count_vs_obs)
    }
    if (level == "gene") {
        ntraits <- length(permus_list)
        npaths <- dim(temp)[2]
        npermus <- dim(temp)[1]
        id_thre <- round(npermus * threshold) + 1
        reslts <- NULL
        for (i in 1:dim(get(permus_list[1],envir=envir))[2]) {
            reslts <- c(reslts, strsplit(colnames(get(permus_list[1],envir=envir)), 
                "_")[[i]][3])
        }
        trts <- NULL
        for (i in 1:length(permus_list)) {
            trts <- c(trts, strsplit(permus_list, "_")[[i]][2])
        }
        hyper_vs_empirical <- matrix(data = 0, nrow = 0, ncol = 5)
        colnames(hyper_vs_empirical) <- c("PathID", "Trait", 
            "Threshold", "P-Value", "Observed")
        for (i in 1:length(permus_list)) {
            temp <- get(permus_list[i],envir=envir)
            mat <- matrix(data = 0, nrow = npaths, ncol = 5)
            mat[, 1] <- reslts
            for (j in 1:npaths) {
                x <- sort(temp[, j])
                thre <- x[id_thre]
                mat[j, 2] <- trts[i]
                mat[j, 3] <- thre
if(thre>0){
pval <- temp[1, j]
mat[j, 4] <- pval
indx <- which(x == pval)[1]
obs <- indx/npermus
mat[j, 5] <- obs
}
if(thre==0){
pval <- 1
mat[j, 4] <- pval
indx <- which(x == pval)[1]
obs <- indx/npermus
mat[j, 5] <- obs
}
            }
            hyper_vs_empirical <- rbind(hyper_vs_empirical, mat)
        }
        hyper_vs_empirical <- as.data.frame(hyper_vs_empirical, 
            stringsAsFactors = FALSE)
        hyper_vs_empirical[, 3] <- as.numeric(hyper_vs_empirical[, 
            3])
        hyper_vs_empirical[, 4] <- as.numeric(hyper_vs_empirical[, 
            4])
        hyper_vs_empirical[, 5] <- as.numeric(hyper_vs_empirical[, 
            5])
        return(hyper_vs_empirical)
    }
}
