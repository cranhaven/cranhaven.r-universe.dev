## usethis namespace: start
#' @useDynLib VARDetect, .registration = TRUE
## usethis namespace: end
NULL
#' Generate VAR(p) model data with break points
#'
#' @description This function is used for generate simulated time series
#' @param method the structure of time series: "sparse","group sparse", "fLS", "LS"
#' @param nob sample size
#' @param k dimension of transition matrix
#' @param lags lags of VAR time series. Default is 1.
#' @param lags_vector a vector of lags of VAR time series for each segment
#' @param brk a vector of break points with (nob+1) as the last element
#' @param sparse_mats transition matrix for sparse case
#' @param group_mats transition matrix for group sparse case
#' @param group_index group index for group lasso.
#' @param group_type type for group lasso: "columnwise", "rowwise". Default is "columnwise".
#' @param sp_pattern a choice of the pattern of sparse component: diagonal, 1-off diagonal, random, custom
#' @param sp_density if we choose random pattern, we should provide the sparsity density for each segment
#' @param rank if we choose method is low rank plus sparse, we need to provide the ranks for each segment
#' @param info_ratio the information ratio leverages the signal strength from low rank and sparse components
#' @param signals manually setting signal for each segment (including sign)
#' @param singular_vals singular values for the low rank components
#' @param spectral_radius to ensure the time series is piecewise stationary.
#' @param sigma the variance matrix for error term
#' @param skip an argument to control the leading data points to obtain a stationary time series
#' @param seed an argument to control the random seed. Default seed is 1.
#' @return A list object, which contains the followings
#' \describe{
#'   \item{series}{matrix of timeseries data}
#'   \item{noises}{matrix of noise term data}
#'   \item{sparse_mats}{list of sparse matrix in the transition matrix}
#'   \item{lowrank_mats}{list of low-rank matrix in the transition matrix}
#' }
#' @import mvtnorm
#' @importFrom igraph erdos.renyi.game
#' @importFrom igraph get.adjacency
#' @import pracma
#' @export
#' @examples
#' nob <- (10^3 * 4) # number of time points
#' p <- 15 # number of time series components
#' brk <- c(floor(nob / 3), floor(2 * nob / 3), nob + 1)
#' m0 <- length(brk) - 1 # number of break points
#' q.t <- 2 # the true AR order
#' m <- m0 + 1 # number of segments
#' sp_density <- rep(0.05, m * q.t) # sparsity level (5%)
#' try <- simu_var("sparse", nob = nob, k = p, lags = q.t, brk = brk,
#'                 sp_pattern = "random", sp_density = sp_density)
#' print(plot_matrix(do.call("cbind", try$model_param), m * q.t))
#'
simu_var <- function(method = c("sparse", "group sparse", "fLS", "LS"), nob = 300, k = 20,
                     lags = 1, lags_vector = NULL, brk, sigma = NULL, skip = 50,
                     spectral_radius = 0.98, seed = NULL, sp_density = NULL,
                     group_mats = NULL, group_index = NULL, group_type = c("columnwise", "rowwise"),
                     sparse_mats = NULL, sp_pattern = c("off-diagonal", "diagonal", "random"),
                     rank = NULL, info_ratio = NULL, signals = NULL, singular_vals = NULL ){
    method <- match.arg(method)
    group_type <- match.arg(group_type)
    sp_pattern <- match.arg(sp_pattern)
    if(!is.null(seed)){
        set.seed(seed)
    }
    if(length(lags) == 1){
        arlags=seq(1, lags, 1)
    }

    ### Error conditions
    m <- length(brk)
    nar <- length(arlags)
    if(is.null(sp_density)){
        if(!is.null(lags_vector)){
            sp_density <- rep(0.05, sum(lags_vector))
        }else{
            sp_density <- rep(0.05, m * nar)
        }
    }

    if(!is.null(lags_vector)){
        if(length(lags_vector) != m){
            stop("the length of lags_vecotr should be m.")
        }
        arlags_vector <- vector('list', m)
        for(i in 1:m){
            arlags_vector[[i]] <- seq(1, lags_vector[i], 1)
        }
    }

    if (!(method %in% c("sparse", "group sparse", "fLS", "LS"))){
        stop("the method should be sparse or lowrank plus sparse or grouped.")
    }
    if (spectral_radius > 1){
        stop("the spectral radius should be less than 1!")
    }
    if(is.null(sigma)){
        sigma <- as.matrix(1*diag(k))
    }
    if(is.null(signals) & is.null(lags_vector)){
        signals <- rep(0.8, m*nar)
        for(j in 1:m){
            for(i in 1:nar){
                signals[(j-1) * nar + i] <- (-1)^(j) * signals[(j-1) * nar + i]
            }
        }
    }
    if(is.null(signals) & !is.null(lags_vector)){
        signals <- rep(0.8, sum(lags_vector))
        nar <- lags_vector[1]
        for(i in 1:nar){
            signals[i] <- (-1) * signals[i]
        }
        if(m > 1){
            for(j in 2:m){
                nar <- lags_vector[j]
                for(i in 1:nar){
                    signals[sum(lags_vector[1:(j-1)]) + i] <- (-1)^(j) * signals[sum(lags_vector[1:(j-1)]) + i]
                }
            }
        }
    }
    if (!is.matrix(sigma)){
        sigma <- as.matrix(sigma)
    }


    ###### Pure sparse structure for model parameter ######
    phi_mats <- vector('list', m)
    if(method == 'sparse'){
        if(!(sp_pattern %in% c("diagonal", "off-diagonal", "random", "custom"))){
            stop("the sparsity pattern should be determined correctly!")
        }
        if(is.null(sparse_mats)){
            sparse_mats <- vector('list', m)
        }else{
            sp_pattern = "custom"
        }

        if(is.null(lags_vector)){
            if(sp_pattern == "diagonal"){
                for (j in 1:m){
                    sparse_mats[[j]] <- matrix(0, k, k * nar)
                    for(i in 1:nar){
                        sparse_mats[[j]][, ((i-1) * k + 1):(i*k)] <- signals[(j-1) * nar + i] * diag(k)
                    }
                }
            }
            if(sp_pattern == 'off-diagonal'){
                for(j in 1:m){
                    sparse_mats[[j]] <- matrix(0, k, k * nar)
                    for(i in 1:nar){
                        for(r in 1:(k - 1)){
                            sparse_mats[[j]][r, (i-1) * k + r + 1] <- signals[(j-1) * nar + i]
                        }
                    }
                }
            }
            if(sp_pattern == 'random'){
                if(length(sp_density) != nar * m){
                    stop("the length of sparsity density doesn't match! should equal number of segment*number of lags.")
                }
                for(j in 1:m){
                    sparse_mats[[j]] <- matrix(0, k, k * nar)
                    for(i in 1:nar){
                        g <- erdos.renyi.game(k, round(sp_density[(j-1) * nar + i] * k * k), type = "gnm", directed = TRUE)
                        adj_mat <- as.matrix(get.adjacency(g))
                        sparse_mats[[j]][, ((i-1) * k + 1):(i * k)]  <- signals[(j-1) * nar + i] * adj_mat
                    }
                }
            }
            if(sp_pattern == 'custom'){
                if(length(sparse_mats) != m){
                    stop("the length of transition matrix doesn't match!")
                }
                if((nrow(sparse_mats[[1]]) != k) | (ncol(sparse_mats[[1]]) != k * nar)){
                    stop("the length of transition matrix doesn't match!")
                }
            }

            # examine if each segment is stationary, we force the spectral radius to be 0.9.
            max_eigen <- rep(0, m)
            phi <- NULL
            for(j in 1:m){
                if(nar == 1){
                    max_eigen[j] <- max(abs(eigen(sparse_mats[[j]])$values))
                }else{
                    temp_mat <- matrix(0, k*nar, k*nar)
                    temp_mat[1:k,] <- sparse_mats[[j]]
                    for(i in 1:(nar - 1)){
                        temp_mat[(i * k + 1):((i+1) * k), ((i-1) * k + 1):(i * k)] <- diag(k)
                    }
                    max_eigen[j] <- max(abs(eigen(temp_mat)$values))
                }

                # if the current segment is not stable, we re-scale the spectral radius
                if(max_eigen[j] >= 1){
                    phi_mats[[j]] <- sparse_mats[[j]] * spectral_radius / max_eigen[j]
                    sparse_mats[[j]] <- phi_mats[[j]]
                }else{
                    phi_mats[[j]] <- sparse_mats[[j]]
                }
                flag = TRUE
                while(flag){
                    if(nar == 1){
                        max_eigen[j] <- max(abs(eigen(sparse_mats[[j]])$values))
                    }else{
                        temp_mat <- matrix(0, k * nar, k * nar)
                        temp_mat[1:k, ] <- sparse_mats[[j]]
                        for(i in 1:(nar-1)){
                            temp_mat[(i * k + 1):((i+1) * k), ((i-1) * k + 1):(i*k)] <- diag(k)
                        }
                        max_eigen[j] <- max(abs(eigen(temp_mat)$values))
                    }

                    # if the current segment is not stable, we re-scale the spectral radius
                    if(max_eigen[j] >= 1){
                        phi_mats[[j]] <- sparse_mats[[j]] * spectral_radius / max_eigen[j]
                        sparse_mats[[j]] <- phi_mats[[j]]
                    }else{
                        phi_mats[[j]] <- sparse_mats[[j]]
                        flag = FALSE
                    }
                }
                phi <- cbind(phi, phi_mats[[j]])
            }
        }else{
            if(sp_pattern == "diagonal"){
                nar_max <- max(lags_vector)
                arlags <- arlags_vector[[1]]
                nar <- length(arlags)
                sparse_mats[[1]] <- matrix(0, k, k * nar_max)
                for(i in 1:nar){
                    sparse_mats[[1]][, ((i-1) * k + 1):(i * k)] <- signals[i] * diag(k)
                }
                if(m > 1){
                    for(j in 2:m){
                        arlags <- arlags_vector[[j]]
                        nar <- length(arlags)
                        sparse_mats[[j]] <-  matrix(0, k, k * nar_max)
                        for(i in 1:nar){
                            sparse_mats[[j]][, ((i-1)*k + 1):(i*k)] <- signals[sum(lags_vector[1:(j-1)]) + i] * diag(k)
                        }
                    }
                }
                nar <- nar_max
                arlags <- seq(1, nar, 1)
            }
            if(sp_pattern == 'off-diagonal'){
                nar_max <- max(lags_vector)
                arlags <- arlags_vector[[1]]
                nar <- length(arlags)
                sparse_mats[[1]] <- matrix(0, k, k*nar_max)
                for(i in 1:nar){
                    for (r in 1:(k-1)){
                        sparse_mats[[1]][r, (i-1) * k + r + 1] <- signals[i]
                    }
                }
                if(m > 1){
                    for(j in 2:m){
                        arlags <- arlags_vector[[j]]
                        nar <- length(arlags)
                        sparse_mats[[j]] <-  matrix(0, k, k * nar_max)
                        for(i in 1:nar){
                            for(r in 1:(k-1)){
                                sparse_mats[[j]][r, (i-1) * k + r + 1] <- signals[sum(lags_vector[1:(j-1)]) + i]
                            }
                        }
                    }
                }
                nar <- nar_max
                arlags <- seq(1, nar, 1)
            }
            if(sp_pattern == 'random'){
                if(length(sp_density) != sum(lags_vector)){
                    stop("the length of sparsity density doesn't match! should equal number of segment*number of lags.")
                }
                nar_max <- max(lags_vector)
                arlags <- arlags_vector[[1]]
                nar <- length(arlags)
                sparse_mats[[1]] <- matrix(0, k, k * nar_max)
                for(i in 1:nar){
                    g <- erdos.renyi.game(k, round(sp_density[i] * k * k), type = "gnm", directed = TRUE)
                    adj_mat <- as.matrix(get.adjacency(g))
                    sparse_mats[[1]][, ((i-1) * k + 1):(i * k)] <- signals[i] * adj_mat
                }
                if(m > 1){
                    for(j in 2:m){
                        sparse_mats[[j]] <- matrix(0, k, k * nar_max)
                        arlags <- arlags_vector[[j]]
                        nar <- length(arlags)
                        for(i in 1:nar){
                            g <- erdos.renyi.game(k, round(sp_density[sum(lags_vector[1:(j-1)]) + i] * k * k), type = "gnm", directed = TRUE)
                            adj_mat <- as.matrix(get.adjacency(g))
                            sparse_mats[[j]][, ((i-1) * k + 1):(i * k)] <- signals[sum(lags_vector[1:(j-1)]) + i] * adj_mat
                        }
                    }
                }
            }
            if(sp_pattern == 'custom'){
                if(length(sparse_mats) != m){
                    stop("the length of transition matrix doesn't match!")
                }
                if((nrow(sparse_mats[[1]]) != k) | (ncol(sparse_mats[[1]]) != k * nar)){
                    stop("the length of transition matrix doesn't match!")
                }
            }

            # examine if each segment is stationary, we force the spectral radius to be 0.9.
            max_eigen <- rep(0, m)
            phi <- NULL
            for(j in 1:m){
                if(nar == 1){
                    max_eigen[j] <- max(abs(eigen(sparse_mats[[j]])$values))
                }else{
                    temp_mat <- matrix(0, k * nar, k * nar)
                    temp_mat[1:k,] <- sparse_mats[[j]]
                    for(i in 1:(nar - 1)){
                        temp_mat[(i * k + 1):((i+1) * k), ((i-1) * k + 1):(i * k)] <- diag(k)
                    }
                    max_eigen[j] <- max(abs(eigen(temp_mat)$values))
                }

                # if the current segment is not stable, we re-scale the spectral radius
                if (max_eigen[j] >= 1){
                    phi_mats[[j]] <- sparse_mats[[j]] * spectral_radius / max_eigen[j]
                    sparse_mats[[j]] <- phi_mats[[j]]
                }else{
                    phi_mats[[j]] <- sparse_mats[[j]]
                }

                flag = TRUE
                while(flag){
                    if(nar == 1){
                        max_eigen[j] <- max(abs(eigen(sparse_mats[[j]])$values))
                    }else{
                        temp_mat <- matrix(0, k * nar, k * nar)
                        temp_mat[1:k,] <- sparse_mats[[j]]
                        for(i in 1:(nar - 1)){
                            temp_mat[(i * k + 1):((i+1) * k), ((i-1) * k + 1):(i * k)] <- diag(k)
                        }
                        max_eigen[j] <- max(abs(eigen(temp_mat)$values))
                    }

                    # if the current segment is not stable, we rescale the spectral radius
                    if(max_eigen[j] >= 1){
                        phi_mats[[j]] <- sparse_mats[[j]] * spectral_radius / max_eigen[j]
                        sparse_mats[[j]] <- phi_mats[[j]]
                    }else{
                        phi_mats[[j]] <- sparse_mats[[j]]
                        flag = FALSE
                    }
                }
                phi <- cbind(phi, phi_mats[[j]])
            }
        }
    }

    ###### Group sparse structure for model parameter ######
    if(method == "group sparse"){
        if(!(group_type %in% c("columnwise", "rowwise"))){
            stop("the group type should be determined correctly!")
        }
        if(is.null(group_mats)){
            group_mats <- vector('list', m)
            if(is.null(group_index)){
                if(group_type == "columnwise"){
                    non_zero <- max(ceiling(sp_density * k * nar))
                    num_group <- 2
                    group_index <- vector('list', num_group)
                    group_index[[1]] <- sample(1:(k * nar), non_zero)
                    group_index[[2:num_group]] <- setdiff(1:(k * nar), group_index[[1]])
                }
                if(group_type == "rowwise"){
                    non_zero <- max(ceiling(sp_density * k))
                    num_group <- 2
                    group_index <- vector('list', num_group)
                    group_index[[1]] <- sample(1:k, non_zero)
                    group_index[[2:num_group]] <- setdiff(1:k, group_index[[1]])
                }
            }

            if(group_type == "columnwise"){
                for(j in 1:m){
                    group_mats[[j]] <- zeros(k, k * nar)
                    for(i in 1:(num_group-1)){
                        for(g in group_index[[i]]){
                            group_mats[[j]][, g] <- signals[(j-1) * nar + floor((g - 1) / k) + 1]
                        }
                    }
                }
            }

            if(group_type == "rowwise"){
                for(j in 1:m){
                    group_mats[[j]] <- zeros(k, k * nar)
                    for(i in 1:(num_group-1)){
                        for(g in group_index[[i]]){
                            group_mats[[j]][g %% k, ((i-1) * k + 1):(i * k)] <- signals[(j-1) * nar + floor((g-1) / k) + 1]
                            if(g %% k ==0){
                                group_mats[[j]][k, ((i-1) * k + 1):(i * k)] <- signals[(j-1) * nar + floor((g-1) / k) + 1]
                            }
                        }
                    }
                }
            }
        }

        # examine if each segment is stationary, we force the spectral radius to be 0.9.
        max_eigen <- rep(0, m)
        phi <- NULL
        for(j in 1:m){
            if(nar == 1){
                max_eigen[j] <- max(abs(eigen(group_mats[[j]])$values))
            }else{
                temp_mat <- matrix(0, k * nar, k * nar)
                temp_mat[1:k,] <- group_mats[[j]]
                for(i in 1:(nar - 1)){
                    temp_mat[(i * k + 1):((i+1) * k), ((i-1) * k + 1):(i * k)] <- diag(k)
                }
                max_eigen[j] <- max(abs(eigen(temp_mat)$values))
            }

            # if the current segment is not stable, we rescale the spectral radius
            if (max_eigen[j] >= 1){
                phi_mats[[j]] <- group_mats[[j]] * spectral_radius / max_eigen[j]
                group_mats[[j]] <- phi_mats[[j]]
            }else{
                phi_mats[[j]] <- group_mats[[j]]
            }

            flag = TRUE
            while(flag){
                if(nar == 1){
                    max_eigen[j] <- max(abs(eigen(group_mats[[j]])$values))
                }else{
                    temp_mat <- matrix(0, k * nar, k * nar)
                    temp_mat[1:k,] <- group_mats[[j]]
                    for(i in 1:(nar-1)){
                        temp_mat[(i * k + 1):((i+1) * k), ((i-1) * k + 1):(i * k)] <- diag(k)
                    }
                    max_eigen[j] <- max(abs(eigen(temp_mat)$values))
                }

                # if the current segment is not stable, we rescale the spectral radius
                if(max_eigen[j] >= 1){
                    phi_mats[[j]] <- group_mats[[j]] * spectral_radius / max_eigen[j]
                    group_mats[[j]] <- phi_mats[[j]]
                }else{
                    phi_mats[[j]] <- group_mats[[j]]
                    flag = FALSE
                }
            }
            phi <- cbind(phi, phi_mats[[j]])
        }
    }

    ###### Fixed low rank plus sparse structure model parameter ######
    if(method == "fLS") {
        if(length(rank[!duplicated(rank)]) > 1){
            stop("the ranks should be all the same!")
        }
        if(length(info_ratio[!duplicated(info_ratio)]) > 1){
            stop("the information ratio must be all the same!")
        }
        if(lags > 1){
            stop("the lags should be 1 for low rank plus sparse structure!")
        }

        # generate sparse components
        if(!(sp_pattern %in% c("diagonal", "off-diagonal", "random"))){
            stop("the sparsity pattern should be determined correctly!")
        }

        if(is.null(sparse_mats)){
            sparse_mats <- vector('list', m)
        }else{
            sp_pattern = "custom"
        }
        if(sp_pattern == "diagonal"){
            for (j in 1:m){
                sparse_mats[[j]] <-  matrix(0, k, k*nar)
                for(i in 1:nar){
                    sparse_mats[[j]][, ((i-1)*k+1): (i*k)  ] <- signals[(j-1)*nar+i] * diag(k)
                }
            }
        }

        if(sp_pattern == 'off-diagonal'){
            for (j in 1:m){
                sparse_mats[[j]] <- matrix(0, k, k * nar)
                for(i in 1:nar){
                    for(r in 1:(k-1)){
                        sparse_mats[[j]][r, (i-1) * k + r + 1] <- signals[(j-1) * nar + i]
                    }
                }
            }
        }

        if(sp_pattern == 'random'){
            if(length(sp_density) != nar*m){
                stop("the length of sparsity density doesn't match! should equal number of segment*number of lags.")
            }
            for(j in 1:m){
                sparse_mats[[j]] <- matrix(0, k, k * nar)
                for(i in 1:nar){
                    g <- erdos.renyi.game(k, round(sp_density[(j-1)*nar+i]*k*k), type="gnm",directed = TRUE)
                    adj_mat <- as.matrix(get.adjacency(g))
                    sparse_mats[[j]][, ((i-1)*k+1): (i*k)  ]  <- signals[(j-1)*nar+i]  * adj_mat
                }
            }
        }

        # generate low rank component
        lowrank_mats <- vector('list', m)
        if(length(singular_vals) != max(rank)) {
            stop("The number of singular values should be the same as the given rank!")
        }
        L_basis <- randortho(k)
        for(j in 1:m){
            lowrank_mats[[j]] <- matrix(0, k, k)
            for(rk in 1:(rank[j])){
                lowrank_mats[[j]] <- lowrank_mats[[j]] + singular_vals[rk] * (L_basis[,rk] %*% t(L_basis[,rk]))
            }
            lowrank_mats[[j]] <- (abs(signals[j]) * info_ratio[j] / max(lowrank_mats[[j]])) * lowrank_mats[[j]]
        }

        # combine sparse and low rank components together
        for(j in 1:m){
            phi_mats[[j]] <- sparse_mats[[j]] + lowrank_mats[[j]]
        }

        # examine if each segment is stationary, we force the spectral radius to be pre-determined input.
        max_eigen <- rep(0, m)
        phi <- NULL
        for(j in 1:m){
            max_eigen[j] <- max(abs(eigen(phi_mats[[j]])$values))
            if(max_eigen[j] >= 1){
                phi_mats[[j]] <- phi_mats[[j]] * spectral_radius / max_eigen[j]
                sparse_mats[[j]] <- sparse_mats[[j]] * spectral_radius / max_eigen[j]
                lowrank_mats[[j]] <- lowrank_mats[[j]] * spectral_radius / max_eigen[j]
            }
            phi <- cbind(phi, phi_mats[[j]])
        }
    }


    ###### Low rank plus sparse structure model parameter ######
    if(method == "LS"){
        if(length(rank) != m){
            stop("the length of ranks doesn't match!")
        }

        if(lags > 1){
            stop("the lags should be 1 for Low rank plus sparse structure!")
        }

        # generate sparse components
        if(!(sp_pattern %in% c("diagonal", "off-diagonal", "random"))){
            stop("the sparsity pattern should be determined correctly!")
        }

        if(is.null(sparse_mats)){
            sparse_mats <- vector('list', m)
        }else{
            sp_pattern = "custom"
        }

        if(sp_pattern == "diagonal"){
            for(j in 1:m){
                sparse_mats[[j]] <-  matrix(0, k, k*nar)
                for(i in 1:nar){
                    sparse_mats[[j]][, ((i-1)*k+1): (i*k)  ] <- signals[(j-1)*nar+i] * diag(k)
                }
            }
        }

        if(sp_pattern == 'off-diagonal'){
            for(j in 1:m){
                sparse_mats[[j]] <- matrix(0, k, k*nar)
                for(i in 1:nar){
                    for(r in 1:(k-1)){
                        sparse_mats[[j]][r, (i-1)*k+r+1] <- signals[(j-1)*nar+i]
                    }
                }
            }
        }

        if(sp_pattern == 'random'){
            if(length(sp_density) != nar * m){
                stop("the length of sparsity density doesn't match! should equal number of segment*number of lags.")
            }
            for(j in 1:m){
                sparse_mats[[j]] <- matrix(0, k, k*nar)
                for(i in 1:nar){
                    g <- erdos.renyi.game(k, round(sp_density[(j-1)*nar+i]*k*k), type="gnm",directed = TRUE)
                    adj_mat <- as.matrix(get.adjacency(g))
                    sparse_mats[[j]][, ((i-1)*k+1): (i*k)  ]  <- signals[(j-1)*nar+i]  * adj_mat
                }
            }
        }

        # generate low rank components
        lowrank_mats <- vector('list', m)
        if(length(singular_vals) != max(rank)) {
            stop("The number of singular values should be the same as the maximum rank across all segments!")
        }
        for(j in 1:m){
            L_basis <- randortho(k)
            lowrank_mats[[j]] <- matrix(0, k, k)
            for(rk in 1:(rank[j])){
                lowrank_mats[[j]] <- lowrank_mats[[j]] + singular_vals[rk] * (L_basis[,rk] %*% t(L_basis[,rk]))
            }
            lowrank_mats[[j]] <- (signals[j] * info_ratio[j] / max(lowrank_mats[[j]])) * lowrank_mats[[j]]
        }

        # combine sparse and low rank components together
        for(j in 1:m){
            phi_mats[[j]] <- sparse_mats[[j]] + lowrank_mats[[j]]
        }

        # examine if each segment is stationary, we force the spectral radius to be 0.9.
        max_eigen <- rep(0, m)
        phi <- NULL
        for(j in 1:m){
            max_eigen[j] <- max(abs(eigen(phi_mats[[j]])$values))
            if(max_eigen[j] >= 1){
                phi_mats[[j]] <- phi_mats[[j]] * spectral_radius / max_eigen[j]
                sparse_mats[[j]] <- sparse_mats[[j]] * spectral_radius / max_eigen[j]
                lowrank_mats[[j]] <- lowrank_mats[[j]] * spectral_radius / max_eigen[j]
            }
            phi <- cbind(phi, phi_mats[[j]])
        }
    }

    ### generate time series with respect to the transition matrices
    n <- nob + skip
    at <- rmvnorm(n, rep(0, k), sigma)
    nar <- length(arlags)
    p <- 0
    if(nar > 0){
        arlags <- sort(arlags)
        p <- arlags[nar]
    }
    ist <- p+1
    zt <- matrix(0, n, k)

    # case 1: there is only one single change point
    if(m == 1){
        for(it in ist:n){
            tmp <- matrix(at[it,], 1, k)
            if(nar > 0){
                for(i in 1:nar){
                    idx <- (i-1) * k
                    phj <- phi[,(idx+1):(idx+k)]
                    ztm <- matrix(zt[it - arlags[i], ], 1, k)
                    tmp <- tmp + ztm %*% t(phj)
                }
            }
            zt[it, ] = tmp
        }
    }

    # case 2: there are multiple change points
    if(m > 1){
        for(it in ist:(skip + brk[1] - 1)){
            tmp <- matrix(at[it,], 1, k)
            if(nar > 0){
                for(i in 1:nar){
                    idx <- (i-1) * k
                    phj <- phi[, (idx+1):(idx+k)]
                    ztm <- matrix(zt[it - arlags[i], ], 1, k)
                    tmp <- tmp + ztm %*% t(phj)
                }
            }
            zt[it, ] <- tmp
        }
        for(mm in 1:(m-1)){
            for(it in (skip + brk[mm]):(skip + brk[mm + 1] - 1)){
                tmp <- matrix(at[it, ], 1, k)
                if(nar > 0){
                    for(i in 1:nar){
                        idx <- (i-1) * k
                        phj <- phi[, (mm * p * k + idx + 1):(mm * p * k + idx + k)]
                        ztm <- matrix(zt[it - arlags[i], ], 1, k)
                        tmp <- tmp + ztm %*% t(phj)
                    }
                }
                zt[it, ] <- tmp
            }
        }
    }

    # return the list
    zt <- zt[(1 + skip):n, ]
    at <- at[(1 + skip):n, ]
    if(method == "sparse"){
        simulated_var <- list(series = zt, noises = at, model_param = sparse_mats)
    }
    if(method == "group sparse"){
        simulated_var <- list(series = zt, noises = at, model_param = group_mats)
    }
    if(method == "fLS"){
        simulated_var <- list(series = zt, noises = at, model_param = phi_mats,
                              sparse_param = sparse_mats, lowrank_param = lowrank_mats)
    }
    if(method == "LS"){
        simulated_var <- list(series = zt, noises = at, model_param = phi_mats,
                              sparse_param = sparse_mats, lowrank_param = lowrank_mats)
    }
    return(simulated_var)
}



#' Block segmentation scheme (BSS).
#'
#' @description Perform the block segmentation scheme (BSS) algorithm to detect the structural breaks
#' in large scale high-dimensional non-stationary VAR models.
#'
#' @param data input data matrix, with each column representing the time series component
#' @param method method: sparse, group sparse, and fixed low rank plus sparse. Default is sparse
#' @param group.case group sparse pattern: column, row.
#' @param group.index group index for group sparse case
#' @param lambda.1.cv tuning parameter lambda_1 for fused lasso
#' @param lambda.2.cv tuning parameter lambda_2 for fused lasso
#' @param mu tuning parameter for low rank component, only available when method is set to "fLS"
#' @param q the VAR lag
#' @param max.iteration max number of iteration for the Fused lasso
#' @param tol tolerance for the fused lasso
#' @param block.size the block size
#' @param blocks the blocks
#' @param refit logical; if TRUE, refit the VAR model for parameter estimation. Default is FALSE.
#' @param use.BIC use BIC for k-means part
#' @param an.grid a vector of an for grid searching
#' @param verbose a Boolean argument to determine whether provide detailed outputs for each step. Default is FALSE
#' @return S3 object of class \code{VARDetect.result}, which contains the followings
#' \describe{
#'   \item{data}{the original dataset}
#'   \item{q}{the time lag user specified, a numeric value}
#'   \item{cp}{final estimated change points, a numeric vector}
#'   \item{sparse_mats}{estimated sparse components for each segment, a list of numeric matrices}
#'   \item{lowrank_mats}{estimated low rank components for each segment, a list of numeric matrices}
#'   \item{est_phi}{estimated final model parameters, the summation of the sparse and the low rank components}
#'   \item{time}{computation time for each step}
#' }
#' @export
#' @importFrom Rcpp sourceCpp
#' @importFrom stats ar
#' @importFrom sparsevar fitVAR
#' @examples
#' #### sparse VAR model
#' nob <- (10^3) # number of time points
#' p <- 15; # number of time series components
#' brk <- c(floor(nob/3),floor(2*nob/3),nob+1); # true break points with nob+1 as the last element
#' m0 <- length(brk) -1; # number of break points
#' q.t <- 1; # the true AR order
#' m <- m0+1 #number of segments
#' try<-simu_var('sparse',nob=nob,k=p,lags=q.t,brk = brk,sp_pattern="off-diagonal",seed=1)
#' data <- try$series
#' data <- as.matrix(data)
#' #run the bss method
#' fit <- tbss(data, method = "sparse", q = q.t)
#' print(fit)
#' summary(fit)
#' plot(fit, data, display = "cp")
#' plot(fit, data, display = "param")
#'
#'
#' ###### Example for fixed low rank plus sparse structure VAR model
#' nob <- 300
#' p <- 15
#' brk <- c(floor(nob/3), floor(2*nob/3), nob+1)
#' m <- length(brk)
#' q.t <- 1
#' signals <- c(-0.7, 0.7, -0.7)
#' rank <- c(2, 2, 2)
#' singular_vals <- c(1, 0.75)
#' info_ratio <- rep(0.35, 3)
#' try <- simu_var(method = "fLS", nob = nob, k = p, lags = 1, brk = brk,
#'                 sigma = as.matrix(diag(p)), signals = signals, seed=1,
#'                 rank = rank, singular_vals = singular_vals, info_ratio = info_ratio,
#'                 sp_pattern = "off-diagonal", spectral_radius = 0.9)
#' data <- try$series
#' data <- as.matrix(data)
#' fit <- tbss(data, method = "fLS", mu = 150)
#' print(fit)
#' summary(fit)
#' plot(fit, data, display = "cp")
#' plot(fit, data, display = "param")
tbss <- function(data, method = c("sparse", "group sparse", "fLS"),
                 group.case = c("columnwise", "rowwise"), group.index = NULL,
                 lambda.1.cv = NULL, lambda.2.cv = NULL, mu = NULL, q = 1,
                 max.iteration = 50, tol = 10^(-2), block.size = NULL, blocks = NULL,
                 refit = FALSE, use.BIC = TRUE, an.grid = NULL, verbose = FALSE){

    # Sanity check for arguments
    method <- match.arg(method)
    group.case <- match.arg(group.case)
    nob <- length(data[,1])
    p <- length(data[1,])
    second.brk.points <- c()
    pts.final <- c()

    ############# block size and blocks ###########
    if(is.null(block.size) && is.null(blocks)){
        block.size = floor(sqrt(nob))
        blocks <- seq(0, nob, block.size)
    }else if(!is.null(block.size) && is.null(blocks)){
        blocks <- seq(0, nob, block.size)
    }else if(!is.null(block.size) && !is.null(blocks)){
        # check if the block.size and blocks match
        n.new <- length(blocks) - 1
        blocks.size.check <- sapply(c(1:n.new), function(jjj) blocks[jjj+1] - blocks[jjj])
        if(sum(blocks.size.check[1:(length(blocks.size.check) - 1)] != block.size) > 0){
            stop("The block.size and blocks can't match!")
        }
    }

    if(blocks[length(blocks)] < nob){
        blocks <- c(blocks[-length(blocks)], nob)
    }

    n.new <- length(blocks) - 1
    blocks.size <- sapply(c(1:n.new), function(jjj) blocks[jjj+1] - blocks[jjj])

    # sample the cv index for cross-validation
    bbb <- floor(n.new / 5)
    aaa <- 4
    cv.index <- seq(aaa, n.new, floor(n.new / bbb))

    ############# Tuning parameter ################
    if(is.null(lambda.1.cv)){
        lambda.1.max <- lambda_warm_up(data, q, blocks, cv.index)$lambda_1_max
        if(blocks[2] <= 2 * p){
            epsilon <- 10^(-3)
        }else{
            epsilon <- 10^(-4)
        }
        nlam <- 10
        lambda.1.min <-  lambda.1.max * epsilon
        delata.lam <- (log(lambda.1.max) - log(lambda.1.min)) / (nlam - 1)
        lambda.1.cv <-  sapply(1:(nlam), function(jjj) lambda.1.min * exp(delata.lam * (nlam - jjj)))
    }

    if(is.null(lambda.2.cv)){
        lambda.2.cv <- c(10 * sqrt(log(p) / nob), sqrt(log(p) / nob), 0.1 * sqrt(log(p) / nob))
        if(method == "group sparse"){
            lambda.2.cv <- sqrt(p) * c(10 * sqrt(log(p) / nob), sqrt(log(p) / nob), 0.1 * sqrt(log(p) / nob))
        }
    }


    ######## group index #######
    if(is.null(group.index)){
        if(group.case == "columnwise"){
            # column-wise seperate across all lags
            group.index <- as.list(c(0:(p * q - 1)))
        }
        if(group.case == "rowwise"){
            # row-wise simultaneously across all lags
            group.index <- vector("list", p)
            for(i in 1:p){
                group.index[[i]] <- rep(i-1, q) + seq(0, p * (q-1), p)
            }
        }
    }else{
        # error conditions
        if(max(unlist(group.index)) > p * q - 1 | max(unlist(group.index)) < 0){
            stop("incorrect group index! Should among the column index or row index across all lags")
        }
        index.diff <- setdiff(seq(0, p * q - 1, 1), unique(unlist(group.index)))
        if(length(index.diff) > 0){
            # if the group index list is not complete:
            group.index[[length(group.index) + 1]] <- index.diff
        }
    }

    ######################################################################
    ######## First Step: Initial Break Points Selection ##################
    ######################################################################
    time.comparison <- rep(0, 3)

    # run the first step
    ptm.temp <- proc.time()
    if(method == "sparse"){
        # run the first block fused lasso step
        temp.first <- first.step.blocks(data, lambda.1.cv, lambda.2.cv, q, max.iteration = max.iteration, tol = tol, cv.index, blocks=blocks)
    }
    if(method == "group sparse"){
        # run the first block fused lasso step
        temp.first <- first.step.blocks.group(data, lambda.1.cv, lambda.2.cv, q, max.iteration = max.iteration, tol = tol,
                                              cv.index, blocks = blocks,  group.case = group.case, group.index = group.index)
    }
    if(method == "fLS"){
        n <- dim(data)[1]
        p <- dim(data)[2]

        # estimate low-rank components
        X <- data[1:(n-1), ]
        Y <- data[2:n, ]
        fit_lr <- fista.nuclear(X, Y, mu, p, niter = max.iteration,
                                backtracking = TRUE, diag(p))
        L_est <- t(fit_lr$phi.hat)

        # remove low-rank effects from the data
        data_remove <- matrix(0, n, p)
        data_remove[1,] <- data[1,]
        Y_temp <- matrix(0, n-1, p)
        Y_temp <- data[(2:n),]
        for(i in 1:(n-1)){
            xtm <- matrix(data_remove[i,], 1, p)
            ytm <- matrix(Y_temp[i,], 1, p)
            tmp <- ytm - xtm %*% t(L_est)
            data_remove[i+1,] <- tmp
        }

        # first step for fixed low rank plus sparse
        temp.first <- first.step.blocks(data_remove, lambda.1.cv, lambda.2.cv, q,
                                        max.iteration = max.iteration, tol = tol, cv.index, blocks = blocks)
    }

    time.temp <- proc.time() - ptm.temp
    time.comparison[1] <- c(time.temp[3])

    first.brk.points <- temp.first$brk.points
    phi.est.full <- temp.first$phi.full

    if(verbose){
        cat("first.brk.points: \n")
        cat(first.brk.points)
        cat("\n")

        cat("Selected lambda1: \n")
        cat(temp.first$cv1.final)
        cat("\n")

        cat("Selected lambda2: \n")
        cat(temp.first$cv2.final)
        cat("\n")
    }

    if(method == "group sparse"){
        if(length(first.brk.points) > 0){
            # construct the grid values of neighborhood size a_n
            n <- nob - q
            an.lb <- max(floor(mean(blocks.size)), floor((log(n) * log(p))))
            an.ub <-  min(5 * an.lb, 0.95 * (min(first.brk.points) - 1 - q), 0.95 * (n - max(first.brk.points) - 1))
            if(is.null(an.grid)){
                an.grid <- seq(an.lb, an.ub, length.out = 5)
            }

            an.idx.final <- length(an.grid)
            an.grid <- floor(an.grid)
            final.pts.res <- vector("list", length(an.grid))
            final.phi.hat.list.res <- vector("list",length(an.grid))
            flag <- c("FALSE")
            an.idx <- 0
            phi.local.1.full <- vector("list", length(an.grid))
            phi.local.2.full <- vector("list", length(an.grid))

            # for each a_n, run the second and third step
            while(an.idx < length(an.grid)){
                an.idx <- an.idx + 1
                an <- an.grid[an.idx]

                # remove the boundary points
                remove.ind <- c()
                if(length(first.brk.points) != 0){
                    for(i in 1:length(first.brk.points)){
                        if(first.brk.points[i] < (an - 1 - q)){
                            remove.ind <- c(remove.ind, i)
                        }
                        if((nob-first.brk.points[i]) < (an - 1 - q)){
                            remove.ind <- c(remove.ind, i)
                        }
                    }
                }
                if(length(remove.ind) > 0){
                    first.brk.points <- first.brk.points[-remove.ind]
                }

                # if there are selected break points after the first step
                if(length(first.brk.points) != 0){

                    #####################################################
                    ######## Second Step: Local Screening      ##########
                    #####################################################
                    eta <- (log(2 * an) * log(p)) / (2 * an) # the tuning parameter for second and third steps.
                    # run the second local screening step
                    ptm.temp <- proc.time()
                    temp <- second.step.local(method=method, data, eta = eta, q, max.iteration = 1000, tol = tol, first.brk.points,
                                              an, phi.est.full = phi.est.full, blocks, use.BIC, group.case = group.case, group.index = group.index)

                    time.temp <- proc.time() - ptm.temp
                    time.comparison[2] <- time.comparison[2] + c(time.temp[3])

                    second.brk.points <- temp$pts
                    if(verbose){
                        cat("Second step selected break points: \n")
                        cat(second.brk.points)
                        cat("\n")
                    }

                    ######################################################
                    ######## Third Step: Exhaustive Search      ##########
                    ######################################################
                    pts.final <- second.brk.points
                    phi.local.1.full <- temp$phi.local.1
                    phi.local.2.full <- temp$phi.local.2

                    if(length(pts.final) == 0){
                        idx <- floor(n.new / 2)
                        phi.hat.list <- phi.est.full[[idx]]
                    }

                    ptm.temp <- proc.time()
                    if(min(abs(diff(pts.final)), 3 * an) > 2 * an){
                        if(length(pts.final) != 0){
                            pts.list <- block.finder(pts.final, 2 * an)
                            local.idx = sapply(1:length(pts.list),
                                               function(jjj) which.min(abs(pts.list[[jjj]][1] - first.brk.points)))

                            temp.third <- third.step.exhaustive.search(data, q, max.iteration = 1000, tol = tol, pts.list,
                                                                       an, phi.est.full= phi.est.full,
                                                                       phi.local.1 = phi.local.1.full[local.idx],
                                                                       phi.local.2 = phi.local.2.full[local.idx],
                                                                       blocks)
                            phi.hat.list <- temp.third$phi.hat.list
                            pts.final <- temp.third$pts
                        }
                    }else{
                        # keep running until none of the selected break points close to any other selected break points
                        while(min(abs(diff(pts.final)), 3 * an) <= 2 * an){
                            if(length(pts.final) != 0){
                                # cluster the selected break points by size 2a_n
                                pts.list <- block.finder(pts.final, 2 * an)
                                local.idx = sapply(1:length(pts.list),
                                                   function(jjj) which.min(abs(pts.list[[jjj]][1] - first.brk.points)))
                                temp.third<- third.step.exhaustive.search(data, q, max.iteration = 1000, tol = tol, pts.list,
                                                                          an, phi.est.full= phi.est.full,
                                                                          phi.local.1 = phi.local.1.full[local.idx],
                                                                          phi.local.2 = phi.local.2.full[local.idx],
                                                                          blocks)
                                phi.hat.list <- temp.third$phi.hat.list
                                pts.final <- temp.third$pts
                            }
                        }
                    }
                    time.temp <- proc.time() - ptm.temp;
                    time.comparison[3] <- time.comparison[3] + c(time.temp[3])

                    # record the final selected break points for each given a_n
                    final.pts.res[[an.idx]] <- pts.final
                    final.phi.hat.list.res[[an.idx]] <- phi.hat.list

                    # terminate the grid search of an if the number of final selected break points is stable
                    if(an.idx > 2){
                        if(length(final.pts.res[[an.idx]]) == length(final.pts.res[[an.idx-1]]) && length(final.pts.res[[an.idx-1]]) == length(final.pts.res[[an.idx-2]])){
                            flag <- c("TRUE")
                            an.idx.final <- an.idx
                            an.sel <- an.grid[an.idx]
                            break
                        }
                    }
                }
            }

            # if the stable criterion hasn't been met
            # find the length that happen the most
            # if there are multiple lengths with same occurrence, find the longest one
            if(flag == FALSE){
                loc.final <- rep(0, length(an.grid))
                for(i in 1:length(an.grid)){
                    loc.final[i] <- length(final.pts.res[[i]])
                }
                loc.table <- table(loc.final)
                counts.final <- sort(loc.table, decreasing = TRUE)[1]
                if(counts.final >= 3){
                    len.final <- max(as.integer(names(loc.table)[loc.table == counts.final]))
                }else if(counts.final == 2){
                    len.final = 0
                    for(ii in 2:length(an.grid)){
                        if(length(final.pts.res[[ii]]) == length(final.pts.res[[ii-1]])){
                            len.final = max(len.final, length(final.pts.res[[ii]]))
                        }
                    }
                    if(len.final == 0){
                        # choose the longest one instead
                        len.final <- max(loc.final)
                    }
                }else{
                    # choose the longest one instead
                    len.final <- max(loc.final)
                }
                an.idx.final <- max(c(1:length(loc.final))[loc.final == len.final])
                an.sel <- an.grid[an.idx.final]
            }
            if(refit){
                if(verbose){
                    cat("Refit for the parameter estimation")
                }
                temp <- final.phi.hat.list.res[[an.idx.final]]
                cp.final <- final.pts.res[[an.idx.final]]
                cp.full <- c(1, cp.final, nob + 1)
                for(i in 1:(length(cp.final) + 1)){
                    data_y_temp <- as.matrix(data[(cp.full[i] + mean(blocks.size)): (cp.full[i+1] - 1 - mean(blocks.size)), ])
                    if(ncol(data_y_temp) == 1){
                        # AR model AR(q)
                        fit <- ar(data_y_temp, FALSE, order.max	= q)
                        temp[[i]] <- fit$ar
                    }else{
                        # VAR(q) model
                        fit <- fitVAR(data_y_temp, p = q)
                        temp[[i]] <- do.call(cbind, fit$A)
                    }
                }
                phi.hat.list <- temp
                final.result <- structure(list(data = data,
                                               q = q,
                                               cp = cp.final,
                                               sparse_mats = phi.hat.list,
                                               lowrank_mats = NULL,
                                               est_phi = phi.hat.list,
                                               time = time.comparison), class = "VARDetect.result")
                return(final.result)
            }else{
                if(verbose){
                    cat("No refit for the parameter estimation")
                }
                final.result <- structure(list(data = data,
                                               q = q,
                                               cp = final.pts.res[[an.idx.final]],
                                               sparse_mats = final.phi.hat.list.res[[an.idx.final]],
                                               lowrank_mats = NULL,
                                               est_phi = final.phi.hat.list.res[[an.idx.final]],
                                               time = time.comparison),
                                          class = "VARDetect.result")
                return(final.result)
            }
        }else{
            final.result <- structure(list(data = data,
                                           q = q,
                                           cp = first.brk.points,
                                           sparse_mats = NULL,
                                           lowrank_mats = NULL,
                                           est_phi = NULL,
                                           time = NULL),
                                      class = "VARDetect.result")
        return(final.result)
        }
    }

    if(method == "sparse" | method == "fLS"){
        if(length(first.brk.points) > 0){
            # construct the grid values of neighborhood size a_n
            n <- nob - q
            an.lb <- max(floor(mean(blocks.size)), floor((log(n) * log(p))))
            an.ub <- min(5 * an.lb, 0.95 * (min(first.brk.points) - 1 - q), 0.95*(n - max(first.brk.points) - 1))
            if(is.null(an.grid)){
                an.grid <- seq(an.lb,  an.ub, length.out = 5)
            }

            an.idx.final <- length(an.grid)
            an.grid <- floor(an.grid)
            final.pts.res <- vector("list", length(an.grid))
            final.phi.hat.list.res <- vector("list", length(an.grid))

            flag <- c("FALSE")
            an.idx <- 0
            phi.local.1.full <- vector("list", length(an.grid))
            phi.local.2.full <- vector("list", length(an.grid))

            # for each a_n, run the second and third step
            while(an.idx < length(an.grid)){
                an.idx <- an.idx + 1
                an <- an.grid[an.idx]

                # remove the boundary points
                remove.ind <- c()
                if(length(first.brk.points) != 0){
                    for(i in 1:length(first.brk.points)){
                        if(first.brk.points[i] < (an - 1 - q)){
                            remove.ind <- c(remove.ind, i)
                        }
                        if((nob - first.brk.points[i]) < (an - 1 - q)){
                            remove.ind <- c(remove.ind, i)
                        }
                    }
                }
                if(length(remove.ind) > 0){
                    first.brk.points <- first.brk.points[-remove.ind]
                }

                # if there are selected break points after the first step
                if(length(first.brk.points) != 0){

                    #####################################################
                    ######## Second Step: Local Screening      ##########
                    #####################################################
                    eta <- (log(2 * an) * log(p)) / (2 * an) # the tuning parameter for second and third steps.
                    # run the second local screening step
                    ptm.temp <- proc.time()
                    temp <- second.step.local(method = method, data, eta = eta, q, max.iteration = 1000, tol = tol, first.brk.points, an,
                                              phi.est.full = phi.est.full, blocks, use.BIC)

                    time.temp <- proc.time() - ptm.temp
                    time.comparison[2] <- time.comparison[2] + c(time.temp[3])

                    # record the selected break points after local screening step
                    second.brk.points <- temp$pts

                    if(verbose){
                        cat("Second step selected break points: \n")
                        cat(second.brk.points)
                        cat("\n")
                    }

                    ######################################################
                    ######## Third Step: Exhaustive Search      ##########
                    ######################################################
                    pts.final <- second.brk.points
                    phi.local.1.full <- temp$phi.local.1
                    phi.local.2.full <- temp$phi.local.2

                    if(length(pts.final) == 0){
                        idx <- floor(n.new / 2)
                        phi.hat.list <- phi.est.full[[idx]]
                    }

                    ptm.temp <- proc.time()
                    if(min(abs(diff(pts.final)), 3 * an) > 2 * an){
                        if(length(pts.final) != 0){
                            pts.list <- block.finder(pts.final, 2 * an)
                            local.idx = sapply(1:length(pts.list),
                                               function(jjj) which.min(abs(pts.list[[jjj]][1] - first.brk.points)))
                            temp.third <- third.step.exhaustive.search(data, q, max.iteration = 1000, tol = tol, pts.list,
                                                                       an, phi.est.full= phi.est.full,
                                                                       phi.local.1 = phi.local.1.full[local.idx],
                                                                       phi.local.2 = phi.local.2.full[local.idx],
                                                                       blocks)
                            phi.hat.list <- temp.third$phi.hat.list
                            pts.final <- temp.third$pts
                        }
                    }else{
                        # keep running until none of the selected break points close to any other selected break points
                        while(min(abs(diff(pts.final)), 3 * an) <= 2 * an){
                            if(length(pts.final) != 0){
                                # cluster the selected break points by size 2a_n
                                pts.list <- block.finder(pts.final, 2 * an)
                                local.idx = sapply(1:length(pts.list),
                                                   function(jjj) which.min(abs(pts.list[[jjj]][1] - first.brk.points)))
                                temp.third<- third.step.exhaustive.search(data, q, max.iteration = 1000, tol = tol, pts.list,
                                                                          an, phi.est.full= phi.est.full,
                                                                          phi.local.1 = phi.local.1.full[local.idx],
                                                                          phi.local.2 = phi.local.2.full[local.idx],
                                                                          blocks)
                                phi.hat.list <- temp.third$phi.hat.list
                                pts.final <- temp.third$pts
                            }
                        }
                    }

                    time.temp <- proc.time() - ptm.temp
                    time.comparison[3] <- time.comparison[3] + c(time.temp[3])

                    # record the final selected break points for each given a_n
                    final.pts.res[[an.idx]] <- pts.final
                    final.phi.hat.list.res[[an.idx]] <- phi.hat.list

                    # terminate the grid search of an if the number of final selected break points is stable
                    if(an.idx > 2){
                        if(length(final.pts.res[[an.idx]]) == length(final.pts.res[[an.idx-1]]) && length(final.pts.res[[an.idx-1]]) == length(final.pts.res[[an.idx-2]])){
                            flag <- c("TRUE")
                            an.idx.final <- an.idx
                            an.sel <- an.grid[an.idx]
                            break
                        }
                    }
                }
            }

            # if the stable criterion hasn't been met
            # find the length that happen the most
            # if there are multiple lengths with same occurrence, find the longest one
            if(flag == FALSE){
                loc.final <- rep(0, length(an.grid))
                for(i in 1:length(an.grid)){
                    loc.final[i] <- length(final.pts.res[[i]])
                }
                loc.table <- table(loc.final)
                counts.final <- sort(loc.table, decreasing = TRUE)[1]
                if(counts.final >= 3){
                    len.final <- max(as.integer(names(loc.table)[loc.table == counts.final]))
                }else if(counts.final == 2){
                    len.final = 0
                    for(ii in 2:length(an.grid)){
                        if(length(final.pts.res[[ii]]) == length(final.pts.res[[ii-1]])){
                            len.final = max(len.final, length(final.pts.res[[ii]]))
                        }
                    }
                    if(len.final == 0){
                        # choose the longest one instead
                        len.final <- max(loc.final)
                    }
                }else{
                    # choose the longest one instead
                    len.final <- max(loc.final)
                }
                an.idx.final <- max(c(1:length(loc.final))[loc.final == len.final])
                an.sel <- an.grid[an.idx.final]
            }
            if(method == "sparse"){
                if(refit){
                    if(verbose){
                        cat("Refit for the parameter estimation")
                    }
                    temp <- final.phi.hat.list.res[[an.idx.final]]
                    cp.final <- final.pts.res[[an.idx.final]]
                    cp.full <- c(1, cp.final, nob + 1)
                    for(i in 1:(length(cp.final) + 1)){
                        data_y_temp <- as.matrix(data[(cp.full[i] + mean(blocks.size)):(cp.full[i+1] - 1 - mean(blocks.size)), ])
                        if(ncol(data_y_temp) == 1){
                            # AR model AR(q)
                            fit <- ar(data_y_temp, FALSE, order.max	= q)
                            temp[[i]] <- fit$ar
                        }else{
                            # VAR(q) model
                            fit <- fitVAR(data_y_temp, p = q)
                            temp[[i]] <- do.call(cbind, fit$A)
                        }
                    }
                    phi.hat.list <- temp
                    final.result <- structure(list(data = data,
                                                   q = q,
                                                   cp = cp.final,
                                                   sparse_mats = phi.hat.list,
                                                   lowrank_mats = NULL,
                                                   est_phi = phi.hat.list,
                                                   time = time.comparison),
                                              class = "VARDetect.result")
                    return(final.result)
                }else{
                    if(verbose){
                        cat("No refit for the parameter estimation")
                    }
                    final.result <- structure(list(data = data,
                                                   q = q,
                                                   cp = final.pts.res[[an.idx.final]],
                                                   sparse_mats = final.phi.hat.list.res[[an.idx.final]],
                                                   lowrank_mats = NULL,
                                                   est_phi = final.phi.hat.list.res[[an.idx.final]],
                                                   time = time.comparison),
                                              class = "VARDetect.result")
                    return(final.result)
                }
            }else if(method == "fLS"){
                if(refit){
                    if(verbose){
                        cat("Refit for the parameter estimation")
                    }
                    temp <- final.phi.hat.list.res[[an.idx.final]]
                    cp.final <- final.pts.res[[an.idx.final]]
                    cp.full <- c(1, cp.final, nob + 1)
                    for(i in 1:(length(cp.final) + 1)){
                        data_y_temp <- as.matrix(data_remove[(cp.full[i] + mean(blocks.size)): (cp.full[i+1] - 1 - mean(blocks.size)), ])
                        if(ncol(data_y_temp) == 1){
                            # AR model AR(q)
                            fit <- ar(data_y_temp, FALSE, order.max = q)
                            temp[[i]] <- fit$ar
                        }else{
                            # VAR(q) model
                            fit <- fitVAR(data_y_temp, p = q)
                            temp[[i]] <- do.call(cbind, fit$A)
                        }
                    }
                    phi.hat.list <- temp
                    est_phi <- vector('list', length(cp.final) + 1)
                    for(j in 1:length(est_phi)){
                        est_phi[[j]] <- L_est + phi.hat.list[[j]]
                    }
                    final.result <- structure(list(cp = cp.final,
                                                   sparse_mats = phi.hat.list,
                                                   lowrank_mats = list(L_est),
                                                   est_phi = est_phi,
                                                   time = time.comparison),
                                              class = "VARDetect.result")
                    return(final.result)
                }else{
                    if(verbose){
                        cat("No refit for the parameter estimation")
                    }
                    est_phi <- vector('list', length(final.pts.res[[an.idx.final]]) + 1)
                    for(j in 1:length(est_phi)){
                        est_phi[[j]] <- L_est + final.phi.hat.list.res[[an.idx.final]][[j]]
                    }
                    final.result <- structure(list(data = data,
                                                   q = q,
                                                   cp = final.pts.res[[an.idx.final]],
                                                   sparse_mats = final.phi.hat.list.res[[an.idx.final]],
                                                   lowrank_mats = list(L_est),
                                                   est_phi = est_phi,
                                                   time = time.comparison),
                                              class = "VARDetect.result")
                    return(final.result)
                }
            }
        }else{
            final.result <- structure(list(data = data,
                                           q = q,
                                           cp = first.brk.points,
                                           sparse_mats = NULL,
                                           lowrank_mats = NULL,
                                           est_phi = NULL,
                                           time = NULL),
                                      class = "VARDetect.result")
            return(final.result)
        }
    }
}

#' block fused lasso step (first step for BSS).
#'
#' @description Perform the block fused lasso to detect candidate break points.
#'
#' @param data.temp input data matrix, with each column representing the time series component
#' @param lambda.1.cv tuning parameter lambda_1 for fused lasso
#' @param lambda.2.cv tuning parameter lambda_2 for fused lasso
#' @param q the AR order
#' @param max.iteration max number of iteration for the fused lasso
#' @param tol tolerance for the fused lasso
#' @param cv.index the index of time points for cross-validation
#' @param blocks the blocks
#' @return A list object, which contains the followings
#' \describe{
#'   \item{brk.points}{a set of selected break point after the first block fused lasso step}
#'   \item{cv}{the cross validation values for tuning parmeter selection}
#'   \item{cv1.final}{the selected lambda_1}
#'   \item{cv2.final}{the selected lambda_2}
#' }
#' @keywords internal
#'
first.step.blocks <- function(data.temp, lambda.1.cv, lambda.2.cv, q, max.iteration = max.iteration, tol = tol,cv.index, blocks){
    cv.l <- length(cv.index)
    data.org <- data.temp
    nob.org <- length(data.temp[,1])
    p <- length(data.temp[1,])
    n.new <- length(blocks) - 1
    blocks.size <- sapply(c(1:n.new), function(jjj) blocks[jjj+1] - blocks[jjj])

    # create the tuning parameter combination of lambda1 and lambda2
    lambda.full <- expand.grid(lambda.1.cv, lambda.2.cv)
    kk <- length(lambda.full[,1])

    cv <- rep(NA, kk)
    phi.final <- vector("list", kk)
    nob <- length(data.temp[,1])
    p <- length(data.temp[1,])
    brk.points.final <- vector("list", kk)
    flag.full <- rep(0, kk)

    # cross-validation for each values of lambda1 and lambda2
    nlam1 <- length(lambda.1.cv)
    nlam2 <- length(lambda.2.cv)
    kk <- nlam1 * nlam2
    i = 1
    while(i <= kk){
        i.lam1 <- i %% nlam1
        if(i.lam1 == 0){
            i.lam1 = nlam1
        }
        i.lam2 <- floor((i - 1) / nlam1) + 1
        if(i == 1){
            test <- var_break_fit_block_cpp(data.temp,
                                            lambda.full[i,1],
                                            lambda.full[i,2],
                                            q, max.iteration, tol = tol,
                                            initial_phi = 0.0 + matrix(0.0, p, p * q * n.new),
                                            blocks, cv.index)
            flag.full[i] <- test$flag
        }else if(is.na(cv[i-1])){
            test <- var_break_fit_block_cpp(data.temp,
                                            lambda.full[i,1],
                                            lambda.full[i,2],
                                            q, max.iteration, tol = tol,
                                            initial_phi = 0.0 + matrix(0.0, p, p * q * n.new),
                                            blocks, cv.index)
            flag.full[i] <- test$flag
        }else{
            initial.phi <- phi.final[[i-1]]
            if(max(abs(phi.final[[(i-1)]])) > 10^3){
                initial.phi <- 0 * phi.final[[i-1]]
            }
            test <- var_break_fit_block_cpp(data.temp,
                                            lambda.full[i,1],
                                            lambda.full[i,2],
                                            q, max.iteration, tol = tol,
                                            initial_phi = initial.phi,
                                            blocks, cv.index)
            flag.full[i] <- test$flag
        }
        phi.hat.full <- test$phi.hat
        phi.final[[i]] <- phi.hat.full
        ll <- c(0)
        brk.points.list <- vector("list", length(ll))

        for(j in 1:length(ll)){
            phi.hat <- phi.hat.full
            n <- nob - q
            m.hat <- 0
            brk.points <- rep(0, n.new)

            for (iii in 1:(n.new - 1)){
                if(sum((phi.hat[,((iii - 1) * p * q + 1):(iii * p * q)])^2) > tol){
                    m.hat <- m.hat + 1
                    brk.points[m.hat] <- blocks[iii+1]
                }
            }
            loc <- rep(0, m.hat)
            brk.points <- brk.points[1:m.hat]

            # remove the boundary points and clean up
            brk.points <- brk.points[which(brk.points > 3 * mean(blocks.size))]
            brk.points <- brk.points[which(brk.points < (n - 3 * mean(blocks.size)))]
            m.hat <- length(brk.points)
            del <- 0
            if(m.hat >= 2){
                while(del < m.hat){
                    if(length(brk.points) <= 1){
                        break
                    }
                    del <- del + 1
                    deleted <- 0
                    for(i.3 in 2:length(brk.points)){
                        if(deleted == 0 && abs(brk.points[i.3] - brk.points[i.3-1]) <= max(q, min(blocks.size))){
                            brk.points <- brk.points[-i.3]
                            deleted <- 1
                        }
                    }
                }
            }
            brk.points.list[[j]] <- brk.points
        }

        brk.points.final[[i]] <- brk.points
        m.hat <- length(brk.points)

        # forecast the time series based on the estimated matrix Phi
        # and compute the forecast error
        phi.full.all <- vector("list", n.new)
        forecast <- matrix(0, p, nob)
        phi.full.all[[1]] <- phi.hat[,1:(p * q)]
        for(i.1 in 2:n.new){
            phi.full.all[[i.1]] <- phi.full.all[[i.1-1]] + phi.hat[,((i.1 - 1) * p * q + 1):(i.1 * p * q)]
            forecast[,(blocks[i.1] + 1):(blocks[i.1 + 1])] <- pred.block(t(data.org),
                                                                         phi.full.all[[i.1-1]],
                                                                         q, blocks[i.1], p,
                                                                         blocks[i.1+1] - blocks[i.1])
        }
        forecast.new <- matrix(0, p, cv.l)
        for(j in 1:cv.l){
            forecast.new[,j] <- pred(t(data.org),
                                     phi.full.all[[(cv.index[j])]],
                                     q, blocks[cv.index[j]+1] - 1, p, 1)
        }
        temp.index <- rep(0, cv.l)
        for(ff in 1:cv.l){
            temp.index[ff] <- blocks[cv.index[ff] + 1]
        }
        cv[i] <- (1 / (p * cv.l)) * sum((forecast.new - t(data.org[temp.index,]))^2)

        # break condition
        if(nlam1 >= 2){
            if(!(i %in% c(seq(1, kk, nlam1), seq(2, kk, nlam1))) && cv[i] > cv[i-1]){
                i.lam2 <- i.lam2 + 1
                i <- (i.lam2 - 1) * nlam1 + 1
            }else{
                i <- i + 1
            }
        }else{
            i <- i + 1
        }
    }

    # select the tuning parameters that has the small cross-validation value
    lll <- min(which(cv == min(cv, na.rm = TRUE)))
    phi.hat.full <- phi.final[[lll]]

    # compute the estimated phi
    phi.par.sum <- vector("list", n.new)
    phi.par.sum[[1]] <- phi.hat.full[, 1:(p * q)]
    for(i in 2:n.new){
        phi.par.sum[[i]] <- phi.par.sum[[i-1]] + phi.hat.full[,((i - 1) * p * q + 1):(i * p * q)]
    }

    return(list(brk.points = brk.points.final[[lll]], cv = cv,
                cv1.final = lambda.full[lll,1], cv2.final = lambda.full[lll,2],
                phi.full = phi.par.sum))
}


#' block fused sparse group lasso step (first step).
#'
#' @description Perform the block fused lasso to detect candidate break points.
#'
#' @param data.temp input data matrix, with each column representing the time series component
#' @param lambda.1.cv tuning parmaeter lambda_1 for fused lasso
#' @param lambda.2.cv tuning parmaeter lambda_2 for fused lasso
#' @param q the AR order
#' @param max.iteration max number of iteration for the fused lasso
#' @param tol tolerance for the fused lasso
#' @param cv.index the index of time points for cross-validation
#' @param blocks the blocks
#' @param group.case group sparse pattern: column, row.
#' @param group.index group index for group sparse case
#' @return A list object, which contains the followings
#' \describe{
#'   \item{brk.points}{a set of selected break point after the first block fused lasso step}
#'   \item{cv}{the cross validation values for tuning parmeter selection}
#'   \item{cv1.final}{the selected lambda_1}
#'   \item{cv2.final}{the selected lambda_2}
#' }
#' @keywords internal
#'
first.step.blocks.group <- function(data.temp, lambda.1.cv, lambda.2.cv, q,
                                    max.iteration = max.iteration, tol = tol,
                                    cv.index, blocks,
                                    group.case = "columnwise", group.index){

    cv.l <- length(cv.index)
    data.org <- data.temp
    nob.org <- length(data.temp[,1])
    p <- length(data.temp[1,])
    n.new <- length(blocks) - 1
    blocks.size <- sapply(c(1:n.new), function(jjj) blocks[jjj+1] - blocks[jjj])

    # create the tuning parameter combination of lambda1 and lambda2
    lambda.full <- expand.grid(lambda.1.cv, lambda.2.cv)
    kk <- length(lambda.full[,1])

    cv <- rep(NA, kk)
    phi.final <- vector("list", kk)
    nob <- length(data.temp[,1])
    p <- length(data.temp[1,])
    brk.points.final <- vector("list", kk)
    flag.full <- rep(0, kk)

    # cross-validation for each values of lambda1 and lambda2
    nlam1 <- length(lambda.1.cv)
    nlam2 <- length(lambda.2.cv)
    kk <- nlam1 * nlam2
    i = 1
    while(i <= kk){
        i.lam1 <- i %% nlam1
        if(i.lam1 == 0){
            i.lam1 <- nlam1
        }
        i.lam2 <- floor((i - 1) / nlam1) + 1
        if(group.case == "columnwise"){
            if(i == 1){
                test <- var_break_fit_block_group_cpp(data.temp,
                                                      lambda.full[i,1],
                                                      lambda.full[i,2],
                                                      q, max.iteration, tol = tol,
                                                      initial_phi = 0.0 + matrix(0.0, p, p * q * n.new),
                                                      blocks, cv.index, group.index)
            flag.full[i] <- test$flag
            }else if(is.na(cv[i-1])){
                test <- var_break_fit_block_group_cpp(data.temp,
                                                      lambda.full[i,1],
                                                      lambda.full[i,2],
                                                      q, max.iteration, tol = tol,
                                                      initial_phi = 0.0 + matrix(0.0, p, p * q * n.new),
                                                      blocks, cv.index, group.index)
                flag.full[i] <- test$flag
            }else{
                initial.phi <- phi.final[[i-1]]
                if(max(abs(phi.final[[i-1]])) > 10^3){
                    initial.phi <- 0 * phi.final[[i-1]]
                }
                test <- var_break_fit_block_group_cpp(data.temp,
                                                      lambda.full[i,1],
                                                      lambda.full[i,2],
                                                      q, max.iteration, tol = tol,
                                                      initial_phi = initial.phi,
                                                      blocks, cv.index, group.index)
                flag.full[i] <- test$flag
            }

        }
        group.index.full <- group.index
        for(ii in 1:length(group.index)){
            tmp <- c()
            for(idx in group.index[[ii]]){
                tmp <- c(tmp, floor(idx / p) * p * p + seq(idx %% p, (p - 1) * p + idx %% p, by = p))
            }
            group.index.full[[ii]] <- tmp
        }
        if(group.case == "rowwise"){
            if(i == 1){
                test <- var_break_fit_block_groupidx_cpp(data.temp,
                                                         lambda.full[i,1],
                                                         lambda.full[i,2],
                                                         q, max.iteration, tol = tol,
                                                         initial_phi = 0.0 + matrix(0.0, p, p * q * n.new),
                                                         blocks, cv.index, group.index.full)

                flag.full[i] <- test$flag
            }else if(is.na(cv[i-1])){
                test <- var_break_fit_block_groupidx_cpp(data.temp,
                                                         lambda.full[i,1],
                                                         lambda.full[i,2],
                                                         q, max.iteration, tol = tol,
                                                         initial_phi = 0.0 + matrix(0.0, p, p * q * n.new),
                                                         blocks, cv.index, group.index.full)

                flag.full[i] <- test$flag
            }else{
                initial.phi <- phi.final[[i-1]]
                if(max(abs(phi.final[[i-1]])) > 10^3){
                    initial.phi <- 0 * phi.final[[i-1]]
                }
                test <- var_break_fit_block_groupidx_cpp(data.temp,
                                                         lambda.full[i,1],
                                                         lambda.full[i,2],
                                                         q, max.iteration, tol = tol,
                                                         initial_phi = initial.phi,
                                                         blocks, cv.index, group.index.full)

                flag.full[i] <- test$flag
            }

        }

        phi.hat.full <- test$phi.hat
        phi.final[[i]] <- phi.hat.full

        ll <- c(0)
        brk.points.list <- vector("list",length(ll));

        for(j in 1:length(ll)){
            phi.hat <- phi.hat.full
            n <- nob - q
            m.hat <- 0
            brk.points <- rep(0, n.new)

            for(iii in 1:(n.new - 1)){
                if(sum((phi.hat[,((iii - 1) * p * q + 1):(iii * p * q)])^2) > tol){
                    m.hat <- m.hat + 1
                    brk.points[m.hat] <- blocks[iii+1]
                }
            }

            loc <- rep(0, m.hat)
            brk.points <- brk.points[1:m.hat]

            # remove the boundary points and clean up
            brk.points <- brk.points[which(brk.points > 3 * mean(blocks.size))]
            brk.points <- brk.points[which(brk.points < (n - 3 * mean(blocks.size)))]
            m.hat <- length(brk.points)
            del <- 0
            if(m.hat >= 2){
                while(del < m.hat){
                    if(length(brk.points) <= 1){
                        break
                    }
                    del <- del + 1
                    deleted <- 0
                    for(i.3 in 2:length(brk.points)){
                        if(deleted == 0 &&  abs(brk.points[i.3] - brk.points[i.3-1]) <= max(q,min(blocks.size))){
                            brk.points <- brk.points[-i.3]
                            deleted <- 1
                        }
                    }
                }
            }

            brk.points.list[[j]] <- brk.points;
        }

        brk.points.final[[i]] <- brk.points;
        m.hat <- length(brk.points);

        # forecast the time series based on the estimated matrix Phi
        # and compute the forecast error
        phi.full.all <- vector("list",n.new);
        forecast <- matrix(0,p,nob);
        phi.full.all[[1]] <- phi.hat[,(1):(p*q)];
        for(i.1 in 2:n.new){
            phi.full.all[[i.1]] <- phi.full.all[[i.1 - 1]] + phi.hat[,((i.1 - 1) * p * q + 1):(i.1 * p * q)]
            forecast[,(blocks[i.1]+1):(blocks[i.1+1])] <- pred.block(t(data.org),
                                                                     phi.full.all[[i.1-1]],
                                                                     q, blocks[i.1], p, blocks[i.1 + 1] - blocks[i.1])
        }
        forecast.new <- matrix(0,p,cv.l);
        for(j in 1:cv.l){
            forecast.new[,j] <- pred(t(data.org), phi.full.all[[(cv.index[j])]],
                                     q, blocks[cv.index[j] + 1] - 1, p, 1)
        }
        temp.index <- rep(0, cv.l)
        for(ff in 1:cv.l){
            temp.index[ff] <- blocks[cv.index[ff] + 1]
        }
        cv[i] <- (1 / (p * cv.l)) * sum((forecast.new - t(data.org[temp.index,]))^2)

        # break condition
        if(nlam1 >= 2){
            if(!(i %in% c(seq(1, kk, nlam1), seq(2, kk, nlam1))) && cv[i] > cv[i-1]){
                i.lam2 <- i.lam2 + 1
                i <- (i.lam2 - 1) * nlam1 + 1
            }else{
                i <- i + 1
            }
        }else{
            i <- i + 1
        }
    }

    # select the tuning parameter that has the small cross-validation value
    lll <- min(which(cv == min(cv, na.rm = TRUE)))
    phi.hat.full <- phi.final[[lll]]

    # compute the estimated phi
    phi.par.sum <- vector("list", n.new)
    phi.par.sum[[1]] <- phi.hat.full[, 1:(p * q)]
    for(i in 2:n.new){
        phi.par.sum[[i]] <- phi.par.sum[[i-1]] + phi.hat.full[,((i - 1) * p * q + 1):(i * p * q)]
    }

    return(list(brk.points = brk.points.final[[lll]], cv = cv,
                cv1.final = lambda.full[lll, 1], cv2.final = lambda.full[lll, 2],
                phi.full = phi.par.sum))
}



#' BIC and HBIC function
#' @param residual residual matrix
#' @param phi estimated coefficient matrix of the model
#' @param gamma.val hyperparameter for HBIC, if HBIC == TRUE.
#' @return A list object, which contains the followings
#' \describe{
#'   \item{BIC}{BIC value}
#'   \item{HBIC}{HBIC value}
#' }
#' @keywords internal
#'
BIC <- function(residual, phi, gamma.val = 1){
    p <- length(phi[, 1])
    q <- length(phi[1, ]) / p
    nob.new <- length(residual[1, ])
    count <- 0
    count = sum(phi != 0)

    sigma.hat <- 0 * diag(p)
    for(i in 1:nob.new){
        sigma.hat <- sigma.hat + residual[, i] %*% t(residual[, i])
    }
    sigma.hat <- (1 / (nob.new)) * sigma.hat
    ee.temp <- min(eigen(sigma.hat)$values)
    if(ee.temp <= 10^(-8)){
        sigma.hat <- sigma.hat + 2.0 * (abs(ee.temp) + 10^(-3)) * diag(p)
    }

    log.det <- log(det(sigma.hat))
    count <- count
    return(list(
        BIC = log.det + log(nob.new) * count / nob.new,
        HBIC = log.det + 2 * gamma.val * log(p * q * p) * count / nob.new)
    )
}



#' local screening step (second step).
#'
#' @description Perform the local screening to "thin out" redundant break points.
#'
#' @param method method: sparse, group sparse
#' @param data input data matrix, with each column representing the time series component
#' @param eta tuning parameter eta for lasso
#' @param q the AR order
#' @param max.iteration max number of iteration for the fused lasso
#' @param tol tolerance for the fused lasso
#' @param pts the selected break points after the first step
#' @param an the neighborhood size a_n
#' @param phi.est.full parameter matrix
#' @param blocks a vector of blocks
#' @param use.BIC use BIC for k-means part
#' @param group.case group sparse pattern: columnwise, rowwise.
#' @param group.index group index for group sparse case
#' @return A list object, which contains the followings
#' \describe{
#'   \item{pts}{a set of selected break point after the second local screening step}
#'   \item{omega}{the selected Omega value}
#' }
#' @importFrom stats kmeans
#' @keywords internal
#'
second.step.local <- function(method = "sparse", data, eta, q, max.iteration = 1000, tol = 10^(-4), pts, an,
                              phi.est.full = NULL, blocks = NULL, use.BIC = FALSE,
                              group.case = "columnwise", group.index = NULL){
    m <- length(pts)
    p = length(data[1,])

    # compute the local loss functions for each selected break points
    try <- break.var.local.new(method, data, eta, q, max.iteration = 1000, tol = tol, pts, an,
                               group.case= group.case, group.index = group.index)

    # record the local loss function that include or exclude some break point
    L.n.1 = try$L.n.1
    L.n.2 = try$L.n.2

    # record the local estimate left (1) and right (2)
    phi.local.1 = try$phi.local.1
    phi.local.2 = try$phi.local.2

    # OMEGA is selected by data-driven method
    # first, compute the V value as the difference of loss functions that include and exclude some break point
    V = rep(0, m)
    for(i in 1:m){
        V[i] = L.n.2[i] - (L.n.1[2 * i - 1] + L.n.1[2 * i])
    }

    # add two boundary points as reference points (by assumption, their V values shoule be extremly small)
    nob <- length(data[,1])
    pts.redundant <- c(an + q, nob - an)
    try.redundant <- break.var.local.new(method, data, eta, q, max.iteration = 1000, tol = tol, pts.redundant, an,
                                         group.case= group.case, group.index = group.index)
    L.n.1.redundant = try.redundant$L.n.1
    L.n.2.redundant = try.redundant$L.n.2
    V.redundant <- rep(0, 2)
    for(i in 1:2){
        V.redundant[i] = L.n.2.redundant[i] - (L.n.1.redundant[2 * i - 1] + L.n.1.redundant[2 * i])
    }

    # use the maximum value of V.redundant as the reference V value
    V <- c(V, rep(max(V.redundant), 2))

    if(use.BIC == FALSE){
        if(length(unique(V)) <= 2){
            omega <- max(V) + 10^(-6)
        }else{
        # use k-means to cluster the V
            clus.2 <- kmeans(V, centers = 2)
            fit.2 <- clus.2$betweenss / clus.2$totss
            if(fit.2 < 0.20){
                omega <- max(V) + 10^(-6);
            }else{
            # if the reference point is in the subset with larger center, this means no change points: set omeage = max(V)
            # otherwise, set omega = min(V) -1
                loc <- clus.2$cluster
                if(clus.2$centers[1] > clus.2$centers[2]){
                    omega <- min(V[which(loc == 1)]) - 10^(-6)
                    if(loc[length(loc)] == 1){
                        omega <- max(V) + 10^(-6);
                    }
                }
                if(clus.2$centers[1] < clus.2$centers[2]){
                    omega <- min(V[which(loc == 2)]) - 10^(-6)
                    if(loc[length(loc)] == 2){
                        omega <- max(V) + 10^(-6);
                    }
                }
            }
        }
    }else{
        n.new <- length(blocks) - 1
        ###### use BIC to determine the k-means
        BIC.diff <- 1
        BIC.old <- 10^8
        pts.sel <- c()
        omega <- 0
        loc.block.full <- c()
        while(BIC.diff > 0 & length(unique(V)) > 1){
            pts.sel.old <- pts.sel
            omega.old <- omega

            # use k-means to cluster the V
            clus.2 <- kmeans(V, centers = 2)
            fit.2 <- clus.2$betweenss / clus.2$totss
            if(fit.2 < 0.20){
                omega <- max(V) + 10^(-6)
                pts.sel <- c(pts.sel)
                break
            }else{
                loc <- clus.2$cluster
                if(clus.2$centers[1] > clus.2$centers[2]){
                    omega <- min(V[which(loc == 1)]) - 10^(-6)
                    if(loc[length(loc)] == 1){
                        loc.idx <- which(loc == 1)
                    }else{
                        loc.idx <- which(loc == 1)
                    }
                }
                if(clus.2$centers[1] < clus.2$centers[2]){
                    omega <- min(V[which(loc == 2)]) - 10^(-6)
                    if(loc[length(loc)] == 2){
                        loc.idx <- which(loc == 2)
                    }else{
                        loc.idx <- which(loc == 2)
                    }
                }
                pts.sel <- sort(c(pts.sel, pts[loc.idx]))
                V[loc.idx] <- V[length(V)]
                loc.block.full <- match(pts.sel, blocks)
            }

            m.temp <- length(pts.sel)
            if(m.temp == 0){
                stop("No change points! stop!")
            }
            phi.est.new <- vector("list", m.temp + 1)
            cp.index.list <- vector("list", m.temp + 2)
            cp.index.list[[1]] <- c(1)
            cp.index.list[[m.temp+2]] <- c(n.new+1)
            for(i.1 in 1:m.temp){
                pts.temp <- pts.sel[i.1]
                cp.index.list[[i.1+1]] <- match(pts.temp, blocks)
            }
            phi.full.all <- vector("list", n.new);
            for(i.1 in 1:(m.temp + 1)){
                idx <- floor((cp.index.list[[i.1 + 1]] + cp.index.list[[i.1]]) / 2)
                phi.est.new[[i.1]] <- matrix(phi.est.full[[idx]], ncol = p * q)
                for(i.2 in cp.index.list[[i.1]]:(cp.index.list[[i.1 + 1]] - 1)){
                    phi.full.all[[i.2]] <- phi.est.new[[i.1]]
                }
            }

            forecast.all.new <- matrix(0, p, nob)
            forecast.all.new[, (blocks[1]+1+q):(blocks[1+1])] <-
                sapply(c((blocks[1] + 1 + q):(blocks[1+1])), function(jjj) pred(t(data), phi.full.all[[1]], q, jjj - 1 , p, 1))
            for(i.1 in 2:n.new){
                forecast.all.new[, (blocks[i.1] + 1):(blocks[i.1 + 1])] <-
                    sapply(c((blocks[i.1] + 1):(blocks[i.1 + 1])), function(jjj) pred(t(data), phi.full.all[[i.1]], q, jjj - 1 , p, 1))
            }
            residual <- t(data[((1 + q):nob), ]) - forecast.all.new[, (1 + q):nob]
            BIC.new <- BIC(residual, phi = do.call(cbind, phi.est.new))$BIC
            BIC.diff <- BIC.old - BIC.new
            BIC.old <- BIC.new
            if(BIC.diff <= 0){
                pts.sel <- sort(pts.sel.old)
                omega <- omega.old
                break
            }
        }
    }


    # select the break points by localized information criterion (LIC)
    L.n.1.temp <- L.n.1
    L.n.2.temp <- L.n.2
    L.n.plot <- rep(0, m + 1)
    L.n.plot[1] <- sum(L.n.1) + m * omega

    mm <- 0
    ic <- 0
    add.temp <- 0
    pts.full <- vector("list", m + 1)
    pts.full[[1]] <- pts
    ind.pts <- rep(0, m)

    while(mm < m){
        mm <- mm + 1
        L.n.temp <- rep(0, length(pts))
        for(i in 1:length(pts)){
            L.n.temp[i] <- sum(L.n.1.temp) - L.n.1.temp[(2*i-1)] - L.n.1.temp[(2*i)] + L.n.2.temp[i] + add.temp
        }
        ll <- min(which.min(L.n.temp))
        ind.pts[mm] <- ll
        pts <- pts[-ll]
        L.n.1.temp <- L.n.1.temp[-c(2 * ll - 1, 2 * ll)]
        add.temp <- add.temp + L.n.2.temp[ll]
        L.n.2.temp <- L.n.2.temp[-ll]
        L.n.plot[mm + 1] <- L.n.temp[ll] + (m - mm) * omega
        pts.full[[mm + 1]] <- pts
    }
    ind <- min(which.min(L.n.plot))

    return(list(pts = pts.full[[ind]], omega = omega,
                phi.local.1 = phi.local.1, phi.local.2 = phi.local.2))
}



#' Compute local loss function.
#'
#' @param method method: sparse, group sparse
#' @param data input data matrix, with each column representing the time series component
#' @param eta tuning parameter eta for lasso
#' @param q the AR order
#' @param max.iteration max number of iteration for the fused lasso
#' @param tol tolerance for the fused lasso
#' @param pts the selected break points after the first step
#' @param an the neighborhood size a_n
#' @param group.case group sparse pattern: columnwise, rowwise.
#' @param group.index group index for group sparse case
#' @return A list oject, which contains the followings
#' \describe{
#'   \item{L.n.1}{A vector of loss functions that include some break point}
#'   \item{L.n.2}{A vector of loss functions that exclude some break point}
#' }
#' @keywords internal
#'
break.var.local.new <- function(method = "sparse", data, eta, q, max.iteration = 1000, tol = 10^(-4), pts, an,
                                group.case = "columnwise", group.index = NULL){
    p <- length(data[1,])
    nob <- length(data[,1])
    m <- length(pts)

    # construct the local interval for computing the loss function
    bounds.1 <- vector("list", 2 * m)
    bounds.2 <- vector("list", m)
    for(i in 1:m){
        bounds.1[[(2*i-1)]] <- c(pts[i] - an, pts[i] - 1)
        bounds.1[[(2*i)]] <- c(pts[i], pts[i] + an)
        bounds.2[[(i)]] <- c(pts[i] - an, pts[i] + an)
    }

    # compute the local loss function that include the given break point
    L.n.1 <- c()

    # add a hash table to store the local estimate
    phi.local.1 <- vector("list", m)
    phi.local.2 <- vector("list", m)

    for(mm in 1:(2*m)){
        data.temp <- data[(bounds.1[[mm]][1]):(bounds.1[[mm]][2]),]
        if(method == "sparse" | method == "fLS"){
            try <- var_lasso_brk(data = data.temp, eta, q, 1000, tol = tol)
        }
        if(method == "group sparse"){
            if(group.case == "columnwise"){
                try <- var_lasso_brk_group(data = data.temp, eta, q, 1000, tol = tol, group.index)
            }
            if(group.case == "rowwise"){
                group.index.full <- group.index
                for(ii in 1:length(group.index)){
                    tmp <- c()
                    for(idx in group.index[[ii]]){
                        tmp <- c(tmp, (idx * p):((idx + 1) * p - 1))
                    }
                    group.index.full[[ii]] <- tmp
                }
                try <- var_lasso_brk_group_idx(data = data.temp, eta, q, 1000, tol = tol, group.index.full)
            }
            if(group.case == 'index'){
                group.index.full <- group.index
                try <- var_lasso_brk_group_idx(data = data.temp, eta, q, 1000, tol = tol, group.index.full)
            }
        }

        L.n.1 <- c(L.n.1, try$pred.error)
        key <- ceiling(mm / 2)
        if(mm %% 2 == 1){
            phi.local.1[[key]] <- try$phi.hat
        }else{
            phi.local.2[[key]] <- try$phi.hat
        }
    }

    # compute the local loss function that include the given break point
    L.n.2 <- c()
    for(mm in 1:m){
        data.temp <- data[(bounds.2[[mm]][1]):(bounds.2[[mm]][2]),]
        if(method == "sparse" | method == "fLS"){
            try <- var_lasso_brk(data = data.temp, eta, q, 1000, tol = tol)
        }
        if(method == "group sparse"){
            try <- var_lasso_brk_group(data = data.temp, eta, q, 1000, tol = tol, group.index)
        }
        L.n.2 <- c(L.n.2, try$pred.error)
    }

    return(list(L.n.1 = L.n.1, L.n.2 = L.n.2,
                phi.local.1 = phi.local.1, phi.local.2 = phi.local.2))
}



#' Exhaustive search step (third step).
#'
#' @description Perform the exhaustive search to select the break point for each cluster.
#'
#' @param data input data matrix, with each column representing the time series component
#' @param q the AR order
#' @param max.iteration max number of iteration for the fused lasso
#' @param tol tolerance for the fused lasso
#' @param pts.list the selected break points clustered by a_n after the second step
#' @param an the neighborhood size a_n
#' @param phi.est.full list of local parameter estimator
#' @param phi.local.1 a list of loca parameter estimator
#' @param phi.local.2 a list of loca parameter estimator
#' @param blocks the blocks
#' @return A list object, which contains the followings
#' \describe{
#'   \item{pts}{a set of final selected break point after the third exhaustive search step}
#' }
#' @keywords internal
#'
third.step.exhaustive.search <- function(data, q, max.iteration = 1000, tol = tol, pts.list,
                              an, phi.est.full = NULL, phi.local.1 = NULL, phi.local.2 = NULL,
                              blocks = NULL){

    N <- length(data[,1])
    p <- length(data[1,])
    n <- length(pts.list)  # number of cluster
    n.new <- length(phi.est.full)

    final.pts <- rep(0, n)
    pts.list.full <- pts.list
    pts.list.full <- c(1, pts.list.full, N)
    phi.hat.list <- vector("list", n + 1)

    cp.index.list <- vector("list", n + 2)
    cp.index.list[[1]] <- c(1)
    cp.index.list[[n+2]] <- c(n.new + 1)

    cp.list.full <- vector("list", n+2)
    cp.list.full[[1]] <- c(1)
    cp.list.full[[n+2]] <- c(N + 1)

    bn = median(diff(blocks))
    for(i in 1:n){
        pts.temp <- pts.list.full[[i+1]]
        m <- length(pts.temp)
        if(m <= 1){
            cp.list.full[[i+1]] <- c((pts.temp - bn + 1):(pts.temp + bn - 1))
            cp.index.list[[i+1]] <- sapply(1:m, function(jjj) which.min(abs(pts.temp[jjj] - blocks)))
        }else{
            cp.list.full[[i+1]] <- c((round(median(pts.temp)) - bn + 1):(round(median(pts.temp)) + bn - 1))
            cp.index.list[[i+1]] <- sapply(1:m, function(jjj) which.min(abs(pts.temp[jjj] - blocks)))
        }
    }
    fx <- function(i){
        idx <- floor((min(cp.index.list[[i+1]]) + max(cp.index.list[[i]])) / 2)

        # change the global variable phi.hat.list[[i]]
        phi.hat.list[[i]] <<- phi.est.full[[idx]]
        pts.temp <- pts.list.full[[i+1]]
        m <- length(pts.temp)
        lb.1 <- min(pts.temp) - an
        ub.2 <- max(pts.temp) +  an - 1

        nums = cp.list.full[[i+1]]
        phi.hat.1 = matrix(phi.local.1[[i]], ncol = p * q)
        phi.hat.2 = matrix(phi.local.2[[i]], ncol = p * q)
        res = local_refine(data, q = q, blocks, cp.list.full[[i+1]], lb.1, ub.2, phi.hat.1, phi.hat.2)
        sse.full = res$sse_full

        # select the point that has the smallest SSE among the cluster
        cp.list.full[[i+1]][min(which(sse.full == min(sse.full)))]

    }
    final.pts <- sapply(1:n, fx)
    idx <- floor((min(cp.index.list[[n+2]]) + max(cp.index.list[[n+1]])) / 2)
    phi.hat.list[[n+1]] <- phi.est.full[[idx]]
    return(list(pts = final.pts, phi.hat.list = phi.hat.list))
}



#' Select the lag of the VAR model using total BIC method
#'
#' @description Select the lag of the VAR model (if the lag is unknown) using BIC method for total segments
#'
#' @param data input data matrix, each column represents the time series component
#' @param method method is sparse, group sparse and fixed lowrank plus sparse
#' @param group.case two different types of group sparse, column-wise and row-wise, respectively.
#' @param group.index specify group sparse index. Default is NULL.
#' @param lambda.1.cv tuning parameter lambda_1 for fused lasso
#' @param lambda.2.cv tuning parameter lambda_2 for fused lasso
#' @param mu tuning parameter for low rank component, only available when method is set to "fLS".
#' @param block.size the block size
#' @param blocks the blocks
#' @param use.BIC use BIC for k-means part
#' @param an.grid a vector of an for grid searching.
#' @param threshold a numeric argument, give the threshold for estimated model parameter matrices. Default is NULL.
#' @param lag_candidates potential lag selection set
#' @param verbose A Boolean argument, if TRUE, it provides detailed information. Default is FALSE
#' @return selected lag for VAR series
#' \describe{
#'     \item{select_lag}{An integer no less than 1 represents the selected lag of time series.}
#' }
#'
#' @import sparsevar
#' @examples
#' \donttest{
#' nob <- 1000; p <- 15
#' brk <- c(floor(nob / 2), nob + 1)
#' m <- length(brk)
#' q.t <- 2 # the lag of VAR model for simulation
#' signals <- c(-0.8, 0.6, 0.4)
#' try <- simu_var(method = "sparse", nob = nob, k = p, brk = brk,
#'                 signals = signals, lags_vector = c(1, 2),
#'                 sp_pattern = "off-diagonal")
#' data <- try$series; data <- as.matrix(data)
#'
#' # Apply lag selection to determine the lag for the given time series
#' lag_candi <- c(1, 2, 3, 4)
#' select_lag <- lag_selection(data = data,
#'                             method = "sparse", lag_candidates = lag_candi)
#' print(select_lag)
#' }
#' @export
lag_selection <- function(data, method = c("sparse", "group sparse", "fLS"),
                          group.case = c("columnwise", "rowwise"), group.index = NULL,
                          lambda.1.cv = NULL, lambda.2.cv = NULL, mu = NULL,
                          block.size = NULL, blocks = NULL,
                          use.BIC = TRUE, an.grid = NULL, threshold = NULL,
                          lag_candidates, verbose = FALSE){
    nob <- length(data[,1])
    p <- length(data[1,])

    BIC_full <- rep(0, length(lag_candidates))
    for(i in 1:length(lag_candidates)){
        d <- lag_candidates[i]
        fit <- tbss(
            data = data,
            method = method,
            group.case = group.case,
            group.index = group.index,
            lambda.1.cv = lambda.1.cv,
            lambda.2.cv = lambda.2.cv,
            mu = mu,
            q = d,
            block.size = block.size,
            blocks = blocks,
            use.BIC = use.BIC,
            an.grid = an.grid,
            refit = TRUE,
            verbose = verbose
        )
        sparse_mats <- fit$sparse_mats
        cp_est <- fit$cp
        cp_full <- c(1, cp_est, nob + 1)
        BIC <- 0
        for(j in 1:(length(cp_est) + 1)){
            data_temp <- as.matrix(data[(cp_full[j]):(cp_full[j+1]-1), ])
            n_temp <- dim(data_temp)[1]
            sparse_mat_temp <- sparse_mats[[j]]
            residual <- c()
            for(t in ((d + 1):n_temp)){
                y_pred <- 0
                for(dd in 1:d){
                    phi <- sparse_mat_temp[, ((dd - 1) * p + 1):(dd * p)]
                    if(length(threshold)){
                        phi[abs(phi) <= threshold] <- 0
                    }
                    y_pred <- y_pred + phi %*% (data_temp[t - dd, ])
                }
                residual <- cbind(residual, data_temp[t, ] - y_pred)
            }
            sigma.hat <- 0 * diag(p)
            for(t in 1:(n_temp - d)){
                sigma.hat <- sigma.hat + residual[, t] %*% t(residual[, t])
            }
            sigma.hat <- (1 / (n_temp - d)) * sigma.hat
            log.det <- log(det(sigma.hat))
            count <- sum(sparse_mat_temp != 0)
            BIC <- BIC + log.det + log((n_temp - d)) * count / (n_temp - d)
        }
        BIC_full[i] <- BIC
    }
    select_lag <- lag_candidates[which.min(BIC_full)]
    return(select_lag)
}



#' cluster the points by neighborhood size a_n
#' @description helper function for determining the clusters
#'
#' @param pts vector of candidate change points
#' @param an radius of the cluster
#' @return A list of change points clusters
#' @keywords internal
#'
block.finder <- function(pts, an){
    nn <- length(pts)
    if(nn == 1){
        b <- pts
    }
    if(nn > 1){
        b <- vector("list", nn)
        i.ind <- 1
        jj <- 0
        while(i.ind < nn){
            ct <- 1
            jj <- jj + 1
            for(j in (i.ind + 1):nn){
                if(abs(pts[i.ind] - pts[j]) <= an){
                    ct <- ct + 1
                }
            }
            b[[jj]] <- pts[(i.ind):(i.ind + ct - 1)]
            i.ind <- i.ind + ct
        }
        l <- length(b[[jj]])
        if(b[[jj]][l] != pts[nn]){
            jj <- jj + 1
            b[[jj]] <- c(pts[nn])
        }
        b <- b[1:jj]
    }
    return(b = b)
}



#' Prediction function (block)
#'
#' @param Y data for prediction
#' @param phi parameter matrix
#' @param q lag
#' @param nob total length of data
#' @param p the dimension of time series components
#' @param h the length of observation to predict
#' @return prediction matrix
#' @keywords internal
#'
pred.block <- function(Y, phi, q, nob, p, h){
    concat.Y <- matrix(0, p, q + h)
    concat.Y[,1:q] <- Y[,(nob - q + 1):nob]
    for(j in 1:h){
        temp <- matrix(0, p, 1)
        for (i in 1:q){
            temp <- temp + phi[,((i - 1) * p + 1):(i * p)] %*% concat.Y[,q + j - i]
        }
        concat.Y[,q + j] <- temp
    }
    return(as.matrix(concat.Y[,(q + 1):(q + h)]))
}


#' Prediction function (single observation)
#' @param Y data for prediction
#' @param phi parameter matrix
#' @param q lag
#' @param nob total length of data
#' @param p the dimension of time series components
#' @param h the h-th observation to predict
#' @return prediction matrix
#' @keywords internal
#'
pred <- function(Y, phi, q, nob, p, h){
    concat.Y <- matrix(0, p, q + h)
    concat.Y[,1:q] <- Y[,(nob - q + 1):nob]
    for(j in 1:h){
        temp <- matrix(0, p, 1)
        for (i in 1:q){
            temp <- temp + phi[,((i-1) * p + 1):(i * p)] %*% concat.Y[,q + j - i]
        }
        concat.Y[,q + j] <- temp
    }
    return(as.matrix(concat.Y[,q + h]))
}

#' Plot the AR coefficient matrix
#' @param phi combined coefficient matrices for all lags
#' @param p number of segments times number of lags
#' @return a plot of AR coefficient matrix
#' @import lattice
#' @importFrom grDevices colorRampPalette
#' @export
#' @examples
#' nob <- 4 * 10^3
#' p <- 15
#' brk <- c(floor(nob / 3), floor(2 * nob / 3), nob + 1)
#' m0 <- length(brk) - 1
#' q.t <- 2
#' m <- m0 + 1
#' sp_density <- rep(0.05, m*q.t) #sparsity level (5%)
#' try <- simu_var("sparse", nob = nob, k = p, lags = q.t, brk = brk,
#'                 sp_pattern = "random", sp_density = sp_density)
#' print(plot_matrix(do.call("cbind", try$model_param), m * q.t))
#'
plot_matrix <- function(phi, p){
    name <- NULL
    B <- phi
    if(nrow(B) == 1){
        B <- matrix(B[, 1:ncol(B)], nrow = 1)
    }else{
        B <- B[, 1:ncol(B)]
    }
    k <- nrow(B)
    s1 <- 0
    m <- 0
    s <- 0
    s <- s + s1
    text <- c()
    for(i in 1:p){
        text1 <- as.expression(bquote(bold(Phi)^(.(i))))
        text <- append(text, text1)
    }
    if(m > 0){
        for(i in (p + 1):(p + s + 1)){
            text1 <- as.expression(bquote(bold(beta)^(.(i - p - s1))))
            text <- append(text, text1)
        }
    }
    f <- function(m) t(m)[, nrow(m):1]
    rgb.palette <- colorRampPalette(c("blue", "white", "red"), space = "Lab")
    at <- seq(k / 2 + 0.5, p * k + 0.5, by = k)
    if(m > 0){
        at2 <- seq(p * k + m / 2 + 0.5, p * k + s * m + 0.5, by = m)
    }else{
        at2 = c()
    }
    at <- c(at, at2)
    se2 = seq(1.75, by = k, length = k)
    L2 <- levelplot(as.matrix(f(B)), at =seq( -max(abs(B)), max(abs(B)), length=101),col.regions = rgb.palette(100),
                    colorkey = NULL, xlab = NULL, ylab = NULL, main = list(label = name, cex = 1),
                    panel = function(...){
                        panel.levelplot(...)
                        panel.abline(a = NULL, b = 1, h = seq(1.5, m * s + p * k + 0.5, by = 1), v = seq(1.5, by = 1, length = p * k + m * s), lwd = 0.5)
                        bl1 <- seq(k + 0.5, p * k + 0.5, by = k)
                        b23 <- seq(p * k + 0.5, p * k + 0.5 + s * m, by = m)
                        b1 <- c(bl1, b23)
                        panel.abline(a = NULL, b = 1, v = p * k + 0.5, lwd = 3)
                        panel.abline(a = NULL, b = 1, v = b1, lwd = 2)
                    }, scales = list(x = list(alternating = 1, labels = text, cex = 2, at = at, tck = c(0, 0)),
                                     y = list(alternating = 0, tck = c(0, 0))))
    return(L2)
}


#' helper function for detection check
#' @param pts the estimated change points
#' @param brk the true change points
#' @return a vector of time points
#' @keywords internal
#'
remove.extra.pts <- function(pts, brk){
    m.hat <- length(brk) - 1
    if(length(pts) <= m.hat){
        return(pts)
    }
    pts.temp <- rep(0, m.hat)
    for(i in 1:m.hat){
        origin <- brk[i]
        dis <- rep(0, length(pts))
        for(j in 1:length(pts)){
            dis[j] <- abs(origin - pts[j])
        }
        ll <- min(which.min(dis))
        pts.temp[i] <- pts[ll]
    }

    pts <- pts.temp
    return(pts)
}




#' A helper function for implementing FISTA algorithm to estimate low-rank matrix
#'
#' @description Function to estimate low-rank matrix using FISTA algorithm
#' @param A A n by p design matrix
#' @param b A correspond vector, or a matrix
#' @param lambda tuning parameter
#' @param d model dimension
#' @param niter the maximum number of iterations required for applying FISTA algorithm
#' @param backtracking a boolean argument, indicate whether use backtracking or not
#' @param phi.true true model parameter, only available for simulations
#' @return A list object, including
#' \describe{
#'     \item{phi.hat}{Estimated low-rank matrix}
#'     \item{obj.vals}{Values of objective function for all iterations}
#'     \item{rel.err}{Relative error to the true model parameter, only available for simulation}
#' }
#' @keywords internal
#'
fista.nuclear <- function(A, b, lambda, d, niter, backtracking = TRUE, phi.true){
    tnew = t <- 1
    x <- matrix(0, d, d)
    xnew <- x
    y <- x
    AtA <- t(A) %*% A
    Atb <- t(A) %*% b

    obj.val = rel.err <- c()
    if(backtracking == TRUE){
        L <- norm(A, "2")^2 / 5
        eta <- 2
    }else{
        L <- norm(A, "2")^2
    }
    for(i in 1:niter){
        if(backtracking == TRUE){
            L.bar <- L
            flag <- FALSE
            while(flag == FALSE){
                prox <- prox.nuclear.func.fLS(y, A, b, L.bar, lambda, AtA, Atb)
                if(f.func(prox, A, b) <= Q.func(prox, y, A, b, L.bar, AtA, Atb)){
                    flag <- TRUE
                }else{
                    L.bar <- L.bar * eta
                }
            }
            L <- L.bar
        }
        x <- xnew
        xnew <- prox
        t <- tnew
        tnew <- (1 + sqrt(1 + 4 * t^2)) / 2
        y <- xnew + ((t - 1) / tnew) * (xnew - x)

        obj.val <- c(obj.val, f.func(xnew, A, b) + nuclear.pen(xnew, lambda))
        rel.err <- c(rel.err, norm(xnew - phi.true, "F") / norm(phi.true, "F"))
    }
    return(list(phi.hat = xnew, obj.vals = obj.val, rel.err = rel.err))
}


#' Proximal function for nuclear norm penalty
#' @param y model parameter
#' @param A design matrix
#' @param b correspond vector, or matrix
#' @param L learning rate
#' @param lambda tuning parameter
#' @param AtA Gram matrix obtained by design matrix
#' @param Atb inner product for design matrix A and correspond vector b
#' @return value of proximal function
#' @keywords internal
#'
prox.nuclear.func.fLS <- function(y, A, b, L, lambda, AtA, Atb){
    Y <- y - (1 / L) * gradf.func(y, AtA, Atb)
    d <- shrinkage.lr(svd(Y)$d, 2*lambda / L)
    return(svd(Y)$u %*% diag(d) %*% t(svd(Y)$v))
}


#' Function for detection performance check
#' @param pts.final a list of estimated change points
#' @param brk the true change points
#' @param nob length of time series
#' @param critval critical value for selection rate. Default value is 5. Specifically, to compute the selection rate,  a selected break point is counted as a ``success'' for the \eqn{j}-th true break point, \eqn{t_j}, if it falls in the interval \eqn{[t_j - {(t_{j} - t_{j-1})}/{critval}, t_j + {(t_{j+1} - t_{j})}/{critval}]}, \eqn{j = 1,\dots, m_0}.
#' @return a matrix of detection summary results, including the absolute error, selection rate and relative location. The absolute error of the locations of the estimated break points is defined as \eqn{{error}_j =|\tilde{t}_j^f - t_j|}, \eqn{j = 1,\dots, m_0}.
#' @export
#' @examples
#' # an example of 10 replicates result
#' set.seed(1)
#' nob <- 1000
#' brk <- c(333, 666, nob+1)
#' cp.list <- vector('list', 10)
#' for(i in 1:10){
#'     cp.list[[i]] <-  brk[1:2] + sample(c(-50:50),1)
#' }
#' # some replicate fails to detect all the change point
#' cp.list[[2]] <- cp.list[[2]][1]
#' cp.list[4] <- list(NULL)      # setting 4'th element to NULL.
#' # some replicate overestimate the number of change point
#' cp.list[[3]] <- c(cp.list[[3]], 800)
#' cp.list
#' res <- detection_check(cp.list, brk, nob, critval = 5)
#' res
#' # use a stricter critical value
#' res <- detection_check(cp.list, brk, nob, critval = 10)
#' res
#'
detection_check <- function(pts.final, brk, nob, critval = 5){
    N <- length(pts.final)
    m <- length(brk)
    len <- rep(0, N)
    for(i in 1:N){
        len[i] <- length(pts.final[[i]])
    }
    pts.final.full.1 <- vector("list", N)
    pts.final.full.2 <- vector("list", N)
    for(i in 1:N){
        if(length(pts.final[[i]]) > (m - 1)){
            pts.final[[i]] <- remove.extra.pts(pts.final[[i]], brk)
        }
        if(length(pts.final[[i]]) == 0){
            # pts.final.full.1 only counts those points within 1 / critval intervals in order to compute the selection rate
            # pts.final.full.2 counts all the points in order to calculate the mean and sd for the location error
            pts.final[[i]] <- rep(0, m - 1)
            pts.final.full.1[[i]] <- rep(0, m - 1)
            pts.final.full.2[[i]] <- rep(0, m - 1)
        }
        if((length(pts.final[[i]]) > 0) && (length(pts.final[[i]]) <= m-1)){
            ll <- length(pts.final[[i]])
            pts.final.full.1[[i]] <- rep(0, m - 1)
            pts.final.full.2[[i]] <- rep(0, m - 1)
            for(j in 1:ll){
                if(m == 2){
                    if(pts.final[[i]][j] < (brk[1] + (1 / critval) * (brk[2] - brk[1])) && pts.final[[i]][j] >= (brk[1] - (1 / critval) * (brk[1]))){
                        pts.final.full.1[[i]][1] <- pts.final[[i]][j]
                    }
                    if(pts.final[[i]][j] < (brk[1] + (1/2) * (brk[2] - brk[1]))){
                        pts.final.full.2[[i]][1] <- pts.final[[i]][j]
                    }
                }else{
                    if(pts.final[[i]][j] < (brk[1] + (1 / critval) * (brk[2] - brk[1])) && pts.final[[i]][j] >= (brk[1] - (1 / critval) * (brk[1]))){
                        pts.final.full.1[[i]][1] <- pts.final[[i]][j]
                    }
                    if(pts.final[[i]][j] < (brk[1] + (1/2) * (brk[2] - brk[1]))){
                        pts.final.full.2[[i]][1] <- pts.final[[i]][j]
                    }

                    for(kk in 2:(m-1)){
                        if(pts.final[[i]][j] >= (brk[(kk)] - (1 / critval)*(brk[kk] - brk[(kk-1)])) && pts.final[[i]][j] < (brk[(kk)] + (1 / critval) * (brk[kk+1] - brk[(kk)]))){
                            pts.final.full.1[[i]][kk] <- pts.final[[i]][j]
                        }
                        if(kk == m-1){
                            if(pts.final[[i]][j] >= (brk[(kk)] - (1/2) * (brk[kk] - brk[(kk-1)]))){
                                pts.final.full.2[[i]][kk] <- pts.final[[i]][j]
                            }
                        }else{
                            if(pts.final[[i]][j] >= (brk[(kk)] - (1/2) * (brk[kk] - brk[(kk-1)])) && pts.final[[i]][j] < (brk[(kk)] + (1/2) * (brk[kk+1] - brk[(kk)]))){
                                pts.final.full.2[[i]][kk] <- pts.final[[i]][j]
                            }
                        }
                    }
                }
            }
        }
    }

    # detection result matrix
    detection <- matrix(0, m, 7)
    detection[1, 1] <- c("break points")
    detection[1, 2] <- c("truth")
    detection[1, 3] <- c("mean(absolute errors)")
    detection[1, 4] <- c("std(absolute errors)")
    detection[1, 5] <- c("selection rate")
    detection[1, 6] <- c("mean(relative location)")
    detection[1, 7] <- c("std(relative location)")
    for(i in 1:(m-1)){
        detection[(i+1), 1] <- c(i)
        detection[(i+1), 2] <- c(brk[i])
        loc.1 <- rep(0, N)
        loc.2 <- rep(0, N)
        for(j in 1:N){
            temp.1 <- pts.final.full.1[[j]]
            loc.1[j] <- temp.1[i]
            temp.2 <- pts.final.full.2[[j]]
            loc.2[j] <- temp.2[i]
        }
        loc.1 <- loc.1[which(loc.1 != 0)]
        loc.2 <- loc.2[which(loc.2 != 0)]
        nob.new.1 <- length(loc.1)
        detection[(i+1), 3] <- mean(abs(loc.2 - brk[i]))
        detection[(i+1), 4] <- sd(abs(loc.2 - brk[i]))
        detection[(i+1), 5] <- nob.new.1 / N
        detection[(i+1), 6] <- mean(loc.2 / nob)
        detection[(i+1), 7] <- sd(loc.2 / nob)
    }

    for(i in 2:(m)){
        for(j in 2:7){
            detection[i, j] <- round(as.numeric(detection[i, j]), digits = 4)
        }
    }
    df.result <- data.frame(truth = round(as.numeric(detection[-1, 2]) / nob, 4),
                            mean_rl = round(as.numeric(detection[-1, 6]), 4),
                            std_rl = round(as.numeric(detection[-1, 7]), 4),
                            sr = round(as.numeric(detection[-1, 5]), 4))

    colnames(df.result) <- c("Truth", "Mean", "Std", "Selection rate")
    return(list(df_detection = df.result, full_detection = detection))
}


#' Function for Hausdorff distance computation
#'
#' @description The function includes two Hausdorff distance.
#' The first one is hausdorff_true_est (\eqn{d(A_n, \tilde{A}_n^f)}): for each estimated change point, we find the closest true CP and compute the distance, then take the maximum of distances.
#' The second one is hausdorff_est_true(\eqn{d(\tilde{A}_n^f, A_n)}): for each true change point, find the closest estimated change point and compute the distance, then take the maximum of distances.
#'
#' @param pts.final a list of estimated change points
#' @param brk the true change points
#' @return Hausdorff distance summary results, including mean, standard deviation and median.
#' @importFrom stats sd
#' @importFrom stats median
#' @export
#' @examples
#' ## an example of 10 replicates result
#' set.seed(1)
#' nob <- 1000
#' brk <- c(333, 666, nob+1)
#' cp.list <- vector('list', 10)
#' for(i in 1:10){
#'     cp.list[[i]] <-  brk[1:2] + sample(c(-50:50),1)
#' }
#' # some replicate fails to detect all the change point
#' cp.list[[2]] <- cp.list[[2]][1]
#' cp.list[4] <- list(NULL)      # setting 4'th element to NULL.
#' # some replicate overestimate the number of change point
#' cp.list[[3]] <- c(cp.list[[3]], 800)
#' cp.list
#' res <- hausdorff_check(cp.list, brk)
#' res
#'
hausdorff_check <- function(pts.final, brk){
    N <- length(pts.final)
    m <- length(brk)
    len <- rep(0,N)
    for(i in 1:N){
        len[i] <- length(pts.final[[i]])
    }

    # Implementation of hausdorff_true_est d(A_n, \tilde{A}_n^f)
    # for each estimated cp, find the closest true CP and compute the distance,
    # then take the maximum of distances
    pts.final.full.1 <- vector("list", N)
    for(i in 1:N){
        ll <- length(pts.final[[i]])
        pts.final.full.1[[i]] <- rep(NA, ll)
        if(ll>0){
            for(j in 1:ll){
                if(pts.final[[i]][j] < (brk[1] + (1/2) * (brk[2] - brk[1]))){
                    pts.final.full.1[[i]][j] <- brk[1]
                }
                if(m - 2 > 0){
                    if(pts.final[[i]][j] >= (brk[m-2] + (1/2) * (brk[m-1] - brk[m-2]))){
                        pts.final.full.1[[i]][j] <- brk[m-1]
                    }
                }
                if(m - 2 >= 2){
                    for(kk in 2:(m-2)){
                        if(pts.final[[i]][j] >= (brk[(kk-1)] + (1/2) * (brk[kk] - brk[(kk-1)])) && pts.final[[i]][j] < (brk[(kk)] + (1/2) * (brk[kk+1] - brk[(kk)]))){
                            pts.final.full.1[[i]][j] <- brk[kk]
                        }
                    }
                }
            }
        }else{
            cat("There is no estimated break points!")
        }
    }

    # Implementation of Hausdorff_est_true d(\tilde{A}_n^f, A_n)
    # for each true change point, find the closest estimate CP and compute the distance,
    # then take the maximum of distances
    pts.final.full.2 <- vector("list", N)
    for(i in 1:N){
        ll <- length(pts.final[[i]])
        pts.final.full.2[[i]] <- rep(NA, m - 1)
        if(ll > 0){
            if(ll > 1){
                for(j in 1:(m-1)){
                    if(brk[j] < (pts.final[[i]][1] + (1/2) * (pts.final[[i]][2] - pts.final[[i]][1]))){
                        pts.final.full.2[[i]][j] <- pts.final[[i]][1]
                    }
                    if(brk[j] >= (pts.final[[i]][ll-1] + (1/2) * (pts.final[[i]][ll] - pts.final[[i]][ll-1]))){
                        pts.final.full.2[[i]][j] <- pts.final[[i]][ll]
                    }
                    if(ll >= 3){
                        for(kk in 2:(ll-1)){
                            if(brk[j] >= (pts.final[[i]][(kk-1)] + (1/2) * (pts.final[[i]][kk] - pts.final[[i]][(kk-1)]))
                               && brk[j] < (pts.final[[i]][(kk)] + (1/2) * (pts.final[[i]][kk+1] - pts.final[[i]][(kk)]))){
                                pts.final.full.2[[i]][j] <- pts.final[[i]][kk]
                            }
                        }
                    }
                }
            }else{
                pts.final.full.2[[i]] <- rep(pts.final[[i]], m - 1)
            }
        }
    }

    distance.1 <- rep(NA, N)
    distance.2 <- rep(NA, N)
    distance.full <- rep(NA, N)
    for(j in 1:N){
        temp.1 <- pts.final.full.1[[j]]
        if(length(temp.1) > 0){
            distance.1[j] <- max(abs(pts.final.full.1[[j]] - pts.final[[j]]))
        }
        temp.2 <- pts.final.full.2[[j]]
        distance.2[j] <- max(abs(pts.final.full.2[[j]] - brk[1:(m-1)]))
        distance.full[j] <- max(distance.1[j], distance.2[j])
    }

    Mean <- mean(distance.full, na.rm = TRUE)
    Std <- sd(distance.full, na.rm = TRUE)
    Median <- median(distance.full, na.rm = TRUE)
    detection <- data.frame(Mean, Std, Median)

    return(distance = detection)
}


#' Evaluation function, return the performance of simulation results
#' @param true_mats a list of true matrices for all segments, the length of list equals to the true number of segments
#' @param est_mats a list of estimated matrices for all simulation replications, for each element, it is a list of numeric matrices,
#' representing the estimated matrices for segments
#' @return A list, containing the results for all measurements
#' \describe{
#'     \item{sensitivity}{A numeric vector, containing all the results for sensitivity over all replications}
#'     \item{specificity}{A numeric vector, including all the results for specificity over all replications}
#'     \item{accuracy}{A numeric vector, the results for accuracy over all replications}
#'     \item{mcc}{A numeric vector, the results for Matthew's correlation coefficients over all replications}
#'     \item{false_reps}{An integer vector, recording all the replications which falsely detects the change points, over-detect or under-detect}
#' }
#' @export
#' @examples
#' true_mats <- vector('list', 2)
#' true_mats[[1]] <- matrix(c(1, 0, 0.5, 0.8), 2, 2, byrow = TRUE)
#' true_mats[[2]] <- matrix(c(0, 0, 0, 0.75), 2, 2, byrow = TRUE)
#' est_mats <- vector('list', 5)
#' for(i in 1:5){
#'     est_mats[[i]] <- vector('list', 2)
#'     est_mats[[i]][[1]] <- matrix(sample(c(0, 1, 2), size = 4, replace = TRUE), 2, 2, byrow = TRUE)
#'     est_mats[[i]][[2]] <- matrix(sample(c(0, 1), size = 4, replace = TRUE), 2, 2, byrow = TRUE)
#' }
#' perf_eval <- eval_func(true_mats, est_mats)
eval_func <- function(true_mats, est_mats){
    true_length <- length(true_mats)
    reps <- length(est_mats)
    incorrect_rep <- c()
    SEN = SPC = ACC = MCC <- c()
    for(rep in 1:reps){
        est_mat <- est_mats[[rep]]
        if(length(est_mat) != true_length){
            incorrect_rep <- c(incorrect_rep, rep)
        }else{
            true_seg_mats <- do.call("cbind", true_mats)
            est_seg_mats <- do.call("cbind", est_mat)
            nr <- nrow(true_seg_mats)
            nc <- ncol(true_seg_mats)
            tp = fn = tn = fp <- 0
            for(i in 1:nr){
                for(j in 1:nc){
                    if(true_seg_mats[i,j] != 0){
                        if(est_seg_mats[i,j] != 0){
                            tp <- tp + 1
                        }else if(est_seg_mats[i,j] == 0){
                            fn <- fn + 1
                        }
                    }else if(true_seg_mats[i,j] == 0){
                        if(est_seg_mats[i,j] != 0){
                            fp <- fp + 1
                        }else if(est_seg_mats[i,j] == 0){
                            tn <- tn + 1
                        }
                    }
                }
            }
            SEN <- c(SEN, tp / (tp + fn))
            SPC <- c(SPC, tn / (fp + tn))
            ACC <- c(ACC, (tp + tn) / (tp + tn + fp + fn))
            MCC <- c(MCC, (tp*tn - fp*fn) / sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)))
        }
    }
    eval_mat <- data.frame("SEN" = c(round(mean(SEN), 4), round(sd(SEN), 4)),
                           "SPC" = c(round(mean(SPC), 4), round(sd(SPC), 4)),
                           "ACC" = c(round(mean(ACC), 4), round(sd(ACC), 4)),
                           "MCC" = c(round(mean(MCC), 4), round(sd(MCC), 4)))
    rownames(eval_mat) <- c("Mean", "Std")
    return(list(perf_eval = eval_mat, false_reps = incorrect_rep))
}



#' Function to plot Granger causality networks
#' @description A function to plot Granger causal network for each segment via estimated sparse component. Note that if it has multiple lags, it only provides the first order Granger causality plot.
#' @param est_mats A list of numeric sparse matrices, indicating the estimated sparse components for each segment
#' @param threshold A numeric positive value, used to determine the threshold to present the edges
#' @param layout A character string, indicates the layout for the igraph plot argument
#' @return A series of plots of Granger networks of VAR model parameters
#' @importFrom igraph plot.igraph
#' @importFrom igraph graph.adjacency
#' @importFrom igraph layout_as_star
#' @importFrom igraph layout_nicely
#' @importFrom igraph layout_in_circle
#' @importFrom igraph V
#' @export
#' @examples
#' set.seed(1)
#' est_mats <- list(matrix(rnorm(400, 0, 1), 20, 20))
#' plot_granger(est_mats, threshold = 2, layout = "circle")
#' plot_granger(est_mats, threshold = 2, layout = "star")
#' plot_granger(est_mats, threshold = 2, layout = "nicely")
plot_granger <- function(est_mats, threshold = 0.1, layout){
    layout_options <- c("circle", "star", "nicely")
    if(!(layout %in% layout_options)){
        stop("Layouts are only available for circle, star, and nicely!!")
    }
    seg_length <- length(est_mats)
    for(i in 1:seg_length){
        est_mat <- est_mats[[i]]
        k <- nrow(est_mat)
        adj_mat <- matrix(0, k, k)
        for(r in 1:k){
            for(c in 1:k){
                if(abs(est_mat[r,c]) > threshold){
                    adj_mat[r,c] <- 1
                }
            }
        }
        varnames <- rownames(est_mat)
        colnames(adj_mat) = rownames(adj_mat) <- varnames
        net <- graph.adjacency(adj_mat, "directed", diag = FALSE)
        if(layout == "circle"){
            l <- layout_in_circle(net)
            plot.igraph(net, vertex.label = V(net)$name, layout = l, vertex.label.font = 2,
                        vertex.label.color = "black", edge.color = "gray40", edge.arrow.size = 0.1,
                        vertex.shape ="circle", vertex.color = "light blue", vertex.label.cex = 0.8)
        }else if(layout == "star"){
            l <- layout_as_star(net)
            plot.igraph(net, vertex.label = V(net)$name, layout = l, vertex.label.font = 2,
                        vertex.label.color = "black", edge.color = "gray40", edge.arrow.size = 0.1,
                        vertex.shape ="circle", vertex.color = "light blue", vertex.label.cex = 0.8)
        }else if(layout == "nicely"){
            l <- layout_nicely(net)
            plot.igraph(net, vertex.label = V(net)$name, layout = l, vertex.label.font = 2,
                        vertex.label.color = "black", edge.color = "gray40", edge.arrow.size = 0.1,
                        vertex.shape ="circle", vertex.color = "light blue", vertex.label.cex = 0.8)
        }
    }
}


#' Function to plot the sparsity levels for estimated model parameters
#' @description A function to plot lineplot for sparsity levels of estimated model parameters
#' @param est_mats A list of numeric matrices, the length of list equals to the number of estimated segments
#' @param threshold A numeric value, set as a threshold, the function only counts the non-zeros with absolute
#' magnitudes larger than threshold
#' @return A plot for sparsity density across over all estimated segments
#' @export
#' @examples
#' set.seed(1)
#' est_mats <- list(matrix(rnorm(400, 0, 2), 20, 20), matrix(rnorm(400), 20, 20))
#' plot_density(est_mats, threshold = 0.25)
plot_density <- function(est_mats, threshold = 0.1){
    nmats <- length(est_mats)
    density <- c()
    for(i in 1:nmats){
        mat <- est_mats[[i]]
        d <- 0
        for(r in 1:dim(mat)[1]){
            for(c in 1:dim(mat)[2]){
                if(abs(mat[r,c]) > threshold){
                    d <- d + 1
                }
            }
        }
        density <- c(density, d / (dim(mat)[1] * dim(mat)[2]))
    }
    plot(density, type = 'o')
}



#' Plotting the output from VARDetect.result class
#' @description Plotting method for S3 object of class \code{VARDetect.result}
#' @method plot VARDetect.result
#' @param x a \code{VARDetect.result} object
#' @param display a character string, indicates the object the user wants to plot; possible values are
#' \describe{
#'     \item{\code{"cp"}}{input time series together with the estimated change points}
#'     \item{\code{"param"}}{estimated model parameters}
#'     \item{\code{"granger"}}{present the model parameters through Granger causal networks}
#'     \item{\code{"density"}}{plot the sparsity levels across all segments}
#' }
#' @param threshold a positive numeric value, indicates the threshold to present the entries in the sparse matrices
#' @param layout a character string, indicating the layout of the Granger network
#' @param ... not in use
#' @importFrom graphics abline
#' @importFrom stats ts.plot
#' @importFrom utils data
#' @return A plot for change points or a series of plots for Granger causal networks for estimated model parameters
#' @examples
#' nob <- 1000
#' p <- 15
#' brk <- c(floor(nob / 3), floor(2 * nob / 3), nob + 1)
#' m <- length(brk)
#' q.t <- 1
#' try <- simu_var('sparse',nob=nob,k=p,lags=q.t,brk=brk,sp_pattern="off-diagonal",seed = 1)
#' data <- try$series
#' data <- as.matrix(data)
#' fit <- tbss(data, method = "sparse", q = q.t)
#' plot(fit, display = "cp")
#' plot(fit, display = "param")
#' plot(fit, display = "granger", threshold = 0.2, layout = "nicely")
#' plot(fit, display = "density", threshold = 0.2)
#' @export
plot.VARDetect.result <- function(x,
                                  display = c("cp", "param", "granger", "density"),
                                  threshold = 0.1,
                                  layout = c("circle", "star", "nicely"), ...){
    display <- match.arg(display)
    layout <- match.arg(layout)
    n <- dim(x$data)[1]
    p <- dim(x$data)[2]
    if(display == "cp"){
        if(p >= 15){
            MTS::MTSplot(x$data)
            abline(v = x$cp, col = "red", lwd = 2)
        }else{
            ts.plot(x$data)
            abline(v = x$cp, col = "red", lwd = 2)
        }

    }

    if(display == "param"){
        if(!is.null(x$est_phi)){
            print(plot_matrix(do.call("cbind", x$est_phi), length(x$est_phi) * x$q))
        }else{
            stop("Estimated model parameter is not available!!!")
        }
    }

    if(display == "granger"){
        plot_granger(x$sparse_mats, threshold = threshold, layout = layout)
    }

    if(display == "density"){
        plot_density(x$sparse_mats, threshold = threshold)
    }
}


#' Function to summarize the change points estimated by VARDetect
#' @description Summary method for objects of class \code{VARDetect.result}
#' @method summary VARDetect.result
#' @param object a \code{VARDetect.result} object
#' @param threshold A numeric positive value, used to determine the threshold of nonzero entries
#' @param ... not in use
#' @return A series of summary, including the estimated change points, running time
#' @examples
#' nob <- 1000
#' p <- 15
#' brk <- c(floor(nob / 3), floor(2 * nob / 3), nob + 1)
#' m <- length(brk)
#' q.t <- 1
#' try <- simu_var('sparse',nob=nob,k=p,lags=q.t,brk=brk,sp_pattern="off-diagonal",seed=1)
#' data <- try$series
#' data <- as.matrix(data)
#' fit <- tbss(data, method = "sparse", q = q.t)
#' summary(fit)
#' @export
summary.VARDetect.result <- function(object, threshold = 0.1, ...){
    ncp <- length(object$cp)
    if(is.null(object$est_phi)){
        cat("No change point is finally detected! \n")
    }else{
        cat("============================= Summary ============================\n")
        cat(paste("Detected", ncp, "change points, located at: \n", sep = " "))
        cat(object$cp)
        cat("\n")
        cat("==================================================================\n")
        sp_levels <- c()
        for(j in 1:length(object$sparse_mats)){
            mat <- object$sparse_mats[[j]]
            d <- 0
            for(r in 1:dim(mat)[1]){
                for(c in 1:dim(mat)[2]){
                    if(abs(mat[r,c]) > threshold){
                        d <- d + 1
                    }
                }
            }
            sp_levels <- c(sp_levels, d / (dim(mat)[1]*dim(mat)[2]))
        }
        cat("Sparsity levels for estimated sparse components are: \n")
        cat(sp_levels)
        cat("\n")
        cat("==================================================================\n")
        if(!is.null(object$lowrank_mats)){
            ranks <- c()
            for(j in 1:length(object$lowrank_mats)){
                mat <- object$lowrank_mats[[j]]
                ranks <- c(ranks, qr(mat)$rank)
            }
            cat("The ranks for the estimated low rank components are: \n")
            cat(ranks)
            cat("\n")
            cat("==================================================================\n")
        }else{
            cat("There is no low rank components in the current model! \n")
        }
    }

    # running time summary
    cat("==================================================================\n")
    cat("Running time is: ")
    cat("\n")
    cat(paste(round(object$time[3], 3), "seconds", sep = " "))
    cat("\n")
    cat("==================================================================\n")
}



#' Function to print the change points estimated by VARDetect
#' @description Print the estimated change points of class \code{VARDetect.result}
#' @param x a \code{VARDetect.result} class object
#' @param ... not in use
#' @return Print the estimated change points
#' @examples
#' nob <- 1000
#' p <- 15
#' brk <- c(floor(nob / 3), floor(2 * nob / 3), nob + 1)
#' m <- length(brk)
#' q.t <- 1
#' try <- simu_var('sparse',nob=nob,k=p,lags=q.t,brk=brk,sp_pattern="off-diagonal",seed=1)
#' data <- try$series
#' data <- as.matrix(data)
#' fit <- tbss(data, method = "sparse", q = q.t)
#' print(fit)
#' @export
print.VARDetect.result <- function(x, ...){
    if(!is.null(x$est_phi)){
        cat("Estimated change points are: ")
        cat(x$cp)
        cat("\n")
    }else{
        cat("There is no change point detected! \n")
    }
}




#' Simulation function for TBSS algorithm
#' @description Function for deploying simulation using TBSS algorithm
#' @param nreps A numeric integer number, indicates the number of simulation replications
#' @param simu_method the structure of time series: "sparse","group sparse", and "fLS"
#' @param nob sample size
#' @param k dimension of transition matrix
#' @param lags lags of VAR time series. Default is 1.
#' @param lags_vector a vector of lags of VAR time series for each segment
#' @param brk a vector of break points with (nob+1) as the last element
#' @param sparse_mats transition matrix for sparse case
#' @param group_mats transition matrix for group sparse case
#' @param group_index group index for group lasso.
#' @param group_type type for group lasso: "columnwise", "rowwise". Default is "columnwise".
#' @param sp_pattern a choice of the pattern of sparse component: diagonal, 1-off diagonal, random, custom
#' @param sp_density if we choose random pattern, we should provide the sparsity density for each segment
#' @param rank if we choose method is low rank plus sparse, we need to provide the ranks for each segment
#' @param info_ratio the information ratio leverages the signal strength from low rank and sparse components
#' @param signals manually setting signal for each segment (including sign)
#' @param singular_vals singular values for the low rank components
#' @param spectral_radius to ensure the time series is piecewise stationary.
#' @param sigma the variance matrix for error term
#' @param skip an argument to control the leading data points to obtain a stationary time series
#' @param est_method method: sparse, group sparse, and fixed low rank plus sparse. Default is sparse
#' @param group.case group sparse pattern: column, row.
#' @param group.index group index for group sparse case
#' @param lambda.1.cv tuning parameter lambda_1 for fused lasso
#' @param lambda.2.cv tuning parameter lambda_2 for fused lasso
#' @param mu tuning parameter for low rank component, only available when method is set to "fLS"
#' @param q the AR order
#' @param max.iteration max number of iteration for the fused lasso
#' @param tol tolerance for the fused lasso
#' @param block.size the block size
#' @param blocks the blocks
#' @param refit logical; if TRUE, refit the VAR model for parameter estimation. Default is FALSE.
#' @param use.BIC use BIC for k-means part
#' @param an.grid a vector of an for grid searching
#' @param verbose a Boolean argument; if TRUE, function provides detailed information. Default is FALSE
#' @return A S3 object of class, named \code{VARDetect.simu.result}
#' \describe{
#'     \item{est_cps}{A list of estimated change points, including all replications}
#'     \item{est_sparse_mats}{A list of estimated sparse components for all replications}
#'     \item{est_lowrank_mats}{A list of estimated low rank components for all replications}
#'     \item{est_phi_mats}{A list of estimated model parameters, transition matrices for VAR model}
#'     \item{running_times}{A numeric vector, containing all running times}
#' }
#' @export
#' @examples
#' \donttest{
#' nob <- 4000; p <- 15
#' brk <- c(floor(nob / 3), floor(2 * nob / 3), nob + 1)
#' m <- length(brk); q.t <- 1
#' sp_density <- rep(0.05, m * q.t)
#' signals <- c(-0.6, 0.6, -0.6)
#' try_simu <- simu_tbss(nreps = 3, simu_method = "sparse", nob = nob,
#'                       k = p, lags = q.t, brk = brk, sigma = diag(p),
#'                       signals = signals, sp_density = sp_density,
#'                       sp_pattern = "random", est_method = "sparse", q = q.t,
#'                       refit = TRUE)
#' }
simu_tbss <- function(nreps, simu_method = c("sparse", "group sparse", "fLS"), nob, k, lags = 1,
                      lags_vector = NULL, brk, sigma, skip = 50, group_mats = NULL,
                      group_type = c("columnwise", "rowwise"), group_index = NULL,
                      sparse_mats = NULL, sp_density = NULL, signals = NULL, rank = NULL, info_ratio = NULL,
                      sp_pattern = c("off-diagonal", "diagoanl", "random"),
                      singular_vals = NULL, spectral_radius = 0.9, est_method = c("sparse", "group sparse", "fLS"),
                      q = 1, tol = 1e-2, lambda.1.cv = NULL, lambda.2.cv = NULL, mu = NULL,
                      group.index = NULL, group.case = c("columnwise", "rowwise"), max.iteration = 100,
                      refit = FALSE, block.size = NULL, blocks = NULL, use.BIC = TRUE, an.grid = NULL, verbose = FALSE){

    # arguments type sanity check
    simu_method <- match.arg(simu_method)
    group_type <- match.arg(group_type)
    sp_pattern <- match.arg(sp_pattern)
    est_method <- match.arg(est_method)
    group.case <- match.arg(group.case)

    # storage variables
    est_cps <- vector('list', nreps)
    est_sparse_mats <- vector('list', nreps)
    est_lowrank_mats <- vector('list', nreps)
    est_phi_mats <- vector('list', nreps)
    running_times <- rep(0, nreps)

    # start fitting
    for(rep in 1:nreps){
        cat(paste("========================== Start replication:", rep, "==========================\n", sep = " "))
        try <- simu_var(method = simu_method, nob = nob, k = k, lags = lags, lags_vector = lags_vector,
                 brk = brk, sigma = sigma, skip = skip, group_mats = group_mats, group_type = group_type,
                 group_index = group_index, sparse_mats = sparse_mats, sp_pattern = sp_pattern,
                 sp_density = sp_density, signals = signals, rank = rank, info_ratio = info_ratio,
                 singular_vals = singular_vals, spectral_radius = spectral_radius, seed = rep)

        data <- as.matrix(try$series)
        fit <- tbss(data, method = est_method, q = q, tol = tol, lambda.1.cv = lambda.1.cv,
                    lambda.2.cv = lambda.2.cv, mu = mu, group.index = group.index,
                    group.case = group.case, max.iteration = max.iteration, refit = refit,
                    block.size = block.size, blocks = blocks, use.BIC = use.BIC, an.grid = an.grid,
                    verbose = verbose)
        est_cps[[rep]] <- fit$cp
        est_sparse_mats[[rep]] <- fit$sparse_mats
        est_lowrank_mats[[rep]] <- fit$lowrank_mats
        est_phi_mats[[rep]] <- fit$est_phi
        running_times[rep] <- fit$time[3]
        cat("========================== Finished ==========================\n")
    }

    if(simu_method == "fLS"){
        ret <- structure(list(sizes = c(nob, k),
                              true_lag = lags,
                              true_lagvector = lags_vector,
                              true_cp = brk,
                              true_sparse = try$sparse_param,
                              true_lowrank = try$lowrank_param,
                              true_phi = try$model_param,
                              est_cps = est_cps,
                              est_lags = q,
                              est_lagvector = q,
                              est_sparse_mats = est_sparse_mats,
                              est_lowrank_mats = est_lowrank_mats,
                              est_phi_mats = est_phi_mats,
                              running_times = running_times), class = "VARDetect.simu.result")
    }else{
        ret <- structure(list(sizes = c(nob, k),
                              true_lag = lags,
                              true_lagvector = lags_vector,
                              true_cp = brk,
                              true_sparse = try$sparse_param,
                              true_lowrank = NULL,
                              true_phi = try$model_param,
                              est_cps = est_cps,
                              est_lags = q,
                              est_lagvector = q,
                              est_sparse_mats = est_sparse_mats,
                              est_lowrank_mats = NULL,
                              est_phi_mats = est_phi_mats,
                              running_times = running_times), class = "VARDetect.simu.result")
    }
    return(ret)
}


#' A function to summarize the results for simulation
#' @description A function to summarize the results for simulation class \code{VARDetect.simu.result}
#' @param object A S3 object of class \code{VARDetect.simu.result}
#' @param critical A positive integer, set as the critical value defined in selection rate, to control the range of success, default is 5
#' @param ... not in use
#' @return A series of summary, including the selection rate, Hausdorff distance, and statistical measurements, running times
#' @examples
#' \donttest{
#' nob <- 4000; p <- 15
#' brk <- c(floor(nob / 3), floor(2 * nob / 3), nob + 1)
#' m <- length(brk); q.t <- 1
#' sp_density <- rep(0.05, m * q.t)
#' signals <- c(-0.6, 0.6, -0.6)
#' try_simu <- simu_tbss(nreps = 3, simu_method = "sparse", nob = nob,
#'                       k = p, lags = q.t, brk = brk, sigma = diag(p),
#'                       signals = signals, sp_density = sp_density,
#'                       sp_pattern = "random", est_method = "sparse",
#'                       q = q.t, refit = TRUE)
#' summary(try_simu, critical = 5)
#' }
#' @export
summary.VARDetect.simu.result <- function(object, critical = 5, ...){
    # loading variables
    reps <- length(object$est_cps)
    n <- object$sizes[1]
    p <- object$sizes[2]

    true_lag <- object$lags
    true_cp <- object$true_cp
    true_sparse <- object$true_sparse
    true_lowrank <- object$true_lowrank
    true_phi <- object$true_phi

    est_cps <- object$est_cps
    est_sparse <- object$est_sparse_mats
    est_lowrank <- object$est_lowrank_mats
    est_phi <- object$est_phi_mats

    times <- object$running_times

    # selection rate
    cat("========================== Selection rate: ==========================\n")
    select_rate <- detection_check(est_cps, true_cp, n, critval = critical)
    cat("Selection rate: \n")
    print(select_rate$df_detection)
    cat("\n")
    cat("=====================================================================\n")

    # Hausdorff distance
    cat("======================== Hausdorff distance: ========================\n")
    haus_dist <- hausdorff_check(est_cps, true_cp)
    cat("Hausdorff distance: \n")
    print(round(haus_dist, 4))
    cat("\n")
    cat("=====================================================================\n")

    # statistical measurements
    cat("====================== Statistical Measurment: ======================\n")
    res <- eval_func(true_phi, est_sparse)
    cat("Performance evaluation: \n")
    print(res$perf_eval)
    cat("\n")
    cat("Incorrect estimation replication: \n")
    print(res$false_reps)
    cat("\n")
    cat("=====================================================================\n")

    # computation summary
    cat("======================== Computational Time: ========================\n")
    cat("Averaged running time: \n")
    cat(round(mean(times), 4))
    cat(" seconds\n")
    cat("=====================================================================\n")
}
