#' Discrete application of clusGap - core function.
#'
#' Based on the implementation of the function found in the `cluster` R package.
#'
#' @param x A matrix object specifying category attributes in the columns and observations in the rows.
#' @param FUNcluster a function that accepts as first argument a matrix like `x`; second argument specifies number of `k` (k=>2) clusters
#' This function should return a list with a component named `cluster`, a vector of length `n=nrow(x)` of integers from `1:k` indicating observation cluster assignment.
#' Make sure `FUNcluster` and `Input2Alg` agree.
#' @param K.max Integer. Maximum number of clusters `k` to consider
#' @param value.range String, character vector or a list of character vectors with the length matching the number of columns (nQ) of the array.
#' A vector with all categories to consider when bootstrapping the null distribution sample (KS: Known Support option).
#' By DEFAULT vals=NULL, meaning unique range of categories found in the data will be used when drawing the null (DS: Data Support option).
#' If a character vector of categories is provided, these values would be used for the null distribution drawing across the array.
#' If a list with category character vectors is provided, it has to have the same number of columns as the input array. The order of list element corresponds to the array's columns.
#' @param verbose Integer or logical. Determines if “progress” output should be printed. The default prints one bit per bootstrap sample.
#' @param distName String. Name of categorical distance to apply.
#' Available distances: 'bhattacharyya', 'chisquare', 'cramerV', 'hamming' and 'hellinger'.
#' @param Input2Alg Specifies the kind of input provided to the algorithm function in `FUNcluster`.
#' For algorithms that only accept a distance matrix use `'distMatr'` option (default).
#' For algorithms that require the dataset and a prespecified distance function (e.g. `stats::dist`) use the `'distFun'` option.
#' This case the distance function is defined internally and determined by parameter `distName`.
#' @param B Number of bootstrap samples. By default B = nrow(x).
#' @param verbose Integer or logical. Determines whether progress output should printed while running. By DEFAULT one bit is printed per bootstrap sample.
#' @param useLog Logical. Use log function after estimating `W.k`. Following the original formulation `useLog=TRUE` by default.
#' @param ... optionally further arguments for `FUNcluster()`
#'
#' @return a matrix with K.max rows and 4 columns, named "logW", "E.logW", "gap", and "SE.sim",
#' where gap = E.logW - logW, and SE.sim correspond to the standard error of `gap`.

clusGapDiscr0 <- function (x,
                          FUNcluster,
                          K.max,
                          B = nrow(x),
                          value.range = "DS",
                          verbose = interactive(),
                          distName = "hamming",
                          useLog = TRUE,
                          Input2Alg = 'distMatr',
                          ...){

   stopifnot(is.function(FUNcluster),
             length(dim(x)) == 2,
             K.max >= 2,
             (n <- nrow(x)) >= 1,
             ncol(x) >= 1)

   if(!is.matrix(x))
      x <- as.matrix(x)

   uniLevs <- unique(as.vector(x))
   uniLevs <- sort(uniLevs)

   if (is.numeric(x)) {
      message(paste0("x array is numerical and has ", length(uniLevs),
                     " levels."))
      stopifnot(length(uniLevs) < 10)
      message("Array values would be transformed to categorical levels: ")
      myCats <- paste0(paste0(uniLevs, " -> c", uniLevs), collapse = ", ")
      message(myCats)
      x <- apply(x, 1, function(y) paste0('c', y)) %>% t ## Transformed matrix
   }
   else {
      message(paste0("Found levels: ", paste0(uniLevs, collapse = ", ")))
   }

   if (B != (B. <- as.integer(B)) || (B <- B.) <= 0)
      stop("'B' has to be a positive integer")
   cl. <- match.call

   ii <- seq_len(n)

   if(Input2Alg == 'distMatr'){

      W.k <- function(X, kk) {
         clus <- if (kk > 1) {
            Xdist <- distancematrix(X, d = distName)
            FUNcluster(Xdist, kk)$cluster
         }
         else {
            rep.int(1L, nrow(X))
         }
         mySplit <- split(ii, clus)
         mySplit <- mySplit[sapply(mySplit, length) != 1]
         # 0.5 * sum(vapply(mySplit, function(I) {
         sum(vapply(mySplit, function(I) {
            xs <- X[I, , drop = FALSE]
            sum(distancematrix(xs, d = distName)/nrow(xs), na.rm = TRUE)
         }, 0))
      }

   }else if(Input2Alg == 'distFun'){

      PWdistFun <- function(x, y){
         x <- unlist(x)
         y <- unlist(y)
         distancematrix(rbind(x, y), d = distName)[1]
      }

      myFUNcluster <- function(X, kk){
         ## Condition: FUNcluster is expecting a distFun parameter
         FUNcluster(X,
                    kk,
                    distFun = function(x, y) PWdistFun(x, y) )
      }

      W.k <- function(X, kk){
         clus <- if (kk > 1) {
            ## Xdist <- distancematrix(X, d = distName)
            myFUNcluster(X, kk)$cluster
         }
         else {
            rep.int(1L, nrow(X))
         }
         mySplit <- split(ii, clus) ## Split into a list with potential clusters
         mySplit <- mySplit[sapply(mySplit, length) != 1] ## Removes singletons
         # 0.5 * sum(vapply(mySplit, function(I) {
         sum(vapply(mySplit, function(I) {
            xs <- X[I, , drop = FALSE]
            sum(distancematrix(xs, d = distName)/nrow(xs), na.rm = TRUE)
         }, 0))
      }

   }

   logW <- E.logW <- SE.sim <- numeric(K.max)
   if (verbose)
      cat("Clustering k = 1,2,..., K.max (= ", K.max, "): .. ",
          sep = "")

   for (k in 1:K.max){
      if(useLog){
         logW[k] <- log(W.k(x, k))
      }else{
         logW[k] <- W.k(x, k)
      }
   }

   ## If all distances are 0; option 1.
   ## if(any(!is.finite(logW)))
   ##     logW[!is.finite(logW)] <- min(logW[is.finite(logW)])

   if (verbose)
      cat("done\n")

   if(is.numeric(value.range)){
      ## If value.range are numeric, turn them into c0, c1, ...
      message('value.range cannot be numerical and will be transformed to characters.')
      vals <- as.character(paste0('c', value.range))
   }else if (is.character(value.range) &
             !is.list(value.range) &
             value.range[1] == "DS" & length(value.range) == 1) {
      ## Data-Support null option
      vals <- NULL
      rng.x1 <- lapply(1:ncol(x), function(i) unique(x[, i]))
   }
   else if (all(is.character(value.range)) &
            !is.list(value.range) &
            (length(value.range) > 1)) {
      ## User-defined single range
      vals <- value.range
   }
   else if (!is.character(value.range) & is.list(value.range)) {
      ## When user defines the value range for each question
      stopifnot(ncol(x) == length(value.range))
      vals <- value.range
   }

   logWks <- matrix(0, B, K.max)
   if (verbose)
      cat("Bootstrapping, b = 1,2,..., B (= ", B, ")  [one \".\" per sample]:\n",
          sep = "")

   for (b in 1:B) {
      if (is.null(vals)) {
         ## Data-Support case. Notice that rng.x1 is a list with individual values ranges.
         z <- lapply(rng.x1, function(M, nn) sample(size = nn,
                                                    x = M,
                                                    replace = TRUE),
                     nn = n) %>%
            do.call(what = cbind)
      }
      else if (!is.list(vals) & length(vals) > 2) {
         ## KS: Known-Support User-specified categories as vector.
         z <- matrix(sample(x = vals, size = nrow(x) * ncol(x),
                            replace = TRUE), nrow = nrow(x))
      }
      else if (is.list(vals)) {
         ## User-defined categories as list (length nQ)
         z <- sapply(1:length(vals),
                     function(i, nn) sample(size = nn,
                                            x = vals[[i]],
                                            replace = TRUE),
                     nn = n)
      }

      for (k in 1:K.max) {
         if(useLog){
            logWks[b, k] <- log(W.k(z, k))
         }else{
            logWks[b, k] <- W.k(z, k)
         }
      }
      if (verbose)
         cat(".", if (b%%50 == 0)
            paste(b, "\n"))
   }
   if (verbose && (B%%50 != 0))
      cat("", B, "\n")
   E.logW <- colMeans(logWks)
   SE.sim <- sqrt((1 + 1/B) * apply(logWks, 2, stats::var))

   return(structure(class = "clusGap",
                     list(Tab = cbind(logW, E.logW,
                                      gap = E.logW - logW,
                                      SE.sim),
                          call = cl.,
                          n = n,
                          B = B,
                          FUNcluster = FUNcluster,
                          useLog = useLog,
                          distName = distName,
                          value.range = value.range)) )
}

#' Discrete application of clusGap
#'
#' Based on the implementation of the function found in the `cluster` R package.
#'
#' @param x A matrix object specifying category attributes in the columns and observations in the rows.
#' @param clusterFUN Character string with one of the available clustering implementations.
#' Available options are: 'pam' (default) from `cluster::pam`, 'diana' from `cluster::diana`, 'fanny' from `cluster::fanny`,
#' 'agnes-\{average, single, complete, ward, weighted\}' from `cluster::fanny`,
#' 'hclust-\{ward.D, ward.D2, single, complete, average, mcquitty, median, centroid\}' from `stats::hclust`,
#' 'kmodes' from `klar::kmodes` (`iter.max = 10`, `weighted = FALSE` and `fast= TRUE`).
#' 'kmodes-N' enables to run the `kmodes` algorithm with a given number N of iterations where `iter.max = N`.
#' @param K.max Integer. Maximum number of clusters `k` to consider
#' @param value.range String character vector or a list of character vector with the length matching the number of columns (nQ) of the array.
#' A vector with all categories to consider when bootstrapping the null distribution sample (KS: Known Support option).
#' By DEFAULT vals=NULL, meaning unique range of categories found in the data will be used when drawing the null (DS: Data Support option).
#' If a character vector of categories is provided, these values would be used for the null distribution drawing across the array.
#' If a list with category character vectors is provided, it has to have the same number of columns as the input array. The order of list element corresponds to the array's columns.
#' @param verbose Integer or logical. Determines if “progress” output should be printed. The default prints one bit per bootstrap sample.
#' @param distName String. Name of categorical distance to apply.
#' Available distances: 'bhattacharyya', 'chisquare', 'cramerV', 'hamming' and 'hellinger'.
#' @param B Number of bootstrap samples. By default B = nrow(x).
#' @param verbose Integer or logical. Determines whether progress output should printed while running. By DEFAULT one bit is printed per bootstrap sample.
#' @param useLog Logical. Use log function after estimating `W.k`. Following the original formulation `useLog=TRUE` by default.
#' @param ... optionally further arguments for `FUNcluster()`
#'
#' @return a matrix with K.max rows and 4 columns, named "logW", "E.logW", "gap", and "SE.sim",
#' where gap = E.logW - logW, and SE.sim correspond to the standard error of `gap`.
#' @export

clusGapDiscr <- function(x,
                         clusterFUN,
                         K.max,
                         B = nrow(x),
                         value.range = "DS",
                         verbose = interactive(),
                         distName = "hamming",
                         useLog = TRUE,
                         ...){

   if(clusterFUN == 'pam'){
      clusGapDiscr0(x = x,
                    FUNcluster = cluster::pam,
                    K.max = K.max,
                    B = B,
                    value.range = value.range,
                    verbose = verbose,
                    distName = distName,
                    useLog = useLog,
                     ...)

   }else if(clusterFUN == 'fanny'){
      clusGapDiscr0(x = x,
                    FUNcluster = cluster::fanny,
                    K.max = K.max,
                    B = B,
                    value.range = value.range,
                    verbose = verbose,
                    distName = distName,
                    useLog = useLog,
                    ...)

   }else if(clusterFUN == 'diana'){
      dianaK <- function(d, k){
         out <- cluster::diana(x = d) %>%
                 stats::cutree(k = k)
         list(cluster = out)
      }

      clusGapDiscr0(x = x,
                    FUNcluster = dianaK,
                    K.max = K.max,
                    B = B,
                    value.range = value.range,
                    verbose = verbose,
                    distName = distName,
                    useLog = useLog,
                    ...)

   }else if(grepl(pattern = '^agnes-.+', x = clusterFUN)){

      cMeth <- sub('agnes-', '', clusterFUN)
      agnesK <- function(d = d, k = k){
         out <- cluster::agnes(x = d,
                               diss = TRUE,
                               method = cMeth) %>%
            stats::cutree(k = k)
         list(cluster = out)
      }

      clusGapDiscr0(x = x,
                    FUNcluster = agnesK,
                    K.max = K.max,
                    B = B,
                    value.range = value.range,
                    verbose = verbose,
                    distName = distName,
                    useLog = useLog,
                    ...)

   }else if(grepl(pattern = '^hclust-.+', x = clusterFUN)){

      cMeth <- sub('hclust-', '', clusterFUN)
      hClustK <- function(d = d, k = k){
            out <- stats::hclust(d = d, method = cMeth) %>%
               stats::cutree(k = k)
            list(cluster = out)
         }

      clusGapDiscr0(x = x,
                    FUNcluster = hClustK,
                    K.max = K.max,
                    B = B,
                    value.range = value.range,
                    verbose = verbose,
                    distName = distName,
                    useLog = useLog,
                    ...)

   }else if(grepl(pattern = '^kmodes.*', x = clusterFUN)){

      cIter <- ifelse(grepl(pattern = 'kmodes-.+',  x = clusterFUN),
                      as.numeric(sub('kmodes-', '', clusterFUN)),
                      10)

      kmodesD_ <- function(x, k, distFun){
         kmodesD(data = x, modes = k, distFun = distFun, iter.max = cIter)
      }

      clusGapDiscr0(x = x,
                    FUNcluster = kmodesD_,
                    K.max = K.max,
                    B = B,
                    value.range = value.range,
                    verbose = verbose,
                    distName = distName,
                    useLog = useLog,
                    Input2Alg = 'distFun',
                    ...)
   }else
      message('Clustering algorithm not available.')

}

#' Criteria to determine number of clusters k
#'
#' Same function as found in `cluster` package.
#'
#' @import dplyr
#' @param cG_obj Output object obtained from `clusGapDiscr`
#' @param meth Method to use to determine optimal k number of clusters.
#' @return A numerical value from 1 to K.max, contained in the input `cG_obj` object.
#' @export

findK <- function (cG_obj, meth = "Tibs2001SEmax"){
   logW <- gap <- SE.sim <- NULL
   if (!meth %in% c("minSE", "minGap", "maxChange")) {

      ## Tibs2001 SEmax criterion
      # myTab <- data.frame(cG_obj$Tab) %>%
      #    mutate(nClus = 1:nrow(cG_obj$Tab) ) %>%
      #    subset(is.finite(logW) & !is.na(gap) & !is.na(SE.sim))

      outDF <- data.frame(cG_obj['Tab'])
      outDF[, 'nClus'] <- 1:nrow(outDF)
      colnames(outDF) <- c('logW', 'E.logW', 'gap', 'SE.sim', 'nClus')

      myTab <- outDF %>%
         subset(is.finite(logW) & !is.na(gap) & !is.na(SE.sim))
      selK <- with(myTab,
                   cluster::maxSE(f = gap,
                                  SE.f = SE.sim,
                                  method = meth) )
      myTab[selK, 'nClus']

   }
   else if (meth == "minSE") {
      which.min(cG_obj$Tab[, "SE.sim"])
   }
   else if (meth == "minGap") {
      which.min(cG_obj$Tab[, "gap"])
   }
   else if (meth == "maxChange") {
      which.max(abs(cG_obj$Tab[, "gap"])) + 1
   }
}
