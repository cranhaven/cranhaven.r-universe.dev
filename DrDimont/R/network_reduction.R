chunk <- function(x, chunk_size) (
    #' @title [INTERNAL] Create chunks from a vector for parallel computing
    #' @description [INTERNAL] Deprecated! This function will be removed in future versions. Create chunks from a vector for parallel computing
    #'
    #' @param x Vector
    #' @param chunk_size [int] Length of chunks
    #'
    #' @return A list of chunks of length chunk_size
    #' @source https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks
    #' 
    #' @keywords internal
    #' @noRd
    
    mapply(function(a, b) (x[a:b]),
           seq.int(from=1, to=length(x), by=chunk_size),
           pmin(seq.int(from=1, to=length(x), by=chunk_size) + (chunk_size-1), length(x)),
           SIMPLIFY=FALSE)
)

chunk_2gether <- function(x, y, chunk_size) (
    #' @title [INTERNAL] Create chunks from two vectors for parallel computing
    #' @description [INTERNAL] Deprecated! This function will be removed in future versions. Create chunks from two vectors for parallel computing
    #'
    #' @param x Vector    
    #' @param y Vector
    #' @param chunk_size [int] Length of chunks
    #'
    #' @return A list of lists. Each second level list contains a list of chunks of length chunk_size of each
    #' input vector.
    #' @source modified from: https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks
    #' 
    #' @keywords internal
    #' @noRd
    
    mapply(function(a, b) (list(x[a:b], y[a:b])),
           seq.int(from=1, to=length(x), by=chunk_size),
           pmin(seq.int(from=1, to=length(x), by=chunk_size) + (chunk_size-1), length(x)),
           SIMPLIFY=FALSE)
)

corPvalueStudentParallel <- function(adjacency_matrix, number_of_samples, chunk_size) {
    #' @title [INTERNAL] Compute p-values for upper triangle of correlation matrix in parallel
    #' @description [INTERNAL] Deprecated! This function will be removed in future versions. Compute p-values for upper triangle of correlation matrix in parallel
    #'
    #' @param adjacency_matrix [matrix] Adjacency matrix of correlations computed using \code{\link[WGCNA]{cor}} in
    #' \code{\link[DrDimont]{compute_correlation_matrices}}
    #' @param number_of_samples [matrix] Matrix of number of samples used in computation of each correlation value. Computed applying
    #' \code{\link[DrDimont]{sample_size}}
    #' @param chunk_size [int] Smallest unit of work in parallel computation (number of p-values to compute)
    #' 
    #' @return Vector of p-values for upper triangle
    #'
    #' @keywords internal
    #' @noRd
    
    if (is.matrix(number_of_samples)) {
        # if number_of_samples is a matrix, 'pairwise.complete.obs' was used -> supply number of samples for each individual correlation calculated
        # WGCNA::corPvalueStudent can also take a vector as input

        chunks <- chunk_2gether(adjacency_matrix[upper.tri(adjacency_matrix)], number_of_samples[upper.tri(number_of_samples)], chunk_size)
        rm(adjacency_matrix)
        rm(number_of_samples)
        gc()

        p <- parallel::parLapply(parallel::getDefaultCluster(), chunks, function(chunk){
            # WGCNA::corPvalueStudent
            2*stats::pt(abs(sqrt(chunk[[2]]-2) * chunk[[1]]/sqrt(1-chunk[[1]]^2)), chunk[[2]]-2, lower.tail=FALSE)})

    } else {
        chunks = chunk(adjacency_matrix[upper.tri(adjacency_matrix)], chunk_size)

        p <- parallel::parLapply(parallel::getDefaultCluster(), chunks, function(chunk, number_of_samples) {
            # WGCNA::corPvalueStudent
            2*stats::pt(abs(sqrt(number_of_samples-2) * chunk/sqrt(1-chunk^2)),number_of_samples-2, lower.tail=FALSE)}, number_of_samples)

    }

    rm(chunks)
    return(unlist(p, recursive=FALSE, use.names=FALSE))
}

network_reduction_by_p_value <- function(adjacency_matrix,
                                         number_of_samples,
                                         p_value_adjustment_method="BH",
                                         reduction_alpha=0.05) {

    #' @title [INTERNAL] Reduce the the entries in an adjacency matrix by thresholding on p-values
    #'
    #' @description [INTERNAL] This function reduces an adjacency matrix of correlations based on p-values.
    #' If computations are done non-parallel \code{\link[WGCNA]{corPvalueStudent}} is used. 
    #' P-values are adjusted using \link[stats]{p.adjust} function. The upper triangle without diagonal entries
    #' of the adjacency matrix is passed for faster computation. P-values can be adjusted using one
    #' of several methods. A significance threshold `alpha` can be set. All value entries below this threshold within the
    #' initial adjacency matrix will be set to NA.
    #' 
    #' @param adjacency_matrix [matrix] Adjacency matrix of correlations computed using \code{\link[WGCNA]{cor}} in
    #' \code{\link[DrDimont]{compute_correlation_matrices}}
    #' @param number_of_samples [int|matrix] The number of samples used to calculate the correlation matrix. Computed applying
    #' \code{\link[DrDimont]{sample_size}}
    #' @param p_value_adjustment_method ["holm"|"hochberg"|"hommel"|"bonferroni"|"BH"|"BY"|"fdr"|"none"] String
    #' of the correction method applied to p-values. Passed to \link[stats]{p.adjust}. (default: "BH")
    #' @param reduction_alpha [float] A number indicating the significance value for correlation p-values
    #' during reduction. Not-significant edges are dropped. (default: 0.05)
    #' 
    #' @return A reduced adjacency matrix with NA's at martix entries with p-values below threshold.
    #' @source \code{\link[WGCNA]{corPvalueStudent}}
    #' 
    #' @keywords internal
    #' @noRd

    
    # compute p values on upper triangle only (-> symmetric matrix)
    upper_adjacency_matrix <- adjacency_matrix[upper.tri(adjacency_matrix)]
    if (is.matrix(number_of_samples)) { number_of_samples <- number_of_samples[upper.tri(number_of_samples)] }
    p_values <- WGCNA::corPvalueStudent(upper_adjacency_matrix, number_of_samples)

    message(format(Sys.time(), "[%y-%m-%d %X] "), "p-value matrix calculated.")

    adjusted_p <- stats::p.adjust(p_values, method=p_value_adjustment_method)

    message(format(Sys.time(), "[%y-%m-%d %X] "), "p-values adjusted.")

    adjusted_p_matrix <- matrix(0, nrow=dim(adjacency_matrix)[[1]], ncol=dim(adjacency_matrix)[[2]])

    adjusted_p_matrix[upper.tri(adjusted_p_matrix)] <- adjusted_p

    adjusted_p_matrix[lower.tri(adjusted_p_matrix)] <- base::t(adjusted_p_matrix)[lower.tri(adjusted_p_matrix)]

    message(format(Sys.time(), "[%y-%m-%d %X] "), "full adjusted p-value matrix complete.")

    not_significant <- adjusted_p_matrix > reduction_alpha


    message(format(Sys.time(), "[%y-%m-%d %X] "), "thresholding done.")

    adjacency_matrix[not_significant] <- -999

    return(adjacency_matrix)
}




network_reduction_by_pickHardThreshold <- function(adjacency_matrix,
                                                   r_squared_cutoff=0.85,
                                                   cut_vector=seq(0.2, 0.8, by = 0.01),
                                                   mean_number_edges=NULL,
                                                   edge_density=NULL) {
    #' @title [INTERNAL] Reduces network based on WGCNA::pickHardThreshold function
    #'
    #' @description  [INTERNAL] This function uses \code{\link[WGCNA]{pickHardThreshold.fromSimilarity}} to analyze
    #' scale free topology for multiple hard thresholds. A cutoff is estimated, if no cutoff is
    #' found the function terminates with an error message. All values below the cutoff will be set to NA and the
    #' reduced adjacency is returned.
    #'
    #' @param adjacency_matrix [matrix] Adjacency matrix of correlations computed using \code{\link[WGCNA]{cor}} in
    #' \code{\link[DrDimont]{compute_correlation_matrices}}
    #' @param r_squared_cutoff [float] A number indicating the desired minimum scale free topology fitting index R^2 for reduction
    #' using \code{\link[WGCNA]{pickHardThreshold}}. (default: 0.85)
    #' @param cut_vector [sequence of float] A vector of hard threshold cuts for which the scale free topology fit indices are to
    #' be calculated during reduction with \code{\link[WGCNA]{pickHardThreshold}}. (default: seq(0.2, 0.8, by = 0.01))
    #' @param mean_number_edges [int] Find a suitable edge weight cutoff employing \code{\link[WGCNA]{pickHardThreshold}} to reduce
    #' the network to at most the specified mean number of edges. Attention: This parameter overwrites the 'r_squared_cutoff' and
    #' 'edge_density' parameters if not set to NULL. (default: NULL)
    #' @param edge_density [float] Find a suitable edge weight cutoff employing \code{\link[WGCNA]{pickHardThreshold}} to reduce the
    #' network to at most the specified edge density. Attention: This parameter overwrites the 'r_squared_cutoff' parameter if not set
    #' to NULL. (default: NULL)
    #' 
    #' @source The original implementation of pickHardThreshold is used from
    #' \code{\link[WGCNA]{pickHardThreshold.fromSimilarity}}
    #'
    #' @return A reduced adjacency matrix of correlations with NA's inserted at positions below
    #' estimated cutoff.
    #' 
    #' @keywords internal
    #' @noRd
    
    message(format(Sys.time(), "[%y-%m-%d %X] "), 'Reducing network by WGCNA::pickHardThreshold...')

    ### if mean number of edges given calculate cut threshold based on WGCNA::pickHardThreshold.fromSimilarity() mean.k. values
    ### else use the r_squared_cutoff
    if (!is.null(mean_number_edges)){

        message(format(Sys.time(), "[%y-%m-%d %X] "), 'Mean number of edges: ', as.character(mean_number_edges))

        n_entities <- nrow(adjacency_matrix)

        ### "invisible(capture.output())" suppresses printing of a table of cutoff and R^2 values
        ### WGCNA pickHardThreshold computation
        invisible(utils::capture.output(
            wgcna_tresholds <- WGCNA::pickHardThreshold.fromSimilarity(abs(adjacency_matrix), cutVector=cut_vector)
        ))

        ### get WGCNA cut with mean number of edges <= mean_number_edges
        cut_estimate = NULL
        for (row in 1:nrow(wgcna_tresholds$fitIndices)) {
            cut_val <- wgcna_tresholds$fitIndices[row, "Cut"]
            r2_val  <- wgcna_tresholds$fitIndices[row, "SFT.R.sq"]
            slope_val  <- wgcna_tresholds$fitIndices[row, "slope."]
            mean_k_val <- wgcna_tresholds$fitIndices[row, "mean.k."]

            if ((mean_k_val <= mean_number_edges*2)){ #(slope_val<0) & 
                cut_estimate=cut_val
                message(format(Sys.time(), "[%y-%m-%d %X] "), 'R2 cutoff: ', round(r2_val, 2))
                message(format(Sys.time(), "[%y-%m-%d %X] "), 'Cut Threshold: ', as.character(cut_estimate))
                break
            }
        }

        ### terminate execution if pickHardThreshold cannot find cutoff
        if (is.null(cut_estimate)) {
            message(format(Sys.time(), "[%y-%m-%d %X] "),
                    "ERROR: WGCNA::pickHardThreshold: failed to find a threshold with given mean number of edges per node and cut vector.
                    Please, try a different mean number of edges or cut vector. Lowest mean number of edges computed was ",
                    round(min(wgcna_tresholds$fitIndices[wgcna_tresholds$fitIndices$slope.<0, "mean.k."], na.rm=T)/2, 3),
                    ". Highest mean number of edges computed was ",
                    round(max(wgcna_tresholds$fitIndices[wgcna_tresholds$fitIndices$slope.<0, "mean.k."], na.rm=T)/2, 3), ".")
            stop("WGCNA::pickHardThreshold: failed to find a threshold with given mean number of edges per node and cut vector.
                 Please, try a different mean number of edges or cut vector. Lowest mean number of edges computed was ",
                 round(min(wgcna_tresholds$fitIndices[wgcna_tresholds$fitIndices$slope.<0, "mean.k."], na.rm=T)/2, 3),
                 ". Highest mean number of edges computed was ",
                 round(max(wgcna_tresholds$fitIndices[wgcna_tresholds$fitIndices$slope.<0, "mean.k."], na.rm=T)/2, 3), ".")
        }

    ### if mean number of edges not given and edge density given calculate cut threshold based on WGCNA::pickHardThreshold.fromSimilarity() mean.k. values
    } else if (!is.null(edge_density)){

        message(format(Sys.time(), "[%y-%m-%d %X] "), 'Network edge density: ', as.character(edge_density))

        n_entities <- nrow(adjacency_matrix)

        ### "invisible(capture.output())" suppresses printing of a table of cutoff and R^2 values
        ### WGCNA pickHardThreshold computation
        invisible(utils::capture.output(
            wgcna_tresholds <- WGCNA::pickHardThreshold.fromSimilarity(abs(adjacency_matrix), cutVector=cut_vector)
        ))

        ### get WGCNA cut with edge density <= edge_density
        cut_estimate <- NULL
        for (row in 1:nrow(wgcna_tresholds$fitIndices)) {
            cut_val <- wgcna_tresholds$fitIndices[row, "Cut"]
            r2_val  <- wgcna_tresholds$fitIndices[row, "SFT.R.sq"]
            slope_val  <- wgcna_tresholds$fitIndices[row, "slope."]
            mean_k_val <- wgcna_tresholds$fitIndices[row, "mean.k."]

            if ((slope_val<0) & (mean_k_val <= (edge_density*n_entities-edge_density)*2)){
                cut_estimate=cut_val
                message(format(Sys.time(), "[%y-%m-%d %X] "), 'R2 cutoff: ', round(r2_val, 2))
                message(format(Sys.time(), "[%y-%m-%d %X] "), 'Cut Threshold: ', as.character(cut_estimate))
                break
            }
        }

        ### terminate execution if pickHardThreshold cannot find cutoff
        if (is.null(cut_estimate)) {
            message(format(Sys.time(), "[%y-%m-%d %X] "),
                    "ERROR: WGCNA::pickHardThreshold: failed to find a threshold with given edge density and cut vector.
                    Please, try a different edge density or cut vector. Lowest edge density computed was ",
                    round(min(wgcna_tresholds$fitIndices[wgcna_tresholds$fitIndices$slope.<0, "mean.k."], na.rm=T)/(2*(n_entities-1)), 5),
                    ". Highest edge density computed was ",
                    round(max(wgcna_tresholds$fitIndices[wgcna_tresholds$fitIndices$slope.<0, "mean.k."], na.rm=T)/(2*(n_entities-1)), 3), ".")
            stop("WGCNA::pickHardThreshold: failed to find a threshold with given edge density and cut vector.
                 Please, try a different edge density or cut vector. Lowest edge density computed was ",
                 round(min(wgcna_tresholds$fitIndices[wgcna_tresholds$fitIndices$slope.<0, "mean.k."], na.rm=T)/(2*(n_entities-1)), 5),
                 ". Highest edge density computed was ",
                 round(max(wgcna_tresholds$fitIndices[wgcna_tresholds$fitIndices$slope.<0, "mean.k."], na.rm=T)/(2*(n_entities-1)), 3), ".")
        }
    ### else use the r_squared_cutoff
    } else {

        message(format(Sys.time(), "[%y-%m-%d %X] "), 'R2 cutoff: ', as.character(r_squared_cutoff))
        ### "invisible(capture.output())" suppresses printing of a table of cutoff and R^2 values
        ### WGCNA pickHardThreshold computation
        invisible(utils::capture.output(
            wgcna_tresholds <- WGCNA::pickHardThreshold.fromSimilarity(abs(adjacency_matrix), r_squared_cutoff, cut_vector)
        ))

        cut_estimate = wgcna_tresholds$cutEstimate
        ### terminate execution if pickHardThreshold cannot find cutoff
        if (is.na(cut_estimate)) {
            message(format(Sys.time(), "[%y-%m-%d %X] "),
                    "ERROR: WGCNA::pickHardThreshold failed to find a suitable cutoff with the given cut_vector at the given R^2 cutoff.
                    Please, try a different cut_vetor or a different cutoff. Highest R^2 cutoff computed was ",
                    round(max(wgcna_tresholds$fitIndices[wgcna_tresholds$fitIndices$slope.<0, "SFT.R.sq"], na.rm=T), 3), ".")
            stop("WGCNA::pickHardThreshold failed to find a suitable cutoff with the given cut_vector at the given R^2 cutoff.
                    Please, try a different cut_vetor or a different cutoff. Highest R^2 cutoff computed was ",
                 round(max(wgcna_tresholds$fitIndices[wgcna_tresholds$fitIndices$slope.<0, "SFT.R.sq"], na.rm=T), 3), ".")
        ##### check if cut estimate was found and if slope at cut is negative
        ##### if true continue, else find a cut estimate with negative slope
        } else if ((is.na(cut_estimate)==FALSE) & (wgcna_tresholds$fitIndices[wgcna_tresholds$fitIndices$Cut==cut_estimate,]$slope < 0)){
            message(format(Sys.time(), "[%y-%m-%d %X] "), 'Cut Threshold: ', as.character(cut_estimate))
        } else {
            cut_estimate=NA
            for (row in 1:nrow(wgcna_tresholds$fitIndices)) {
                cut_val <- wgcna_tresholds$fitIndices[row, "Cut"]
                r2_val  <- wgcna_tresholds$fitIndices[row, "SFT.R.sq"]
                slope_val  <- wgcna_tresholds$fitIndices[row, "slope."]

                if ((r2_val>r_squared_cutoff) & (slope_val<0)){
                    cut_estimate=cut_val
                    message(format(Sys.time(), "[%y-%m-%d %X] "), 'Cut Threshold: ', as.character(cut_estimate))
                    break
                }
            }
        }

        ### terminate execution if pickHardThreshold cannot find cutoff
        if(is.na(cut_estimate)){ #Please try setting a lower R-squared or using a different network reduction method. Highest R-squared computed was ",round(highestR2, 3)
            message(format(Sys.time(), "[%y-%m-%d %X] "),
                    "ERROR: WGCNA::pickHardThreshold failed to find a suitable cutoff with the given cut_vector at the given R^2 cutoff.
                    Please, try a different cut_vetor or a different cutoff. Highest R^2 cutoff computed was ",
                    round(max(wgcna_tresholds$fitIndices[wgcna_tresholds$fitIndices$slope.<0, "SFT.R.sq"], na.rm=T), 3), ".")
            stop("WGCNA::pickHardThreshold failed to find a suitable cutoff with the given cut_vector at the given R^2 cutoff.
                    Please, try a different cut_vetor or a different cutoff. Highest R^2 cutoff computed was ",
                 round(max(wgcna_tresholds$fitIndices[wgcna_tresholds$fitIndices$slope.<0, "SFT.R.sq"], na.rm=T), 3), ".")
        }
    }

    cutEstimate <- cut_estimate

    ### set all entries below the threshold to NA
    adjacency_matrix[abs(adjacency_matrix) < cutEstimate] <- -999

    return(adjacency_matrix)
}


