chunk <- function(x, n) (
  #' Create chunks from a vector for parallel computing
  #'
  #' (INTERNAL)
  #'
  #' @param x vector
  #' @param n length of chunks
  #'
  #' @export
  #'
  #' @return a list of chunks of length n
  #' @source https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks
  mapply(function(a, b) (x[a:b]),
         seq.int(from = 1, to = length(x), by = n),
         pmin(seq.int(from = 1, to = length(x), by = n) + (n-1), length(x)),
         SIMPLIFY=FALSE
  )
)

chunk_2gether <- function(x, y, n) (
  #' Create chunks from two vectors for parallel computing
  #'
  #' (INTERNAL)
  #'
  #' @param x,y vectors
  #' @param n length of chunks
  #'
  #' @export
  #'
  #' @return A list of lists. Each second level list contains a list of chunks of length n of each
  #' input vector.
  #' @source modified from: https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks
  mapply(function(a, b) (list(x[a:b], y[a:b])),
         seq.int(from = 1, to = length(x), by = n),
         pmin(seq.int(from = 1, to = length(x), by = n) + (n-1), length(x)),
         SIMPLIFY = FALSE
  )
)

corPvalueStudentParallel <- function(correlation, n_samples, chunk_size) {
  #' Compute p-values for upper triangle of correlation matrix in parallel
  #'
  #' (INTERNAL)
  #'
  #' @param correlation matrix of correlation values
  #' @param n_samples matrix of number of samples used in computation of each correlation value
  #' @param chunk_size smallest unit of work in parallel computation (number of p-values to compute)
  #'
  #' @export
  #'
  #' @return vector of p-values for upper triangle
  #'
  if (is.matrix(n_samples)) {
    # if n_samples is a matrix, 'pairwise.complete.obs' was used -> supply number of samples for each individual correlation calculated
    # WGCNA::corPvalueStudent can also take a vector as input

    chunks = chunk_2gether(correlation[upper.tri(correlation)], n_samples[upper.tri(n_samples)], chunk_size)
    rm(correlation)
    rm(n_samples)
    gc()

    p <- parallel::parLapply(parallel::getDefaultCluster(), chunks, function(chunk) {
      # WGCNA::corPvalueStudent
      2*stats::pt(abs(sqrt(chunk[[2]]-2) * chunk[[1]]/sqrt(1-chunk[[1]]^2)),chunk[[2]]-2, lower.tail = FALSE)}
    )
  } else {
    chunks = chunk(correlation[upper.tri(correlation)], chunk_size)
    p <- parallel::parLapply(parallel::getDefaultCluster(), chunks, function(chunk, n_samples) {
      # WGCNA::corPvalueStudent
      2*stats::pt(abs(sqrt(n_samples-2) * chunk/sqrt(1-chunk^2)),n_samples-2, lower.tail = FALSE)
    }, n_samples)
  }
  rm(chunks)
  return(unlist(p, recursive = FALSE, use.names = FALSE))
}

network_reduction_by_p_value <- function(adjacency_matrix,
                                         number_of_samples,
                                         reduction_alpha = 0.05,
                                         p_value_adjustment_method = "BH",
                                         parallel_chunk_size = 10^6) {

  #' Reduce the the entries in an adjacency matrix by thresholding on p-values
  #'
  #' (INTERNAL) This function reduces an adjacency matrix of correlations. If computations are done
  #' non-parallel \code{\link[WGCNA]{corPvalueStudent}} is used. If computations are done in
  #' parallel, our own parallel implementation (\code{\link{corPvalueStudentParallel}}) of this
  #' function is used.
  #' function to calculate Student asymptotic p-values taking the number of samples into account.
  #' P-values are adjusted using \link[stats]{p.adjust} function. The upper triangle without
  #' diagonal entries
  #' of the adjacency matrix is passed for faster computation. P-values can be adjusted using one
  #' of several methods.
  #' A significance threshold `alpha` can be set. All value entries below this threshold within the
  #' initial adjacency matrix
  #' will be set to NA. If a default cluster is registered with the `parallel` package the
  #' computation will happen in
  #' parallel automatically.
  #'
  #'
  #' @param adjacency_matrix An adjacency matrix of correlation values.
  #' @param number_of_samples The number of samples used to calculate the correlation matrix.
  #' @param reduction_alpha A number indicating the alpha value applied for thresholding
  #' @param p_value_adjustment_method A string of the correction method applied to p-values. Passed
  #' to stats::p.adjust().
  #' @param parallel_chunk_size Number of p-values in smallest work unit when computing in parallel.
  #'
  #' @export
  #' @examples
  #' adj_mat <- matrix(rnorm(36),nrow=6)
  #' sum(is.na(adj_mat)) # before reduction
  #' reduced_by_p_value_matrix <- network_reduction_by_p_value(adjacency_matrix=adj_mat,
  #'                              number_of_samples=200, reduction_alpha = 0.05,
  #'                              p_value_adjustment_method = "BH")
  #' sum(is.na(reduced_by_p_value_matrix)) # after reduction
  #' @return A reduced adjacency matrix with NA's at martix entries with p-values below threshold.
  #' @source \code{\link[WGCNA]{corPvalueStudent}}

  if (is.null(parallel::getDefaultCluster())) {
    # compute p values on upper triangle only (-> symmetric matrix)
    upper_adjacency_matrix <- adjacency_matrix[upper.tri(adjacency_matrix)]
    if (is.matrix(number_of_samples)) number_of_samples <- number_of_samples[upper.tri(number_of_samples)]
    p_values <- WGCNA::corPvalueStudent(upper_adjacency_matrix,
                                        number_of_samples)
  } else {
    p_values <- corPvalueStudentParallel(correlation = adjacency_matrix,
                                         n_samples = number_of_samples,
                                         chunk_size = parallel_chunk_size)
  }

  message("p-value matrix calculated.\n")

  adjusted_p <- stats::p.adjust(p_values,
                                method = p_value_adjustment_method)

  message("p-values adjusted.\n")

  adjusted_p_matrix <- matrix(0,
                              nrow = dim(adjacency_matrix)[[1]],
                              ncol = dim(adjacency_matrix)[[2]])

  adjusted_p_matrix[upper.tri(adjusted_p_matrix)] <- adjusted_p

  adjusted_p_matrix[lower.tri(adjusted_p_matrix)] <- base::t(adjusted_p_matrix)[lower.tri(adjusted_p_matrix)]
  message("full adjusted p-value matrix complete.\n")

  not_significant <- adjusted_p_matrix > reduction_alpha

  message("thresholding done.\n")

  adjacency_matrix[not_significant] <- NA

  return(adjacency_matrix)
}




network_reduction_by_pickHardThreshold <- function(adjacency_matrix,
                                                   RsquaredCut = 0.85,
                                                   cutVector = seq(0.2, 0.8, by = 0.05),
                                                   method = "pickHardThreshold") {
  #' Reduces network based on WGCNA::pickHardThreshold function
  #'
  #' (INTERNAL) This function uses \code{\link[WGCNA]{pickHardThreshold.fromSimilarity}} or an
  #' alternative implementation \code{\link{pickHardThreshold_alternative}} contained in this
  #' package to analyze scale free topology for multiple hard thresholds. Within the first iteration
  #'  a `coarse` cutoff is estimated. If no cutoff is found the function terminates with an error
  #'  message. The second iteration determines a `fine-grained` cutoff based on the first iterations
  #' cut estimate (+/- 0.25) in sequence steps of 0.01.
  #' All values below the cutoff will be set to NA and the reduced adjacency is returned.
  #'
  #' @param adjacency_matrix Adjacency matrix of correlation values.
  #' @param RsquaredCut A number indicating the desired minimum scale free topology fitting
  #' index R^2.
  #' @param cutVector A vector of hard threshold cuts for which the scale free topology fit indices
  #' are to be calculated.
  #' @param method String. Determines whether the original implementation of
  #' \code{\link[WGCNA]{pickHardThreshold.fromSimilarity}} is used ("pickHardThreshold") or the
  #' alternative implementation contained in this package
  #' \code{\link{pickHardThreshold_alternative}} ("pickHardThreshold_alternative").
  #' @export
  #' @source The original implementation of pickHardThreshold is used from
  #' \code{\link[WGCNA]{pickHardThreshold.fromSimilarity}}
  #' @examples
  #' \dontshow{
  #' WGCNA::disableWGCNAThreads()
  #' }
  #' data(mrna_data)
  #' adj_mat <- WGCNA::cor(mrna_data$group1$data)
  #' reduced_by_PHT <- network_reduction_by_pickHardThreshold(adj_mat,
  #' RsquaredCut = 0.1, cutVector = seq(0.2, 0.8, by = 0.05))
  #'
  #' @return A reduced adjacency matrix of correlations with NA's inserted at positions below
  #' estimated cutoff.
  #'

  if (method == "pickHardThreshold") {
    message('Reducing network by WGCNA::pickHardThreshold.\n')
    ## "invisible(capture.output())" suppresses printing of a table of cutoff and Rsquared values

    ## first "coarse" pickHardThreshold iteration
    invisible(utils::capture.output(
      cut_1 <- WGCNA::pickHardThreshold.fromSimilarity(abs(adjacency_matrix),
                                                       RsquaredCut,
                                                       cutVector)
    ))

    # terminate execution if pickHardThreshold cannot find cutoff
    if (is.na(cut_1$cutEstimate)){
      stop("WGCNA::pickHardThreshold failed to find a suitable cutoff at the set R^2")

    } else {
      ## second "fine" pickhardThreshold iteration
      invisible(utils::capture.output(
        cut_2 <- WGCNA::pickHardThreshold.fromSimilarity(abs(adjacency_matrix),
                                                         RsquaredCut,
                                                         cutVector = seq(cut_1$cutEstimate - 0.25,
                                                                         cut_1$cutEstimate + 0.25,by = 0.01))
      ))

      cutEstimate <- cut_2$cutEstimate
    }
  }
  if (method == "pickHardThreshold_alternative") {
    message('Reducing network by alternative implementation of WGCNA::pickHardThreshold.\n')
    cutEstimate_coarse <- pickHardThreshold_alternative(abs(adjacency_matrix),
                                                                  RsquaredCut,
                                                                  cutVector)
    cutEstimate <- pickHardThreshold_alternative(abs(adjacency_matrix),
                                                           RsquaredCut,
                                                           cutVector = seq(cutEstimate_coarse - 0.25,
                                                                           cutEstimate_coarse + 0.25,by = 0.01))

  }

  # set all entries below the threshold to NA
  adjacency_matrix[abs(adjacency_matrix) < cutEstimate] <- NA

  return(adjacency_matrix)
}





