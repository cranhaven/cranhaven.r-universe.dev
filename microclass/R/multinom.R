#' @name multinomTrain
#' @title Training multinomial model
#' 
#' @description Training the multinomial K-mer method on sequence data.
#' 
#' @param sequence Character vector of 16S sequences.
#' @param taxon Character vector of taxon labels for each sequence.
#' @param K Word length (integer).
#' @param col.names Logical indicating if column names should be added to the trained model matrix.
#' @param n.pseudo Number of pseudo-counts to use (positive numerics, need not be integer). Special case -1
#' will only return word counts, not log-probabilities.
#' 
#' @details The training step of the multinomial method (Vinje et al, 2015) means counting K-mers
#' on all sequences and compute the multinomial probabilities for each K-mer for each unique taxon. 
#' \code{n.pseudo} pseudo-counts are added, divided equally over all K-mers, before probabilities
#' are estimated. The optimal choice of \code{n.pseudo} will depend on \code{K} and the 
#' training data set. The default value \code{n.pseudo=100} has proven good for \code{K=8} and the
#' \code{\link[microcontax]{contax.trim}} data set (see the \code{microcontax} R-package).
#' 
#' Adding the actual K-mers as column names (\code{col.names=TRUE}) will slow down the
#' computations.
#' 
#' The relative taxon sizes are also computed, and may be used as an empirical prior in the
#' classification step (see "prior" below).
#' 
#' @return A list with two elements. The first element is \code{Method}, which is the text 
#' \code{"multinom"} in this case. The second element is \code{Fitted}, which is a matrix
#' of probabilities with one row for each unique \code{taxon} and one column for each possible word of
#' length\code{K}. The sum of each row is 1.0. No probabilities are 0 if \code{n.pseudo}>0.0.
#' 
#' The matrix \code{Fitted} has an attribute \code{attr("prior",)}, that contains the relative
#' taxon sizes.
#' 
#' @author Kristian Hovde Liland and Lars Snipen.
#' 
#' @references Vinje, H, Liland, KH, Almøy, T, Snipen, L. (2015). Comparing K-mer based methods for
#' improved classification of 16S sequences. BMC Bioinformatics, 16:205.
#' 
#' @seealso \code{\link{KmerCount}}, \code{\link{multinomClassify}}.
#' 
#' @examples # See examples for multinomClassify
#' 
#' @export multinomTrain
#' 
multinomTrain <- function(sequence, taxon, K = 8, col.names = FALSE, n.pseudo = 100){
  taxInt <- taxon
  if(is.character(taxon)){
    taxInt  <- factor(taxon)
    taxLevels <- levels(taxInt)
    taxInt  <- as.integer(taxInt)
    prior <- as.numeric(table(taxon)/length(taxon))
  }
  classesIn <- lapply(1:max(taxInt), function(i)which(i==taxInt))
  multinom.prob <- multinomTrainCpp(charToInt(sequence), K, col.names, classesIn, -1)
  if(n.pseudo >= 0){ # Apply pseudo counts in R
    multinom.prob <- CountsToMultinom(multinom.prob, n.pseudo)
  }
  if(is.character(taxon)){
    dimnames(multinom.prob) <- list(taxLevels, NULL) # Avoids copying
  }
  attr(multinom.prob, "prior") <- prior
  trained.model <- list(Method = "multinom", Fitted = multinom.prob)
  return(trained.model)
}

#' @name multinomClassify
#' @title Classifying with a Multinomial model
#' 
#' @description Classifying sequences by a trained Multinomial model.
#' 
#' @param sequence Character vector of 16S sequences to classify.
#' @param trained.model A list with a trained model, see \code{\link{multinomTrain}}.
#' @param post.prob Logical indicating if posterior log-probabilities should be returned.
#' @param prior Logical indicating if classification should be done by flat priors (default)
#' or with empirical priors (prior=TRUE).
#' 
#' @details The classification step of the Multinomial method (Vinje et al, 2015) means counting 
#' K-mers on all sequences, and computing the posterior probabilities for each taxon in the trained model.
#' The predicted taxon for each input sequence is the one with the maximum posterior probability for
#' that sequence.
#' 
#' By setting \code{post.prob=TRUE} you will get the log-probability of the best and second best taxon
#' for each sequence. This can be used for evaluating the certainty in the classifications,
#' see \code{\link{taxMachine}}.
#' 
#' The classification is parallelized through RcppParallel
#' employing Intel TBB and TinyThread. By default all available
#' processing cores are used. This can be changed using the
#' function \code{\link{setParallel}}.
#' 
#' @return If \code{post.prob=FALSE} a character vector of predicted taxa is returned.
#' 
#' If \code{post.prob=TRUE} a \code{data.frame} with three columns is returned. Taxon
#' is the vector of predicted taxa, one for each sequence in \code{sequence}. The
#' Post.prob.1 and Post.prob.2 are vectors with the maximum and second largest posterior
#' log-probabilities for each sequence.
#' 
#' @author Kristian Hovde Liland and Lars Snipen.
#' 
#' @references Vinje, H, Liland, KH, Almøy, T, Snipen, L. (2015). Comparing K-mer based methods for
#' improved classification of 16S sequences. BMC Bioinformatics, 16:205.
#' 
#' @seealso \code{\link{KmerCount}}, \code{\link{multinomTrain}}.
#' 
#' @examples 
#' data("small.16S")
#' seq <- small.16S$Sequence
#' tax <- sapply(strsplit(small.16S$Header,split=" "),function(x){x[2]})
#' \dontrun{
#' trn <- multinomTrain(seq,tax)
#' primer.515f <- "GTGYCAGCMGCCGCGGTAA"
#' primer.806rB <- "GGACTACNVGGGTWTCTAAT"
#' reads <- amplicon(seq, primer.515f, primer.806rB)
#' predicted <- multinomClassify(unlist(reads[nchar(reads)>0]),trn)
#' print(predicted)
#' }
#' 
#' @export multinomClassify
#' 
multinomClassify <- function(sequence, trained.model, post.prob = FALSE, prior = FALSE){
  if(trained.model$Method != "multinom") stop("Trained model is not a multinomial!")
  multinom.prob <- trained.model$Fitted
  int.list <- charToInt(sequence)
  if(prior){
    priors <- log2(attr(multinom.prob, "prior"))
  } else {
    priors <- rep(0, dim(multinom.prob)[1])
  }
  X <- multinomClassifyCpp(int.list, log(ncol(multinom.prob), 4), multinom.prob, priors, post.prob)
  if(post.prob){
    return(data.frame(Taxon.1 = rownames(multinom.prob)[X$first_ind], Post.prob.1 = X$first,
                      Taxon.2 = rownames(multinom.prob)[X$second_ind], Post.prob.2 = X$second,
                      stringsAsFactors=FALSE))
  } else {
    return(rownames(multinom.prob)[X$first_ind])
  }
}


#' Set number of parallel threads
#' 
#' @description Simple function to set the number of threads to use in parallel
#' computations. The default equals all available logical cores. An integer is
#' interpreted as the number of threads. A numeric < 1 is interpreted as a proportion
#' of the avialable logical cores.
#'
#' @param C a scalar indicating the number of threads, default = NULL (#available logical cores)
#'
#' @return NULL, returned silently.
#' @export
#'
#' @examples
#' \dontrun{
#' setParallel() # Use all available logical cores.
#' }
#' 
#' @importFrom RcppParallel RcppParallelLibs setThreadOptions defaultNumThreads
setParallel <- function(C = NULL){
  if(is.null(C)){ # Reset to default
    setThreadOptions(numThreads = defaultNumThreads())
  } else {
    if(C < 1) # Fraction of available cores
      setThreadOptions(numThreads = max(1, round(defaultNumThreads()*C )))
    if(C >= 1)
      setThreadOptions(numThreads = C)
  }
  invisible(NULL)
}

# Unexported functions for transforming sparse count matrices to
# multinomial classification matrices
CountsToRDP <- function(X, sizes){
  log2(X + rep(attr(X, 'p') + 0.5, each = nrow(X)) / (sum(sizes) + 1)) - log2(sizes + 1)
}
CountsToMultinom <- function(X, n.pseudo){
  log2(X + n.pseudo / ncol(X)) - log2(rowSums(X) + n.pseudo)
}
CountsToMultinomMulti <- function(X, n.pseudo, rowsums){
  log2(X + n.pseudo / ncol(X)) - log2(rowsums + n.pseudo)
}

