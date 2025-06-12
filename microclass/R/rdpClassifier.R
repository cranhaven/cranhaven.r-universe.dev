#' @name rdpTrain
#' @title Training the RDP classifier
#' 
#' @description Training the RDP presence/absence K-mer method on sequence data.
#' 
#' @param sequence Character vector of 16S sequences.
#' @param taxon Character vector of taxon labels for each sequence.
#' @param K Word length (integer).
#' @param cnames Logical indicating if column names should be added to the trained model matrix.
#' 
#' @details The training step of the RDP method means looking for K-mers on all sequences,
#' and computing the probability of each K-mer being present for each unique taxon. This is an
#' attempt to re-implement the method described by Wang et tal (2007), but without the bootstrapping. 
#' See that publications for all details.
#' 
#' The word-length \code{K} is by default 8, since this is the value used by Wang et al. Larger values
#' may lead to memory-problems since the trained model is a matrix with 4^K columns. Adding the K-mers 
#' as column names will slow down all computations.
#' 
#' The relative taxon sizes are also computed, and returned as an attribute to the model matrix. They may 
#' be used as empirical priors in the classification step.
#' 
#' @return A list with two elements. The first element is \code{Method}, which is the text 
#' \code{"RDPclassifier"} in this case. The second element is \code{Fitted}, which is a 
#' matrix with one row for each unique \code{taxon} and one column for 
#' each possible word of length \code{K}. The value in row i and column j is the probability that
#' word j is present in taxon i.
#' 
#' @references Wang, Q, Garrity, GM, Tiedje, JM, Cole, JR (2007). Naive Bayesian Classifier for 
#' Rapid Assignment of rRNA Sequences into the New Bacterial Taxonomy. Applied and Enviromental 
#' Microbiology, 73: 5261-5267.
#' 
#' @author Kristian Hovde Liland and Lars Snipen.
#' 
#' @seealso \code{\link{rdpClassify}}.
#' 
#' @examples 
#' # See examples for rdpClassify.
#' 
#' @export rdpTrain
#' 
rdpTrain <- function(sequence, taxon, K = 8, cnames = FALSE){
  taxInt    <- taxon
  sizes     <- as.numeric(table(taxon))
  taxInt    <- factor(taxon)
  taxLevels <- levels(taxInt)
  taxInt    <- as.integer(taxInt)
  prior     <- sizes / length(taxon)
  classesIn <- lapply(1:max(taxInt), function(i)which(i==taxInt) )
  presence.prob <- rdpTrainCpp(charToInt(sequence), K, cnames, classesIn, -1)
  presence.prob <- CountsToRDP(presence.prob, sizes)
  if(is.character(taxon)){
    dimnames(presence.prob) <- list(taxLevels, NULL) # Avoids copying
  }
  attr(presence.prob, "prior") <- prior
  trained.model <- list(Method = "RDPclassifier", Fitted = presence.prob)
  return(trained.model)
}


#' @name rdpClassify
#' @title Classifying with the RDP classifier
#' 
#' @description Classifying sequences by a trained presence/absence K-mer model.
#' 
#' @param sequence Character vector of sequences to classify.
#' @param trained.model A list with a trained model, see \code{\link{rdpTrain}}.
#' @param post.prob Logical indicating if posterior log-probabilities should be returned.
#' @param prior Logical indicating if classification should be done by flat priors (default)
#' or with empirical priors (prior=TRUE).
#' 
#' @details The classification step of the presence/absence method known as the RDP classifier
#' (Wang et al 2007) means looking for K-mers on all sequences, and computing the posterior
#' probabilities for each taxon using a trained model and a naive Bayes assumption. The predicted
#' taxon is the one producing the maximum posterior probability, for each \code{sequence}.
#' 
#' The classification is parallelized through RcppParallel
#' employing Intel TBB and TinyThread. By default all available
#' processing cores are used. This can be changed using the
#' function \code{\link{setParallel}}.
#' 
#' @return A character vector with the predicted taxa, one for each \code{sequence}.
#' 
#' @references Wang, Q, Garrity, GM, Tiedje, JM, Cole, JR (2007). Naive Bayesian Classifier for 
#' Rapid Assignment of rRNA Sequences into the New Bacterial Taxonomy. Applied and Enviromental 
#' Microbiology, 73: 5261-5267.
#' 
#' @author Kristian Hovde Liland and Lars Snipen.
#' 
#' @seealso \code{\link{rdpTrain}}.
#' 
#' @examples 
#' data("small.16S")
#' seq <- small.16S$Sequence
#' tax <- sapply(strsplit(small.16S$Header,split=" "),function(x){x[2]})
#' \dontrun{
#' trn <- rdpTrain(seq,tax)
#' primer.515f <- "GTGYCAGCMGCCGCGGTAA"
#' primer.806rB <- "GGACTACNVGGGTWTCTAAT"
#' reads <- amplicon(seq, primer.515f, primer.806rB)
#' predicted <- rdpClassify(unlist(reads[nchar(reads)>0]),trn)
#' print(predicted)
#' }
#' 
#' @export rdpClassify
#' 
rdpClassify <- function(sequence, trained.model, post.prob = FALSE, prior = FALSE){
  if(trained.model$Method != "RDPclassifier") stop("Trained model is not an RDPclassifier!")
  presence.prob <- trained.model$Fitted
  K <- as.integer(log2(dim(presence.prob)[2])/2)
  if(prior){
    priors <- log2(attr(presence.prob, "prior"))
  } else {
    priors <- rep(0, dim(presence.prob)[1])
  }
  X <- rdpClassifyCpp(charToInt(sequence), K, presence.prob, priors, TRUE)
  if(post.prob){
    return(data.frame(Taxon.1 = rownames(presence.prob)[X$first_ind], Post.prob.1 = X$first,
                      Taxon.2 = rownames(presence.prob)[X$second_ind], Post.prob.2 = X$second,
                      stringsAsFactors = FALSE))
  } else {
    return(rownames(presence.prob)[X$first_ind])
  }
  # taxon.hat <- rownames(presence.prob)[ret$first_ind]
  # taxon.hat[ret$first == ret$second] <- "unclassified"
  # return( taxon.hat )
}

# Older, slower versions
# rdpClassify <- function( sequence, trained.model ){
#   if( trained.model$Method != "RDPclassifier" ) stop( "Trained model is not an RDPclassifier!" )
#   presence.prob <- trained.model$Fitted
#   K <- as.integer( log2( dim( presence.prob )[2] )/2 )
#   ret <- rdpClassifyCpp( presence.prob, charToInt( sequence ), K )
#   C <- as.integer( ret$C )
#   taxon.hat <- rep( "unclassified", length(C) )
#   is.tie <- (C<0)
#   taxon.hat[!is.tie] <- rownames( presence.prob )[C[!is.tie]]
#   return( taxon.hat )
# }
# rdpTrain <- function( sequence, taxon, K=8, cnames=FALSE ){
#   taxInt <- taxon
#   if( is.character( taxon ) ){
#     taxInt  <- factor( taxon )
#     taxLevels <- levels( taxInt )
#     taxInt  <- as.integer( taxInt )
#     prior <- as.numeric( table( taxon ) /length( taxon ) )
#   }
#   presence.prob <- rdpTrainCpp( charToInt( sequence ), K,
#                                 cnames, taxInt-1, max(taxInt), as.numeric( table( taxon ) ) )
#   if( is.character( taxon ) ){
#     dimnames( presence.prob ) <- list( taxLevels, NULL ) # Avoids copying
#   }
#   attr( presence.prob, "prior" ) <- prior
#   trained.model <- list( Method="RDPclassifier", Fitted=presence.prob )
#   return( trained.model )
# }
