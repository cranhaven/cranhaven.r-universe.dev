#' @title Pairwise Similarity Matrix
#'
#' @description
#' The \code{kmeRs_similarity_matrix} function generates a pairwise similarity score
#' matrix for for k length given k-mers vs. all possible k-mers combination.
#' The pairwise similarity score is calculated using PAM or BLOSUM substitution matrix;
#' 30, 40, 70, 120, 250 and 62, 45, 50, 62, 80, 100 matrix versions are available for
#' PAM or BLOSUM, respectively. The results are evaluated by global similarity score;
#' higher similarity score indicates more similar sequences for BLOSUM and opposite for
#' PAM matrix.
#'
#' @aliases kmeRs_similarity_matrix
#'
#' @param q query vector with given k-mers
#' @param x kmers to search the query vector against. If unspecified, \code{q} will 
#' be compared to either other k-mers within \code{q} (\code{compare.all = FALSE}),
#' or all possible combinations specified by the parameter \code{k}
#' @param k length of k-mers to calculate the similarity matrix for, defaults to 3; e.g. for DNA, N = 4^3 = 64 combinations if \code{k = 3};
#' @param seq.type type of sequence in question, either 'DNA' or 'AA' (default);
#' this will also modify \code{q} accordingly, if \code{q} is unspecified.
#' @param compare.all if \code{TRUE}, the query vector will be compared to all
#' possible combinations of k-mers (defaults to \code{FALSE})
#' @param align.type type of alignment, either \code{global} or \code{local}.
#' \code{global} uses Needleman-Wunsch global alignment to calculate scores, while
#' \code{local} represents Smith-Waterman local alignment instead
#' @param submat substitution matrix, default to 'BLOSUM62'; other choices are
#' 'BLOSUM45', 'BLOSUM50', 'BLOSUM62', 'BLOSUM80', 'BLOSUM100', 'PAM30',
#' 'PAM40', 'PAM70', 'PAM120' or 'PAM250'
#' @param save_to_file if specified, the results will be saved to the path in
#' comma-separated format (.CSV)
#' @param ... other parameters, e.g. gap opening/extension penalties (\code{gapOpening},
#' \code{gapExtension}), or DNA match/mismatch scores (\code{na.match}, \code{na.mismatch})
#'
#' @return similarity matrix is returned as a data.frame
#'
#' @examples
#' # Simple BLOSUM62 similarity matrix for all amino acid nucleotides
#' kmeRs_similarity_matrix(submat = "BLOSUM62")
#' 
#' @importFrom utils write.csv2
#' @importFrom BiocGenerics score
#' 
#' @export
#' 
kmeRs_similarity_matrix <- function(q = NULL, x = NULL,
	align.type = "global", k = 3, seq.type = "AA",
	submat = ifelse(
		test = (match.arg(toupper(seq.type), c("DNA", "AA")) == "AA"),
		yes = "BLOSUM62", no = NA),
	compare.all = FALSE, save_to_file = NULL, ...) {
	
	# this version supports AA and DNA only
	seq.type <- match.arg(toupper(seq.type), c("DNA", "AA"))
	if (seq.type == "AA") {
		seq.alpha <- unlist(strsplit("GALMFWKQESPVICYHRNDT", ""))
	} else {
		seq.alpha <- unlist(strsplit("ATCG", ""))
		# should the extended nucleic/amino acid alphabets be included?
		# not implemented in this version
		#if (extended) {
		#	seq.alpha <- unlist(strsplit("ATCGWSMKRYBDHVNZ", ""))
		#}
	}
	
	# rewriting the x parameters as follows
	if (is.null(q)) {
		# no kmers_specified -> set according to sequence type as originally intended
		q <- seq.alpha
	} else {
		q <- toupper(q)
	}
	if (is.null(x)) {
		# unspecified; compare to q if compare.all is left as default
		if (compare.all) {
			x <- kmeRs_generate_kmers(k, bases = seq.alpha)
		} else {
			x <- q
		}
	}
			
	# matrix initialization
	kmers_dist_matrix <- matrix(NA, ncol = length(q), nrow = length(x))
	kmers_dist_matrix <- data.frame(kmers_dist_matrix, row.names = x)
    colnames(kmers_dist_matrix) <- q
		
	# Calculate the distance matrix
	# column ops are vectorized in this version instead
	for (i in 1:dim(kmers_dist_matrix)[1]) {
		kmers_dist_matrix[i,] <- as.vector(sapply(q, function(col) {
		  BiocGenerics::score(kmeRs_twoSeqSim(col, x[i], 
				seq.type = seq.type, align.type = align.type, submat = submat, verbose = FALSE, ...))
		}))
	}
    
	# Save to the file if requested
	if (!is.null(save_to_file)) {
		utils::write.csv2(x = kmers_dist_matrix, file = save_to_file)
	}
	return(kmers_dist_matrix)
}

#' @title kmeRs generate kmers
#' @param k times
#' @param bases follow the kmeRs_similarity_matrix()
#'
kmeRs_generate_kmers <- function(k, bases) {
  kmers.list <- ''
  for (i in 1:k) {
    kmers.list <- unlist(lapply(kmers.list, function (kmer) {
      paste0(kmer, bases)
    }))
  }
  return(kmers.list)
}

