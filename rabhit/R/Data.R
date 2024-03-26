# Documentation and definitions for data and constants

#### Data ####

#' Example IGH human naive b-cell repertiore
#'
#' A \code{data.frame} of example IGH human naive b-cell repertiore data from five
#' individuals (see references). Overall, the data set includes 6 samples.
#' A single individual has two samples (Individual I5),
#' one is short read sequences from  BIOMED-2 protocol primers for framework 2 region (The sample is annotated I5_FR2).
#'
#'
#' @name samples_db
#' @docType data
#' @format A \code{data.frame} in Change-O format (\url{https://changeo.readthedocs.io/en/stable/standard.html}) containing the following columns:
#' \itemize{
#'   \item \code{'SUBJECT'}: subject names
#'   \item \code{'V_CALL'}: V allele call(s) (in an IMGT format)
#'   \item \code{'D_CALL'}: D allele call(s) (in an IMGT format, only for heavy chains)
#'   \item \code{'J_CALL'}: J allele call(s) (in an IMGT format)
#' }
#'
#'
#' @references Gidoni, Moriah, \emph{et al}. Mosaic deletion patterns of the human antibody heavy chain
#' gene locus shown by Bayesian haplotyping.
#' \emph{Nature Communications}. 10.1 (2019): 628.
#' @keywords data antibody AIRR NGS
"samples_db"

#' Example haplotype inference results
#'
#' A \code{data.frame} of example haplotype infrence results from \link{createFullHaplotype} after double chromosome deletion inference via \link{deletionsByBinom} and non reliable V genes detection via \link{nonReliableVGenes}.
#' Source data is a colletion of IGH human naive b-cell repertiore data from five
#' individuals (see references). Overall, the data set includes 6 samples.
#' A single individual has two samples (Individual I5),
#' one is short read sequences from  BIOMED-2 protocol primers for framework 2 region (The sample is annotated I5_FR2).
#'
#'
#' @name samplesHaplotype
#' @docType data
#' @format A \code{data.frame}, in which each row is the haplotype inference summary of a gene of an individual, from the column selected to prefrom the haplptype infrence on.
#' @seealso See \link{createFullHaplotype} for detailed column descriptions.
#'
#' @references Gidoni, Moriah, \emph{et al}. Mosaic deletion patterns of the human antibody heavy chain
#' gene locus shown by Bayesian haplotyping.
#' \emph{Nature Communications}. 10.1 (2019): 628.
#' @keywords data antibody AIRR NGS haplotype
"samplesHaplotype"

#' Human IGHV germlines
#'
#' A \code{character} vector of all 342 human IGHV germline gene segment alleles
#' in IMGT Gene-db release 2018-12-4.
#'
#' @name HVGERM
#' @docType data
#' @format Values correspond to IMGT-gaped nuceltoide sequences (with
#' nucleotides capitalized and gaps represented by '.').
#'
#' @references Xochelli \emph{et al}. (2014) Immunoglobulin heavy variable
#' (IGHV) genes and alleles: new entities, new names and implications for
#' research and prognostication in chronic lymphocytic leukaemia.
#' \emph{Immunogenetics}. 67(1):61-6.
#' @keywords data
"HVGERM"

#' Human IGHD germlines
#'
#' A \code{character} vector of all 37 human IGHD germline gene segment alleles
#' in IMGT Gene-db release 2018-12-4.
#'
#' @name HDGERM
#' @docType data
#' @format Values correspond to IMGT nuceltoide sequences.
#'
#' @references Xochelli \emph{et al}. (2014) Immunoglobulin heavy variable
#' (IGHV) genes and alleles: new entities, new names and implications for
#' research and prognostication in chronic lymphocytic leukaemia.
#' \emph{Immunogenetics}. 67(1):61-6.
#' @keywords data
"HDGERM"

#' Human IGHJ germlines
#'
#' A \code{character} vector of all 13 human IGHJ germline gene segment alleles
#' in IMGT Gene-db release 2018-12-4.
#'
#' @name HJGERM
#' @docType data
#' @format Values correspond to IMGT nuceltoide sequences.
#'
#' @references Xochelli \emph{et al}. (2014) Immunoglobulin heavy variable
#' (IGHV) genes and alleles: new entities, new names and implications for
#' research and prognostication in chronic lymphocytic leukaemia.
#' \emph{Immunogenetics}. 67(1):61-6.
#' @keywords data
"HJGERM"

#' Human IGKV germlines
#'
#' A \code{character} vector of all 342 human IGKV germline gene segment alleles
#' in IMGT Gene-db release 2019-11-18.
#'
#' @name KVGERM
#' @docType data
#' @format Values correspond to IMGT-gaped nuceltoide sequences (with
#' nucleotides capitalized and gaps represented by '.').
#' @keywords data
"KVGERM"

#' Human IGKJ germlines
#'
#' A \code{character} vector of all 342 human IGKJ germline gene segment alleles
#' in IMGT Gene-db release 2019-11-18.
#'
#' @name KJGERM
#' @docType data
#' @format Values correspond to IMGT-gaped nuceltoide sequences (with
#' nucleotides capitalized and gaps represented by '.').
#' @keywords data
"KJGERM"

#' Human IGLV germlines
#'
#' A \code{character} vector of all 342 human IGLV germline gene segment alleles
#' in IMGT Gene-db release 2019-11-18.
#'
#' @name KVGERM
#' @docType data
#' @format Values correspond to IMGT-gaped nuceltoide sequences (with
#' nucleotides capitalized and gaps represented by '.').
#' @keywords data
"LVGERM"

#' Human IGLJ germlines
#'
#' A \code{character} vector of all 342 human IGLJ germline gene segment alleles
#' in IMGT Gene-db release 2019-11-18.
#'
#' @name LJGERM
#' @docType data
#' @format Values correspond to IMGT-gaped nuceltoide sequences (with
#' nucleotides capitalized and gaps represented by '.').
#' @keywords data
"LJGERM"

#' Human germlines
#'
#' A list of the germline genes from the human immunoglobulin loci
#'
#' @name GERM
#' @docType data
#' @format Values correspond to IMGT-gaped nuceltoide sequences (with
#' nucleotides capitalized and gaps represented by '.').
#' @keywords data
"GERM"

#' Human Gene order on the chromosome
#'
#' A list of the chains genes order by their location on the chromosomes
#'
#' @name GENE.loc
#' @docType data
#' @format A nested list with three enteries, each a vector of the IG chains (IGH, IGL, and IGK) genes ordered by location.
#' @keywords data
"GENE.loc"

#### Sysdata ####

# Human IG germlines location @format A nested list with three enteries, each a vector of the IG chains (IGH, IGL, and IGK) genes ordered by location.
# @rdname GENE.loc
#'GENE.loc'

# Human IG germlines pseudo genes @format A nested list with three enteries, each a vector of the IG chains (IGH, IGL, and IGK) pseudo genes.  @rdname
# PSEUDO
#'PSEUDO'

# Human IGH gene relative usage cutoffs for the binomial test @format A nested list with of data frames.  \describe{ \item{GENE}{gene name}
# \item{min_frac}{gene cutoff for binomial test} } @rdname Binom.test.gene.cutoff
#'Binom.test.gene.cutoff'
