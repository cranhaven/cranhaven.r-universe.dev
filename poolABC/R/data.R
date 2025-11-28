#' @name sumstats
#'
#' @docType data
#'
#' @aliases sumstats
#'
#' @title Matrix of summary statistics computed from simulated data
#'
#' @description This data set contains a set of 14 summary statistics computed
#'   from data simulated under an isolation with migration model of two
#'   populations.
#'
#' @usage sumstats
#'
#' @format a matrix with 10000 rows and 14 columns:
#'
#'   \describe{
#'
#'   \item{Sf}{fraction of sites fixed between populations.}
#'
#'   \item{Sx1}{fraction of exclusive sites for the first population.}
#'
#'   \item{Sx2}{fraction of exclusive sites for the second population.}
#'
#'   \item{SS}{fraction of sites shared between the two populations.}
#'
#'   \item{Mean_Het1}{mean expected heterozygosity of the first population.}
#'
#'   \item{Mean_Het2}{mean expected heterozygosity of the second population.}
#'
#'   \item{SD_Het1}{standard deviation across loci of the mean expected
#'   heterozygosity of the first population.}
#'
#'   \item{SD_Het2}{standard deviation across loci of the mean expected
#'   heterozygosity of the second population.}
#'
#'   \item{Mean_HetBet}{mean heterozygosity between the two populations.}
#'
#'   \item{SD_HetBet}{standard deviation across loci of the mean heterozygosity
#'   between the two populations.}
#'
#'   \item{Mean_FST}{mean pairwise FST between the two populations.}
#'
#'   \item{SD_FST}{standard deviation across loci of the mean pairwise FST
#'   between the two populations.}
#'
#'   \item{FSTQ1}{5% quantile of the mean pairwise FST distribution.}
#'
#'   \item{FSTQ2}{95% quantile of the mean pairwise FST distribution.} }
#'
#' @source simulations performed
#'
#'
#' @keywords datasets
"sumstats"


#' @name params
#'
#' @docType data
#'
#' @aliases params
#'
#' @title Matrix of simulated parameter values
#'
#' @description This data set contains a matrix of simulated parameter values.
#'   These parameter values were sampled from prior distributions and used to
#'   perform simulations under a isolation with migration model with two
#'   populations. Each row of this matrix corresponds to a different simulation.
#'
#' @usage params
#'
#' @format a matrix with 10000 rows and 8 columns:
#'
#'   \describe{
#'
#'   \item{N1}{relative size of the first population. This population
#'   corresponds to the C ecotype.}
#'
#'   \item{N2}{relative size of the second population. This population
#'   corresponds to the W ecotype.}
#'
#'   \item{Split}{time, in 4Nref scale, of the split event that creates the two
#'   populations.}
#'
#'   \item{PoolError}{error associated with DNA pooling.}
#'
#'   \item{SeqError}{error associated with DNA sequencing.}
#'
#'   \item{pM}{proportion of the genome with no barriers against gene flow. This
#'   is the proportion of simulated loci where migration occurs in both
#'   directions between the divergent ecotypes.}
#'
#'   \item{mig_CW}{scaled migration rate between the two divergent ecotypes This
#'   is the migration rate from ecotype C to ecotype W.}
#'
#'   \item{mig_WC}{scaled migration rate between the two divergent ecotypes This
#'   is the migration rate from ecotype W to ecotype C.} }
#'
#' @source simulations performed
#'
#' @keywords datasets
"params"

#' @name myparams
#'
#' @docType data
#'
#' @aliases myparams
#'
#' @title Matrix of simulated parameter values
#'
#' @description This data set contains a matrix of simulated parameter values.
#'   These parameter values were sampled from prior distributions and used to
#'   perform simulations under a isolation with migration model with two
#'   populations. Each row of this matrix corresponds to a different simulation.
#'
#' @usage myparams
#'
#' @format a matrix with 5000 rows and 8 columns:
#'
#'   \describe{
#'
#'   \item{N1}{relative size of the first population. This population
#'   corresponds to the C ecotype.}
#'
#'   \item{N2}{relative size of the second population. This population
#'   corresponds to the W ecotype.}
#'
#'   \item{Split}{time, in 4Nref scale, of the split event that creates the two
#'   populations.}
#'
#'   \item{PoolError}{error associated with DNA pooling.}
#'
#'   \item{SeqError}{error associated with DNA sequencing.}
#'
#'   \item{mCW}{migration rate between the two divergent ecotypes This
#'   is the migration rate from ecotype C to ecotype W.}
#'
#'   \item{mWC}{migration rate between the two divergent ecotypes This
#'   is the migration rate from ecotype W to ecotype C.}
#'
#'   \item{pM}{proportion of the genome with no barriers against gene flow. This
#'   is the proportion of simulated loci where migration occurs in both
#'   directions between the divergent ecotypes.} }
#'
#' @source simulations performed
#'
#' @keywords datasets
"myparams"


#' @name limits
#'
#' @docType data
#'
#' @aliases limits
#'
#' @title Matrix of prior limits
#'
#' @description this imports a matrix with the limits of the prior distribution
#'   for each parameter. Each row of the matrix is a different parameter,
#'   indicated by the row name. The matrix contains two columns, the first being
#'   the minimum value of the distribution and the second being the maximum
#'   value.
#'
#' @usage limits
#'
#' @format a matrix with 8 rows and 2 columns. Each of the rows corresponds to a
#'   different parameter:
#'
#'   \describe{
#'
#'   \item{N1}{relative size of the first population. This population
#'   corresponds to the C ecotype.}
#'
#'   \item{N2}{relative size of the second population. This population
#'   corresponds to the W ecotype.}
#'
#'   \item{Split}{time, in 4Nref scale, of the split event that creates the two
#'   populations.}
#'
#'   \item{PoolError}{error associated with DNA pooling.}
#'
#'   \item{SeqError}{error associated with DNA sequencing.}
#'
#'   \item{pM}{proportion of the genome with no barriers against gene flow. This
#'   is the proportion of simulated loci where migration occurs in both
#'   directions between the divergent ecotypes.}
#'
#'   \item{mig_CW}{scaled migration rate between the two divergent ecotypes This
#'   is the migration rate from ecotype C to ecotype W.}
#'
#'   \item{mig_WC}{scaled migration rate between the two divergent ecotypes This
#'   is the migration rate from ecotype W to ecotype C.} }
#'
#' @source simulations performed
#'
#' @keywords datasets
"limits"


#' @name rc1
#'
#' @docType data
#'
#' @aliases rc1
#'
#' @title Data frame with an example of observed data
#'
#' @description Data frame with data in the `_rc` format for 25 populations Each
#'   row of the data frame is a different site. The first 9 columns contain
#'   general information about the site, while the remaining contain the number
#'   of reads observed at that site for each of the 25 populations.
#'
#' @usage rc1
#'
#' @format a data frame with 5000 rows and 59 columns. Each of the columns
#'   corresponds to :
#'
#'   \describe{
#'
#'   \item{col1}{reference chromosome (contig).}
#'
#'   \item{col2}{reference position.}
#'
#'   \item{col3}{reference character.}
#'
#'   \item{col4}{number of alleles found in all populations.}
#'
#'   \item{col5}{allele characters in all populations (sorted by counts in all
#'   populations).}
#'
#'   \item{col6}{sum of deletions in all populations (should be zero, if not the
#'   position may not be reliable).}
#'
#'   \item{col7}{SNP type: \code{[pop, rc, rc|pop]}; pop: a SNP within or
#'   between the populations; rc: a SNP between the reference sequence character
#'   and the consensus of at least one populaton; rc|pop: both.}
#'
#'   \item{col8}{most frequent allele in all populations \code{[12345..]}.}
#'
#'   \item{col9}{second most frequent allele in all populations
#'   \code{[12345..]}.}
#'
#'   \item{col10 - col34}{frequencies of the most frequent allele (major) in the
#'   form "allele-count/coverage.}
#'
#'   \item{col35 - col59}{frequencies of the second most frequent allele (minor)
#'   in the form "allele-count/coverage".} }
#'
#' @source
#'
#'   Hernán E. Morales et al., Genomic architecture of parallel ecological
#'   divergence: Beyond a single environmental contrast. Sci. Adv.5,
#'   eaav9963(2019). DOI:10.1126/sciadv.aav9963
#'
#' @keywords datasets
"rc1"


#' @name rc2
#'
#' @docType data
#'
#' @aliases rc2
#'
#' @title Data frame with an example of observed data
#'
#' @description Data frame with data in the `_rc` format for 25 populations Each
#'   row of the data frame is a different site. The first 9 columns contain
#'   general information about the site, while the remaining contain the number
#'   of reads observed at that site for each of the 25 populations.
#'
#' @usage rc2
#'
#' @format a data frame with 5000 rows and 59 columns. Each of the columns
#'   corresponds to :
#'
#'   \describe{
#'
#'   \item{col1}{reference chromosome (contig).}
#'
#'   \item{col2}{reference position.}
#'
#'   \item{col3}{reference character.}
#'
#'   \item{col4}{number of alleles found in all populations.}
#'
#'   \item{col5}{allele characters in all populations (sorted by counts in all
#'   populations).}
#'
#'   \item{col6}{sum of deletions in all populations (should be zero, if not the
#'   position may not be reliable).}
#'
#'   \item{col7}{SNP type: \code{[pop, rc, rc|pop]}; pop: a SNP within or
#'   between the populations; rc: a SNP between the reference sequence character
#'   and the consensus of at least one populaton; rc|pop: both.}
#'
#'   \item{col8}{most frequent allele in all populations \code{[12345..]}.}
#'
#'   \item{col9}{second most frequent allele in all populations
#'   \code{[12345..]}.}
#'
#'   \item{col10 - col34}{frequencies of the most frequent allele (major) in the
#'   form "allele-count/coverage.}
#'
#'   \item{col35 - col59}{frequencies of the second most frequent allele (minor)
#'   in the form "allele-count/coverage".} }
#'
#' @source
#'
#'  Hernán E. Morales et al., Genomic architecture of parallel ecological
#'  divergence: Beyond a single environmental contrast. Sci. Adv.5,
#'  eaav9963(2019). DOI:10.1126/sciadv.aav9963
#'
#' @keywords datasets
"rc2"
