#' FourGamete Test
#'
#' @description The four-gamete test is based on the infinite-sites model which assumes that the probability of the same mutation occurring twice (recurrent or parallel mutations) and the probability of a mutation back to the original state (reverse mutations) are close to zero. Without these types of mutations, the only explanation for observing the four dilocus genotypes (example below) is recombination (Hudson and Kaplan 1985, Genetics 111:147-164). Thus, the presence of all four gametes is also called phylogenetic incompatibility.
#' @description FourgameteP contains a single function: '\strong{FGf()}'.
#' @description This function determines if, across all pairwise comparisons, it is possible to find all four gametes
#'
#'@description \strong{Example:}
#'
#'
#'
#'\itemize{
#'\item{ }{...............Locus1....Locus2}
#'\item{ }{ Gamete 1.......A.........A}
#'\item{ }{ Gamete 2.......A.........B}
#'\item{ }{ Gamete 3.......B.........A}
#'\item{ }{ Gamete 4.......B.........B}
#'}
#'@description While the example above indicates two alleles at each of two loci, FourGamete will output all possible allele combinations between two loci.
#'
#' @details Please make sure that your data fits all of the following requirements:
#'\itemize{
#'\item{1) }{Data must represent alleles at haploid loci (maximum of 26 loci)}
#'\item{2) }{Rows are individuals and columns are loci}
#'\item{3) }{Loci must labeled in the top row with names containing no spaces}
#'\item{4) }{Do not include metadata columns (e.g., individual names or other }
#'\item{5) }{Alleles at a locus must be numerical}
#'\item{6) }{Missing data should be coded "0"}
#'\item{7) }{Files should be saved as tab-delimited text}
#'}
#'
#' @docType package
#'
#'
#' @author Milton T Drott \email{mtd66@cornell.edu}
#'
#' @name FourgameteP
NULL

