#' @name contax.trim
#' @docType data
#' @title The ConTax data set
#' 
#' @description The trimmed version of the ConTax data set.
#' 
#' @usage 
#' data(contax.trim)
#' 
#' @details 
#' \code{contax.trim} is a \code{data.frame} object containing 38 781 full-length 16S rRNA 
#' sequences. It is the trimmed version of the full data set (see below). Large taxa (many sequences) have
#' been trimmed as described in Vinje et al. (2016) to obtain a data set with a more even representation of
#' the prokaryotic taxonomy.
#' 
#' The \code{contax.full} is the full consensus taxonomy data set as described in Vinje et al. (2016). The data
#' set is too large for CRAN and thus available as a separate package \code{microcontax.data}. See example
#' below for how to obtain \code{contax.full}.
#' 
#' The Header of every sequence starts with a unique tag, in this case the text "ConTax" and some integer.
#' This is followed by a token describing the origin of the sequence. It is typically
#' 
#' "Intersection=SRG"
#' 
#' meaning it is found in both the Silva, RDP and Greengenes data repository. Intersections can also be
#' SR, SG and RG if the sequence was found in two repositories only. The taxonomy information for each
#' sequence is found in the third token. It follows a commonly used format:
#' 
#' "k__<...>;p__<...>;c__<...>;o__<...>;f__<...>;g__<...>;"
#' 
#' where <...> is some proper text. The letters, followed by a double underscore, refer to the taxonomic levels
#' Domain (Kingdom), Phylum, Class, Order, Family and Genus.
#' Here is an example of a proper string:
#' 
#' "k__Bacteria;p__Firmicutes;c__Bacilli;o__Bacillales;f__Staphylococcaceae;g__Staphylococcus;"
#' 
#' As long as this format is used the taxonomy information can be extracted by the supplied 
#' extractor-functions \code{\link{getDomain}}, \code{\link{getPhylum}},...,\code{\link{getGenus}}.
#' 
#' 
#' @author Hilde Vinje, Kristian Hovde Liland, Lars Snipen.
#' 
#' @seealso \code{\link{medoids}}, \code{\link{getDomain}}, \code{\link[microcontax.data]{contax.full}}.
#' 
#' @examples 
#' data(contax.trim)
#' dim(contax.trim)
#' 
#' # Write to FASTA-file
#' \dontrun{
#' writeFasta(contax.trim,out.file="ConTax_trim.fasta")
#' 
#' # Install microcontax.data with the BIG contax.full data set
#' if (!requireNamespace("microcontax.data", quietly = TRUE)) {
#'  install.packages("microcontax.data")
#' }
#' # Load data
#' data("contax.full", package = "microcontax.data")
#' }
#' 
NULL


#' @name medoids
#' @docType data
#' @title The ConTax medoids
#' 
#' @description The genus medoids from the ConTax data set.
#' 
#' @usage 
#' data(medoids)
#' 
#' @details 
#' \code{medoids} is a \code{data.frame} object containing the medoide sequences for each genus in 
#' the ConTax data sets (both \code{contax.trim} and \code{contax.full}).
#' 
#' The medoide sequence in a genus is the sequence having the smallest sum of distance to all other members
#' of the same genus. Thus, it is the sequence closest to the centre of the genus. The medoids can be used as
#' the representative of each genus, e.g. for building trees for the entire taxonomy.
#' 
#' The taxonomy information for each sequence can be extracted from the \code{Header} column by the supplied
#' extractor-functions \code{\link{getDomain}}, \code{\link{getPhylum}},...,\code{\link{getGenus}}.
#' 
#' @author Hilde Vinje, Kristian Hovde Liland, Lars Snipen.
#' 
#' @seealso \code{\link[microcontax.data]{contax.full}}, \code{\link{getDomain}}.
#' 
#' @examples 
#' data(medoids)
#' summary(medoids)
#' 
NULL


#' @name taxonomy.table
#' @docType data
#' @title Taxonomy look-up table
#' 
#' @description A data frame consisting of the taxonomy information used in the ConTax data sets.
#' 
#' @usage 
#' data(taxonomy.table)
#' 
#' @details 
#' \code{taxonomy.table} is a \code{data.frame} consisting of the seven columns Domain,
#' Phylum, Class, Order, Family, Genus and LPSN. The first six are taxonomy informations, the last
#' is "Yes" or "No" indiocating if the Genus listed is also found in the List of prokaryotic names
#' with standing in nomenclature (LPSN) database, see http://www.bacterio.net/.
#' 
#' Each row contains the taxonomy information for a genus, hence the number of rows equals the number
#' of unique genera.
#' 
#' To quickly look-up the higher rank taxonomy for a given genus, see the function \code{\link{genusLookup}}.
#' 
#' @author Hilde Vinje, Kristian Hovde Liland, Lars Snipen.
#' 
#' @seealso \code{\link{genusLookup}}, \code{\link[microcontax.data]{contax.full}},
#' \code{\link{contax.trim}}, \code{\link{getDomain}}.
#' 
#' @examples 
#' data(taxonomy.table)
#' dim(taxonomy.table)
#' taxonomy.table[1:10,]
#' genusLookup(taxonomy.table$Genus[1:10], rank = "Family")
#' 
NULL
