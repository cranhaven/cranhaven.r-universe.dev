#' Search taxonomic reference lists including concept synonomy and taxonomic hierarchy.
#'
#' @name tax
#' @aliases tax
#'
#' @export
#' @param x Species number, lettercode or species name(s)
#' @param refl Taxonomic reference list
#' @param syn Return also synonym names
#' @param concept Name of the file with an alternative taxon view stored in the reference list directory, see details.
#' @param strict Exact match or partial matching with \code{\link{grep}}
#' @param idtype is x of type 'is_id', 'is_shortletter', or 'is_scientific'
#'
#' @description Input is either species number (integer), shortletter (7 characters) or full (exact!) species name.
#'
#' @details
#'   \dfn{concept}: GermanSL is a list with a single taxon view according to the standard lists of the different taxon groups (e.g Wisskirchen and Haeupler for higher plants, see).
#'   Nevertheless a huge number of synonyms is included which allows in many cases the transformation into different concepts.
#'   For illustration the concept of \emph{Armeria maritima} from Korneck 1996 is included, which accepts e.g. \emph{Armeria maritima ssp. bottendorfensis}.
#'   \dfn{parse.taxa}: parse genus and epitheta from name strings.
#'   \dfn{taxname.removeAuthors} Remove name authors from full scientific name strings.
#'
#' @references
#'   Jansen, F. and Dengler, J. (2008) GermanSL - eine universelle taxonomische Referenzliste für Vegetationsdatenbanken. Tuexenia, 28, 239-253.
#'
#' @author Florian Jansen \email{florian.jansen@uni-rostock.de}
#'

tax <- function(x, refl = tax.refl(), syn = TRUE, concept = NULL, strict = FALSE, idtype) {
	tv_home <- tv.home()
	if(missing(refl)) {
	  refl <- tax.refl(tv_home=tv_home)
	    message('Reference list used: ', refl)
	 }
	if(is.character(refl))
	  species <- load.refl(refl) else
	    species <- refl
	if(missing(x)) stop('x is missing!')
	if(missing(idtype))
	  idtype <- ifelse(grepl("[0-9]+", x), 'is_id', ifelse(grepl("^[A-Z]+$", x) & nchar(x) < 10, 'is_shortletter', 'is_scientific'))
  # grepl("^[A-Z][a-z]+\\s[a-z]+$", strings)
	if(length(unique(idtype)) > 1)
	  stop("ID's ambiguos. Please specify idtype explicitly.") else idtype <- idtype[1]
###------ internal functions
######################################################## #
# Subsetting
select.taxa <- function(x, species, strict = FALSE) {
  if(idtype == 'is_id') {
    ## Tax numbers
    l <- species[match(x, species$TaxonUsageID),]
  }
  if(idtype == 'is_shortletter') {
    ## Selection by Lettercode
      l <- species[species$LETTERCODE %in% x,]
      if(nrow(l) > 1) l <- l[!l$SYNONYM,]
  }
  if(idtype == 'is_scientific') {
		if(strict) {
		  l <- species[match(x, species$TaxonName, nomatch=0),]
		} else {
		  s <- sapply(x, function(f) grep(f, species$TaxonName, useBytes=TRUE))
		  l <- species[unlist(s),]
		}
  }
 	if(nrow(l) == 0) message('No species found!')
 	return(l)
}
###--- end of tax internal functions ---###

#### begin function call #
### Filter
if(length(x) == 0 | is.na(x[1])) stop('Input taxon value is missing.')

if(tolower(x[1]) != 'all') {
	species <- select.taxa(x, species, strict)
 }
if(nrow(species) > 0) {
  if(!syn) species <- species[species$SYNONYM == FALSE,]
}
if(!is.character(species$TaxonUsageID))
  species$TaxonUsageID <- as.character(species$TaxonUsageID)
return(species)
}


