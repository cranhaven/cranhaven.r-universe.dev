#' Human MetO sites oxidized by hydrogen peroxide treatment.
#'
#' A dataset containing data regarding human MetO sites oxidized by H2O2.
#'
#' @format A data frame with 4472 rows and 15 variables:
#' \describe{
#'   \item{prot_id}{UniProt ID of the oxidized protein}
#'   \item{prot_name}{the protein's name}
#'   \item{met_pos}{the position of the MetO site in the primary structure}
#'   \item{met_vivo_vitro}{conditions under which the oxidation experiment was carried out}
#'   \item{MetOsites}{array with all the sites oxidized in that protein}
#'   \item{site_id}{primary key identifying the site}
#'   \item{positive}{sequence environment of the MetO site}
#'   \item{control}{sequence environment of a non oxidized Met from the same protein}
#'   \item{IDP}{Intrinsically Disordered Proteins, 0: the protein is not found in DisProt; 1: the protein contains disordered regions; 2: the protein may contain disordered regions but the experimental evidences are ambiguous}
#'   \item{IDR}{Intrinsically Disordered Region, TRUE: the MetO site belong to the IDR, FALSE: the MetO site doesn't belong to the IDR}
#'   \item{abundance}{protein abundance, in ppm}
#'   \item{N}{protein length, in number of residues}
#'   \item{met}{number of methionine residues}
#'   \item{fmet}{relative frequency of Met in that protein}
#'   \item{prot_vivo_vitro}{whether the protein has been described to be oxidized in vivo, in vitro or under both conditions}
#' }
#' @source \url{https://metosite.uma.es/}
"hmeto"
