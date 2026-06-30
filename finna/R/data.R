#' Sample Subset of Fennica Bibliographic Records
#'
#' A 300-record sample from the Fennica dataset (Melinda) for demonstration and testing.
#'
#' @format A data frame with 300 rows and 28 variables:
#' \describe{
#'   \item{melinda_id}{Melinda record ID (001)}
#'   \item{leader}{MARC leader field}
#'   \item{008}{Fixed-length data elements (008)}
#'   \item{author_name}{Personal name of the main author (100a)}
#'   \item{author_date}{Birth/death dates or period of activity (100d)}
#'   \item{author_ID}{Combined authority ID from fields 100$0 and 264a}
#'   \item{language}{Language code(s) (041a)}
#'   \item{language_original}{Original language (041h)}
#'   \item{title_uniform}{Uniform title (240a)}
#'   \item{title}{Main title (245a)}
#'   \item{title_remainder}{Remainder of title (245b)}
#'   \item{publication_place}{Place of publication (260a)}
#'   \item{publisher}{Name of publisher (260b)}
#'   \item{physical_dimensions}{Dimensions (300c)}
#'   \item{physical_extent}{Extent (e.g., pages or volumes) (300a)}
#'   \item{publication_frequency}{Current publication frequency (310a)}
#'   \item{publication_interval}{Dates of publication or sequential designation (362a)}
#'   \item{signum}{Call number or shelf mark}
#'   \item{location_852}{Library location (852a)}
#'   \item{UDK}{Universal Decimal Classification (080a)}
#'   \item{UDK_aux}{Auxiliary UDC notation (080x)}
#'   \item{245n}{Part/section of a work (245n)}
#'   \item{genre_655}{Genre/form terms (655a)}
#'   \item{650a}{Topical subject headings (650a)}
#'   \item{general_note}{General note (500a)}
#'   \item{700a}{Added entry – personal name (700a)}
#'   \item{700_0}{Authority ID for added personal name (700$0)}
#'   \item{a.15}{Unidentified field (likely placeholder or parsing artifact)}
#' }
#' @source \url{https://www.finna.fi/}
"fennica_subset"
