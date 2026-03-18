#' Cytochrome C Oxidase Sequences of Geophagus Eartheaters
#'
#' @description
#' This is a set of 354 sequences of the mitochondrial gene cytochrome c oxidase
#' subunit I (COI-5P) of the eartheaters of the _Geophagus sensu stricto_ species group
#' downloaded from GenBank. Most of these sequences are from the data analysed by
#' Ximenes et al. (2021).
#'
#' @format
#' An object of class [DNAbin][ape::DNAbin]
#'
#' @source
#' Ximenes AM, Bittencourt PS, Machado VN, Hrbek T, Farias IP. 2021.
#' Mapping the hidden diversity of the Geophagus sensu stricto species group
#' (Cichlidae: Geophagini) from the Amazon basin. PeerJ 9:e12443.
"geophagus"

#' Geophagus Earthearts Associated Metadata
#'
#' @description
#' This is the associated metadata for the 354 sequences of the mitochondrial gene
#' cytochrome c oxidase subunit I (COI-5P) of the _Geophagus sensu stricto_ species
#' group downloaded from GenBank and stored in [geophagus].
#'
#' @format
#' A data frame with 354 rows and 19 columns:
#' \describe{
#'    \item{scientificName}{scientific name}
#'    \item{scientificNameGenBank}{scientific name following NCBI taxonomy}
#'    \item{class}{class}
#'    \item{order}{order}
#'    \item{family}{family}
#'    \item{genus}{genus}
#'    \item{dbid}{NCBI Nucleotide Database internal ID}
#'    \item{gbAccession}{NCBI Nucleotide Database accession number}
#'    \item{gene}{Gene acronym}
#'    \item{length}{Sequence length in base pairs (bp)}
#'    \item{organelle}{Organelle from which gene was sequenced}
#'    \item{catalogNumber}{An identifier for the record within a data set or collection}
#'    \item{country}{Name of the Country followed by sampling locality (when available)}
#'    \item{publishedAs}{Title of the article which generated the sequences}
#'    \item{publishedIn}{Journal which published the article}
#'    \item{publishedBy}{A person, group, or organization responsible for depositing the sequence}
#'    \item{date}{Date published}
#'    \item{decimalLatitude}{Latitude in decimal degrees}
#'    \item{decimalLongitude}{Longitude in decimal degrees}
#' }
"geophagus_info"

#' Geophagus Eartheaters Ultrametric Tree
#'
#' @description
#' This is a Maximum Clade Credibility (MCC) tree containing unique haplotypes from
#' [geophagus] estimated using BEAST2 v2.6.7. Unique haplotypes
#' were select using [hap_collapse].
#'
#' @format
#' An object of class [treedata][tidytree::treedata-class].
#'
"geophagus_beast"

#' Geophagus Eartheaters Phylogram
#'
#' @description
#' This is a Maximum Likelihood Estimation Tree containing unique haplotypes from
#' [geophagus] estimated using RAxML-NG v. 1.1.0-master.
#' Unique haplotypes were select using [hap_collapse].
#'
#' @format
#' An object of class [treedata][tidytree::treedata-class].
"geophagus_raxml"

#' Geophagus Eartheaters Posterior Trees
#'
#' @description
#' This is a set of 100 ultrametric trees sampled from the posterior trees used to
#' estimate [geophagus_beast] using BEAST2 v2.6.7. Meant to be
#' used for [confidence_intervals] estimation.
#'
#' @format
#' An object of class [multiphylo][ape::multiphylo]
"geophagus_posterior"

#' Geophagus Eartheaters Bootstrap Trees
#'
#' @description
#' This is a set of 100 Maximum Likelihood trees sampled from bootstrap trees used to
#' estimate [geophagus_raxml] using RAxML-NG v. 1.1.0-master.
#' Meant to be used for [confidence_intervals] estimation.
#'
#' @format
#' An object of class [multiphylo][ape::multiphylo]
"geophagus_bootstraps"

#' Geophagus Eartheaters Species Partitions
#'
#' @description
#' This is a data frame containing species delimitation partitions for all the
#' 137 unique haplotypes of [geophagus] generated using
#' functions contained in this package. Use [report_delim]
#' to check number of lineages per method.
#'
#' @format
#' A dataframe with 137 rows and 9 columns:
#' \describe{
#'    \item{labels}{Unique haplotype labels}
#'    \item{abgd}{species partitions for `ABGD` method}
#'    \item{asap}{species partitions for `ASAP` method}
#'    \item{bgmyc}{species partitions for `bGMYC` method}
#'    \item{gmyc}{species partitions for `GMYC` method}
#'    \item{locmin}{species partitions for `locmin` method}
#'    \item{morph}{species partitions following NCBI taxonomy}
#'    \item{mptp}{species partitions for `mPTP` method}
#'    \item{ptp}{species partitions for `PTP` method}
#'    }
"geophagus_delims"
