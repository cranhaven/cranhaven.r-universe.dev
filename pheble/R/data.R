#' Data from: The utility of cranial ontogeny for phylogenetic inference: a case study in crocodylians using geometric morphometrics
#'
#' Abstract: The degree to which the ontogeny of organisms could facilitate our understanding of phylogenetic relationships
#' has long been a subject of contention in evolutionary biology. The famed notion that ‘ontogeny recapitulates phylogeny’ has
#' been largely discredited, but there remains an expectation that closely related organisms undergo similar morphological
#' transformations throughout ontogeny. To test this assumption, we used three-dimensional geometric morphometric methods to
#' characterize the cranial morphology of 10 extant crocodylian species and construct allometric trajectories that model the
#' post-natal ontogenetic shape changes. Using time-calibrated molecular and morphological trees, we employed a suite of
#' comparative phylogenetic methods to assess the extent of phylogenetic signal in these trajectories. All analyses largely
#' demonstrated a lack of significant phylogenetic signal, indicating that ontogenetic shape changes contain little phylogenetic
#' information. Notably, some Mantel tests yielded marginally significant results when analysed with the morphological tree,
#' which suggest that the underlying signal in these trajectories is correlated with similarities in the adult cranial morphology.
#' However, despite these instances, all other analyses, including more powerful tests for phylogenetic signal, recovered
#' statistical and visual evidence against the assumption that similarities in ontogenetic shape changes are commensurate with
#' phylogenetic relatedness and thus bring into question the efficacy of using allometric trajectories for phylogenetic inference.
#'
#' @format ## `ph_crocs`
#' A data frame of Procrustes superimposed shape data with 183 rows and 236 columns:
#' \describe{
#'   \item{Biosample}{Biosample}
#'   \item{Species}{Species}
#'   ...
#' }
#' @source
#' Downloaded from
#' <\doi{dx.doi.org/10.5061/dryad.14fn1}>
"ph_crocs"

#' Data from: Sexually mediated phenotypic variation within and between sexes as a continuum structured by ecology: The mosaic nature of skeletal variation across body regions in Threespine stickleback (Gasterosteus aculeatus L.)
#'
#' Abstract: Ecological character displacement between the sexes, and sexual selection, integrate into a convergent set of
#' factors that produce sexual variation. Ecologically-modulated, sexually mediated variation within and between
#' sexes may be a major contributor to the amount of total variation that selection can act on in species. Threespine
#' stickleback (Gasterosteus aculeatus) display rapid adaptive responses and sexual variation in many phenotypic
#' traits. We examined phenotypic variation in the skull, pectoral and pelvic girdles of threespine stickleback from
#' two freshwater and two coastal marine sites on the Sunshine Coast of British Columbia, Canada, using an approach
#' that avoids a priori assumptions about bimodal patterns of variation. We quantified shape and size of the cranial,
#' pectoral and pelvic regions of sticklebacks in marine and freshwater habitats using 3D geometric morphometrics and
#' an index of sexually mediated variation. We show that the expression of phenotypic variation is structured in part
#' by the effects of both habitat marine vs freshwater and the effects of individual sites within each habitat.
#' Relative size exerts variable influence, and patterns of phenotypic variation associated with sex vary among body
#' regions. This fine-grained quantification of sexually mediated variation in the context of habitat difference and
#' different anatomical structures indicates a complex relationship between genetically inferred sex and environmental
#' factors, demonstrating that the interplay between shared genetic background and sexually mediated, ecologically-
#' based selective pressures structures the phenotypic expression of complex traits.
#'
#' @format ## `ph_stickleback`
#' A data frame of Procrustes superimposed shape data with 190 rows and 214 columns:
#' \describe{
#'   \item{Biosample}{Biosample}
#'   \item{Habitat}{Habitat}
#'   \item{Population}{Population}
#'   \item{Sex}{Sex}
#'   ...
#' }
#' @source
#' Downloaded from
#' <\doi{doi.org/10.5061/dryad.xd2547dkw}>
"ph_stickleback"
