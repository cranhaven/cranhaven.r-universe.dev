#' Dataset for nitrogren fluxes in a Trinidadian mountain stream (Collins 2016)
#'
#' Dataset built from the article "Fish introductions and light modulate food
#' web fluxes in tropical streams: a whole-ecosystem experimental approach” by
#' Collins et al. (2016).
#'
#' In the original study, 15N-enriched ammonium was dripped into two mountain
#' streams in Trinidad (Upper Lalaja stream and Lower Lalaja stream) and
#' samples of the different foodweb compartments were taken during the drip and
#' after the drip in several transects in each stream. The transects were
#' located at different locations downstream of each drip. There were three
#' transects per stream. The drip phase lasted 10 days, and the post-drip phase
#' lasted 30 days. The complete dataset from the original study is available in the
#' \code{trini_mod} model shipped with the \code{isotracer} package.
#'
#' The \code{lalaja} dataset is a subset of the full dataset and is used for
#' illustrative purpose in the "Trinidadian streams" case study, which is part
#' of the documentation of \code{isotracer}. It contains only the data for the
#' Upper Lalaja stream, and for some but not all of the foodweb compartments.
#'
#' For more details about the dripping regime and how to use this dataset in a
#' network model, one should refer to the case study in the \code{isotracer}
#' package documentation.
#' 
#' @source
#'
#' This network model contains data from the original article: Collins, Sarah
#' M., Steven A. Thomas, Thomas Heatherly, Keeley L. MacNeill, Antoine
#' O.H.C. Leduc, Andrés López-Sepulcre, Bradley A. Lamphere, et al. 2016. “Fish
#' Introductions and Light Modulate Food Web Fluxes in Tropical Streams: A
#' Whole-Ecosystem Experimental Approach.” Ecology, <doi:10.1002/ecy.1530>.
#'
#' This dataset was also used in the paper: López-Sepulcre, Andrés, Matthieu
#' Bruneaux, Sarah M. Collins, Rana El-Sabaawi, Alexander S. Flecker, and
#' Steven A. Thomas. 2020. “A New Method to Reconstruct Quantitative Food Webs
#' and Nutrient Flows from Isotope Tracer Addition Experiments.” The American
#' Naturalist 195 (6): 964–85. <doi:10.1086/708546>.
#'
#' @format Tibble with columns
#' \describe{
#'   \item{stream}{Stream identity. It is always "UL" (for "Upper lalaja") in
#'     this dataset. See the model \code{trini_mod} also shipped with the
#'     package for the full dataset from the original Collins et al. study,
#'     including data from the Lower Lajaja stream.}
#'   \item{transect}{Transect identity. Three transects were sampled downstream
#'     of the drip location: \code{c("transect.1", "transect.2",
#'     "transect.3")}.}
#'   \item{compartment}{Foodweb compartments. Eight compartments are included
#'     in this dataset: "NH4", dissolved ammonium; "NH3", dissolved nitrate;
#'     "epi", epilithon (primary producers growing on the surface of rocks on
#'     the stream bed); "FBOM", fine benthic organic material; "tricor",
#'     \emph{Tricorythodes} (invertebrate); "pseph", \emph{Psephenus}
#'     (invertebrate); "petro", \emph{Petrophila} (invertebrate); "arg",
#'     \emph{Argia} (invertebrate).}
#'   \item{mgN.per.m2}{Size of compartment, in mg of nitrogen per m2.}
#'   \item{prop15N}{Proportion of 15N nitrogen in a compartment nitrogen pool
#'     (i.e. 15N / (15N + 14N)).}
#'   \item{time.days}{Sampling time, in days.}
#' }

"lalaja"
