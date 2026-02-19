#' @title Dataset: Cutler's Global sea level curve (0-140000 BP)
#' @description Global sea level curve of Cutler et al. (2003) from 0 to 140000 BP. 
#' @source Cutler, Kirsten B; Edwards, Ross L; Taylor, Frederick W; Cheng, H; Adkins, Jess F; Gallup, Christina D; Cutler, P M; Burr, George S; Bloom, Arthur L (2003): Rapid sea-level fall and deep-ocean temperature change since the last interglacial period. Earth and Planetary Science Letters, 206(3-4), 253-271. \doi{https://doi.org/10.1016/S0012-821X(02)01107-X}
#' @format A vector with:
#' \describe{
#'  \item{year_before_after_present}{years before or after present}
#'  \item{sea_level_m}{meters below or above the reference sea level expressed in m.}
#'
#' }
#' @examples
#' curve <- cutler
#' 
"cutler"

#' @title Dataset: Lambeck's Global sea level curve (0-35000 BP)
#' @description Global sea level curve of Lambeck et al. (2014) from 0 to 35000 BP. 
#' @source Lambeck, Kurt; Rouby, Hélène; Purcell, Anthony; Sun, Y; Sambridge, Malcom (2014): Sea level and global ice volumes from the Last Glacial Maximum to the Holocene. Proceedings of the National Academy of Sciences, 111(43), 15296-15303. \doi{https://doi.org/10.1073/pnas.1411762111}
#' @format A vector with:
#' \describe{
#'  \item{year_before_after_present}{years before or after present}
#'  \item{sea_level_m}{meters below or above the reference sea level expressed in m.}
#'
#' }
#' @examples
#' curve <- lambeck
#' 
"lambeck"

#' @title Dataset: Bintanja's Global sea level curve (0-3000000 BP)
#' @description Global sea level curve of Bintanja & van de Wal (2008) from 0 to 3000000 BP. 
#' @source Bintanja, R., van de Wal, R. (2008) North American ice-sheet dynamics and the onset of 100,000-year glacial cycles. Nature 454, 869–872 (2008). \doi{https://doi.org/10.1038/nature07158}
#' @format A vector with:
#' \describe{
#'  \item{year_before_after_present}{years before or after present}
#'  \item{sea_level_m}{meters below or above the reference sea level expressed in m.}
#'
#' }
#' @examples
#' curve <- bintanja
#' 
"bintanja"

#' @title Dataset: IPCC future predictions (2021-2100)
#' @description Mean global sea level rise according to different scenarios (ssp1,ssp2,ssp3,ssp5) for 2021-2040, 2041-2060 and 2081-2100. The global means were calculated from global raster datasets available for download in the interactive IPCC atlas.
#' 
#' @source IPCC, Kirsten B; Edwards, Ross L; Taylor, Frederick W; Cheng, H; Adkins, Jess F; Gallup, Christina D; Cutler, P M; Burr, George S; Bloom, Arthur L (2003): Rapid sea-level fall and deep-ocean temperature change since the last interglacial period. Earth and Planetary Science Letters, 206(3-4), 253-271. \doi{https://doi.org/10.1016/S0012-821X(02)01107-X} \url{https://interactive-atlas.ipcc.ch/regional-information}
#' @format A list four RSL vectors
#' \describe{
#'  \item{year_before_after_present}{years before or after present}
#'  \item{sea_level_m}{meters below or above the reference sea level expressed in m.}
#'
#' }
#' @examples
#' curves <- IPCC_global_mean
#'  
"IPCC_global_mean"

#' @title Dataset: Funza (29500 up to 1000000 BP)
#' @description The ‘páramos’ UFL, i.e. high altitude mountain ecosystem in Venezuela (Northern Andes), during the last 1 million years, regularized to intervals of 1 ky.
#' @source Flantua et al. (2019) \doi{https://doi.org/10.3389/fevo.2021.615223}
#' @format A vector with:
#' \describe{
#'  \item{year_before_after_present}{years before or after present}
#'  \item{tree_line_m}{tree line expressed in meters above the present day reference sea level.}
#'
#' }
#' @examples
#' curve <- funza
#' 
"funza"

#' @title Dataset: features 
#' @description Features that can be defined as labeling points in the default dataset. point_reference dataset has been retrieved from geoNames.
#' 
#' @source GeoNames \url{https://download.geonames.org/export/dump/} 
#' @format data frame 
#' \describe{
#'  \item{feature_class}{feature class}
#'  \item{feature_class_description}{description of the feature class}
#'  \item{feature}{feature names}
#'
#' }
#' @examples
#' f <- features
#' 
"features"

#' @title Dataset: regions
#' @description mountain ranges at different hierarchical levels, islands, archipelagoes, countries and plates that can be used for region selection
#' 
#' 
#' @source Islands: Sayre et al. 2019 \doi{https://doi.org/10.1080/1755876X.2018.1529714} 
#' @source Plates: Bird, P. (2003) \doi{https://doi.org/10.1029/2001GC000252} 
#' @source Archipelagoes: Weigelt et al. 2013 \doi{https://doi.org/10.5061/dryad.fv94v} 
#' @source Countries: from rnaturalearth \url{https://docs.ropensci.org/rnaturalearth/} 
#' @source Mountains: Snethlage et al. 2022 \doi{https://doi.org/10.48601/earthenv-t9k2-1407} 
#' 
#' @format vector
#' \describe{
#'  \item{dataset}{the dataset (islands,mountains) for which the region definition can be used}
#'  \item{region}{region type}
#'  \item{name}{name of the region}
#'  \item{name_ascii}{name in ascii format}
#' }
#' @examples
#' r <- regions
#' 
"regions"


