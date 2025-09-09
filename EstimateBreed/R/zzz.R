#' Dataset: Oat data
#'
#' Data set with oat genotypes and industry variables.
#'
#' @format A data.frame with 54 observations and 6 variables:
#' \describe{
#'   \item{GEN}{14 white oat genotypes.}
#'   \item{BLOCO}{Experiment blocks.}
#'   \item{NG2M}{Number of grains larger than 2 mm.}
#'   \item{MG}{Grain mass}
#'   \item{MC}{Caryopsis dough}
#'   \item{RG}{Grain yield (in kg per ha)}
#' }
#' @source Real field data for use.
"aveia"

#' Data: Climate Data Set for Predictions
#'
#' Average air temperature and relative humidity data for the period of
#' one year, with time, day and month.
#'
#' @format A data.frame with 8760 observations and 5 variables:
#' \describe{
#'   \item{MO}{Month of the year.}
#'   \item{DY}{Day of the year.}
#'   \item{HR}{Time of the day.}
#'   \item{TMED}{Average Air Temperature - in  degree C.}
#'   \item{RH}{Relative Humidity - in \%.}
#' }
#' @source Data obtained from the Nasa Power platform (https://power.larc.nasa.gov/).
"clima"

#' Data: Data: Endogamy Coefficient Data Set
#'
#' Data set of phenotypic and genotypic variance, heritability and
#' differential selection for different variables.
#'
#' @format A data.frame with 7 observations and 5 variables:
#' \describe{
#'   \item{Var}{Variable name.}
#'   \item{VF}{Phenotypic Variance.}
#'   \item{VG}{Genotypic Variance.}
#'   \item{h}{Broad-sense heritability}
#'   \item{DS}{Selection Differential}
#' }
#' @source Real data for use.
"coefend"

#' Data: Data set for calculating the environmental deviation
#'
#' Data set with average air temperature and precipitation values per environment
#'
#' @format A data.frame with 449 observations and 3 variables:
#' \describe{
#'   \item{ENV}{Selection environment.}
#'   \item{TMED}{Average Air Temperature (in degree C).}
#'   \item{PREC}{Precipitation (in mm)}
#' }
#' @source Real field data for use.
"desvamb"

#' Data: GxE Interaction
#'
#' Data set with strains and test subjects from a GxE experiment.
#'
#' @format A data.frame with 55 observations and 5 variables:
#' \describe{
#'   \item{GEN}{Selected lines in a GXE experiment.}
#'   \item{ENV}{Selection environments.}
#'   \item{NG}{Number of grains measured in the lines.}
#'   \item{MG}{Grain mass measured in the lines (in g)}
#'   \item{CICLO}{Length of crop cycle (in days)}
#' }
#' @source Real field data for use.
"genot"

#' Data Set for Leaf Area Index
#'
#' Data set with 10 genotypes and values for leaf length, leaf width, number
#' of total leaves and number of dry leaves
#'
#' @format A data.frame with 10 observations and 5 variables:
#' \describe{
#'   \item{GEN}{Column with the genotypes.}
#'   \item{C}{Leaf lenght}
#'   \item{L}{Leaf width}
#'   \item{TNL}{Total number of leaves.}
#'   \item{TDL}{Total dry leavesh.}
#' }
#' @source Simulated data.
"leafarea"

#' Data: Wheat Data Set with Protein and Grain Yield
#'
#' Data set with wheat genotypes, protein percentage and grain yield.
#'
#' @format A data.frame with 24 observations and 7 variables:
#' \describe{
#'   \item{POP}{Base population.}
#'   \item{MGP_MF}{Phenotypic average of grain mass per plant.}
#'   \item{MGP_GP}{Genotypic average of grain mass per plant.}
#'   \item{VF}{Phenotypic variance}
#'   \item{VG}{Genetic variance}
#'   \item{H2}{Heritability in the broad sense}
#'   \item{Test}{Witness parameters}
#' }
#' @source Real field data for use.
"lin"

#' Data: Maize Dataset
#'
#' Data set with progenies and maternal and paternal maize genitors.
#'
#' @format A data.frame with 4 observations and 3 variables:
#' \describe{
#'   \item{P}{Progenies.}
#'   \item{GM}{Maternal Parent}
#'   \item{GP}{Patern Parent}
#' }
#' @source Simulated Data.
"maize"

#' Soybean Plastochron Estimation Data Set
#'
#' Fictitious data set for estimating soybean plastochron based on
#' on the number of nodes
#'
#' @format A data.frame with 135 observations and 5 variables:
#' \describe{
#'   \item{CICLO}{Days in the soybean cycle.}
#'   \item{GEN}{The column with the name of the genotype.}
#'   \item{TMED}{The column with the average temperature values.}
#'   \item{EST}{The column with the phenological stage.}
#'   \item{NN}{The column with the number of nodes.}
#' }
#' @source Simulated data for use.
"pheno"

#' Data: Wheat Dataset 1
#'
#' Data set with wheat cultivars and grain rheological characters.
#'
#' @format A data.frame with 360 observations and 5 variables:
#' \describe{
#'   \item{Cult}{Wheat cultivars.}
#'   \item{Am}{Sample identification number.}
#'   \item{NQ}{Falling Number.}
#'   \item{W}{Gluten Strength (W).}
#'   \item{PTN}{Grain Protein.}
#' }
#' @source Real laboratory data.
"ptn"

#' Data: Wheat Dataset 2
#'
#' Wheat genotype, protein and grain yield data set
#'
#' @format A data.frame with 360 observations and 5 variables:
#' \describe{
#'   \item{CULTIVAR}{Wheat cultivars.}
#'   \item{REP}{Repetition number.}
#'   \item{PTN}{Grain protein.}
#'   \item{RG}{Grain yield (kg ha)}
#' }
#' @source Real field data.
"ptnrg"

#' Data: Wheat Dataset 3
#'
#' Data set from a wheat experiment with different herbicide management.
#'
#' @format A data.frame with 19 observations and 6 variables:
#' \describe{
#'   \item{TEST}{Treatment identification.}
#'   \item{CE}{Ear length.}
#'   \item{ME}{Ear mass}
#'   \item{NGE}{Number of grains on the cob.}
#'   \item{MGE}{Grain mass of ear.}
#'   \item{NEE}{Number of spikelets per spike}
#' }
#' @source Real field data for use.
"trigo"

#' Data Set for Seed Vigor Extraction
#'
#' Data set from experiment with wheat genotypes subjected to different
#' sowing density.
#'
#' @format A data.frame with 54 observations and 6 variables:
#' \describe{
#'   \item{Trat}{Column with treatments.}
#'   \item{PC}{First Count}
#'   \item{G}{Germination percentage.}
#'   \item{CPA}{Length of aerial part.}
#'   \item{RAD}{Root length.}
#'   \item{MS}{Seedling dry mass.}
#'   \item{EC}{See what EC is.}
#' }
#' @source Real field data for use.
"vig"

#' Data Set for obtaining genetic parameters.
#'
#' Dataset with two breeding populations, 20 genotypes per population and three
#'  replicates per genotype.
#'
#' @format A data.frame with 60 observations and 4 variables:
#' \describe{
#'   \item{Pop}{Column with population names.}
#'   \item{Gen}{Column with genotype names.}
#'   \item{Rep}{Column with replications.}
#'   \item{VAR1}{Column with numerical values of the random variable.}
#' }
#' @source Simulated data for use.
"genot2"

#' Data Set with air temperature and incident radiation.
#'
#' @format A data.frame with 100 observations and 4 variables:
#' \describe{
#'   \item{Day}{Column with cycle lenght.}
#'   \item{Period}{Column with two periods (vegetative and reproductive).}
#'   \item{Temperature}{Average air temperature values.}
#'   \item{Radiation}{Incident radiation values.}
#' }
#' @source Simulated data for use.
"termaldata"

.estimatebreed_env <- new.env(parent = emptyenv())

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("EstimateBreed package loaded.")
}

utils::globalVariables(
  c(
    ".estimatebreed_env", "Ciclo", "MONTH", "Control", "valores", "dados",
    "sigmaE", "sigmaG", "sigmaP", "ECV", "GCV", "PCV", "H2", "GA", "GAM",
    "u", "primeiro_dia_germinacao", "DPclim", "AF", "ALA", "AS", "PotLAI",
    "RealLAI", "b2", "HW", "TTd", "ATT", "Class", "modelo", "model",
    "model_summary", "rsq", "STA", "eq_text", "pred", "RH", "Temp", "Td",
    "DELTAT", "WindS", "Prec", "RH2M", "T2M", "PRECTOTCORR", ":=", "<<-",
    "AAT","DAS", "RED"
  )
)

#' @import dplyr
#' @import ggplot2
#' @import hrbrthemes
#' @import broom
#' @import ggrepel
#' @import grid
#' @import httr
#' @import nasapower
#' @import tidyr
#' @import viridis
#' @import cowplot
#' @import sommer
#' @import lme4
#' @import minque
#' @import utils
#' @importFrom lubridate with_tz
#' @importFrom jsonlite fromJSON
#' @importFrom purrr flatten map map_dbl map2 map2_dbl
#' @importFrom grDevices rgb
#' @importFrom graphics boxplot layout
#' @importFrom stats aggregate aov coef lm nls nls.control pnorm sd setNames
#'  residuals fitted na.omit as.formula
#' @importFrom car some leveneTest
#' @importFrom lmtest bptest
#' @importFrom stats shapiro.test bartlett.test
NULL
