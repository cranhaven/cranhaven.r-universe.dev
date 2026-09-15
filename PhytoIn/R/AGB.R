#' Estimate the above-ground biomass
#'
#' Estimate the above-ground biomass (AGB), carbon (C) and CO\eqn{_2} equivalent (CO\eqn{_2}e) of trees.
#'
#' \code{AGB} is a wrapper around \pkg{BIOMASS} functions \code{getWoodDensity}, \code{computeAGB},
#' and \code{correctTaxo} (Rejou-Mechain et al., 2017). Tree biomasses are computed using the
#' allometric model of Chave et al. (2014).
#'
#' It is expected that taxon names are binomials (genus and species). The function splits the taxon
#' string into two columns (\code{genus}, \code{species}) to retrieve wood density. Single-word
#' taxa (e.g., \code{Indet}, \code{Dead}) receive \code{species = NA}.
#'
#' Wood density (g/cm\eqn{^3}) is obtained from a global database (~16,500 species). If a species
#' is missing, the genus mean is used; if the genus is missing, the sample-unit (\code{su}) mean is used.
#'
#' The input \code{x} must include columns for sample-unit labels, taxon names, CBH/DBH, and optionally
#' height. If height is absent, \code{coord} is mandatory to allow height estimation.
#'
#' The CBH/DBH column allows multi-stem notation such as \code{"17.1+8+5.7+6.8"}. The plus sign separates
#' stems; decimal separators can be points or commas; spaces around \code{"+"} are ignored. Column names
#' in \code{x} are coerced to lower case at runtime, making the function case-insensitive.
#'
#' Measurement units: CBH/DBH in centimeters; height in meters.
#'
#' @param x A \code{data.frame} with the community sample data. See \strong{Details}.
#' @param measure.label Name of the column with circumference/diameter at breast height (cm).
#' @param h Name of the column with tree height (m). If omitted in \code{x}, height is estimated from \code{coord}.
#' @param taxon Name of the column with sampled taxon names. Default \code{"taxon"}. Use UTF-8; accents and special characters are not allowed.
#' @param dead String used to identify dead individuals. Default \code{"dead"}.
#' @param circumference Logical; if \code{TRUE} (default), CBH is assumed; otherwise DBH is assumed.
#' @param su Name of the column with sample-unit identifiers. Default \code{"quadrat"}.
#' @param area Numeric scalar: total sampled area (ha).
#' @param coord A vector \code{c(longitude, latitude)} or a two-column matrix/\code{data.frame} of site coordinates (decimal degrees).
#'             Required when \code{h} is missing in \code{x}.
#' @param rm.dead Logical; if \code{TRUE} (default) dead individuals are removed prior to biomass calculation.
#' @param check.spelling Logical; if \code{TRUE}, near-matching taxon names are flagged for correction. Default \code{FALSE}.
#' @param correct.taxon Logical; if \code{TRUE} (default) taxon names are standardized via TNRS.
#' @param sort Logical; if \code{TRUE} (default) taxa are sorted by AGB.
#' @param decreasing Logical; if \code{TRUE} (default) sorting is in decreasing order.
#' @param cache Logical or \code{NULL}; if \code{TRUE} the function with write and use a cache to reduce online search of taxa names.
#'  (\code{NULL} means use cache but clear it first). Default is \code{cache = FALSE}.
#' @param long Logical; if \code{FALSE} (default) the \code{$tree} component is omitted (see \strong{Value}).
#'
#' @return An object of class \code{"biomass"} with up to four components:
#' \itemize{
#'   \item \code{$tree}: individual records (taxon, wood density \code{wd}, assignment level, \code{AGB}, \code{C}, \code{CO2e});
#'   \item \code{$taxon}: AGB, C, and CO\eqn{_2}e per taxon (Mg per ha);
#'   \item \code{$total}: total AGB, C, and CO\eqn{_2}e per ha;
#'   \item \code{$WD.level}: percentage of wood-density assignments at \code{species}, \code{genus}, and sample-unit levels.
#' }
#'
#' @references
#' Boyle, B. et al. (2013) \emph{BMC Bioinformatics} 14:16.\cr
#' Chave, J. et al. (2014) \emph{Global Change Biology} 20(10):3177--3190.\cr
#' Rejou-Mechain, M. et al. (2017) \emph{Methods in Ecology and Evolution} 8:1163--1167.\cr
#' Zanne, A.E. et al. (2009) Global wood density database. Dryad.\cr
#'
#' @examples
#' data <- quadrat.df
#' head(data)
#' \donttest{
#' resul1 <- AGB(
#'   data, measure.label = "CBH", h = "h", taxon = "Species", dead = "Morta",
#'   circumference = TRUE, su = "Plot", area = 0.0625, rm.dead = TRUE,
#'   check.spelling = FALSE, correct.taxon = TRUE, sort = TRUE,
#'   decreasing = TRUE, long = TRUE
#' )
#' head(resul1$tree)
#' resul1$taxon
#' resul1$total
#' resul1$WD.level
#'
#' quadrat.default <- quadrat.df
#' colnames(quadrat.default) <- c("quadrat", "family", "taxon", "cbh", "h")
#'
#' Resul2 <- AGB(x = quadrat.default, measure.label = "cbh",
#'               circumference = TRUE, h = "h", dead = "Morta", area = 0.0625)
#' head(Resul2$tree)
#' Resul2$taxon
#' Resul2$total
#' Resul2$WD.level
#' \dontrun{
#' Resul3 <- AGB(data, measure.label = "CBH", taxon = "Species", dead = "Morta",
#'               circumference = TRUE, su = "Plot", area = 0.0625,
#'               coord = c(-47.85, -21.17))
#' Resul3$taxon
#' Resul3$total
#' Resul3$WD.level
#' }
#' }
#' @seealso \pkg{BIOMASS} (\code{\link[BIOMASS]{getWoodDensity}},
#'   \code{\link[BIOMASS]{computeAGB}}, \code{\link[BIOMASS]{correctTaxo}})
#'
#' @importFrom BIOMASS getWoodDensity computeAGB correctTaxo createCache
#' @export

AGB <- function(x, measure.label, h, taxon="taxon", dead="dead", circumference=TRUE, su="quadrat", area, coord,
                rm.dead=TRUE, check.spelling=FALSE, correct.taxon=TRUE, sort=TRUE, decreasing=TRUE, cache=FALSE,
                long=FALSE)

{
  #  if(all((.packages())!= "BIOMASS")) require(BIOMASS)
  if (!requireNamespace("BIOMASS", quietly = TRUE)) {
    stop("Package 'BIOMASS' is required for AGB(). Please install it.", call. = FALSE)
  }

  measure.label <- tolower(measure.label)
  taxon <- tolower(taxon)
  if(!missing(h)) h<- tolower(h)
  dead<- tolower(dead)
  su<- tolower(su)

  colnames(x) <- tolower(colnames(x)) # rename the data.frame column names using only lowercase letters
  y<-is.na(x) | x == "" # Logical test to find all empty cells or with NAs
  x <- x[!apply(y == TRUE, 1, all), !apply(y == TRUE, 2, all)]  # Remove all empty/NA rows/columns

  # Check taxon spelling
  if(check.spelling){
    initial.list<-unique(x[[taxon]])
    spp.list<-initial.list
    for(k in 1:length(spp.list)){
      close.strings<-agrep(initial.list[k], spp.list, value=TRUE)
      if(length(close.strings)>1)
      {
        opt<-menu(close.strings, title="Choose the number corresponding the correct name. Type 0 to skip.")
        correct.name<-close.strings[opt]
        incorrect<-close.strings[-opt]
        if(opt!=0) for(i in 1:length(incorrect)) {
          spp<-gsub(incorrect[i], correct.name, x[[taxon]])
          spp.list<-unique(gsub(incorrect[i], correct.name, spp.list))
        }
      }
    }
  }
  # Remove dead individuals
  if (rm.dead) # if the option is to remove dead plants from the data frame
  {
    filter = tolower(x[[taxon]]) == tolower(dead) # test which individuals are "dead"
    x <- x[!filter, ] # remove rows containing dead plants from the data frame
    #    message("\n", sum(filter), "dead individuals removed from the dataset \n") # display a message on screen with the number of "dead" removed from the data frame
    n.dead <- sum(filter, na.rm = TRUE)
    if (n.dead > 0) message(n.dead, " dead individuals removed from the dataset.")
  }

  # Split taxon in genus and species
  x[[taxon]] <- gsub(" +", " ", x[[taxon]])
  #  list<-strsplit(x[[taxon]], split=" ", fixed=TRUE)
  #  taxon.names<-as.data.frame(matrix(unlist(list), ncol=2, byrow = TRUE))
  #  if(dim(taxon.names)[1]!=dim(x)[1]) stop("Check multiple spaces in the taxon column")
  #  colnames(taxon.names)<-c("genus","species")

  list <- lapply(x[[taxon]], function(z) {
    split_result <- strsplit(z, split = " ", fixed = TRUE)[[1]]
    if (length(split_result) > 1) {
      split_result
    } else {
      c(split_result, NA)
    }
  })
  taxon.names <- data.frame(genus = sapply(list, "[", 1), species = sapply(list, "[", 2))

  # Correct taxon
  # Cache robusto, sem 'rappdirs'
  if (isTRUE(correct.taxon)) {
    # Definir diretorio de cache (compativel com R >= 4.0)
    if (isTRUE(cache) || is.null(cache)) {
    cache_dir <- tryCatch({
      if (getRversion() >= "4.0.0") {
        tools::R_user_dir("PhytoIn", which = "cache")
      } else {
        file.path(path.expand("~"), ".cache", "PhytoIn")
      }
    }, error = function(e) tempdir())

    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }

    BIOMASS::createCache(path = cache_dir)
    }

    # So executa correctTaxo se httr2 estiver disponivel
    if (!requireNamespace("httr2", quietly = TRUE)) {
      warning("Skipping taxon standardization: package 'httr2' not available.", call. = FALSE)
    } else {
      taxon.corrected <- BIOMASS::correctTaxo(
        genus   = taxon.names$genus,
        species = taxon.names$species,
        useCache = cache # Uses the value of argument cache
      )
      taxon.names <- data.frame(
        genus   = taxon.corrected$genusCorrected,
        species = taxon.corrected$speciesCorrected,
        stringsAsFactors = FALSE
      )
    }
  }
  #####################
  # Individual diameter

  measure <- x[[measure.label]]  # extract the column of measurement values
  measure <- gsub(",", ".", measure) # replace any commas with dots

  # Split multiple stems into columns
  my_list<-strsplit(x=measure, split="+", fixed=TRUE)
  n.obs <- sapply(my_list, length)
  seq.max <- seq_len(max(n.obs))
  mat <- t(sapply(my_list, "[", i = seq.max))
  measure.table<-matrix(as.numeric(mat), ncol = max(seq.max))    # Convert to numeric matrix
  measure.table[is.na(measure.table)] <- 0
  if (circumference) AB <- (measure.table)^2/(4*pi) else
    AB <- (pi*(measure.table)^2)/4  # calculate basal area in m² of each stem from CBH or DBH values
  ABi <- apply(AB, 1, sum)        # Calculate individual basal area
  d <- sqrt(ABi/pi)*2   # Calculate individual diameter

  # Wood density
  WDdata<-BIOMASS::getWoodDensity(genus=taxon.names$genus, species=taxon.names$species, stand=x[[su]])

  # Biomass
  # Biomassa
  if (missing(h)) {
    # Estimara H via coord -> exige httr2 (e dependências usadas pelo BIOMASS)
    if (!requireNamespace("httr2", quietly = TRUE)) {
      stop("Estimating height from `coord` requires the optional package 'httr2'. ",
           "Install it (and dependencies) or provide `h`.", call. = FALSE)
    }
    AGBtree <- BIOMASS::computeAGB(D = d, WD = WDdata$meanWD, coord = coord)
  } else {
    AGBtree <- BIOMASS::computeAGB(D = d, WD = WDdata$meanWD, H = x[[h]])
  }

  res.tree <- data.frame(su=x[[su]], taxon=x[[taxon]], wd=WDdata$meanWD, level=WDdata$levelWD,
                         AGB=AGBtree, C=AGBtree*0.471, CO2e=AGBtree*0.471*3.6705069)
  AGBsp <- aggregate(cbind(AGB=AGB/area, C=C/area, CO2e=CO2e/area) ~ taxon, data = res.tree, sum)
  if(sort) AGBsp <- AGBsp[order(AGBsp$AGB, decreasing=decreasing), ]
  row.names(AGBsp) <- 1:dim(AGBsp)[1]
  AGBtot <- cbind(AGBtot=sum(AGBsp$AGB), Ctot=sum(AGBsp$C), CO2etot=sum(AGBsp$CO2e))
  # WD level
  n <- dim(res.tree)[1] # number of trees
  sp <- sum(res.tree$level=="species")
  gn <- sum(res.tree$level=="genus")
  s.u <- n - sp - gn
  WD.level <- data.frame(percentage=c(sp, gn, s.u)/n*100)
  row.names(WD.level) <- c("species", "genus", "sample unit")
  if(long) res <- list(tree = res.tree, taxon = AGBsp, total = AGBtot, WD.level=WD.level)
  else res <- list(taxon = AGBsp, total = AGBtot, WD.level=WD.level)
  class(res) <- "biomass"
  invisible(res)
}

