#' Handling of taxonomy in vegetation data.
#'
#' @name taxval
#' @aliases taxval
#'
#' @description
#'   Performs taxonomic valuation of species names according to synonomy, taxonomic level, unambiguous biotic content etc.
#'   Necessary prerequisite is information about taxonomic status (synonomy) and hierarchy (next higher aggregate).
#'   Until now only applicable for reference list 'GermanSL' (>= version 1.1, see References Section), which is valid in Germany and adjacent countries.
#'
#' @export
#' @param obs data.frame of observations in TURBOVEG format, for example loaded with \code{\link{tv.obs}}
#' @param refl Name of taxonomic reference list
#' @param db a name of a Turboveg database directory containing \code{tvabund.dbf}, \code{tvhabita.dbf} and \code{twin.set}
#' @param ag Treatment of children and parents within the dataset, see details
#' @param rank If ag='adapt', rank specifies the taxonomic rank to which taxa should be coarsened to. All higher taxa in this taxonomic tree will be deleted, see maxtaxlevel.
#' @param mono Should monotypic taxa be combined at subspecies = \option{lower} or species level = \option{higher}
#' @param monolist Name of monotypic species list, must be in dBase format and in the same directory as the reference list, e.g. \code{"monotypic-D"} for the area of Germany.
#' @param maxtaxlevel Maximum taxonomic levels to be used. See details.
#' @param ranklevels Ordered factor of taxonomic ranks, see vegdata:taxlevels
#' @param check.critical Check for critical names in your dataset and give warnings.'
#' @param interactive Do you want to adapt the list of changes.
#' @param ... Other parameters passed to functions.
#'
#' @details
#' Working with vegetation datasets, especially from different sources needs taxonomic valuation. The function tries to automate this process. Therefore the German taxonomic reference list (GermanSL, \url{https://germansl.infinitenature.org} ) contains additional taxon attributes (tax.dbf) and monotypic taxa of Germany (monotypic.dbf). Without an appropriate species list (see \code{\link{tax}}) the function will not work.
#'
#'The taxonomic reference list needs Taxonrank corresponding to values given in  taxlevels
#' Possible values for adapting the taxonomic hierarchy within the dataset (child/parent taxa) are: \code{preserve}: Leave everything untouched. \code{conflict}: Dissolve only in case of conflicts, e.g. if a subspecies occurs also at the species level within the same dataset. In this case the subspecies will be aggregated to the higher level. \code{adapt}: Dissolve all nested taxa to e.g. species level for option ag. For this option also option \code{rank}, specifying the rank to which the taxa shall be adapted, must be given.
#'
#' Monotypic taxa, e.g. a species which occur only with 1 subspecies in the survey area. They have to be combined, since otherwise two different (valid) taxa would denominate the same entity. If lower the higher taxon (e.g. species rank) is replaced by the lower level (subspecies rank). If neither \code{lower} nor \code{higher} monotypic species are preserved. Since the list of monotypic species strongly depends on the considered area you have to choose, which area is covered by your database and create an appropriate list of monotypic taxa. Within the package \code{"monotypic-D.csv"} is provided as a compilation of monotypic species within the GermanSL list.
#'
#' Option maxtaxlevel determines the maximum taxonomic level within the given names, which should be used. All higher taxon observations are deleted. If you have a single field observation determined as \emph{Asteraceae spec.} all your obervations of taxa from that family will be aggregated to the family level, if you choose ag=conflict.
#'
#' \code{Interactive} If you want to manually adapt the taxonomic harmonization \code{interactive=TRUE} will create a table with all original names and NewTaxonID's according to the chosen rules. The table will be saved as \code{taxvalDecisionTable.csv} in your actual working directory. You can manipulate the column NewTaxonID. If you run \code{taxval} again (e.g. through function \code{\link{tv.veg}}) and a file with this name exist in your working directory, it will be used.
#'
#' @return Functions return the input dataframe of observations with harmonised taxon numbers.
#'
#' @seealso tv.veg, tv.obs
#'
#' @references
#'  Jansen, F. and Dengler, J. (2008) GermanSL - eine universelle taxonomische Referenzliste f\"ur Vegetationsdatenbanken. Tuexenia, 28, 239-253.
#' Jansen, F. and Dengler, J. (2010) Plant names in vegetation databases - a neglected source of bias. Journal of Vegetation Science, 21, 1179-1186.
#'
#' @author Florian Jansen \email{florian.jansen@uni-rostock.de}
#'
#' @examples
#' \dontrun{
#' # Turboveg installation needed
#'  obs <- taxval(db='taxatest')
#' # For explanations see vignette('vegdata').
#'
#'  veg <- tv.veg('taxatest')
#'  veg <- comb.species(veg, c('ARMEM-E','ARMEM-H'))
#' }
#'

taxval <- function (obs, refl, db,
  ag  = c('conflict', 'adapt', 'preserve'),
  rank,
  mono = c('species','higher','lower','preserve'),
  monolist = "monotypic-D",
  maxtaxlevel = 'AGG',
  ranklevels = NULL,
  check.critical = TRUE,
  interactive = FALSE,

  ...)
{

### ------- config  end ####
########################## #
#  syn <- match.arg(syn)
#   depr.syn <- function(syn) if(!missing(syn)) warning('Option syn has been removed from function taxval. Synonyms will always be adapted to accepted names.')
#   depr.syn()
  ag <- match.arg(ag)
  mono <- match.arg(mono)
  if(missing(obs))
    if(missing(db))
      stop("Please specify either an observation dataframe or the name of your Turboveg database.")
        else  obs <- tv.obs(db=db, tv_home)
  tv_home <- tv.home()
  if(missing(refl)) {
    if('tax.refl' %in% names(options()))
      refl <- unlist(options('tax.refl'), use.names = FALSE) else
      if(missing(db)) stop('If you do not give a taxonomic reference list name, you have to specify at least a name of a Turboveg database.') else
     refl <- tax.refl(db = db[1], tv_home = tv_home)
  }
  if(is.character(refl)) {
    suppressMessages(species <- tax('all', refl=refl, ...))
    } else species <- refl
  if(!is.character(species$TaxonUsageID)) species$TaxonUsageID <- as.character(species$TaxonUsageID)
  if(is.null(ranklevels))
     ranklevels <- factor(vegdata::taxlevels$level, levels = vegdata::taxlevels$level, ordered=TRUE)
  # if(is.factor(taxlevels)) if(ordered(taxlevels)) ranklevels <- taxlevels
  # else stop('"taxlevel" must be an ordered factor')) else stop('"taxlevels" must be an ordered factor')

  if(any(!species$TaxonRank %in% taxlevels$level)) warning('Not all taxon rank levels in taxlevels. Please check!')

  ##-- start taxval functions
  if(ag == 'adapt' & missing(rank)) stop('Please specify to which "rank" the taxa shall be adapted.')
  # if(ag == 'adapt' & !missing(maxtaxlevel)) warning('maxtaxlevel will be ignored for ag="adapt"')
  if(missing(maxtaxlevel)) maxtaxlevel <- as.character(taxlevels$level[taxlevels$rank == max(taxlevels$rank)])
  if(ag == 'conflict' & !missing(rank)) warning('Option "rank" is ignored for ag="conflict". You might want to run taxval twice, see vgnette.')
# ########################## #
  names(obs) <- TCS.replace(names(obs))
###--- check TaxonID's
if(any(!obs$TaxonUsageID %in% species$TaxonUsageID))
  stop(paste("The following Taxon ID's do not exist in the given reference list:", paste(unique(obs$TaxonUsageID[which(!obs$TaxonUsageID %in% species$TaxonUsageID)]), collapse = ', ')))
if(ag %in% c('conflict', 'adapt') & !missing(rank))
  if (which(ranklevels == maxtaxlevel) < which(ranklevels == rank))
    stop('Maximum allowed taxonomic rank lower than the aggregation level!')

  message("Original number of names: ", length(unique(obs$TaxonUsageID)))

# ############################# #
### ------ define functions  ####
# ############################# #
agg.conflict <- function(fr, ...) {
  # Subsuming elements into higher rank observations (if necessary) for adapt or conflict .
  fr$round <- 0
  r <- 1
  suppressMessages(
  repeat{
    ChildsOfOccurringTaxa <- unique(unlist(sapply(fr$NewTaxonID[!fr$TaxlevelTooHigh], function(x)
        suppressMessages( child(x)$TaxonUsageID))) )
    for(i in fr$NewTaxonID[!fr$TaxlevelTooHigh])
      suppressMessages(child(i)$TaxonUsageID)
    OccurringChilds <- ChildsOfOccurringTaxa[ChildsOfOccurringTaxa %in% fr$NewTaxonID[!fr$TaxlevelTooHigh]]
    if(length(OccurringChilds) > 0) {
      message(length(OccurringChilds), 'conflicting child taxa found in dataset. Harmonised to higher level.')
      if(length(OccurringChilds) < 10)
          print(sort(species$TaxonName[match(OccurringChilds, species$TaxonUsageID)]))
      for(i in 1:length(OccurringChilds)) {
        suppressMessages(
          nested.in <- parent(OccurringChilds[i], refl = species)
        )
        nested.occ <- nested.in[match(nested.in$TaxonRank, ranklevels) <= match(maxtaxlevel, ranklevels),]
        if(nrow(nested.occ) > 0) {
          fr$round[fr$NewTaxonID == OccurringChilds[i] & !is.na(fr$NewTaxonID)] <- r
          fr$NewTaxonID[fr$NewTaxonID == OccurringChilds[i] & !is.na(fr$NewTaxonID)] <- nested.occ$TaxonConceptID[1]
          fr$adaptHierarchy[i] <- TRUE
        }
      }
#      write.csv(fr, file=paste('fr', r, 'csv', sep='.'))
      r <- r + 1
    } else break
  }
)
  return(fr)
}

agg.adapt <- function(fr, ...) {
  for(i in which(!fr$TaxlevelTooHigh)) {
    suppressMessages(
      p <- parent(fr$NewTaxonID[i], refl = species)
    )
      p$level <- match(p$TaxonRank, ranklevels)
      rankl <- which(ranklevels == rank)
      taxl <- which(ranklevels == species$TaxonRank[match(fr$TaxonUsageID[i], species$TaxonUsageID)])
      if(taxl <= rankl) {
        new <- tail(p$TaxonUsageID[which(p$level <= rankl)], n=1)
        if(length(new) > 0) {
          fr$NewTaxonID[i] <- new
          fr$adaptHierarchy[i] <- TRUE
        }
      }
  }
  message('For ', sum(fr$adaptHierarchy[!fr$Synonym]), ' taxa the taxonomic hierarchy will be adapted.')
  return(fr)
}

## Run the code ####

if((interactive & !file.exists('taxvalDecisionTable.csv')) | !interactive)  {

#  Create decision table
  fr <- as.data.frame(table(obs$TaxonUsageID), stringsAsFactors = FALSE)
  names(fr)[1] <- 'TaxonUsageID'
  fr$TaxonName <- species$TaxonName[match(fr$TaxonUsageID, species$TaxonUsageID)]
  if(any(is.na(fr$TaxonName))) {
    message('Can not find the following taxon ids')
    print(fr$TaxonUsageID[is.na(fr$TaxonName)])
    stop('Wrong taxon ids.')
  }
  fr$Secundum <- species$AccordingTo[match(fr$TaxonUsageID, species$TaxonUsageID)]
  fr$Synonym <- species$SYNONYM[match(fr$TaxonUsageID, species$TaxonUsageID)]
  fr$TaxonRank <- species$TaxonRank[match(fr$TaxonUsageID, species$TaxonUsageID)]
  fr$TaxlevelTooHigh <- NA
  fr$NewTaxonID <- fr$TaxonUsageID
  fr$adaptHierarchy <- FALSE

# ############################# #
  obs$OriginalUsageID <- obs$TaxonUsageID
### ------ adjust synonyms
  synonyms <- if(any(species$SYNONYM[match(fr$NewTaxonID, species$TaxonUsageID)]))
    species[species$SYNONYM & species$TaxonUsageID %in% fr$TaxonUsageID, ] else NULL
  if(!is.null(synonyms)) if(nrow(synonyms) > 0) {
    message(paste(nrow(synonyms), 'synonyms found in dataset.', if(!interactive) 'Changed to valid names.'))
    fr$NewTaxonID[match(synonyms$TaxonUsageID, fr$TaxonUsageID)] <- synonyms$TaxonConceptID
    fr$TaxonRank[match(synonyms$TaxonUsageID, fr$TaxonUsageID)] <- species$TaxonRank[match(fr$NewTaxonID, species$TaxonUsageID)][match(synonyms$TaxonUsageID, fr$TaxonUsageID)]
  }

# ############################################### #
###------ restrict to maximum taxonomic level  ####
if(is.character(refl)) if(grepl('GermanSL', refl) & maxtaxlevel == 'AGG') maxtaxlevel <- 'AG1'
if(maxtaxlevel %in% ranklevels) {
  fr$TaxlevelTooHigh <- species$TaxonRank[match(fr$NewTaxonID, species$TaxonUsageID)] %in% ranklevels[ranklevels > ranklevels[match(maxtaxlevel, ranklevels)]]
  if(sum(fr$TaxlevelTooHigh) > 0) {
    message(sum(fr$TaxlevelTooHigh), "taxon observation id's higher than", maxtaxlevel, 'found. Deleted!')
    obs <- obs[!obs$TaxonUsageID %in% fr$TaxonUsageID[fr$TaxlevelTooHigh], ]
  }
 } else stop(paste('The given rank code', maxtaxlevel, 'is not a known rank identifier:', paste(ranklevels, collapse=', ')))

# ############################################## #
# ------ resolve monotypic taxa           ####
if (mono %in% c("species", "lower", "higher") & is.character(refl)) {
  if (file.access(file.path(tv_home, 'Species', refl, paste(monolist, "csv", sep = ".")))) {
    warning("You have chosen to care about monotypic taxa but the specified list of monotypic taxa is not available!")
  } else {
    Mono <- read.csv(file.path(tv_home, 'Species', refl, paste(monolist, "csv", sep = ".")), sep=';')
    r = 0
    repeat{
      r <- r + 1
      if(refl %in% c('GermanSL 1.3', 'GermanSL 1.4')) names(Mono)[1] <- 'Parent_NR'
      if (mono == "lower")  tmp <- Mono$MEMBER_NR[match(fr$NewTaxonID, Mono$Parent_NR)]
      if (mono == "higher") tmp <- Mono$Parent_NR[match(fr$NewTaxonID, Mono$MEMBER_NR)]
      if (mono == 'species') {
        tmp <- Mono$Parent_NR[match(fr$NewTaxonID, Mono$MEMBER_NR)]
        tmp[Mono$MEMB_Rank[match(tmp, Mono$Parent_NR)] %in% ranklevels[ranklevels >= 'SPE']] <- NA
        tmp <- Mono$MEMBER_NR[match(fr$NewTaxonID, Mono$Parent_NR)]
        tmp[Mono$MEMB_Rank[match(tmp, Mono$MEMBER_NR)] %in% ranklevels[ranklevels <= 'SPE']] <- NA
      }
      if(sum(tmp > 0, na.rm = TRUE) == 0) {break}
      message(sum(tmp > 0, na.rm = TRUE), " monotypic taxa found in dataset.")
      fr$Monotypic <- !is.na(tmp)
      fr$NewTaxonID[which(!is.na(tmp))] <- tmp[!is.na(tmp)]
      if(any(is.na(fr$NewTaxonID))) message('Not for all taxa new TaxonIDs could be found. ')
    }
  }
#  message(sum(fr$Monotypic), "monotypic taxa found in dataset.")
  if(sum(fr$Monotypic) > 0) message("  Will be set to ", mono, " rank", if(mono == 'species') " if possible.", sep='')
} else message('Monotypic taxa preserved!')

# ##################################################### #
## ------ harmonize differing taxonomic levels       ####
## ------ apply functions

  fr <- switch(ag,
               preserve = {message(' Aggregates preserved!'); fr},
               conflict = agg.conflict(fr),
               adapt    = agg.adapt(fr),
               stop('You need to specify how you want to handle different taxonomic levels in your data: either "preserve", "adapt", or adapt only in case of "conflict".')
  )

  ############################# #
  fr$NewTaxonName <- species$TaxonName[match(fr$NewTaxonID, species$TaxonUsageID)]
  fr$willBeAdapted <- ifelse(fr$TaxonUsageID != fr$NewTaxonID | fr$TaxlevelTooHigh, 1, 0)
  if(interactive) {
  message('Interactive mode: Nothing changed. Please check and adapt column "NewTaxonID" in "taxvalDecisionTable.csv" \n and re-run this function again with interactive = TRUE.')
  write.csv2(fr, file='taxvalDecisionTable.csv')
  } else
    obs$TaxonUsageID <- fr$NewTaxonID[match(obs$TaxonUsageID, fr$TaxonUsageID)]

} else
  ### apply taxvalDecisionTable.csv   ####
  if(interactive & file.exists('taxvalDecisionTable.csv')) {
    message('File ./taxvalDecisionTable.csv is used for taxonomic harmonization.')
    fr <- read.csv2('taxvalDecisionTable.csv')
    obs <- obs[!obs$TaxonUsageID %in% fr$TaxonUsageID[fr$TaxlevelTooHigh],]
    obs$OriginalUsageID <- obs$TaxonUsageID
    obs$TaxonUsageID <- fr$NewTaxonID[match(obs$TaxonUsageID, fr$TaxonUsageID)]
    check.critical = FALSE
}

############################ #
##  Cleaning              ####
############################ #

message('Number of taxa after ', if(interactive & !file.exists('taxvalDecisionTable.csv')) 'interactive',  'harmonisation: ', length(unique(fr$NewTaxonID[!fr$TaxlevelTooHigh])))

################################## #
###------ check for Critical species
if(check.critical) {
  if(!'AccordingTo' %in% names(species)) species$AccordingTo <- ''
  if(!'IsChildTaxonOf' %in% names(species)) species$IsChildTaxonOf <- species$TaxonName[match(species$IsChildTaxonOfID, species$TaxonUsageID)]
  fr <- as.data.frame(table(obs$TaxonUsageID), stringsAsFactors = FALSE, responseName = 'Count')
  names(fr)[1] <- 'TaxonUsageID'
  # Pseudonyms
  auct <- species[grep("\ auct.", species$TaxonName, perl=TRUE), ] #c(1:5, 11, 13, 14, 15)
  auct$to_check <- sub("\ auct.", "", auct$TaxonName, perl=TRUE)
  auct$check_No <- species$TaxonUsageID[match(auct$to_check, species$TaxonName)]
  auct <- auct[!is.na(auct$check_No), ]
  auct <- auct[,  c('to_check', 'check_No', 'TaxonName','TaxonUsageID', 'AccordingTo')]
  names(auct)[3] <- "check against"
  if (any(fr$TaxonUsageID %in% auct$check_No)) {
    message('Warning: Potential pseudonyms in dataset, please check.')
    print(auct[match(fr$TaxonUsageID, auct$check_No, nomatch = FALSE), ], row.names = FALSE)
  }

  ### Extent of taxon interpretation
  sl <- species[grep("\ s.\ l.", species$TaxonName, perl=TRUE), which(names(species) %in% c('TaxonUsageID','TaxonName','TaxonConceptID','TaxonConcept','TaxonRank','IsChildTaxonOfID','IsChildTaxonOf','AccordingTo')) ]
  sl$to_check <- sub("\ s.\ l.$", "", sl$TaxonName, perl=TRUE)
  sstr <- species[grep("\ s.\ str.$", species$TaxonName, perl=TRUE), which(names(species) %in% c('TaxonUsageID','TaxonName','TaxonConceptID','TaxonConcept','TaxonRank','IsChildTaxonOfID','IsChildTaxonOf','AccordingTo'))]
  sstr$to_check <- sub("\ s.\ str.$", "", sstr$TaxonName, perl=TRUE)
  ext <- rbind(sl,sstr)

  ext$check_No <- species$TaxonUsageID[match(ext$to_check, species$TaxonName)]
  ext <- ext[!is.na(ext$check_No), which(names(ext) %in% c('to_check', 'check_No', 'TaxonName','TaxonUsageID', 'AccordingTo'))]
  names(ext)[3] <- "check against"
  if (any(fr$TaxonUsageID %in% ext$check_No)) {
    message('Warning: Critical species in dataset, please check')
    u <- ext[match(fr$TaxonUsageID, ext$check_No, nomatch = FALSE), ]
    print(u[order(u$to_check),], row.names = FALSE)
  }
}
############################# #
 return(obs)
}

