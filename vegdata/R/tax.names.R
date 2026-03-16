#' Standardisation of taxonomic names, especially taxon rank indicators and hybrid signs
#' name taxname.abbr
#'
#' @export
#' @param x (integer or character) Species number, lettercode or species name(s)
#' @param hybrid (logical) remove hybrid markers for comparisons
#' @param concept (logical) remove concept additions like "s. str.", "s. l.
#' @param cf (logical) remove 'in doubt' marker
#' @param \dots additional attributes
#'
#' @author Florian Jansen florian.jansen@uni-rostock.de
#'

taxname.abbr <- function(x,
                         hybrid = TRUE,
                         concept = FALSE,
                         cf = TRUE, ...) {
    #  loc <- Sys.getlocale(category='LC_CTYPE')
    #  Sys.setlocale("LC_ALL","C")
    trim <- function (x, whitespace = "[ \t\r\n]") {
      mysub <- function(re, x) sub(re, "", x, perl = TRUE)
      mysub(paste0(whitespace, "+$"), mysub(paste0("^", whitespace, "+"), x))
    }
    squish <- function (x) gsub("[[:space:]]+", " ", x)
    x <- squish(trim(x))
    x <- gsub('\"', '', x)
    x <- gsub("\\s+", " ", x)
    x <- gsub('\u00A0', ' ', x)
    x <- trimws(sub('\ aggr\\.?(\ |$)', ' agg. ', x))
    x <- trimws(sub('\ ag\\.?(\ |$)', ' agg. ', x))
    x <- sub('\ species group[\ ]', ' agg.', x)
    x <- sub('\ ssp\\.?', ' subsp.', x)
    x <- sub('\ v\\.?\ ', ' var. ', x)
    x <- sub('\ sv\\.?\ ', ' subvar. ', x)
    x <- sub('\ Sec\\.?\ ', ' sectio ', x)
    x <- sub('\ sect\\.?[(]bot\\.?[)]\ ', ' sectio ', x)
    x <- sub('\ Subs\\.?\ ', ' subsectio ', x)
    x <- sub('\ Ser\\.?\ ', ' ser. ', x)
    ## in the wider sense
    # for(x in c('Genus species sens. lat.', 'Genus species sens lat', 'Genus species sens l.', 'Genus species s. lat.', 'Genus species s. l.', 'Genus species s.l.', 'Genus species sl'))  print(  sub('\ s(ens)?\\.?\\s?l(at)?\\.?($|\ )', ' sensulato', x))
    x <-  sub('\ s(ens)?\\.?\\s?l(at)?\\.?($|\ )', ' sensulato', x)

    ## in the stricter sense
    # for(x in c('Genus species sens. str.', 'Genus species sens str', 'Genus species sens s', 'Genus species sens s.', 'Genus species s. str.', 'Genus species s.str.', 'Genus species s.s.', 'Genus species ss'))   print( sub('\ s(ens)?\\.?\\s*?s(tr)?\\.?($|\ )', ' sensustricto', x)  )
    x <- sub('\ s(ens)?\\.?\\s*?s(tr)?\\.?($|\ )', ' sensustricto', x)

    x <- sub('\ f\\.?\ ', ' fo. ', x)
    x <- sub('\ species', ' spec.', x)
  if(concept) {
      x <- sub('\ sensulato', '', x)
      x <- sub('\ sensustricto', '', x)
    } else {
      x <- sub('\ sensulato($|\ )', ' s. l.', x)
      x <- sub('\ sensustricto($|\ )', ' s. str.', x)
   }
    # Hybrids
    x <- gsub('^x ', ' \u00d7', x)
    x <- gsub(' x ',' \U00D7', x)
    x <- gsub('\U00D7 ', '\U00D7', x)
    x <- gsub('\ x\ ', ' \u00d7', x)
    x <- gsub('^x\ ', '\u00d7', x)

    x <- sub('\ nothovar\\.?' , '\ nvar.', x)
    x <- sub('\ nothosubsp\\.?', '\ nsubsp.', x)
    x <- sub('\ nssp\\.?', '\ nsubsp.', x)
    x <- sub('\ nothossp\\.?' , '\ nsubsp.', x)

   if(hybrid)  x <- gsub('\U00D7', '', x, fixed = TRUE)

   if(cf) x <- sub('cf.\ ', '', x, ignore.case=TRUE)

    x <- trimws(x) # trim leading and trailing leading spaces
    #  Sys.setlocale(category='LC_CTYPE', locale=loc)
   return(x)
}



#' @title Simplify name parts for better string matching
#'
#' @name taxname.simpl
#'
#' @export
#' @param x (integer or character) Species number, lettercode or species name(s)
#' @param genus (logical) simplify genus name part
#' @param concept (logical) remove name parts which describe taxon concept size like "s. str.","s. l."
#' @param rank (logical) remove rank specifications
#' @param epithet (logical) simplify epithet(s)
#' @param tax.status (logical) remove taxon status like 'nom. illeg.' or 'auct.'
#' @param \dots additional attributes
#'
#' @details taxname.abbr will be applied beforehand automatically. The function simplifies name parts which are empirically unstable, i.e. sylvatica might also written as silvatica, or majus vs. maius. Sex of latin genus or epithet name parts often change and are therefore deleted (us vs. a, ea vs. eos, etc.). Hybrid signs are removed. taxname.simpl works well for plant names, but be careful with very long name lists or if combined with animal taxa which are sometimes very short and can be confused after applying taxname.simpl
#'
#' @author Florian Jansen florian.jansen@uni-rostock.de
#'

taxname.simplify <- function(x, genus=TRUE, epithet=TRUE, concept = TRUE, rank=TRUE, tax.status=TRUE, ...) {
  x <- taxname.abbr(x, ...)
  if(tax.status) {
    x <- gsub(' nom\\.? illeg\\.?', '', x)
    x <- gsub(' nom\\.? inval\\.?', '', x)
    x <- gsub(' nom\\.? rej\\.?', '', x)
    x <- gsub(' auct\\.?', '', x)
  }
  if(concept) {
    x <- gsub('\ sensulato', '', x)
    x <- gsub('\ sensustricto', '', x)
    x <- gsub(' s\\.?\ str\\.?', '', x)
    x <- gsub(' s\\.?\ l\\.?', '', x)
  } else {
    x <- gsub('\ sensulato', ' s. l.', x)
    x <- gsub('\ sensustricto', ' s. str.', x)
  }

    x <- gsub('\U00EB', 'e', x)
    x <- gsub('\U00CF', 'i', x)
    x <- gsub('ii', 'i', x)
    x <- gsub('ck', 'k', x)
    x <- gsub('nn', 'n', x)
    x <- gsub('fa', 'pha', x)
    x <- gsub('ej', 'ei', x)
    x <- gsub('oe', 'ae', x)
    x <- gsub('ph', 'p', x)
    x <- gsub('rh', 'h', x)
    x <- gsub('rr', 'r', x)
    x <- gsub('th', 't', x)
    x <- gsub('tt', 't', x)
    x <- gsub( 'y', 'i', x)
    x <- gsub( '-', '', x)
    x <- gsub('ranum', 'rianum', x)

    # Hybrids
      x <- gsub('^X ', '\U00D7', x)
      x <- gsub('^x ' , '\U00D7', x)
      x <- gsub(' x ',' \U00D7', x)
      x <- gsub('\U00D7 ', '\U00D7', x)

    if(rank) {
      x <- gsub(' fo.|f.', '', x, fixed = TRUE)
      x <- gsub(' race', '', x, fixed = TRUE)
      x <- gsub(' var.|v.', '', x, fixed = TRUE)
      x <- gsub(' subsp.|ssp.', '', x, fixed = TRUE)
      x <- gsub(' [ranglos]', '', x, fixed = TRUE)
    }

    if(genus) {
      x <-	paste(sub('a$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
      x <-	paste(sub('as$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
      x <-	paste(sub('e$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
      x <-	paste(sub('es$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
      x <-	paste(sub('eus$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
      x <-	paste(sub('is$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
      x <-	paste(sub('on$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
      x <-	paste(sub('u$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
      x <-	paste(sub('um$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
      x <-	paste(sub('us$', '', substr(x, 1, regexpr('\ ', x)-1)), substr(x, regexpr('\ ', x), nchar(x)), sep='')
    }
    if(epithet) {
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('us\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('ae\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('arum\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('ea\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('ei\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('eos\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('ia\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('ium\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('ius\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('orum\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')

        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('a\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('e\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('ens\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('es\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('i\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('is\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('on\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('um\\b', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
        x <- paste(substr(x, 1, regexpr('\ ', x)-1), gsub('-', '', substr(x, regexpr('\ ', x), nchar(x))), sep='')
    }
 return(x)
}


#' Remove name authors from taxon names
#'
#' @export
#' @param x (character) vector of taxon names
#' @param rankattr (character) vector of rank attributes used in the taxonname strings
#' @name taxname.removeAuthors
#'

# taxname.removeAuthors <- function(x, sep) {
#     trimws(sapply(x, function(x) {
#       UC <- unlist(gregexpr("[A-Z]", x, perl=TRUE))
#       if(length(UC) == 0) stop(paste("Can not detect a valid taxon name in:", x))
#       if(length(UC) == 1) x else {
#       rest <- substr(x, UC[length(UC)], nchar(x))
#       if(grepl('subsp.|nothosubsp.|nssp.|var.|f.|cv.|agg.', rest)) {
#         paste(trimws(substr(x, 1, UC[2]-2)), trimws(sub(stringr::word(rest), '', rest)) )
#       } else
#       substr(x, 1, unlist(gregexpr("[A-Z]", x, perl=TRUE))[2]-2)
#       }
#     }
#     ))
# }
taxname.removeAuthors <- function(x, rankattr) {
  if(missing(rankattr)) rankattr = c("\ subsp\\.?\ |nothosubsp\\.?\ |\ nssp\\.?\ |var\\.?\ |\ for\\.?|\ cv\\.?\ |\ agg\\.|\ sect\\.\ |\ sectio\ |\ subg\\.\ |\ x\ ")
  trimws(sapply(x, function(x) {
    #UC <-       gregexpr("(?<!\\bsect\\.\\s||-)(?=\\s(?:[A-Z]|d'))", x, perl = TRUE) # include " d'Urv
    UC <- unlist(gregexpr("(?<!sect\\. |-)(d'|[A-Z])", x, perl = TRUE)) # Uppercase except after sect. or -
    if(length(UC) == 0) stop(paste("Can not detect a valid taxon name in:", x, '. No Uppercase word detected.'))
    if(length(UC) == 1) x else {
      if(grepl('[', x, fixed = TRUE))  x <- trimws(strsplit(x, '[', fixed = TRUE)[[1]][1])
      RI <- gregexpr(rankattr, x)
      rest <- substr(x, UC[length(UC)], nchar(x))
      if(all(RI[[1]] > UC[length(UC)])) {
        paste(trimws(substr(x, 1, UC[2]-2)), trimws(sub(stringr::word(rest), '', rest)) )
      } else
      if(all(RI[[1]] > UC[2])) {
        species <- stringr::word(trimws(substr(x, RI[[1]], nchar(x))), 2)
        paste(trimws(substr(x, 1, UC[2]-2)), trimws(substr(x, RI[[1]], (RI[[1]] + attr(RI[[1]], 'match.length')-1))), species, trimws(sub(stringr::word(rest), '', rest)) )
    } else
      substr(x, 1, unlist(gregexpr("[A-Z\U00E5\u00c1\u00c5\u010c\u00d6\u0160\u00dc\u017d]", x, perl=TRUE))[2]-2)
    }}
  ))
}
# ÁÅČÖŠÜŽ
# x <- c('Circaea x intermedia Ehrh. [alpina subsp. alpina x lutetiana subsp. lutetiana]', 'Anthriscus sylvestris (L.) Hoffm. subsp. sylvestris', 'Polycarpon polycarpoides subsp. catalaunicum O.Bolòs & Vigo', "Centaurium erythraea subsp. erythraea var. capitatum (Willd.) Melderis", "Eleocharis vulgaris Á. Löve & D. Löve 1975", 'xCalammophila')
# gregexpr("f\\.?|cv\\.?\ |agg\\.?\ ", x, perl=TRUE)
#
# taxname.removeAuthors(x)

#How to extent gregexpr("(?<!sect\\. |-)[A-Z]", x, perl = TRUE) so that it divides "Cyperus noanus Boiss." before " B" and "Taraxacum sect. Hamata H. Øllg." before " H." but "Crepis ramosissima d'Urv." before " d'Urv."?

#' Extract name authors from taxon names
#'
#' @export
#' @param x (character) vector of taxon names
#' @param rankattr (character) vector of rank attributes used in the taxonname strings
#' @name taxname.removeAuthors
#'
taxname.authors <- function(x, rankattr) {
  if(missing(rankattr)) rankattr <- c("\ subsp\\.?\ |nothosubsp\\.?\ |\ nssp\\.?\ |var\\.?\ |\ for\\.?|\ cv\\.?\ |\ agg\\.|\ sect\\.\ |\ subg\\.\ |\ x\ ")
  trimws(sapply(x, function(x) {
    UC <- unlist(gregexpr("(?<!sect\\. |-)[A-Z]", x, perl = TRUE)) # Uppercase except after sect.
    if(length(UC) == 0) stop(paste("Can not detect a valid taxon name in:", x, '. No Uppercase word detected.'))
    if(length(UC) == 1) NA else {
      # if(grepl('[', x, fixed = TRUE))
      #   x <- trimws(strsplit(x, '[', fixed = TRUE)[[1]][1])
      # RI <- gregexpr(sep, x)
      # a <- if(stringr::word(x, 2, 2))
      strsplit( substr(x, UC[2]-1, nchar(x)), split = paste0('\\[|', rankattr) )[[1]][1]
    }
    }))
}

# taxname.authors(df$name[1:10])
# taxname.authors(x)
# x <- df$name[3]
# rest1 <- substr(x, UC[1], nchar(x))
# rest2 <- substr(x, UC[2], nchar(x))
# rest3 <- substr(x, UC[3]-2, nchar(x))
# rest <- substr(x, UC[length(UC)], nchar(x))
#
# start1 <- substr(x, UC[1], UC[2])
# start2 <- substr(x, UC[1], UC[3])
# paste(trimws(substr(x, 1, UC[3]-2)), trimws(sub(stringr::word(rest), '', rest)) )
# paste(trimws(substr(x, 1, UC[2]-2)), trimws(sub(stringr::word(rest), '', rest)) )
#
#
# paste(trimws(substr(x, UC[2], UC[3]-2)), trimws(sub(stringr::word(rest), '', rest)) )
#

#' Parse taxon strings into genus part and epitheta
#'
#' @export
#' @param x (character) taxon names
#' @param epis (character) vector of separators for epithets (like e.g. "subsp.")
#' @name parse.taxa
#'

parse.taxa <- function(x, epis) {
  warning('Function does not account for intraspecific taxa without specifier ("subsp./var." etc. or aggregates ("agg.")')
  if(length(x) <= 1) stop('Only for vectors of length > 1, i.e. more than one taxon name')
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
          sep="", collapse=" ")
  }

  if(missing(epis)) epis <- c('subsp.', 'var.', 'v.')
  original <- x
  x <- taxname.abbr(x, species = TRUE)
  x <- sub(' sp.', replacement = '', x = x, fixed = TRUE)
  x <- sub(' spec.', replacement = '', x = x, fixed = TRUE)
  x <- sub(' SP.', replacement = '', x = x, fixed = TRUE)
  x <- sub(' SPEC.', replacement = '', x = x, fixed = TRUE)
  genus <- sapply(x, stringr::word, 1)
  genus <- sapply(genus, simpleCap)
  epi1 <- sapply(x, stringr::word, 2)
  intra <- sapply(epis, function(y) grepl(y, x, fixed = TRUE))
  rank.suff <- as.character(apply(intra, 1, function(x) epis[x]))
  epi2 <- character(length = length(x))
  if(any(intra)) for(i in 1:length(x)) if(rank.suff[i] != 'character(0)') epi2[i] <- paste(rank.suff[i], stringr::word(strsplit(x[i], rank.suff[i], fixed = TRUE)[[1]][2],2))
  species <- trimws(paste(genus, tolower(epi1), tolower(epi2)))
  species[species == "NA NA"] <- ''
  species <- sub(' NA', replacement = '', x = species, fixed = TRUE)
  epi2 <-  sub('subsp. ', replacement = '', x = epi2, fixed = TRUE)
  result <- data.frame(original, genus, epi1=tolower(epi1), rank.suff= if(length(rank.suff)>0) rank.suff else NA, epi2, scientificName = species)
  result$rank.suff[result$rank.suff == 'character(0)'] <- NA
  return(result)
}
# parse.taxa(x)
# add.legal <- c('-',';')



#' Recode taxon names, lettercodes or ID's
#'
#' @export
#' @param x vector of species
#' @param names one of 7digit shortletter codes, species id's or scientific species names
#' @param refl (character) name of taxon reference list
#' @name recoding.taxa
#'
recoding.taxa <- function(x, names = c('shortletters', 'Numbers', 'ScientificNames'), refl) {
  if('veg' %in% class(x)) {
    message('Vegetation matrix detected.\n It is assumed that species identifier are given in column names and consist either of 7 character lettercodes, or integer species IDs, with or without appendices for layer "species.layer' )
    refl <- attr(x, 'taxreflist')
    x <- colnames(x)
  } else  if(missing(refl)) refl <- tax.refl()
  names <- match.arg(names)
  taxonlist <- tax('all', refl)
  sp <- strsplit(x, split = '.', fixed = TRUE)
  taxa <- sapply(sp, '[', 1)
  if(all(nchar(taxa) == 6 | nchar(taxa) == 7)) {
    paste('Shortletters detected.')
    df.names <- data.frame(colnames = x, shortletters = sapply(sp, '[', 1), layer = sapply(sp, '[', 2))
    df.names$TaxonName <- taxonlist$TaxonConcept[match(df.names$shortletters, taxonlist$LETTERCODE)]
    df.names$TaxonUsageID <- taxonlist$TaxonUsageID[match(df.names$TaxonName, taxonlist$TaxonName)]
    if(any(is.na(df.names$TaxonUsageID))) warning('Not all shortletters could be resolved. Check column names and reflist.')
  }
  if(!any(is.na(suppressWarnings(as.integer(taxa))))) {
    paste('Numerical Species IDs detected.')
    df.names <- data.frame(colnames = x, TaxonUsageID = sapply(sp, '[', 1), layer = sapply(sp, '[', 2))
    df.names$TaxonName <- taxonlist$TaxonName[match(df.names$TaxonUsageID, taxonlist$TaxonUsageID)]
    df.names$shortletters <- taxonlist$LETTERCODE[match(df.names$TaxonUsageID, taxonlist$TaxonUsageID)]
    if(any(is.na(df.names$shortletters))) warning('Not all TaxonUsageIDs could be resolved. Check column names and reflist.')
  }
  if(!exists('df.names')) stop('Column names of vegetation matrix must consist either of 7 charachter lettercodes, or integer species IDs, with or without appendices for layer "species.layer"')
  if(names == 'shortletters')
    return(ifelse(is.na(df.names$layer), df.names$shortletters, paste(df.names$shortletters, df.names$layer, sep='.')))
  if(names == 'Numbers')
    return(ifelse(is.na(df.names$layer), df.names$TaxonUsageID, paste(df.names$TaxonUsageID, df.names$layer, sep='.')))
  if(names == 'ScientificNames')
    return(ifelse(df.names$layer == '0', df.names$TaxonName, paste(df.names$TaxonName, df.names$layer, sep='.')))
}

# rename.taxa(veg, 'N')



#' Standardise taxon list field names to match the Taxonomic Concept Transfer Schema (TCS)
#'
#' @name TCS.replace
#' @description Applies Taxonomic Concept Transfer Schema (TCS) to the different name list conventions of different sources
#'
#' @export
#' @param x (character) string of column names used in data.frames storing taxon lists
#'
#' @author Florian Jansen florian.jansen@uni-rostock.de
#' @references Taxonomic Names and Concepts interest group. 2006. Taxonomic Concept Transfer Schema (TCS), version 1.01. Biodiversity Information Standards (TDWG) http://www.tdwg.org/standards/117

TCS.replace <- function(x) {
  ## Turboveg & ## Florkart Germany
  x <- replace(x, toupper(x) %in% c('TAXONUSAGEID', 'SPECIES_NR', 'TAXNR', 'NAMNR', 'NAMEID', "NAME_ID", 'TAXONUSAGEID','TAXONID'), 'TaxonUsageID')
  x <- replace(x, toupper(x) %in% c('TAXONNAME','ABBREVIAT','TAXONNAME','TAXON','TAXNAME',"WISS_NAME"), 'TaxonName')
  x <- replace(x, toupper(x) %in% c('TAXONCONCEPTID','VALID_NR', 'SIPNR', 'NAMNR_GUELT', 'SYNNAMEID', 'TAXONCONCEPTID',"ACCEPTEDNAMEUSAGEID"), 'TaxonConceptID')
  x <- replace(x, toupper(x) %in% c('TAXONCONCEPT', 'VALID_NAME', 'VALIDNAME', 'TAXONCONCEPT'), 'TaxonConcept')
  x <- replace(x, toupper(x) %in% c('ISCHILDTAXONOFID', 'AGG', 'AGGNR', 'NAMEPARENTID', 'ISCHILDTAXONOFID', "PARENTNAMEUSAGEID"), 'IsChildTaxonOfID')
  x <- replace(x, toupper(x) %in% c('ISCHILDTAXONOF', 'AGG_NAME', 'AGGNAME', 'ISCHILDTAXONOF'), 'IsChildTaxonOf')
  x <- replace(x, toupper(x) %in% c('ACCORDINGTO', 'SECUNDUM'), 'AccordingTo')
  x <- replace(x, toupper(x) %in% c('VERNACULARNAME', 'NATIVENAME', "COMMONNAME"), 'VernacularName')
  x <- replace(x, toupper(x) %in% c('CLASSIFICATION','CLASSIFICA'), 'Classification')
  x <- replace(x, toupper(x) %in% c('TAXONRANK', 'RANG', 'RANK', "TAXONOMICRANK"), 'TaxonRank')
  x <- replace(x, toupper(x) %in% c('NAMEAUTHOR', 'AUTOR', 'AUTHOR',"SCIENTIFICNAMEAUTHORSHIP"), 'NameAuthor')
  x <- replace(x, toupper(x) %in% c('REFERENCE', 'BIBLIO'), 'REFERENCE')

  ## ESveg & FloraEuropaaea & sPlot
  x <- replace(x, toupper(x) %in% c("TAXONCODE", 'ETAXON', 'SPECIESID'), 'TaxonUsageID')
  x <- replace(x, toupper(x) %in% c('TAXON_NAME'), 'TaxonName')
  x <- replace(x, toupper(x) %in% c('PARENT'), 'IsChildTaxonOfID')
  x <- replace(x, toupper(x) %in% c('RANK_NAME'), 'TaxonRank')
  x <- replace(x, toupper(x) %in% c('SYNONYM_OF'), 'TaxonConceptID')
  x <- replace(x, toupper(x) %in% c('PLOTOBSERVATIONID', "OBSERVATIONCODE", "OBSERVATIONID","PLOTID", "RELEVE_NR"), "PlotObservationID")
  x <- replace(x, toupper(x) %in% c("STRATUMCODE"), "LAYER_ID")
  x <- replace(x, toupper(x) %in% c("STRATUM"), "LAYER")
  x <- replace(x, toupper(x) %in% c("PERCENTAGE_MEAN", "COVERPERCENT"), "Cover_Perc")

  ## CDM & EuroMed
  x <- replace(x, toupper(x) %in% c('NAME', 'TAXON', 'NAMECACHE'), 'TaxonName')
  x <- replace(x, toupper(x) %in% c('TAXONUUID', 'SYNUUID', 'RIDENTIFIER'), 'TaxonUsageID')
  x <- replace(x, toupper(x) %in% c('ACCTAXONID', 'ACCEPTEDTAXONFK'), 'TaxonConceptID')
  x <- replace(x, toupper(x) %in% c('RANKABBREV'), 'TaxonRank')
  x <- replace(x, toupper(x) %in% c('PARENTKEY'), 'IsChildTaxonOfID')
  x <- replace(x, toupper(x) %in% c('AUTHORSTRING'), 'NameAuthor')

  ## Veg-X
  x <- replace(x, toupper(x) %in% c('PLOTNAME', 'RELEVE_NR'), 'PlotObservationID')
  x <- replace(x, toupper(x) %in% c('ORGANISMIDENTITYNAME'), 'TaxonUsageID')
  x <- replace(x, toupper(x) %in% c('STRATUMNAME'), 'LAYER')
  x <- replace(x, toupper(x) %in% c('AGG_1_VALUE'), 'COVER_CODE')

  ## APG IV, wfo / taxize(db)
  x <- replace(x, toupper(x) %in% c('SCIENTIFICNAME'), 'TaxonName')
  x <- replace(x, toupper(x) %in% c("TAXONRANK"), 'TaxonRank')
  x <- replace(x, toupper(x) %in% c("PARENTNAMEUSAGE"), 'IsChildTaxonOf')

  return(x)
}

#' Rename data.frame columns to match Turboveg 2 conventions
#'
#' @export
#' @param x (character) string vector of column names
#' @name TV.replace
#'

TV.replace <- function(x) {
  ## Florkart Germany (BfN lists) and others
  x <- replace(x, toupper(x) %in% c("TAXONUSAGE", 'TAXONUSAGEID', 'TAXNR', 'NAMNR', 'NAMEID', "NAME_ID"), 'SPECIES_NR')
  x <- replace(x, toupper(x) %in% c('TAXONNAME','TAXON','TAXNAME',"WISS_NAME"), 'ABBREVIAT')
  x <- replace(x, toupper(x) %in% c('TAXONCONCE', 'VALIDNAME', 'TAXONCONCEPT'), 'VALID_NAME')
  x <- replace(x, toupper(x) %in% c('"TAXONCONC2"', 'TAXONCONCEPTID', 'SIPNR', 'SYNNAMEID','NAMNR_GUELT'), 'VALID_NR')
  x <- replace(x, toupper(x) %in% c('ISCHILDTAX', 'AGGNR', 'NAMEPARENTID', 'ISCHILDTAXONOFID',"TAXON_CHILD_OF"), 'AGG')
  x <- replace(x, toupper(x) %in% c( "ISCHILDTA2", 'ISCHILDTAXONOF'), 'AGG_NAME')
  x <- replace(x, toupper(x) %in% c('ACCORDINGTO'), 'SECUNDUM')
  x <- replace(x, toupper(x) %in% c("COMMONNAME", 'VERNACULAR', 'VERNACULARNAME'), 'NATIVENAME')
  x <- replace(x, toupper(x) %in% c('CLASSIFICATION'), 'CLASSIFICA')
  x <- replace(x, toupper(x) %in% c('RANK', 'TAXONOMICRANK', 'TAXONRANK'), 'RANG')
  x <- replace(x, toupper(x) %in% c('NAMEAUTHOR', 'AUTHOR'), 'AUTHOR')

  ## CDM & EuroMed
  x <- replace(x, toupper(x) %in% c('SYNUUID', 'RIDENTIFIER'), 'SPECIES_NR')
  x <- replace(x, toupper(x) %in% c('ACCTAXONID'), 'VALID_NR')
  x <- replace(x, toupper(x) %in% c('AGGNAME'), 'PARNAME')
  x <- replace(x, toupper(x) %in% c('PARENTKEY'), 'PARENT')

  ## Veg-X
  x <- replace(x, toupper(x) %in% c('PLOTNAME', 'PLOTOBSERVATIONID'), 'RELEVE_NR')
  x <- replace(x, toupper(x) %in% c('ORGANISMIDENTITYNAME'), 'SPECIES_NR')
  x <- replace(x, toupper(x) %in% c('STRATUMNAME'), 'LAYER')
  x <- replace(x, toupper(x) %in% c('AGG_1_VALUE'), 'COVER_CODE')

  return(x)
}

