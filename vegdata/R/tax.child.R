#' Search taxonomic reference lists including concept synonomy and taxonomic hierarchy.
#'
#' @description Search all (accepted) children of a taxon down to gen generations
#' @name child
#'
#' @export
#' @param x Species number, lettercode or species name(s)
#' @param refl Taxonomic reference list
#' @param gen Number of child generations to return
#' @param syn Should synonyms be included in results
#' @param include.parent Should the parent taxon be included in results
#' @param \dots additional paarmeters for function tax
#'
#' @inherit tax return details
#'
#' @references
#'   Jansen, F. and Dengler, J. (2008) GermanSL - eine universelle taxonomische Referenzliste für Vegetationsdatenbanken. Tuexenia, 28, 239-253.
#'
#' @author Florian Jansen \email{florian.jansen@uni-rostock.de}
#'
child <- function (x, refl = tax.refl(), gen = 3, syn = FALSE, include.parent = FALSE, ...) {
  if(syn) stop('Integrating synonyms not yet implemented!')
  if(gen < 1) stop('At least one generation (positive integers) needs to be selected.')
  if(is.character(refl))
    suppressMessages(species <- tax("all", refl = refl, syn = TRUE, ...))
  else species <- refl
  if(any(!c('IsChildTaxonOfID', 'TaxonUsageID', 'TaxonName') %in% names(species)))
    stop('IsChildTaxonOfID, TaxonUsageID, and TaxonName need to be present in the reference list')
  suppressMessages(
    x <- tax(x, refl = refl, strict = TRUE, ...)$TaxonUsageID
  )
  if(length(x) > 1) {
    warning('More than one taxon, using only the first.')
    x <- x[1]
  }
  ## function
  chfun <- function(df, species, t=1, include.parent = FALSE, syn = FALSE, ...) {
    if(!'SYNONYM' %in% names(df) & 'status' %in% names(df)) df$SYNONYM <- df$status == 'Accepted'
    y <- df$TaxonUsageID[!df$SYNONYM & !is.na(df$SYNONYM)]
    ch <- species[species$IsChildTaxonOfID %in% y, ]
    if(nrow(ch) > 0) {
      ch$Generation <- t
      if(include.parent) {
        df$Generation = t-1
        rbind(df, ch)
      } else ch
    } else df[0, ]
  }

  ## taxon
  # if(is.factor(x)) x <- as.character(x)
  # if(is.numeric(x) | is.integer(x))  ## Tax numbers
    s <- species[match(x, species$TaxonUsageID, nomatch = 0),]
  # if(is.character(x)) ## Selecting by string
    # if(all(sapply(x, function(x) nchar(x) == 36))) {
    #   stop('taxon id seems to be 36 character Taxon UUID . Not implemented.')
    # } else    																				## Taxnames
      # s <- species[match(x, species$TaxonName, nomatch=0),]
  if(nrow(s) == 0) {
    warning('Could not find valid taxon.')
    return(s)
    } else
    {
    x <- species[match(s$TaxonConceptID, species$TaxonUsageID),'TaxonConceptID']
    df <- species[match(x, species$TaxonUsageID),]
    df$Generation <- 0

    G <- df
    # t = 1
    for(t in 1:gen) {
      Gl <- split(G[G$Generation == t-1, ], seq(sum(G$Generation == t-1)))
      for(i in 1:length(Gl))
        if(nrow(Gl[[i]]) > 0)
          if(Gl[[i]]$Generation == t-1)  Gl[[i]] <- chfun(Gl[[i]], species, t, include.parent = include.parent)
          G = rbind(G, do.call("rbind", Gl), make.row.names = FALSE)
    }
    if(!include.parent) G <- G[G$TaxonUsageID != x, ]
    G <- G[!duplicated(G), ]
    if(nrow(G) > 0) {
      message('Children of ', df$TaxonName, ' (', df$TaxonUsageID, ')', ':', sep='')
      message(paste0(capture.output(G[, names(G) %in% c('TaxonUsageID','TaxonName','NameAuthor','TaxonRank','IsChildTaxonOfID','GENERATION','EDITSTATUS', 'Generation')]), collapse = '\n'))
    invisible(G)
    }
  }
}

#' #' Parents of a taxon
#'
#' @name parent
#' @export
#' @param x Species number, lettercode or species name(s)
#' @param refl Taxonomic reference list
#' @param rank taxonomic level of taxa to find
#' @param taxlevels A data frame defining the hierarchy of taxon levels, see vegdata::taxlevels
#' @param \dots additional attributes for function tax
#'
#' @inherit tax return details
#'
#' @references
#'   Jansen, F. and Dengler, J. (2008) GermanSL - eine universelle taxonomische Referenzliste für Vegetationsdatenbanken. Tuexenia, 28, 239-253.
#'
#' @author Florian Jansen \email{florian.jansen@uni-rostock.de}
#'

parent <- function (x, refl = tax.refl(), rank, taxlevels, ...) {
  if(missing(taxlevels))
    taxonlevels <- factor(vegdata::taxlevels$level, levels= vegdata::taxlevels$level, ordered = TRUE) else taxonlevels <- taxlevels
  if(is.character(refl))
    suppressMessages(species <- tax("all", refl = refl, syn = TRUE, ...))
  else species <- refl
  if(length(x)>1) {
  	warning('More than one match, using only first.')
  	x <- x[1]
  }
  suppressMessages(
    s <- tax(x, refl = refl, strict = TRUE, ...)
  )
  if(nrow(s) != 1) stop(x, ' can not be found in list.', s$TaxonUsageID)
  y <- species[match(s$TaxonConceptID, species$TaxonUsageID),]
  if(y$TaxonUsageID != s$TaxonUsageID) warning('Synonym, will use valid taxon "', y$TaxonName, '" instead.')
  # Fehler in if (y$TaxonUsageID != s$TaxonUsageID) warning("Synonym, will use valid taxon \"",  :
  #                                                           Argument hat Länge 0
  y$GENERATION <- 0
  p <- species[match(unique(y$IsChildTaxonOfID),species$TaxonUsageID),]
  p$GENERATION <- 1

  lo <- function(y, p) {
    if(nrow(p) == 0) { message(y$TaxonName, 'has no parents.\n') }
    else {
      p2 <- p
      t <- 1
        repeat {
          t <- t+1
          p2 <- species[match(p2$IsChildTaxonOfID,species$TaxonUsageID),]
  				if(is.na(p2$TaxonName)) break
          p2$GENERATION <- t
          p <- rbind(p, p2)
          if(p2$TaxonUsageID == 0 ) break
        }
      }
    return(p)
  }

  if(!missing(rank)) {
    if(!rank %in% taxonlevels) stop(c('Rank must be one of ', taxonlevels))
    if(taxonlevels[match(rank, taxonlevels)] <= taxonlevels[match(y$TaxonRank, taxonlevels)]) {
      warning('Species is of equal or higher rank than the specified parent level.')
      p <- c(TaxonName='')
    } else {
      P <- lo(y, p)
      P <- P[which(P$TaxonRank == rank), ]
      message('Parent level', rank, ' of ', y$TaxonName, '(', y$TaxonUsageID, '):\n')
      if(nrow(P) == 0)
        cat('"Incertae sedis" = uncertain placement within this level.\n')
    }
  } else P <- lo(y, p)
    message('Parents of ', s$TaxonName, ' (', y$TaxonUsageID, ')', if(y$TaxonUsageID != s$TaxonUsageID) {paste(" = Synonym of ", y$TaxonName, '(', y$TaxonConceptID, ')')}, ':\n', sep='')
    message(paste0(capture.output(P[, names(P)[names(P) %in% c('TaxonUsageID','TaxonName','TaxonRank','IsChildTaxonOfID','GENERATION')]]), collapse = "\n"))
  invisible(P)
}


parents <- function(...) parent(...)

#' Search synonyms of a taxon
#'
#' @name syn
#' @export
#' @param x Species number, lettercode or species name(s)
#' @param refl Taxonomic reference list
#' @param \dots additional attributes for function tax
#'
#' @inherit tax return details
#'
#' @references
#'   Jansen, F. and Dengler, J. (2008) GermanSL - eine universelle taxonomische Referenzliste für Vegetationsdatenbanken. Tuexenia, 28, 239-253.
#'
#' @author Florian Jansen \email{florian.jansen@uni-rostock.de}
#'

syn <- function (x, refl = tax.refl(), ...) {
  suppressMessages(species <- tax('all', refl = refl, syn = TRUE, strict = TRUE) )
  suppressMessages(x <- tax(x, refl=refl, strict=TRUE)$TaxonUsageID )
  if(length(x) == 0) {
	  warning('Taxon (id) Unknown.')
	  s <- species[0, ]
	} else {
	  if(length(x) > 1) {
      warning('More than one match, using only first.')
      x <- x[1]
    }
	v <- species[match(x, species$TaxonUsageID), 'TaxonConceptID']
  s <- species[which(species$TaxonConceptID == v),]
	}

    message('Name swarm of ', s$TaxonName[s$TaxonUsageID == x],':\n')
    if('EDITSTATUS' %in% names(s))
      print(s[, c('TaxonUsageID','TaxonName','SYNONYM','EDITSTATUS')])  else
      print(s[, c('TaxonUsageID','TaxonName','SYNONYM')], row.names=FALSE)
  invisible(s)
}


agg <- function(x, refl = 'GermanSL 1.2', species, ...) {
  message('Deprecated function. Use child(x, gen=1, ...) instead.')
}

