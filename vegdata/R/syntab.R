#' Syntaxonomic frequency tables
#'
#' @rdname syntab
#' @aliases syntab print.syntab
#'
#' @description Calculate and display relative or absolute frequency tables with or without use of function multipatt from package indicspecies
#'
#' @param veg Vegetation dataframe
#' @param clust Vector with cluster information with length equal to number of rows of veg
#' @param type Relative (species or type) or absolute frequency, mean species response values or strength of association.
#' @param mupa Either logical for (not) using multipatt from package indicspecies to detect significance of cluster association strength or supply output from previous use of multipatt.
#' @param x Object from function syntab
#' @param zero.print Replacement for zero values.
#' @param trait Optional vector of trait values to be plotted behind the species.
#' @param limit Minimum value to display.
#' @param minstat Minimal indicator value
#' @param alpha Significance threshold.
#' @param dec Number of decimals in result.
#' @param refl Name of Turboveg taxonomic reference list to use for fullnames.
#' @param ... additional arguments
#'
#' @seealso Package indicspecies with function \link[indicspecies]{multipatt} for indicator species analysis along multiple cluster combinations
#'
#' @author Florian Jansen \email{florian.jansen@uni-rostock.de}
#'
#' @importFrom data.table .N ":="
#' @importFrom forcats as_factor fct_count
#'
#' @examples
#' \dontrun{
#'  elbaue <- tv.veg('elbaue')
#'  elbaue.env <- tv.site('elbaue')
#'  clust <- vector('integer', nrow(elbaue.env))
#'  clust[elbaue.env$MGL < -50 & elbaue.env$SDGL < 50] <- 1
#'  clust[elbaue.env$MGL < -50 & elbaue.env$SDGL >= 50] <- 2
#'  clust[elbaue.env$MGL >= -50 & elbaue.env$SDGL >= 50] <- 3
#'  clust[elbaue.env$MGL >= -50 & elbaue.env$SDGL < 50] <- 4
#'  levels(clust) <- c('dry.ld','dry.hd', 'wet.hd','wet.ld')
#'  st <- syntab(elbaue, clust, mupa=TRUE)
#'  traits <- tv.traits()
#'  m <- match(rownames(st$syntab), traits$LETTERCODE, nomatch = 0)
#'  trait <- traits[m, c("OEK_F","OEK_N")]
#'  rownames(trait) <- traits$LETTERCODE[m]
#'  print(st, limit=30, trait=trait)
#'  #'  Configure the syntaxonomic table
#'  sttable <- st$syntab
#'  sttable <- sttable[sttable$p.value < 0.05 & !is.na(sttable$p.value),
#'  !names(sttable) %in% c('stat')]
#'  taxa <- tax(rownames(sttable))
#'  rownames(sttable) <- taxa[match(rownames(sttable), taxa$LETTERCODE, nomatch = 0),'TaxonName']
#'  write.csv(sttable, 'sttable.csv')
#' }
#'

#' @export
"syntab" <- function(veg, clust, type = c('rel','abs','mean.cover'), mupa, dec = 0, refl, ...) UseMethod("syntab")

#' @export
syntab.veg <- function (veg, clust, type = c('rel','abs','mean.cover'), mupa = FALSE, dec = 0, refl, ...) {
    type <- match.arg(type)
    clust <- if (missing(clust)) as_factor(sample(c('one','two','three'), size = nrow(veg), replace = TRUE)) else
      as_factor(clust)
    ncl <- fct_count(clust)

    cat(' Number of clusters: ', nrow(ncl), '\n')
    nb.rel.clust <- as.numeric(table(clust))
    cat(' Cluster frequency', nb.rel.clust,'\n')
    if(any(nb.rel.clust == 0)) stop("All cluster levels must be represented by plots.")

    if(any(colSums(veg)==0)) stop('Some species without occurrence.')
    sp.veg <- split(veg, clust, drop=FALSE)
    if(type=='rel') {
    	tmp <- lapply(sp.veg, function(x) colSums(x > 0))
    	temp <- vector('list', length = nrow(ncl))
	    for(i in 1:length(nb.rel.clust))
	      temp[[i]] <- round(tmp[[i]] / nb.rel.clust[i] * 100, dec)
    } else
    if(type=='mean.cover') {
      temp <- lapply(sp.veg, function(x) {x[x==0] <- NA; round(colMeans(x, na.rm=TRUE),dec)})
      is.na(temp) <- 0
    } else
    if(type=='abs')  temp <- lapply(sp.veg, function(x) colSums(x > 0))

    df <- as.data.frame(temp)
    names(df) <- names(sp.veg)

  # Multipatt analysis
    if(is.logical(mupa)) {
      if(mupa) {
          requireNamespace("indicspecies", quietly = TRUE)
          mu <- indicspecies::multipatt(veg, clust, ...)
          mu$sign[, 1:nrow(ncl)] <- df
          df <- mu$sign
          names(df) <- gsub('s\\.', '', names(df))
        }
      } else
          if(inherits(mupa, 'multipatt')) {
            df <- mupa$sign
            names(df) <- gsub('s\\.', '', names(df))
          } else
             stop('Give multipatt object or set mupa to true or false.')
    df[is.na(df)] <- 0
#    colnames(df)[1:ncl] <- levels(clust)
    out <- list(clust=clust, syntab=df)
    class(out) <- c('syntab', 'list')
   invisible(out)
}

#' @export
syntab.data.table <- function (veg, clust, type = c('rel', 'relspec', 'abs', 'mean.cover'), mupa, dec = 0, refl, ...) {
  requireNamespace("data.table", quietly = TRUE)
  type <- match.arg(type)
  N <- NULL
  Total <- NULL
  RelativeFreq <- NULL
  TaxonName <- NULL
  clust <- as.factor(clust)
  if (missing(clust)) clust <- sample(1:2, size = nrow(veg), replace = TRUE)
  ncl <- length(unique(clust))
  cat(' Number of clusters: ', ncl, '\n')
  nb.rel.clust <- as.numeric(table(clust))
  cat(' Cluster frequency', nb.rel.clust,'\n')
  if(any(nb.rel.clust == 0)) stop("All cluster levels must be represented by plots.")
  if(is.null(levels(clust))) levels(clust) <- 1:length(table(clust))
  if(any(table(veg$TaxonName)==0)) stop('Some species without occurrence.')
  veg$clust <- clust[match(veg$RELEVE_NR, names(clust))]
  if(type=='relspec') {
    counts <- veg[, .N, by = .(TaxonName, clust)]  # Compute counts per TaxonName and type
    counts[, Total := sum(N), by = TaxonName]   # Compute total counts per TaxonName
    counts[, RelativeFreq := round(N / Total * 100, 1) ]   # Compute relative frequencies
    st <- data.table::dcast(counts, TaxonName ~ clust, value.var = "RelativeFreq", fill = 0)   # Convert to wide format
  } else
    if(type=='rel') {
      counts <- veg[, .N, by = .(TaxonName, clust)]  # Compute counts per TaxonName and type
      counts[, Total := sum(N), by = clust]   # Compute total counts per type
      counts[, RelativeFreq := round(N / Total * 100, 1) ]   # Compute relative frequencies
      st <- data.table::dcast(counts, TaxonName ~ clust, value.var = "RelativeFreq", fill = 0)   # Convert to wide format
  } else
    if(type=='mean.cover') {
      st <- data.table::dcast(veg, TaxonName ~ clust, fun.aggregate = mean)
    } else
      if(type=='abs')
        st <- data.table::dcast(veg, TaxonName ~ clust, fun.aggregate = length)

  out <- list(clust=clust, syntab=as.data.frame(st))
  class(out) <- c('syntab', 'list')
  invisible(out)
}

#--------------

if(getRversion() >= "2.15.1")  utils::globalVariables(c("st"))
#'
#' @export
#' @rdname syntab
print.syntab <- function(x, zero.print='.', trait, limit = 1, minstat = 0, alpha = 0.05, ...) {
  clust <- x$clust
  ncl <- length(unique(clust))
  cll <- levels(factor(clust))
  ntc <- as.numeric(table(clust))
  x <- x$syntab
  #suppressWarnings(colnames(x)[2:ncl] <- cll)
  if(any(c('stat','index','p.value') %in% names(x))) {
    if(any(is.na(x[1:(ncol(x)-3)]))) stop('NA values in frequency table. Do you have species without occurrences in your matrix?')
    mu = TRUE
    } else {
      if(any(is.na(x))) stop('NA values in frequency table. Do you have species without occurrences in your matrix?')
      mu = FALSE
    }
  if(mu) {
    stat <- x[,'stat']; x <- x[,-which(names(x)=='stat')]
    index <- x[,'index']; x <- x[,-which(names(x)=='index')]
    p.value <- x[,'p.value']; x <- x[,-which(names(x)=='p.value')]
    select <- stat > minstat & !is.na(stat) & p.value < alpha & !is.na(p.value) & apply(x, 1, function(y) max(y) >= limit)
    } else  select <- apply(x, 1, function(y) max(y) >= limit)
  if(sum(select)>0) {
    x <- x[select, ]
  if(zero.print != "0" && any(i0 <- x == 0)) {
      x[i0] <- sub("0", zero.print, x[i0])
      x[i0] <- sub("0.0", zero.print, x[i0])
      x[i0] <- sub("0.00", zero.print, x[i0])
  }

  if(mu) {
    if(sum(select) > 0)
       x <- cbind(x, index=index[select], stat=stat[select], p.value=p.value[select])
    x <- x[order(x$index),]
    }

  if(!missing(trait)) {
    if(is.null(names(trait))) stop('Trait vector must have names of taxa according to the vegetation matrix.')
    traitname <- names(trait) #as.character(substitute(trait))
    trait.df <- as.data.frame(trait[match(rownames(x), rownames(trait)),])
		x <- cbind(x, trait.df)
	}
  } else warning('No species exceed the chosen significance threshold.')
  cat('Number of clusters: ', ncl, '\n\n')
#  cat(' Cluster names           ', cll,'\n')
  cl <- t(data.frame(ntc)) #, matrix(nrow=2, ncol = (ncol(x)-ncl)))
  dimnames(cl) <- list(c('Cluster frequ:  '), cll)
  print(cl, row.names = FALSE, quote = FALSE)
  cat('\n')
  if(sum(select)>0) print.data.frame(x, ...)
  invisible(x)
 }

