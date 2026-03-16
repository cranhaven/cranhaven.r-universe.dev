#' Indicate site conditions with community weighted mean values of traits or with mode of gradient classes (sum of species amplitudes).
#' @name cwm
#' @aliases cwm
#' @export
#' @param veg Vegetation matrix with plots in rows and species in columns
#' @param refl Name of Turboveg taxonomic reference list
#' @param trait.db data frame with species trait values
#' @param ivname Name of the trait in trait.db to be used
#' @param keyname Name of the column in trait dataframe to join with colnames of veg table
#' @param method mean (weighted value of single traits, or mode (maximum) of trait classes)
#' @param db name of Turboveg database
#' @param weight additional weight, e.g niche breath of species
#' @param \dots additional arguments
#'
#' @description   Calculates community weighted mean trait values, like mean Ellenberg indicator values.
#'   Alternatively (method = 'mode') environmental conditions can be calculated according to the concept of sums of amplitudes of species along ecological gradients.
#' @details
#'   Trait values of 0 will be handled as NA values because Turboveg dBase can not handle NA values properly.
#'
#' @return Vector with the ecological classification of sites. Either mean trait values or mode of gradient classes.
#' @author Florian Jansen \email{florian.jansen@uni-rostock.de}
#'
#' @examples
#'   \dontrun{
#'     db <- 'elbaue'
#'     veg <- tv.veg(db, cover.transform='sqrt', check.critical = FALSE)
#'     site <- tv.site(db, verbose = FALSE)
#'
#'     #' Mean indicator values of Ellenberg F values
#'     mEIV_F <- cwm(veg, ivname = 'OEK_F', method = 'mean')
#'     plot(site$MGL, mEIV_F, xlab = 'Mean groundwater level')
#'   }
#'


#### community weighted trait means
cwm <- function(veg,
                refl,
                trait.db = 'ecodbase.dbf',
                ivname,
                keyname = 'LETTERCODE',
                method = c('mean', 'mode'),
                weight,
                db,
                ...
) {
  if(missing(refl) & missing(veg) & missing(db)) stop('Either refl, db, or a class "veg" object have to be provided.')
  if(!missing('refl'))
    if('veg' %in% class(veg)) refl <- attr(veg, 'taxreflist') else
      refl = tax.refl()
  suppressMessages(
    species <- tax('all', refl = refl, ...)
  )
  if(is.character(trait.db)) iv <- tv.traits(trait.db = trait.db, refl = refl, ...) else iv = trait.db
  if(missing(veg)) veg <- tv.veg(db, ...) else if(!'data.frame' %in% class(veg)) veg <- as.data.frame(veg)
  if(!all(ivname %in% names(iv))) stop('Not all ivname in table of indicators.')
  if(missing(weight)) {
    iv$weight <- 1
  #  weight <- 'weight'
  } else names(iv)[names(iv) == weight] <- 'weight'
  iv <- as.data.frame(cbind(iv[, match(ivname, names(iv))], iv[, keyname], iv[, 'weight']))
  names(iv) <- c(ivname, keyname, 'weight')
  # workaround
  for(i in 1:length(ivname)) {
    colnames(iv)[i] <- as.character(ivname[i])
    iv[,i] <- as.integer(as.character(iv[,i]))
  }
  if(!keyname %in% names(iv)) stop(paste(keyname, 'not in column names of trait dataframe.'))
  if(ivname %in% c('OEK_F', 'OEK_L', 'OEK_K', 'OEK_N', 'OEK_T') & any(iv[,ivname] == 0 & !is.na(iv[,ivname])))
    message('There are 0 values in the indicator list.')
  if(all(is.na(match(names(veg), iv[, keyname])))) stop('Taxon names in trait table and names in vegetation matrix do not match, please check.') else
  v <- as.matrix(iv[match(names(veg), iv[, keyname]), ivname])
  rownames(v) <- names(veg)
#  if(length(ivname) == 1) {
#    print(table(is.na(v)))
#    veg <- veg[,!is.na(v)] #?
#    v <- as.matrix(v[!is.na(v),]) #?
#  } else  v[is.na(v)] <- 0
  # Species * indicator Matrix of available Species
  w <- as.character(iv$weight[match(names(veg), iv[, keyname])])
  w[is.na(w)] <- "1"
  w <- as.numeric(w)
  veg <- t(t(veg) * w)
  # Method == mean

  if(method == 'mean') {
    io <- matrix(0, nrow = nrow(veg), ncol = ncol(v)) # Plots * sum of WS indicators for WS
    io <- apply(v, 2, function(x) rowSums(as.matrix(veg/apply(veg, 1, sum)) %*% x, na.rm=TRUE) )
    # io <- apply(v, 2, function(x) rowSums(as.matrix(veg) %*% x, na.rm=TRUE) )
    rS <- rowSums(io, na.rm = TRUE)
    IV <- io
    names(IV) <- rownames(veg)
  }
  # Method == max
  if(method == 'mode') {
    funMode <- function(x) {
        tab <- table(v[rownames(v) %in% dimnames(veg)[[2]][x > 0],])
        return(names(tab[which.max(tab)]))
    }
  IV <- apply(veg, 1, funMode)
  }
  if(any(IV == 0) & ivname != 'OEK_S') {
    message('The following plots might be without a single indicator species:\n')
    print(names(IV)[IV == 0])
  }
  return(IV)
}
### end of function

# showplot <- function(veg, plotids)  for(i in 1:length(plotids)) print(veg[plotids[i], veg[plotids[i],]>0])
# # showplot(veg, c("361", "362", "363"))
#
# showindiplot <- function(veg, trait.db, plotid, weight, keyname = 'LETTERCODE') {
#   if(length(plotid) > 1) {
#     warning('more than one plot selected. using only the first.')
#     plotid <- plotid[1]
#   }
#   if(missing(weight)) { trait.db$weight <- 1 } else names(trait.db)[names(trait.db) == weight] <- 'weight'
#   pl <- veg[plotid, veg[plotid,]>0] * trait.db[,'weight']
#   indi <- trait.db[match(names(pl), trait.db[, keyname]) , 3:8]
#   rownames(indi) <- colnames(pl)
#
#   indsum <- matrix(unlist(sapply(indi, function(x) x * pl)), nrow=nrow(indi))
#   rbind(sapply(indi, function(x) x * pl), SUM = colSums(indsum, na.rm = TRUE))
# }
# # showindiplot(veg, wsingo, which(ingo == '5+/4+/3+'))

