

reShape.veg <- function (veg, crop = TRUE, refl) {
if(!'veg' %in% class(veg)) stop('Only applicable for objects of class \"veg\".')
if(any(is.null(dimnames(veg)))) stop('Only applicable if dimnames are present (Plot IDs as rownames, species IDs as colnames.')
if(is.null(attr(veg, 'taxreflist')) & missing(refl)) stop('Set option refl because attribute \"taxreflist\" is not set for object \"veg\".')
if(any(is.na(as.integer(row.names(veg))))) stop("Plot ID's must be integer numbers.")
  plots <- as.integer(as.character(dimnames(veg)[[1]][row(veg)]))
  perf <- as.vector(as.matrix(veg))
  spec <- dimnames(veg)[[2]][col(veg)]
  spec <- sapply(spec, function(x) ifelse(grepl('\\.+[A-Z]', x), sub('.', '_', x, fixed = TRUE), x))
  spec <- sapply(spec, function(x) ifelse(grepl('\\.+[0-9]$', x), x, paste(x, '0', sep = '.')))
  spcnames <- sapply(spec, function(x) strsplit(as.character(x), ".", fixed = TRUE))
  layer <- as.integer(lapply(spcnames, function(x) x[2]))
  code <- unlist(lapply(spcnames, function(x) x[1]))
  code <- type.convert(code, as.is = TRUE)
  if(is.character(code)) {
    if(all(sapply(code, nchar) == 7)) {
      TaxonUsageID <- integer(length(code))
      taxa <- tax(code, syn = FALSE)
      TaxonUsageID <- taxa$TaxonUsageID[match(code, taxa$LETTERCODE)]
    } else {
      TaxonUsageID <- integer(length(code))
      taxa <- tax(code, refl = refl, strict = TRUE, syn = FALSE)
      TaxonUsageID <- taxa$TaxonUsageID[match(code, taxa$TaxonName)]
    }
  } else TaxonUsageID <- as.integer(code)
  df <- data.frame(PlotObservationID = plots, SPECIES_NR = TaxonUsageID,
                   COVER_CODE = as.character(perf), LAYER = layer, stringsAsFactors = FALSE)
  df <- df[order(df$PlotObservationID, df$SPECIES_NR), ]
  df <- df[df$COVER_CODE != 0 & !is.na(df$COVER_CODE), ]
  class(df) <- c("tv.obs", "data.frame")
  if (!is.null(attr(veg, "taxreflist")))
    attr(df, "taxreflist") <- attr(veg, "taxreflist")
  return(df)
}
