## ---------------------------------------------------------------------
## qrfactor: compatibility layer for retired dependencies
##
## Replaces functions from packages that are no longer installable on
## R >= 4.2:
##     rgdal::readOGR      -> sf::st_read
##     maptools::spCbind   -> data-frame cbind on the sf object
##     mgraph::histmap     -> base graphics hist + normal curve
##     mgraph::boxmap      -> base graphics boxplot
##
## Design note: the original code manipulates Spatial*DataFrame objects
## via slot(obj, "data") and obj@data. An sf object IS a data frame with
## a geometry column, so attribute access becomes direct. The helpers
## .qr_attr() / .qr_attr<-() isolate that difference so the call sites in
## rq() and plot.qrfactor() need only a one-line change each.
## ---------------------------------------------------------------------

## --- spatial I/O ------------------------------------------------------

.qr_read <- function(source, layer) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required to read spatial data. ",
         "Install it with install.packages('sf').", call. = FALSE)
  }
  ## sf::st_read takes dsn = directory, layer = layer name (no extension)
  obj <- sf::st_read(dsn = source, layer = layer, quiet = TRUE,
                     stringsAsFactors = FALSE)
  obj
}

## Attribute table of a spatial object, WITHOUT the geometry column.
## Replaces: slot(object, "data")  and  object@data
.qr_attr <- function(object) {
  if (inherits(object, "sf")) {
    return(sf::st_drop_geometry(object))
  }
  if (isS4(object) && methods::.hasSlot(object, "data")) {
    return(methods::slot(object, "data"))   # legacy Spatial*DataFrame
  }
  as.data.frame(object)
}

## Replacement for: slot(object, "data") <- value
`.qr_attr<-` <- function(object, value) {
  if (inherits(object, "sf")) {
    geom <- attr(object, "sf_column")
    g    <- object[[geom]]
    out  <- value
    out[[geom]] <- g
    return(sf::st_as_sf(out))
  }
  if (isS4(object) && methods::.hasSlot(object, "data")) {
    methods::slot(object, "data") <- value
    return(object)
  }
  value
}

## Replacement for maptools::spCbind(spatial_obj, data_frame).
## spCbind matched rows by row.names; sf objects keep row order, so we
## re-order the incoming data frame to match when row names are usable.
.qr_cbind <- function(object, df) {
  df <- as.data.frame(df)
  a  <- .qr_attr(object)
  if (!is.null(rownames(df)) && !is.null(rownames(a)) &&
      all(rownames(a) %in% rownames(df)) && nrow(df) == nrow(a)) {
    df <- df[rownames(a), , drop = FALSE]
  }
  if (nrow(df) != nrow(a)) {
    stop("Cannot combine: spatial object has ", nrow(a),
         " features but data has ", nrow(df), " rows.", call. = FALSE)
  }
  dup <- intersect(names(df), names(a))
  if (length(dup)) df <- df[, setdiff(names(df), dup), drop = FALSE]
  .qr_attr(object) <- cbind(a, df)
  object
}

## --- spplot bridge (sf -> Spatial at the plot boundary) ---------------
## plot.qrfactor was written against sp: it calls coordinates(), spplot()
## and tests typeof(gisdata)=="S4" throughout. rq() now stores gisdata as
## an sf object. Rather than rewrite ~190 spplot/coordinates sites, we
## convert the geometry back to a Spatial*DataFrame once, where it enters
## the plot method. Non-sf input (a plain data frame) is returned intact.
.qr_as_sp <- function(x) {
  if (inherits(x, "sf")) {
    if (!requireNamespace("sp", quietly = TRUE)) {
      stop("Package 'sp' is required for map plots. ",
           "Install it with install.packages('sp').", call. = FALSE)
    }
    return(methods::as(x, "Spatial"))
  }
  x
}

## --- plotting replacements (formerly mgraph) --------------------------

.qr_histmap <- function(data, attribute, label = attribute,
                        col = "blue", type = "normal", ...) {
  v <- suppressWarnings(as.numeric(data[[attribute]]))
  v <- v[is.finite(v)]
  if (!length(v)) {
    graphics::plot.new(); graphics::title(main = label); return(invisible(NULL))
  }
  dens <- identical(type, "normal")
  h <- graphics::hist(v, main = label, xlab = attribute,
                      col = col, freq = !dens, ...)
  if (dens && stats::sd(v) > 0) {
    xs <- seq(min(v), max(v), length.out = 200)
    graphics::lines(xs, stats::dnorm(xs, mean(v), stats::sd(v)), lwd = 2)
  }
  invisible(h)
}

.qr_boxmap <- function(data, attribute, factor = NULL, label = attribute,
                       col = "black", type = "notch", ...) {
  notch <- identical(type, "notch")
  v <- suppressWarnings(as.numeric(data[[attribute]]))
  if (!is.null(factor) && factor %in% names(data)) {
    f <- as.factor(data[[factor]])
    b <- suppressWarnings(
      graphics::boxplot(v ~ f, main = label, xlab = factor,
                        ylab = attribute, border = col, notch = notch, ...))
  } else {
    b <- suppressWarnings(
      graphics::boxplot(v, main = label, ylab = attribute,
                        border = col, notch = notch, ...))
  }
  invisible(b)
}
