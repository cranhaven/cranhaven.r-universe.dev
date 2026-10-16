#' Convert road geometries to modeling segments
#'
#' Takes an `sf` object of LINESTRING/MULTILINESTRING road geometries and returns
#' a segment-level `sf` with stable segment IDs and metric lengths.
#'
#' v1 behavior:
#' * Drops Z/M dimensions
#' * Casts MULTILINESTRING -> LINESTRING (one row per linestring)
#' * Optionally splits at intersections (noding) when `split_at_intersections=TRUE`
#' * Computes `length_m` in meters (projects if lon/lat)
#' * Drops empty and (optionally) zero-length segments
#'
#' @param roads An `sf` object with LINESTRING or MULTILINESTRING geometries.
#' @param crs_m Metric CRS used for length calculation (and intersection splitting)
#'   when `roads` is lon/lat. Default 3857. For best accuracy, pass a local UTM EPSG.
#' @param keep_attrs Optional character vector of non-geometry columns to keep.
#'   If `NULL`, keeps all attributes.
#' @param drop_zero Logical; drop segments with non-positive length. Default TRUE.
#' @param split_at_intersections Logical; if TRUE, split lines at all intersections.
#'   Implemented via GEOS noding (`sf::st_union` + `sf::st_cast`)
#'   Default FALSE.
#' @param verbose Logical; emit simple messages about dropped rows. Default FALSE.
#'
#' @return An `sf` with columns:
#'   * `seg_id` integer 1..n
#'   * `length_m` numeric meters
#'   * geometry LINESTRING
#'   plus kept attributes.
#'
#' @export
roads_to_segments <- function(roads,
                              crs_m = 3857,
                              keep_attrs = NULL,
                              drop_zero = TRUE,
                              split_at_intersections = FALSE,
                              verbose = FALSE) {
  roads <- load_roads(roads)
  if (!inherits(roads, "sf")) stop("`roads` must be an sf object.")
  if (!is.numeric(crs_m) || length(crs_m) != 1L) stop("`crs_m` must be a single EPSG code (numeric).")

  # Optionally subset attributes early (but keep geometry)
  if (!is.null(keep_attrs)) {
    keep_attrs <- unique(as.character(keep_attrs))
    missing_cols <- setdiff(keep_attrs, names(roads))
    if (length(missing_cols) > 0) {
      stop("`keep_attrs` contains missing columns: ", paste(missing_cols, collapse = ", "))
    }
    roads <- roads[, unique(c(keep_attrs, attr(roads, "sf_column"))), drop = FALSE]
  }

  # Drop Z/M dimensions to avoid length surprises
  roads <- sf::st_zm(roads, drop = TRUE, what = "ZM")

  # Drop empty geometries
  empty <- sf::st_is_empty(roads)
  if (any(empty)) {
    if (verbose) message("Dropping ", sum(empty), " empty geometries.")
    roads <- roads[!empty, , drop = FALSE]
  }
  if (nrow(roads) == 0L) {
    out <- roads
    out$seg_id <- integer(0)
    out$length_m <- numeric(0)
    return(out[, c("seg_id", "length_m", setdiff(names(out), c("seg_id","length_m"))), drop = FALSE])
  }

  # Cast to LINESTRING, safely handling mixed LINESTRING + MULTILINESTRING inputs
  gtype <- as.character(sf::st_geometry_type(roads, by_geometry = TRUE))

  bad_types <- setdiff(unique(gtype), c("LINESTRING", "MULTILINESTRING"))
  if (length(bad_types) > 0) {
    stop("`roads` must have LINESTRING/MULTILINESTRING geometry. Found: ",
         paste(bad_types, collapse = ", "))
  }

  is_ls  <- gtype == "LINESTRING"
  is_mls <- gtype == "MULTILINESTRING"

  segs_ls <- if (any(is_ls)) roads[is_ls, , drop = FALSE] else roads[0, , drop = FALSE]

  # cast ONLY the MULTILINESTRING rows; casting a mixed sfc can drop parts
  segs_mls <- if (any(is_mls)) {
    sf::st_cast(roads[is_mls, , drop = FALSE], "LINESTRING", warn = FALSE)
  } else {
    roads[0, , drop = FALSE]
  }

  segs <- rbind(segs_ls, segs_mls)

  # Optional: split lines at intersections (noding) using GEOS via st_union
  if (isTRUE(split_at_intersections)) {
    crs_out <- sf::st_crs(segs)

    # Work in planar/metric CRS for robust topology
    if (isTRUE(sf::st_is_longlat(segs))) {
      segs_work <- sf::st_transform(segs, crs_m)
    } else {
      segs_work <- segs
    }

    # Union nodes the linework at all intersections; attributes will be dropped here,
    # so restore attributes by intersecting back with original features below.
    u <- sf::st_union(sf::st_geometry(segs_work))
    pieces <- sf::st_cast(u, "LINESTRING", warn = FALSE)

    # Wrap as sf for attribute restoration
    pieces_sf <- sf::st_sf(piece_id = seq_along(pieces), geometry = pieces)

    # Restore attributes, assign each piece to the original feature it intersects
    idx <- sf::st_intersects(pieces_sf, segs_work)

    if (length(idx) == 0L) {
      segs_work2 <- pieces_sf
    } else {
      # deterministic choice: first intersecting original segment
      pick <- vapply(idx, function(ii) if (length(ii) == 0L) NA_integer_ else ii[1L], integer(1))

      keep <- !is.na(pick)
      pieces_sf <- pieces_sf[keep, , drop = FALSE]
      pick <- pick[keep]

      # bind attributes from segs_work onto pieces
      attr_cols <- setdiff(names(segs_work), attr(segs_work, "sf_column"))
      segs_work2 <- cbind(segs_work[pick, attr_cols, drop = FALSE], pieces_sf)
      sf::st_geometry(segs_work2) <- sf::st_geometry(pieces_sf)
    }

    # Drop empty pieces that can appear after union/cast
    empty2 <- sf::st_is_empty(segs_work2)
    if (any(empty2)) {
      if (verbose) message("Dropping ", sum(empty2), " empty pieces after intersection splitting.")
      segs_work2 <- segs_work2[!empty2, , drop = FALSE]
    }

    # Transform back to original CRS for output geometry
    if (isTRUE(sf::st_is_longlat(segs))) {
      segs <- sf::st_transform(segs_work2, crs_out)
    } else {
      segs <- segs_work2
      sf::st_crs(segs) <- crs_out
    }
  }

  # Compute length in meters
  if (isTRUE(sf::st_is_longlat(segs))) {
    segs_m <- sf::st_transform(segs, crs_m)
    len <- sf::st_length(segs_m)
  } else {
    len <- sf::st_length(segs)
  }
  len_m <- as.numeric(units::set_units(len, "m"))
  segs$length_m <- len_m

  # Drop zero/negative lengths (and any NA)
  bad <- is.na(segs$length_m) | (!is.finite(segs$length_m))
  if (drop_zero) bad <- bad | (segs$length_m <= 0)

  if (any(bad)) {
    if (verbose) message("Dropping ", sum(bad), " segments with bad length.")
    segs <- segs[!bad, , drop = FALSE]
  }
  if (nrow(segs) == 0L) {
    out <- segs
    out$seg_id <- integer(0)
    return(out[, c("seg_id", "length_m", setdiff(names(out), c("seg_id","length_m"))), drop = FALSE])
  }

  # Stable IDs
  segs$seg_id <- seq_len(nrow(segs))

  # Put seg_id first
  keep_order <- c("seg_id", "length_m", setdiff(names(segs), c("seg_id", "length_m")))
  segs <- segs[, keep_order, drop = FALSE]

  segs
}








#' Build segment adjacency from segment geometries
#'
#' Constructs an undirected adjacency matrix `A` where segments are neighbors if
#' they share a node (endpoint). Intended to be used after `roads_to_segments()`.
#'
#' Isolates (degree 0) are kept (all-zero rows/cols). Connected components are
#' returned for ICAR sum-to-zero centering per component.
#'
#' @param segments An `sf` with LINESTRING geometries and (optionally) `seg_id`.
#' @param crs_m Metric CRS used when `segments` is lon/lat (for robust node keys).
#'   Default 3857.
#' @param tol Nonnegative numeric tolerance for snapping node coordinates (in meters
#'   if projected). If 0, uses exact coordinates. Default 0.
#' @param verbose Logical; emit simple messages. Default FALSE.
#'
#' @return A list with:
#' \describe{
#'   \item{A}{Sparse symmetric adjacency matrix (`dgCMatrix`).}
#'   \item{components}{Integer vector component id (length n). Isolates are their own components.}
#'   \item{isolates}{Logical vector (length n).}
#' }
#'
#' @export
build_adjacency <- function(segments, crs_m = 3857, tol = 0, verbose = FALSE) {
  if (!inherits(segments, "sf")) stop("`segments` must be an sf object.")
  if (!is.numeric(crs_m) || length(crs_m) != 1L) stop("`crs_m` must be a single EPSG code (numeric).")
  if (!is.numeric(tol) || length(tol) != 1L || tol < 0) stop("`tol` must be a single nonnegative number.")

  n <- nrow(segments)
  if (n == 0L) {
    A <- Matrix::Matrix(0, 0, 0, sparse = TRUE)
    return(list(A = A, components = integer(0), isolates = logical(0)))
  }

  gtype <- unique(as.character(sf::st_geometry_type(segments)))
  if (length(setdiff(gtype, "LINESTRING")) > 0) {
    stop("`segments` must have LINESTRING geometry (did you run roads_to_segments()?).")
  }

  # work in planar CRS for stable node keys
  segs_work <- segments
  if (isTRUE(sf::st_is_longlat(segs_work))) {
    segs_work <- sf::st_transform(segs_work, crs_m)
  }

  # ensure index 1..n in the current row order
  seg_index <- seq_len(n)

  # extract endpoints for each LINESTRING
  coords <- sf::st_coordinates(sf::st_geometry(segs_work))
  # coords has columns X,Y and grouping columns L1 (feature id) and L2 (part id) if present
  if (!("L1" %in% colnames(coords))) stop("Unexpected geometry coordinate structure from sf::st_coordinates().")

  # for each feature (L1), take first and last vertex
  split_by_feat <- split(seq_len(nrow(coords)), coords[, "L1"])
  first_idx <- vapply(split_by_feat, function(ii) ii[1L], integer(1))
  last_idx  <- vapply(split_by_feat, function(ii) ii[length(ii)], integer(1))

  x1 <- coords[first_idx, "X"]; y1 <- coords[first_idx, "Y"]
  x2 <- coords[last_idx,  "X"]; y2 <- coords[last_idx,  "Y"]

  # build node keys with optional snapping
  make_key <- function(x, y) {
    if (tol > 0) {
      x <- round(x / tol) * tol
      y <- round(y / tol) * tol
    }
    paste0(formatC(x, digits = 15, format = "fg"), ",", formatC(y, digits = 15, format = "fg"))
  }
  k1 <- make_key(x1, y1)
  k2 <- make_key(x2, y2)

  # node -> segments incidence
  keys <- c(k1, k2)
  segs <- c(seg_index, seg_index)
  node_map <- split(segs, keys)

  # build adjacency edges from node incidence sets
  ii_all <- integer(0)
  jj_all <- integer(0)

  for (nm in names(node_map)) {
    s <- unique(node_map[[nm]])
    k <- length(s)
    if (k >= 2L) {
      # all unordered pairs
      cmb <- utils::combn(s, 2L)
      ii_all <- c(ii_all, cmb[1L, ])
      jj_all <- c(jj_all, cmb[2L, ])
    }
  }

  # sparse adjacency; symmetrize; force 0 diagonal
  if (length(ii_all) == 0L) {
    A <- Matrix::Matrix(0, n, n, sparse = TRUE)
  } else {
    A <- Matrix::sparseMatrix(
      i = ii_all, j = jj_all, x = 1,
      dims = c(n, n), giveCsparse = TRUE
    )
    A <- A + Matrix::t(A)
    A@x[A@x != 0] <- 1
    Matrix::diag(A) <- 0
  }

  cc <- components_from_adjacency(A)
  list(A = A, components = cc$components, isolates = cc$isolates)

}







#' Connected components and isolates from an adjacency matrix
#'
#' Computes connected component membership (1..K) and isolates (degree 0) from an
#' undirected adjacency matrix. Intended for ICAR centering per component.
#'
#' @param A Square adjacency matrix (base matrix or `Matrix` sparse).
#'
#' @return A list with:
#' \describe{
#'   \item{components}{Integer vector of length n giving component id (1..K).}
#'   \item{isolates}{Logical vector of length n, TRUE if degree 0.}
#' }
#'
#' @keywords internal
components_from_adjacency <- function(A) {
  if (!(is.matrix(A) || inherits(A, "Matrix"))) stop("`A` must be a matrix or Matrix sparse object.")
  if (nrow(A) != ncol(A)) stop("`A` must be square.")
  n <- nrow(A)
  if (n == 0L) return(list(components = integer(0), isolates = logical(0)))

  if (!inherits(A, "Matrix")) A <- Matrix::Matrix(A, sparse = TRUE)

  Matrix::diag(A) <- 0
  A@x[A@x != 0] <- 1

  deg <- Matrix::rowSums(A != 0)
  isolates <- (deg == 0)

  # Build adjacency list from sparse summary robustly
  adj_list <- vector("list", n)
  if (Matrix::nnzero(A) > 0L) {
    sm <- Matrix::summary(A)
    sm <- as.matrix(sm)

    col_i <- match("i", colnames(sm))
    col_j <- match("j", colnames(sm))
    if (is.na(col_i) || is.na(col_j)) {
      col_i <- 1L
      col_j <- 2L
    }

    ii <- as.integer(sm[, col_i])
    jj <- as.integer(sm[, col_j])

    adj_list <- split(jj, ii)

    # Ensure all indices exist
    present <- as.integer(names(adj_list))
    miss <- setdiff(seq_len(n), present)
    for (m in miss) adj_list[[as.character(m)]] <- integer(0)
    adj_list <- adj_list[as.character(seq_len(n))]
  } else {
    for (i in seq_len(n)) adj_list[[i]] <- integer(0)
  }

  comp <- integer(n)
  visited <- rep(FALSE, n)
  comp_id <- 0L

  for (v in seq_len(n)) {
    if (!visited[v]) {
      comp_id <- comp_id + 1L
      stack <- v
      visited[v] <- TRUE
      comp[v] <- comp_id

      while (length(stack) > 0L) {
        cur <- stack[[length(stack)]]
        stack <- stack[-length(stack)]
        nb <- adj_list[[cur]]
        if (length(nb) > 0L) {
          new_nb <- nb[!visited[nb]]
          if (length(new_nb) > 0L) {
            visited[new_nb] <- TRUE
            comp[new_nb] <- comp_id
            stack <- c(stack, new_nb)
          }
        }
      }
    }
  }

  list(components = comp, isolates = isolates)
}
