#' Build a road network graph from sf LINESTRING data
#'
#' @param roads_sf An sf object with LINESTRING geometry
#' @param crs_out Integer EPSG code for projected CRS
#' @param node_intersections Logical; if TRUE, "node" the linework by splitting
#'   at interior intersections/junctions (via `sf::st_union()`), so that crossings
#'   and T-junctions become graph nodes even when they are not endpoints.
#'   This may increase the number of edge segments.
#' @param snap_tol Nonnegative numeric; optional snapping tolerance (in projected
#'   CRS units) used to merge nearly identical endpoints. Use 0 to disable.
#' @param simplify Logical; if TRUE, remove self-loops and parallel edges.
#'
#' @return A list with components:
#' \itemize{
#'   \item roads: cleaned sf object
#'   \item nodes: sf POINT object with node_id
#'   \item edges: sf LINESTRING object with from, to, length
#'   \item graph: igraph object
#'   \item A: sparse adjacency matrix
#' }
#'
#' @export
build_network <- function(roads_sf,
                          crs_out = 3857,
                          node_intersections = FALSE,
                          snap_tol = 0,
                          simplify = TRUE) {
  stopifnot(inherits(roads_sf, "sf"))

  if (!is.logical(node_intersections) || length(node_intersections) != 1L) {
    stop("`node_intersections` must be a single logical value.")
  }
  if (!is.numeric(snap_tol) || length(snap_tol) != 1L || !is.finite(snap_tol) || snap_tol < 0) {
    stop("`snap_tol` must be a single nonnegative finite number.")
  }
  if (!is.logical(simplify) || length(simplify) != 1L) {
    stop("`simplify` must be a single logical value.")
  }

  # clean + project (+ optional noding at intersections)
  roads <- roads_sf[!sf::st_is_empty(roads_sf), ]
  roads <- sf::st_transform(roads, crs_out)
  roads <- suppressWarnings(sf::st_cast(roads, "LINESTRING"))

  if (node_intersections) {
    # GEOS union "nodes" the linework by splitting at crossings/intersections
    u <- sf::st_union(sf::st_geometry(roads))
    roads_geom_noded <- suppressWarnings(sf::st_cast(u, "LINESTRING"))
    roads <- sf::st_sf(geometry = roads_geom_noded)
    sf::st_crs(roads) <- sf::st_crs(crs_out)
  }

  # nodes (unique by geometry)
  coords <- sf::st_coordinates(sf::st_geometry(roads))
  if (nrow(coords) == 0L) {
    # empty after cleaning
    nodes <- sf::st_sf(geometry = sf::st_sfc(crs = sf::st_crs(roads)))
    nodes$node_id <- integer(0)
    edges <- roads
    edges$from <- integer(0)
    edges$to <- integer(0)
    edges$length <- units::set_units(numeric(0), "m")
    g <- igraph::make_empty_graph(0, directed = FALSE)
    A <- Matrix::Matrix(0, 0, 0, sparse = TRUE)
    return(list(roads = roads, nodes = nodes, edges = edges, graph = g, A = A))
  }

  # NOTE: if node_intersections = FALSE, nodes are only segment endpoints.
  # if node_intersections = TRUE, lines are noded first so interior junctions
  # become endpoints before building nodes/edges

  # Identify endpoints of each LINESTRING via L1 (first) and Ln (last) per feature
  # st_coordinates() groups coordinates by feature via L1 (and sometimes L2/L3)
  key_cols <- intersect(colnames(coords), c("L1", "L2", "L3"))
  if (length(key_cols) == 0L) {
    stop("Could not identify feature grouping columns in `st_coordinates()` output.")
  }
  split_idx <- do.call(
    interaction,
    c(as.data.frame(coords[, key_cols, drop = FALSE]), drop = TRUE)
  )

  first_row <- tapply(seq_len(nrow(coords)), split_idx, min)
  last_row  <- tapply(seq_len(nrow(coords)), split_idx, max)

  start_xy <- coords[first_row, c("X", "Y"), drop = FALSE]
  end_xy   <- coords[last_row,  c("X", "Y"), drop = FALSE]

  # optional snapping to merge nearly identical endpoints
  if (snap_tol > 0) {
    start_xy <- round(start_xy / snap_tol) * snap_tol
    end_xy   <- round(end_xy   / snap_tol) * snap_tol
  }

  # stable keying to avoid NA matches from formatting differences
  fmt <- function(x) sprintf("%.8f", x)
  mk_key <- function(xy) paste0(fmt(xy[, 1]), ",", fmt(xy[, 2]))

  pts_xy <- rbind(start_xy, end_xy)
  pts_key <- mk_key(pts_xy)
  keep <- !duplicated(pts_key)
  pts_xy_unique <- pts_xy[keep, , drop = FALSE]

  node_geom <- sf::st_sfc(
    lapply(seq_len(nrow(pts_xy_unique)), function(i) sf::st_point(unname(pts_xy_unique[i, ]))),
    crs = sf::st_crs(roads)
  )
  nodes <- sf::st_sf(geometry = node_geom)
  nodes$node_id <- seq_len(nrow(nodes))

  # map each endpoint to node id
  # build a lookup from key -> node_id
  node_key <- mk_key(pts_xy_unique)
  key_to_id <- seq_along(node_key)
  names(key_to_id) <- node_key

  start_key <- mk_key(start_xy)
  end_key   <- mk_key(end_xy)

  from <- unname(key_to_id[start_key])
  to   <- unname(key_to_id[end_key])

  # edges
  edges <- roads
  edges$from <- as.integer(from)
  edges$to   <- as.integer(to)
  edges$length <- sf::st_length(edges)

  # remove NA endpoints and self-loops at the edge level (start == end)
  keep_edges <- !is.na(edges$from) & !is.na(edges$to) & (edges$from != edges$to)
  edges <- edges[keep_edges, ]

  # graph (integer node IDs)
  # keep all nodes as vertices so isolates are retained
  g <- igraph::make_empty_graph(n = nrow(nodes), directed = FALSE)

  if (nrow(edges) > 0L) {
    edge_mat <- as.matrix(sf::st_drop_geometry(edges[, c("from", "to")]))
    g <- igraph::add_edges(g, as.vector(t(edge_mat)))
  }

  # adjacency
  A <- igraph::as_adjacency_matrix(g, sparse = TRUE)
  Matrix::diag(A) <- 0
  A <- A + Matrix::t(A)
  A@x[A@x != 0] <- 1
  A <- Matrix::drop0(A, tol = 0)

  net <- list(
    roads = roads,
    nodes = nodes,
    edges = edges,
    graph = g,
    A = A
  )

  if (simplify) {
    net <- simplify_network(net, keep_edge = "first")
  }

  net
}

