#' Convert an lb_result to an igraph object
#'
#' Extracts node and relationship columns from a LadybugDB query result and
#' constructs an [igraph::graph_from_data_frame()] object.
#'
#' Node columns must have LadybugDB type `NODE` and relationship columns type
#' `REL`/`RELATIONSHIP`. When the result contains no structural columns the
#' function aborts with an informative error.
#'
#' @param x An `lb_result` object.
#' @param ... Unused; kept for S3 compatibility.
#'
#' @return A directed `igraph` object whose vertices and edges represent the
#'   nodes and relationships in the query result. Database properties become
#'   vertex and edge attributes; repeated internal identifiers are deduplicated.
#'
#' @examples
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'   with_lb_connection(":memory:", {
#'     lb_close(lb_execute(
#'       conn, "CREATE NODE TABLE Person (name STRING, PRIMARY KEY(name))"
#'     ))
#'     lb_close(lb_execute(conn, "CREATE REL TABLE Knows (FROM Person TO Person)"))
#'     lb_close(lb_execute(
#'       conn, "CREATE (:Person {name:'A'})-[:Knows]->(:Person {name:'B'})"
#'     ))
#'     result <- lb_execute(
#'       conn, "MATCH (a:Person)-[r:Knows]->(b:Person) RETURN a, r, b"
#'     )
#'     graph <- as_igraph(result)
#'     lb_close(result)
#'     graph
#'   }, num_threads = 2)
#' }
#'
#' @export
as_igraph <- function(x, ...) {
  UseMethod("as_igraph")
}

#' @export
as_igraph.lb_result <- function(x, ...) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    rlang::abort(
      "Package `igraph` is required for graph conversion.",
      class = "rladybugdb_error_missing_pkg"
    )
  }

  df <- as.data.frame(x)
  .build_igraph(df, x$col_types)
}

#' Convert an lb_result to a tbl_graph (tidygraph)
#'
#' @param x An `lb_result` object.
#' @param ... Unused.
#'
#' @return A `tbl_graph` object, inheriting from `igraph`, whose nodes and
#'   edges represent the query's nodes and relationships, with their properties
#'   available as attributes for tidygraph operations.
#'
#' @export
as_tbl_graph <- function(x, ...) {
  UseMethod("as_tbl_graph")
}

#' @export
as_tbl_graph.lb_result <- function(x, ...) {
  if (!requireNamespace("tidygraph", quietly = TRUE)) {
    rlang::abort(
      "Package `tidygraph` is required for tidygraph conversion.",
      class = "rladybugdb_error_missing_pkg"
    )
  }
  g <- as_igraph(x, ...)
  tidygraph::as_tbl_graph(g)
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Build an igraph from a data frame with node/rel list-columns
#'
#' Heuristic: columns whose values are named lists containing `_id` /
#' `_label` / `properties` are NODE columns; those containing `_src` / `_dst`
#' are REL columns.
#'
#' @return A directed `igraph` object representing the query's nodes and
#'   relationships, with database properties stored as attributes.
#' @keywords internal
.build_igraph <- function(df, column_types = NULL) {
  nodes_list <- list()
  edges_list <- list()

  for (column_index in seq_along(df)) {
    col <- names(df)[[column_index]]
    vals <- df[[col]]
    if (!is.list(vals)) next

    declared_type <- if (length(column_types) >= column_index) {
      column_types[[column_index]]
    } else {
      NA_character_
    }
    if (!length(vals)) {
      if (identical(declared_type, "NODE")) {
        nodes_list[[col]] <- data.frame(`_ID` = character(), check.names = FALSE)
      } else if (declared_type %in% c("REL", "RELATIONSHIP")) {
        edges_list[[col]] <- data.frame(from = character(), to = character(),
                                        check.names = FALSE)
      } else if (identical(declared_type, "RECURSIVE_REL")) {
        nodes_list[[paste0(col, "_nodes")]] <-
          data.frame(`_ID` = character(), check.names = FALSE)
        edges_list[[paste0(col, "_relationships")]] <-
          data.frame(from = character(), to = character(), check.names = FALSE)
      }
      next
    }
    present <- which(!vapply(vals, is.null, logical(1)))
    if (!length(present)) {
      if (identical(declared_type, "NODE")) {
        nodes_list[[col]] <- data.frame(`_ID` = character(), check.names = FALSE)
      } else if (declared_type %in% c("REL", "RELATIONSHIP")) {
        edges_list[[col]] <- data.frame(from = character(), to = character(),
                                        check.names = FALSE)
      }
      next
    }
    first <- vals[[present[[1L]]]]

    if (.is_node_val(first)) {
      nodes_list[[col]] <- .extract_nodes(vals, col)
    } else if (.is_rel_val(first)) {
      edges_list[[col]] <- .extract_rels(vals, col)
    } else if (inherits(first, "lb_path") ||
               all(c("nodes", "relationships") %in% names(first))) {
      path_nodes <- unlist(lapply(vals, function(v) v[["nodes"]]), recursive = FALSE)
      path_rels <- unlist(lapply(vals, function(v) v[["relationships"]]), recursive = FALSE)
      nodes_list[[paste0(col, "_nodes")]] <- .extract_nodes(path_nodes, col)
      edges_list[[paste0(col, "_relationships")]] <- .extract_rels(path_rels, col)
    }
  }

  if (length(edges_list) == 0L && length(nodes_list) == 0L) {
    rlang::abort(
      c(
        "No NODE or REL columns found in the query result.",
        i = "Return node/relationship variables in your MATCH clause, e.g. RETURN a, r, b"
      ),
      class = "rladybugdb_error_no_graph_cols"
    )
  }

  # Combine all node data frames, deduplicate by internal id
  if (length(nodes_list) > 0L) {
    all_nodes <- .rbind_fill(nodes_list)
    all_nodes <- all_nodes[!duplicated(all_nodes[["_ID"]]), , drop = FALSE]
  } else {
    all_nodes <- data.frame(`_ID` = character(0), check.names = FALSE)
  }

  if (length(edges_list) > 0L) {
    all_edges <- .rbind_fill(edges_list)
    if ("_ID" %in% names(all_edges)) {
      all_edges <- all_edges[!duplicated(all_edges[["_ID"]]), , drop = FALSE]
    }
  } else {
    all_edges <- data.frame(from = character(0), to = character(0),
                            check.names = FALSE)
  }

  if (!nrow(all_nodes) && nrow(all_edges)) {
    ids <- unique(c(all_edges$from, all_edges$to))
    all_nodes <- data.frame(`_ID` = ids, check.names = FALSE)
  }

  igraph::graph_from_data_frame(
    d        = all_edges,
    vertices = all_nodes,
    directed = TRUE
  )
}

# LadybugDB C layer returns node lists with uppercase keys: _ID, _LABEL
# and rel lists with: _SRC, _DST, _LABEL, _ID
# Properties are at the top level (not nested under "properties")

.is_node_val <- function(v) {
  is.list(v) && "_ID" %in% names(v) && "_LABEL" %in% names(v) && !"_SRC" %in% names(v)
}

.is_rel_val <- function(v) {
  is.list(v) && all(c("_SRC", "_DST") %in% names(v))
}

.extract_nodes <- function(vals, col_name) {
  vals <- Filter(Negate(is.null), vals)
  if (!length(vals)) return(data.frame(`_ID` = character(), check.names = FALSE))
  rows <- lapply(vals, function(v) {
    # Properties are all keys that don't start with "_"
    prop_keys <- names(v)[!startsWith(names(v), "_")]
    props <- v[prop_keys]
    c(list(`_ID` = .node_id(v), `_LABEL` = v[["_LABEL"]]), props)
  })
  .rows_to_df(rows)
}

.extract_rels <- function(vals, col_name) {
  vals <- Filter(Negate(is.null), vals)
  if (!length(vals)) {
    return(data.frame(from = character(), to = character(), check.names = FALSE))
  }
  rows <- lapply(vals, function(v) {
    prop_keys <- names(v)[!startsWith(names(v), "_")]
    props <- v[prop_keys]
    c(list(from     = .node_id_from_ref(v[["_SRC"]]),
           to       = .node_id_from_ref(v[["_DST"]]),
           `_LABEL` = v[["_LABEL"]],
           `_ID`    = .node_id_from_ref(v[["_ID"]])), props)
  })
  .rows_to_df(rows)
}

.node_id <- function(v) {
  id <- v[["_ID"]]
  if (is.list(id)) {
    paste0(id[["table"]], ":", id[["offset"]])
  } else {
    as.character(id)
  }
}

.node_id_from_ref <- function(ref) {
  if (is.list(ref)) {
    paste0(ref[["table"]], ":", ref[["offset"]])
  } else {
    as.character(ref)
  }
}

# rbind a list of data frames that may have different columns (fills with NA)
.rbind_fill <- function(dfs) {
  dfs <- Filter(is.data.frame, dfs)
  if (!length(dfs)) return(data.frame())
  all_cols <- unique(unlist(lapply(dfs, names)))
  if (!length(all_cols)) return(data.frame())
  dfs_aligned <- lapply(dfs, function(df) {
    missing <- setdiff(all_cols, names(df))
    for (col in missing) df[[col]] <- NA
    df[, all_cols, drop = FALSE]
  })
  do.call(rbind, dfs_aligned)
}

#' @importFrom stats setNames
.rows_to_df <- function(rows) {
  if (!length(rows)) return(data.frame())
  all_keys <- unique(unlist(lapply(rows, names)))
  columns <- setNames(vector("list", length(all_keys)), all_keys)
  for (key in all_keys) {
    values <- lapply(rows, function(row) {
      value <- row[[key]]
      if (is.null(value)) NA else value
    })
    scalar <- vapply(values, function(value) length(value) == 1L && !is.list(value), logical(1))
    columns[[key]] <- if (all(scalar)) unlist(values, recursive = FALSE) else I(values)
  }
  as.data.frame(columns, stringsAsFactors = FALSE, check.names = FALSE)
}
