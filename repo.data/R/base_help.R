
#' Help pages without links
#'
#' Help pages without links to other help pages.
#' This makes harder to navigate to related help pages.
#' @returns A data.frame with two columns: Package and Source.
#' `NA` if not able to collect the data from CRAN.
#' @export
#' @family functions related to BASE help pages
#' @examples
#' \donttest{
#' bhnl <- base_help_pages_not_linked()
#' head(bhnl)
#' }
base_help_pages_not_linked <- function() {
    bal <- base_alias()
    bl <- base_links()
    if (is_not_data(bl)) {
        return(NA)
    }
    bl2 <- split_anchor(bl)
    rbl <- targets2files(bl2, bal)

    alias_cols <- c("Package", "Source")
    links_cols <- c("from_pkg", "from_Rd")
    ubal <- unique(bal[, alias_cols])

    links_cols2 <- c("to_pkg", "to_Rd")
    pages <- merge(ubal, unique(rbl[, c(links_cols2, links_cols)]),
                   by.x = alias_cols, by.y = links_cols2,
                   all.x = TRUE, all.y = FALSE, sort = FALSE)
    p <- pages[is.na(pages$from_pkg), alias_cols, drop = FALSE]
    p <- sort_by(p, p[, c("Package", "Source")])
    rownames(p) <- NULL
    p
}

#' Help pages not linked from base R
#'
#' Help pages without links from other help pages.
#' This makes harder to find them.
#' @returns A data.frame with two columns: Package and Source
#' @export
#' @family functions related to BASE help pages
#' @examples
#' \donttest{
#' bhwl <- base_help_pages_wo_links()
#' head(bhwl)
#' }
base_help_pages_wo_links <- function() {

    bal <- base_alias()
    bl <- base_links()
    if (is_not_data(bl)) {
        return(NA)
    }
    bl2 <- split_anchor(bl)
    rbl <- targets2files(bl2, bal)
    alias_cols <- c("Package", "Source")
    links_cols <- c("from_pkg", "from_Rd")
    ubal <- unique(bal[, alias_cols])

    links_cols2 <- c("to_pkg", "to_Rd")
    pages2 <- merge(ubal, unique(rbl[, c(links_cols, links_cols2)]),
                    by.x = alias_cols, by.y = links_cols,
                    all.x = TRUE, all.y = FALSE, sort = FALSE)
    p <- pages2[is.na(pages2$from_pkg), alias_cols, drop = FALSE]
    p <- sort_by(p, p[, c("Package", "Source")])
    rownames(p) <- NULL
    p
}

#' Help pages with cliques
#'
#' Some help pages have links to and from but they are closed networks.
#'
#' Requires igraph
#' @returns Return a data.frame of help pages not connected to the network of help pages.
#' `NA` if not able to collect the data from CRAN.
#' @family functions related to BASE help pages
#' @export
#' @examples
#' \donttest{
#' if (requireNamespace("igraph", quietly = TRUE)) {
#'     base_help_cliques()
#' }
#' }
base_help_cliques <- function() {
    if (!check_installed("igraph")) {
        stop("This function requires igraph to find closed networks.")
    }
    bal <- base_alias()
    bl <- base_links()
    if (is_not_data(bl)) {
        return(NA)
    }
    bl2 <- split_anchor(bl)
    rbl <- targets2files(bl2, bal)
    df_links <- data.frame(from = paste0(rbl$from_pkg, ":", rbl$from_Rd),
               to = paste0(rbl$to_pkg, ":", rbl$to_Rd))
    df_links <- unique(df_links)

    graph <- igraph::graph_from_edgelist(as.matrix(df_links))

    graph_decomposed <- igraph::decompose(graph)
    lengths_graph <- lengths(graph_decomposed)
    isolated_help <- sapply(graph_decomposed[-which.max(lengths_graph)], igraph::vertex_attr)

    l <- strsplit(funlist(isolated_help), ":", fixed = TRUE)
    df <- as.data.frame(t(list2DF(l)))
    colnames(df) <- c("from_pkg", "from_Rd")
    lengths_graph2 <- lengths_graph[-which.max(lengths_graph)]
    df$clique <- rep(seq_len(length(lengths_graph2)), times = lengths_graph2)
    m <- merge(df, unique(rbl[, -4]), all.x = TRUE,
               by = c("from_pkg", "from_Rd"), sort = FALSE)
    msorted <- sort_by(m, m[, c("clique", "from_pkg", "from_Rd")])
    rownames(msorted) <- NULL
    msorted
}
