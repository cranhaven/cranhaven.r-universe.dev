
#' @title Plot grPipe Nodes
#'
#' @description
#' save grPipe nodes in \strong{pngfile} path.
#'
#' @param nodes data.frame
#' @param pngfile character
#' @param title character
#' @param plot logical
#' @param showGrid logical
#' @param colSpace numeric
#' @param rowSpace numeric
#'
#' @return No return value.
#'
#' @author Daniel Gaspar Gonçalves
#'
#' @examples
#' nodes = grPipe.create(2,5)
#' nodes = grPipe.node(nodes, "A1",  "A2",  "input")
#' nodes = grPipe.node(nodes, "A2",  "B2",  "step 1")
#' nodes = grPipe.node(nodes, "B2",  "B3",  "step 2")
#' nodes = grPipe.node(nodes, "B3",  "B4",  "step 3")
#' nodes = grPipe.node(nodes, "B4",  "A4",  "step 4")
#' nodes = grPipe.node(nodes, "A4",  "A5",  "step 5")
#' nodes = grPipe.node(nodes, "A5",  NA,  "output")
#' grPipe.plot(nodes, tempfile(), showGrid = TRUE)
#' grPipe.plot(nodes, tempfile(), showGrid = FALSE)
#'
#' @export

grPipe.plot = function(nodes, pngfile, title="", plot=TRUE, showGrid=FALSE, colSpace=0.5, rowSpace=0.5) {
  if (showGrid) {
    gridStyle = "filled"
  } else {
    gridStyle = "invis"
  }

  # rows
  rows = nodes %>%
    select(id) %>%
    unlist %>%
    as.character
  rows = gsub(pattern = "([A-Z])[0-9]*", replacement = "\\1", x = rows) %>%
    unique %>%
    max
  rows = grep(pattern = rows, x = LETTERS)
  rows = LETTERS[1:rows]

  # cols
  cols = nodes %>%
    select(id) %>%
    unlist %>%
    as.character
  cols = gsub(pattern = "[A-Z]([0-9]*)", replacement = "\\1", x = cols) %>% as.integer
  cols = 1:max(cols)

  # rank cols
  rank_col = c()
  for (i in cols) {
    rank_col = c(rank_col, paste0(rows, i, collapse = " -> "))
  }
  rank_col = paste0(rank_col, collapse = "\n")

  # rank rows
  rank_row = c()
  for (i in rows) {
    rank_row = c(rank_row, paste0("rank=same {", paste0(i, cols, collapse = " -> "), "}"))
  }
  rank_row = paste0(rank_row, collapse = "\n")

  # nodes label
  nodes_label = c()
  for (i in 1:nrow(nodes)) {
    if (!is.na(nodes[i, "text"])) {
      nodes_label = c(
        nodes_label,
        paste0(nodes[i,"id"], " [label=\"", nodes[i, "text"], "\", style=filled]")
      )
    }
  }
  nodes_label = paste0(nodes_label, collapse = "\n")

  # node arrows
  node_arrow = c()
  for (i in 1:nrow(nodes)) {
    if (!is.na(nodes[i, "id_next"])) {
      node_arrow = c(
        node_arrow,
        paste0(nodes[i, "id"], " -> ", nodes[i, "id_next"])
      )
    }
  }
  node_arrow = paste0(node_arrow, collapse = "\n")

  # save png
  grViz(paste0('
    digraph {
        fontname="Verdana"
        graph [splines=ortho, nodesep="', colSpace, '", ranksep="', rowSpace, '"]
        node [shape=plaintext, fontname="Verdana", style=', gridStyle, ']
        //node [shape=box, fontname="Verdana", style=filled]
        edge [fontname="Verdana"]
        layout=dot
        label="', title, '"
        labelloc = "t"

        ', nodes_label, '

        // arbitrary path on rigid grid
        ', node_arrow, '

        edge [weight=1000 style=dashed color=dimgrey]

        // uncomment to hide the grid
        edge [style=invis]

        ', rank_col, '

        ', rank_row, '

    }'), width = 2400) %>%
    export_svg %>%
    charToRaw %>%
    rsvg_png(pngfile, width = 2400)

  # plot image on notebook
  if (plot) {
    readPNG(pngfile) %>%
      grid.raster
  }
}
