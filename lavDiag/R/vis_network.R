#' Build a visNetwork widget with consistent styling
#' @param nodes Data frame with at least: id, label; optional: x,y,shape,font.size,...
#' @param edges Data frame with at least: from, to; optional: arrows,width,label,title,...
#' @param main  Character title shown above the network.
#' @return htmlwidget (visNetwork)
#' @noRd
#' @keywords internal
.vis_network <- function(nodes, edges, main) {
  has_xy <- all(c("x","y") %in% names(nodes)) &&
    all(is.finite(nodes$x)) && all(is.finite(nodes$y))

  g <- visNetwork::visNetwork(nodes = nodes, edges = edges, main = main) |>
    visNetwork::visPhysics(enabled = FALSE, stabilization = FALSE) |>
    visNetwork::visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE) |>
    visNetwork::visNodes(
      physics = FALSE, borderWidth = 1.5, chosen = FALSE, labelHighlightBold = FALSE,
      shapeProperties = list(borderRadius = 0),
      font = list(align = "center"),
      color = list(
        background = "#FFFFFF", border = "#000000",
        highlight = list(background = "#FFFFFF", border = "#000000"),
        hover     = list(background = "#FFFFFF", border = "#000000")
      )
    ) |>
    visNetwork::visEdges(
      smooth = list(type = "diagonalCross"),
      chosen = FALSE, labelHighlightBold = FALSE,
      color = list(color = "#000000", highlight = "#2B7CE9"),
      arrowStrikethrough = FALSE,
      font = list(background = "#FFFFFF", strokeWidth = 10, mono = TRUE)
    ) |>
    visNetwork::visOptions(highlightNearest = list(enabled = TRUE, degree = 0))

  if (!has_xy) {
    # fallback only when x/y are missing
    g <- g |> visNetwork::visIgraphLayout()
  }
  g
}
