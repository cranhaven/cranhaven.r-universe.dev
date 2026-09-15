utils::globalVariables(c(
  ".estimate", ".metric", "HC", "Sample", "child_label", "data_type",
  "dist", "id", "kids", "level", "na.omit", "node_id", "p.value",
  "parent", "parent_label", "prob_text", "recalculated_nodesize",
  "setNames", "splitvar", "x", "x_child", "x_parent", "y", "y_child",
  "y_hat", "y_parent"
))

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom magrittr %>%
#' @importFrom ComplexHeatmap %v% Heatmap rowAnnotation anno_barplot
#'   anno_text anno_simple draw Legend packLegend
#' @importFrom circlize colorRamp2
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @importFrom ggparty ggparty
#' @importFrom grid unit gpar grid.newpage pushViewport viewport
#'   grid.segments grid.text grid.rect grid.draw upViewport seekViewport
#'   textGrob convertWidth convertHeight grobWidth grobHeight grid.grabExpr
#' @importFrom dplyr filter select mutate arrange pull left_join bind_rows
#'   count distinct rename transmute across all_of if_else rowwise ungroup
#'   c_across add_count add_row desc as_tibble n row_number mutate_at vars
#' @importFrom stringr str_detect str_extract
#' @importFrom stats as.formula cor sd as.dist order.dendrogram as.dendrogram
#'   dist na.omit setNames
#' @importFrom utils combn capture.output modifyList
#' @importFrom grDevices gray png pdf svg dev.off
#' @importFrom rpart rpart
#' @importFrom partykit ctree cforest gettree as.party varimp
#' @importFrom C50 C5.0 C5imp
#' @importFrom caret train confusionMatrix
#' @importFrom seriation seriate get_order list_seriation_methods
#' @importFrom yardstick metric_set accuracy bal_accuracy kap precision
#'   recall specificity rsq mae rmse ccc
#' @importFrom rlang sym
## usethis namespace: end
NULL
