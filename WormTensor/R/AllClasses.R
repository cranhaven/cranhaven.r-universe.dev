#' S4 class used by as_worm_tensor.R, worm_membership.R, worm_clustering.R,
#' worm_evaluate.R, worm_visualize.R
#' @slot dist_matrices list
#' @slot n_animals numeric
#' @slot union_cellnames character
#' @slot n_union_cells numeric
#' @slot membership_tensor Tensor
#' @slot k numeric
#' @slot clustering_algorithm character
#' @slot clustering numeric
#' @slot weight numeric
#' @slot factor matrix
#' @slot consensus matrix
#' @slot eval list
#' @slot dimension_reduction_algorithm character
#' @import rTensor
#' @export
setClass("WormTensor",
    slots = c(
        # Filled by as_worm_tensor()
        dist_matrices = "list",
        n_animals = "numeric",
        union_cellnames = "character",
        n_union_cells = "numeric",
        # Filled by worm_membership()
        membership_tensor = "Tensor",
        k = "numeric",
        # Filled by worm_clustering()
        clustering_algorithm = "character",
        clustering = "numeric",
        weight = "numeric",
        factor = "matrix",
        consensus = "matrix",
        # Filled by worm_evaluate()
        eval = "list",
        # Filled by worm_visualize()
        dimension_reduction_algorithm = "character"))
