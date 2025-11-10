#' Apply Procrustes rotation to PCA loadings and scores.
#'
#' It is sometimes convenient to rotate principal components to align PCA
#' applied to one dataset with PCA applied to another. This function
#' allows for Procrustes rotation of Principal Components, without scaling.
#' That is, we rotate and/or flip loadings or scores so that the PCA analyses
#' to be rotated most closely matches the loadings (or scores) from the target
#' PCA analysis.
#'
#' **NB: rotated components are not principal components.** They no longer explain
#' maximal variance. Rotated components should not be referred to as 'principal
#' components'. The simplest approach is just to call them 'components' after
#' describing the rotation. This function modifies objects of the class 'prcomp'
#' and 'princomp', adding an additional 'note' which collects all the rotations
#' which have been applied. This allows any plotting function which works with
#' the outputs of `prcomp()` or `princomp()` to work. This may result in plots
#' which incorrectly identify rotated components as principal components. Be
#' careful not to include any such plot in a research output.
#'
#' @param to_rotate an object of class `princomp` or `prcomp`.
#' @param target an object of class `princomp` or `prcomp`
#' @param max_pcs an integer. Rotation will be applied from PC1 up to `max_pcs`.
#' @param rotate a string, either "loadings" or "scores", to identify whether
#'   the loadings of `to_rotate` should be aligned with `target` or the scores
#'   (default: "loadings")
#' @param rotation_variables a string, names of variables to be used in the
#'   rotation. Applied to rotation of loadings when two datasets have only
#'   partial overlap of variables. (default: "all", which uses all variables).
#'
#' @importFrom vegan procrustes
#' @importFrom purrr map_dbl
#'
#' @returns an object matching the class of `to_rotate`.
#' @export
#'
#' @examples
#'   # PCA on a subset of ONZE speakers
#'   onze_pca <- prcomp(
#'     onze_intercepts |> dplyr::select(-speaker),
#'     scale = TRUE
#'    )
#'
#'    # PCA on all ONZE speakers
#'    onze_full <- prcomp(
#'     onze_intercepts_full |> dplyr::select(-speaker),
#'     scale = TRUE
#'    )
#'
#'    # rotate subset to match loadings of `onze_full`
#'    rotated_pca <- onze_pca |>
#'      pca_rotate_procrustes(
#'        onze_full, max_pcs = 5
#'      )
pca_rotate_procrustes <- function(
    to_rotate, target, max_pcs, rotate="loadings", rotation_variables = "all"
) {
  stopifnot(
      # strictly speaking this is unnecessarily strict - they shouldn't need
      # to match
      "Target and PCA to rotate must be produced by the same PCA,
      function,i.e. both by `prcomp` or both by `princomp()`." =
        class(to_rotate) == class(target),
      "rotate must be either 'loadings' or 'scores'." =
        rotate %in% c("loadings", "scores")
  )

  # Determine if pca is result of prcomp() or princomp().
  if (inherits(to_rotate, "prcomp")) {
    scores_var <- "x"
    loadings_var <- "rotation"
  } else if (inherits(to_rotate, "princomp")) {
    scores_var <- "scores"
    loadings_var <- "loadings"
  }

  # check landmarks are the same.
  if (
    any(
      rownames(target[[loadings_var]]) != rownames(to_rotate[[loadings_var]])
    )
  ) {
    warning(
      paste(
        "Variable names or variable order differs between target",
        "and matrix to rotate."
      )
    )
  }

  # Work out how many PCs to include, if max PCs is greater than the number
  # of cols in the matrix to be rotated or the target matrices, pick the smallest
  # of these.
  max_pc = min(
    ncol(to_rotate[[loadings_var]]),
    ncol(target[[loadings_var]]),
    max_pcs
  )

  if (all(rotation_variables == "all")) {
    rotvar_target <- rownames(target[[loadings_var]])
    rotvar_rotate <- rownames(to_rotate[[loadings_var]])
  } else {
    rotvar_target <- rotation_variables
    rotvar_rotate <- rotation_variables
  }

  if (rotate == "loadings") {
    proc <- procrustes(
      target[[loadings_var]][rotvar_target, 1:max_pc],
      to_rotate[[loadings_var]][rotvar_rotate, 1:max_pc],
      scale = FALSE, scores = "sites"
    )
    rot_loadings <- to_rotate[[loadings_var]][, 1:max_pc] %*%
      proc$rotation
    rot_scores <- to_rotate[[scores_var]][, 1:max_pc] %*%
      proc$rotation
  } else if (rotate == "scores") {
    proc <- procrustes(
      target[[scores_var]][, 1:max_pc], to_rotate[[scores_var]][, 1:max_pc],
      scale = FALSE, scores = "sites"
    )
    scores_transform <- matrix(
      rep(proc$xmean, times = nrow(to_rotate[[scores_var]])),
      nrow = nrow(to_rotate[[scores_var]]), byrow=TRUE
    )
    rot_scores <- proc$Yrot + scores_transform
    rot_loadings <- to_rotate[[loadings_var]][, 1:max_pc] %*%
      proc$rotation
  }

  # update scores
  to_rotate[[loadings_var]][, 1:max_pc] <- rot_loadings
  to_rotate[[scores_var]][, 1:max_pc] <- rot_scores

  # Change variance explained
  to_rotate[['sdev']] <- map_dbl(
    1:ncol(to_rotate[[scores_var]]),
    \(x) stats::sd(to_rotate[[scores_var]][,x])
  )

  if (inherits(to_rotate, "princomp")) {
    names(to_rotate[['sdev']]) = paste0("Comp.", 1:ncol(to_rotate[[scores_var]]))
  }

  # Add note
  # Add a note to indicate rotation has happened.
  if ("note" %in% names(to_rotate)) {
    to_rotate[["note"]] <- append(
      to_rotate[["note"]],
      paste0(
        "Procrustes rotation applied to PCs 1 through ", max_pc, " with ",
        deparse(substitute(target)), " as target."
      )
    )
  } else {
    to_rotate[["note"]] <- list(
      paste0(
        "Procrustes rotation applied to PCs 1 through ", max_pc, " with ",
        deparse(substitute(target)), " as target."
      )
    )
  }

  to_rotate
}
