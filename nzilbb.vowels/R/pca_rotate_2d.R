#' Manually rotate two PCs around the origin
#'
#' It is sometimes convenient to rotate principal components to most closely
#' align with a sensible interpretation in terms of the original variables or
#' to compare the results of PCA applied to two distinct datasets. This function
#' allows for manual 2D rotations of principal components.
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
#' @param pca_obj The result of a call to `prcomp()` or `princomp()`. **NB** It
#'  does not make sense to apply this function to the output of `pca_test`.
#' @param angle A number indicating the number of degrees to rotate around the
#'   origin clockwise. Negative values will rotated counterclockwise.
#' @param pcs A two-element vector identifying the two PCs to rotate.
#'
#' @importFrom purrr map_dbl
#'
#' @returns An object matching the class of `pca_obj` with loadings, scores, and
#'   variance explained by each component modified.
#' @export
#'
#' @examples
#'   pca_obj <- prcomp(onze_intercepts |> dplyr::select(-speaker), scale=TRUE)
#'
#'   # Rotate PCs 3 and 6 by 10 degrees.
#'   rotated_pca <- pca_rotate_2d(pca_obj, 10, pcs = c(3,6))
pca_rotate_2d <- function(pca_obj, angle, pcs = c(1, 2)) {

  # Convert from degrees to radians and swap to clockwise
  theta <- - angle * (pi / 180)

  rot_mat <- matrix(
    c(
      cos(theta), -sin(theta),
      sin(theta), cos(theta)
    ),
    nrow = 2
  )

  # Determine if pca is result of prcomp or princomp.
  if (inherits(pca_obj, "prcomp")) {
    scores_var <- "x"
    loadings_var <- "rotation"
  } else if (inherits(pca_obj, "princomp")) {
    scores_var <- "scores"
    loadings_var <- "loadings"
  }

  # rotate scores
  pca_obj[[scores_var]][, pcs] <- pca_obj[[scores_var]][, pcs] %*%
    rot_mat

  # rotate loadings
  pca_obj[[loadings_var]][, pcs] <- pca_obj[[loadings_var]][, pcs] %*%
    rot_mat

  # Change variance explained
  pca_obj[['sdev']] <- map_dbl(
    1:ncol(pca_obj[[scores_var]]),
    \(x) stats::sd(pca_obj[[scores_var]][,x])
  )

  if (inherits(pca_obj, "princomp")) {
    names(pca_obj[['sdev']]) = paste0("Comp.", 1:ncol(pca_obj[[scores_var]]))
  }

  # Add a note to indicate rotation has happened.
  if ("note" %in% names(pca_obj)) {
    pca_obj[["note"]] <- append(
      pca_obj[["note"]],
      paste0(
        "PC", pcs[1], " and PC", pcs[2], " rotated by ", angle,
        " degrees (clockwise)."
      )
    )
  } else {
    pca_obj[["note"]] <- list(paste0(
      "PC", pcs[1], " and PC", pcs[2], " rotated by ",
      angle, " degrees (clockwise)."
    ))
  }

  pca_obj
}

