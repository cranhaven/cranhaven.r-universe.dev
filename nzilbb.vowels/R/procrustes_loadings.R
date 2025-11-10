#' Generate distribution of (index) loadings using the bootstrap and Procrustes rotation.
#'
#' `r lifecycle::badge('experimental')` Generate distribution of loadings or
#' signed index loadings for Principal Components. These are used in order to
#' estimate confidence intervals for loadings and, if signed index loadings
#' are used, also a null distribution for tests of statistical significance.
#' Plot the results using [`plot_procrustes_loadings()`].
#'
#' @param pca_data data fed to the `prcomp` function.
#' @param max_pcs maximum number of PCs to rotate.
#' @param index whether to use signed index loadings rather than loadings (default: TRUE)
#' @param n the number of bootstrapped and permuted samples.
#' @param scale whether the variables in `pca_data` should be scaled before PCA (default: TRUE)
#' @importFrom rsample bootstraps
#' @importFrom purrr map_lgl map pluck list_rbind
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate select
#'
#' @returns a tibble, with columns:
#' * `source` either "Sampling", "Null" or "Original", identifying where the
#' loadings comes from. "Original" identifies loadings from the full dataset,
#' "Sampling" identifies loadings from the bootstrapped samples, "Null" identifes
#' loadings from permuted versions of the data.
#' * `id` identifies which iteration of either permutation or bootstrapping the
#' loading comes from.
#' * `variable` indicates the variable corresponding to the loading.
#' * a column containing the loading for each `PC` up to `max_pcs`.
#' @export
#'
#' @examples
#'   proc_loadings <- procrustes_loadings(
#'     pca_data = onze_intercepts |> dplyr::select(-speaker),
#'     max_pcs = 3,
#'     index = TRUE,
#'     n = 10, # set this to at least 100 in actual use.
#'     scale = TRUE
#'    )
#'
procrustes_loadings <- function(
    pca_data, max_pcs, index = TRUE, n = 500,  scale = TRUE
  ) {

  stopifnot(
    "All columns of pca_data must be numeric or integers." =
      all(map_lgl(pca_data, is.numeric)),
    "`max_pcs` must be numeric" = is.numeric(max_pcs)
  )

  original_pca <- stats::prcomp(pca_data, scale = scale)

  if (index == TRUE) {
    original_pca <- original_pca |>
      indexify(signed=TRUE)
  }

  data_boots <- bootstraps(pca_data, times = n)

  # Bootstrap, apply PCA, and rotate to match original.
  bootstrapped_pca <- map(
    data_boots$splits,
    ~ {
        boot_pca <- stats::prcomp(
          as_tibble(.x),
          scale = scale
        )

        if (index == TRUE) {
          boot_pca <- boot_pca |>
            indexify(signed=TRUE)
        }

        boot_pca |>
          pca_rotate_procrustes(
            target = original_pca,
            max_pcs = max_pcs,
            rotate = "loadings",
            rotation_variables = "all"
          )
      }
  )

  loadings <- map(
      1:n,
      ~ bootstrapped_pca[[.x]] |>
        pluck("rotation") |>
        as_tibble(rownames = "variable") |>
        mutate(source = "Sampling", id = .x)
    ) |>
    append(
      list(original_pca |>
       pluck("rotation") |>
       as_tibble(rownames = "variable") |>
       mutate(source = "Original", id = 1))
    ) |>
    # add null distribution *if* index loadings are being used.
    (\(x) {
      if (index == TRUE) {
        x |>
          append(
            map(
              1:n,
              ~ stats::prcomp(
                pca_data %>%
                  mutate(
                    across(
                      .cols = everything(),
                      .fns = ~ base::sample(.x, length(.x), replace = FALSE)
                    )
                  ),
                scale = scale
              ) |>
              indexify(signed=TRUE) |>
              pca_rotate_procrustes(
                target = original_pca,
                max_pcs = max_pcs,
                rotate = "loadings",
                rotation_variables = "all"
              ) |>
              pluck("rotation") |>
              as_tibble(rownames = "variable") |>
              mutate(source = "Null", id = .x)
            )
          )
      } else {
        x
      }
    })() |>
    list_rbind() |>
    select(.data$source, .data$id, .data$variable, any_of(glue("PC{1:max_pcs}")))

  loadings
}

# helper to generate index loadings.
indexify <- function(pca_obj, signed=TRUE) {
  if (signed == TRUE) {
    pca_obj[['rotation']] <- t(
      # index loadings are squared eigenvalues * squared loadings
      sign(t(pca_obj[['rotation']])) * t(pca_obj[['rotation']])^2 *
        pca_obj[['sdev']]^4 # eigenvalue^2 = sd^2^2 = sd^4
    )
  } else {
    pca_obj[['rotation']] <- t(
      # index loadings are squared eigenvalues * squared loadings
      t(pca_obj[['rotation']])^2 * pca_obj[['sdev']]^4 # eigenvalue^2 = sd^2^2 = sd^4
    )
  }
  pca_obj
}
