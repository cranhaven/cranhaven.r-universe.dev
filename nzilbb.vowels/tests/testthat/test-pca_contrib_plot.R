test_that("pca_contrib_plot 60%", {
  vdiffr::expect_doppelganger(
    title = "pca_contrib_plot 60%",
    fig = {
      onze_pca <- prcomp(
        onze_intercepts |> dplyr::select(-speaker), scale = TRUE
      )
      pca_contrib_plot(onze_pca, pc_no = 1, cutoff = 60)
    }
  )
})

test_that("pca_contrib_plot no cutoff", {
  vdiffr::expect_doppelganger(
    title = "pca_contrib_plot no cutoff",
    fig = {
      onze_pca <- prcomp(
        onze_intercepts |> dplyr::select(-speaker), scale = TRUE
      )
      pca_contrib_plot(onze_pca, pc_no = 2, cutoff = NULL)
    }
  )
})

