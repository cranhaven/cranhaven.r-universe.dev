test_that(
  "plot_variance_explained snapshot",
  {
    set.seed(10)
    onze_pca <- pca_test(onze_intercepts |> select(-speaker), n=10)
    vdiffr::expect_doppelganger(
      title = "plot_variance_explained",
      fig = {
        plot_variance_explained(onze_pca)
      }
    )
  }
)

test_that(
  "plot_loadings snapshot",
  {
    set.seed(10)
    onze_pca <- pca_test(onze_intercepts |> select(-speaker), n=10)
    vdiffr::expect_doppelganger(
      title = "plot_loadings",
      fig = {
        plot_loadings(onze_pca)
      }
    )
  }
)
