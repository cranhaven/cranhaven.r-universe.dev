test_that(
  "plot_correlation_counts snapshot",
  {
    skip_if(getRversion() < "4.4")
    set.seed(10)
    onze_cor <- correlation_test(onze_intercepts |> select(-speaker), n=10)
    vdiffr::expect_doppelganger(
      title = "plot_correlation_counts",
      fig = {
        plot_correlation_counts(onze_cor)
      }
    )
  }
)

test_that(
  "plot_correlation_magnitudes snapshot",
  {
    skip_if(getRversion() < "4.4")
    set.seed(10)
    onze_cor <- correlation_test(onze_intercepts |> select(-speaker), n=10)
    vdiffr::expect_doppelganger(
      title = "plot_correlation_mag",
      fig = {
        plot_correlation_magnitudes(onze_cor)
      }
    )
  }
)
