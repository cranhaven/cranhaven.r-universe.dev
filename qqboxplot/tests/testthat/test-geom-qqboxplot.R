testthat::test_that("geom_qqboxplot draws correctly", {
  testthat::skip("requires manual verification")
  sim_against_theoretical <- simulated_data %>%
    ggplot2::ggplot(ggplot2::aes(factor(group, levels=c("normal, mean=2", "t distribution, df=32", "t distribution, df=16", "t distribution, df=8", "t distribution, df=4")), y=y)) +
    qqboxplot::geom_qqboxplot(notch=TRUE, varwidth = TRUE, reference_dist="norm") +
    ggplot2::xlab("reference: normal distribution") +
    ggplot2::ylab(NULL) +
    ggplot2::guides(color=FALSE) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 23, size = 15), axis.title.y = ggplot2::element_text(size=15),
          axis.title.x = ggplot2::element_text(size=15),
          panel.border = ggplot2::element_blank(), panel.background = ggplot2::element_rect(fill="white"),
          panel.grid = ggplot2::element_line(colour = "grey70"))

  vdiffr::expect_doppelganger("sim against theoretical", sim_against_theoretical)

  sim_against_sim <- simulated_data %>%
    ggplot2::ggplot(ggplot2::aes(factor(group, levels=c("normal, mean=2", "t distribution, df=32", "t distribution, df=16", "t distribution, df=8", "t distribution, df=4")), y=y)) +
    qqboxplot::geom_qqboxplot(notch=TRUE, varwidth = TRUE, compdata=comparison_dataset, numboots = 100) +
    ggplot2::xlab("reference: simulated normal dataset") +
    ggplot2::ylab(NULL) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 23, size = 15), axis.title.y = ggplot2::element_text(size=15),
          axis.title.x = ggplot2::element_text(size=15),
          panel.border = ggplot2::element_blank(), panel.background = ggplot2::element_rect(fill="white"),
          panel.grid = ggplot2::element_line(colour = "grey70"))

  vdiffr::expect_doppelganger("sim against sim", sim_against_sim)
})
