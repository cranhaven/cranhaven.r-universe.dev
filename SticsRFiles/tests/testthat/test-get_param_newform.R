library(SticsRFiles)


stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()

xml_path <- file.path(
  get_examples_path("xml", stics_version = stics_version),
  "param_newform.xml"
)
context("Getting newform param values")

test_that("get option parameters from option node", {
  if (version_num < 10) {
    expect_equal(
      unname(unlist(get_param_xml(xml_path, c(
        "SurfApex(1)", "SeuilMorTalle(1)", "SigmaDisTalle(1)",
        "VitReconsPeupl(1)", "SeuilReconsPeupl(1)",
        "MaxTalle(1)", "SeuilLAIapex(1)",
        "tigefeuilcoupe(1)"
      )))),
      c(5e-6, 0.02, 0.1, 1e-5, 800, 4000, 1, 5)
    )
  } else {
    expect_equal(
      unname(unlist(get_param_xml(xml_path, c(
        "dosimxN", "codetesthumN", "codeNmindec", "rapNmindec",
        "fNmindecmin", "coef_calcul_qres", "coef_calcul_doseN",
        "rapNmindec"
      )))),
      c(40.000, 1.000, 2.000, 0.001, 0.100, 7.530, 16.250, 0.001)
    )
  }
})
