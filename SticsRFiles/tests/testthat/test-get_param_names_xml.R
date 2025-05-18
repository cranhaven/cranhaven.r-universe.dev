library(SticsRFiles)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num(stics_version = stics_version)

xml_dir <- get_examples_path("xml", stics_version = stics_version)
xml_plt <- file.path(xml_dir, "file_plt.xml")
xml_sols <- file.path(xml_dir, "sols.xml")
xml_ini <- file.path(xml_dir, "file_ini.xml")
xml_sta <- file.path(xml_dir, "file_sta.xml")
xml_tec <- file.path(xml_dir, "file_tec.xml")
xml_usms <- file.path(xml_dir, "usms.xml")
xml_gen <- file.path(xml_dir, "param_gen.xml")
xml_new <- file.path(xml_dir, "param_newform.xml")
# ----------------------------------------------------------

context("Getting plt param names")

param_names <- unlist(get_param_names_xml(xml_plt)[[1]])

test_that("option name", {
  expect_true("codetemp" %in% param_names)
  expect_true(all(c("codetemp", "codegdh") %in% param_names))
})

test_that("param name", {
  expect_true("jvcmini" %in% param_names)
  expect_true(all(c("jvcmini", "innsen") %in% param_names))
})

# ----------------------------------------------------------

context("Getting sol param names")

param_names <- unlist(get_param_names_xml(xml_sols, bounds = FALSE)[[1]])

test_that("option name", {
  expect_true("codecailloux" %in% param_names)
  expect_true(all(c("codecailloux", "codemacropor") %in% param_names))
})

test_that("param name", {
  expect_true("argi" %in% param_names)
  expect_true(all(c("argi", "profhum") %in% param_names))
})

test_that("option param name", {
  expect_true("profimper" %in% param_names)
  expect_true(all(c("profimper", "ksol") %in% param_names))
})

test_that("colonne name", {
  expect_true("epc" %in% param_names)
  expect_true(all(c("epc", "cailloux") %in% param_names))
})

# ----------------------------------------------------------
context("Getting ini param names")
param_names <- unlist(get_param_names_xml(xml_ini)[[1]])

test_that("node name", {
  expect_true("nbplantes" %in% param_names)
  expect_true(all(c("nbplantes", "lai0") %in% param_names))
})

no3 <- "NO3init"
if (version_num >= 10) no3 <- "NO3initf"
test_that("parent node name", {
  expect_true("densinitial" %in% param_names)
  expect_true(all(c("densinitial", no3) %in% param_names))
})

# ----------------------------------------------------------
context("Getting station param names")
param_names <- unlist(get_param_names_xml(xml_sta)[[1]])

test_that("option name", {
  expect_true("codeetp" %in% param_names)
  expect_true(all(c("codeetp", "codaltitude") %in% param_names))
})

test_that("param name", {
  expect_true("zr" %in% param_names)
  expect_true(all(c("zr", "latitude") %in% param_names))
})

test_that("option param name", {
  expect_true("altistation" %in% param_names)
  expect_true(all(c("altistation", "altinversion") %in% param_names))
})



# ----------------------------------------------------------
context("Getting tec param names")
param_names <- unlist(get_param_names_xml(xml_tec)[[1]])

test_that("option name", {
  expect_true("codetradtec" %in% param_names)
  expect_true(all(c("codetradtec", "codedecisemis") %in% param_names))
})

test_that("param name", {
  expect_true("iplt0" %in% param_names)
  expect_true(all(c("iplt0", "densitesem") %in% param_names))
})

test_that("option param name", {
  expect_true("interrang" %in% param_names)
  expect_true(all(c("interrang", "ilev") %in% param_names))
})


test_that("colonne name", {
  expect_true("coderes" %in% param_names)
  expect_true(all(c("coderes", "julapI_or_sum_upvt") %in% param_names))
})


# ----------------------------------------------------------
context("Getting usms param names")
param_names <- unlist(get_param_names_xml(xml_usms)[[1]])


test_that("node name", {
  expect_true("datedebut" %in% param_names)
  expect_true(all(c("datedebut", "fplt") %in% param_names))
})


# ----------------------------------------------------------
context("Getting param_gen param names")
param_names <- unlist(get_param_names_xml(xml_gen, bounds = FALSE)[[1]])

test_that("option name", {
  expect_true("codeh2oact" %in% param_names)
  expect_true(all(c("codeh2oact", "codeinitprec") %in% param_names))
})

test_that("param name", {
  expect_true("proprac" %in% param_names)
  expect_true(all(c("proprac", "daseuilbas") %in% param_names))
})

test_that("option param name", {
  expect_true("vnitmax" %in% param_names)
  expect_true(all(c("vnitmax", "tnitopt") %in% param_names))
})


# ----------------------------------------------------------
context("Getting param_new param names")
param_names <- unlist(get_param_names_xml(xml_new)[[1]])

if (version_num < 10) {
  test_that("option name", {
    expect_true("codetempfauche" %in% param_names)
    expect_true(all(c("codetempfauche", "codecalferti") %in% param_names))
  })

  test_that("param name", {
    expect_true("nbj_pr_apres_semis" %in% param_names)
    expect_true(all(c("nbj_pr_apres_semis", "codetranspitalle")
    %in% param_names))
  })

  test_that("option param name", {
    expect_true("SigmaDisTalle(1)" %in% param_names)
    expect_true(all(c("SigmaDisTalle(1)", "ratiolN") %in% param_names))
  })
} else {
  test_that("option name", {
    expect_true("codeNmindec" %in% param_names)
    expect_true(all(c("codeNmindec", "codecalferti") %in% param_names))
  })


  test_that("option param name", {
    expect_true("rapNmindec" %in% param_names)
    expect_true(all(c("rapNmindec", "ratiolN") %in% param_names))
  })
}
