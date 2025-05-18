

stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()


xml_path <- file.path(
  get_examples_path("xml", stics_version = stics_version),
  "usms.xml"
)
context("Getting usms param values")

test_that("getting all param from an usm", {
  expect_equal(
    unname(unlist(get_param_xml(xml_path, c(
      "datedebut", "datefin", "finit", "nomsol",
      "fstation", "fclim1", "fclim2", "culturean",
      "nbplantes", "codesimul"
    ),
    select = "usm", select_value = "SugarCane"
    )$usms.xml)),
    c(
      286, 650, "canne_ini.xml", "solcanne", "climcanj_sta.xml",
      "climcanj.1998", "climcanj.1999", 0, 1, 0
    )
  )


  expect_equal(
    get_param_xml(xml_path, "fplt",
      select = "plante",
      select_value = "1"
    )$usms.xml$fplt[1],
    "proto_sugarcane_plt.xml"
  )


  expect_equal(
    get_param_xml(xml_path, "ftec",
      select = "plante",
      select_value = "1"
    )$usms.xml$ftec[1],
    "canne_tec.xml"
  )


  expect_equal(
    get_param_xml(xml_path, "flai",
      select = "plante",
      select_value = "1"
    )$usms.xml$flai[1],
    "null"
  )


  expect_equal(
    unname(unlist(get_param_xml(xml_path, c(
      "datedebut", "datefin", "finit", "nomsol",
      "fstation", "fclim1", "fclim2", "culturean",
      "nbplantes", "codesimul"
    ),
    select = "usm", select_value = "potato"
    )$usms.xml)),
    c(
      91, 250, "patate_ini.xml", "solpatate", "climpdtj_sta.xml",
      "climpdtj.1997", "climpdtj.1997", 1, 1, 0
    )
  )


  expect_equal(
    get_param_xml(xml_path, "fplt",
      select = "plante",
      select_value = "1"
    )$usms.xml$fplt[2],
    "proto_potato_plt.xml"
  )


  expect_equal(
    get_param_xml(xml_path, "ftec",
      select = "plante",
      select_value = "1"
    )$usms.xml$ftec[2],
    "patate_tec.xml"
  )


  expect_equal(
    get_param_xml(xml_path, "flai",
      select = "plante",
      select_value = "1"
    )$usms.xml$flai[2],
    "null"
  )
})


test_that("Changing output type", {


  set_param_xml(xml_path, param = "fclim1",
                select = "usm",
                select_value = "potato",
                values = 12345.1997,
                overwrite = TRUE)
  fclim1 <- get_param_xml(xml_path, param = "fclim1",
                          select = "usm",
                          select_value = "potato")

  expect_type(object = fclim1$usms.xml$fclim1, type = "double")

  fclim1 <- get_param_xml(xml_path, param = "fclim1",
                          select = "usm",
                          select_value = "potato",
                          to_num = FALSE)

  expect_type(object = fclim1$usms.xml$fclim1, type = "character")

})
