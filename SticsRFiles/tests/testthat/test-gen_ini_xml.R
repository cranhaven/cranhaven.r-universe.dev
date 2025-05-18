library(SticsRFiles)
library(dplyr)

stics_version <- get_stics_versions_compat()$latest_version

if (get_version_num(stics_version = stics_version)
>= 10) {
  stics_version <- "V10.0"
}
parnames <- list()
parnames$hinit$`V10.0` <- "Hinitf"
parnames$hinit$`V9.2` <- "hinit"
parnames$no3init$`V10.0` <- "NO3initf"
parnames$no3init$`V9.2` <- "NO3hinit"
parnames$nh4init$`V10.0` <- "NH4initf"
parnames$nh4init$`V9.2` <- "NH4hinit"


xl_path <- file.path(
  get_examples_path("xl", stics_version = stics_version),
  "inputs_stics_example.xlsx"
)

ini_param <- read_params_table(file = xl_path, sheet_name = "Ini")

out_dir <- file.path(tempdir(), "gen_xml")
if (!dir.exists(out_dir)) dir.create(out_dir)

gen_ini_xml(
  param_df = ini_param[1, ], out_dir = out_dir,
  stics_version = stics_version
)

ini_xml <- file.path(out_dir, ini_param[1, ]$Ini_name)


# For plante 1
xl_plt1_values <- select(ini_param[1, ],
                         ends_with("Crop1"),
                         -starts_with("code"))
xl_params <- gsub(
  pattern = "(.*)_(.*)", x = names(xl_plt1_values),
  replacement = "\\1"
)

xl_params <- gsub(pattern = "(.*)(\\_[0-9]*$)",
                  x = xl_params,
                  replacement = "\\1")

xl_plt1_values <- select(xl_plt1_values, starts_with(xl_params))
xml_plt1_values <- get_param_xml(
  file = ini_xml, select = "plante",
  select_value = 1
)[[1]]
idx <- names(xml_plt1_values) %in% xl_params
xml_plt1_values <- unlist(xml_plt1_values[idx])

# For plant 2
xml_plt2_values <- unlist(get_param_xml(
  file = ini_xml, select = "plante",
  select_value = 2
)[[1]])

xl_plt2_values <- select(ini_param[1, ],
                         ends_with("Crop2"),
                         -starts_with("code"))
xl_params <- gsub(
  pattern = "(.*)_(.*)", x = names(xl_plt1_values),
  replacement = "\\1"
)

xl_params <- gsub(pattern = "(.*)(\\_[0-9]*$)",
                  x = xl_params,
                  replacement = "\\1")

xl_plt2_values <- select(xl_plt2_values, starts_with(xl_params))
xml_plt2_values <- get_param_xml(
  file = ini_xml, select = "plante",
  select_value = 2
)[[1]]
idx <- names(xml_plt2_values) %in% xl_params
xml_plt2_values <- unlist(xml_plt2_values[idx])

# for sol
# hinit
hinit_name <- parnames$hinit[[stics_version]]
xml_hinit_values <- unlist(get_param_xml(
  file = ini_xml,
  param = hinit_name
)[[1]])

xl_hinit_values <- select(ini_param[1, ], starts_with(hinit_name))

# NO3init
no3init_name <- parnames$no3init[[stics_version]]
xml_no3init_values <- unlist(get_param_xml(
  file = ini_xml,
  param = no3init_name
)[[1]])
xl_no3init_values <- select(ini_param[1, ], starts_with(no3init_name))

# NH4init
nh4init_name <- parnames$nh4init[[stics_version]]
xml_nh4init_values <- unlist(get_param_xml(
  file = ini_xml,
  param = nh4init_name
)[[1]])
xl_nh4init_values <- select(ini_param[1, ], starts_with(nh4init_name))

context("Comparing table values vs xml ini file values")

test_that("Testing values for plant parameters", {
  expect_true(all(xml_plt1_values == xl_plt1_values))
  expect_equal(
    all(xml_plt2_values %in% c("", "-999")),
    ncol(xl_plt2_values) == 0
  )
})

test_that("Testing values for soil parameters", {
  expect_true(all(xml_hinit_values == xl_hinit_values))
  expect_true(all(xml_no3init_values == xl_no3init_values))
  expect_true(all(xml_nh4init_values == xl_nh4init_values))
})
