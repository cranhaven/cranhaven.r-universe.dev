library(SticsRFiles)
library(dplyr)


stics_version <- get_stics_versions_compat()$latest_version


xl_path <- file.path(
  get_examples_path("xl", stics_version = stics_version),
  "inputs_stics_example.xlsx"
)

tec_param <- read_params_table(file = xl_path, sheet_name = "Tec")

out_dir <- file.path(tempdir(), "gen_xml")
if (!dir.exists(out_dir)) dir.create(out_dir)

gen_tec_xml(
  param_df = tec_param[4, ], out_dir = out_dir,
  stics_version = stics_version
)

tec_xml <- file.path(out_dir, tec_param[4, ]$Tec_name)


# For residues
xml_res_vec <- unlist(get_param_xml(
  file = tec_xml, select = "formalisme",
  select_value = "supply of organic residus"
)[[1]])

xl_res_vec <- dplyr::select(tec_param[4, ], starts_with(names(xml_res_vec)))

# For irrigation
xml_irr_values <- get_param_xml(
  file = tec_xml, select = "formalisme",
  select_value = "irrigation"
)[[1]]

# renaming param according to table
names(xml_irr_values)[names(xml_irr_values) %in%
                        c("julapI_or_sum_upvt", "amount")] <- c("julapI", "doseI")


# select columns with no NA values
xl_irr_values <- dplyr::select(
  tec_param[4, ],
  starts_with(sort(names(xml_irr_values)))
) %>%
  dplyr::select(where(function(x) !is.na(x)) &
                  where(function(x) {
                    c <- x != "NA"
                    if (is.na(c)) c <- TRUE
                    return(c)

                  }
                  ))


#
xl_names <- sort(unique(gsub(pattern = "(.*)(\\_[0-9]*)",
                             x = colnames(xl_irr_values), replacement = "\\1")))

common_names <- sort(intersect(names(xml_irr_values), xl_names))

xml_irr_values <- unlist(xml_irr_values[common_names],
                         use.names = FALSE
)

xl_irr_values <- dplyr::select(
  tec_param[4, ],
  starts_with(common_names)
) %>%
  dplyr::select(where(function(x) !is.na(x)) &
                  where(function(x) {
                    c <- x != "NA"
                    if (is.na(c)) c <- TRUE
                    return(c)

                  }
                  ))



# For N supply
xml_fert_values <- get_param_xml(
  file = tec_xml, select = "formalisme",
  select_value = "fertilisation"
)[[1]]

# renaming param according to table (TODO: use param dict)
names(xml_fert_values)[names(xml_fert_values) %in%
                         c("julapN_or_sum_upvt", "absolute_value/%")] <- c("julapN", "doseN")

xl_fert_values <- dplyr::select(
  tec_param[4, ],
  starts_with(sort(names(xml_fert_values)))
) %>%
  dplyr::select_if(~ !any(is.na(.)))


xl_names <- sort(unique(gsub(pattern = "(.*)(\\_[0-9]*)",
                             x = colnames(xl_fert_values),
                             replacement = "\\1")))

common_names <- sort(intersect(names(xml_fert_values), xl_names))

xml_fert_values <- unlist(xml_fert_values[common_names],
                          use.names = FALSE
)

xl_fert_values <- dplyr::select(
  tec_param[4, ],
  starts_with(common_names)
) %>%
  dplyr::select_if(~ !any(is.na(.)))


context("Comparing table values vs xml tec file values")

test_that("Testing values for interventions", {
  expect_true(all(xml_res_vec == xl_res_vec))
  expect_true(all(xml_irr_values == xl_irr_values))
  expect_true(all(xml_fert_values == xml_fert_values))
})
