test_that("progenesis_formatter reads data correctly", {
  pt <- progenesis_formatter(test_path("exttestdata",
                                       "102623_peaktable_coculture_simple.csv"))

  expect_equal(class(pt$peak_table), c("data.table", "data.frame"))
  expect_true(all(c("data.table", "data.frame") %in% class(pt$peak_table)))
  expect_true(nrow(pt$peak_table) > 0)
})
test_that("metaboscape reads data correctly", {
  samples <- c(
    "UM1850B_ANGDT_0.25_mL_36_1_4792",
    "UM1850B_ANGDT_0.25_mL_36_1_4802",
    "UM1850B_ANGDT_0.25_mL_36_1_4815",
    "MixedMonoculture_1mg_mL total_31_1_4799",
    "MixedMonoculture_1mg_mL total_31_1_4822",
    "MixedMonoculture_31_1_4787",
    "UM1852B_Coculture_42_1_4685",
    "UM1852B_Coculture_42_1_4695",
    "UM1852B_Coculture_42_1_4709",
    "UM1852B_Coculture_42_1_4785"
  )
  peak_table_name <- "MJB_MonoVSCoculture_metaboscape_ft.csv"
  pt <- metaboscape_formatter(test_path("exttestdata",
                                        peak_table_name),
                              sample_names = samples)

  expect_equal(class(pt$peak_table), c("data.table", "data.frame"))
  expect_true(all(c("data.table", "data.frame") %in% class(pt$peak_table)))
  expect_true(nrow(pt$peak_table) > 0)
  expect_equal(colnames(pt$peak_table), c("Compound", "mz", "rt", samples))
})
test_that("mz_mine formatter reads data correctly", {

})

test_that("format_by_type formats the data properly based on the parameters", {
  samples <- c(
    "UM1850B_ANGDT_0.25_mL_36_1_4792",
    "UM1850B_ANGDT_0.25_mL_36_1_4802",
    "UM1850B_ANGDT_0.25_mL_36_1_4815",
    "MixedMonoculture_1mg_mL total_31_1_4799",
    "MixedMonoculture_1mg_mL total_31_1_4822",
    "MixedMonoculture_31_1_4787",
    "UM1852B_Coculture_42_1_4685",
    "UM1852B_Coculture_42_1_4695",
    "UM1852B_Coculture_42_1_4709",
    "UM1852B_Coculture_42_1_4785"
  )
  progenesis_peak_table <- "102623_peaktable_coculture_simple.csv"
  metabscape_peak_table <- "MJB_MonoVSCoculture_metaboscape_ft.csv"
  progenesis_format_table <- format_by_type(test_path("exttestdata",
                                                      progenesis_peak_table),
                                            type_of_peak_table = "Progenesis")
  # mzmine_format_table <- format_by_type(test_path("../../inst/extdata",
  # "MJB_MonoVSCoculture_metaboscape_ft.csv"),
  # sample_names = samples, type_of_peak_table = "MzMine")
  metaboscape_format_table <- format_by_type(test_path("exttestdata",
                                                       metabscape_peak_table),
                                             sample_names = samples,
                                             type_of_peak_table = "Metaboscape")

  expect_true(all(c("data.table", "data.frame") %in%
                    class(progenesis_format_table$peak_table)))
  # expect_true(all(c("data.table",
  # "data.frame") %in% class(mzmine_format_table$peak_table)))
  expect_true(all(c("data.table", "data.frame") %in%
                    class(metaboscape_format_table$peak_table)))
})

test_that("Formatter errors when giving a type that
does not exist", {
            samples <- c(
              "UM1850B_ANGDT_0.25_mL_36_1_4792",
              "UM1850B_ANGDT_0.25_mL_36_1_4802",
              "UM1850B_ANGDT_0.25_mL_36_1_4815",
              "MixedMonoculture_1mg_mL total_31_1_4799",
              "MixedMonoculture_1mg_mL total_31_1_4822",
              "MixedMonoculture_31_1_4787",
              "UM1852B_Coculture_42_1_4685",
              "UM1852B_Coculture_42_1_4695",
              "UM1852B_Coculture_42_1_4709",
              "UM1852B_Coculture_42_1_4785"
            )
            progenesis_peak_table <- "102623_peaktable_coculture_simple.csv"
            expect_error(format_by_type(test_path("exttestdata",
                                                  progenesis_peak_table),
                                        type_of_peak_table = "Wrong"))

          })

test_that("The 'None' format option works as expected", {
  samples <- c(
    "UM1850B_ANGDT_0.25_mL_36_1_4792",
    "UM1850B_ANGDT_0.25_mL_36_1_4802",
    "UM1850B_ANGDT_0.25_mL_36_1_4815",
    "MixedMonoculture_1mg_mL total_31_1_4799",
    "MixedMonoculture_1mg_mL total_31_1_4822",
    "MixedMonoculture_31_1_4787",
    "UM1852B_Coculture_42_1_4685",
    "UM1852B_Coculture_42_1_4695",
    "UM1852B_Coculture_42_1_4709",
    "UM1852B_Coculture_42_1_4785"
  )
  pt <- "peak_table_post_formatted.csv"
  pt <- format_by_type(test_path("exttestdata",
                                 pt),
                       type_of_peak_table = "None")

  expect_equal(class(pt$peak_table), c("data.table", "data.frame"))
  expect_true(all(c("data.table", "data.frame") %in% class(pt$peak_table)))
  expect_true(nrow(pt$peak_table) > 0)
})

test_that("format_by_type errors when given incomplete file paths", {
  pt <- "peak_table_post_formatted.csv"
  expect_error(format_by_type(pt,
                              type_of_peak_table = "None"))
})
