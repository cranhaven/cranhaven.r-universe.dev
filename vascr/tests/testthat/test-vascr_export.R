test_that("export works", {

  small_growth = growth.df %>% vascr_subset(time = c(0,10), unit = c("R", "Rb"))
  # expect_snapshot(vascr_export(small_growth))
  
  suppressMessages({
  filepath = tempfile("test_export", fileext = ".xlsx")
  vascr_export_prism(small_growth, filepath)
  re_import = readxl::read_xlsx(filepath,2)
  remove(filepath)
  })
  
  expect_snapshot({
                    re_import
                    colnames(re_import)
  })
  
  expect_snapshot({
    filepath = tempfile("test_export", fileext = ".xlsx")
    suppressMessages(vascr_export_prism(small_growth, filepath, level = "wells"))
    re_import = readxl::read_xlsx(filepath,2)
    re_import
    colnames(re_import)
    remove(filepath)
  })
                    

  
})
