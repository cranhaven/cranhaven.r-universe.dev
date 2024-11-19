  test_that("Error is returned for invalid table id", {
    expect_message(get_overview("N_1002_1"))
  }
  )

  list_of_tables <- list("NM_1_1","NM_20_1","NM_99_1","NM_103_1","NM_147_1","NM_189_1","NM_210_1","NM_399_1","NM_403_1","NM_547_1","NM_1089_1")
  for (tab_id in list_of_tables){
  test_that(paste0("No error is returned for valid table id. Table id = ",tab_id), {
    expect_no_message(get_overview(tab_id))
  }
  )
  }
  
  
  test_that("Error is returned for invalid table id", {
    expect_message(get_table_info_brief("02_1"))
  }
  )
  
  test_that("No error is returned for valid table id", {
    expect_no_message(get_table_info_brief("NM_42_1"))
  }
  )
  
  
  list_of_tables <- list("NM_1_1","NM_20_1","NM_99_1","NM_103_1","NM_147_1","NM_189_1","NM_210_1","NM_399_1","NM_403_1","NM_547_1","NM_1089_1")
  for (tab_id in list_of_tables){
    test_that(paste0("No error is returned for valid table id. Table id = ",tab_id), {
      expect_no_message(get_table_info_brief(tab_id))
    }
    )
  }