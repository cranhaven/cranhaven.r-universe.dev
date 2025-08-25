test_that("Test < 32000 feature example", 
  { expect_no_error(get_table_link_lookup('LAD22_CTY22_EN_LU', 'LAD22CD', 'CTY22CD', 'LAD22NM', 'CTY22NM')) })

test_that("Test raw_count > 32000 example", 
  { expect_no_error(get_table_link_lookup('LSOA_DEC_2021_EW_NC_v3', 'LSOA21NM', 'LSOA21NM', 'LSOA21CD', 'LSOA21CD')) })
