test_that("A valid table ID
          should return a list
          ", {
            expect_type(get_available_scales("NM_1_1"), "list")
          })

# 

#' THIS TEST RUNS FOR ALL TABLES SO HAS BEEN COMMENTED OUT
#availTables <- list_tables() %>% dplyr::filter(id != "NM_45_1")
# 
# for (i in c(1:length(availTables[[1]]))) {
#   test_that(paste0(availTables[i, 2], " returns
#           a character vector"), {
#             expect_type(get_available_scales(availTables[[i, 2]]), "character")
#           })
# }
#   