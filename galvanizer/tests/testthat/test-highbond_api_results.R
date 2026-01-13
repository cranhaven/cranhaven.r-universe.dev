upload <- data.frame(field_one = c('A','B','C'),
                     field_two = c(1, 2, 3),
                     field_three = c(TRUE, FALSE, TRUE),
                     field_four = c(as.Date('2019-01-01'), as.Date('2020-01-01'), as.Date('2021-12-31')),
                     field_five = c(as.POSIXct(Sys.time()), as.POSIXct(Sys.time()), as.POSIXct(Sys.time())),
                     field_six =  c(as.POSIXlt(Sys.time()), as.POSIXlt(Sys.time()), as.POSIXlt(Sys.time())),
                     field_seven = c(10L, 11L, 12L))

check_api <- function(){
  key <- Sys.getenv('highbond_openapi') 
  if (!nzchar(key)){
    skip('API not available')
  }
}

test_that("Highbond Results - POST with PURGE", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  highbond_table <- Sys.getenv('results_table')
  
  # Check if upload worked
  
  expect_null(post_results_records(hb_creds, highbond_table, upload = upload, purge = TRUE))
  
  # Col type check for re-download
  
  Sys.sleep(20) # Allow Highbond to process...
  
  download <- get_results_records(hb_creds, highbond_table, timezone = 'Canada/Pacific')
  
  coltypes <- download$content$columns %>%
    dplyr::filter(field_name %in% c('field_one', 'field_two', 'field_three', 'field_four', 'field_five', 'field_six', 'field_seven')) %>%
    dplyr::mutate(correctType = dplyr::case_when(
      field_name == 'field_one' & data_type == 'character'~ TRUE,
      field_name == 'field_two' & data_type == 'numeric'~ TRUE,
      field_name == 'field_three' & data_type == 'logical'~ TRUE,
      field_name == 'field_four' & data_type == 'date'~ TRUE,
      field_name == 'field_five' & data_type == 'datetime'~ TRUE,
      field_name == 'field_six' & data_type == 'datetime'~ TRUE,
      field_name == 'field_seven' & data_type == 'numeric'~ TRUE,
      TRUE ~ FALSE
    ))
  
  expect_equal(sum(coltypes$correctType), ncol(upload))
})

test_that("Highbond Results - POST without Purge", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  highbond_table <- Sys.getenv('results_table')
  
  # Start from a blank slate, and purge
  
  expect_null(post_results_records(hb_creds, highbond_table, upload = upload, purge = TRUE))
  
  Sys.sleep(20)
  
  # Get the current reference
  download <- get_results_records(hb_creds, highbond_table)
  current_count <- nrow(download$content$data)
  
  # Then upload again without a purge
  expect_null(post_results_records(hb_creds, highbond_table, upload = upload, purge = FALSE))
  
  # Wait delay
  
  Sys.sleep(20)
  
  # Get the new state
  download <- get_results_records(hb_creds, highbond_table)
  
  new_count <- nrow(download$content$data)
  expect_equivalent(current_count + nrow(upload), new_count)
})

test_that("Highbond Results - GET", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  highbond_table <- Sys.getenv('results_table')
  
  Sys.sleep(20)
  
  download <- get_results_records(hb_creds, highbond_table)
  
  expect_true(nrow(download$content$data) >  0)
})

test_that("Highbond Results - POST - Stress test", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  highbond_table <- Sys.getenv('results_table')
  
  massupload <- do.call("rbind", replicate(1000, upload, simplify = FALSE))
  
  expect_null(post_results_records(hb_creds, highbond_table, upload = massupload, purge = TRUE))
  
  Sys.sleep(20)
})

test_that("Highbond Results - Create, Update, Delete Collections", {
  check_api()
  hb_creds <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  
  name <- 'galvanizer R Testthat Collection'
  response1 <- create_results_collections(hb_creds, name)
  expect_true(nrow(response1) == 1)

  collection_id <- response1$id[[1]]
  myattr <- list(name = 'galvanizer Super Test Collection', description = 'My second description')
  response2 <- update_results_collections(hb_creds, collection_id, attributes = myattr)
  expect_true(response2$description[[1]] == 'My second description')
  
  response3 <- delete_results_collections(hb_creds, collection_id)
  expect_true(response3$status_code == 202)
})

test_that("Highbond Results - Create, Update, Delete everything", {
  check_api()
  auth <- setup_highbond(Sys.getenv('highbond_openapi'), Sys.getenv('highbond_org'), Sys.getenv('highbond_datacenter'))
  collection_name <- 'galvanizer Test Collection'
  response1 <- create_results_collections(auth, collection_name)
  collection_id <- response1$id[[1]]
  expect_true(nrow(response1) == 1)
  
  ### ANALYSES
  analysis_name <- 'galvanizer test analysis'
  response2 <- create_results_analyses(auth, collection_id, name = analysis_name)
  analysis_id <- response2$id[[1]]
  expect_true(nrow(response2) == 1)
  
  myattr <- list(description = 'My second description')
  response3 <- update_results_analyses(auth, analysis_id, attributes = myattr)
  expect_true(response3$description[[1]] == 'My second description')
  
  ### TABLES
  table_name <- 'galvanizer test table'
  response4 <- create_results_tables(auth, analysis_id, name = analysis_name)
  expect_true(nrow(response4) == 1)
  
  table_id <- response4$id[[1]]
  myattr <- list(description = 'My third description', script_name = "myscript.R")
  response5 <- update_results_tables(auth, table_id, attributes = myattr)
  expect_true(response5$description[[1]] == 'My third description')
  
  ### COLUMNS
  field_name <- c("a", "b", "c", "d", "e", "f", "g")
  display_name <- c("field_one", "field_two", "field_three", "field_four", "field_five", "field_six", "field_seven")
  data_type <- c("character", "numeric", 'logical', 'date', 'datetime', 'file', 'digital_signature')
  
  response6 <- create_results_columns(auth, table_id, data.frame(field_name, display_name, data_type))
  expect_true(nrow(response6) == 7)
  
  response7 <- get_results_columns(auth, table_id)
  expect_true(nrow(response7) == 7)
  
  # Cleanup
  response8 <- delete_results_analyses(auth, analysis_id)
  expect_false(httr::http_error(response8))
  
  response9 <- delete_results_tables(auth, table_id)
  expect_false(httr::http_error(response9))
  
  response10 <- delete_results_collections(auth, collection_id)
  expect_false(httr::http_error(response10))
})