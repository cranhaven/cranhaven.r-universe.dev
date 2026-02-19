# Helper function to create a mock RSA public key
create_mock_pub_key <- function(temp_dir, pw = NULL) {
  key <- openssl::rsa_keygen()
  openssl::write_pem(key$pubkey, file.path(temp_dir, 'pub.pem'))
  openssl::write_pem(key, file.path(temp_dir, 'prv.pem'), password = pw)

  return(file.path(temp_dir, 'pub.pem'))
}


test_that("write_rcdf creates a valid RCDF file", {

  # Create a mock dataset
  mock_data <- list(
    dataset1 = data.frame(a = 1:5, b = letters[1:5]),
    dataset2 = data.frame(x = 6:10, y = letters[6:10])
  )

  mock_data <- as_rcdf(mock_data)

  # Create a mock public key
  dir_temp <- tempdir()
  pub_key <- create_mock_pub_key(dir_temp)

  # Create a temporary path for the RCDF file
  rcdf_path <- tempfile(fileext = ".rcdf")

  # Write the data to RCDF format
  write_rcdf(data = mock_data, path = rcdf_path, pub_key = pub_key)

  meta <- extract_rcdf(rcdf_path)

  expect_true(file.exists(file.path(meta$dir, 'metadata.json')))
  expect_true(file.exists(file.path(meta$dir, 'lineage')))

  # Clean up the mock public key
  unlink(pub_key, force = T)
  unlink(file.path(dir_temp, 'prv.pem'), force = T)

})



test_that("write_rcdf creates RCDF file with correct encryption", {

  # Create a mock dataset
  mock_data <- list(
    dataset1 = data.frame(a = 1:5, b = letters[1:5]),
    dataset2 = data.frame(x = 6:10, y = letters[6:10])
  )

  mock_data <- as_rcdf(mock_data)

  # Create a mock public key
  dir_temp <- tempdir()
  pub_key <- create_mock_pub_key(dir_temp)

  # Create a temporary path for the RCDF file
  rcdf_path <- tempfile(fileext = ".rcdf")

  # Write the data to RCDF format
  write_rcdf(data = mock_data, path = rcdf_path, pub_key = pub_key)

  meta <- extract_rcdf(rcdf_path)

  metadata_file <- jsonlite::fromJSON(file.path(meta$dir, 'metadata.json'), simplifyVector = T)

  expect_true("key_app" %in% names(metadata_file))
  expect_true("iv_app" %in% names(metadata_file))

  unlink(dir_temp, recursive = TRUE)

})
