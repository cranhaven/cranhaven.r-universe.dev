# Helper function to simulate an RCDF file (this is just a mock)
create_mock_rcdf <- function(temp_dir, pw = NULL) {
  # Create a mock RCDF zip file structure

  key <- openssl::rsa_keygen()
  openssl::write_pem(key$pubkey, file.path(temp_dir, 'pub.pem'))
  openssl::write_pem(key, file.path(temp_dir, 'prv.pem'), password = pw)

  data <- list(
    dataset1 = data.frame(a = 1:5, b = letters[1:5]),
    dataset2 = data.frame(a = 6:10, b = letters[6:10])
  )

  write_rcdf(
    as_rcdf(data),
    path = file.path(temp_dir, 'mock.rcdf'),
    pub_key = file.path(temp_dir, 'pub.pem')
  )

  return(file.path(temp_dir, 'mock.rcdf'))

}


test_that("read_rcdf can read and decrypt RCDF files", {

  # Create a temporary directory for testing
  temp_dir <- tempdir()
  mock_rcdf <- create_mock_rcdf(temp_dir)

  # Test reading the RCDF
  rcdf_data <- read_rcdf(
    path = mock_rcdf,
    decryption_key = file.path(temp_dir, 'prv.pem')
  )

  # Check if the RCDF object is returned correctly
  expect_s3_class(rcdf_data, "rcdf")
  expect_true("dataset1" %in% names(rcdf_data))

  # Clean up mock RCDF file
  unlink(rcdf_data)
  unlink(file.path(temp_dir, 'prv.pem'))
  unlink(file.path(temp_dir, 'pub.pem'))

})


test_that("read_rcdf can read and decrypt RCDF files with RSA password protected key", {

  # Create a temporary directory for testing
  temp_dir <- tempdir()
  mock_rcdf <- create_mock_rcdf(temp_dir, pw = 'xxx')

  # Test reading the RCDF
  rcdf_data <- read_rcdf(
    path = mock_rcdf,
    decryption_key = file.path(temp_dir, 'prv.pem'),
    password = 'xxx'
  )

  # Check if the RCDF object is returned correctly
  expect_s3_class(rcdf_data, "rcdf")
  expect_true("dataset1" %in% names(rcdf_data))

  # Clean up mock RCDF file
  unlink(rcdf_data)
  unlink(file.path(temp_dir, 'prv.pem'))
  unlink(file.path(temp_dir, 'pub.pem'))

})
