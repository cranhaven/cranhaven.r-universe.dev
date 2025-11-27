test_that("encryption and decryption", {
  expect_equal({
    df <- data.frame(vec = c("foo", "bar"))
    df$vec <- encrypt_vec(df$vec)
    decrypt_vec(df$vec)
  }, c("foo", "bar"))
})
