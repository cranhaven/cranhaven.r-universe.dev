test_that('stringterpolate working with basic objects',{
  user_name <- "smbache"
  amount <- 6.656
  account <- 1337

  x = stringterpolate("User ${user_name} (account $[08d]{account}) has $$[.2f]{amount}.")
  expect_equal(x, "User smbache (account 00001337) has $6.66.")

  x = stringterpolate("Nasty } nested { example $ : $[.2f]{{{2 + 2}*{amount}}}")
  expect_equal(x, "Nasty } nested { example $ : 26.62")


  x = stringterpolate("One value, ${value1}, and then another, ${value2*2}.",
                  list(value1 = 10, value2 = 20))
  expect_equal(x, "One value, 10, and then another, 40.")

  y = "Min and max are $[.2f]{max(Sepal.Width)} and $[.2f]{min(Sepal.Width)}."
  x = stringterpolate(y, iris)
  expect_equal(x, "Min and max are 4.40 and 2.00.")

})
