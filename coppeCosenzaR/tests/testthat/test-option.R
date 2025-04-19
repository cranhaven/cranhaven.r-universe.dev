require(testthat)




context("\n\nOption.factor.availability constructor\n")

fat1 <- Factor("fat1")
fat2 <- Factor("_23FAT")
# Ex", "G", "R", "W", "Em", "Z", "In"

test_that("Option.factor.availability constructor\n", {
  expect_error(Option.factor.availability("fat1", "R"), )
  expect_error(Option.factor.availability(fat2, "not.valid"), )
  expect_error(Option.factor.availability(, "W"), )
  expect_error(Option.factor.availability(fat1, ), )
  expect_is(Option.factor.availability(Factor("fat1"), "Ex"), "Option.factor.availability")
  expect_is(Option.factor.availability(fat1, "G"), "Option.factor.availability")
}
)




context("\n\n Option.resources constructor\n")

pc1 <- Option.factor.availability(Factor("fator1"), "R")
pc2 <- Option.factor.availability(Factor("fator1"), "W")
pc3 <- Option.factor.availability(Factor("fator2"), "Ex")

test_that("Option.resources  constructor\n", {
  expect_error(Option.resources(), )
  expect_error(Option.resources(list()),)
  expect_error(Option.resources(list(pc1,pc2)),)
  expect_is(Option.resources(list(pc1,pc3)), "Option.resources")
}
)






context("\n\n Option constructor\n")

ofa1 <- Option.factor.availability(Factor("fator0"), "R")
ofa2 <- Option.factor.availability(Factor("fator1"), "Ex")
ofa3 <- Option.factor.availability(Factor("fator2"), "R")
ofa4 <- Option.factor.availability(Factor("fator3"), "W")
ofa5 <- Option.factor.availability(Factor("fator4"), "Ex")

option.resources1 <- Option.resources(list(ofa1, ofa2, ofa3))
option.resources2 <- Option.resources(list(ofa2, ofa3, ofa4))
option.resources3 <- Option.resources(list(ofa3, ofa4, ofa5))



test_that("Option  constructor\n", {
  expect_error(Option(), )
  expect_error(Option("test", list()),)
  expect_error(Option("o1", list(ofa1,ofa2)), )
  expect_error(Option("o2", ), )
  expect_is(Option("o3",option.resources1),"Option")
  expect_is(Option("o2",option.resources2),"Option")
}
)




context("\n\n Option.portfolio constructor\n")

ofa1 <- Option.factor.availability(Factor("fator0"), "R")
ofa2 <- Option.factor.availability(Factor("fator1"), "Ex")
ofa3 <- Option.factor.availability(Factor("fator2"), "R")
ofa4 <- Option.factor.availability(Factor("fator3"), "W")
ofa5 <- Option.factor.availability(Factor("fator4"), "Ex")

option.resources1 <- Option.resources(list(ofa1, ofa2, ofa3))
option.resources2 <- Option.resources(list(ofa2, ofa3, ofa4))
option.resources3 <- Option.resources(list(ofa3, ofa4, ofa5))

option1 <- Option("o3",option.resources1)
option2 <- Option("o2",option.resources2)
option3 <- Option("o2",option.resources3)

option.portfolio <- Option.portfolio(list(option1,option3))

test_that("Option.portfolio  constructor\n", {
  expect_error(Option.portfolio(), )
  expect_error(Option.portfolio(list()),)
  expect_error(Option.portfolio(list(option1,option.resources1)),)
  expect_error(Option.portfolio(list(option1, option2, option3)),)
  expect_is(Option.portfolio(list(option1,option3)), "Option.portfolio")
}
)
