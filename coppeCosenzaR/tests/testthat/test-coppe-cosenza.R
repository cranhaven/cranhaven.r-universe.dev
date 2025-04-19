require(testthat)


#load test data
#
# Option
#
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

#
#
#  Projects
#

pc1 <- Project.criterion(Factor("fator0"), "LC", FALSE)
pc2 <- Project.criterion(Factor("fator1"), "C", FALSE)
pc3 <- Project.criterion(Factor("fator2"), "I", FALSE)
pc4 <- Project.criterion(Factor("fator3"), "C", TRUE)
pc5 <- Project.criterion(Factor("fator4"), "Cr", TRUE)

project.criteria1 <- Project.criteria(list(pc1, pc2, pc3))
project.criteria2 <- Project.criteria(list(pc2, pc3, pc4))
project.criteria3 <- Project.criteria(list(pc3, pc4, pc5))

project1 <- Project("p3",project.criteria1)
project2 <- Project("p2",project.criteria2)
project3 <- Project("p2",project.criteria1)


project.portfolio <- Project.portfolio(list(project1,project2))

#
#
# Factors of interest
#
factors.of.interest <-
  Factors.of.interest(list(Factor("fator2")))


context("\n\n Coppe.cosenza \n")



test_that("Coppe.cosenza constructor\n", {
  expect_error(
    Coppe.cosenza(project3, option.portfolio, factors.of.interest), )
  expect_error(
    Coppe.cosenza(project.portfolio, option2, factors.of.interest), )
  expect_error(
    Coppe.cosenza(project.portfolio, option.portfolio, Factor("factor")), )

  expect_error(
    Coppe.cosenza( , option.portfolio, factors.of.interest), )
  expect_error(
    Coppe.cosenza(project.portfolio,  , factors.of.interest), )
  expect_error(
    Coppe.cosenza(project.portfolio, option.portfolio,  ), )

  expect_error(
    Coppe.cosenza(
      project.portfolio,
      option.portfolio,
      Factors.of.interest("fator1")
      ),
    )

  expect_is(
    Coppe.cosenza(
      project.portfolio,
      option.portfolio,
      factors.of.interest
      ),
    "Coppe.cosenza"
  )
}
)
