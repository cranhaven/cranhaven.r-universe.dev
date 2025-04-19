require(testthat)





context("\n\nProject.criterion \n")

fat1 <- Factor("fat1")
fat2 <- Factor("_23FAT")

test_that("Project.criterion constructor\n", {
  expect_error(Project.criterion("fat1", "C", TRUE), )
  expect_error(Project.criterion(fat2, "not.valid", TRUE), )
  expect_error(Project.criterion(fat2, "C", 12), )
  expect_error(Project.criterion(fat1, "C", ), )
  expect_is(Project.criterion(Factor("fat1"), "C", TRUE), "Project.criterion")
  expect_is(Project.criterion(fat1, "C", TRUE), "Project.criterion")
}
)




context("\n\nProject.criteria \n")

pc1 <- Project.criterion(Factor("fator1"), "LC", FALSE)
pc2 <- Project.criterion(Factor("fator1"), "C", FALSE)
pc3 <- Project.criterion(Factor("fator2"), "I", FALSE)

test_that("Project.criteria  constructor\n", {
  expect_error(Project.criteria(), )
  expect_error(Project.criteria(list()),)
  expect_error(Project.criteria(list(pc1,pc2)),)
  expect_is(Project.criteria(list(pc1,pc3)), "Project.criteria")
}
)






context("\n\nProject \n")

pc1 <- Project.criterion(Factor("fator0"), "LC", FALSE)
pc2 <- Project.criterion(Factor("fator1"), "C", FALSE)
pc3 <- Project.criterion(Factor("fator2"), "I", FALSE)
pc4 <- Project.criterion(Factor("fator3"), "C", TRUE)
pc5 <- Project.criterion(Factor("fator4"), "Cr", TRUE)

project.criteria1 <- Project.criteria(list(pc1, pc2, pc3))
project.criteria2 <- Project.criteria(list(pc2, pc3, pc4))
project.criteria3 <- Project.criteria(list(pc3, pc4, pc5))

project1 <- Project("p3",project.criteria1)


test_that("Project  constructor\n", {
  expect_error(Project(), )
  expect_error(Project("test", list()),)
  expect_error(Project("p1", list(pc1,pc2)), )
  expect_error(Project("p2", ), )
  expect_is(Project("p3",project.criteria1),"Project")
  expect_is(Project("p2",project.criteria2),"Project")
}
)


test_that("getProjectFactorsNames\n", {
  expect_error(getProjectFactorsNames(), )
  expect_error(getProjectFactorsNames(list()),)
  expect_error(getProjectFactorsNames(list("pc1",22)), )


  expect_is(getProjectFactorsNames(project1),"character")
  getProjectFactorsNames(project1)

}
)


context("\n\nProject.portfolio \n")

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

Project.portfolio <- Project.portfolio(list(project1,project2))

test_that("Project.portfolio  constructor\n", {
  expect_error(Project.portfolio(), )
  expect_error(Project.portfolio(list()),)
  expect_error(Project.portfolio(list(project1,pc2)),)
  expect_error(Project.portfolio(list(project1, project2, project3)),)
  expect_is(Project.portfolio(list(project1,project2)), "Project.portfolio")
}
)
