context("Project operations")

# get data ----------------------------------------------------------------

branch = "master"

DF = mtcars

p = Project(
  file = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_basic",
    "project_config.yaml",
    package = "pepr"
  )
)
pFileMissing = p
pFileMissing@config$sample_table = "missing"
pSubproj1 = Project(
  file = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_amendments1",
    "project_config.yaml",
    package = "pepr"
  )
)
pSubproj2 = Project(
  file = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_amendments2",
    "project_config.yaml",
    package = "pepr"
  )
)
pSub = Project(
  file = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_subtable1",
    "project_config.yaml",
    package = "pepr"
  )
)
pImplied = Project(
  file = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_imply",
    "project_config.yaml",
    package = "pepr"
  )
)

pDerived = Project(
    file = system.file(
        "extdata",
        paste0("example_peps-",branch),
        "example_derive",
        "project_config.yaml",
        package = "pepr"
    )
)

# tests -------------------------------------------------------------------

test_that("getSubsample method throws errors", {
  expect_error(getSubsample(mtcars))
  expect_error(getSubsample(p, "frog_1", "test"))
  expect_error(getSubsample(p, "x", "test"))
})


test_that("getSubsample method returns a correct size DF", {
  expect_equal(dim(getSubsample(pSub, "frog_1", "sub_a")), c(1, 4))
})


test_that(".implyAttributes returns Project object", {
  expect_is(.implyAttrs(pImplied), 'Project')
})

test_that(".deriveAttributes returns Project object", {
  expect_is(.deriveAttrs(pDerived), 'Project')
})

test_that(".listAmendments internal function returns correct object type, length and throws errors",
          {
            expect_equal(length(.listAmendments(pSubproj1@config)), 2)
            expect_is(.listAmendments(pSubproj2@config), 'character')
            expect_null(.listAmendments(p@config))
          })

test_that("listSubprojects exported method returns correct object type, length and throws errors",
          {
            expect_equal(length(listAmendments(pSubproj1)), 2)
            expect_is(listAmendments(pSubproj1), 'character')
            expect_null(listAmendments(p))
            expect_error(listAmendments(1))
            expect_equal(length(listAmendments(pSubproj1)), 2)
          })

test_that("checkSection returns a correct type", {
  expect_is(checkSection(config(p),"metadata"),"logical")
  expect_is(checkSection(config(p),"test"),"logical")
})

test_that("checkSection returns correct value", {
  expect_equal(checkSection(config(p),c("sample_table")), T)
  expect_equal(checkSection(config(p),c("test")), F)
})

test_that("activateAmendments does not fail and throws a warning when called 
          with invalid subproject name", {
              expect_warning(activateAmendments(pSubproj1, "test"))
})

test_that("activateAmendments returns a correct object type", {
    expect_is(activateAmendments(pSubproj1, "newLib2"), "Project")
})

test_that(".listAmendments works with different styles of printing", {
    expect_message(.listAmendments(config(pSubproj1)))
    expect_output(.listAmendments(config(pSubproj1),style="cat"))
})

test_that("show methods work", {
    expect_output(show(p))
    expect_output(show(config(p)))
})

test_that("getSample errors", {
    expect_error(getSample(p, "test"))
})

test_that("getSample returns a correct object type", {
    expect_is(getSample(p,"frog_1"), "data.table")
})

test_that("getSample errors", {
    expect_error(getSubsample(p, "test","test"))
    expect_error(getSubsample(pSub,"frog_1", "test"))
})

test_that("getSample returns a correct object type", {
    expect_is(getSubsample(pSub,"frog_1", "sub_a"), "data.table")
})