# get data ----------------------------------------------------------------


branch = "master"

cfg = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_basic",
    "project_config.yaml",
    package = "pepr"
  )

sampleTableBasic = system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_basic",
  "sample_table.csv",
  package = "pepr"
)

sampleTableSubsamples = system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_subsamples",
  "sample_table.csv",
  package = "pepr"
)

f = yaml::yaml.load_file(system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_amendments2",
  "project_config.yaml",
  package = "pepr"
))

cfgAutomerge = system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_automerge",
  "project_config.yaml",
  package = "pepr"
)

cfgAutomergeSubtable = system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_subtable_automerge",
  "project_config.yaml",
  package = "pepr"
)


cfgAmend = system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_amendments2",
  "project_config.yaml",
  package = "pepr"
)

cfgSubtable = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_subtable1",
    "project_config.yaml",
    package = "pepr"
)

cfgSubtableMulti = system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_subtables",
  "project_config.yaml",
  package = "pepr"
)

configAppend = system.file(
    "extdata",
    paste0("example_peps-",branch),
    "example_append",
    "project_config.yaml",
    package = "pepr"
)

configDerive = system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_derive",
  "project_config.yaml",
  package = "pepr"
)

configImply = system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_imply",
  "project_config.yaml",
  package = "pepr"
)

configDuplicate = system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_duplicate",
  "project_config.yaml",
  package = "pepr"
)

configImports = system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_imports",
  "project_config.yaml",
  package = "pepr"
)

configRemove = system.file(
  "extdata",
  paste0("example_peps-",branch),
  "example_remove",
  "project_config.yaml",
  package = "pepr"
)

# tests -------------------------------------------------------------------

context("Project object creation")

test_that("Project creates an object of class Project", {
  expect_is(Project(cfg), 'Project')
})

test_that("empty Project can be created", {
  expect_is(Project(), "Project")
})

context("Project object creation -- no config")

test_that("Project can be instantiated with no config", {
  expect_is(Project(file = sampleTableBasic), "Project")
})

context("Project object creation -- remote sample table")

test_that("Project can be instantiated with a remote sample table", {
  if (require("curl")) {  # only if curl is installed
    expect_is(Project(file = "https://raw.githubusercontent.com/pepkit/example_peps/master/example_basic/sample_table.csv"), "Project")
  }
})

context("Nonexistant init files")

test_that("Project constructor throws errors if nonexistant init files are provided", {
  expect_warning(Project(file = "test.csv"))
  expect_error(Project(file = "test.yaml"))
})


context("Sample automerging")

test_that("Samples with duplicated names are auto-merged", {
  expect_equal(nrow(sampleTable(Project(file = cfgAutomerge))), 3)
})

test_that("Samples with duplicated names are disallowed when subsample table is specified", {
  expect_error(Project(file = cfgAutomergeSubtable))
})

context("Amendments")

test_that("Project succesfully activates amendments at initialization", {
    expect_equal(NROW(sampleTable(Project(cfgAmend, amendments = "noFrog"))), 4)
})

context("Modifiers: append")

test_that("append modifier works", {
    expect_equal(length(sampleTable(Project(configAppend))[["read_type"]]), 4)
})

context("Modifiers: derive")

test_that("derive modifier works", {
  expect_true(all(vapply(sampleTable(Project(configDerive))[["file_path"]], function(x){x=="source1"}, TRUE))==FALSE)
})

context("Modifiers: imply")

test_that("imply modifier works", {
  s = sampleTable(Project(configImply))
  expect_true(all(s[which(s[,"organism"]=="human") ,"genome"] == "hg38"))
})

context("Modifiers: duplicate")


test_that("duplicate modifier works", {
  s = sampleTable(Project(configDuplicate))
  expect_true(all(s[,"organism"]==s[,"animal"]))
})

context("Modifiers: remove")


test_that("remove modifier works", {
  p = Project(configRemove)
  removed = p@config$sample_modifiers$remove
  expect_false(removed %in% colnames(sampleTable(p)))
})

context("Import external configs")


test_that("importing external configs works", {
  expect_true(all(sampleTable(Project(configImports))[,"imported_attr"] == "imported_val"))
})

context("Version number parsing")
test_that("invalid version string is exceptional", {
  path = paste(tempdir(), "test.yaml", sep="/")
  f$pep_version = "2"
  yaml::write_yaml(file = path, x = f)
  expect_error(Project(file = path))
})


test_that("version has to be 2.0.0 if sample_modifiers key is in the config", {
  path = paste(tempdir(), "test.yaml", sep="/")
  f$pep_version = "1.0.0"
  yaml::write_yaml(file = path, x = f)
  expect_error(Project(file = path))
})


test_that("Version number is required", {
  path = paste(tempdir(), "test.yaml", sep="/")
  f$pep_version = NULL
  yaml::write_yaml(file = path, x = f)
  expect_error(Project(file = path))
})


test_that("Subsample table works", {
  p=Project(file=cfgSubtable)
  expect_true("subsample_name" %in% colnames(p@samples))
  expect_equal(length(p@samples$file[[1]]), 3)
})


test_that("Subsample table works for multiple subtables", {
  p=Project(file=cfgSubtableMulti)
  expect_true("desc" %in% colnames(p@samples))
})


