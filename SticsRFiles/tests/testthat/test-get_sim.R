options(warn=-1)

stics_version <- get_stics_versions_compat()$latest_version
version_num <- get_version_num()

context("get model outputs")

path <- get_examples_path(file_type = "sti", stics_version = stics_version)

# Testing with a JavaSTICS workspace like
test_that("output is always list", {
  outputs <- get_sim(path, "banana")
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  # Should always returns a list, even for one usm
})

test_that("get output without usm argument", {
  outputs <- get_sim(workspace = path)
  # There are two USMs in the usms.xml file, but only one output file (banana):
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  expect_true(is.data.frame(outputs$banana))
})

# Testing without the usms.xml file
if (file.exists(file.path(path, "usms.xml"))) {
  file.rename(file.path(path, "usms.xml"), file.path(path, "usms.xml.ori"))
}

test_that("output is always list, without usms.xml", {
  outputs <- get_sim(path, "banana")
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  # Should always returns a list, even for one usm
})

test_that("get output without usm argument, without usms.xml", {
  outputs <- get_sim(workspace = path)
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  expect_true(is.data.frame(outputs$banana))
})

# Testing with 2 workspaces ------------------------------------
# returning always a single list
path1 <- file.path(path, "workspace1")
path2 <- file.path(path, "workspace2")
paths <- c(path1, path2)

test_that(
  "get output for 2 workspaces without usm argument, without usms.xml",
  {
    files_nb <- length(list.files(paths))
    outputs <- get_sim(workspace = paths)
    expect_true(is.list(outputs) && !is.data.frame(outputs))
    expect_true(all(unlist(lapply(outputs, is.data.frame))))
  }
)

# if usm is given, even with one usm in each workspace
test_that("get output for 2 workspaces with usm argument, without usms.xml", {
  files_nb <- length(list.files(paths))
  outputs <- get_sim(workspace = paths, usm = c("maize", "wheat"))
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  expect_true(all(unlist(lapply(outputs, is.data.frame))))
  expect_true(all(names(outputs) %in% c("maize", "wheat")))
})

test_that("get output for 2 workspaces without usm argument, with usms.xml", {
  files_nb <- length(list.files(paths))
  outputs <- get_sim(
    workspace = paths,
    usms_file = file.path(path, "usms_example.xml")
  )
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  expect_true(all(unlist(lapply(outputs, is.data.frame))))
})


test_that("get output for 2 workspaces with usm argument, with usms.xml", {
  files_nb <- length(list.files(paths))
  outputs <- get_sim(
    workspace = paths, usms_file = file.path(path, "usms_example.xml"),
    usm = c("maize", "wheat")
  )
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  expect_true(all(unlist(lapply(outputs, is.data.frame))))
  expect_true(all(names(outputs) %in% c("maize", "wheat")))
})

# Common root dir
path3 <- file.path(path, "workspace_root")
if (!dir.exists(path3)) dir.create(path3)

# Testing usm as path sub-directories  ------------------------------------
if (!dir.exists(file.path(path3, "banana"))) {
  dir.create(
    file.path(path3, "banana")
  )
}
file.copy(file.path(path, "mod_sbanana.sti"), file.path(path3, "banana"))
if (!dir.exists(file.path(path3, "wheat"))) {
  dir.create(
    file.path(path3, "wheat")
  )
}
file.copy(file.path(path2, "mod_swheat.sti"), file.path(path3, "wheat"))

test_that("output is always list, without usms.xml, banana, wheat sub-dir", {
  outputs <- get_sim(path3, "banana")
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  outputs <- get_sim(path3, usm = c("banana", "wheat"))
  expect_true(is.list(outputs) && !is.data.frame(outputs))

})

# Restoring usms.xml
if (file.exists(file.path(path, "usms.xml.ori"))) {
  file.rename(file.path(path, "usms.xml.ori"), file.path(path, "usms.xml"))
}

test_that("output is always list, with usms.xml, banana, wheat sub-dir", {
    outputs <- get_sim(path3, "banana",
    usms_file = file.path(path, "usms_example.xml")
  )
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  outputs <- get_sim(path3,
    usm = c("banana", "wheat"),
    usms_file = file.path(path, "usms_example.xml")
  )
  expect_true(is.list(outputs) && !is.data.frame(outputs))

})

unlink(file.path(path, "banana"))
unlink(file.path(path, "wheat"))


# Testing intercropping usms  ------------------------------------
example_ic <- download_data(
  example_dirs = "study_case_intercrop",
  stics_version = stics_version
)

test_that("get simulations with intercrops", {
  outputs <- get_sim(workspace = example_ic)
  # There are two USMs in the usms.xml file, but only one output file (banana):
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  expect_true(all(names(outputs) %in%
    c(
      "IC_Wheat_Pea_2005-2006_N0", "SC_Pea_2005-2006_N0",
      "SC_Wheat_2005-2006_N0"
    )))
  expect_true(is.data.frame(outputs$`SC_Pea_2005-2006_N0`))
  expect_equal(
    unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Plant),
    c("plant_1", "plant_2")
  )
  expect_equal(
    unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Dominance),
    c("Principal", "Associated")
  )
  expect_equal(unique(outputs$`SC_Pea_2005-2006_N0`$Plant), "plant_1")
  expect_null(outputs$`SC_Pea_2005-2006_N0`$Dominance)
})

test_that("get simulations with intercrops, giving usms.xml file", {
  outputs <- get_obs(
    workspace = example_ic,
    usms_file = file.path(example_ic, "usms.xml")
  )
  # There are two USMs in the usms.xml file, but only one output file (banana):
  expect_true(is.list(outputs) && !is.data.frame(outputs))
  expect_true(all(names(outputs) %in%
    c(
      "IC_Wheat_Pea_2005-2006_N0", "SC_Pea_2005-2006_N0",
      "SC_Wheat_2005-2006_N0"
    )))
  expect_true(is.data.frame(outputs$`SC_Pea_2005-2006_N0`))
  expect_equal(
    unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Plant),
    c("ble", "poi")
  )
  expect_equal(
    unique(outputs$`IC_Wheat_Pea_2005-2006_N0`$Dominance),
    c("Principal", "Associated")
  )
  expect_equal(unique(outputs$`SC_Pea_2005-2006_N0`$Plant), "poi")
  expect_null(outputs$`SC_Pea_2005-2006_N0`$Dominance)
})
