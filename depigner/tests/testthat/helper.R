# ATTRIBUTION: the following code is a slight adaptation from the
# r-lib/usethis corresponding one:
# https://github.com/r-lib/usethis/blob/master/tests/testthat/helper.R


## attempt to activate a project, which is nice during development
tryCatch(usethis::proj_set("."), error = function(e) NULL)

## If session temp directory appears to be, or be within, a project, there
## will be large scale, spurious test failures.
## The IDE sometimes leaves .Rproj files behind in session temp directory or
## one of its parents.
## Delete such files manually.
session_temp_proj <- proj_find(fs::path_temp())

if (!is.null(session_temp_proj)) {
  rproj_files <- fs::dir_ls(session_temp_proj, glob = "*.Rproj")
  ui_line(c(
    "Rproj file(s) found at or above session temp dir:",
    paste0("* ", rproj_files),
    "Expect this to cause spurious test failures."
  ))
}

scoped_temporary_project <- function(dir = fs::file_temp(pattern = "prj"),
                                     env = parent.frame(),
                                     rstudio = FALSE) {
  scoped_temporary_thing(dir, env, rstudio, "project")
}


scoped_temporary_package <- function(dir = fs::file_temp(pattern = "aaa"),
                                     env = parent.frame(),
                                     rstudio = FALSE) {
  scoped_temporary_thing(dir, env, rstudio, "package")
}



scoped_temporary_thing <- function(dir = fs::file_temp(pattern = "aaa"),
                                   env = parent.frame(),
                                   rstudio = FALSE,
                                   thing = c("package", "project")) {
  thing <- match.arg(thing)
  if (fs::dir_exists(dir)) {
    usethis::ui_stop("Target {ui_code('dir')} {ui_path(dir)} already exists.")
  }

  old_project <- usethis:::proj_get_()
  old_wd <- getwd()

  withr::defer({
    usethis::ui_done("Deleting temporary project: {ui_path(dir)}")
    fs::dir_delete(dir)
  }, envir = env)

  usethis::ui_silence(
    switch(
      thing,
      package = usethis::create_package(
        dir, rstudio = rstudio, open = FALSE, check_name = FALSE),
      project = usethis::create_project(
        dir, rstudio = rstudio, open = FALSE)
    )
  )

  withr::defer(usethis::proj_set(
    old_project, force = TRUE), envir = env)
  usethis::proj_set(dir)

  withr::defer({
    usethis::ui_done(
      "Restoring original working directory: {ui_path(old_wd)}")
    setwd(old_wd)
  }, envir = env)
  setwd(dir)

  invisible(dir)
}



expect_usethis_error <- function(...) {
  testthat::expect_error(..., class = "usethis_error")
}
