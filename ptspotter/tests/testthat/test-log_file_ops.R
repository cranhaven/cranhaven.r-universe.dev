with(globalenv(), {.old_wd <- setwd(tempdir())})

# fileops -----------------------------------------------------------------
log_file_ops(dir_path = "log_testing", logfile_nm = "test")


# pre-existing architecture -----------------------------------------------

dir.create("old_log_dir")
invisible(file.create("old_log_dir/old_logfile.txt"))

# pre-existing dir --------------------------------------------------------
dir.create("just_log_dir")

# tests -------------------------------------------------------------------
# -------------------------------------------------------------------------

# dir_path ----------------------------------------------------------------
test_that("func creates logging dir",
          expect_true(dir.exists("log_testing"))
          )

# logfile_nm --------------------------------------------------------------
test_that("func creates logfile",
          expect_true(file.exists("log_testing/test.txt"))
)

# logfile pre-exists ------------------------------------------------------
test_that("func errors if logfile_nm pre-exists",
          expect_error(log_file_ops(dir_path = "old_log_dir",
                                    logfile_nm = "old_logfile"),
                       "Logfile with name matching")
          )

# log dir pre-exists ------------------------------------------------------
test_that("func produces message on success",
          expect_message(
            log_file_ops(dir_path = "just_log_dir", logfile_nm = "testing"),
            "Logfile successfully created at"
            )
          )

with(globalenv(), {setwd(.old_wd)})
