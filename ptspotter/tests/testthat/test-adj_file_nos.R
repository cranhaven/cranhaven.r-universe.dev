with(globalenv(), {.old_wd <- setwd(tempdir())})

# create infrastructure for testing ---------------------------------------
seq_file_ops(n = 5, target_dir = "munge")

writeLines("start of seq", "munge/01-.R")
writeLines("end of seq", "munge/05-.R")

start_count <- length(list.files("munge"))
munge_nums <- as.numeric(str_extract(list.files("munge"), "^[0-9]."))
adj_file_nos(target = 1, directory = "munge")
end_count <- length(list.files("munge"))

# create mixed folder -----------------------------------------------------
dir.create("mixed_folder")
seq_file_ops(n = 5, target_dir = "mixed_folder")
mixed_nums <- as.numeric(str_extract(list.files("mixed_folder"),
                                              "^[0-9]."))
file.create("mixed_folder/non_numbered.R")

writeLines("start of seq", "mixed_folder/01-.R")
writeLines("end of seq", "mixed_folder/05-.R")
writeLines("not in seq", "mixed_folder/non_numbered.R")

mixed_start_count <- length(list.files("mixed_folder"))
adj_file_nos(target = 1, directory = "mixed_folder")
mixed_end_count <- length(list.files("mixed_folder"))

# down --------------------------------------------------------------------
dir.create("action_down")
seq_file_ops(c(5:10), target_dir = "action_down")

writeLines("start of seq", "action_down/05-.R")
writeLines("end of seq", "action_down/10-.R")

down_nums <- as.numeric(str_extract(list.files("action_down"),
                                             "^[0-9]."))
adj_file_nos(target = 5, directory = "action_down", action = "down")

# part increment ----------------------------------------------------------
dir.create("part_inc")
seq_file_ops(c(1:10), target_dir = "part_inc")

writeLines("start of seq", "part_inc/01-.R")
writeLines("end of seq", "part_inc/10-.R")
writeLines("start of inc", "part_inc/06-.R")

part_inc_names <- list.files("part_inc")
part_inc_nums <- as.numeric(str_extract(part_inc_names, "^[0-9]."))
adj_file_nos(target = 6, directory = "part_inc")

# part decrement ----------------------------------------------------------
dir.create("part_dec")
seq_file_ops(c(1:5, 7:11), target_dir = "part_dec")

writeLines("start of seq", "part_dec/01-.R")
writeLines("end of seq", "part_dec/11-.R")
writeLines("start of dec", "part_dec/07-.R")

part_dec_names <- list.files("part_dec")
part_dec_nums <- as.numeric(str_extract(part_dec_names, "^[0-9]."))
adj_file_nos(target = 7, directory = "part_dec", action = "down")

# tests -------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# file counts -------------------------------------------------------------

test_that("input file count matches output", {
  expect_identical(start_count, end_count)

  expect_identical(mixed_start_count, mixed_end_count)

  expect_identical(length(down_nums), length(list.files("action_down")))

  expect_identical(length(part_inc_nums), length(list.files("part_inc")))

  expect_identical(length(part_dec_nums), length(list.files("part_dec")))

})

# duplicated files --------------------------------------------------------

test_that("filenames are unique", {
  expect_false(object = any(duplicated(list.files("munge"))))

  expect_false(object = any(duplicated(list.files("mixed_folder"))))

  expect_false(object = any(duplicated(list.files("action_down"))))

  expect_false(object = any(duplicated(list.files("part_inc"))))

  expect_false(object = any(duplicated(list.files("part_dec"))))

          })

# recurring fullstops -----------------------------------------------------

test_that("there are no double dots in filenames", {
  expect_false(any(grepl(pattern = "\\.{2,}", x = list.files("munge"))))

  expect_false(any(grepl(pattern = "\\.{2,}", x = list.files("mixed_folder"))))

  expect_false(any(grepl(pattern = "\\.{2,}", x = list.files("action_down"))))

  expect_false(any(grepl(pattern = "\\.{2,}", x = list.files("part_inc"))))

  expect_false(any(grepl(pattern = "\\.{2,}", x = list.files("part_dec"))))

          })

# action == "down" --------------------------------------------------------
test_that("all file numbers are decreased",
          expect_true(
            all(
              as.numeric(str_extract(
                list.files("action_down"), "^[0-9].")) < down_nums)
            )
          )

# step --------------------------------------------------------------------
test_that("step argument works", {
          expect_true(
            all(as.numeric(str_extract(
              list.files("munge"), "^[0-9].")) - munge_nums == 1)
          )
          expect_true(
            all(as.numeric(str_extract(
              list.files(
                "mixed_folder", pattern = "^[0-9]."),
              "^[0-9].")) - mixed_nums == 1)
          )
          expect_true(
            all(as.numeric(stringr::str_extract(
              list.files("action_down"), "^[0-9].")) - down_nums == -1)
            )
          })

# target ------------------------------------------------------------------
test_that("files lower than assigned value are preserved", {
  expect_identical(list.files("part_inc")[1:5], part_inc_names[1:5])

  expect_identical(list.files("part_dec")[1:5], part_dec_names[1:5])

  })

test_that("files >= target are adjusted", {
  expect_true(
    all(
      as.numeric(str_extract(
        list.files("part_inc")[6:10], "^[0-9].")) - part_inc_nums[6:10] == 1)
    )

  expect_true(
    all(
      as.numeric(str_extract(
        list.files("part_dec")[6:10], "^[0-9].")) - part_dec_nums[6:10] == -1)
    )

          })

# expect message ----------------------------------------------------------
test_that("func produces message on success",
          expect_message(
            adj_file_nos(1, directory = "munge"), "5 Filenames adjusted."
          )
)

# script content ----------------------------------------------------------

test_that("file contents are incremented as expected", {
  expect_identical(readLines("munge/03-.R"), "start of seq")
  expect_identical(readLines("munge/07-.R"), "end of seq")

  expect_identical(readLines("mixed_folder/02-.R"), "start of seq")
  expect_identical(readLines("mixed_folder/06-.R"), "end of seq")
  expect_identical(readLines("mixed_folder/non_numbered.R"), "not in seq")

  expect_identical(readLines("action_down/04-.R"), "start of seq")
  expect_identical(readLines("action_down/09-.R"), "end of seq")

  expect_identical(readLines("part_inc/01-.R"), "start of seq")
  expect_identical(readLines("part_inc/11-.R"), "end of seq")
  expect_identical(readLines("part_inc/07-.R"), "start of inc")

  expect_identical(readLines("part_dec/01-.R"), "start of seq")
  expect_identical(readLines("part_dec/10-.R"), "end of seq")
  expect_identical(readLines("part_dec/06-.R"), "start of dec")

  })


# errors ------------------------------------------------------------------

test_that("func errors as expected", {
  expect_error(adj_file_nos(c(1, 2), "munge"),
               "Please use single digits for `target` only.")
  expect_error(adj_file_nos(1),
               "invalid 'path' argument")

})

with(globalenv(), {setwd(.old_wd)})
