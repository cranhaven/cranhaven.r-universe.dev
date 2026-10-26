# ==============================================================================
# House style, checked against the sources
# ==============================================================================
# A comment says what the code does or why the domain wants it that way. A
# tracker pointer says neither: it is only readable to someone who can reach
# the tracker, it outlives the issue it names, and the fact it stands in for is
# usually one sentence that could have been written down instead. These ran
# ahead of the check, so the check exists to keep them from coming back.
#
# The sources are read from the installed package when there is one and from
# the working tree otherwise, so the test runs under devtools::test() and under
# R CMD check alike. It skips when neither is reachable, which is what happens
# on a build that ships no sources.
# ==============================================================================

.hygiene_source_files <- function() {
  roots <- c(
    testthat::test_path("..", ".."),          # the package root under devtools
    system.file(package = "couplr")           # the installed copy
  )
  for (root in roots) {
    if (!nzchar(root) || !dir.exists(root)) next
    dirs <- file.path(root, c("R", "src", "tests"))
    dirs <- dirs[dir.exists(dirs)]
    if (length(dirs) == 0L) next
    files <- list.files(dirs, pattern = "[.](R|r|cpp|h|hpp)$",
                        recursive = TRUE, full.names = TRUE)
    if (length(files) > 0L) return(files)
  }
  character(0)
}

test_that("no source file points at the issue tracker", {
  files <- .hygiene_source_files()
  skip_if(length(files) == 0L, "no package sources to read")

  # This file names the pattern in order to test for it, so it is not its own
  # counterexample.
  files <- files[basename(files) != "test-source-hygiene.R"]

  pattern <- paste0("gcol33/", "couplr#")
  offenders <- character(0)
  for (f in files) {
    lines <- readLines(f, warn = FALSE)
    hit <- grep(pattern, lines, fixed = TRUE)
    if (length(hit) > 0L) {
      offenders <- c(offenders, sprintf("%s:%d", basename(f), hit))
    }
  }

  expect_equal(
    offenders, character(0),
    info = paste0(
      "A comment names an issue instead of stating what it stands for. ",
      "Write the constraint into the comment, or delete the comment if the ",
      "issue is resolved and the note is stale."
    )
  )
})
