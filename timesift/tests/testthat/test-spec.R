spec_section <- function() {
  path <- system.file("spec", "representation.md", package = "timesift")
  lines <- readLines(path, encoding = "UTF-8")
  lines[seq(grep("^## What each language carries", lines), length(lines))]
}

spec_names <- function(lines) {
  hits <- regmatches(lines, gregexpr("`[A-Za-z_][A-Za-z0-9_.]*(\\(\\))?`", lines))
  unique(sub("\\(\\)$", "", gsub("`", "", unlist(hits))))
}

# The first column of the table whose header is `header`, as the bare names it carries. A name
# written with an argument, such as `elasticnet(s =)`, names an argument rather than an export
# and is left out.
spec_table_names <- function(lines, header, column = 1L) {
  start <- grep(header, lines, fixed = TRUE)
  stopifnot(length(start) == 1L)
  rows <- character()
  for (i in seq(start + 2L, length(lines))) {
    if (!startsWith(lines[i], "|")) break
    cells <- strsplit(sub("^\\|", "", lines[i]), "|", fixed = TRUE)[[1L]]
    rows <- c(rows, cells[column])
  }
  spec_names(rows)
}

r_exports <- function() {
  ns <- readLines(system.file("NAMESPACE", package = "timesift"))
  sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE))
}

test_that("every export is recorded in the spec's account of what each language carries", {
  lines <- spec_section()
  missing <- setdiff(r_exports(), spec_names(lines))
  expect_true(!length(missing),
              info = paste("exported but not in the spec:", paste(missing, collapse = ", ")))
})

test_that("a name the spec records on both sides is exported here", {
  lines <- spec_section()
  shared <- spec_table_names(lines, "| concept | the name, on both sides |", column = 2L)
  shared <- shared[shared != "n_inner"]
  missing <- setdiff(shared, r_exports())
  expect_true(!length(missing),
              info = paste("recorded on both sides but not exported:",
                            paste(missing, collapse = ", ")))
})

test_that("a name the spec records as Python only is not an export here", {
  lines <- spec_section()
  python_only <- spec_table_names(lines, "| in Python only | what it is |")
  leaked <- intersect(python_only, r_exports())
  expect_true(!length(leaked),
              info = paste("recorded as Python only but exported:", paste(leaked, collapse = ", ")))
  r_only <- spec_table_names(lines, "| in R only | why |")
  r_only <- setdiff(r_only, "plot")
  missing <- setdiff(r_only, r_exports())
  expect_true(!length(missing),
              info = paste("recorded as R only but not exported:", paste(missing, collapse = ", ")))
})
