capture_extra_params <- function(...) {
  dots <- rlang::list2(...)
  if (length(dots) > 0 && !rlang::is_named(dots))
    cli::cli_abort("All extra-arguments should be named arguments.")
  dots
}

extract_class_name <- function(py_class) {
  nm <- py_class |>
    format() |>
    strsplit("\\.")
  nm <- nm[[1]]
  nm <- nm[length(nm)]
  substr(nm, 1, nchar(nm) - 1)
}

get_r6_class <- function(cls) {
  if (inherits(cls, "R6")) return(cls)
  if (!inherits(cls, "python.builtin.object"))
    cli::cli_abort("The input object should be a Python class.")
  out_cls <- get(extract_class_name(cls))
  out_cls$new(py_cls = cls)
}
