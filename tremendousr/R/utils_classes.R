# Copied from [jdtools](https://jdtools.jdtrat.com)
cls_check <- function(object, expected_class) {

  obj_name <- deparse(substitute(object))
  obj_class <- class(object)

  if (all(obj_class %nin% expected_class)) {
    cli::cli_abort("{.arg {obj_name}} must be {.cls {expected_class}} not of {.cls {obj_class}}.",
                   class = "cls_abort")
  }

  invisible(obj_class)

}

# Copied from [jdtools](https://jdtools.jdtrat.com)
`%nin%` <- function(x, table) {
  !match(x, table, nomatch = 0L) > 0L
}
