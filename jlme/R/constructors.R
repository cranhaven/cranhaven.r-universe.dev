matrix_to_literal <- function(x, fractions = TRUE) {
  if (fractions) x <- MASS::fractions(x)
  x <- as.character(x)
  x[] <- apply(x, 2, function(col) sprintf(paste0("%", max(nchar(col)), "s"), col))
  flat <- paste(apply(x, 1, paste, collapse = " "), collapse = ";\n")
  sprintf("[\n%s\n]", flat)
}

vec_to_literal <- function(x) {
  if (is.null(x)) return(x)
  flat <- paste(sprintf('"%s"', x), collapse = ", ")
  sprintf("[%s]", flat)
}

as_julia_contrasts <- function(fct, nm = "COLUMN") {
  stopifnot(is.factor(fct))
  .contrasts <- contrasts(fct)
  .hypotheses <- matrix_to_literal(MASS::ginv(.contrasts))
  .levels <- vec_to_literal(levels(fct))
  .labels <- vec_to_literal(colnames(.contrasts)) %||%
    vec_to_literal(levels(fct)[-1])
  sprintf("
    :%1$s => HypothesisCoding(
      %2$s;
      levels=%3$s,
      labels=%4$s
    )
  ", nm, .hypotheses, .levels, .labels)
}

construct_contrasts <- function(df, cols = NULL, format = FALSE) {
  if (is.null(cols)) {
    cols <- colnames(Filter(is_fct_custom_contrast, df))
  }
  if (length(cols) == 0) return(NULL)
  all_contrasts <- vapply(cols, function(col) {
    as_julia_contrasts(df[[col]], col)
  }, character(1))
  dict <- sprintf("Dict(\n%s\n)", paste(all_contrasts, collapse = ", "))
  if (format) {
    jl_format(dict)
  } else {
    dict
  }
}

list2ntuple <- function(x) {
  stopifnot(is.list(x), all(nzchar(names(x))))
  JuliaConnectoR::juliaLet("NamedTuple(x.namedelements)", x = x)
}
