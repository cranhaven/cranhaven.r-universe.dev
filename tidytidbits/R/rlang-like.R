
#' Execute code after tidy evaluation
#'
#' This function takes R code as arguments and executes this code in the calling environment.
#' All quoted variables (using rlang's quasiquotation, !! or !!!) will be unquoted prior to evaluation.
#' This results in executed in code in which the variable is replaced verbatim by its value,
#' as if you had typed the variable's value.
#' This is particularly useful for functions using base R's substitute() approach, such
#' as functions taking formulas, and you have built the formula dynamically.
#' It is unnecessary for all functions based on tidy_eval (dplyr).
#'
#' @param ... R code snippets
#'
#' @return The value of the last evaluated expression.
#' @export
#'
#' @examples
#' library(rlang)
#' # Note that evaluation takes place in the calling environment!
#' l <- quo(l <- 1) # l is a quosure in our env
#' eval_unquoted(!!l)
#' l == 1 # TRUE: l is now a vector
eval_unquoted <- function(...)
{
  snippets <- quos(...)
  for (snippet in snippets)
  {
    expr <- quo_squash(snippet)
    env <- get_env(snippet)
    res <- rlang::eval_bare(expr, env)
  }
  res
}

#' Syntactically safe names
#'
#' Makes the names syntactically safe by wrapping them in `` if necessary
#'
#' @param expr_strings Strings to convert to syntactically safe form
#'
#' @return Strings converted to syntactically safe form
#' @export
syntactically_safe <- function(expr_strings)
{
  map_chr(expr_strings,
          function(expr_string)
          {
            if (make.names(expr_string) != expr_string)
              str_c("`", expr_string, "`")
            else
              expr_string
          }
  )
}

#' Print deparsed language
#'
#' Prints deparsed R language tree of given expression
#'
#' @param language R language
#'
#' @return Invisible null
#' @export
print_deparsed <- function(language)
{
  language <- quo_squash(enquo(language))
  cat("Language ", as.character(language), " \n")
  if (is_symbol(language))
    return()

  car <- node_car(language)
  car_type <- typeof(car)
  cat(" car ", as.character(car), " ", typeof(car), "\n")

  cdrs <- node_cdr(language)
  for (cdr in cdrs)
    cat(" cdr ", as.character(cdr), " ", typeof(cdr), " \n")
  for (cdr in cdrs)
    if (!is_symbol(cdr)) print_deparsed(!!cdr)
  invisible()
}

# From an expression with symbols and operators, extracts the symbols
#' Extract symbols from an expression of symbols and operators
#'
#' @param expr A language expression
#' @param seps Operators to consider as separators
#'
#' @return A list of all symbols in the expression, as symbol, quosure or text.
#' @export
#'
#' @examples
#' expression_list(a+b+c+d)
expression_list <- function(expr, seps = "+")
{
  expr <- quo_squash(enquo(expr))
  if (is_symbol(expr))
    return(list(expr))

  car <- node_car(expr)
  if (!as.character(car) %in% seps)
    return(list(expr))

  symbols <- list()
  for (l in node_cdr(expr))
  {
    symbols <- c(symbols, expression_list(!!l, seps))
  }
  return(symbols)
}

#' @param env Environment for the created quosure
#' @export
#' @rdname expression_list
quosure_list <- function(expr, seps = "+", env = caller_env())
{
  map(expression_list(!!enquo(expr)), new_quosure, env=env)
}

#' @export
#' @rdname expression_list
symbol_string_list <- function(expr, seps = "+")
{
  map(expression_list(!!enquo(expr)), as.character)
}

# takes a string or a symbol
#' Make quosure from symbol
#'
#' @param x Symbol
#' @param env Environment for the created quosure
#'
#' @return Quosure containing the symbol
#' @export
symbol_as_quosure <- function(x, env = caller_env())
{
  x <- enquo(x)
  new_quosure(sym(quo_name(x)), env = env)
}
