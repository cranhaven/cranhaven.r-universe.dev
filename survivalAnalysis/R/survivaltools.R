
# Extracts the p-value from a survdiff object
pValueOfSurvDiff <- function(diff)
{
  possibly(extract_p_value_from_surv_diff, otherwise = NA, quiet = TRUE)(diff)
}

extract_p_value_from_surv_diff <- function(diff)
{
  # inspired by print.survdiff from survival package
  if (length(diff$n)==1)
  {
    pchisq(diff$chisq, 1, lower.tail = FALSE)
  }
  else
  {
    if (is.matrix(diff$obs))
    {
      n_exp <- apply(diff$exp, 1, sum)
    }
    else
    {
      n_exp <- diff$exp
    }
    df <- (sum(1*(n_exp>0))) -1
    pchisq(diff$chisq, df, lower.tail = FALSE)
  }
}

survivalFormatPValue <- function(p,
                                 with_prefix = TRUE,
                                 psprintfFormat = "%.3f",
                                 p.lessthan.cutoff = 0.001,
                                 NA_string = "NA",
                                 pad_for_less_than=FALSE)
{
  if (length(p) > 1)
  {
    return(map_chr(p, survivalFormatPValue, with_prefix, psprintfFormat, p.lessthan.cutoff))
  }
  if (is.na(p))
  {
    return(NA_string)
  }
  if (p < p.lessthan.cutoff)
  {

    return(sprintf(str_c(ifelse(with_prefix, "p < ", "<"),
                         "%.",
                         abs(log(p.lessthan.cutoff, base = 10)),
                         "f"),
                   p.lessthan.cutoff))
  }
  else
  {
    return(sprintf(str_c(ifelse(with_prefix, "p = ",
                                ifelse(pad_for_less_than, " ", "")),
                         psprintfFormat),
                   p))
  }
}

.survivalBuildFormulas <- function(survivalFields, factorId)
{
  survFormulaPart <- str_c("Surv(",
                           syntactically_safe(survivalFields[[1]]),
                           ",",
                           syntactically_safe(survivalFields[[2]]),
                           ") ~ ")
  return(list(
    "formula" = as.formula(str_c(survFormulaPart,
                                 ifelse(factorId == "1" || factorId == "",
                                        "1",
                                        syntactically_safe(factorId)))),
    "formulaOverall" = as.formula(str_c(survFormulaPart,
                                        "1"))
  ))
}

.survivalResultArguments <- function(...)
{
  args <- dots_list(...)

  # take care for the case where we have a list of survival results
  # and the user forgot to splice explicitly
  if (has_length(args, 1) && is_list(args[[1]]) &&
      has_length(args[[1]]) && inherits(args[[1]][[1]], "SurvivalAnalysisResult"))
    args <- args[[1]]

  # Filter out invalid arguments. No need to cry, just give a warning.
  invalid_args <- map_lgl(args, invalid)
  if (any(invalid_args))
  {
    warning(str_c("Encountered ", sum(invalid_args), " invalid arguments among the ", length(args), " survival result arguments"))
    args <- args[!invalid_args]
  }

  # Check that all arguments are actually survival results
  free_args_correct_class <- map_lgl(args, ~inherits(., "SurvivalAnalysisResult"))
  if (!all(free_args_correct_class))
  {
    # If the user mistyped a named argument, it will appear in this list and makes for a hard-to-spot error.
    # Give explicit warning in this case.
    wrong_args <- args[!free_args_correct_class]
    named_wrong_args <- wrong_args[have_name(wrong_args)]
    unnamed_wrong_args <- wrong_args[!have_name(wrong_args)]
    message <- "There are arguments which do not have class SurvivalAnalysisResult. "
    if (has_length(named_wrong_args))
      message <- str_c(message,
                       "Maybe you mistyped an argument name? Found these argument names: ",
                       str_c(names(named_wrong_args), collapse = ", "))
    if (has_length(unnamed_wrong_args))
      message <- str_c(message, "Unnamed args with the following classes: ",
                       str_c(map_chr(unnamed_wrong_args, class), collapse = ", "))
    stop(message)
  }

  return(args)
}

quo_category <- function(q)
{
  # quo_is...(quo(...))
  #       lang	symbolic symbol	null
  # 3+z()	T	    T	       F	    F
  # c()	  T	    T	       F	    F
  # c	    F	    T	       T	    F
  # 5	    F	    F	       F	    F
  # "a"	  F	    F	       F	    F
  # NULL	F	    F	       F	    T

  if (!is_quosure(q))
  {
    if (is_null(q))
      "null"
    else
      "value"
  }
  else if (quo_is_symbolic(q))
  {
    if (quo_is_call(q))
      "language"
    else
      "symbol"
  }
  else
  {
    if (quo_is_null(q))
      "null"
    else if (quo_is_missing(q))
      "missing"
    else
      "literal"
  }
}

# data: a data frame
# var: a quosure referring to a column in data, or a quosure of evaluable language based on data,
#      or a non-quosure referencing a data column (by name or position)
# make_factor: should become a factor
.build_column <- function(data, var, make_factor)
{
  result <- list()
  original_name <- quo_name(var)
  colname <- tidy_names(original_name, syntactic = TRUE, quiet = TRUE)
  category <- quo_category(var)
  if (category == "language" || category == "symbol")
  {
    data %>%
      transmute(!!colname := !!var) ->
    result[[original_name]]
  }
  else if (category == "literal" || category == "value")
  {
    data %>%
      select(!!colname := !!var) ->
    result[[original_name]]
  }

  if (make_factor && has_length(result))
    result[[1]] %<>% mutate_all(factor)

  result
}

.build_columns <- function(data, vars)
{
  map(vars, ~.build_column(data, ., F)) %>%
    flatten
}

.build_survival_columns <- function(data, vars)
{
  if (!has_length(vars, 2))
    stop("Need survival fields as a vector of length 2, be it characters, numbers, symbols in vars() or language in vars()")
  .build_columns(data, vars)
}
