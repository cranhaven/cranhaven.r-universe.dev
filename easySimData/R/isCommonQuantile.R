
# Return TRUE if a common quantile is called.
# Common quantile functions are beta, gamma, binomial, normal, etc. A full list
# can be found by
# ls('package:stats', pattern = '^r')
# Function stops if desired arguments are missing (e.g. size and prob of a
# binomial random variable) so no need to return FALSE.
#
# call: a call specifying the marginal distribution
# distribution_name: can be `norm`, `t`, `f`, `gamma`, etc., i.e.,
#                    remove 'q' from names of quantile functions in `stats`.
#                    For arguments of those functions, please refer to their
#                    R functions (e.g., qnorm, qt, qf, etc.).
isCommonQuantile <- function(call, distribution_name, par_list){

  pattern <- str_glue('^\\s*{distribution_name}\\s*\\(.*\\)\\s*$')
  if(!grepl(pattern, deparse(call), ignore.case = TRUE)){
    return(FALSE)
  }

  isArgumentInCall(par_list, call)

  TRUE

}
