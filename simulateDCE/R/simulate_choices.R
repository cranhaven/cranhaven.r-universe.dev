#' Simulate choices based on a data.frame with a design and respondents
#'
#' @param data a dataframe that includes a design repeated for the number of observations
#' @param utility a list with the utility functions, one utility function for each alternatives
#' @param setspp an integer, the number of choice sets per person
#' @param bcoeff List of initial coefficients for the utility function. List content/length can vary based on application. I ideally begins (but does not have to) with b and need be the same as those entered in the utility functions
#' @param decisiongroups A vector showing how decision groups are numerically distributed
#' @param manipulations A variable to alter terms of the utility functions examples may be applying a factor or applying changes to terms selectively for different groups
#' @param preprocess_function = NULL You can supply a function that reads in external data (e.g. GIS coordinates) that will be merged with the simulated dataset. Make sure the the function outputs a data.frame that has a variable called ID which is used for matching.
#' @return a data.frame that includes simulated choices and a design
#' @export
#' @import data.table
#' @examples
#' example_df <- data.frame(
#'   ID = rep(1:100, each = 4),
#'   price = rep(c(10, 10, 20, 20), 100),
#'   quality = rep(c(1, 2, 1, 2), 100)
#' )
#'
#' beta <- list(
#'   bprice   = -0.2,
#'   bquality =  0.8
#' )
#'
#' ut <- list(
#'   u1 = list(
#'     v1 = V.1 ~ bprice * price + bquality * quality,
#'     v2 = V.2 ~ 0
#'   )
#' )
#' simulate_choices(example_df, ut, setspp = 4, bcoeff = beta)
#'
simulate_choices <- function(data, utility, setspp, bcoeff, decisiongroups = c(0, 1), manipulations = list(),  preprocess_function = NULL) { # the part in dataset that needs to be repeated in each run

  if (!is.null(preprocess_function)) {
    if (!is.function(preprocess_function)) {
      stop("`preprocess_function` must be a function.")
    } else {
      # Execute the user-supplied `preprocess_function`
      prepro_data <- preprocess_function()
      if (!is.null(prepro_data) && (!is.data.frame(prepro_data) || !"ID" %in% names(prepro_data))) {
        stop("The output of `preprocess_function` must be a data.frame with a column named 'ID'.")
      }
      message("\n Preprocess function has been executed.\n")
    }
  } else {
    message("\n No preprocess function provided. Proceeding without additional preprocessing.\n")
  }


  # Check 1: user specified multiple decision groups
  if (length(decisiongroups) > 2 && length(utility) != (length(decisiongroups) - 1)) {
    stop(glue::glue(
      "Length of `utility` ({length(utility)}) does not match number of decision groups ",
      "defined by `decisiongroups` ({length(decisiongroups) - 1})."
    ))
  }

  tictoc::tic("whole simulate choices")

  tictoc::tic("assign keys for bcoeff")
  ### unpack the bcoeff list so variables are accessible
  for (key in names(bcoeff)) {
    assign(key, bcoeff[[key]])
  }

  message( utils::capture.output(tictoc::toc(log = FALSE, quiet = TRUE)) )

### new functions to calculate utility
  compile_one <- function(fm) {
    name <- as.character(formula.tools::lhs(fm))
    rhs  <- formula.tools::rhs(fm)
    fn   <- eval(bquote(function(d) with(d, .(rhs))))
    list(name = name, fun = compiler::cmpfun(fn))
  }

  compile_utility_list <- function(u) {
    lapply(u, function(fl) {
      tmp <- lapply(fl, compile_one)
      stats::setNames(lapply(tmp, `[[`, "fun"),
               vapply(tmp, `[[`, "", "name"))
    })
  }




  if (!exists("manipulations")) manipulations <- list() ## If no user input on further data manipulations

  n <- seq_along(1:length(utility[[1]])) # number of utility functions


  message("\n dataset preprossed_data exists: ", exists("prepro_data"), "\n")

  if (exists("prepro_data")) data <- dplyr::left_join(data, prepro_data, by = "ID")

  message("\n decisiongroups exists: ", length(decisiongroups) > 2)

  if (length(decisiongroups) > 2) { ### create a new variable to classify decision groups.

    data <- dplyr::mutate(data, group = as.numeric(cut(dplyr::row_number(),
      breaks = decisiongroups * dplyr::n(),
      labels = seq_along(decisiongroups[-length(decisiongroups)]),
      include.lowest = TRUE
    )))

    message(
      "\nGroup counts:\n",
      paste(utils::capture.output(print(table(data$group))), collapse = "\n")
    )
  } else {
    data$group <- 1
  }

  ### give an error if things did not work out

  if (any(!unique(data$group) %in% seq_along(utility))) {
    stop("Mismatch: data includes group IDs not covered by the utility list.")
  }

  tictoc::tic("user entered manipulations")

  ## Do user entered manipulations to choice set
  data <- data %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(!!!manipulations)
  message( utils::capture.output(tictoc::toc(log = FALSE, quiet = TRUE)) )


  tictoc::tic("for each group calculate utility")


  ufuns <- compile_utility_list(utility)

  dt <- data.table::as.data.table(data)



  all_vars <- unique(unlist(lapply(utility, function(fl) {
    unlist(lapply(fl, function(fm) all.vars(formula.tools::rhs(fm))))
  })))
  sdcols <- intersect(all_vars, names(dt))


  varn <- names(ufuns[[1]])

  dt[
    ,
    (varn) := lapply(ufuns[[.GRP]], function(f) f(.SD)),
    by = group,
    .SDcols = sdcols
  ]





  tictoc::tic("add random component")
  ## add random component and calculate total utility
  data <- dt %>%
    dplyr::ungroup() %>%
    dplyr::rename_with(~ stringr::str_replace_all(., pattern = "\\.", "_"), tidyr::everything()) %>%
    dplyr::mutate(
      dplyr::across(.cols = dplyr::all_of(n), .fns = ~ evd::rgumbel(dplyr::n(), loc = 0, scale = 1), .names = "{'e'}_{n}"),
      dplyr::across(dplyr::starts_with("V_"), .names = "{'U'}_{n}") + dplyr::across(dplyr::starts_with("e_"))
    )  %>%
    as.data.frame() %>%
    dplyr::mutate(CHOICE = max.col(.[, grep("U_", names(.))]))


  message( utils::capture.output(tictoc::toc(log = FALSE, quiet = TRUE)) )

  message( utils::capture.output(tictoc::toc(log = FALSE, quiet = TRUE)) )

  message("\n data has been created \n")

  message(
    "\nFirst few observations of the dataset\n",
    paste(utils::capture.output(utils::head(data)), collapse = "\n"),
    "\n\n"
  )

  return(data)
}
