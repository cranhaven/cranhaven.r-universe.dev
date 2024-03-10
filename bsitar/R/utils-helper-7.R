
#' An internal function to prepare priors
#'
#' For \code{univariate_by} and \code{multivariate} models (see
#' [bsitar::bsitar()]) each argument is automatically matched with the sub
#' model.
#'
#' @param prior_argument A list containing the prior arguments specified in the
#'   [bsitar::bsitar()] function and then passed from the
#'   [bsitar::set_priors_initials()] function to the \code{prepare_priors}.
#'
#' @param prior_data An optional argument (a named list) specified in the
#'   [bsitar::bsitar()] function and then passed from the
#'   [bsitar::set_priors_initials()] function to the \code{prepare_priors}. The
#'   \code{prior_data} used to pass value for priors. See [bsitar::bsitar()]
#'   function, \code{prior_data} for details.
#'
#' @param prior_data_internal An internal argument (as named list) specified in
#'   the [bsitar::bsitar()] function and then passed from the
#'   [bsitar::set_priors_initials()] function to the \code{prepare_priors}.
#'
#' @param prior_internal_args An internal argument list that is passed from the
#'   [bsitar::set_priors_initials()] function to the \code{set_priors_initials}
#'   and is used in setting the priors.
#'
#' @param init_arguments A list containing the initial arguments specified in
#'   the [bsitar::bsitar()] function and then passed from the
#'   [bsitar::set_priors_initials()] function to the \code{prepare_priors}.
#'
#' @param init_data An optional argument (as named list) specified in the
#'   [bsitar::bsitar()] function and then passed from the
#'   [bsitar::set_priors_initials()] function to the \code{prepare_priors}. The
#'   \code{init_data} is used for setting the initials.
#'
#' @param init_data_internal An internal argument (as named list) specified in
#'   the [bsitar::bsitar()] function and then passed from the
#'   [bsitar::set_priors_initials()] function to the \code{prepare_priors}.
#'
#' @param init_args_internal An internal argument list that is passed from the
#'   [bsitar::set_priors_initials()] function to the \code{set_priors_initials}
#'   and is used in setting the initials.
#'
#' @return An object of class \code{brmsprior}. See \code{brmsprior} function
#'   for more details.
#'
#' @author Satpal Sandhu  \email{satpal.sandhu@bristol.ac.uk}
#' 
#' @keywords internal
#' @noRd
#' 
prepare_priors <- function(prior_argument,
                           prior_data,
                           prior_data_internal,
                           prior_internal_args,
                           init_arguments,
                           init_data,
                           init_data_internal,
                           init_args_internal) {
  
  
  # Initiate non formalArgs()
  nlpar <- NULL;
  verbose <- NULL;
  cov_nlpar <- NULL;
  dpar <- NULL;
  cov_dpar <- NULL;
  fixedsi <- NULL;
  a_form_0 <- NULL;
  resp_ <- NULL;
  nrep_of_parms <- NULL;
  b_form_0 <- NULL;
  c_form_0 <- NULL;
  d_form_0 <- NULL;
  e_form_0 <- NULL;
  f_form_0 <- NULL;
  g_form_0 <- NULL;
  h_form_0 <- NULL;
  i_form_0 <- NULL;
  s_form_0 <- NULL;
  s_form_0_gr <- NULL;
  sncov <- NULL;
  ancov <- NULL;
  bncov <- NULL;
  cncov <- NULL;
  dncov <- NULL;
  encov <- NULL;
  fncov <- NULL;
  gncov <- NULL;
  hncov <- NULL;
  incov <- NULL;
  sncov <- NULL;
  sigma_dpar <- NULL;
  sigma_form_0 <- NULL;
  cov_sigma_dpar <- NULL;
  sigmancov <- NULL;
  randomsi <- NULL;
  ancov_gr <- NULL;
  bncov_gr <- NULL;
  cncov_gr <- NULL;
  dncov_gr <- NULL;
  encov_gr <- NULL;
  fncov_gr <- NULL;
  gncov_gr <- NULL;
  hncov_gr <- NULL;
  incov_gr <- NULL;
  sncov_gr <- NULL;
  sigmancov_gr <- NULL;
  dparncov <- NULL;
  setautocorr <- NULL;
  group <- NULL;
  normalize <- NULL;
  initsi <- NULL;
  
  eout <- list2env(prior_internal_args)
  for (eoutii in names(eout)) {
    assign(eoutii, eout[[eoutii]])
  }
  
  
  eout <- list2env(init_arguments)
  for (eoutii in names(eout)) {
    assign(eoutii, eout[[eoutii]])
  }
  
  
  empty_sufx <- NULL
  
  # set to 'character(0)' to avoid overhead of reduce sum
  # set to '' to mimic default behavious whihc adds pll_args for data block
  # if change_default_data_pll_args <- FALSE, then nothing changed, i.e., 
  # default brms behavious
  
  change_default_data_pll_args <- TRUE
  set_data_pll_args <- 'character(0)'
  
  stanvars_data <- list()
  
  eout <- list2env(prior_data_internal)
  for (eoutii in names(eout)) {
    assign(eoutii, eout[[eoutii]])
  }
  
  # evaluate user defined prior_data
  if (!is.null(prior_data[[1]])) {
    eout <- list2env(prior_data)
    for (eoutii in names(eout)) {
      assign(eoutii, eout[[eoutii]])
    }
  }
  
  
  # get elements
  get_within_fist_last_paranthesese <- function(x__) {
    x__ <- sub('\\(', '[', x__)
    x__ <- sub("\\)([^)]*)$", "]\\1", x__)
    x__ <-
      gsub("[\\[\\]]", "", regmatches(x__, gregexpr("\\[.*?\\]", x__))[[1]])
    x__ <- gsub("\\[|\\]", "", x__)
    x__
  }
  
  gsub_comma_within_paranthesese <-
    function(x__, replace_comma_by) {
      tt <-
        gsub("[\\(\\)]", "", regmatches(x__, gregexpr("\\(.*?\\)", x__))[[1]])
      tt2 <- gsub(",", replace_comma_by, tt, fixed = T)
      j <- 0
      for (i in tt) {
        j <- j + 1
        x__ <- gsub(tt[j], tt2[j], x__, fixed = T)
      }
      x__
    }
  
  sep_indicator <- "_"
  p_str_in <- gsub("\\s", "", prior_str_arg)
  splitmvar <- p_str_in
  splitmvar <- gsub("\\s", "", splitmvar)
  splitmvar <- paste(splitmvar, collapse = "")
  splitmvar_w <- get_within_fist_last_paranthesese(splitmvar)
  
  # This for flat prior when no () i.e, flat instead of flat()
  if (identical(splitmvar_w, character(0))) {
    splitmvar_w <- ""
  }
  
  
  splitmvar_w <-
    gsub_comma_within_paranthesese(splitmvar_w, "_comma_")
  splitmvar_w2 <- strsplit(splitmvar_w, ",")[[1]]
  splitmvar_w2 <- gsub("_comma_" , ",", splitmvar_w2)
  splitmvar_w3 <- sub("=[^=]+$", "", splitmvar_w2)
  
  ept <- function(x)
    eval(parse(text = x), envir = parent.frame())
  
  
  # extract sethp distribution
  get_sethp_arg <- splitmvar_w2[grepl("^sethp=", splitmvar_w2)]
  sethp_tt <- sub('.*=', '', get_sethp_arg)
  sethp_tt <- paste0("'", sethp_tt, "'")
  get_sethp_arg <-
    paste0(sub("=[^=]+$", "=", get_sethp_arg),  sethp_tt)
  sethp_dist <- ept(get_sethp_arg)
  sethp_dist <- gsub("\"" , "", sethp_dist)
  
  
  if (!is.null(sethp_dist) &
      (sethp_dist == "T" | sethp_dist == "TRUE")) {
    sethp_dist <- "normal"
    prior_str_arg <-
      gsub(paste0("=", sethp_dist),
           paste0("=", "TRUE"),
           prior_str_arg)
    splitmvar_w2 <- gsub(sethp_dist, "TRUE", splitmvar_w2)
  }
  
  
  if (!is.null(sethp_dist) &
      (
        sethp_dist != "''" &
        sethp_dist != "NA" &
        sethp_dist != "F" &
        sethp_dist != "FALSE" &
        sethp_dist != ""
      )) {
    if (sethp_dist == "normal" |
        sethp_dist == "cauchy" |
        sethp_dist == "student_t" |
        sethp_dist == "student_nu" |
        sethp_dist == "exponential") {
      prior_str_arg <-
        gsub(paste0("=", sethp_dist),
             paste0("=", "TRUE"),
             prior_str_arg)
      splitmvar_w2 <-
        gsub(sethp_dist, "TRUE", splitmvar_w2)
    } else {
      stop(
        "Hierarchial distribution (i.e,, sethp = distribution) can only",
        "\n",
        " be normal, cauchy, student_nu, student_t, exponential",
        "\n",
        " if sethp = NA, then priors are set directly and not as hierarchial",
        "\n",
        " if sethp = TRUE, then priors are set as hierarchial",
        "\n",
        " with default normal distribution"
      )
    }
  }
  
  
  dist <- strsplit(gsub("\\s", "", prior_str_arg), "\\(")[[1]][1]
  
  if(dist != 'flat') {
    
    add_missing_mandate_names <- function(x, testi, testi2) {
      j = 0
      out_c <- c()
      for (i in 1:length(x)) {
        j <- j + 1
        if (!x[j] %in% testi[j]) {
          out <- paste0(x[j], "=", testi[j])
        } else {
          out <- testi2[j]
        }
        out_c <- c(out_c, out)
      }
      lengthii <- length(testi)
      testi3 <- testi2[length(out_c) + 1:length(testi2)]
      out_c <- c(out_c, testi3)
      out_c <- head(out_c, lengthii)
      out_c
    }
    
    
    error_handle1 <- function(i, dist, testi) {
      if (!i %in% testi) {
        stop(
          "for ",
          dist,
          " distribution,",
          " parameter '",
          i,
          "' is missing",
          "\n  Please specify ",
          dist,
          " distribution as: ",
          paste0(dist, "(", paste(set_str_names, collapse = ", "), ")")
        )
      }
    }
    
    
    if (dist == "normal" |
        dist == "cauchy" |
        dist == "lognormal") {
      set_str_names <- c("location", "scale")
      if (length(splitmvar_w2) < length(set_str_names))
        stop(
          "please sepecify minimum required ",
          length(set_str_names),
          " parameters, i.e., ",
          paste(set_str_names, collapse = ", ")
        )
      splitmvar_w2 <-
        add_missing_mandate_names(set_str_names, splitmvar_w3, splitmvar_w2)
      splitmvar_w3 <- sub("=[^=]+$", "", splitmvar_w2)
      for (i in set_str_names)
        error_handle1(i, dist, splitmvar_w3)
    }
    
    if (dist == "gamma") {
      set_str_names <- c("shape", "scale")
      if (length(splitmvar_w2) < length(set_str_names))
        stop(
          "please sepecify minimum required ",
          length(set_str_names),
          " parameters, i.e., ",
          paste(set_str_names, collapse = ", ")
        )
      splitmvar_w2 <-
        add_missing_mandate_names(set_str_names, splitmvar_w3, splitmvar_w2)
      splitmvar_w3 <- sub("=[^=]+$", "", splitmvar_w2)
      for (i in set_str_names)
        error_handle1(i, dist, splitmvar_w3)
    }
    
    
    if (dist == "inv_gamma") {
      set_str_names <- c("shape", "scale")
      if (length(splitmvar_w2) < length(set_str_names))
        stop(
          "please sepecify minimum required ",
          length(set_str_names),
          " parameters, i.e., ",
          paste(set_str_names, collapse = ", ")
        )
      splitmvar_w2 <-
        add_missing_mandate_names(set_str_names, splitmvar_w3, splitmvar_w2)
      splitmvar_w3 <- sub("=[^=]+$", "", splitmvar_w2)
      for (i in set_str_names)
        error_handle1(i, dist, splitmvar_w3)
    }
    
    if (dist == "uniform") {
      set_str_names <- c("lower", "upper")
      if (length(splitmvar_w2) < length(set_str_names))
        stop(
          "please sepecify minimum required ",
          length(set_str_names),
          " parameters, i.e., ",
          paste(set_str_names, collapse = ", ")
        )
      splitmvar_w2 <-
        add_missing_mandate_names(set_str_names, splitmvar_w3, splitmvar_w2)
      splitmvar_w3 <- sub("=[^=]+$", "", splitmvar_w2)
      for (i in set_str_names)
        error_handle1(i, dist, splitmvar_w3)
    }
    
    if (dist == "exponential") {
      set_str_names <- c("rate")
      if (length(splitmvar_w2) < length(set_str_names))
        stop(
          "please sepecify minimum required ",
          length(set_str_names),
          " parameters, i.e., ",
          paste(set_str_names, collapse = ", ")
        )
      splitmvar_w2 <-
        add_missing_mandate_names(set_str_names, splitmvar_w3, splitmvar_w2)
      splitmvar_w3 <- sub("=[^=]+$", "", splitmvar_w2)
      for (i in set_str_names)
        error_handle1(i, dist, splitmvar_w3)
    }
    
    if (dist == "student_t") {
      set_str_names <- c("df", "location", "scale")
      if (length(splitmvar_w2) < length(set_str_names))
        stop(
          "please sepecify minimum required ",
          length(set_str_names),
          " parameters, i.e., ",
          paste(set_str_names, collapse = ", ")
        )
      splitmvar_w2 <-
        add_missing_mandate_names(set_str_names, splitmvar_w3, splitmvar_w2)
      splitmvar_w3 <- sub("=[^=]+$", "", splitmvar_w2)
      for (i in set_str_names)
        error_handle1(i, dist, splitmvar_w3)
    }
    
    if (dist == "student_nu") {
      set_str_names <- c("nu_shape", "nu_scale", "location", "scale")
      if (length(splitmvar_w2) < length(set_str_names))
        stop(
          "please sepecify minimum required ",
          length(set_str_names),
          " parameters, i.e., ",
          paste(set_str_names, collapse = ", ")
        )
      splitmvar_w2 <-
        add_missing_mandate_names(set_str_names, splitmvar_w3, splitmvar_w2)
      splitmvar_w3 <- sub("=[^=]+$", "", splitmvar_w2)
      for (i in set_str_names)
        error_handle1(i, dist, splitmvar_w3)
    }
    if (dist == "lkj") {
      set_str_names <- c("eta")
      if (length(splitmvar_w2) < length(set_str_names))
        stop(
          "please sepecify minimum required ",
          length(set_str_names),
          " parameters, i.e., ",
          paste(set_str_names, collapse = ", ")
        )
      splitmvar_w2 <-
        add_missing_mandate_names(set_str_names, splitmvar_w3, splitmvar_w2)
      splitmvar_w3 <- sub("=[^=]+$", "", splitmvar_w2)
      for (i in set_str_names)
        error_handle1(i, dist, splitmvar_w3)
    }
    
    
    vacoublary_prior_parnames <- c(
      "location",
      "scale",
      "df",
      "nu_shape",
      "nu_scale",
      "rate",
      "shape",
      "lower",
      "upper",
      "eta",
      "lb",
      "ub",
      "autoscale",
      "addrange",
      "sethp"
    )
    
    
    optional_prior_names <-
      c("lb", "ub", "autoscale", "addrange", "sethp")
    
    # Add missing optional_prior_names
    missing_optional_prior_names <-
      optional_prior_names[!optional_prior_names %in% splitmvar_w3]
    
    if (!identical(missing_optional_prior_names, character(0))) {
      splitmvar_w2 <-
        c(splitmvar_w2,
          paste0(missing_optional_prior_names, "=", "NA"))
      splitmvar_w3 <-
        c(splitmvar_w3,
          paste0(missing_optional_prior_names, "", ""))
    }
    
    
    
    min_par_names <-
      names(Filter(function(check__)
        check__ > 0, colSums(
          sapply(splitmvar_w3, grepl, vacoublary_prior_parnames)
        )))
    incorrect_names <- splitmvar_w3[!splitmvar_w3 %in% min_par_names]
    
    if (!identical(incorrect_names, character(0))) {
      ttt_n1 <- paste(incorrect_names, collapse = ", ")
      ttt_nn2 <- paste(vacoublary_prior_parnames, collapse = ", ")
      stop(
        "\nFollowing prior parameter name(s) are incorrect/misspelled:\n ",
        ttt_n1,
        "\n",
        "Available prior parameter names are:\n",
        ttt_nn2,
        "\n",
        "For ",
        dist,
        " prior distribution, the mandatory parameter are: ",
        paste(set_str_names, collapse = ", "),
        "\n",
        paste0(dist, "(", paste(set_str_names, collapse = ", "), ")")
      )
    }
    
    
    # Now make all enclosed in ''
    tt <- sub('.*=', '', splitmvar_w2)
    tt <- paste0("'", tt, "'")
    
    x <- parms_ <- paste0(splitmvar_w3, "=", tt)
    
    # Not needed, directly using name_parameter towards the end
    collect_name_parameter <- c()
    
    strict_positive_dists  <- c("lognormal", "gamma", "inv_gamma", "exponential")
    
    for (i in 1:length(x)) {
      pname_ <- substr(x[i], 1, regexpr("\\=", x[i]) - 1)
      x_i <- gsub("\"" , "", x[i])
      x_i <- eval(parse(text = x_i))
      
      sethp <-
        isTRUE(ept(gsub("\"" , "", splitmvar_w2[grepl("sethp", splitmvar_w2)])))
      
      if (ept(sethp) & (dist != "normal" &
                        dist != "cauchy" &
                        dist != "student_nu" &
                        dist != "student_t")) {
        stop(
          "Hierarchical priors are supported only for normal, cauchy, student_nu",
          "\n",
          " and student_t distributins.",
          "\n",
          " Please set sethp = NA or sethp = FALSE for the '",
          dist,
          "\n",
          "'distributins specified for nlpar '",
          nlpar,
          "', class '",
          class,
          "'"
        )
      }
      
      
      # Get scale_factor to multiply with scale parameters
      enverr. <- parent.frame()
      assign('err.', FALSE, envir = enverr.)
      tryCatch(
        expr = {
          check_for_autoscale <-
            ept(gsub("\"" , "", splitmvar_w2[grepl("autoscale", splitmvar_w2)]))
          ! is.na(check_for_autoscale)
          ! is.logical(check_for_autoscale)
          ! is.numeric(check_for_autoscale)
        },
        error = function(e) {
          assign('err.', TRUE, envir = enverr.)
        }
      )
      err. <- get('err.', envir = enverr.)
      if (err.) {
        stop("scale factor set by autoscale can only be",
             "\n",
             " NA / TRUE / FLASE or a numeric value")
      } else {
        check_for_autoscale <-
          ept(gsub("\"" , "", splitmvar_w2[grepl("autoscale", splitmvar_w2)]))
        if (!is.na(check_for_autoscale) &
            !is.logical(check_for_autoscale) &
            !is.numeric(check_for_autoscale)) {
          stop("scale factor set by autoscale can only be",
               "\n",
               "NA / TRUE / FLASE or a numeric value")
        }
      }
      
      if (is.na(check_for_autoscale) | !(check_for_autoscale)) {
        scale_factor <- 1
      } else if (check_for_autoscale &
                 !is.numeric(check_for_autoscale)) {
        scale_factor <- 2.5
        if (verbose)
          message("scale factor for autoscale option set to 2.5")
      } else if (is.numeric(check_for_autoscale)) {
        scale_factor <- check_for_autoscale
      }
      
      # Get addrange to add to uniform prior range
      
      if (is.na(ept(gsub("\"" , "", 
                         splitmvar_w2[grepl("addrange", splitmvar_w2)])))) {
        addrange <- 0
      } else {
        addrange <-
          ept(gsub("\"" , "", splitmvar_w2[grepl("addrange", splitmvar_w2)]))
      }
      
      # assigning required parameters
      
      if (pname_ %in% set_str_names) {
        check_evalation_of_numeric_pdata_obj <-
          function(prior_argument,
                   p_str_in,
                   eit,
                   x,
                   pname_,
                   dist,
                   nlpar,
                   class,
                   allowed_parm_options,
                   splitmvar_w2) {
            whatin <-
              sub("=[^=]+$", "", splitmvar_w2[grepl(eit, splitmvar_w2)])
            const_msg <- 
              paste0(" - a numeric value (e.g., 2) or a charater string such as",
                     "\n",
                     "xxx with xxx defined in the use-specified 'prior_data'",
                     "\n",
                     "argument e.g., prior_data = list(xxx = 2)"
              )
            
            if (!is.null(allowed_parm_options)) {
              allowed_parm_options <-
                paste0(allowed_parm_options, collapse = ", ")
              allowed_parm_options <-
                paste0(" - ", allowed_parm_options)
              const_msg <- paste0(allowed_parm_options, "\n", const_msg)
            }
            enverr. <- parent.frame()
            assign('err.', FALSE, envir = enverr.)
            tryCatch(
              expr = {
                out <- ept(eit)
              },
              error = function(e) {
                assign('err.', TRUE, envir = enverr.)
              }
            )
            err. <- get('err.', envir = enverr.)
            if (err.) {
              if (class == 'b' | class == 'sd') {
                stop(
                  "\nFor nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  ", you have specified '",
                  eit,
                  "' as ",
                  pname_,
                  " for the ",
                  dist,
                  " distribution",
                  "\n" ,
                  " But '",
                  eit,
                  "' is not found in the 'prior_data_internal'",
                  "\n" ,
                  " or use-specified 'prior_data' argument",
                  "\n ",
                  " [see specified prior argument: ",
                  prior_argument,
                  " = ",
                  p_str_in,
                  "]",
                  "\n" ,
                  "Avilable ",
                  pname_,
                  " parameter options are:" ,
                  "\n" ,
                  const_msg
                )
              } else if (class == 'sigma') {
                stop(
                  "\nFor residual standard deviation parameter i.e., ",
                  "class ",
                  class,
                  ", you have specified '",
                  eit,
                  "' as ",
                  pname_,
                  " for the ",
                  dist,
                  " distribution",
                  "\n" ,
                  " But '",
                  eit,
                  "' is not found in the 'prior_data_internal'",
                  "\n" ,
                  " or use-specified 'prior_data' argument",
                  "\n ",
                  " [see specified prior argument: ",
                  prior_argument,
                  " = ",
                  p_str_in,
                  "]",
                  "\n" ,
                  "Avilable ",
                  pname_,
                  " parameter options are:" ,
                  "\n" ,
                  const_msg
                )
              } else if (class == '' &
                         grepl("dpar_", prior_argument) &
                         !grepl("dpar_cov", prior_argument)) {
                stop(
                  "\nFor for distributional Intercept parameter i.e., ",
                  "Intercept_sigma ",
                  ", you have specified '",
                  eit,
                  "' as ",
                  pname_,
                  " for the ",
                  dist,
                  " distribution",
                  "\n" ,
                  " But '",
                  eit,
                  "' is not found in the 'prior_data_internal'",
                  "\n" ,
                  " or use-specified 'prior_data' argument",
                  "\n ",
                  " [see specified prior argument: ",
                  prior_argument,
                  " = ",
                  p_str_in,
                  "]",
                  "\n" ,
                  "Avilable ",
                  pname_,
                  " parameter options are:" ,
                  "\n" ,
                  const_msg
                )
              }
              
            }
          }
        
        
        
        
        allowed_parm_options <- NULL
        
        if (grepl("^location$", pname_)) {
          if (class == "b") {
            if (nlpar == "a" & cov_nlpar == "") {
              allowed_parm_options <- c("lm", "ymean", 
                                        "ymedian", "ymax",
                                        "ymeanxmax", "ymeanxmin")
            } else if (nlpar == "b") {
              allowed_parm_options <- c("lm", "ymaxs", "bstart")
            } else if (nlpar == "c") {
              allowed_parm_options <- c("lm", "cstart")
            } else if (nlpar == "d") {
              allowed_parm_options <- c("lm", "dstart", "ymeanxmid")
            } else if (nlpar == "e") {
              allowed_parm_options <- c("lm", "estart")
            } else if (nlpar == "f") {
              allowed_parm_options <- NULL
            } else if (nlpar == "g") {
              allowed_parm_options <- c("ymeanxmax", "ymeanxmidxmaxdiff")
            } else if (nlpar == "h") {
              allowed_parm_options <- NULL
            } else if (nlpar == "i") {
              allowed_parm_options <- c("lm", "estart", "bstart")
            } else if (nlpar == "s") {
              allowed_parm_options <- c("lm")
            } else if (cov_nlpar != "") {
              allowed_parm_options <- c("lm")
            } else if (sigma_dpar == "sigma") {
              allowed_parm_options <- NULL
            } else {
              allowed_parm_options <- NULL
            }
          }
          
          
          if (class == "sd") {
            allowed_parm_options <- NULL
          }
          if (class == "cor") {
            allowed_parm_options <- NULL
          }
          if (class == "sigma") {
            allowed_parm_options <- NULL
          }
          if (class == '' &
              grepl("dpar_", prior_argument) &
              !grepl("dpar_cov", prior_argument)) {
            allowed_parm_options <- NULL
          }
          if (class == "") {
            allowed_parm_options <- NULL
          }
          if (dpar != "") {
            allowed_parm_options <- NULL
          }
          if (cov_dpar != "") {
            allowed_parm_options <- NULL
          }
          
          if (!is.null(allowed_parm_options)) {
            allowed_init_options_beta <- allowed_parm_options
          } else {
            allowed_init_options_beta <- NULL
          }
        }
        allowed_parm_options <- c("ysd", "ymad")
        ##########
        if (grepl("^scale$", pname_) &
            dist != "gamma" & dist != "inv_gamma") {
          if (class == "b" | class == "sd") {
            if (nlpar == "a" & cov_nlpar == "") {
              allowed_parm_options <- c("ysd", "ymad", "lme_sd_a",
                                        "ysdxmax", "ysdxmin")
            } else if (nlpar == "b") {
              allowed_parm_options <- c("lm", "ymaxs", "bstart")
            } else if (nlpar == "c") {
              allowed_parm_options <- c("lm", "cstart")
            } else if (nlpar == "d") {
              allowed_parm_options <- c("ysd", "ymad", "ysdxmid")
            } else if (nlpar == "e") {
              allowed_parm_options <- c("lm", "estart")
            } else if (nlpar == "f") {
              allowed_parm_options <- NULL
            } else if (nlpar == "g") {
              allowed_parm_options <- c("ysdxmax", "ysdxmidxmaxdiff")
            } else if (nlpar == "h") {
              allowed_parm_options <- NULL
            } else if (nlpar == "i") {
              allowed_parm_options <- NULL
            } else if (nlpar == "s") {
              allowed_parm_options <- c("sdx")
            } else if (cov_nlpar != "") {
              allowed_parm_options <- c("lm")
            } else if (sigma_dpar == "sigma") {
              allowed_parm_options <- c("ysd", "ymad")
            } else {
              allowed_parm_options <- NULL
            }
          }
          
          
          
          
          if (class == "cor") {
            allowed_parm_options <- NULL
          }
          
          if (class == "sigma") {
            allowed_parm_options <- c("ysd", "ymad", "lme_rsd", "lm_rsd")
          }
          
          if (class == "") {
            allowed_parm_options <- NULL
          }
          
          if (dpar != "") {
            allowed_parm_options <- NULL
          }
          
          if (cov_dpar != "") {
            allowed_parm_options <- NULL
          }
          
          if (class == '' &
              grepl("dpar_", prior_argument) &
              !grepl("dpar_cov", prior_argument)) {
            allowed_parm_options <- c("ysd", "ymad", "lme_rsd", "lm_rsd")
          }
          
          if (!is.null(allowed_parm_options)) {
            allowed_init_options_sd <- allowed_parm_options
          } else {
            allowed_init_options_sd <- NULL
          }
        }
        
        
        if (grepl("^scale$", pname_) &
            dist == "gamma" | dist == "inv_gamma") {
          allowed_parm_options <- NULL
        }
        
        if (grepl("^shape$", pname_)) {
          allowed_parm_options <- NULL
        }
        
        if (grepl("^rate$", pname_)) {
          if (class == "sigma") {
            allowed_parm_options <-
              c("ysd", "ymad", "lme_sd_a", "lme_rsd", "lm_rsd")
          } else {
            allowed_parm_options <- NULL
          }
          
          if (!is.null(allowed_parm_options)) {
            allowed_init_options_rate <- allowed_parm_options
          } else {
            allowed_init_options_rate <- NULL
          }
          
        } # if (grepl("^rate$", pname_)) {
        
        
        if (grepl("^shape$", pname_)) {
          allowed_init_options_shape <- NULL # 22 4 2023
          allowed_init_options_scale <- NULL # 22 4 2023
        }
        
        if (grepl("^scale$", pname_)) {
          allowed_init_options_shape <- NULL # 22 4 2023
          allowed_init_options_scale <- NULL # 22 4 2023
        }
        
        
        if (grepl("^df$", pname_)) {
          allowed_parm_options <- NULL
        }
        
        if (grepl("^eta$", pname_)) {
          allowed_parm_options <- NULL
        }
        
        
        if (!exists('allowed_init_options_beta'))
          allowed_init_options_beta <- NULL
        if (!exists('allowed_init_options_sd'))
          allowed_init_options_sd <- NULL
        if (!exists('allowed_init_options_rate'))
          allowed_init_options_rate <- NULL
        
        if (!exists('allowed_init_options_shape'))
          allowed_init_options_shape <- NULL
        if (!exists('allowed_init_options_scale'))
          allowed_init_options_scale <- NULL
        
        
        # set location parameter -> for normal, log normal, cauchy, studdent_t
        
        if (grepl("^location$", pname_)) {
          # location nlpar a (class b)
          if (nlpar == "a" & class == "b" & grepl("a", fixedsi)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (a_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "all", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "", "", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
            } else if (x_i == paste0("ymean", empty_sufx)) {
              eit <-  gsub("ymean", paste0("ymean", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else if (x_i == paste0("ymax", empty_sufx)) {
              eit <-  gsub("ymax", paste0("ymax", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else if (x_i == paste0("ymaxs", empty_sufx)) {
              eit <-  gsub("ymaxs", paste0("ymaxs", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else if (x_i == paste0("ymedian", empty_sufx)) {
              eit <-  gsub("ymedian", paste0("ymedian", resp_), x_i)
              evaluated_parameter <- ept(eit) 
            } else if (x_i == paste0("ymeanxmin", empty_sufx)) {
              eit <-  gsub("ymeanxmin", paste0("ymeanxmin", resp_), x_i)
              evaluated_parameter <- ept(eit) 
            } else if (x_i == paste0("ymeanxmax", empty_sufx)) {
              eit <-  gsub("ymeanxmax", paste0("ymeanxmax", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, ymax, ymeanxmin, ymeanxmax,
                a numeric value (e.g., 2)",
                  "\n",
                  " or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "\n", 
                  " e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # location nlpar b (class b)
          if (nlpar == "b" & class == "b" & grepl("b", fixedsi)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (b_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "all", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "", "", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
              if (verbose)
                message("location parameter specified as lm for nlpar",
                        nlpar,
                        " is set as 0")
            } else if (x_i == paste0("ymax", empty_sufx)) {
              eit <-  gsub("ymax", paste0("ymax", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else if (x_i == paste0("ymaxs", empty_sufx)) {
              eit <-  gsub("ymaxs", paste0("ymaxs", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else if (x_i == paste0("ymean", empty_sufx)) {
              stop("option ymean as location parameter not alloweed for nlpar ",
                   nlpar)
            } else if (x_i == paste0("ymedian", empty_sufx)) {
              stop("option ymedian as location parameter not alloweed for nlpar ",
                   nlpar)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, a numeric value (e.g., 2) or a charater like zzz",
                  "\n with zzz defined in the",
                  "prior_data e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # location nlpar c (class b)
          if (nlpar == "c" & class == "b" & grepl("c", fixedsi)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (c_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "all", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "", "", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
              if (verbose)
                message("location parameter specified as lm for nlpar",
                        nlpar,
                        " is set as 0")
            } else if (x_i == paste0("ymean", empty_sufx)) {
              stop("option ymean as location parameter not alloweed for nlpar ",
                   nlpar)
            } else if (x_i == paste0("ymedian", empty_sufx)) {
              stop("option ymean as location parameter not alloweed for nlpar ",
                   nlpar)
            } else if (x_i == paste0("cstart", empty_sufx)) {
              eit <-  gsub("cstart", paste0("cstart", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, a numeric value (e.g., 2) or a charater like zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # location nlpar d (class b)
          if (nlpar == "d" & class == "b" & grepl("d", fixedsi)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (d_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "all", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "", "", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
              if (verbose)
                message("location parameter specified as lm for nlpar",
                        nlpar,
                        " is set as 0")
            } else if (x_i == paste0("ymeanxmid", empty_sufx)) {
              eit <-  gsub("ymeanxmid", paste0("ymeanxmid", resp_), x_i)
              evaluated_parameter <- ept(eit) 
            } else if (x_i == paste0("ymeanxmid", empty_sufx)) {
              eit <-  gsub("ymeanxmid", paste0("ymeanxmid", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else if (x_i == paste0("ymean", empty_sufx)) {
              stop("option ymean as location parameter not alloweed for nlpar ",
                   nlpar)
            } else if (x_i == paste0("ymedian", empty_sufx)) {
              stop("option ymean as location parameter not alloweed for nlpar ",
                   nlpar)
            } else if (x_i == paste0("dstart", empty_sufx)) {
              eit <-  gsub("dstart", paste0("dstart", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, a numeric value (e.g., 2) or a charater like zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # location nlpar e (class b)
          if (nlpar == "e" & class == "b" & grepl("e", fixedsi)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (e_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "all", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "", "", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
              if (verbose)
                message("location parameter specified as lm for nlpar",
                        nlpar,
                        " is set as 0")
            } else if (x_i == paste0("ymean", empty_sufx)) {
              stop("option ymean as location parameter not alloweed for nlpar ",
                   nlpar)
            } else if (x_i == paste0("ymedian", empty_sufx)) {
              stop("option ymean as location parameter not alloweed for nlpar ",
                   nlpar) 
            } else if (x_i == paste0("estart", empty_sufx)) {
              eit <-  gsub("estart", paste0("estart", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, a numeric value (e.g., 2) or a charater like zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # location nlpar f (class b)
          if (nlpar == "f" & class == "b" & grepl("f", fixedsi)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (f_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "all", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "", "", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
              if (verbose)
                message("location parameter specified as lm for nlpar",
                        nlpar,
                        " is set as 0")
            } else if (x_i == paste0("ymean", empty_sufx)) {
              stop("option ymean as location parameter not alloweed for nlpar ",
                   nlpar)
            } else if (x_i == paste0("ymedian", empty_sufx)) {
              stop("option ymean as location parameter not alloweed for nlpar ",
                   nlpar) 
            } else if (x_i == paste0("estart", empty_sufx)) {
              eit <-  gsub("estart", paste0("estart", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, a numeric value (e.g., 2) or a charater like zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # location nlpar g (class b)
          if (nlpar == "g" & class == "b" & grepl("g", fixedsi)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (g_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "all", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "", "", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
              if (verbose)
                message("location parameter specified as lm for nlpar",
                        nlpar,
                        " is set as 0")
            } else if (x_i == paste0("ymeanxmax", empty_sufx)) {
              eit <-  gsub("ymeanxmax", paste0("ymeanxmax", resp_), x_i)
              evaluated_parameter <- ept(eit) 
            } else if (x_i == paste0("ymeanxmidxmaxdiff", empty_sufx)) {
              eit <-  gsub("ymeanxmidxmaxdiff", 
                           paste0("ymeanxmidxmaxdiff", resp_), x_i)
              evaluated_parameter <- ept(eit) 
            } else if (x_i == paste0("ymax", empty_sufx)) {
              eit <-  gsub("ymax", paste0("ymax", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else if (x_i == paste0("ymean", empty_sufx)) {
              stop("option ymean as location parameter not alloweed for nlpar ",
                   nlpar)
            } else if (x_i == paste0("ymedian", empty_sufx)) {
              stop("option ymean as location parameter not alloweed for nlpar ",
                   nlpar) 
            } else if (x_i == paste0("estart", empty_sufx)) {
              eit <-  gsub("estart", paste0("estart", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, a numeric value (e.g., 2) or a charater like zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # location nlpar h (class b)
          if (nlpar == "h" & class == "b" & grepl("h", fixedsi)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (h_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "all", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "", "", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
              if (verbose)
                message("location parameter specified as lm for nlpar",
                        nlpar,
                        " is set as 0")
            } else if (x_i == paste0("ymean", empty_sufx)) {
              stop("option ymean as location parameter not alloweed for nlpar ",
                   nlpar)
            } else if (x_i == paste0("ymedian", empty_sufx)) {
              stop("option ymean as location parameter not alloweed for nlpar ",
                   nlpar) 
            } else if (x_i == paste0("estart", empty_sufx)) {
              eit <-  gsub("estart", paste0("estart", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, a numeric value (e.g., 2) or a charater like zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          # location nlpar i (class b)
          if (nlpar == "i" & class == "b" & grepl("i", fixedsi)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (i_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "all", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "", "", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
              if (verbose)
                message("location parameter specified as lm for nlpar",
                        nlpar,
                        " is set as 0")
            } else if (x_i == paste0("ymean", empty_sufx)) {
              stop("option ymean as location parameter not alloweed for nlpar ",
                   nlpar)
            } else if (x_i == paste0("ymedian", empty_sufx)) {
              stop("option ymean as location parameter not alloweed for nlpar ",
                   nlpar) 
            } else if (x_i == paste0("estart", empty_sufx)) {
              eit <-  gsub("estart", paste0("estart", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, a numeric value (e.g., 2) or a charater like zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # location nlpar s (class b) - sitar
          if (nlpar == "s" & class == "b") {
            if (x_i == paste0("lm", empty_sufx)) {
              if (s_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "all", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "", "", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2) or",
                  "a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            # checks
            if (nlpar == "s" & !is.null(sncov)) {
              if (length(evaluated_parameter) == 1) {
                evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
              } else if (length(evaluated_parameter) == df) {
                repeach <- nrep_of_parms / df
                evaluated_parameter <-
                  rep(
                    evaluated_parameter,
                    times = 1,
                    each = repeach,
                    length.out = nrep_of_parms
                  )
              }
            } else {
              if (length(evaluated_parameter) < nrep_of_parms)
                evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
              if (length(evaluated_parameter) > nrep_of_parms)
                stop(
                  "prior elements for nlpar ",
                  nlpar, ", class ",  class,
                  " are greater than the parameter dimensions"
                )
            }
          }
          
          
          
          # location nlpar a cov (class b)
          if (cov_nlpar == "a" & class == "b" & !is.null(ancov)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (!a_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, ymax a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # location nlpar b cov (class b)
          if (cov_nlpar == "b" & class == "b" & !is.null(bncov)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (!b_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, ymaxs a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # location nlpar c cov (class b)
          if (cov_nlpar == "c" & class == "b" & !is.null(cncov)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (!c_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # location nlpar d cov (class b)
          if (cov_nlpar == "d" & class == "b" & !is.null(dncov)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (!d_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # location nlpar e cov (class b)
          if (cov_nlpar == "e" & class == "b" & !is.null(encov)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (!e_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          # location nlpar f cov (class b)
          if (cov_nlpar == "f" & class == "b" & !is.null(fncov)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (!f_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # location nlpar g cov (class b)
          if (cov_nlpar == "g" & class == "b" & !is.null(gncov)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (!g_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # location nlpar h cov (class b)
          if (cov_nlpar == "h" & class == "b" & !is.null(hncov)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (!h_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          # location nlpar i cov (class b)
          if (cov_nlpar == "i" & class == "b" & !is.null(incov)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (!i_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # location nlpar s cov (class b)
          if (cov_nlpar == "s" & !is.null(sncov)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (!s_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            # checks
            if (nlpar == "s" & !is.null(sncov)) {
              if (length(evaluated_parameter) == 1) {
                evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
              } else if (length(evaluated_parameter) == df) {
                repeach <- nrep_of_parms / df
                evaluated_parameter <-
                  rep(evaluated_parameter,
                      times = 1,
                      each = repeach)
              } else {
                #
              }
            } else {
              if (length(evaluated_parameter) < nrep_of_parms)
                evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
              if (length(evaluated_parameter) > nrep_of_parms)
                stop("prior elements for nlpar ",
                     nlpar, ", class ",  class,
                     " are greater than the parameter dimensions"
                )
            }
          }
          
          
          
          
          # location sigma (class b)
          
          if (nlpar == "" & class == "b" & sigma_dpar == 'sigma') {
            if (x_i == paste0("lm", empty_sufx)) {
              if (sigma_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "all", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "", "", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
            } else if (x_i == paste0("ymean", empty_sufx)) {
              eit <-  gsub("ymean", paste0("ymean", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else if (x_i == paste0("ymedian", empty_sufx)) {
              eit <-  gsub("ymedian", paste0("ymedian", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for distributional ",
                  sigma_dpar,
                  ", class ",
                  class,
                  " are:a numeric value (e.g., 2)",
                  "\n",
                  " or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "\n", 
                  " e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for distributional ",
                   sigma_dpar,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # location sigma cov (class b)
          if (cov_sigma_dpar != "" & class == "b" & cov_sigma_dpar == 'sigma_cov' & 
              !is.null(sigmancov)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (!sigma_form_0) {
                # lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              } else {
                # lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              }
              # eit <-  gsub("lm", lm_gsubby, x_i)
              # evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for distributional ",
                  sigma_dpar,
                  ", class ",
                  class,
                  " are:\n a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for distributional ",
                   sigma_dpar,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          # location a b c d e random effects
          
          # location nlpar a (class sd, typically 0)
          
          if (nlpar == "a" & class == "sd" & grepl("a", randomsi)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                "or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # location nlpar b (class sd, typically 0)
          if (nlpar == "b" & class == "sd" & grepl("b", randomsi)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, a numeric value (e.g., 2) or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          # location nlpar c (class sd, typically 0)
          if (nlpar == "c" & class == "sd" & grepl("c", randomsi)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, a numeric value (e.g., 2) or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          # location nlpar d (class sd, typically 0)
          if (nlpar == "d" & class == "sd" & grepl("d", randomsi)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, a numeric value (e.g., 2) or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # location nlpar e (class sd, typically 0)
          if (nlpar == "e" & class == "sd" & grepl("e", randomsi)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, a numeric value (e.g., 2) or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # location nlpar f (class sd, typically 0)
          if (nlpar == "f" & class == "sd" & grepl("f", randomsi)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, a numeric value (e.g., 2) or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # location nlpar g (class sd, typically 0)
          if (nlpar == "g" & class == "sd" & grepl("g", randomsi)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, a numeric value (e.g., 2) or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # location nlpar h (class sd, typically 0)
          if (nlpar == "h" & class == "sd" & grepl("h", randomsi)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, a numeric value (e.g., 2) or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          # location nlpar i (class sd, typically 0)
          if (nlpar == "i" & class == "sd" & grepl("i", randomsi)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, a numeric value (e.g., 2) or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          
          # location nlpar s (class sd, typically 0)
          if (nlpar == "s" & class == "sd") {
            if (x_i == paste0("lm", empty_sufx)) {
              if (s_form_0_gr) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "all", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "", "", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2) or",
                  "a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            # checks
            if (nlpar == "s" & !is.null(sncov_gr)) {
              if (length(evaluated_parameter) == 1) {
                evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
              } else if (length(evaluated_parameter) == df) {
                repeach <- nrep_of_parms / df
                evaluated_parameter <-
                  rep(
                    evaluated_parameter,
                    times = 1,
                    each = repeach,
                    length.out = nrep_of_parms
                  )
              }
            } else {
              if (length(evaluated_parameter) < nrep_of_parms)
                evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
              if (length(evaluated_parameter) > nrep_of_parms)
                stop(
                  "prior elements for nlpar ",
                  nlpar, ", class ",  class,
                  " are greater than the parameter dimensions"
                )
            }
          }
          
          
          
          
          # location nlpar a cov (class sd, typically 0)
          if (cov_nlpar == "a" & class == "sd" & !is.null(ancov_gr)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                "or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # location nlpar b cov (class sd, typically 0)
          if (cov_nlpar == "b" & class == "sd" & !is.null(bncov_gr)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                "or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # location nlpar c cov (class sd, typically 0)
          if (cov_nlpar == "c" & class == "sd" & !is.null(cncov_gr)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                "or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # location nlpar d cov (class sd, typically 0)
          if (cov_nlpar == "d" & class == "sd" & !is.null(dncov_gr)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                "or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          # location nlpar e cov (class sd, typically 0)
          if (cov_nlpar == "e" & class == "sd" & !is.null(encov_gr)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                "or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          # location nlpar f cov (class sd, typically 0)
          if (cov_nlpar == "f" & class == "sd" & !is.null(fncov_gr)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                "or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          # location nlpar g cov (class sd, typically 0)
          if (cov_nlpar == "g" & class == "sd" & !is.null(gncov_gr)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                "or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # location nlpar h cov (class sd, typically 0)
          if (cov_nlpar == "h" & class == "sd" & !is.null(hncov_gr)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                "or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # location nlpar i cov (class sd, typically 0)
          if (cov_nlpar == "i" & class == "sd" & !is.null(incov_gr)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                "or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          
          # location nlpar i cov (class sd, typically 0)
          if (cov_nlpar == "s" & !is.null(sncov_gr)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (!s_form_0_gr) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "cov", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            # checks
            if (nlpar == "s" & !is.null(sncov_gr)) {
              if (length(evaluated_parameter) == 1) {
                evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
              } else if (length(evaluated_parameter) == df) {
                repeach <- nrep_of_parms / df
                evaluated_parameter <-
                  rep(evaluated_parameter,
                      times = 1,
                      each = repeach)
              } else {
                #
              }
            } else {
              if (length(evaluated_parameter) < nrep_of_parms)
                evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
              if (length(evaluated_parameter) > nrep_of_parms)
                stop("prior elements for nlpar ",
                     nlpar, ", class ",  class,
                     " are greater than the parameter dimensions"
                )
            }
          }
          
          
          
          
          # location sigma (class sd, typically 0)
          if (nlpar == "" & class == "sd" & sigma_dpar == 'sigma') {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for distributional ",
                sigma_dpar,
                ", class ",
                class,
                " are:\n a numeric value (e.g., 2)",
                "or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for distributional ",
                   sigma_dpar,
                   " are greater than the parameter dimensions")
          }
          
          
          # location sigma cov (class sd, typically 0)
          if (cov_sigma_dpar != "" & class == "sd" & cov_sigma_dpar == 'sigma_cov' & 
              !is.null(sigmancov_gr)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for distributional ",
                sigma_dpar,
                ", class ",
                class,
                " are:\n a numeric value (e.g., 2)",
                "or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for distributional ",
                   sigma_dpar,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          # location rsd param
          if (class == "sigma") {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                "or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # location dpar param ~
          if (!is.null(dparncov) & class == "") {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "location parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                "or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
        } # if(grepl("^location$", i))
        
        
        
        
        
        # set scale parameter -> for normal, log normal, cauchy, studdent_t
        
        if (grepl("^scale$", pname_)) {
          # scale a b c d e fixed effects
          
          # scale nlpar a (class b)
          if (nlpar == "a" & class == "b" & grepl("a", fixedsi)) {
            if (x_i == paste0("ysd", empty_sufx)) {
              eit <-  gsub("ysd", paste0("ysd", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("ymad", empty_sufx)) {
              eit <-  gsub("ymad", paste0("ymad", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("lme_sd_a", empty_sufx)) {
              eit <-  gsub("lme_sd_a", paste0("lme_sd_a", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("ysdxmin", empty_sufx)) {
              eit <-  gsub("ysdxmin", paste0("ysdxmin", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit) 
            } else if (x_i == paste0("ysdxmin", empty_sufx)) {
              eit <-  gsub("ysdxmin", paste0("ysdxmin", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- scale_factor * ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n ysd, ysd, lme_sd_a, or a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # scale nlpar b (class b)
          if (nlpar == "b" & class == "b" & grepl("b", fixedsi)) {
            if (x_i == paste0("ysd", empty_sufx)) {
              eit <-  gsub("ysd", paste0("ysd", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("ymad", empty_sufx)) {
              eit <-  gsub("ymad", paste0("ymad", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("lme_sd_a", empty_sufx)) {
              eit <-  gsub("lme_sd_a", paste0("lme_sd_a", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- scale_factor * ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n ysd, ysd, lme_sd_a, or a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # scale nlpar c (class b)
          if (nlpar == "c" & class == "b" & grepl("c", fixedsi)) {
            if (x_i == paste0("vsd", empty_sufx)) {
              eit <-  gsub("vsd", paste0("vsd", resp_), x_i)
              evaluated_parameter <- 1 * ept(eit)
            } else if (x_i == paste0("vmad", empty_sufx)) {
              eit <-  gsub("vmad", paste0("vmad", resp_), x_i)
              evaluated_parameter <- 1 * ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- 1 * ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, a numeric value (e.g., 2) or a charater like zzz",
                  "\n with zzz defined in the prior_data", 
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # scale nlpar d (class b)
          if (nlpar == "d" & class == "b" & grepl("d", fixedsi)) {
            if (x_i == paste0("ysd", empty_sufx)) {
              eit <-  gsub("ysd", paste0("ysd", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("ymad", empty_sufx)) {
              eit <-  gsub("ymad", paste0("ymad", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("dsd", empty_sufx)) {
              eit <-  gsub("dsd", paste0("vsd", resp_), x_i)
              evaluated_parameter <- 1 * ept(eit)
            } else if (x_i == paste0("dmad", empty_sufx)) {
              eit <-  gsub("dmad", paste0("dmad", resp_), x_i)
              evaluated_parameter <- 1 * ept(eit)
            } else if (x_i == paste0("ysdxmid", empty_sufx)) {
              eit <-  gsub("ysdxmid", paste0("ysdxmid", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit) 
            } else if (x_i == paste0("ysdxmid", empty_sufx)) {
              eit <-  gsub("ysdxmid", paste0("ysdxmid", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- 1 * ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, a numeric value (e.g., 2) or a charater like zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # scale nlpar e (class b)
          if (nlpar == "e" & class == "b" & grepl("e", fixedsi)) {
            if (x_i == paste0("xsd", empty_sufx)) {
              eit <-  gsub("xsd", paste0("xsd", resp_), x_i)
              evaluated_parameter <- 1 * ept(eit)
            } else if (x_i == paste0("xmad", empty_sufx)) {
              eit <-  gsub("xmad", paste0("xmad", resp_), x_i)
              evaluated_parameter <- 1 * ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- 1 * ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, a numeric value (e.g., 2) or a charater like zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          # scale nlpar f (class b)
          if (nlpar == "f" & class == "b" & grepl("f", fixedsi)) {
            if (x_i == paste0("xsd", empty_sufx)) {
              eit <-  gsub("xsd", paste0("xsd", resp_), x_i)
              evaluated_parameter <- 1 * ept(eit)
            } else if (x_i == paste0("xmad", empty_sufx)) {
              eit <-  gsub("xmad", paste0("xmad", resp_), x_i)
              evaluated_parameter <- 1 * ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- 1 * ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, a numeric value (e.g., 2) or a charater like zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # scale nlpar g (class b)
          if (nlpar == "g" & class == "b" & grepl("g", fixedsi)) {
            if (x_i == paste0("xsd", empty_sufx)) {
              eit <-  gsub("xsd", paste0("xsd", resp_), x_i)
              evaluated_parameter <- 1 * ept(eit)
            } else if (x_i == paste0("xmad", empty_sufx)) {
              eit <-  gsub("xmad", paste0("xmad", resp_), x_i)
              evaluated_parameter <- 1 * ept(eit)
            } else if (x_i == paste0("ysdxmax", empty_sufx)) {
              eit <-  gsub("ysdxmax", paste0("ysdxmax", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit) 
            } else if (x_i == paste0("ysdxmidxmaxdiff", empty_sufx)) {
              eit <-  gsub("ysdxmidxmaxdiff", 
                           paste0("ysdxmidxmaxdiff", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- 1 * ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, a numeric value (e.g., 2) or a charater like zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # scale nlpar h (class b)
          if (nlpar == "h" & class == "b" & grepl("h", fixedsi)) {
            if (x_i == paste0("xsd", empty_sufx)) {
              eit <-  gsub("xsd", paste0("xsd", resp_), x_i)
              evaluated_parameter <- 1 * ept(eit)
            } else if (x_i == paste0("xmad", empty_sufx)) {
              eit <-  gsub("xmad", paste0("xmad", resp_), x_i)
              evaluated_parameter <- 1 * ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- 1 * ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, a numeric value (e.g., 2) or a charater like zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # scale nlpar i (class b)
          if (nlpar == "i" & class == "b" & grepl("i", fixedsi)) {
            if (x_i == paste0("xsd", empty_sufx)) {
              eit <-  gsub("xsd", paste0("xsd", resp_), x_i)
              evaluated_parameter <- 1 * ept(eit)
            } else if (x_i == paste0("xmad", empty_sufx)) {
              eit <-  gsub("xmad", paste0("xmad", resp_), x_i)
              evaluated_parameter <- 1 * ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- 1 * ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, a numeric value (e.g., 2) or a charater like zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # scale nlpar s (class b) - sitar
          if (nlpar == "s" & class == "b") {
            if (x_i == paste0("lm", empty_sufx)) {
              if (s_form_0) {
                lm_gsubby <- paste0("lm", "_", 'sdx', "_", "all", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", 'sdx', "", "", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("stau", empty_sufx)) {
              evaluated_parameter <- rep(NA, nrep_of_parms)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- scale_factor * ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data", 
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            # checks
            if (nlpar == "s" & !is.null(sncov)) {
              if (length(evaluated_parameter) == 1) {
                evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
              } else if (length(evaluated_parameter) == df) {
                repeach <- nrep_of_parms / df
                evaluated_parameter <-
                  rep(evaluated_parameter,
                      times = 1,
                      each = repeach)
              }
            } else {
              if (length(evaluated_parameter) < nrep_of_parms)
                evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
              if (length(evaluated_parameter) > nrep_of_parms)
                stop("prior elements for nlpar ",
                     nlpar, ", class ",  class,
                     " are greater than the parameter dimensions"
                )
            }
          }
          
          
          
          # scale nlpar a cov (class b)
          if (cov_nlpar == "a" & class == "b" & !is.null(ancov)) {
            if (x_i == paste0("sdacov", empty_sufx)) {
              eit <-  gsub("sdacov", paste0("acov_sd", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # scale nlpar b cov (class b)
          if (cov_nlpar == "b" & class == "b" & !is.null(bncov)) {
            if (x_i == paste0("sdbcov", empty_sufx)) {
              eit <-  gsub("sdbcov", paste0("bcov_sd", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          } # if (cov_nlpar == "b" & class == "b" & !is.null(bncov)) {
          
          
          
          # scale nlpar c cov (class b)
          if (cov_nlpar == "c" & class == "b" & !is.null(cncov)) {
            if (x_i == paste0("sdccov", empty_sufx)) {
              eit <-  gsub("sdccov", paste0("ccov_sd", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # scale nlpar d cov (class b)
          if (cov_nlpar == "d" & class == "b" & !is.null(dncov)) {
            if (x_i == paste0("sddcov", empty_sufx)) {
              eit <-  gsub("sddcov", paste0("dcov_sd", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # scale nlpar e cov (class b)
          if (cov_nlpar == "e" & class == "b" & !is.null(encov)) {
            if (x_i == paste0("sdecov", empty_sufx)) {
              eit <-  gsub("sdecov", paste0("ecov_sd", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          # scale nlpar f cov (class b)
          if (cov_nlpar == "f" & class == "b" & !is.null(fncov)) {
            if (x_i == paste0("sdfcov", empty_sufx)) {
              eit <-  gsub("sdfcov", paste0("fcov_sd", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          # scale nlpar g cov (class b)
          if (cov_nlpar == "g" & class == "b" & !is.null(gncov)) {
            if (x_i == paste0("sdfcov", empty_sufx)) {
              eit <-  gsub("sdfcov", paste0("gcov_sd", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # scale nlpar h cov (class b)
          if (cov_nlpar == "h" & class == "b" & !is.null(hncov)) {
            if (x_i == paste0("sdfcov", empty_sufx)) {
              eit <-  gsub("sdfcov", paste0("hcov_sd", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # scale nlpar i cov (class b)
          if (cov_nlpar == "f" & class == "b" & !is.null(incov)) {
            if (x_i == paste0("sdfcov", empty_sufx)) {
              eit <-  gsub("sdfcov", paste0("icov_sd", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # scale nlpar s cov (class b) - sitar
          if (cov_nlpar == "s" & class == "b" & !is.null(sncov)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (!s_form_0) {
                lm_gsubby <- paste0("lm", "_", 'sdx', "_", "cov", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", 'sdx', "_", "cov", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              }
            }
            # checks
            if (nlpar == "s" & !is.null(sncov)) {
              if (length(evaluated_parameter) == 1) {
                evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
              } else if (length(evaluated_parameter) == df) {
                repeach <- nrep_of_parms / df
                evaluated_parameter <-
                  rep(evaluated_parameter,
                      times = 1,
                      each = repeach)
              } else {
                #
              }
            } else {
              if (length(evaluated_parameter) < nrep_of_parms)
                evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
              if (length(evaluated_parameter) > nrep_of_parms)
                stop("prior elements for nlpar ",
                     nlpar, ", class ",  class,
                     " are greater than the parameter dimensions"
                )
            }
          }
          
          
          
          
          
          
          # scale sigma (class b)
          if (nlpar == "" & class == "b" & sigma_dpar == "sigma") {
            if (x_i == paste0("ysd", empty_sufx)) {
              eit <-  gsub("ysd", paste0("ysd", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("ysd", empty_sufx)) {
              eit <-  gsub("vsd", paste0("vsd", resp_), x_i)
              evaluated_parameter <- 1 * ept(eit)
            } else if (x_i == paste0("ymad", empty_sufx)) {
              eit <-  gsub("vmad", paste0("vmad", resp_), x_i)
              evaluated_parameter <- 1 * ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- 1 * ept(eit)
              } else {
                stop(
                  "scale parameter options for distributional ",
                  sigma_dpar,
                  ", class ",
                  class,
                  " are:\n a numeric value (e.g., 2) or a charater like zzz",
                  "\n with zzz defined in the prior_data", 
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for distributional ",
                   sigma_dpar,
                   " are greater than the parameter dimensions")
          }
          
          
          # scale sigma cov (class b)
          if (cov_sigma_dpar != "" & class == "b" & cov_sigma_dpar == 'sigma_cov' & 
              !is.null(sigmancov)) {
            if (x_i == paste0("sdacov", empty_sufx)) {
              eit <-  gsub("sdacov", paste0("acov_sd", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for distributional ",
                  sigma_dpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements fordistributional ",
                   sigma_dpar,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          
          
          
          
          # scale a b c d e f random effects
          
          # scale a (class sd)
          if (nlpar == "a" & class == "sd" & grepl("a", randomsi)) {
            if (x_i == paste0("ysd", empty_sufx)) {
              eit <-  gsub("ysd", paste0("ysd", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("ymad", empty_sufx)) {
              eit <-  gsub("ymad", paste0("ymad", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("lme_sd_a", empty_sufx)) {
              eit <-  gsub("lme_sd_a", paste0("lme_sd_a", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("ysdxmax", empty_sufx)) {
              eit <-  gsub("ysdxmax", paste0("ysdxmax", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit) 
            } else if (x_i == paste0("ysdxmin", empty_sufx)) {
              eit <-  gsub("ysdxmin", paste0("ysdxmin", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- scale_factor * ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n ysd, ysd, lme_sd_a, or a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # scale b (class sd)
          if (nlpar == "b" & class == "sd" & grepl("b", randomsi)) {
            if (x_i == paste0("ysd", empty_sufx)) {
              eit <-  gsub("ysd", paste0("ysd", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("ymad", empty_sufx)) {
              eit <-  gsub("ymad", paste0("ymad", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("lme_sd_a", empty_sufx)) {
              eit <-  gsub("lme_sd_a", paste0("lme_sd_a", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- scale_factor * ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n ysd, ysd, lme_sd_a, or a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # scale c (class sd)
          if (nlpar == "c" & class == "sd" & grepl("c", randomsi)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "scale parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, a numeric value (e.g., 2) or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # scale d (class sd)
          if (nlpar == "d" & class == "sd" & grepl("d", randomsi)) {
            if (x_i == paste0("ysd", empty_sufx)) {
              eit <-  gsub("ysd", paste0("ysd", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("ymad", empty_sufx)) {
              eit <-  gsub("ymad", paste0("ymad", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("ysdxmid", empty_sufx)) {
              eit <-  gsub("ysdxmid", paste0("ysdxmid", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit) 
            } else if (x_i == paste0("ysdxmidxmaxdiff", empty_sufx)) {
              eit <-  gsub("ysdxmidxmaxdiff", 
                           paste0("ysdxmidxmaxdiff", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit) 
            } else if (x_i == paste0("ysdxmax", empty_sufx)) {
              eit <-  gsub("ysdxmax", paste0("ysdxmax", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- scale_factor * ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n ysd, ysd, lme_sd_a, or a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # scale e (class sd)
          if (nlpar == "e" & class == "sd" & grepl("e", randomsi)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "scale parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, a numeric value (e.g., 2) or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          # scale f (class sd)
          if (nlpar == "f" & class == "sd" & grepl("f", randomsi)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "scale parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, a numeric value (e.g., 2) or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # scale g (class sd)
          if (nlpar == "g" & class == "sd" & grepl("g", randomsi)) {
            if (x_i == paste0("ysd", empty_sufx)) {
              eit <-  gsub("ysd", paste0("ysd", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("ymad", empty_sufx)) {
              eit <-  gsub("ymad", paste0("ymad", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("ysdxmid", empty_sufx)) {
              eit <-  gsub("ysdxmid", paste0("ysdxmid", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit) 
            } else if (x_i == paste0("ysdxmidxmaxdiff", empty_sufx)) {
              eit <-  gsub("ysdxmidxmaxdiff", 
                           paste0("ysdxmidxmaxdiff", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit) 
            } else if (x_i == paste0("ysdxmax", empty_sufx)) {
              eit <-  gsub("ysdxmax", paste0("ysdxmax", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- scale_factor * ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n ysd, ysd, lme_sd_a, or a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # scale h (class sd)
          if (nlpar == "h" & class == "sd" & grepl("h", randomsi)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "scale parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, a numeric value (e.g., 2) or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # scale i (class sd)
          if (nlpar == "i" & class == "sd" & grepl("i", randomsi)) {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "scale parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, a numeric value (e.g., 2) or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # scale nlpar s (class sd) - sitar
          if (nlpar == "s" & class == "sd" & grepl("s", randomsi)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (s_form_0_gr) {
                lm_gsubby <- paste0("lm", "_", 'sdx', "_", "all", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", 'sdx', "", "", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("stau", empty_sufx)) {
              evaluated_parameter <- rep(NA, nrep_of_parms)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- scale_factor * ept(eit)
              } else {
                stop(
                  "location parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data", 
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            # checks
            if (nlpar == "s" & !is.null(sncov_gr)) {
              if (length(evaluated_parameter) == 1) {
                evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
              } else if (length(evaluated_parameter) == df) {
                repeach <- nrep_of_parms / df
                evaluated_parameter <-
                  rep(evaluated_parameter,
                      times = 1,
                      each = repeach)
              }
            } else {
              if (length(evaluated_parameter) < nrep_of_parms)
                evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
              if (length(evaluated_parameter) > nrep_of_parms)
                stop("prior elements for nlpar ",
                     nlpar, ", class ",  class,
                     " are greater than the parameter dimensions"
                )
            }
          }
          
          
          
          
          # scale a cov (class sd)
          if (cov_nlpar == "a" & class == "sd" & !is.null(ancov_gr)) {
            if (x_i == paste0("sdacov", empty_sufx)) {
              eit <-  gsub("sdacov", paste0("acov_sd_gr", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # scale b cov (class sd)
          if (cov_nlpar == "b" & class == "sd" & !is.null(bncov_gr)) {
            if (x_i == paste0("sdbcov", empty_sufx)) {
              eit <-  gsub("sdbcov", paste0("bcov_sd_gr", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # scale c cov (class sd)
          if (cov_nlpar == "c" & class == "sd" & !is.null(cncov_gr)) {
            if (x_i == paste0("sdccov", empty_sufx)) {
              eit <-  gsub("sdccov", paste0("ccov_sd_gr", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # scale d cov (class sd)
          if (cov_nlpar == "c" & class == "sd" & !is.null(dncov_gr)) {
            if (x_i == paste0("sdccov", empty_sufx)) {
              eit <-  gsub("sdccov", paste0("ccov_sd_gr", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          # scale e cov (class sd)
          if (cov_nlpar == "e" & class == "sd" & !is.null(encov_gr)) {
            if (x_i == paste0("sdecov", empty_sufx)) {
              eit <-  gsub("sdecov", paste0("ecov_sd_gr", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          # scale f cov (class sd)
          if (cov_nlpar == "f" & class == "sd" & !is.null(fncov_gr)) {
            if (x_i == paste0("sdfcov", empty_sufx)) {
              eit <-  gsub("sdfcov", paste0("fcov_sd_gr", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # scale g cov (class sd)
          if (cov_nlpar == "g" & class == "sd" & !is.null(fncov_gr)) {
            if (x_i == paste0("sdfcov", empty_sufx)) {
              eit <-  gsub("sdfcov", paste0("gcov_sd_gr", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          # scale h cov (class sd)
          if (cov_nlpar == "h" & class == "sd" & !is.null(hncov_gr)) {
            if (x_i == paste0("sdfcov", empty_sufx)) {
              eit <-  gsub("sdfcov", paste0("hcov_sd_gr", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          # scale i cov (class sd)
          if (cov_nlpar == "i" & class == "sd" & !is.null(incov_gr)) {
            if (x_i == paste0("sdfcov", empty_sufx)) {
              eit <-  gsub("sdfcov", paste0("icov_sd_gr", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          
          # scale nlpar s cov (class sd) - sitar
          if (cov_nlpar == "s" & class == "sd" & !is.null(sncov_gr)) {
            if (x_i == paste0("lm", empty_sufx)) {
              if (!s_form_0_gr) {
                lm_gsubby <- paste0("lm", "_", 'sdx', "_", "cov", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", 'sdx', "_", "cov", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              }
            }
            # checks
            if (nlpar == "s" & !is.null(sncov_gr)) {
              if (length(evaluated_parameter) == 1) {
                evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
              } else if (length(evaluated_parameter) == df) {
                repeach <- nrep_of_parms / df
                evaluated_parameter <-
                  rep(evaluated_parameter,
                      times = 1,
                      each = repeach)
              } else {
                #
              }
            } else {
              if (length(evaluated_parameter) < nrep_of_parms)
                evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
              if (length(evaluated_parameter) > nrep_of_parms)
                stop("prior elements for nlpar ",
                     nlpar, ", class ",  class,
                     " are greater than the parameter dimensions"
                )
            }
          }
          
          
          
          
          
          # scale sigma (class sd)
          if (nlpar == "" & class == "sd" & sigma_dpar == "sigma") {
            if (x_i == paste0("vsd", empty_sufx)) {
              eit <-  gsub("vsd", paste0("vsd", resp_), x_i)
              evaluated_parameter <- 1 * ept(eit)
            } else if (x_i == paste0("vmad", empty_sufx)) {
              eit <-  gsub("vmad", paste0("vmad", resp_), x_i)
              evaluated_parameter <- 1 * ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- 1 * ept(eit)
              } else {
                stop(
                  "scale parameter options for distributional ",
                  sigma_dpar,
                  ", class ",
                  class,
                  " are:\n a numeric value (e.g., 2) or a charater like zzz",
                  "\n with zzz defined in the prior_data", 
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for distributional ",
                   sigma_dpar,
                   " are greater than the parameter dimensions")
          }
          
          
          
          # scale sigma cov (class sd)
          if (cov_sigma_dpar != "" & class == "sd" & sigma_dpar == "sigma" & 
              !is.null(sigmancov)) {
            if (x_i == paste0("sdacov", empty_sufx)) {
              eit <-  gsub("sdacov", paste0("acov_sd", resp_), x_i)
              evaluated_parameter <- ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- ept(eit)
              } else {
                stop(
                  "scale parameter options for distributional ",
                  sigma_dpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements fordistributional ",
                   sigma_dpar,
                   " are greater than the parameter dimensions")
          }
          
          
          
          
          
          # scale sigma (class sd)
          if (class == "sigma") {
            if (x_i == paste0("ysd", empty_sufx)) {
              eit <-  gsub("ysd", paste0("ysd", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("ymad", empty_sufx)) {
              eit <-  gsub("ymad", paste0("ymad", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("lme_rsd", empty_sufx)) {
              eit <-  gsub("lme_rsd", paste0("lme_rsd", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("lm_rsd", empty_sufx)) {
              eit <-  gsub("lm_rsd", paste0("lm_rsd", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- scale_factor * ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          
          
          # scale dpar (class sd) sigma ~ 
          if (!is.null(dparncov) & class == "") {
            if (x_i == paste0("ysd", empty_sufx)) {
              eit <-  gsub("ysd", paste0("ysd", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("ymad", empty_sufx)) {
              eit <-  gsub("ymad", paste0("ymad", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("lme_rsd", empty_sufx)) {
              eit <-  gsub("lme_rsd", paste0("lme_rsd", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else if (x_i == paste0("lm_rsd", empty_sufx)) {
              eit <-  gsub("lm_rsd", paste0("lm_rsd", resp_), x_i)
              evaluated_parameter <- scale_factor * ept(eit)
            } else {
              check_evalation_of_numeric_pdata_obj(
                prior_argument,
                p_str_in,
                x_i,
                x,
                pname_,
                dist,
                nlpar,
                class,
                allowed_parm_options,
                splitmvar_w2
              )
              if (is.numeric(eval(parse(text = x_i))) |
                  !is.null(eval(parse(text = x_i)))) {
                eit <- x_i
                evaluated_parameter <- scale_factor * ept(eit)
              } else {
                stop(
                  "scale parameter options for nlpar ",
                  nlpar,
                  ", class ",
                  class,
                  " are:\n lm, ymean, ymedian, a numeric value (e.g., 2)",
                  "or a charater such as zzz",
                  "\n with zzz defined in the prior_data",
                  "e.g., prior_data = list(zzz = 2)"
                )
              }
            }
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
        }
        
        
        
        
        # set degree of freedom df parameters -> for student_t
        
        if (grepl("^df$", pname_)) {
          check_evalation_of_numeric_pdata_obj(
            prior_argument,
            p_str_in,
            x_i,
            x,
            pname_,
            dist,
            nlpar,
            class,
            allowed_parm_options,
            splitmvar_w2
          )
          if (is.numeric(eval(parse(text = x_i))) |
              !is.null(eval(parse(text = x_i)))) {
            eit <- x_i
            evaluated_parameter <- ept(eit)
          } else {
            stop(
              "df parameter options for nlpar ",
              nlpar,
              ", class ",
              class,
              " are:\n zzzz, a numeric value (e.g., 2) or a charater such as zzz",
              "\n with zzz defined in the prior_data",
              "e.g., prior_data = list(zzz = 2)"
            )
          }
          # checks
          if (nlpar == "s" & !is.null(sncov)) {
            if (length(evaluated_parameter) == 1) {
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            } else if (length(evaluated_parameter) == df) {
              repeach <- nrep_of_parms / df
              evaluated_parameter <-
                rep(evaluated_parameter,
                    times = 1,
                    each = repeach)
            }
          } else {
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
        }
        
        
        # set nu_shape parameters -> for student_nu
        
        if (grepl("^nu_shape$", pname_)) {
          check_evalation_of_numeric_pdata_obj(
            prior_argument,
            p_str_in,
            x_i,
            x,
            pname_,
            dist,
            nlpar,
            class,
            allowed_parm_options,
            splitmvar_w2
          )
          if (is.numeric(eval(parse(text = x_i))) |
              !is.null(eval(parse(text = x_i)))) {
            eit <- x_i
            evaluated_parameter <- ept(eit)
          } else {
            stop(
              "df parameter options for nlpar ",
              nlpar,
              ", class ",
              class,
              " are:\n zzzz, a numeric value (e.g., 2) or a charater such as zzz",
              "\n with zzz defined in the prior_data",
              "e.g., prior_data = list(zzz = 2)"
            )
          }
          # checks
          if (nlpar == "s" & !is.null(sncov)) {
            if (length(evaluated_parameter) == 1) {
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            } else if (length(evaluated_parameter) == df) {
              repeach <- nrep_of_parms / df
              evaluated_parameter <-
                rep(evaluated_parameter,
                    times = 1,
                    each = repeach)
            }
          } else {
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
        }
        
        
        # set nu_scale parameters -> for student_nu
        
        if (grepl("^nu_scale$", pname_)) {
          check_evalation_of_numeric_pdata_obj(
            prior_argument,
            p_str_in,
            x_i,
            x,
            pname_,
            dist,
            nlpar,
            class,
            allowed_parm_options,
            splitmvar_w2
          )
          if (is.numeric(eval(parse(text = x_i))) |
              !is.null(eval(parse(text = x_i)))) {
            eit <- x_i
            evaluated_parameter <- ept(eit)
          } else {
            stop(
              "df parameter options for nlpar ",
              nlpar,
              ", class ",
              class,
              " are:\n zzzz, a numeric value (e.g., 2) or a charater such as zzz",
              "\n with zzz defined in the prior_data",
              "e.g., prior_data = list(zzz = 2)"
            )
          }
          # checks
          if (nlpar == "s" & !is.null(sncov)) {
            if (length(evaluated_parameter) == 1) {
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            } else if (length(evaluated_parameter) == df) {
              repeach <- nrep_of_parms / df
              evaluated_parameter <-
                rep(evaluated_parameter,
                    times = 1,
                    each = repeach)
            }
          } else {
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
        }
        
        
        
        
        # set rate parameters -> for exponential
        
        if (grepl("^rate$", pname_)) {
          if (x_i == paste0("ysd", empty_sufx)) {
            eit <-  gsub("ysd", paste0("ysd", resp_), x_i)
            evaluated_parameter <- 1 / (1 * ept(eit))
          } else if (x_i == paste0("ymad", empty_sufx)) {
            eit <-  gsub("ymad", paste0("ymad", resp_), x_i)
            evaluated_parameter <- 1 / (1 * ept(eit))
          } else if (x_i == paste0("lme_rsd", empty_sufx)) {
            eit <-  gsub("lme_rsd", paste0("lme_rsd", resp_), x_i)
            evaluated_parameter <- scale_factor * ept(eit)
          } else if (x_i == paste0("lm_rsd", empty_sufx)) {
            eit <-  gsub("lm_rsd", paste0("lm_rsd", resp_), x_i)
            evaluated_parameter <- scale_factor * ept(eit)
          } else if (x_i == paste0("lme_sd_a", empty_sufx)) {
            eit <-  gsub("lme_sd_a", paste0("lme_sd_a", resp_), x_i)
            evaluated_parameter <- scale_factor * ept(eit)
          } else {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- 1 / ept(eit)
            }
          }
          # checks
          if (nlpar == "s" & !is.null(sncov)) {
            if (length(evaluated_parameter) == 1) {
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            } else if (length(evaluated_parameter) == df) {
              repeach <- nrep_of_parms / df
              evaluated_parameter <-
                rep(evaluated_parameter,
                    times = 1,
                    each = repeach)
            }
          } else {
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
        }
        
        
        
        # set shape parameter -> for gamma inv_gamma (scale already covered)
        
        if (grepl("^shape$", pname_)) {
          check_evalation_of_numeric_pdata_obj(
            prior_argument,
            p_str_in,
            x_i,
            x,
            pname_,
            dist,
            nlpar,
            class,
            allowed_parm_options,
            splitmvar_w2
          )
          if (is.numeric(eval(parse(text = x_i))) |
              !is.null(eval(parse(text = x_i)))) {
            eit <- x_i
            evaluated_parameter <- ept(eit)
          } else {
            stop(
              "df parameter options for nlpar ",
              nlpar,
              ", class ",
              class,
              " are:\n zzzz, a numeric value (e.g., 2) or a charater such as zzz",
              "\n with zzz defined in the prior_data", 
              "e.g., prior_data = list(zzz = 2)"
            )
          }
          # checks
          if (nlpar == "s" & !is.null(sncov)) {
            if (length(evaluated_parameter) == 1) {
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            } else if (length(evaluated_parameter) == df) {
              repeach <- nrep_of_parms / df
              evaluated_parameter <-
                rep(evaluated_parameter,
                    times = 1,
                    each = repeach)
            }
          } else {
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
        }
        
        
        
        # set lower upper parameters -> for uniform
        
        if (grepl("^lower$", pname_)) {
          if (x_i == paste0("lm", empty_sufx)) {
            if (nlpar == "a" & class == "b" & grepl("a", fixedsi)) {
              if (a_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "all", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "", "", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)[1]
            }
            if (nlpar == "s") {
              if (s_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "all", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "", "", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
            }
          } else {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "lower parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, a numeric value (e.g., 2) or a charater such as zzz",
                "\n with zzz defined in the", 
                "prior_data e.g., prior_data = list(zzz = 2)"
              )
            }
          }
          # checks
          if (nlpar == "s" & !is.null(sncov)) {
            if (length(evaluated_parameter) == 1) {
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            } else if (length(evaluated_parameter) == df) {
              repeach <- nrep_of_parms / df
              evaluated_parameter <-
                rep(evaluated_parameter,
                    times = 1,
                    each = repeach)
            }
          } else {
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          evaluated_parameter <- evaluated_parameter - addrange
          evaluated_parameter_lower <- evaluated_parameter
        }
        
        
        
        if (grepl("^upper$", pname_)) {
          if (x_i == paste0("lm", empty_sufx)) {
            if (nlpar == "a" & class == "b" & grepl("a", fixedsi)) {
              if (a_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "all", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "", "", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)[1]
            }
            if (nlpar == "s") {
              if (s_form_0) {
                lm_gsubby <- paste0("lm", "_", nlpar, "_", "all", resp_)
              } else {
                lm_gsubby <- paste0("lm", "_", nlpar, "", "", resp_)
              }
              eit <-  gsub("lm", lm_gsubby, x_i)
              evaluated_parameter <- ept(eit)
            }
          } else {
            check_evalation_of_numeric_pdata_obj(
              prior_argument,
              p_str_in,
              x_i,
              x,
              pname_,
              dist,
              nlpar,
              class,
              allowed_parm_options,
              splitmvar_w2
            )
            if (is.numeric(eval(parse(text = x_i))) |
                !is.null(eval(parse(text = x_i)))) {
              eit <- x_i
              evaluated_parameter <- ept(eit)
            } else {
              stop(
                "upper parameter options for nlpar ",
                nlpar,
                ", class ",
                class,
                " are:\n lm, a numeric value (e.g., 2) or a charater such as zzz",
                "\n with zzz defined in the prior_data",
                "e.g., prior_data = list(zzz = 2)"
              )
            }
          }
          # checks
          if (nlpar == "s" & !is.null(sncov)) {
            if (length(evaluated_parameter) == 1) {
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            } else if (length(evaluated_parameter) == df) {
              repeach <- nrep_of_parms / df
              evaluated_parameter <-
                rep(evaluated_parameter,
                    times = 1,
                    each = repeach)
            }
          } else {
            if (length(evaluated_parameter) < nrep_of_parms)
              evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
            if (length(evaluated_parameter) > nrep_of_parms)
              stop("prior elements for nlpar ",
                   nlpar, ", class ",  class,
                   " are greater than the parameter dimensions")
          }
          evaluated_parameter <- evaluated_parameter + addrange
          evaluated_parameter_upper <- evaluated_parameter
        }
        
        
        
        # set eta parameter -> for lkj - also for mvr rescor
        
        if (grepl("^eta$", pname_)) {
          check_evalation_of_numeric_pdata_obj(
            prior_argument,
            p_str_in,
            x_i,
            x,
            pname_,
            dist,
            nlpar,
            class,
            allowed_parm_options,
            splitmvar_w2
          )
          if (is.numeric(eval(parse(text = x_i))) |
              !is.null(eval(parse(text = x_i)))) {
            eit <- x_i
            evaluated_parameter <- ept(eit)
          } else {
            stop(
              "df parameter options for nlpar ",
              nlpar,
              ", class ",
              class,
              " are:\n zzz, a numeric value (e.g., 2) or a charater such as zzz",
              "\n with zzz defined in the prior_data",
              "e.g., prior_data = list(zzz = 2)"
            )
          }
          if (length(evaluated_parameter) < nrep_of_parms)
            evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
          if (length(evaluated_parameter) > nrep_of_parms)
            stop("prior elements for nlpar ",
                 nlpar, ", class ",  class,
                 " are greater than the parameter dimensions")
        }
        
        
        # set autocr parameter -> ar ma arma
        
        if (setautocorr & class != "b") {
          check_evalation_of_numeric_pdata_obj(
            prior_argument,
            p_str_in,
            x_i,
            x,
            pname_,
            dist,
            nlpar,
            class,
            allowed_parm_options,
            splitmvar_w2
          )
          if (is.numeric(eval(parse(text = x_i))) |
              !is.null(eval(parse(text = x_i)))) {
            eit <- x_i
            evaluated_parameter <- ept(eit)
          } else {
            stop(
              "df parameter options for nlpar ",
              nlpar,
              ", class ",
              class,
              " are:\n zzzz, a numeric value (e.g., 2) or a charater such as zzz",
              "\n with zzz defined in the prior_data",
              "e.g., prior_data = list(zzz = 2)"
            )
          }
          if (length(evaluated_parameter) < nrep_of_parms)
            evaluated_parameter <- rep(evaluated_parameter, nrep_of_parms)
          if (length(evaluated_parameter) > nrep_of_parms)
            stop("prior elements for nlpar ",
                 nlpar, ", class ",  class,
                 " are greater than the parameter dimensions")
        }
        
        
        
        # make unique names
        
        if (nlpar != "")
          prefix <- nlpar
        if (nlpar != "" &
            cov_nlpar != "")
          prefix <- paste0(cov_nlpar, "_", "cov")
        if (class == 'cor')
          prefix <- 'lkj'
        if (class == 'rescor')
          prefix <- 'lkj'
        if (class == 'sigma')
          prefix <- 'sigma'
        if (setautocorr)
          prefix <- class
        if (dpar != "")
          prefix <- dpar
        
        
        if (sigma_dpar == 'sigma' ) {
          prefix <- 'sigma'
        }
        
        if (cov_sigma_dpar == 'sigma_cov' ) {
          prefix <- 'sigma_cov'
        }
        
        
        
        
        
        if (setautocorr) {
          add_cla_to_name <- NULL
        } else if (class == "" |
                   class == "sigma" | dpar != "" & !is.null(dparncov)) {
          add_cla_to_name <- NULL
        } else {
          add_cla_to_name <- paste0(sep_indicator, class)
        }
        
        
        
        # This is required to set unique stanvar names for higher level sd 
        
        if (class == 'sd' | class == 'cor') {
          group_arg_groupvar_paste <-  group # group_arg_groupvar
          group_arg_groupvar_paste <- gsub(":", "_", group_arg_groupvar_paste)
          suppressWarnings({
            set_str_names[i] <- paste0(set_str_names[i], "_", 
                                       group_arg_groupvar_paste)
          })
        }
        
        
        # This is required to set unique stanvar names for sigma_prior_cor 
        # and gr_prior_cor
        
        if (class == 'cor') {
          group_arg_groupvar_paste <-  group
          if (sigma_dpar == 'sigma') lkj_arg_cor_paste <-  'sigma'
          if (sigma_dpar != 'sigma') lkj_arg_cor_paste <-  'gr'
          set_str_names[i] <- paste0(set_str_names[i], "_", 
                                     lkj_arg_cor_paste)
        }
        
        
        
        
        
        
        name_parameter <-
          paste0(prefix,
                 add_cla_to_name,
                 sep_indicator,
                 set_str_names[i], 
                 resp_)
        
        # name_parameter <- paste0(name_parameter, add_gr_id)
        
        assign(name_parameter, evaluated_parameter)
        
        if (change_default_data_pll_args) {
          stanvars_data[[name_parameter]] <-
            brms::stanvar(
              eval(parse(text = name_parameter)),
              name = name_parameter,
              block = "data",
              pll_args = ept(set_data_pll_args)
            )
        } else {
          stanvars_data[[name_parameter]] <-
            brms::stanvar(eval(parse(text = name_parameter)),
                          name = name_parameter, block = "data")
        }
        
        collect_name_parameter <-
          c(collect_name_parameter, name_parameter)
      }
      
      
      
      
      # assigning bounds
      
      # name_lb <-
      #   paste0(prefix, 
      #          add_cla_to_name, 
      #          sep_indicator, 
      #          "lb",
      #          resp_)
      # 
      # name_ub <-
      #   paste0(prefix, 
      #          add_cla_to_name, 
      #          sep_indicator, 
      #          "ub", 
      #          resp_)
      
      if(is.null(resp_) | resp_ == "") {
        name_lb <- paste0(name_parameter, "_", 'lb')
        name_ub <- paste0(name_parameter, "_", 'ub')
      } else {
        name_lb <- gsub(resp_, paste0("_", 'lb', "", resp_),  name_parameter)
        name_ub <- gsub(resp_, paste0("_", 'ub', "", resp_),  name_parameter)
      }
      
      
      
      if (grepl("^lb$", pname_)  & !grepl("b_", x_i, fixed = T) ) {
        # if (grepl("^lb$", pname_)) {
        if (!(is.na(eval(parse(text = x_i))) |
              eval(parse(text = x_i)) == "NA")) {
          lowerbound <- eval(parse(text = x_i))
          if (length(lowerbound < length(evaluated_parameter))) {
            lowerbound <- rep(lowerbound, length(evaluated_parameter))
          }
          assign(name_lb, lowerbound)
          if (change_default_data_pll_args) {
            stanvars_data[[name_lb]] <- brms::stanvar(
              eval(parse(text = name_lb)),
              name = name_lb,
              block = "data",
              pll_args = ept(set_data_pll_args)
            )
          } else {
            stanvars_data[[name_lb]] <- 
              brms::stanvar(eval(parse(text = name_lb)),
                            name = name_lb, block = "data")
          }
        } else {
          lowerbound <- NA
          if (length(lowerbound < length(evaluated_parameter))) {
            lowerbound <- rep(lowerbound, length(evaluated_parameter))
          }
          assign(name_lb, rep(lowerbound, length(evaluated_parameter)))
        }
      } # if (grepl("^lb$", pname_)) {
      
      # added on 2/8/2023 to allow parameter as lb ub bound 
      
      if (grepl("^lb$", pname_)  & grepl("b_", x_i, fixed = T) ) {
        set_x_i_p_bound <- x_i
        if(resp_ != "") {
          set_x_i_p_bound <- gsub("b_", paste0("b", resp_, "_"), 
                                  set_x_i_p_bound, fixed = T)
        } 
        lowerbound <- set_x_i_p_bound
        assign(name_lb, lowerbound)
      }
      
      
      if (grepl("^ub$", pname_) & !grepl("b_", x_i, fixed = T) ) {
        # if (grepl("^ub$", pname_)) {
        if (!(is.na(eval(parse(text = x_i))) |
              eval(parse(text = x_i)) == "NA")) {
          upperbound <- eval(parse(text = x_i))
          if (length(upperbound < length(evaluated_parameter))) {
            upperbound <- rep(upperbound, length(evaluated_parameter))
          }
          assign(name_ub, upperbound)
          if (change_default_data_pll_args) {
            stanvars_data[[name_ub]] <- brms::stanvar(
              eval(parse(text = name_ub)),
              name = name_ub,
              block = "data",
              pll_args = ept(set_data_pll_args)
            )
          } else {
            stanvars_data[[name_ub]] <- 
              brms::stanvar(eval(parse(text = name_ub)), 
                            name = name_ub, block = "data")
          }
        } else {
          upperbound <- NA
          if (length(upperbound < length(evaluated_parameter))) {
            upperbound <- rep(upperbound, length(evaluated_parameter))
          }
          assign(name_ub, rep(upperbound, length(evaluated_parameter)))
        }
      } # if (grepl("^ub$", pname_)) {
      
      # added on 2/8/2023 to allow parameter as lb ub bound 
      
      if (grepl("^ub$", pname_)  & grepl("b_", x_i, fixed = T) ) {
        set_x_i_p_bound <- x_i
        if(resp_ != "") {
          set_x_i_p_bound <- gsub("b_", paste0("b", resp_, "_"), 
                                  set_x_i_p_bound, fixed = T)
        } 
        upperbound <- set_x_i_p_bound
        assign(name_ub, upperbound)
      }
      
      
      if (grepl("^lb$", pname_)) {
        if (dist == "lognormal" |
            dist == "gamma" | dist == "inv_gamma" | dist == "exponential") {
          if (all(is.na(lowerbound) |
                  lowerbound == "NA"))
            lowerbound <- name_lb
          assign(name_lb, rep(0, length(evaluated_parameter)))
          if (change_default_data_pll_args) {
            stanvars_data[[name_lb]] <- brms::stanvar(
              eval(parse(text = name_lb)),
              name = name_lb,
              block = "data",
              pll_args = ept(set_data_pll_args)
            )
          } else {
            stanvars_data[[name_lb]] <- 
              brms::stanvar(eval(parse(text = name_lb)),
                            name = name_lb, block = "data")
          }
        }
      }
      
      cov_sigma_dpar_mxg <- 
        paste0("It appears you have specified a lower bounded prior",
                 "\n ",
                " '", dist, "' for covariate effect ", 
               cov_sigma_dpar, " with formulation '~1'",
                "\n ",
                 " This does not make sense to restrict 
               the covariate effect strictly positive",
                 "\n ",
                 " Therefore, either use non bounded prior 
               such as 'normal' distribution ",
                "\n ",
                " or else change the formulation to '~0+'")
      
      if (cov_sigma_dpar == 'sigma_cov' & class == 'b' ) {
        if(dist %in% strict_positive_dists) {
          if(!sigma_form_0) stop(cov_sigma_dpar_mxg)
        }
      }
      
      
    } # end of loop for (i in 1:length(x)) {
    
    
    
    
    if (dist == "uniform" ) {
      if (all(is.na(lowerbound) |
              lowerbound == "NA"))
        lowerbound <- name_lb
      assign(name_lb, evaluated_parameter_lower)
      
      if (change_default_data_pll_args) {
        stanvars_data[[name_lb]] <- brms::stanvar(
          eval(parse(text = name_lb)),
          name = name_lb,
          block = "data",
          pll_args = ept(set_data_pll_args)
        )
      } else {
        stanvars_data[[name_lb]] <- 
          brms::stanvar(eval(parse(text = name_lb)),
                        name = name_lb, block = "data")
      }
      
      
      if (all(is.na(upperbound) |
              upperbound == "NA"))
        upperbound <- name_ub
      assign(name_ub, evaluated_parameter_upper)
      
      if (change_default_data_pll_args) {
        stanvars_data[[name_ub]] <- brms::stanvar(
          eval(parse(text = name_ub)),
          name = name_ub,
          block = "data",
          pll_args = ept(set_data_pll_args)
        )
      } else {
        stanvars_data[[name_ub]] <- 
          brms::stanvar(eval(parse(text = name_ub)),
                        name = name_ub, block = "data")
      }
      
      
      if ((identical(evaluated_parameter_lower, evaluated_parameter_upper))) {
        stop(
          "lower and upper parameters for uniform distribution are identical",
          "\n This could be because of same values used for lower and upper",
          "\n", 
          " parameters with addrange set as '0",
          "\n Either change the lower and upper parameter values or set",
          "\n", 
          " addrange other than zero"
        )
      }
      
      for (i in 1:length(evaluated_parameter_lower)) {
        if (evaluated_parameter_lower[i] >= evaluated_parameter_upper[i]) {
          stop(
            "lower parameter value at position '",
            i,
            "' ",
            evaluated_parameter_lower[i],
            "should be less than the specified upper parameter value ",
            evaluated_parameter_lower[i]
          )
        }
      }
    }
    
    
    # name_parameter
    if (dist != "student_nu") {
      if (nrep_of_parms != 1) {
        prior_str_arg_out_c <- c()
        for (i in 1:nrep_of_parms) {
          if (!any(is.na(lowerbound)) | !any(is.na(upperbound))) {
            tt <- paste0(collect_name_parameter, collapse = ", ")
          } else {
            tt <- paste0(collect_name_parameter, "[", i, "]", collapse = ", ")
          }
          prior_str_arg_out_c <- c(prior_str_arg_out_c, tt)
        }
        prior_str_arg_out <- prior_str_arg_out_c
        prior_str_arg_out <- paste0(dist, "(", prior_str_arg_out, ")")
      } else {
        prior_str_arg_out <-
          paste0(dist,
                 "(",
                 paste(collect_name_parameter, collapse = ", "),
                 ")")
      }
    }
    
    
    if (dist == "student_nu") {
      collect_name_parameter_copy <- collect_name_parameter
      collect_name_parameter <-
        c(gsub("_shape", "", collect_name_parameter[1]),
          collect_name_parameter[3:4])
      if (nrep_of_parms != 1) {
        prior_str_arg_out_c <- c()
        for (i in 1:nrep_of_parms) {
          if (!any(is.na(lowerbound)) | !any(is.na(upperbound))) {
            tt <- paste0(collect_name_parameter, collapse = ", ")
          } else {
            tt <- paste0(collect_name_parameter, "[", i, "]", collapse = ", ")
          }
          prior_str_arg_out_c <- c(prior_str_arg_out_c, tt)
        }
        prior_str_arg_out <- prior_str_arg_out_c
        prior_str_arg_out <-
          paste0("student_t", "(", prior_str_arg_out, ")")
      } else {
        prior_str_arg_out <-
          paste0("student_t",
                 "(",
                 paste(collect_name_parameter, collapse = ", "),
                 ")")
      }
    }
    
    
    if (dist == "student_nu") {
      student_nu_left <-
        gsub("_shape", "", collect_name_parameter_copy[1])
      student_nu_right1 <- collect_name_parameter_copy[1]
      student_nu_right2 <- collect_name_parameter_copy[2]
      stanvars_data[[paste0(student_nu_left, "_", "", "_", "pblock")]] <-
        brms::stanvar(
          scode = paste0(
            "vector<lower=1>[",
            nrep_of_parms ,
            "] ",
            student_nu_left,
            ";"
          ),
          block = "parameter",
          position = "end"
        ) # , pll_args = paste0("vector"," ", hptau_nu)
      
      t_or_lp <- 'target'
      dist_student_nu <- "gamma"
      if (nrep_of_parms != 1) {
        for (i in 1:nrep_of_parms) {
          if (normalize) {
            dist_student_1 <-
              "lpdf"
            dist_student_2 <- "lccdf"
            svarblock <- 'model' # 'tparameters'
            define_studentdisttype_1 <-
              paste0(dist_student_nu, "_", dist_student_1)
            define_studentdisttype_2 <-
              paste0(dist_student_nu, "_", dist_student_2)
            define_studentscode_1 <-
              paste0(
                t_or_lp,
                " += ",
                define_studentdisttype_1,
                "(",
                paste0(student_nu_left, "[", i, "]"),
                " | ",
                paste0(student_nu_right1, "[", i, "]") ,
                ", ",
                paste0(student_nu_right2, "[", i, "]") ,
                ")"
              )
            define_studentscode_2 <-
              paste0(
                nrep_of_parms,
                " * ",
                define_studentdisttype_2,
                "(",
                1,
                " | ",
                paste0(student_nu_right1, "[", i, "]") ,
                ", ",
                paste0(student_nu_right2, "[", i, "]") ,
                ")"
              )
            define_studentscode <-
              paste0(define_studentscode_1,
                     "\n    - ",
                     define_studentscode_2,
                     ";")
          } else {
            dist_student_1 <- "lupdf"
            svarblock <- 'model'
            define_studentdisttype_1 <-
              paste0(dist_student_nu, "_", dist_student_1)
            define_studentscode_1 <-
              paste0(
                t_or_lp,
                " += ",
                define_studentdisttype_1,
                "(",
                paste0(student_nu_left, "[", i, "]"),
                " | ",
                paste0(student_nu_right1, "[", i, "]")  ,
                ", ",
                paste0(student_nu_right2, "[", i, "]") ,
                ")"
              )
            define_studentscode <- paste0(define_studentscode_1, ";")
          }
          stanvars_data[[paste0(student_nu_left, "_", i, "_", "mblock")]] <-
            brms::stanvar(scode = define_studentscode,
                          block = svarblock,
                          position = "end")
          
        }
      }
      
      
      if (nrep_of_parms == 1) {
        if (normalize) {
          dist_student_1 <-
            "lpdf"
          dist_student_2 <- "lccdf"
          svarblock <- 'model' # 'tparameters'
          define_studentdisttype_1 <-
            paste0(dist_student_nu, "_", dist_student_1)
          define_studentdisttype_2 <-
            paste0(dist_student_nu, "_", dist_student_2)
          define_studentscode_1 <-
            paste0(
              t_or_lp,
              " += ",
              define_studentdisttype_1,
              "(",
              student_nu_left,
              " | ",
              student_nu_right1,
              ", ",
              student_nu_right2,
              ")"
            )
          define_studentscode_2 <-
            paste0(
              nrep_of_parms,
              " * ",
              define_studentdisttype_2,
              "(",
              1,
              " | ",
              student_nu_right1,
              ", ",
              student_nu_right2,
              ")"
            )
          define_studentscode <-
            paste0(define_studentscode_1,
                   "\n    - ",
                   define_studentscode_2,
                   ";")
        } else {
          dist_student_1 <- "lupdf"
          svarblock <- 'model'
          define_studentdisttype_1 <-
            paste0(dist_student_nu, "_", dist_student_1)
          define_studentscode_1 <-
            paste0(
              t_or_lp,
              " += ",
              define_studentdisttype_1,
              "(",
              student_nu_left,
              " | ",
              student_nu_right1,
              ", ",
              student_nu_right2,
              ")"
            )
          define_studentscode <- paste0(define_studentscode_1, ";")
        }
        stanvars_data[[paste0(student_nu_left, "_", "", "_", "mblock")]] <-
          brms::stanvar(scode = define_studentscode,
                        block = svarblock,
                        position = "end")
        
      }
    }
    
    
    
    
    
    
    if (!is.null(stanvars_data[[name_lb]])) {
      if (nrep_of_parms == 1) {
        lowerbound <- name_lb
      } else {
        if (any(is.na(lowerbound))) {
          lowerbound <- paste0(name_lb, "[", 1:nrep_of_parms, "]")
        } else {
          lowerbound <- name_lb
        }
      }
    }
    
    if (!is.null(stanvars_data[[name_ub]])) {
      if (nrep_of_parms == 1) {
        upperbound <- name_ub
      } else {
        if (any(is.na(upperbound))) {
          upperbound <- paste0(name_ub, "[", 1:nrep_of_parms, "]")
        } else {
          upperbound <- name_ub
        }
      }
    }
    
    
    
    
    if (ept(sethp)) {
      original_scale <-
        paste0(prefix, add_cla_to_name, sep_indicator, "scale", resp_)
      tauid <- 'tau'
      hptau <-
        paste0(prefix, add_cla_to_name, sep_indicator, tauid, resp_)
      prior_str_arg_out <-
        gsub(original_scale, hptau, prior_str_arg_out, fixed = T)
      
      if (dist == "student_t") {
        original_df <-
          paste0(prefix, add_cla_to_name, sep_indicator, "df", resp_)
        original_df_val <-
          ept(ept(
            paste0(prefix, add_cla_to_name, sep_indicator, "df", resp_)
          ))
        hptau_df <-
          paste0(
            prefix,
            add_cla_to_name,
            sep_indicator,
            paste0(tauid, sep_indicator, 'df'),
            resp_
          )
        if (length(original_df_val) < nrep_of_parms)
          original_df_val <- rep(original_df_val, nrep_of_parms)
        assign(hptau_df, original_df_val)
      } else {
        hptau_df <-
          paste0(
            prefix,
            add_cla_to_name,
            sep_indicator,
            paste0(tauid, sep_indicator, 'df'),
            resp_
          )
        assign(hptau_df, rep(3, nrep_of_parms))
      }
      
      if (sethp_dist == "student_nu") {
        hptau_nu <-
          paste0(
            prefix,
            add_cla_to_name,
            sep_indicator,
            paste0(tauid, sep_indicator, 'nu'),
            resp_
          )
        hptau_nu_shape <-
          paste0(
            prefix,
            add_cla_to_name,
            sep_indicator,
            paste0(tauid, sep_indicator, 'nu_shape'),
            resp_
          )
        hptau_nu_scale <-
          paste0(
            prefix,
            add_cla_to_name,
            sep_indicator,
            paste0(tauid, sep_indicator, 'nu_scale'),
            resp_
          )
        assign(hptau_nu_shape, rep(2, nrep_of_parms))
        assign(hptau_nu_scale, rep(0.1, nrep_of_parms))
      }
      
      hptau_scale    <-
        paste0(
          prefix,
          add_cla_to_name,
          sep_indicator,
          paste0(tauid, sep_indicator, 'scale'),
          resp_
        )
      hptau_location <-
        paste0(
          prefix,
          add_cla_to_name,
          sep_indicator,
          paste0(tauid, sep_indicator, 'location'),
          resp_
        )
      
      if (sethp_dist == "exponential") {
        hptau_rate    <-
          paste0(
            prefix,
            add_cla_to_name,
            sep_indicator,
            paste0(tauid, sep_indicator, 'rate'),
            resp_
          )
      }
      
      
      t_or_lp <- 'target'
      if (sethp_dist == "normal" |
          sethp_dist == "cauchy" |
          sethp_dist == "student_t" |
          sethp_dist == "student_nu" |
          sethp_dist == "exponential") {
        if (sethp_dist == "normal") {
          if (normalize) {
            dist_1 <-
              "lpdf"
            dist_2 <- "lccdf"
            svarblock <- 'model' # 'tparameters'
            define_disttype_1 <- paste0(sethp_dist, "_", dist_1)
            define_disttype_2 <- paste0(sethp_dist, "_", dist_2)
            define_scode_1 <-
              paste0(
                t_or_lp,
                " += ",
                define_disttype_1,
                "(",
                hptau,
                " | ",
                hptau_location,
                ", ",
                hptau_scale,
                ")"
              )
            define_scode_2 <-
              paste0(
                nrep_of_parms,
                " * ",
                define_disttype_2,
                "(",
                0,
                " | ",
                hptau_location,
                ", ",
                hptau_scale,
                ")"
              )
            define_scode <-
              paste0(define_scode_1, "\n    - ", define_scode_2, ";")
          } else {
            dist_1 <- "lupdf"
            svarblock <- 'model'
            define_disttype_1 <- paste0(sethp_dist, "_", dist_1)
            define_scode_1 <-
              paste0(
                t_or_lp,
                " += ",
                define_disttype_1,
                "(",
                hptau,
                " | ",
                hptau_location,
                ", ",
                hptau_scale,
                ")"
              )
            define_scode <- paste0(define_scode_1, ";")
          }
          stanvars_data[[paste0(tauid, "_", "mblock")]] <-
            brms::stanvar(scode = define_scode,
                          block = svarblock,
                          position = "end")
        }
        if (sethp_dist == "cauchy") {
          if (normalize) {
            dist_1 <-
              "lpdf"
            dist_2 <- "lccdf"
            svarblock <- 'model' # 'tparameters'
            define_disttype_1 <- paste0(sethp_dist, "_", dist_1)
            define_disttype_2 <- paste0(sethp_dist, "_", dist_2)
            define_scode_1 <-
              paste0(
                t_or_lp,
                " += ",
                define_disttype_1,
                "(",
                hptau,
                " | ",
                hptau_location,
                ", ",
                hptau_scale,
                ")"
              )
            define_scode_2 <-
              paste0(
                nrep_of_parms,
                " * ",
                define_disttype_2,
                "(",
                0,
                " | ",
                hptau_location,
                ", ",
                hptau_scale,
                ")"
              )
            define_scode <-
              paste0(define_scode_1, "\n    - ", define_scode_2, ";")
          } else {
            dist_1 <- "lupdf"
            svarblock <- 'model'
            define_disttype_1 <- paste0(sethp_dist, "_", dist_1)
            define_scode_1 <-
              paste0(
                t_or_lp,
                " += ",
                define_disttype_1,
                "(",
                hptau,
                " | ",
                hptau_location,
                ", ",
                hptau_scale,
                ")"
              )
            define_scode <- paste0(define_scode_1, ";")
          }
          stanvars_data[[paste0(tauid, "_", "mblock")]] <-
            brms::stanvar(scode = define_scode,
                          block = svarblock,
                          position = "end")
        }
        if (sethp_dist == "student_t") {
          if (normalize) {
            dist_1 <-
              "lpdf"
            dist_2 <- "lccdf"
            svarblock <- 'model' # 'tparameters'
            define_disttype_1 <- paste0(sethp_dist, "_", dist_1)
            define_disttype_2 <- paste0(sethp_dist, "_", dist_2)
            define_scode_1 <-
              paste0(
                t_or_lp,
                " += ",
                define_disttype_1,
                "(",
                hptau,
                " | ",
                hptau_df,
                ", ",
                hptau_location,
                ", ",
                hptau_scale,
                ")"
              )
            define_scode_2 <-
              paste0(
                nrep_of_parms,
                " * ",
                define_disttype_2,
                "(",
                0,
                " | ",
                hptau_df,
                ", ",
                hptau_location,
                ", ",
                hptau_scale,
                ")"
              )
            define_scode <-
              paste0(define_scode_1, "\n    - ", define_scode_2, ";")
          } else {
            dist_1 <- "lupdf"
            svarblock <- 'model'
            define_disttype_1 <- paste0(sethp_dist, "_", dist_1)
            define_scode_1 <-
              paste0(
                t_or_lp,
                " += ",
                define_disttype_1,
                "(",
                hptau,
                " | ",
                hptau_df,
                ", ",
                hptau_location,
                ", ",
                hptau_scale,
                ")"
              )
            define_scode <- paste0(define_scode_1, ";")
          }
          stanvars_data[[paste0(tauid, "_", "mblock")]] <-
            brms::stanvar(scode = define_scode,
                          block = svarblock,
                          position = "end")
        }
        if (sethp_dist == "student_nu") {
          sethp_dist_student_t <- "student_t"
          if (normalize) {
            dist_1 <-
              "lpdf"
            dist_2 <- "lccdf"
            svarblock <- 'model' # 'tparameters'
            define_disttype_1 <-
              paste0(sethp_dist_student_t, "_", dist_1)
            define_disttype_2 <-
              paste0(sethp_dist_student_t, "_", dist_2)
            define_scode_1 <-
              paste0(
                t_or_lp,
                " += ",
                define_disttype_1,
                "(",
                hptau,
                " | ",
                hptau_nu,
                ", ",
                hptau_location,
                ", ",
                hptau_scale,
                ")"
              )
            define_scode_2 <-
              paste0(
                nrep_of_parms,
                " * ",
                define_disttype_2,
                "(",
                1,
                " | ",
                hptau_nu,
                ", ",
                hptau_location,
                ", ",
                hptau_scale,
                ")"
              )
            define_scode <-
              paste0(define_scode_1, "\n    - ", define_scode_2, ";")
          } else {
            dist_1 <- "lupdf"
            svarblock <- 'model'
            define_disttype_1 <-
              paste0(sethp_dist_student_t, "_", dist_1)
            define_scode_1 <-
              paste0(
                t_or_lp,
                " += ",
                define_disttype_1,
                "(",
                hptau,
                " | ",
                hptau_nu,
                ", ",
                hptau_location,
                ", ",
                hptau_scale,
                ")"
              )
            define_scode <- paste0(define_scode_1, ";")
          }
          stanvars_data[[paste0(tauid, "_", "mblock")]] <-
            brms::stanvar(scode = define_scode,
                          block = svarblock,
                          position = "end")
          
          # nu gamma
          sethp_dist_student_nu <- "gamma"
          if (normalize) {
            dist_student_1 <-
              "lpdf"
            dist_student_2 <- "lccdf"
            svarblock <- 'model' # 'tparameters'
            define_studentdisttype_1 <-
              paste0(sethp_dist_student_nu, "_", dist_student_1)
            define_studentdisttype_2 <-
              paste0(sethp_dist_student_nu, "_", dist_student_2)
            define_studentscode_1 <-
              paste0(
                t_or_lp,
                " += ",
                define_studentdisttype_1,
                "(",
                hptau_nu,
                " | ",
                hptau_nu_shape,
                ", ",
                hptau_nu_scale,
                ")"
              )
            define_studentscode_2 <-
              paste0(
                nrep_of_parms,
                " * ",
                define_studentdisttype_2,
                "(",
                1,
                " | ",
                hptau_nu_shape,
                ", ",
                hptau_nu_scale,
                ")"
              )
            define_studentscode <-
              paste0(define_studentscode_1,
                     "\n    - ",
                     define_studentscode_2,
                     ";")
          } else {
            dist_student_1 <- "lupdf"
            svarblock <- 'model'
            define_studentdisttype_1 <-
              paste0(sethp_dist_student_nu, "_", dist_student_1)
            define_studentscode_1 <-
              paste0(
                t_or_lp,
                " += ",
                define_studentdisttype_1,
                "(",
                hptau_nu,
                " | ",
                hptau_nu_shape,
                ", ",
                hptau_nu_scale,
                ")"
              )
            define_studentscode <- paste0(define_studentscode_1, ";")
          }
          stanvars_data[[paste0(tauid, "_", "nu", "_", "mblock")]] <-
            brms::stanvar(scode = define_studentscode,
                          block = svarblock,
                          position = "end")
        }
        
        if (sethp_dist == "exponential") {
          if (normalize) {
            dist_1 <-
              "lpdf"
            dist_2 <- "lccdf"
            svarblock <- 'model' # 'tparameters'
            define_disttype_1 <- paste0(sethp_dist, "_", dist_1)
            define_disttype_2 <- paste0(sethp_dist, "_", dist_2)
            define_scode_1 <-
              paste0(t_or_lp,
                     " += ",
                     define_disttype_1,
                     "(",
                     hptau,
                     " | ",
                     hptau_rate,
                     ")")
            define_scode_2 <-
              paste0(nrep_of_parms,
                     " * ",
                     define_disttype_2,
                     "(",
                     0,
                     " | ",
                     hptau_rate,
                     ")")
            define_scode <-
              paste0(define_scode_1, "\n    - ", define_scode_2, ";")
          } else {
            dist_1 <- "lupdf"
            svarblock <- 'model'
            define_disttype_1 <- paste0(sethp_dist, "_", dist_1)
            define_scode_1 <-
              paste0(t_or_lp,
                     " += ",
                     define_disttype_1,
                     "(",
                     hptau,
                     " | ",
                     hptau_rate,
                     ")")
            define_scode <- paste0(define_scode_1, ";")
          }
          stanvars_data[[paste0(tauid, "_", "mblock")]] <-
            brms::stanvar(scode = define_scode,
                          block = svarblock,
                          position = "end")
        }
      } else {
        sethp_dist <- 'normal'
        if (normalize) {
          dist_1 <-
            "lpdf"
          dist_2 <- "lccdf"
          svarblock <- 'model' # 'tparameters'
          define_disttype_1 <- paste0(sethp_dist, "_", dist_1)
          define_disttype_2 <- paste0(sethp_dist, "_", dist_2)
          define_scode_1 <-
            paste0(
              t_or_lp,
              " += ",
              define_disttype_1,
              "(",
              hptau,
              " | ",
              hptau_location,
              ", ",
              hptau_scale,
              ")"
            )
          define_scode_2 <-
            paste0(
              nrep_of_parms,
              " * ",
              define_disttype_2,
              "(",
              0,
              " | ",
              hptau_location,
              ", ",
              hptau_scale,
              ")"
            )
          define_scode <-
            paste0(define_scode_1, "\n    - ", define_scode_2, ";")
        } else {
          dist_1 <- "lupdf"
          svarblock <- 'model'
          define_disttype_1 <- paste0(sethp_dist, "_", dist_1)
          define_scode_1 <-
            paste0(
              t_or_lp,
              " += ",
              define_disttype_1,
              "(",
              hptau,
              " | ",
              hptau_location,
              ", ",
              hptau_scale,
              ")"
            )
          define_scode <- paste0(define_scode_1, ";")
        }
        stanvars_data[[paste0(tauid, "_", "mblock")]] <-
          brms::stanvar(scode = define_scode,
                        block = svarblock,
                        position = "end")
      }
      
      stanvars_data[[paste0(tauid, "_", "pblock")]] <-
        brms::stanvar(
          scode = paste0("vector<lower=0>[", nrep_of_parms , "] ", hptau, ";"),
          block = "parameter",
          position = "end"
        ) 
      
      
      if (sethp_dist == "student_nu") {
        stanvars_data[[paste0(tauid, "_", "nu", "_", "pblock")]] <-
          brms::stanvar(
            scode = paste0("vector<lower=1>[", 
                           nrep_of_parms , "] ", hptau_nu, ";"),
            block = "parameter",
            position = "end"
          ) 
      }
      
      # add data stanvars
      if (sethp_dist == "normal" |
          sethp_dist == "cauchy" |
          sethp_dist == "student_nu" |
          sethp_dist == "student_t") {
        if (sethp_dist == "student_t") {
          if (change_default_data_pll_args) {
            stanvars_data[[hptau_df]] <- brms::stanvar(
              ept(hptau_df),
              name = hptau_df,
              block = "data",
              pll_args = ept(set_data_pll_args)
            )
          } else {
            stanvars_data[[hptau_df]] <- 
              brms::stanvar(ept(hptau_df),
                            name = hptau_df, 
                            block = "data")
          }
        }
        
        
        if (sethp_dist == "student_nu") {
          if (change_default_data_pll_args) {
            stanvars_data[[hptau_nu_shape]] <- brms::stanvar(
              ept(hptau_nu_shape),
              name = hptau_nu_shape,
              block = "data",
              pll_args = ept(set_data_pll_args)
            )
            
            stanvars_data[[hptau_nu_scale]] <-
              brms::stanvar(
                ept(hptau_nu_scale),
                name = hptau_nu_scale,
                block = "data",
                pll_args = ept(set_data_pll_args)
              )
          } else {
            stanvars_data[[hptau_nu_shape]] <- 
              brms::stanvar(ept(hptau_nu_shape),
                            name = hptau_nu_shape,
                            block = "data")
            
            stanvars_data[[hptau_nu_scale]] <-
              brms::stanvar(ept(hptau_nu_scale),
                            name = hptau_nu_scale,
                            block = "data")
          }
        }
        
        
        if (change_default_data_pll_args) {
          stanvars_data[[hptau_location]] <- brms::stanvar(
            rep(0, nrep_of_parms),
            name = hptau_location,
            block = "data",
            pll_args = ept(set_data_pll_args)
          )
        } else {
          stanvars_data[[hptau_location]] <- 
            brms::stanvar(rep(0, nrep_of_parms),
                          name = hptau_location,
                          block = "data")
        }
        
        if (change_default_data_pll_args) {
          stanvars_data[[hptau_scale]]    <-
            brms::stanvar(
              eval(parse(text = original_scale)),
              name = hptau_scale,
              block = "data",
              pll_args = ept(set_data_pll_args)
            )
        } else {
          stanvars_data[[hptau_scale]]    <-
            brms::stanvar(eval(parse(text = original_scale)),
                          name = hptau_scale, block = "data")
        }
      }
      
      
      if (sethp_dist == "exponential") {
        if (change_default_data_pll_args) {
          stanvars_data[[hptau_rate]] <- brms::stanvar(
            1 / ept(original_scale),
            name = hptau_rate,
            block = "data",
            pll_args = ept(set_data_pll_args)
          )
        } else {
          stanvars_data[[hptau_rate]] <- 
            brms::stanvar(1 / ept(original_scale),
                          name = hptau_rate,
                          block = "data")
        }
      }
      stanvars_data[[original_scale]] <- NULL
    }
    
    
  } # end if(dist != 'flat') 
  
  
  if(dist == 'flat') {
    prior_str_arg_out <- ""
    lowerbound <- NA
    upperbound <- NA
    stanvars_data <- NULL
    allowed_init_options_beta <- NULL
    allowed_init_options_sd <- NULL
    allowed_init_options_rate <- NULL
    allowed_init_options_shape <- NULL
    allowed_init_options_scale <- NULL
  }
  
  
  # initials
  if (initsi != "random") {
    # parm <- nlpar
    if(sigma_dpar == 'sigma')  {
      parm <- sigma_dpar
    } else {
      parm <- nlpar
    }
    stanvars_datazz <- stanvars_data
    pstrarg <- prior_str_arg_out
    
    init_internal_args_names <- c(
      'parm',
      'class',
      'dpar',
      'sigma_dpar',
      'resp_',
      'dist',
      'lowerbound',
      'upperbound',
      'allowed_init_options_beta',
      'allowed_init_options_sd',
      'allowed_init_options_rate',
      'allowed_init_options_shape',
      'allowed_init_options_scale',
      'stanvars_datazz',
      'pstrarg',
      'initsi',
      'init_arguments',
      'init_data',
      'init_data_internal',
      'init_args_internal',
      'prior_data',
      'prior_data_internal',
      'prior_internal_args',
      'splitmvar_w2',
      'seed'
    )
    
    
    init_internal_args <- mget(init_internal_args_names)
    init_argument <- gsub("_prior_", "_init_", prior_argument)
    initial_out <-
      prepare_initials(init_argument = init_argument, init_internal_args)
  } else {
    initial_out <- NULL
  }
  
  
  return(
    list(
      prior_str_arg = prior_str_arg_out,
      lowerbound = lowerbound,
      upperbound = upperbound,
      stanvars_data = stanvars_data,
      initial_out = initial_out
    )
  )
}


