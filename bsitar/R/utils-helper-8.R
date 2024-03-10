

#' An internal function to prepare initial values
#' 
#' For \code{univariate_by} and \code{multivariate} models (see [bsitar::bsitar()])
#' each argument is automatically matched with the sub model.
#'
#' @param init_argument A list containing the prior arguments specified in  
#' the [bsitar::bsitar()] function and then passed from the 
#' [bsitar::set_priors_initials()] function to the \code{prepare_priors}. 
#' 
#' @param init_internal_args An internal argument (as named list) specified in  
#' the [bsitar::bsitar()] function and then passed from the 
#' [bsitar::set_priors_initials()] function to the \code{prepare_priors}. 
#'
#' @return A list of initial values. 
#' 
#' @author Satpal Sandhu  \email{satpal.sandhu@bristol.ac.uk}
#' 
#' @keywords internal
#' @noRd
#' 
prepare_initials <- function(init_argument,
                             init_internal_args) {
  
  
  
  ##############################################
  # Initiate non formalArgs()
  ##############################################
  seed <- NULL;
  initsi <- NULL;
  parm <- NULL;
  randomsi <- NULL;
  init_data <- NULL;
  prior_argument <- NULL;
  pstrarg <- NULL;
  resp_ <- NULL;
  allowed_init_options_beta <- NULL;
  nlpar <- NULL;
  stanvars_datazz <- NULL;
  splitmvar_w2 <- NULL;
  ii <- NULL;
  allowed_init_options_sd <- NULL;
  allowed_init_options_rate <- NULL;
  allowed_init_options_shape <- NULL;
  nabcrei <- NULL;
  N_J_all <- NULL;
  nys <- NULL;
  dpar <- NULL;
  ndparcov <- NULL;
  acorclass <- NULL;
  nrep_of_parms_p <- NULL;
  nrep_of_parms_q <- NULL;
  cortimeNlags <- NULL;
  gr_init_cor <- NULL;
  
  
  if (!is.null(init_internal_args)) {
    eout <- list2env(init_internal_args)
    for (eoutii in names(eout)) {
      assign(eoutii, eout[[eoutii]])
    }
  }
  
  if (!is.null(init_internal_args$init_argument)) {
    eout <- list2env(init_internal_args$init_argument)
    for (eoutii in names(eout)) {
      assign(eoutii, eout[[eoutii]])
    }
  }
  
  if (!is.null(init_internal_args$prior_data)) {
    eout <- list2env(init_internal_args$prior_data)
    for (eoutii in names(eout)) {
      assign(eoutii, eout[[eoutii]])
    }
  }
  
  if (!is.null(init_internal_args$prior_data_internal)) {
    eout <- list2env(init_internal_args$prior_data_internal)
    for (eoutii in names(eout)) {
      assign(eoutii, eout[[eoutii]])
    }
  }
  
  if (!is.null(init_internal_args$prior_internal_args)) {
    eout <- list2env(init_internal_args$prior_internal_args)
    for (eoutii in names(eout)) {
      assign(eoutii, eout[[eoutii]])
    }
  }
  
  
  if (!is.null(init_internal_args$init_data)) {
    eout <- list2env(init_internal_args$init_data)
    for (eoutii in names(eout)) {
      assign(eoutii, eout[[eoutii]])
    }
  }
  
  if (!is.null(init_internal_args$init_data_internal)) {
    eout <- list2env(init_internal_args$init_data_internal)
    for (eoutii in names(eout)) {
      assign(eoutii, eout[[eoutii]])
    }
  }
  
  
  
  set.seed(seed)
  
  # For standardized group level effects as implemented in the centerized 
  # parametrisation approach used in the brms package. 
  init_argument_z <- "r_init_z"
  
  if (initsi == "0") {
    assign(init_argument, "0")
  }
  if (initsi == "prior") {
    if(system.file(package='extraDistr') == "") {
      stop("For prior based initials (i.e., init = 'prior), 
           package 'extraDistr' is required. Please install 'extraDistr'"
      )
    }
    assign(init_argument, "prior")
    assign(init_argument_z, "prior")
  }
  
  check_form_0       <- paste0(parm, "_", 'form_0')
  nparcov            <- paste0(parm, "ncov")
  check_form_0_gr    <- paste0(parm, "_", 'form_0_gr')
  nparcov_gr         <- paste0(parm, "ncov_gr")
  check_sigma_form_0 <- paste0('sigma', "_", 'form_0')
  
  if(nparcov == 'nsigmacov') nparcov <- 'nsigmacov'
  
  abcrandomelements <-
    strsplit(gsub("\\+", " ", randomsi), " ")[[1]]
  
  
  if (!is.null(init_data[[1]])) {
    eout <- list2env(init_data)
    for (eoutii in names(eout)) {
      assign(eoutii, eout[[eoutii]])
    }
  }
  
  
  
  
  abcrandomelements_c <- c()
  count_ <- 0
  for (abcrandomelements_i in abcrandomelements) {
    count_ <- count_ + 1
    if (exists(paste0(abcrandomelements_i, "n",  "cov_gr")) &
        length(ept(paste0(abcrandomelements_i, "n",  "cov_gr"))) > 0) {
      i__ <- ept(paste0(abcrandomelements_i, "n",  "cov_gr"))
      nb <- rep(paste0(abcrandomelements_i, "cov"), i__ - 1)
      nb <- c(nb, paste0(nb, 1:length(nb)))
      abcrandomelements_c <- c(abcrandomelements_c, nb)
    } else {
      abcrandomelements_c <- c(abcrandomelements_c, abcrandomelements_i)
    }
  }
  
  abcrandomelements <- abcrandomelements_c
  abcrandomelements <- unique(abcrandomelements)
  
  c_t_nabcri <- length(abcrandomelements)
  # nabcri <- length(c_t_nabcri)
  
  
  suffix <-
    strsplit(init_argument, "_")[[1]][length(strsplit(init_argument, "_")[[1]])]
  
  
  # define function to check validity of the initials options specified
  check_evalation_of_numeric_init_obj <-
    function(eit,
             check,
             x,
             pname_,
             dist,
             nlpar,
             class,
             allowed_init_options,
             splitmvar_w2) {
      whatin <-
        sub("=[^=]+$", "", splitmvar_w2[grepl(eit, splitmvar_w2)])
      const_msg <-
        paste0(
          " - a numeric value (e.g., 2) or a charater string such as",
          "\n",
          "xxx with xxx defined in the use-specified 'prior_data'",
          "\n",
          "argument e.g., prior_data = list(xxx = 2)"
        )
      if (!is.null(allowed_init_options)) {
        allowed_init_options <-
          paste0("random,",
                 " 0, ",
                 paste0(allowed_init_options, collapse = ", "))
        allowed_init_options <- paste0(" - ", allowed_init_options)
        const_msg <- paste0(allowed_init_options, "\n", const_msg)
      } else {
        const_msg <- paste0("random,", " 0, ", "Or ", const_msg)
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
      if (eit == 'NULL' | eit == 'random')
        enverr. <- parent.frame()
        assign('err.', FALSE, envir = enverr.)
      if (err.) {
        if (check == 'args') {
          if (class == 'b' | class == 'sd') {
            stop(
              "\nFor nlpar ",
              nlpar,
              ", class ",
              class,
              ", you have specified '",
              eit,
              "' as an initial argument",
              "\n" ,
              " But '",
              eit,
              "' is not found in the 'init_data_internal'",
              "\ n",
              " or use-specified 'init_data' argument",
              "\n ",
              " [see specified init argument: ",
              init_argument,
              "",
              "",
              "]",
              "\n" ,
              "Avilable  options are:" ,
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
              "' as an initial argument",
              "\n" ,
              " But '",
              eit,
              "' is not found in the 'init_data_internal'",
              "\ n",
              " or use-specified 'init_data' argument",
              "\n ",
              " [see specified init argument: ",
              init_argument,
              "",
              "",
              "]",
              "\n" ,
              "Avilable  options are:" ,
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
              "' as an initial argument",
              "\n" ,
              " But '",
              eit,
              "' is not found in the 'init_data_internal'",
              "\ n",
              " or use-specified 'init_data' argument",
              "\n ",
              " [see specified init argument: ",
              init_argument,
              "",
              "",
              "]",
              "\n" ,
              "Avilable  options are:" ,
              "\n" ,
              const_msg
            )
          }
          
        }
        if (check == 'dist') {
          stop(
            "For nlpar ",
            nlpar,
            ", class ",
            class,
            ", you have specified '",
            eit,
            "' as ",
            "\n" ,
            pname_,
            " for the ",
            dist,
            " distribution. But '",
            eit,
            "' is not found (check init_data!)",
            "\n" ,
            "Avilable options for the ",
            pname_,
            " parameter of " ,
            dist,
            " distribution ",
            "\n" ,
            "for nlpar ",
            nlpar,
            ", class ",
            class,
            " are:",
            "\n" ,
            const_msg
          )
        }
        # rm(err.)
      }
    }
  
  
  # define function to create correlation matrix
  create_cor_mat <- function(n, cor = NULL) {
    n_elements <- n
    m <- diag(n_elements)
    m_upper <- m_lower <- matrix(0, n_elements, n_elements)
    nc <- n_elements * (n_elements - 1) / 2
    if (is.null(cor)) {
      x <- rep(0, nc)
    } else {
      x <- cor
      if (length(x) != nc)
        stop("length of correlation vector must be ",
             nc,
             ", but found ",
             length(x))
    }
    m_lower[lower.tri(m_lower, diag = FALSE)] <- x
    m_upper <- t(m_lower)
    M <- m_lower + m + m_upper
    M
  }
  
  
  # define function evaluation initials based on priors
  eval_prior_based_init <-
    function(dist,
             class,
             lowerbound,
             upperbound,
             length_args,
             ...) {
      if (dist == "normal") {
        z_replace_itn    <- paste0(dist, "\\(")
        z_replace_itb    <- paste0('', ")")
        z_replace_byn    <-
          paste0("extraDistr::rtnorm", "\\(", length_args, ", ")
        if (any(is.na(lowerbound)))
          lowerbound <- '-Inf'
        if (any(is.na(upperbound)))
          upperbound <- 'Inf'
        z_replace_byb   <-
          paste0(",",  paste(lowerbound, upperbound, sep = ",")   , ")")
        init_str_arg_out_init <-
          gsub(z_replace_itn, z_replace_byn, pstrarg)
        init_str_arg_out_init <-
          gsub(z_replace_itb, z_replace_byb, init_str_arg_out_init)
        init_str_arg_out_init <-
          gsub("\\s", "", init_str_arg_out_init)
      } else if (dist == "cauchy") {
        z_replace_itn    <- paste0(dist, "\\(")
        z_replace_itb    <- paste0('', ")")
        if (is.na(ept(lowerbound)) & is.na(ept(upperbound))) {
          z_replace_byn    <- paste0("rcauchy", "\\(", length_args, ", ")
          init_str_arg_out_init <-
            gsub(z_replace_itn, z_replace_byn, pstrarg)
        } else if (ept(lowerbound) == 0 & is.na(upperbound)) {
          z_replace_byn    <-
            paste0("extraDistr::rhcauchy", "\\(", length_args, ", ")
          init_str_arg_out_init <-
            gsub(z_replace_itn, z_replace_byn, pstrarg)
          init_str_arg_out_init    <-
            paste(strsplit(init_str_arg_out_init, ",")[[1]][-2], collapse = ",")
          init_str_arg_out_init <-
            gsub("\\s", "", init_str_arg_out_init)
        } else {
          stop(
            "For ",
            dist,
            " distribution prior based initials,",
            "\n ",
            " allowed options are unbounded distribution or half ",
            dist,
            " (i.e, lb = 0)",
            "\n ",
            " please check following initial argument: ",
            name_initialsi
          )
        }
      } else if (dist == "student_t") {
        z_replace_itn    <- paste0(dist, "\\(")
        z_replace_itb    <- paste0('', ")")
        if (is.na(ept(lowerbound)) & is.na(ept(upperbound))) {
          z_replace_byn    <-
            paste0("extraDistr::rlst", "\\(", length_args, ", ")
          init_str_arg_out_init <-
            gsub(z_replace_itn, z_replace_byn, pstrarg)
        } else if (ept(lowerbound) == 0 & is.na(upperbound)) {
          z_replace_byn    <-
            paste0("extraDistr::rht", "\\(", length_args, ", ")
          init_str_arg_out_init <-
            gsub(z_replace_itn, z_replace_byn, pstrarg)
          init_str_arg_out_init    <-
            paste(strsplit(init_str_arg_out_init, ",")[[1]][-3], collapse = ",")
          init_str_arg_out_init <-
            gsub("\\s", "", init_str_arg_out_init)
        } else {
          stop(
            "For ",
            dist,
            " distribution prior based initials,",
            "\n ",
            " allowed options are unbounded distribution or half ",
            dist,
            " (i.e, lb = 0)",
            "\n ",
            " please check following initial argument: ",
            name_initialsi
          )
        }
      } else if (dist == "gamma") {
        z_replace_itn    <- paste0(dist, "\\(")
        z_replace_byn    <-
          paste0("rgamma", "\\(", length_args, ", ")
        init_str_arg_out_init <-
          gsub(z_replace_itn, z_replace_byn, pstrarg)
      } else if (dist == "lognormal ") {
        z_replace_itn    <- paste0(dist, "\\(")
        z_replace_byn    <-
          paste0("rlnorm", "\\(", length_args, ", ")
        init_str_arg_out_init <-
          gsub(z_replace_itn, z_replace_byn, pstrarg)
      } else if (dist == "exponential") {
        z_replace_itn    <- paste0(dist, "\\(")
        z_replace_byn    <- paste0("rexp", "\\(", length_args, ", ")
        init_str_arg_out_init <-
          gsub(z_replace_itn, z_replace_byn, pstrarg)
      } else if (dist == "inv_gamma") {
        z_replace_itn    <- paste0(dist, "\\(")
        z_replace_byn    <-
          paste0("extraDistr::rinvgamma", "\\(", length_args, ", ")
        init_str_arg_out_init <-
          gsub(z_replace_itn, z_replace_byn, pstrarg)
      } else if (dist == "uniform") {
        z_replace_itn    <- paste0(dist, "\\(")
        z_replace_byn    <-
          paste0("runif", "\\(", length_args, ", ")
        init_str_arg_out_init <-
          gsub(z_replace_itn, z_replace_byn, pstrarg)
      } else if (dist == "xxxxxxxxx") {
        # placeholder for fututre expansion
      }
      
      if (class == 'cor' | class == 'rescor') {
        setlkjcorr <- function (K, eta = 1) {
          # set number of realizations
          n <- 100
          stopifnot(is.numeric(K), K >= 2, K == as.integer(K))
          stopifnot(eta > 0)
          #if (K == 1) return(matrix(1, 1, 1))
          f <- function() {
            alpha <- eta + (K - 2) / 2
            r12 <- 2 * rbeta(1, alpha, alpha) - 1
            R <- matrix(0, K, K)
            R[1, 1] <- 1
            R[1, 2] <- r12
            R[2, 2] <- sqrt(1 - r12 ^ 2)
            if (K > 2)
              for (m in 2:(K - 1)) {
                alpha <- alpha - 0.5
                y <- rbeta(1, m / 2, alpha)
                z <- rnorm(m, 0, 1)
                z <- z / sqrt(crossprod(z)[1])
                R[1:m, m + 1] <- sqrt(y) * z
                R[m + 1, m + 1] <- sqrt(1 - y)
              }
            return(crossprod(R))
          }
          R <- replicate(n , f())
          if (dim(R)[3] == 1) {
            R <- R[, , 1]
          } else {
            R <- aperm(R, c(3, 1, 2))
          }
          return(R)
        }
        
        z_replace_itn    <- paste0(dist, "\\(")
        z_replace_byn    <-
          paste0("setlkjcorr", "\\(", length_args, ", ")
        init_str_arg_out_init <-
          gsub(z_replace_itn, z_replace_byn, pstrarg)
        Rcor <- ept(init_str_arg_out_init)
        c_r_list <- list()
        for (i in 1:dim(Rcor)[1])
          c_r_list[[i]] <-  Rcor[i:i, , ]
        Rcoravg <- Reduce("+", c_r_list) / length(c_r_list)
        outcor <- Rcoravg
      }
      
      if (class != 'cor' & class != 'rescor') {
        out  <- ept(init_str_arg_out_init)
        if (dist == "exponential")
          out <- 1 / out
      } else if (class == 'cor' | class == 'rescor') {
        out  <- outcor
      }
      
      out
    }
  
  
  
  ################################################
  # class b - beta
  if ((class == 'b' & suffix == 'beta') |
      class == 'b' & suffix == 'beta' & ept("sigma_dpar") == "sigma") {
    
    if(ept("sigma_dpar") == "sigma") {
      name_parm <- paste0(class, "_", parm, resp_)
    } else {
      name_parm <- paste0(class, resp_, "_", parm)
    }
    
    suffix <- 'beta'
    
    allowed_init_options <- allowed_init_options_beta
    
    if(!exists('allowed_init_options')) allowed_init_options <- NULL
    
    lowerbound <- lowerbound
    upperbound <- upperbound
    
    out_list <- list_collect <- list()
    start_cnt <- 0
    for (name_initialsi in init_argument) {
      if (grepl("_cov", name_initialsi))
        eit_cov <- TRUE
      else
        eit_cov <- FALSE
      
      start_cnt <- start_cnt + 1
      if (ept(name_initialsi) != 'NULL') {
        if (ept(name_initialsi) != 'random') {
          if (ept(name_initialsi) == '0') {
            evaluated_init <- rep(0, nrep_of_parms)
          } else if (ept(name_initialsi) == 'lm') {
            if (ept(check_form_0)) {
              lm_gsubby <- paste0("lm", "_", nlpar, "_", "all", resp_)
            }
            if (!ept(check_form_0)) {
              if (!eit_cov)
                lm_gsubby <- paste0("lm", "_", nlpar, "", "", resp_)
              if (eit_cov)
                lm_gsubby <-
                  paste0("lm", "_", nlpar, "_", "cov", resp_)
            }
            
            evaluated_init <- ept(lm_gsubby) %>% unname()
          } else if (ept(name_initialsi) == 'ymean') {
            eit <- gsub("ymean",
                        paste0("ymean", resp_),
                        ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'ymax') {
            eit <- gsub("ymax",
                        paste0("ymax", resp_),
                        ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'ymaxs') {
            eit <- gsub("ymaxs",
                        paste0("ymaxs", resp_),
                        ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'median') {
            eit <- gsub("median",
                        paste0("median", resp_),
                        ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'bstart') {
            eit <- gsub("bstart",
                        paste0("bstart", resp_),
                        ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'cstart') {
            eit <- gsub("cstart",
                        paste0("cstart", resp_),
                        ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'dstart') {
            eit <- gsub("dstart",
                        paste0("dstart", resp_),
                        ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'estart') {
            eit <- gsub("estart",
                        paste0("estart", resp_),
                        ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
            
          } else if (ept(name_initialsi) == 'ymeanxmin') {
            eit <- gsub("ymeanxmin",
                        paste0("ymeanxmin", resp_),
                        ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'ymeanxmax') {
            eit <- gsub("ymeanxmax",
                        paste0("ymeanxmax", resp_),
                        ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'ymeanxmid') {
            eit <- gsub("ymeanxmid",
                        paste0("ymeanxmid", resp_),
                        ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'prior') {
            stanvname_ <- unique(unlist(lapply(stanvars_datazz, names)))
            stanvname_cnt <- 0
            for (stanvname_i in stanvname_) {
              stanvname_cnt <- stanvname_cnt + 1
              stanvname_cnt_name <-
                stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$name
              assign(stanvname_cnt_name,
                     stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$sdata)
              length_args <-
                length(stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$sdata)
            }
            evaluated_init <-
              eval_prior_based_init(
                dist = dist,
                class = class,
                lowerbound = lowerbound,
                upperbound = upperbound,
                length_args = length_args
              )
          } else {
            check_evalation_of_numeric_init_obj(
              ept(name_initialsi),
              check = 'args',
              x = name_initialsi,
              pname_ = 'xxx',
              dist = dist,
              nlpar = parm,
              class = class,
              allowed_init_options = allowed_init_options,
              splitmvar_w2 = splitmvar_w2
            )
            name_initialsi <- ept(name_initialsi)
            if (is.numeric(ept(name_initialsi)) |
                !is.null(ept(name_initialsi))) {
              evaluated_init <- ept(name_initialsi)
            }
          }
        } else if (ept(name_initialsi) == 'random') {
          evaluated_init <- NULL
        }
      } else if (ept(name_initialsi) == 'NULL') {
        evaluated_init <- NULL
      }
      list_collect[[name_initialsi]] <- evaluated_init
    }
    
    if (!is.null(list_collect[[name_initialsi]])) {
      tempv <- list_collect %>% unlist() %>% unname()
      tempv <- as.numeric(tempv)
      out_list[[name_parm]] <- tempv
    } else {
      out_list[[name_parm]] <- NULL
    }
  }
  
  
  
  if (class == 'b' & suffix == 'beta' & ept("sigma_dpar") != "sigma") {
    if(ept(check_form_0)) {
      if(length(out_list[[name_parm]]) == 1) {
        out_list[[name_parm]] <- rep(out_list[[name_parm]], ept(nparcov))
      }
    }
  }
  
  
  # Like a b c d e beta when ~ 0 +..., init for sigma are rep of sigma_init_beta 
  if (class == 'b' & suffix == 'beta' & ept("sigma_dpar") == "sigma") {
    if(ept(ept("init_argument")) != 'random' ) {
      if(ept("sigma_form_0")) {
        out_list[[name_parm]] <- rep(out_list[[name_parm]], ept(nparcov))
      } else if(!ept("sigma_form_0")) {
        if(eit_cov) {
          addcovsigma <- unlist(ept(ept(ept("init_argument"))))
          addcovsigma_n <- ept(nparcov) - 1
          if(addcovsigma_n == length(addcovsigma)) {
            addcovsigma <- addcovsigma
          } else if(length(addcovsigma) == 1) {
            addcovsigma <- rep(addcovsigma, addcovsigma_n)
          }
          if(!is.null(ept(nparcov))) out_list[[name_parm]] <-addcovsigma
        }
      } 
    } 
  } 
  
  
  ################################################
  # class sd - sd
  if (class == 'sd' & suffix == 'sd' & ept("sigma_dpar") != "sigma" |
      class == 'sd' & suffix == 'sd' & ept("sigma_dpar") == "sigma"
  ) {
    name_parm <- paste0('sd', "_", ii)
    suffix <- 'sd'
    
    if (dist == 'normal' |
        dist == 'cauchy' |
        dist == 'student_t' | dist == 'student_nu') {
      allowed_init_options <- allowed_init_options_sd
    } else if (dist == 'exponential') {
      allowed_init_options <- allowed_init_options_rate
    } else if (dist == 'gamma') {
      allowed_init_options <- allowed_init_options_shape
    } else {
      allowed_init_options <- NULL
    }
    
    if(!exists('allowed_init_options')) allowed_init_options <- NULL
    
    lowerbound <- 0
    upperbound <- upperbound
    
    out_list <- list_collect <- list()
    start_cnt <- 0
    for (name_initialsi in init_argument) {
      if (grepl("_cov", name_initialsi))
        eit_cov <- TRUE
      else
        eit_cov <- FALSE
      if (eit_cov)
        addcovname <- 'cov'
      else
        addcovname <- NULL
      
      start_cnt <- start_cnt + 1
      if (ept(name_initialsi) != 'NULL') {
        if (ept(name_initialsi) != 'random') {
          if (ept(name_initialsi) == '0') {
            evaluated_init <- rep(0, nrep_of_parms)
          } else if (ept(name_initialsi) == 'ysd') {
            eit <- gsub("ysd", paste0("ysd", resp_), ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'ymad') {
            eit <- gsub("ymad",
                        paste0("ymad", resp_),
                        ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'lme_sd_a') {
            eit <-
              gsub("lme_sd_a",
                   paste0("lme_sd_a", resp_),
                   ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
            
            
          } else if (ept(name_initialsi) == 'ysdxmin') {
            eit <-
              gsub("ysdxmin",
                   paste0("ysdxmin", resp_),
                   ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'ysdxmax') {
            eit <-
              gsub("ysdxmax",
                   paste0("ysdxmax", resp_),
                   ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'ysdxmid') {
            eit <-
              gsub("ysdxmid",
                   paste0("ysdxmid", resp_),
                   ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'ysdxmidxmaxdiff') {
            eit <-
              gsub("ysdxmidxmaxdiff",
                   paste0("ysdxmidxmaxdiff", resp_),
                   ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'prior') {
            stanvname_ <- unique(unlist(lapply(stanvars_datazz, names)))
            stanvname_cnt <- 0
            for (stanvname_i in stanvname_) {
              stanvname_cnt <- stanvname_cnt + 1
              stanvname_cnt_name <-
                stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$name
              assign(stanvname_cnt_name,
                     stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$sdata)
              length_args <-
                length(stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$sdata)
            }
            evaluated_init <-
              eval_prior_based_init(
                dist = dist,
                class = class,
                lowerbound = lowerbound,
                upperbound = upperbound,
                length_args = length_args
              )
          } else {
            check_evalation_of_numeric_init_obj(
              ept(name_initialsi),
              check = 'args',
              x = name_initialsi,
              pname_ = 'xxx',
              dist = 'dis',
              nlpar = parm,
              class = class,
              allowed_init_options = allowed_init_options,
              splitmvar_w2 = splitmvar_w2
            )
            name_initialsi <- ept(name_initialsi)
            if (is.numeric(ept(name_initialsi)) |
                !is.null(ept(name_initialsi))) {
              evaluated_init <- ept(name_initialsi)
            }
          }
        } else if (ept(name_initialsi) == 'random') {
          evaluated_init <- NULL
        }
      } else if (ept(name_initialsi) == 'NULL') {
        evaluated_init <- NULL
      }
      list_collect[[name_initialsi]] <- evaluated_init
    }
    
    if (!is.null(list_collect[[name_initialsi]])) {
      tempv <- list_collect %>% unlist() %>% unname()
      tempv <- as.numeric(tempv)
      if (ept(check_form_0_gr) & !is.null(ept(nparcov_gr))) {
        if (length(tempv) < length(ept(nparcov_gr)) + 1)
          tempv <- rep(tempv, length(ept(nparcov_gr)) + 1)
      }
      for (tempvi in 1:length(tempv)) {
        if (tempv[tempvi] == 0)
          tempv[tempvi] <- 1
      }
      if (dist == 'exponential')
        tempv <- 1 / tempv
      attr(tempv, 'names') <- paste0(parm, addcovname, ii)
      out_list[[name_parm]] <- tempv
    } else {
      out_list[[name_parm]] <- NULL
    }
  }
  
  
  
  
  
  ################################################
  # class cor - cor
  if (class == 'cor' & suffix == 'cor') {
    name_parm <- paste0('L', "_", ii)
    suffix <- 'cor'
    
    allowed_init_options <- NULL
    
    if(!exists('allowed_init_options')) allowed_init_options <- NULL
    
    if (!is.null(c_t_nabcri)) {
      NC_dims <- c_t_nabcri
    } else {
      NC_dims <- nabcrei
    }
    
    NC_cor_elements <- (NC_dims * (NC_dims - 1)) / 2
    
    lowerbound <- lowerbound
    upperbound <- upperbound
    
    out_list <- list_collect <- list()
    start_cnt <- 0
    for (name_initialsi in init_argument) {
      if (grepl("_cov", name_initialsi))
        eit_cov <- TRUE
      else
        eit_cov <- FALSE
      if (eit_cov)
        addcovname <- 'cov'
      else
        addcovname <- NULL
      start_cnt <- start_cnt + 1
      if (ept(name_initialsi) != 'NULL') {
        if (ept(name_initialsi) != 'random') {
          if (ept(name_initialsi) == '0') {
            L_elements     <- rep(0, NC_cor_elements)
            evaluated_init <- create_cor_mat(NC_dims, L_elements)
          } else if (ept(name_initialsi) == 'prior') {
            stanvname_ <- unique(unlist(lapply(stanvars_datazz, names)))
            stanvname_cnt <- 0
            for (stanvname_i in stanvname_) {
              stanvname_cnt <- stanvname_cnt + 1
              stanvname_cnt_name <-
                stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$name
              assign(stanvname_cnt_name,
                     stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$sdata)
              length_args <-
                length(stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$sdata)
            }
            evaluated_init <-
              eval_prior_based_init(
                dist = dist,
                class = class,
                lowerbound = lowerbound,
                upperbound = upperbound,
                length_args = NC_dims
              )
          } else {
            check_evalation_of_numeric_init_obj(
              ept(name_initialsi),
              check = 'args',
              x = name_initialsi,
              pname_ = 'xxx',
              dist = 'dis',
              nlpar = parm,
              class = class,
              allowed_init_options = allowed_init_options,
              splitmvar_w2 = splitmvar_w2
            )
            name_initialsi <- ept(name_initialsi)
            if (is.numeric(ept(name_initialsi)) |
                !is.null(ept(name_initialsi))) {
              if (length(ept(name_initialsi)) == 1) {
                L_elements <- rep(ept(name_initialsi), NC_cor_elements)
              } else if (length(ept(name_initialsi)) == NC_cor_elements) {
                L_elements <- ept(name_initialsi)
              } else {
                stop(
                  "length of correlation vector must be ",
                  NC_cor_elements,
                  ", but found ",
                  length(ept(name_initialsi)),
                  "\n ",
                  " Please check the following init arg :",
                  'gr_init_cor'
                )
              }
              evaluated_init <- create_cor_mat(NC_dims, L_elements)
            }
          }
        } else if (ept(name_initialsi) == 'random') {
          evaluated_init <- NULL
        }
      } else if (ept(name_initialsi) == 'NULL') {
        evaluated_init <- NULL
      }
      list_collect[[name_initialsi]] <- evaluated_init
    }
    
    if (!is.null(list_collect[[name_initialsi]])) {
      tempv <- list_collect %>% unname()
      tempv <- tempv[[1]]
      colnames(tempv) <-
        rownames(tempv) <- paste0(abcrandomelements, addcovname, ii)
      out_list[[name_parm]] <- tempv
    } else {
      out_list[[name_parm]] <- NULL
    }
    
    ####### add "r_init_z"
    set_r_init_z <- TRUE
    
    if (!is.null(c_t_nabcri)) {
      nabcrei_z <- c_t_nabcri
    } else {
      nabcrei_z <- nabcrei
    }
    
    if (set_r_init_z) {
      name_parm <- paste0('z', "_", addcovname, ii)
      name_initialsi <- init_argument_z
      if (ept(name_initialsi) != 'NULL') {
        if (ept(name_initialsi) != 'random') {
          if (ept(name_initialsi) == '0') {
            evaluated_init <- matrix(0, nabcrei_z, N_J_all)
          } else if (ept(name_initialsi) == 'prior') {
            evaluated_init <-
              matrix(rnorm(nabcrei_z * N_J_all, 0, 1), nabcrei_z, N_J_all)
          } else {
            check_evalation_of_numeric_init_obj(
              ept(name_initialsi),
              check = 'args',
              x = name_initialsi,
              pname_ = 'xxx',
              dist = 'dis',
              nlpar = parm,
              class = class,
              allowed_init_options = allowed_init_options,
              splitmvar_w2 = splitmvar_w2
            )
            name_initialsi <- ept(name_initialsi)
            if (is.numeric(ept(name_initialsi)) |
                is.matrix(ept(name_initialsi)) |
                !is.null(ept(name_initialsi))) {
              if (is.numeric(ept(name_initialsi)) &
                  length(ept(name_initialsi)) == 1) {
                z_std <- ept(name_initialsi) %>% as.numeric()
                if (z_std > 1 |
                    z_std < 0)
                  stop("sd for standardized matrix must be between 0 and 1")
                evaluated_init <-
                  matrix(rnorm(nabcrei_z * N_J_all, 0, z_std),
                         nabcrei_z,
                         N_J_all)
              } else if (is.matrix(ept(name_initialsi))) {
                evaluated_init <- ept(name_initialsi) %>% as.numeric()
                if (nrow(evaluated_init) != nabcrei_z &
                    nrow(evaluated_init) != N_J_all) {
                  stop(
                    "standardized matrix must have ",
                    nabcrei_z,
                    " rows and ",
                    N_J_all,
                    " columns"
                  )
                }
              } else {
                stop(
                  "initails for standardized matrix must be a single",
                  "\n",
                  " value of sd between 0 and 1",
                  "\n ",
                  " standardized matrix must have ",
                  nabcrei_z,
                  " rows and ",
                  N_J_all,
                  "columns",
                  "\n ",
                  " Please check the following init arg :",
                  'r_init_z'
                )
              }
              evaluated_init <- evaluated_init
            }
          }
        } else if (ept(name_initialsi) == 'random') {
          evaluated_init <- NULL
        } else if (ept(name_initialsi) == 'NULL') {
          evaluated_init <- NULL
        }
      }
      if (!is.null(evaluated_init)) {
        out_list[[name_parm]] <- evaluated_init
      }
    }
  }
  
  
  
  
  ################################################
  # class Lrescor - mvr rescor
  if (class == 'rescor' & suffix == 'rescor') {
    name_parm <- paste0('Lrescor', "_", ii)
    suffix <- 'rescor'
    
    allowed_init_options <- NULL
    
    if(!exists('allowed_init_options')) allowed_init_options <- NULL
    
    NC_dims         <- ept(nys) %>% as.numeric()
    NC_cor_elements <- (NC_dims * (NC_dims - 1)) / 2
    
    lowerbound <- lowerbound
    upperbound <- upperbound
    
    out_list <- list_collect <- list()
    start_cnt <- 0
    for (name_initialsi in init_argument) {
      if (grepl("_cov", name_initialsi))
        eit_cov <- TRUE
      else
        eit_cov <- FALSE
      start_cnt <- start_cnt + 1
      if (ept(name_initialsi) != 'NULL') {
        if (ept(name_initialsi) != 'random') {
          if (ept(name_initialsi) == '0') {
            L_elements     <- rep(0, NC_cor_elements)
            evaluated_init <- create_cor_mat(NC_dims, L_elements)
          } else if (ept(name_initialsi) == 'prior') {
            stanvname_ <- unique(unlist(lapply(stanvars_datazz, names)))
            stanvname_cnt <- 0
            for (stanvname_i in stanvname_) {
              stanvname_cnt <- stanvname_cnt + 1
              stanvname_cnt_name <-
                stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$name
              assign(stanvname_cnt_name,
                     stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$sdata)
              length_args <-
                length(stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$sdata)
            }
            evaluated_init <-
              eval_prior_based_init(
                dist = dist,
                class = class,
                lowerbound = lowerbound,
                upperbound = upperbound,
                length_args = NC_dims
              )
          } else {
            check_evalation_of_numeric_init_obj(
              ept(name_initialsi),
              check = 'args',
              x = name_initialsi,
              pname_ = 'xxx',
              dist = 'dis',
              nlpar = parm,
              class = class,
              allowed_init_options = allowed_init_options,
              splitmvar_w2 = splitmvar_w2
            )
            name_initialsi <- ept(name_initialsi)
            if (is.numeric(ept(name_initialsi)) |
                !is.null(ept(name_initialsi))) {
              if (length(ept(name_initialsi)) == 1) {
                L_elements <-
                  rep(ept(name_initialsi), NC_cor_elements) %>% as.numeric()
              } else if (length(ept(name_initialsi)) == NC_cor_elements) {
                L_elements <- ept(name_initialsi) %>% as.numeric()
              } else {
                stop(
                  "length of correlation vector must be ",
                  NC_cor_elements,
                  ", but found ",
                  length(ept(name_initialsi)),
                  "\n ",
                  " Please check the following init arg :",
                  'gr_init_cor'
                )
              }
              evaluated_init <- create_cor_mat(NC_dims, L_elements)
            }
          }
        } else if (ept(name_initialsi) == 'random') {
          evaluated_init <- NULL
        }
      } else if (ept(name_initialsi) == 'NULL') {
        evaluated_init <- NULL
      }
      list_collect[[name_initialsi]] <- evaluated_init
    }
    
    if (!is.null(list_collect[[name_initialsi]])) {
      tempv <- list_collect %>% unname()
      tempv <- tempv[[1]]
      out_list[[name_parm]] <- tempv
    } else {
      out_list[[name_parm]] <- NULL
    }
  }
  
  ################################################
  # class sigma
  if (class == 'sigma' & suffix == 'sigma') {
    name_parm <- paste0('sigma', resp_)
    suffix <- 'sigma'
    
    if (dist == 'normal' |
        dist == 'cauchy' |
        dist == 'student_t' | dist == 'student_nu') {
      allowed_init_options <- allowed_init_options_sd
    } else if (dist == 'exponential') {
      allowed_init_options <- allowed_init_options_rate
    } else if (dist == 'gamma') {
      allowed_init_options <- allowed_init_options_shape
    } else {
      allowed_init_options <- NULL
    }
    
    if(!exists('allowed_init_options')) allowed_init_options <- NULL
    
    lowerbound <- 0
    upperbound <- upperbound
    
    out_list <- list_collect <- list()
    start_cnt <- 0
    for (name_initialsi in init_argument) {
      if (grepl("_cov", name_initialsi))
        eit_cov <- TRUE
      else
        eit_cov <- FALSE
      start_cnt <- start_cnt + 1
      if (ept(name_initialsi) != 'NULL') {
        if (ept(name_initialsi) != 'random') {
          if (ept(name_initialsi) == '0') {
            evaluated_init <- ept(name_initialsi) %>% as.numeric()
          } else if (ept(name_initialsi) == 'ysd') {
            eit <- gsub("ysd", paste0("ysd", resp_), ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'ymad') {
            eit <- gsub("ymad",
                        paste0("ymad", resp_),
                        ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'lme_rsd') {
            eit <-
              gsub("lme_rsd",
                   paste0("lme_rsd", resp_),
                   ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'lm_rsd') {
            eit <- gsub("lm_rsd",
                        paste0("lm_rsd", resp_),
                        ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'ysdxmin') {
            eit <-
              gsub("ysdxmin",
                   paste0("ysdxmin", resp_),
                   ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'ysdxmax') {
            eit <-
              gsub("ysdxmax",
                   paste0("ysdxmax", resp_),
                   ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'ysdxmid') {
            eit <-
              gsub("ysdxmid",
                   paste0("ysdxmid", resp_),
                   ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'ysdxmidxmaxdiff') {
            eit <-
              gsub("ysdxmidxmaxdiff",
                   paste0("ysdxmidxmaxdiff", resp_),
                   ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'prior') {
            stanvname_ <- unique(unlist(lapply(stanvars_datazz, names)))
            stanvname_cnt <- 0
            for (stanvname_i in stanvname_) {
              stanvname_cnt <- stanvname_cnt + 1
              stanvname_cnt_name <-
                stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$name
              assign(stanvname_cnt_name,
                     stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$sdata)
              length_args <-
                length(stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$sdata)
            }
            evaluated_init <-
              eval_prior_based_init(
                dist = dist,
                class = class,
                lowerbound = lowerbound,
                upperbound = upperbound,
                length_args = length_args
              )
          } else {
            check_evalation_of_numeric_init_obj(
              ept(name_initialsi),
              check = 'args',
              x = name_initialsi,
              pname_ = 'xxx',
              dist = 'dis',
              nlpar = parm,
              class = class,
              allowed_init_options = allowed_init_options,
              splitmvar_w2 = splitmvar_w2
            )
            name_initialsi <- ept(name_initialsi)
            if (is.numeric(ept(name_initialsi)) |
                !is.null(ept(name_initialsi))) {
              evaluated_init <- ept(name_initialsi) %>% as.numeric()
            }
          }
        } else if (ept(name_initialsi) == 'random') {
          evaluated_init <- NULL
        }
      } else if (ept(name_initialsi) == 'NULL') {
        evaluated_init <- NULL
      }
      list_collect[[name_initialsi]] <- evaluated_init
    }
    
    if (!is.null(list_collect[[name_initialsi]])) {
      tempv <- list_collect %>% unlist() %>% unname()
      tempv <- as.numeric(tempv)
      for (tempvi in 1:length(tempv)) {
        if (tempv[tempvi] == 0)
          tempv[tempvi] <- 1
      }
      if (dist == 'exponential')
        tempv <- 1 / tempv
      out_list[[name_parm]] <- tempv
    } else {
      out_list[[name_parm]] <- NULL
    }
    
  }
  
  
  # sigma modeling via dpar arguments that allows for inclusion of covariates
  
  if (class == '' &
      dpar != "" & suffix == 'sigma' & !ept(check_sigma_form_0)) {
    if (grepl("dpar_init_sigma", init_argument)) {
      name_parm <- paste0('Intercept_sigma', resp_)
      lowerbound <- 0
      upperbound <- upperbound
    }
    
    if (grepl("dpar_cov_init_sigma", init_argument)) {
      name_parm <- paste0('b_sigma', resp_)
      lowerbound <- lowerbound
      upperbound <- upperbound
    }
    
    name_parm <- paste0(name_parm, resp_)
    
    suffix <- 'sigma'
    
    
    if (grepl('Intercept_sigma', name_parm)) {
      if (dist == 'normal' |
          dist == 'cauchy' |
          dist == 'student_t' | dist == 'student_nu') {
        allowed_init_options <- allowed_init_options_sd
      } else if (dist == 'exponential') {
        allowed_init_options <- allowed_init_options_rate
      } else if (dist == 'gamma') {
        allowed_init_options <- allowed_init_options_shape
      } else {
        allowed_init_options <- NULL
      }
    } else if (!grepl('Intercept_sigma', name_parm)) {
      allowed_init_options <- NULL
    }
    
    if(!exists('allowed_init_options')) allowed_init_options <- NULL
    
    
    out_list <- list_collect <- list()
    start_cnt <- 0
    for (name_initialsi in init_argument) {
      if (grepl("_cov", name_initialsi))
        eit_cov <- TRUE
      else
        eit_cov <- FALSE
      start_cnt <- start_cnt + 1
      if (ept(name_initialsi) != 'NULL') {
        if (ept(name_initialsi) != 'random') {
          if (ept(name_initialsi) == '0') {
            evaluated_init <- ept(name_initialsi) %>% as.numeric()
          } else if (ept(name_initialsi) == 'ysd') {
            eit <- gsub("ysd", paste0("ysd", resp_), ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'ymad') {
            eit <- gsub("ymad",
                        paste0("ymad", resp_),
                        ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'lme_rsd') {
            eit <-
              gsub("lme_rsd",
                   paste0("lme_rsd", resp_),
                   ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'lm_rsd') {
            eit <- gsub("lm_rsd",
                        paste0("lm_rsd", resp_),
                        ept(name_initialsi))
            evaluated_init <- ept(eit) %>% as.numeric()
          } else if (ept(name_initialsi) == 'prior') {
            stanvname_ <- unique(unlist(lapply(stanvars_datazz, names)))
            stanvname_cnt <- 0
            for (stanvname_i in stanvname_) {
              stanvname_cnt <- stanvname_cnt + 1
              stanvname_cnt_name <-
                stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$name
              assign(stanvname_cnt_name,
                     stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$sdata)
              length_args <-
                length(stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$sdata)
            }
            evaluated_init <-
              eval_prior_based_init(
                dist = dist,
                class = class,
                lowerbound = lowerbound,
                upperbound = upperbound,
                length_args = length_args
              )
          } else {
            check_evalation_of_numeric_init_obj(
              ept(name_initialsi),
              check = 'args',
              x = name_initialsi,
              pname_ = 'xxx',
              dist = 'dis',
              nlpar = parm,
              class = class,
              allowed_init_options = allowed_init_options,
              splitmvar_w2 = splitmvar_w2
            )
            name_initialsi <- ept(name_initialsi)
            if (is.numeric(ept(name_initialsi)) |
                !is.null(ept(name_initialsi))) {
              evaluated_init <- ept(name_initialsi) %>% as.numeric()
            }
          }
        } else if (ept(name_initialsi) == 'random') {
          evaluated_init <- NULL
        }
      } else if (ept(name_initialsi) == 'NULL') {
        evaluated_init <- NULL
      }
      list_collect[[name_initialsi]] <- evaluated_init
    }
    
    if (!is.null(list_collect[[name_initialsi]])) {
      tempv <- list_collect %>% unlist() %>% unname()
      tempv <- as.numeric(tempv)
      if (grepl('Intercept_sigma', name_parm) &
          tempv == 0)
        tempv <- 1
      if (dist == 'exponential')
        tempv <- 1 / tempv
      out_list[[name_parm]] <- tempv
    } else {
      out_list[[name_parm]] <- NULL
    }
    
  }
  
  
  
  if (class == '' &
      dpar != "" & suffix == 'sigma' & ept(check_sigma_form_0)) {
    name_parm <- paste0('b_sigma', resp_)
    
    suffix <- 'sigma'
    
    covplus1 <- ndparcov + 1
    
    allowed_init_options <- NULL
    
    if(!exists('allowed_init_options')) allowed_init_options <- NULL
    
    lowerbound <- lowerbound
    upperbound <- upperbound
    
    out_list <- list_collect <- list()
    start_cnt <- 0
    for (name_initialsi in init_argument) {
      if (grepl("_cov", name_initialsi))
        eit_cov <- TRUE
      else
        eit_cov <- FALSE
      start_cnt <- start_cnt + 1
      if (ept(name_initialsi) != 'NULL') {
        if (ept(name_initialsi) != 'random') {
          if (ept(name_initialsi) == '0') {
            evaluated_init <- rep(0, nrep_of_parms)
          } else if (ept(name_initialsi) == 'lm_bsigma') {
            if (ept(check_form_0)) {
              lm_gsubby <- paste0("lm_bsigma", "_", nlpar, "_", "all", resp_)
            }
            if (!ept(check_form_0)) {
              if (!eit_cov)
                lm_gsubby <-
                  paste0("lm_bsigma", "_", nlpar, "", "", resp_)
              if (eit_cov)
                lm_gsubby <-
                  paste0("lm_bsigma", "_", nlpar, "_", "cov", resp_)
            }
            evaluated_init <- ept(lm_gsubby) %>% unname()
          } else if (ept(name_initialsi) == 'coef_bsigma') {
            evaluated_init <- ept('coef_bsigma')
          } else if (ept(name_initialsi) == 'prior') {
            stanvname_ <- unique(unlist(lapply(stanvars_datazz, names)))
            stanvname_cnt <- 0
            for (stanvname_i in stanvname_) {
              stanvname_cnt <- stanvname_cnt + 1
              stanvname_cnt_name <-
                stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$name
              assign(stanvname_cnt_name,
                     stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$sdata)
              length_args <-
                length(stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$sdata)
            }
            evaluated_init <-
              eval_prior_based_init(
                dist = dist,
                class = class,
                lowerbound = lowerbound,
                upperbound = upperbound,
                length_args = length_args
              )
          } else {
            check_evalation_of_numeric_init_obj(
              ept(name_initialsi),
              check = 'args',
              x = name_initialsi,
              pname_ = 'xxx',
              dist = 'dis',
              nlpar = parm,
              class = class,
              allowed_init_options = allowed_init_options,
              splitmvar_w2 = splitmvar_w2
            )
            name_initialsi <- ept(name_initialsi)
            if (is.numeric(ept(name_initialsi)) |
                !is.null(ept(name_initialsi))) {
              evaluated_init <- ept(name_initialsi) %>% as.numeric()
              if (length(evaluated_init) < covplus1)
                evaluated_init <- rep(evaluated_init, covplus1)
            }
          }
        } else if (ept(name_initialsi) == 'random') {
          evaluated_init <- NULL
        }
      } else if (ept(name_initialsi) == 'NULL') {
        evaluated_init <- NULL
      }
      list_collect[[name_initialsi]] <- evaluated_init
    }
    
    if (!is.null(list_collect[[name_initialsi]])) {
      tempv <- list_collect %>% unlist() %>% unname()
      tempv <- as.numeric(tempv)
      out_list[[name_parm]] <- tempv
    } else {
      out_list[[name_parm]] <- NULL
    }
    
  }
  
  
  # autocorrelation parameters (ar, ma, and arms)
  if (class == 'ar' | class == 'ma' & suffix == 'acor') {
    if (acorclass == 'arma') {
      init_argument <- rep(init_argument, 2)
      name_parm_s <- c("ar", "ma")
    } else {
      name_parm_s <- class
    }
    
    name_parm_s <- paste0(name_parm_s, resp_)
    
    allowed_init_options <- NULL
    
    if(!exists('allowed_init_options')) allowed_init_options <- NULL
    
    lowerbound <- lowerbound
    upperbound <- upperbound
    
    suffix <- 'acor'
    
    out_list <- list()
    for (name_parm in name_parm_s) {
      
      if(grepl("ar", name_parm)) nrep_of_parms <- nrep_of_parms_p
      if(grepl("ma", name_parm)) nrep_of_parms <- nrep_of_parms_q
      
      list_collect <- list()
      start_cnt <- 0
      for (name_initialsi in init_argument) {
        if (grepl("_cov", name_initialsi))
          eit_cov <- TRUE
        else
          eit_cov <- FALSE
        start_cnt <- start_cnt + 1
        if (ept(name_initialsi) != 'NULL') {
          if (ept(name_initialsi) != 'random') {
            if (ept(name_initialsi) == '0') {
              evaluated_init <- rep(0, nrep_of_parms)
            } else if (ept(name_initialsi) == 'prior') {
              stanvname_ <- unique(unlist(lapply(stanvars_datazz, names)))
              stanvname_cnt <- 0
              for (stanvname_i in stanvname_) {
                stanvname_cnt <- stanvname_cnt + 1
                stanvname_cnt_name <-
                  stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$name
                assign(stanvname_cnt_name,
                       stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$sdata)
                length_args <-
                  length(stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$sdata)
              }
              evaluated_init <-
                eval_prior_based_init(
                  dist = dist,
                  class = class,
                  lowerbound = lowerbound,
                  upperbound = upperbound,
                  length_args = length_args
                )
            } else {
              check_evalation_of_numeric_init_obj(
                ept(name_initialsi),
                check = 'args',
                x = name_initialsi,
                pname_ = 'xxx',
                dist = 'dis',
                nlpar = parm,
                class = class,
                allowed_init_options = allowed_init_options,
                splitmvar_w2 = splitmvar_w2
              )
              name_initialsi <- ept(name_initialsi)
              if (is.numeric(ept(name_initialsi)) |
                  !is.null(ept(name_initialsi))) {
                evaluated_init <- ept(name_initialsi) %>% as.numeric()
                if (evaluated_init > 1 |
                    evaluated_init < -1)
                  stop("initials for autocorrelation must be between -1 and 1")
              }
            }
          } else if (ept(name_initialsi) == 'random') {
            evaluated_init <- NULL
          }
        } else if (ept(name_initialsi) == 'NULL') {
          evaluated_init <- NULL
        }
        list_collect[[name_initialsi]] <- evaluated_init
      }
      if (!is.null(list_collect[[name_initialsi]])) {
        tempv <- list_collect %>% unlist() %>% unname()
        tempv <- as.numeric(tempv)
        if(length(tempv) == 1 & nrep_of_parms == 1) {
          tempv <- tempv
        } else if(length(tempv) == 1 & nrep_of_parms > 1) {
          tempv <- rep(tempv, nrep_of_parms)
        } else if(length(tempv) != nrep_of_parms) {
          stop("Length of initials should be either 1 of equal to dims")
        }
        out_list[[name_parm]] <- tempv
      } else {
        out_list[[name_parm]] <- NULL
      }
    }
  }
  
  
  # autocorrelation parameters (unst) -> similar to cor / lrescor
  
  if (class == 'Lcortime' & suffix == 'acor') {
    
    # name_parm <- paste0('Cortime', "_", ii)
    
    name_parm_s <- class
    name_parm_s <- paste0(name_parm_s, resp_)
    
    name_parm <- name_parm_s # paste0('Lcortime', "_", ii)
    
    suffix <- 'acor'
    
    allowed_init_options <- NULL
    
    if(!exists('allowed_init_options')) allowed_init_options <- NULL
    
    NC_dims         <- ept(cortimeNlags) %>% as.numeric()
    NC_cor_elements <- (NC_dims * (NC_dims - 1)) / 2
    
    lowerbound <- lowerbound
    upperbound <- upperbound
    
    out_list <- list_collect <- list()
    start_cnt <- 0
    for (name_initialsi in init_argument) {
      if (grepl("_cov", name_initialsi))
        eit_cov <- TRUE
      else
        eit_cov <- FALSE
      start_cnt <- start_cnt + 1
      if (ept(name_initialsi) != 'NULL') {
        if (ept(name_initialsi) != 'random') {
          if (ept(name_initialsi) == '0') {
            L_elements     <- rep(0, NC_cor_elements)
            evaluated_init <- create_cor_mat(NC_dims, L_elements)
          } else if (ept(name_initialsi) == 'prior') {
            stanvname_ <- unique(unlist(lapply(stanvars_datazz, names)))
            stanvname_cnt <- 0
            for (stanvname_i in stanvname_) {
              stanvname_cnt <- stanvname_cnt + 1
              stanvname_cnt_name <-
                stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$name
              assign(stanvname_cnt_name,
                     stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$sdata)
              length_args <-
                length(stanvars_datazz[[stanvname_cnt]][[stanvname_i]]$sdata)
            }
            evaluated_init <-
              eval_prior_based_init(
                dist = dist,
                class = class,
                lowerbound = lowerbound,
                upperbound = upperbound,
                length_args = NC_dims
              )
          } else {
            check_evalation_of_numeric_init_obj(
              ept(name_initialsi),
              check = 'args',
              x = name_initialsi,
              pname_ = 'xxx',
              dist = 'dis',
              nlpar = parm,
              class = class,
              allowed_init_options = allowed_init_options,
              splitmvar_w2 = splitmvar_w2
            )
            name_initialsi <- ept(name_initialsi)
            if (is.numeric(ept(name_initialsi)) |
                !is.null(ept(name_initialsi))) {
              if (length(ept(name_initialsi)) == 1) {
                L_elements <-
                  rep(ept(name_initialsi), NC_cor_elements) %>% as.numeric()
              } else if (length(ept(name_initialsi)) == NC_cor_elements) {
                L_elements <- ept(name_initialsi) %>% as.numeric()
              } else {
                stop(
                  "length of correlation vector must be ",
                  NC_cor_elements,
                  ", but found ",
                  length(ept(name_initialsi)),
                  "\n ",
                  " Please check the following init arg :",
                  'gr_init_cor'
                )
              }
              evaluated_init <- create_cor_mat(NC_dims, L_elements)
            }
          }
        } else if (ept(name_initialsi) == 'random') {
          evaluated_init <- NULL
        }
      } else if (ept(name_initialsi) == 'NULL') {
        evaluated_init <- NULL
      }
      list_collect[[name_initialsi]] <- evaluated_init
    }
    
    if (!is.null(list_collect[[name_initialsi]])) {
      tempv <- list_collect %>% unname()
      tempv <- tempv[[1]]
      out_list[[name_parm]] <- tempv
    } else {
      out_list[[name_parm]] <- NULL
    }
  }
  
  
  
  # standardized group level effects (part of centred parametrisation)
  
  if (any(grepl("_init_sd", init_argument)) &
      gr_init_cor == "NULL") {
    nabcrei_z <- 1
    ####### add "r_init_z"
    set_r_init_z <- TRUE
    if (set_r_init_z) {
      if (!is.null(ept(nparcov_gr))) {
        # ept(check_form_0_gr) &
        nabcrei_z <- nabcrei_z * ept(nparcov_gr)
      }
      for (name_initialsi in init_argument_z) {
        name_parm <-
          paste0('z',
                 "_",
                 strsplit(init_argument, "_")[[1]][1],
                 "",
                 addcovname,
                 ii)
        if (ept(name_initialsi) != 'NULL') {
          if (ept(name_initialsi) != 'random') {
            if (ept(name_initialsi) == '0') {
              evaluated_init <- matrix(0, nabcrei_z, N_J_all)
            } else if (ept(name_initialsi) == 'prior') {
              evaluated_init <-
                matrix(rnorm(nabcrei_z * N_J_all, 0, 1),
                       nabcrei_z,
                       N_J_all)
            } else {
              check_evalation_of_numeric_init_obj(
                ept(name_initialsi),
                check = 'args',
                x = name_initialsi,
                pname_ = 'xxx',
                dist = 'dis',
                nlpar = parm,
                class = class,
                allowed_init_options = allowed_init_options,
                splitmvar_w2 = splitmvar_w2
              )
              name_initialsi <- ept(name_initialsi)
              if (is.numeric(ept(name_initialsi)) |
                  is.matrix(ept(name_initialsi)) |
                  !is.null(ept(name_initialsi))) {
                if (is.numeric(ept(name_initialsi)) &
                    length(ept(name_initialsi)) == 1) {
                  z_std <- ept(name_initialsi) %>% as.numeric()
                  if (z_std > 1 |
                      z_std < 0)
                    stop("sd for standardized matrix must be between 0 and 1")
                  evaluated_init <-
                    matrix(rnorm(nabcrei_z * N_J_all, 0, z_std),
                           nabcrei_z,
                           N_J_all)
                } else if (is.matrix(ept(name_initialsi))) {
                  evaluated_init <- ept(name_initialsi) %>% as.numeric()
                  if (nrow(evaluated_init) != nabcrei_z &
                      nrow(evaluated_init) != N_J_all) {
                    stop(
                      "standardized matrix must have ",
                      nabcrei_z,
                      " rows and ",
                      N_J_all,
                      " columns"
                    )
                  }
                } else {
                  stop(
                    "initails for standardized matrix must be a",
                    "\n",
                    " single value of sd between 0 and 1",
                    "\n ",
                    " standardized matrix must have ",
                    nabcrei_z,
                    " rows and ",
                    N_J_all,
                    "columns",
                    "\n ",
                    " Please check the following init arg :",
                    'r_init_z'
                  )
                }
                evaluated_init <- evaluated_init
              }
            }
          } else if (ept(name_initialsi) == 'random') {
            evaluated_init <- NULL
          } else if (ept(name_initialsi) == 'NULL') {
            evaluated_init <- NULL
          }
        }
        if (!is.null(evaluated_init)) {
          out_list[[name_parm]] <- evaluated_init
        }
      }
    }
  }
  
  if (!exists('out_list'))
    out_list <- NULL
  
  out_list
}


