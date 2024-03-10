



#' An internal function to prepare Stan function
#'
#' The \code{prepare_function}) constructs custom Stan function  which is passed
#' on to the [bsitar::bsitar()] function. For univariate-by- subgroup model
#' (\code{univariate_by}) and multivariate (\code{multivariate}) models (see
#' [bsitar::bsitar()]), the \code{x}, \code{y}, \code{id}, \code{knots},
#' \code{nknots}, are automatically matched with the sub-models.
#'
#' @param x Predictor variable in the data. See [bsitar::bsitar()] for details.
#'
#' @param y Response variable in the data. See [bsitar::bsitar()] for details.
#'
#' @param id A vector specifying a unique group identifier for each individual.
#' See [bsitar::bsitar()] for details.
#'
#' @param knots A vector of knots used for constructing the spline design
#' matrix. See [bsitar::bsitar()] for details.
#'
#' @param nknots An integer specifying the number of knots.
#'
#' @param data Data frame containing variables \code{x}, \code{y} and \code{id}.
#'
#' @param internal_function_args Internal arguments passed from the
#'   [bsitar::bsitar()] to the \code{prepare_formula}).
#'
#' @return An character string which later evaluated to a custom function
#'   and inserted into the Stan's functions block.
#'   
#' @author Satpal Sandhu  \email{satpal.sandhu@bristol.ac.uk}
#'
#' @keywords internal
#' @noRd
#'
prepare_function <- function(x,
                             y,
                             id,
                             knots,
                             nknots,
                             data,
                             internal_function_args) {

  # Initiate non formalArgs()
  brms_arguments <- NULL;
  xfunsi <- NULL;
  Var1 <- NULL;
  Var2 <- NULL;
  select_model <- NULL;
  fixedsi <- NULL;
  match_sitar_d_form <- NULL;
  d_adjustedsi <- NULL;
  randomsi <- NULL;
  getxname <- NULL;
  getknotsname <- NULL;
  spfncname <- NULL;
  xoffset <- NULL;
  yfunsi <- NULL;
  all_raw_str <- NULL;
  all_raw_str <- NULL;
  decomp <- NULL;
  nys <- NULL;
  gsub_out_unscaled <- NULL;
  checkscovsi <- NULL;
  add_rcsfunmatqrinv_genquant <- NULL;
  add_b_Qr_genquan_s_coef <- NULL;
  
  
  
  if (!is.null(internal_function_args)) {
    eout <- list2env(internal_function_args)
    for (eoutii in names(eout)) {
      assign(eoutii, eout[[eoutii]])
    }
  }
  
  
  
  
  backend <- eval(brms_arguments$backend)

  vector_X_name <- "Xp"
  
  if (nys == 1)
    resp_ <- ""
  if (nys > 1)
    resp_ <- paste0("_", y)
  
  XR_inv_name <- 'XR_inv'
  XR_inv_name_resp <- paste0(XR_inv_name, resp_)
  
  b_sx_name <- 'b_sx'
  b_sx_name_resp <- paste0('b', resp_, "_", 'sx')
  
  
  iysxi <- 's'
  setp <- paste0(iysxi, '')
  setnlp <- paste0('nlp', resp_, '_', iysxi)
  setnlp_vector <- paste0('b', resp_)
  
  
  if (resp_ == "") {
    szx_name <- paste0('s', 1:(nknots - 1))
    szx_name_resp <- paste0('b', "_", 's', 1:(nknots - 1))
  } else {
    szx_name <- paste0('s', 1:(nknots - 1))
    szx_name_resp <- paste0('b', resp_, "_", szx_name)
  }
  
  
  b_s_name <- szx_name_resp
  
  szxbq_vector <- paste0('v', resp_, "_", 's', 'x')
  
  #########
  
  if (!is.null(xfunsi[[1]][1]) & xfunsi != "NULL") {
    if (xfunsi == "log") {
      tranform_x_int <- 1
    } else if (xfunsi == "sqrt") {
      tranform_x_int <- 2
    } else if (xfunsi != "log" | xfunsi == "sqrt") {
      tranform_x_int <- 0
    }
  } else if (is.null(xfunsi[[1]][1]) | xfunsi == "NULL") {
    tranform_x_int <- 0
  }
  
  
  
  
  set_x_y_scale_factror <- function(xfunsi = NULL,
                                    yfunsi = NULL,
                                    tranformations = "identity") {
    scale_set_comb <- tranformations
    scale_set_comb1 <-
      paste(scale_set_comb, scale_set_comb, sep = "_")
    scale_set_comb2 <-
      with(subset(expand.grid(scale_set_comb, scale_set_comb),
                  Var1 != Var2), paste0(Var1, '_', Var2))
    scale_set_comb <- c(scale_set_comb1, scale_set_comb2)
    
    if (!is.null(xfunsi[[1]][1]) & xfunsi != "NULL") {
      if (xfunsi == "log") {
        xscale_set <- "log"
      } else if (xfunsi == "sqrt") {
        xscale_set <- "sqrt"
      } else if (xfunsi != "log" | xfunsi == "sqrt") {
        xscale_set <- "identity"
      }
    } else if (is.null(xfunsi[[1]][1]) | xfunsi == "NULL") {
      xscale_set <- "identity"
    }
    
    if (!is.null(yfunsi[[1]][1]) & yfunsi != "NULL") {
      if (yfunsi == "log") {
        yscale_set <- "log"
      } else if (yfunsi == "sqrt") {
        yscale_set <- "sqrt"
      } else if (yfunsi != "log" | yfunsi == "sqrt") {
        yscale_set <- "identity"
      }
    } else if (is.null(yfunsi[[1]][1]) | yfunsi == "NULL") {
      yscale_set <- "identity"
    }
    
    
    
    
    if (xscale_set == "identity" & yscale_set == "identity") {
      xscale_factor_str_d1 <- "rep_vector(1, N);"
      xscale_factor_str_d2 <- "rep_vector(1, N);"
      yscale_factor_str_d1 <- "rep_vector(1, N);"
      yscale_factor_str_d2 <- "rep_vector(1, N);"
    } else if (xscale_set == "log" & yscale_set == "log") {
      xscale_factor_str_d1 <- "exp(Xm + xoffset);"
      xscale_factor_str_d2 <- "exp(Xm + xoffset);"
      yscale_factor_str_d1 <- "(pred_d0);"
      yscale_factor_str_d2 <- "(pred_d0);"
    } else if (xscale_set == "sqrt" & yscale_set == "sqrt") {
      xscale_factor_str_d1 <- "(Xm + xoffset);"
      xscale_factor_str_d2 <- "(Xm + xoffset);"
      yscale_factor_str_d1 <- "(sqrt(pred_d0));"
      yscale_factor_str_d2 <- "(sqrt(pred_d0));"
    } else if (xscale_set == "log" & yscale_set == "identity") {
      xscale_factor_str_d1 <- "exp(Xm + xoffset);"
      xscale_factor_str_d2 <- "exp(Xm + xoffset);"
      yscale_factor_str_d1 <- "rep_vector(1, N);"
      yscale_factor_str_d2 <- "rep_vector(1, N);"
    } else if (xscale_set == "sqrt" & yscale_set == "identity") {
      xscale_factor_str_d1 <- "(Xm + xoffset);"
      xscale_factor_str_d2 <- "(Xm + xoffset);"
      yscale_factor_str_d1 <- "rep_vector(0.5, N);"
      yscale_factor_str_d2 <- "rep_vector(0.5, N);"
    } else if (xscale_set == "identity" & yscale_set == "log") {
      xscale_factor_str_d1 <- "rep_vector(1, N);"
      xscale_factor_str_d2 <- "rep_vector(1, N);"
      yscale_factor_str_d1 <- "(pred_d0);"
      yscale_factor_str_d2 <- "(pred_d0);"
    } else if (xscale_set == "sqrt" & yscale_set == "log") {
      xscale_factor_str_d1 <- "(Xm + xoffset);"
      xscale_factor_str_d2 <- "(Xm + xoffset);"
      yscale_factor_str_d1 <- "(rep_vector(0.5, N) .* (pred_d0));"
      yscale_factor_str_d2 <- "(rep_vector(0.5, N) .* (pred_d0));"
    } else if (xscale_set == "identity" & yscale_set == "sqrt") {
      xscale_factor_str_d1 <- "rep_vector(1, N);"
      xscale_factor_str_d2 <- "rep_vector(1, N);"
      yscale_factor_str_d1 <-
        "(rep_vector(2.0, N) .* sqrt(pred_d0));"
      yscale_factor_str_d2 <-
        "(rep_vector(2.0, N) .* sqrt(pred_d0));"
    } else if (xscale_set == "log" & yscale_set == "sqrt") {
      xscale_factor_str_d1 <- "exp(Xm + xoffset);"
      xscale_factor_str_d2 <- "exp(Xm + xoffset);"
      yscale_factor_str_d1 <-
        "(rep_vector(2.0, N) .* sqrt(pred_d0));"
      yscale_factor_str_d2 <-
        "(rep_vector(2.0, N) .* sqrt(pred_d0));"
    }
    
    list(
      xscale_factor_str_d1 = xscale_factor_str_d1,
      xscale_factor_str_d2 = xscale_factor_str_d2,
      yscale_factor_str_d1 = yscale_factor_str_d1,
      yscale_factor_str_d2 = yscale_factor_str_d2
    )
    
  } # end set_x_y_scale_factror
  
  
  setxoffset <- paste0("real xoffset = ", xoffset, ";")
  setxoffset_plane <- paste0("real xoffset = ", xoffset, ";")
  
  # https://mc-stan.org/users/documentation/case-studies/qr_regression.html
  decomp_code_qr <-
    "
      int QK = nknots - 1;
      matrix[N, QK] Qc = Spl;
      matrix[N, QK] XQ;
      matrix[QK, QK] XR;
      matrix[QK, QK] XR_inv;
      XQ = qr_thin_Q(Qc) * sqrt(N - 1);
      XR = qr_thin_R(Qc) / sqrt(N - 1);
      XR_inv = inverse(XR);
      "
  
  decomp_code_qr <-
    gsub("XR_inv", XR_inv_name, decomp_code_qr, fixed = T)
  
  
  add_context_getx_fun <-
    "/* Transform x variable
 * Args:
 * Xp: x variable
 * Transformation code (tranform_x, 0 to 2)
 * 0, no transformation, 1 log, 2 square rooot
 * Note that the xoffset  is already transformed
 * Returns:
 * x variable with log/sqrt transformation
 */"
  
  add_context_getknots_fun <-
    "/* Knots
 * xoffset and Knots already transformed:
 * Returns:
 * Knots
 */"
  
  ##########
  
  create_internal_function <-
    function(y,
             function_str,
             fname,
             fnameout,
             spl,
             splout,
             xfunsi,
             yfunsi,
             setxoffset,
             gsub_out_unscaled,
             body,
             vectorA,
             decomp,
             fixedsi) {
      split1 <- strsplit(function_str, gsub("\\[", "\\\\[", spl))[[1]][-1]
      split2 <- strsplit(split1, "return")[[1]][-2]
      out <- gsub(split2, body, function_str, fixed = T)
      out <- gsub(spl, splout, out, fixed = T)
      out <- gsub(fname, fnameout, out, fixed = T)
      
      if (grepl("d0", fnameout)) {
        out <- out
      } else if (grepl("d1", fnameout) | grepl("d2", fnameout)) {
        out <- gsub("return(A+", "return(0+", out, fixed = T)
      }
      
      if (grepl("c", fixedsi, fixed = T)) {
        if (grepl("d0", fnameout)) {
          out <- gsub("]));", "]));", out, fixed = T)
          out <-
            gsub(
              "end of spline function",
              paste0("end of spline function", "_", y, "d", 0),
              out,
              fixed = T
            )
        } else if (grepl("d1", fnameout)) {
          out <- gsub("]));", "]).*exp(c));", out, fixed = T)
          out <-
            gsub(
              "end of spline function",
              paste0("end of spline function", "_", y, "d", 1),
              out,
              fixed = T
            )
        } else if (grepl("d2", fnameout)) {
          out <- gsub("]));", "]).*exp(c)^2);", out, fixed = T)
          out <-
            gsub(
              "end of spline function",
              paste0("end of spline function", "_", y, "d", 2),
              out,
              fixed = T
            )
        } else {
          out <- out
        }
      }
      
      ####
      if (grepl("d0", fnameout)) {
        pattern <- "return\\(\\s*(.*?)\\s*\\);"
        result <- regmatches(out, regexec(pattern, out))
        out_unscaled <-
          paste0("vector[N] out_unscaled=", result[[1]][2], ";")
        
        
        if (!is.null(gsub_out_unscaled)) {
          if (length(gsub_out_unscaled) != 2)
            stop('Length of gsub_out_unscaled should be 2')
          out_unscaled <-
            gsub(gsub_out_unscaled[1],
                 gsub_out_unscaled[2],
                 out_unscaled,
                 fixed = T)
        }
        
        
        if (yfunsi == "log") {
          out_scaled <-
            paste0("    vector[N] out_scaled=",
                   "exp",
                   "(",
                   "out_unscaled",
                   ")",
                   ";")
        } else if (yfunsi == "sqrt") {
          if ((backend == "rstan" &
               utils::packageVersion("rstan") >= "2.26.1") |
              backend == "mock" |
              backend == "cmdstanr") {
            out_scaled <-
              paste0("    vector[N] out_scaled=",
                     "",
                     "(",
                     "out_unscaled",
                     ")^2",
                     ";")
          }
          if ((backend == "rstan" &
               utils::packageVersion("rstan") < "2.26.1") | # &
              backend == "mock" &
              backend != "cmdstanr") {
            out_scaled <-
              paste0("    vector[N] out_scaled=",
                     "",
                     "(",
                     "pow(",
                     "out_unscaled",
                     ", 2)" ,
                     ")",
                     ";")
          }
        } else if (yfunsi != "log" & yfunsi != "sqrt") {
          out_scaled <-
            paste0("    vector[N] out_scaled=",
                   "",
                   "(",
                   "out_unscaled",
                   ")",
                   ";")
        }
        
        out <- gsub(result[[1]][2], "out_scaled", out, fixed = T)
        out_return <- paste0(out_unscaled, "\n", out_scaled)
        
        # setxoffset <- paste0("real xoffset = ", xoffset, ";")
        
        if (is.null(decomp))
          setxoffset <- paste0(setxoffset, vectorA)
        
        out_return <- paste0(setxoffset,
                             "\n    ",
                             out_return)
        out_return_p <- paste0(out_return, "\n", "    return")
        out <- gsub("return", out_return_p, out, fixed = T)
        
      } else if (grepl("d1", fnameout) | grepl("d2", fnameout)) {
        pattern <- "return\\(\\s*(.*?)\\s*\\);"
        result <- regmatches(out, regexec(pattern, out))
        
        set_x_y_scale <- set_x_y_scale_factror(
          xfunsi = xfunsi,
          yfunsi = yfunsi,
          tranformations =
            c("identity", "log", "sqrt")
        )
        
        if (grepl("d1", fnameout)) {
          xscale_factor <- set_x_y_scale[['xscale_factor_str_d1']]
          yscale_factor <- set_x_y_scale[['yscale_factor_str_d1']]
        } else if (grepl("d2", fnameout)) {
          xscale_factor <- set_x_y_scale[['xscale_factor_str_d2']]
          yscale_factor <- set_x_y_scale[['yscale_factor_str_d2']]
        }
        
        xscale_factor <- gsub(";", "", xscale_factor)
        yscale_factor <- gsub(";", "", yscale_factor)
        out_unscaled <-
          paste0("vector[N] out_unscaled=", result[[1]][2], ";")
        out_scaled <- paste0(
          "    vector[N] out_scaled=",
          "(",
          "(",
          yscale_factor,
          ")",
          " .* ",
          "(",
          'out_unscaled',
          ")",
          ")",
          " ./ ",
          "(",
          xscale_factor,
          ")",
          ";"
        )
        out <- gsub(result[[1]][2], "out_scaled", out, fixed = T)
        out_return <- paste0(out_unscaled, "\n", out_scaled)
        addpdo <- paste0("vector[N] pred_d0=", spl_fun_ford, ";")
        # setxoffset <- paste0("real xoffset = ", xoffset, ";")
        out_return <- paste0(addpdo,
                             "\n    ",
                             setxoffset,
                             "\n    ",
                             out_return)
        out_return_p <- paste0(out_return, "\n", "    return")
        
        if (!is.null(decomp)) {
          if (decomp == 'QR') {
            if (grepl("d1", fnameout)) {
              out_return_p <- gsub(
                vectorA,
                paste0(vectorA, "\n", "XQ[,1] = rep_vector(1, N);"),
                out_return_p,
                fixed = T
              )
              out_return_p <-
                gsub(vectorA, "", out_return_p, fixed = T)
            }
            if (grepl("d2", fnameout)) {
              out_return_p <- gsub(
                vectorA,
                paste0(vectorA, "\n", "XQ[,1] = rep_vector(0, N);"),
                out_return_p,
                fixed = T
              )
              out_return_p <-
                gsub(vectorA, "", out_return_p, fixed = T)
            }
          }
        }
        out <- gsub("return", out_return_p, out, fixed = T)
      }
      
      return(out)
    }
  
  
  
  
  
  ##########
  
  create_internal_function_nonsitar <-
    function(y,
             function_str,
             fname,
             fnameout,
             returnmu,
             xfunsi,
             yfunsi,
             setxoffset,
             gsub_out_unscaled = NULL,
             spl_fun_ford,
             body,
             decomp,
             fixedsi) {
      out <- function_str
      for_out <- gsub(fname, fnameout, out)
      
      ####
      if (grepl("d0", fnameout)) {
        out_unscaled <- paste0("vector[N] out_unscaled=", body, ";")
        
        if (!is.null(gsub_out_unscaled)) {
          if (length(gsub_out_unscaled) != 2)
            stop('Length of gsub_out_unscaled should be 2')
          out_unscaled <-
            gsub(gsub_out_unscaled[1],
                 gsub_out_unscaled[2],
                 out_unscaled,
                 fixed = T)
        }
        
        
        if (yfunsi == "log") {
          out_scaled <-
            paste0("    vector[N] out_scaled=",
                   "exp",
                   "(",
                   "out_unscaled",
                   ")",
                   ";")
        } else if (yfunsi == "sqrt") {
          if ((backend == "rstan" &
               utils::packageVersion("rstan") >= "2.26.1") |
              backend == "mock" |
              backend == "cmdstanr") {
            out_scaled <-
              paste0("    vector[N] out_scaled=",
                     "",
                     "(",
                     "out_unscaled",
                     ")^2.0",
                     ";")
          }
          if ((backend == "rstan" &
               utils::packageVersion("rstan") < "2.26.1") | # &
              backend == "mock" &
              backend != "cmdstanr") {
            out_scaled <-
              paste0("    vector[N] out_scaled=",
                     "",
                     "(",
                     "pow(",
                     "out_unscaled",
                     ", 2)" ,
                     ")",
                     ";")
          }
        } else if (yfunsi != "log" & yfunsi != "sqrt") {
          out_scaled <-
            paste0("    vector[N] out_scaled=",
                   "",
                   "(",
                   "out_unscaled",
                   ")",
                   ";")
        }
        
        out_return <- paste0(out_unscaled, "\n", out_scaled)
        # setxoffset <- paste0("real xoffset = ", xoffset, ";")
        out_return <- paste0(setxoffset,
                             "\n    ",
                             out_return)
        out_return_p <- paste0(out_return, "\n", "    return")
        out_scaled_with_parentehsis <-
          paste0("(", 'out_scaled', ")")
        out <- paste(out_return_p, out_scaled_with_parentehsis, ";")
        out <- paste0(gsub("return.*", "", for_out),
                      out,
                      "\n}")
        
      } else if (grepl("d1", fnameout) | grepl("d2", fnameout)) {
        set_x_y_scale <- set_x_y_scale_factror(
          xfunsi = xfunsi,
          yfunsi = yfunsi,
          tranformations =
            c("identity", "log", "sqrt")
        )
        
        if (grepl("d1", fnameout)) {
          xscale_factor <- set_x_y_scale[['xscale_factor_str_d1']]
          yscale_factor <- set_x_y_scale[['yscale_factor_str_d1']]
        } else if (grepl("d2", fnameout)) {
          xscale_factor <- set_x_y_scale[['xscale_factor_str_d2']]
          yscale_factor <- set_x_y_scale[['yscale_factor_str_d2']]
        }
        
        xscale_factor <- gsub(";", "", xscale_factor)
        yscale_factor <- gsub(";", "", yscale_factor)
        out_unscaled <- paste0("vector[N] out_unscaled=", body, ";")
        out_scaled <-
          paste0(
            "    vector[N] out_scaled=",
            "(",
            "(",
            yscale_factor,
            ")",
            " .* ",
            "(",
            'out_unscaled',
            ")",
            ")",
            " ./ ",
            "(",
            xscale_factor,
            ")",
            ";"
          )
        addpdo <- paste0("vector[N] pred_d0=", spl_fun_ford, ";")
        out_return <- paste0(out_unscaled, "\n", out_scaled)
        # setxoffset <- paste0("real xoffset = ", xoffset, ";")
        out_return <- paste0(addpdo,
                             "\n    ",
                             setxoffset,
                             "\n    ",
                             out_return)
        out_return_p <- paste0(out_return, "\n", "    return")
        out_scaled_with_parentehsis <-
          paste0("(", 'out_scaled', ")")
        out <- paste(out_return_p, out_scaled_with_parentehsis, ";")
        out <- paste0(gsub("return.*", "", for_out),
                      out,
                      "\n}")
      }
      return(out)
    }
  
  ##########
  
  
  
  if (select_model == 'sitar' | select_model == 'rcs') {
    abcnames <-
      paste0(strsplit(gsub("\\+", " ", fixedsi), " ")[[1]], sep = ",")
    
    snames <- c()
    for (i in 1:(nknots - 1)) {
      if (i < (nknots - 1)) {
        name1 <- paste0("s", i, sep = ",")
      }
      else {
        name1 <- paste0("s", i, sep = "")
      }
      snames[i] <- name1
    }
    
    # For some reasons, 'sitar' (Tim Cole) allows random only 'd' parameter
    # In fact for df > 1, it forces 'd' to be random parameter only
    if (match_sitar_d_form) {
      if (!grepl("d", fixedsi, fixed = T) &
          grepl("d", randomsi, fixed = T)) {
        abcnames <- c(abcnames, "d,")
      }
    }
    
    
    if (select_model == 'sitar' | select_model == 'rcs') {
      if (any(grepl("s", abcnames)))
        abcnames <- abcnames[-length(abcnames)]
      if (match_sitar_d_form)
        abcnames <- gsub('s', 'd', abcnames, fixed = T)
    }
    
    fullabcsnames <- c(abcnames, snames)
    fullabcsnames_v <-
      paste("vector", fullabcsnames, collapse = " ")
    
    if (select_model == 'sitar') {
      fullabcsnames_for_mat <- abcnames
      fullabcsnames_for_mat <-
        gsub('.{1}$', '', fullabcsnames_for_mat)
      fullabcsnames_v_for_mat <-
        paste("vector", fullabcsnames_for_mat, collapse = ", ")
    }
    
    if (select_model == 'rcs') {
      fullabcsnames_for_mat <- 'a'
      fullabcsnames_v_for_mat <-
        paste("vector", fullabcsnames_for_mat, collapse = " ")
    }

    if (grepl("b", fixedsi, fixed = T) &
        grepl("c", fixedsi, fixed = T)) {
      defineEx <- paste0("(Xm-b).*exp(c)")
    }
    if (grepl("b", fixedsi, fixed = T) &
        !grepl("c", fixedsi, fixed = T)) {
      defineEx <- paste0("(Xm-b)")
    }
    if (!grepl("b", fixedsi, fixed = T) &
        grepl("c", fixedsi, fixed = T)) {
      defineEx <- paste0("(Xm).*exp(c)")
    }
    if (!grepl("b", fixedsi, fixed = T) &
        !grepl("c", fixedsi, fixed = T)) {
      defineEx <- paste0("(Xm)")
    }
    
    add_knotinfo <- paste0(
      "\n  int N=num_elements(",
      vector_X_name,
      ");",
      paste0(
        "\n  vector[N] Xm=",
        paste0(getxname,
               "(", vector_X_name, ")"),
        ";"
      ),
      paste0("\n  vector[N] X=", defineEx, ";"),
      paste0("\n  int nknots=", eval(parse(text = nknots)), ";"),
      paste0(
        "\n  vector[nknots] knots=",
        paste0(getknotsname, "(", '', ")"),
        ";"
      )
    )
    
    
    if (select_model == 'sitar') {
      vectorA <- "\n  vector[N] A=a-(s1*min(knots));"
      if (!is.null(decomp)) {
        if (decomp == 'QR') {
          vectorA <- "\n  vector[N] A=a;"
          # vectorA <- "\n  vector[N] A=a-((XQ[,1].*s1)*min(knots));"
        }
      }
    }
    
    
    
    if (select_model == 'rcs') {
      vectorA <- "\n  vector[N] A=a;"
    }
    
    
    
    # add_knotinfo <- paste0(add_knotinfo, vectorA)
    if ((backend == "rstan" &
         utils::packageVersion("rstan") >= "2.26.1") |
        backend == "mock" |
        backend == "cmdstanr") {
      fun_body <- "
    matrix[N, nknots-1] Spl;
    matrix[nknots-1, N] rcs;
    matrix[N, nknots] Xx;
    int km1 = nknots - 1;
    int jp1;
    int j=1;
    for(ia in 1:N) {
     for(ja in 1:nknots) {
      Xx[ia,ja] = (X[ia] - knots[ja] > 0 ? X[ia] - knots[ja] : 0);
        }
    }
     Spl[,1]=X;
     while (j <= nknots - 2) {
      jp1 = j + 1;
       Spl[,jp1] = (Xx[,j]^3-(Xx[,km1]^3)*(knots[nknots]-knots[j])/
       (knots[nknots]-knots[km1]) + (Xx[,nknots]^3)*(knots[km1]-knots[j])/
       (knots[nknots]-knots[km1])) / (knots[nknots]-knots[1])^2;
        j = j + 1;
      }"
    }
    
    if ((backend == "rstan" &
         utils::packageVersion("rstan") < "2.26.1") | # &
        backend == "mock" &
        backend != "cmdstanr") {
      fun_body <- "
    matrix[N, nknots-1] Spl;
    matrix[nknots-1, N] rcs;
    matrix[N, nknots] Xx;
    int km1 = nknots - 1;
    int jp1;
    int j=1;
    for(ia in 1:N) {
     for(ja in 1:nknots) {
      Xx[ia,ja] = (X[ia] - knots[ja] > 0 ? X[ia] - knots[ja] : 0);
        }
    }
     Spl[,1]=X;
     while (j <= nknots - 2) {
     for(i in 1:N) {
      jp1 = j + 1;
       Spl[i,jp1] = (pow(Xx[i,j],3)-(pow(Xx[i,km1],3))*(knots[nknots]-knots[j])/
       (knots[nknots]-knots[km1]) +
       (pow(Xx[i,nknots],3))*(knots[km1]-knots[j])/(knots[nknots]-knots[km1]))/
       pow((knots[nknots]-knots[1]),2);
      }
      j = j + 1;
    }
    "
    }
    
    name4 <- c()
    for (i in 1:(nknots - 1)) {
      name1 <- paste0("", "s", i, sep = "")
      if (i < (nknots - 1)) {
        # name2 <- paste0(' .* to_vector(Spl[,',i,"]') +")
        name2 <- paste0(' .* Spl[,', i, "] +")
      }
      else {
        # name2 <- paste0(' .* to_vector(Spl[,',i,"]') ;\n")
        name2 <- paste0(' .* Spl[,', i, "]")
      }
      name3 <- paste0(name1, name2, sep = "")
      name4[i] <- name3
    }
    name50 <- paste("", name4, collapse = " ")
    
    nameadja <- "A"
    
    ###########
    # For some reasons, 'sitar' (Tim Cole) allows random only 'd' parameter
    # In fact for df > 1, it forces 'd' to be random parameter only
    
    if (match_sitar_d_form) {
      if (grepl("d", randomsi, fixed = T)) {
        if( ept(d_adjustedsi)) nameadja <- "A+(d . * Spl[,1])"
        if(!ept(d_adjustedsi)) nameadja <- "A+(d . * Xm)"
      }
    }
    
    if (!match_sitar_d_form) {
      if (grepl("d", fixedsi, fixed = T)) {
        if( ept(d_adjustedsi)) nameadja <- "A+(d . * Spl[,1])"
        if(!ept(d_adjustedsi)) nameadja <- "A+(d . * Xm)"
      }
    }
    
    
    name5 <- paste(" (", name50, ");\n")
    
    if (grepl("c", fixedsi, fixed = T)) {
      name51 <- paste(" (", name50, ") .* exp(c) ;\n")
      name52 <- paste(" (", name50, ") .* exp(c)^2 ;\n")
    }
    if (!grepl("c", fixedsi, fixed = T)) {
      name51 <- paste(" (", name50, ") ;\n")
      name52 <- paste(" (", name50, ") ;\n")
    }
    
    returnmu <-
      paste0("return(",   paste0(nameadja, "+",
                                 gsub(";", "", name5))     , ");")
    
    # need spaces otherwise rstan 2.21 throws error: variable s1. not found
    returnmu <- gsub("\\s", "", returnmu)
    returnmu <- gsub("\\." , " \\." , returnmu, fixed = FALSE)
    returnmu <- gsub("\\*" , "\\* " , returnmu, fixed = FALSE)
    # don't create space for +
    # returnmu <- gsub("+" , " + " , returnmu, fixed = TRUE)
    
    
    setxoffset_d0_noqr <- paste0(setxoffset,  vectorA)
    returnmu_d0_noqr <- paste0(setxoffset,  vectorA)
    
    if (!is.null(decomp)) {
      if (decomp == 'QR') {
        returnmu <- gsub('Spl', 'XQ', returnmu, fixed = T)
        setxoffset <- paste0(setxoffset,  decomp_code_qr, vectorA)
        returnmu <- gsub('Spl', 'XQ', returnmu, fixed = T)
      }
    }
    
    if (is.null(decomp)) {
      fun_body <- paste0(fun_body, "\n", vectorA)
    }
    
    
    
    endof_fun <-
      paste0("\n    ", returnmu, "\n  } // end of spline function", sep = " ")
    
    
    
    
    start_fun <-
      paste0(
        "\nvector ",
        spfncname,
        "(vector ",
        vector_X_name,
        ", ",
        fullabcsnames_v,
        ") {" ,
        collapse = " "
      )
    
    
    
    
    
    rcsfun <-
      paste(start_fun,
            add_knotinfo,
            fun_body,
            "\n",
            setxoffset,
            endof_fun)
    rcsfun_raw <- rcsfun
    
    
    
    rcsfunmultadd <- NULL
    spfncname_multadd <- paste0(spfncname, "X")
    
    start_fun_multadd <-
      paste0(
        "\nvector ",
        spfncname_multadd,
        "(matrix ",
        vector_X_name,
        ", ",
        fullabcsnames_v,
        ") {" ,
        "\n",
        "  int N=num_elements(",
        vector_X_name,
        "[,1]);",
        "\n",
        "  vector[N] Xm=(",
        vector_X_name,
        "[,1]);",
        # getX
        # "\n",
        # insert_getX_name,
        collapse = " "
      )
    
    
    add_knotinfo_multadd <- paste0(
      "\n  int mcolsmat=cols(",
      vector_X_name,
      ");",
      paste0(
        "\n  vector[mcolsmat+1] knots=",
        paste0(getknotsname, "(", '', ")"),
        ";"
      )
    )
    returnmu_multadd <- returnmu
    returnmu_multadd <-
      gsub("XQ",  vector_X_name, returnmu_multadd, fixed = T)
    returnmu_multadd <-
      gsub("Spl", vector_X_name, returnmu_multadd, fixed = T)
    
    start_fun_multadd <-
      gsub(",)" , ")" , start_fun_multadd, fixed = TRUE)
    endof_fun <- paste0("\n    ",
                        returnmu_multadd,
                        ";",
                        "\n  } // end of spline mat function",
                        sep = " ")
    
    endof_fun <- gsub(";;", ";", endof_fun, fixed = T)
    
    if (grepl('s1', vectorA)) {
      rcsfunmultadd <-
        paste(start_fun_multadd,
              add_knotinfo_multadd,
              vectorA,
              endof_fun)
    } else {
      rcsfunmultadd <- paste(start_fun_multadd, vectorA, endof_fun)
    }
    
    
    
    add_rcsfunmat <- TRUE
    add_rcsfunmatqr <- TRUE
    add_rcsfunmatqrinv <- TRUE
    
    
    funmats <- paste0('', '')
    
    if (add_rcsfunmat) {
      rcsfunmat_name <- paste0(spfncname, 'smat', '')
      start_funmat <-
        paste0(
          "\nmatrix ",
          rcsfunmat_name,
          "(vector ",
          vector_X_name,
          ", ",
          fullabcsnames_v_for_mat,
          ") {" ,
          collapse = " "
        )
      setxoffset_format <- paste0(setxoffset_plane, vectorA)
      rcsfunmat <-
        paste(start_funmat,
              add_knotinfo,
              fun_body,
              "\n",
              setxoffset_format)
      rcsfunmat <- gsub(vectorA, "", rcsfunmat, fixed = T)
      rcsfunmat <- paste0(rcsfunmat, "\n", 'return Spl;')
      rcsfunmat <- paste0(rcsfunmat, '\n}')
      funmats <- paste0(funmats, "\n", rcsfunmat)
    }
    
    if (add_rcsfunmatqr) {
      rcsfunmatgr_name <- paste0(spfncname, 'QRsmat', '')
      
      start_funmat <-
        paste0(
          "\nmatrix ",
          rcsfunmatgr_name,
          "(vector ",
          vector_X_name,
          ", ",
          fullabcsnames_v_for_mat,
          ") {" ,
          collapse = " "
        )
      
      rcsfunmatqr <-
        paste(start_funmat, add_knotinfo, fun_body, "\n", setxoffset)
      rcsfunmatqr <- gsub(vectorA, "", rcsfunmatqr, fixed = T)
      
      
      
      szx <- paste0('s', 1:(nknots - 1))
      cnt <- 0
      cn_c <- c()
      for (vi in szx) {
        cnt <- cnt + 1
        tmx <-
          paste0(
            paste0('s', 'x', '[,', cnt, "]", " = "),
            XR_inv_name_resp,
            '[',
            cnt,
            ",",
            cnt,
            "] * ",
            vi,
            ";"
          )
        cn_c <- c(cn_c, tmx)
      }
      rcsfunmatqr <- paste0(rcsfunmatqr, "\n", 'return XQ;')
      rcsfunmatqr <- paste0(rcsfunmatqr, '\n}')
      funmats <- paste0(funmats, "\n", rcsfunmatqr)
    } # if(add_rcsfunmatqr) {
    
    
    
    if (add_rcsfunmatqrinv) {
      rcsfunmatgrinv_name <- paste0(spfncname, 'QRsmat', 'inv')
      
      start_funmat <-
        paste0(
          "\nmatrix ",
          rcsfunmatgrinv_name,
          "(vector ",
          vector_X_name,
          ", ",
          fullabcsnames_v_for_mat,
          ") {" ,
          collapse = " "
        )
      
      rcsfunmatgrinv <-
        paste(start_funmat, add_knotinfo, fun_body, "\n", setxoffset)
      rcsfunmatgrinv <- gsub(vectorA, "", rcsfunmatgrinv, fixed = T)
      
      szx <- paste0('b_s', 1:(nknots - 1))
      cnt <- 0
      cn_c <- c()
      for (vi in szx) {
        cnt <- cnt + 1
        tmx <-
          paste0(
            paste0('b_s', 'q', '[,', cnt, "]", " = "),
            XR_inv_name_resp,
            '_mat',
            '[',
            cnt,
            ",",
            cnt,
            "] * ",
            vi,
            ";"
          )
        cn_c <- c(cn_c, tmx)
      }
      rcsfunmatgrinv <-
        paste0(rcsfunmatgrinv, "\n", 'return ', XR_inv_name, ';')
      rcsfunmatgrinv <- paste0(rcsfunmatgrinv, '\n}')
      funmats <- paste0(funmats, "\n", rcsfunmatgrinv)
    } # if(add_rcsfunmatqrinv) {
    
    
    
    
    funmats_genquant <- ""
    if (add_rcsfunmatqrinv_genquant) {
      rcsfunmatgrinv_name <- paste0(spfncname, 'QRsmat', 'inv')
      
      setnqq <- nknots - 1
      start_funmat <-
        paste0(
          "\nmatrix ",
          rcsfunmatgrinv_name,
          "(vector ",
          vector_X_name,
          ", ",
          fullabcsnames_v_for_mat,
          ") {" ,
          collapse = " "
        )
      
      rcsfunmatqrinv_genquant <- paste(start_funmat, '')
      rcsfunmatqrinv_genquant <-
        gsub(vectorA, "", rcsfunmatqrinv_genquant, fixed = T)
      
      szx <- paste0(setnlp, 1:(nknots - 1))
      szxbq <- b_sx_name_resp
      
      szx_vector <- paste0(setnlp_vector, 1:(nknots - 1))
      
      
      # setnqq <- 4
      tems <- paste0('placeholder', '_', b_sx_name_resp)
      # b_s_name <- paste0('b_s', '')
      b_s_dims1 <- '[1]'
      svector_ <-
        paste(paste0(b_s_name,  b_s_dims1), collapse = ", ")
      svector_ <-
        paste0('vector[',
               setnqq,
               ']',
               " ",
               tems,
               ' = ',
               '[ ',
               svector_,
               ' ]' ,
               "'" ,
               ";")
      
      tems2 <-  paste0('temp_', b_sx_name_resp)
      
      tems3 <- paste0('vector[', setnqq, ']', " ", tems2, ' = ')
      qs_vector <-
        paste0(tems3, XR_inv_name_resp , ' * ', tems, ";")
      
      cn_c <- c()
      cn_c2 <- c()
      cnt <- 0
      for (vi in szx) {
        cnt <- cnt + 1
        tmx <-
          paste0(
            'vector[N] ',
            paste0(szxbq_vector, cnt, " = "),
            XR_inv_name_resp,
            '[',
            cnt,
            ",",
            cnt,
            "] * ",
            vi,
            ";"
          )
        cn_c <- c(cn_c, tmx)
      }
      cnt <- 0
      for (vi in szx_vector) {
        cnt <- cnt + 1
        # '[', cnt, ',', cnt, "]"
        tmx2 <-
          paste0(paste0(szxbq, '[', cnt, ']', '', " = "),
                 tems2,
                 '[',
                 cnt,
                 "]",
                 ";")
        
        cn_c2 <- c(cn_c2, tmx2)
      }
      
      # if no covariate. then only add re-scaled s betas
      if (add_b_Qr_genquan_s_coef) {
        cn_c <- paste(cn_c, collapse = "\n")
        cn_c2 <- paste(cn_c2, collapse = "\n")
        cn_c <- paste0(cn_c2, "\n", cn_c)
        addcn_c2 <-
          paste0('vector[', "", setnqq, ']', " ", szxbq, ';')
        addcn_c2 <- paste0(addcn_c2, "\n", svector_)
        addcn_c2 <- paste0(addcn_c2, "\n", qs_vector)
        addcn_c2 <- paste0(addcn_c2, "\n", cn_c)
      } else if (!add_b_Qr_genquan_s_coef) {
        cn_c <- paste(cn_c, collapse = "\n")
        addcn_c2 <- cn_c
      }
      replacematrixby <-
        paste0('matrix[', setnqq, ", ", setnqq, ']', XR_inv_name_resp , ' =')
      rcsfunmatqrinv_genquant <-
        gsub('matrix',
             replacematrixby,
             rcsfunmatqrinv_genquant,
             fixed = T)
      rcsfunmatqrinv_genquant <-
        gsub('{', ';', rcsfunmatqrinv_genquant, fixed = T)
      rcsfunmatqrinv_genquant <-
        paste0(rcsfunmatqrinv_genquant, '\n', addcn_c2)
    } # if(add_rcsfunmatqrinv_genquant) {
    
   
    if (funmats == "") {
      add_funmats <- FALSE
    } else {
      add_funmats <- TRUE
    }
    
   
    
    
    getknots_fun_raw <-
      paste0(
        "vector ",
        getknotsname,
        "() {" ,
        paste0("\n  int nknots=", eval(parse(text = nknots)), ";"),
        paste0(
          "\n  vector[nknots] knots=",
          "[",
          paste(knots, collapse = ","),
          "]';"
        ),
        "\n  ",
        "return(knots);",
        "\n}  "
        ,
        paste0("// end of ", getknotsname),
        collapse = " "
      )
    
    # add_context_getx_fun
    getx_fun_raw <-
      paste0(
        "vector ",
        getxname,
        paste0(" (vector ", vector_X_name, ") {") ,
        "\n  ",
        paste0("int N=num_elements(", vector_X_name, ");"),
        "\n  ",
        paste0("real xoffset = ", xoffset, ";"),
        paste0("\n  int tranform_x = ",
               eval(parse(text = tranform_x_int)), ";"),
        paste0("\n  vector[N] x;"),
        "\n",
        paste0(
          "  if(tranform_x == 0 ) {",
          "\n   ",
          "x = ",
          vector_X_name,
          " - xoffset",
          ";",
          "\n  }",
          "\n  ",
          "if(tranform_x == 1 ) {",
          "\n   ",
          "x = log(",
          vector_X_name,
          ") - xoffset;",
          "\n  }",
          "\n  ",
          "if(tranform_x == 2 ) {",
          "\n    ",
          "x = sqrt(",
          vector_X_name,
          ") - xoffset;",
          "\n  }"
        ),
        "\n  ",
        "return(x);",
        "\n}  "
        ,
        paste0("// end of ", getxname),
        collapse = " "
      )
    
    
    
    
    getx_fun     <- paste0(add_context_getx_fun, "\n", getx_fun_raw)
    getknots_fun <-
      paste0(add_context_getknots_fun, "\n", getknots_fun_raw)
    
    getx_knots_fun <- paste0(getx_fun,
                             "\n",
                             getknots_fun)
    
    
    ##########
    
    # Create function d0
    fnameout <- paste0(spfncname, "_", "d0")
    spl <- "Spl[,1]=X;"
    splout <- spl
    spl_fun_ford <- paste0(fnameout,
                           "(vector ",
                           vector_X_name,
                           ", ",
                           fullabcsnames_v,
                           ")")
    spl_fun_ford <- gsub("vector", "", spl_fun_ford, fixed = T)
    
    if ((backend == "rstan" &
         utils::packageVersion("rstan") >= "2.26.1") |
        backend == "mock" |
        backend == "cmdstanr") {
      body <- "
     while (j <= nknots - 2) {
      jp1 = j + 1;
      Spl[,jp1] =
        (1*Xx[,j]^3) * (1/((knots[nknots]-knots[1])^2))  -
        (1*Xx[,km1]^3) * (knots[nknots]-knots[j]) /
        ((knots[nknots]-knots[km1]) * (knots[nknots]-knots[1])^2) +
        (1*Xx[,nknots]^3) * (knots[km1]-knots[j]) /
        ((knots[nknots]-knots[km1]) * (knots[nknots]-knots[1])^2) ;
      j = j + 1;
    }
    "
    }
    
    if ((backend == "rstan" &
         utils::packageVersion("rstan") < "2.26.1") | # &
        backend == "mock" &
        backend != "cmdstanr") {
      body <- "
     while (j <= nknots - 2) {
      for(i in 1:N) {
          jp1 = j + 1;
          Spl[i,jp1] = (1*pow(Xx[i,j],3) -
          (1*pow(Xx[i,km1],3))*(knots[nknots]-knots[j]) /
          (knots[nknots]-knots[km1]) +
          (1*pow(Xx[i,nknots],3))*(knots[km1]-knots[j]) /
          (knots[nknots]-knots[km1])) /
          pow((knots[nknots]-knots[1]),2);
      }
      j = j + 1;
    }
    "
    }
    
    
    spl_d0 <- create_internal_function(
      y = y,
      function_str = rcsfun,
      fname = spfncname,
      fnameout = fnameout,
      spl = spl,
      splout = splout,
      xfunsi = xfunsi,
      yfunsi = yfunsi,
      setxoffset = setxoffset,
      # setxoffset setxoffset_d0_noqr
      gsub_out_unscaled = NULL,
      # gsub_out_unscaled = c('QR', 'Spl')
      body = body,
      vectorA = vectorA,
      decomp = decomp,
      fixedsi = fixedsi
    )
    
    
    
    # Create function d1
    fnameout <- paste0(spfncname, "_", "d1")
    spl <- "Spl[,1]=X;"
    splout <- "Spl[,1]=rep_vector(1, N);"
    
    if ((backend == "rstan" &
         utils::packageVersion("rstan") >= "2.26.1") |
        backend == "mock" |
        backend == "cmdstanr") {
      body <- "
     while (j <= nknots - 2) {
      jp1 = j + 1;
      Spl[,jp1] =
        (3*Xx[,j]^2) * (1/((knots[nknots]-knots[1])^2))  -
        (3*Xx[,km1]^2) * (knots[nknots]-knots[j]) /
        ((knots[nknots]-knots[km1]) * (knots[nknots]-knots[1])^2) +
        (3*Xx[,nknots]^2) * (knots[km1]-knots[j]) /
        ((knots[nknots]-knots[km1]) * (knots[nknots]-knots[1])^2) ;
      j = j + 1;
    }
    "
    }
    
    if ((backend == "rstan" &
         utils::packageVersion("rstan") < "2.26.1") | # &
        backend == "mock" &
        backend != "cmdstanr") {
      body <- "
     while (j <= nknots - 2) {
      for(i in 1:N) {
          jp1 = j + 1;
          Spl[i,jp1] = (3*pow(Xx[i,j],2) -
          (3*pow(Xx[i,km1],2))*(knots[nknots]-knots[j]) /
          (knots[nknots]-knots[km1]) +
          (3*pow(Xx[i,nknots],2))*(knots[km1]-knots[j]) /
          (knots[nknots]-knots[km1])) /
          pow((knots[nknots]-knots[1]),2);
      }
      j = j + 1;
    }
    "
    }
    
    
    spl_d1 <- create_internal_function(
      y = y,
      function_str = rcsfun,
      fname = spfncname,
      fnameout = fnameout,
      spl = spl,
      splout = splout,
      xfunsi = xfunsi,
      yfunsi = yfunsi,
      setxoffset = setxoffset,
      # setxoffset setxoffset_d0_noqr
      gsub_out_unscaled = NULL,
      # gsub_out_unscaled = c('QR', 'Spl')
      body = body,
      vectorA = vectorA,
      decomp = decomp,
      fixedsi = fixedsi
    )
    
    
    # Create function d2
    fnameout <- paste0(spfncname, "_", "d2")
    spl <- "Spl[,1]=X;"
    splout <- "Spl[,1]=rep_vector(0, N);"
    
    if ((backend == "rstan" &
         utils::packageVersion("rstan") >= "2.26.1") |
        backend == "mock" |
        backend == "cmdstanr") {
      body <- "
     while (j <= nknots - 2) {
      jp1 = j + 1;
      Spl[,jp1] =
        (6*Xx[,j]^1) * (1/((knots[nknots]-knots[1])^2))  -
        (6*Xx[,km1]^1) * (knots[nknots]-knots[j]) /
        ((knots[nknots]-knots[km1]) * (knots[nknots]-knots[1])^2) +
        (6*Xx[,nknots]^1) * (knots[km1]-knots[j]) /
        ((knots[nknots]-knots[km1]) * (knots[nknots]-knots[1])^2) ;
      j = j + 1;
   }
    "
    }
    
    if ((backend == "rstan" &
         utils::packageVersion("rstan") < "2.26.1") | # &
        backend == "mock" &
        backend != "cmdstanr") {
      body <- "
     while (j <= nknots - 2) {
      for(i in 1:N) {
          jp1 = j + 1;
          Spl[i,jp1] = (6*pow(Xx[i,j],1) -
          (6*pow(Xx[i,km1],1))*(knots[nknots]-knots[j]) /
          (knots[nknots]-knots[km1]) +
          (6*pow(Xx[i,nknots],1))*(knots[km1]-knots[j]) /
          (knots[nknots]-knots[km1])) /
          pow((knots[nknots]-knots[1]),2);
      }
      j = j + 1;
    }
    "
    }
    
    
    
    spl_d2 <- create_internal_function(
      y = y,
      function_str = rcsfun,
      fname = spfncname,
      fnameout = fnameout,
      spl = spl,
      splout = splout,
      xfunsi = xfunsi,
      yfunsi = yfunsi,
      setxoffset = setxoffset,
      # setxoffset setxoffset_d0_noqr
      gsub_out_unscaled = NULL,
      # gsub_out_unscaled = c('QR', 'Spl')
      body = body,
      vectorA = vectorA,
      decomp = decomp,
      fixedsi = fixedsi
    )
    
    
    if (utils::packageVersion('rstan') < 2.26) {
      rcsfun <- paste(getx_knots_fun, rcsfun)
    }
    
    if (utils::packageVersion('rstan') > 2.26 & is.null(decomp)) {
      rcsfun <- paste0(getx_knots_fun,
                       rcsfun,
                       rcsfunmultadd,
                       spl_d0,
                       spl_d1,
                       spl_d2,
                       sep = "\n")
    }
    
    if (utils::packageVersion('rstan') > 2.26 & !is.null(decomp)) {
      if (decomp == 'QR') {
        if (add_funmats) {
          rcsfun <- paste0(
            getx_knots_fun,
            funmats,
            rcsfun,
            rcsfunmultadd,
            spl_d0,
            spl_d1,
            spl_d2,
            sep = "\n"
          )
        } else if (!add_funmats) {
          rcsfun <- paste0(getx_knots_fun,
                           rcsfun,
                           rcsfunmultadd,
                           spl_d0,
                           spl_d1,
                           spl_d2,
                           sep = "\n")
        }
      }
    }
    
    
  } # if(select_model == 'sitar') {
  
  
  
  
  
  if (grepl("^pb", select_model) |
      grepl("^logistic", select_model)) {
    abcnames <- paste0(strsplit(gsub("\\+", " ",
                                     fixedsi), " ")[[1]], sep = ",")
    fullabcsnames <- abcnames
    fullabcsnames_v <-
      paste("vector", fullabcsnames, collapse = " ")
    defineEx <- paste0("(Xm)")
    getx_fun_raw <-
      paste0(
        "vector ",
        getxname,
        paste0(" (vector ", vector_X_name, ") {") ,
        "\n  ",
        paste0("int N=num_elements(", vector_X_name, ");"),
        "\n  ",
        setxoffset,
        paste0("\n  int tranform_x = ",
               eval(parse(text = tranform_x_int)), ";"),
        paste0("\n  vector[N] x;"),
        "\n",
        paste0(
          "  if(tranform_x == 0 ) {",
          "\n   ",
          "x = ",
          vector_X_name,
          " - xoffset",
          ";",
          "\n  }",
          "\n  ",
          "if(tranform_x == 1 ) {",
          "\n   ",
          "x = log(",
          vector_X_name,
          ") - xoffset;",
          "\n  }",
          "\n  ",
          "if(tranform_x == 2 ) {",
          "\n    ",
          "x = sqrt(",
          vector_X_name,
          ") - xoffset;",
          "\n  }"
        ),
        "\n  ",
        "return(x);",
        "\n}  "
        ,
        paste0("// end of ", getxname),
        collapse = " "
      )
    
    
    getx_fun     <- paste0(add_context_getx_fun, "\n", getx_fun_raw)
    
    # a - asymtote
    # b - size at theta
    # c - s0
    # d - s1
    # e - time (theta)
    
    if (select_model == 'pb1') {
      funstring <- "a-2.0*(a-b)./(exp(c.*(Xm-e))+exp(d.*(Xm-e)))"
      if (utils::packageVersion('rstan') < 2.26)
        funstring <-
          gsub(".*", " .* ", funstring, fixed = T)
      returnmu    <- paste0("return ", "(",  funstring, ")")
      returnmu_d0 <- funstring
      returnmu_d1 <- "rep_vector(2.0,N).*(a-b).*(c.*exp(c.*(Xm-e))+
      d.*exp(d.*(Xm-e)))./(exp(c.*(Xm-e))+exp(d.*(Xm-e)))^2.0"
      
      returnmu_d2 <- "-rep_vector(4.0,N).*(a-b).*(c.*exp(c.*(Xm-e))+
      d.*exp(d.*(Xm-e)))^2.0./(exp(c.*(Xm-e))+exp(d.*(Xm-e)))^3.0+
      rep_vector(2.0,N).*(a-b).*(c^2.0.*exp(c.*(Xm-e))+
      d^2.0.*exp(d.*(Xm-e)))./(exp(c.*(Xm-e))+exp(d.*(Xm-e)))^2.0"
      
      returnmu_d3 <- "rep_vector(12.0,N).*(a-b).*(c.*exp(c.*(Xm-e))+
      d.*exp(d.*(Xm-e)))^3.0./(exp(c.*(Xm-e))+
      exp(d.*(Xm-e)))^4.0-rep_vector(12.0,N).*(a-b).*(c.*exp(c.*(Xm-e))+
      d.*exp(d.*(Xm-e))).*(c^2.0.*exp(c.*(Xm-e))+
      d^2.0.*exp(d.*(Xm-e)))./(exp(c.*(Xm-e))+
      exp(d.*(Xm-e)))^3.0+rep_vector(2.0,N).*(a-b).*(c^3.0.*exp(c.*(Xm-e))+
      d^3.0.*exp(d.*(Xm-e)))./(exp(c.*(Xm-e))+exp(d.*(Xm-e)))^2.0"
      
      returnmu_d1 <- paste0(returnmu_d1, ";")
      returnmu_d2 <- paste0(returnmu_d2, ";")
      returnmu_d3 <- paste0(returnmu_d3, ";")
    } # if(select_model == 'pb') {
    
    # a - asymtote
    # b - size at theta
    # c - s0
    # d - s1
    # e - time (theta)
    # f - gamma
    
    if (select_model == 'pb2') {
      funstring <- "a-((a-b)./(((0.5*exp((f.*c).*(Xm-e)))+
      (0.5*exp((f.*d).*(Xm-e))))^(1.0./f)))"
      if (utils::packageVersion('rstan') < 2.26)
        funstring <-
          gsub(".*", " .* ", funstring, fixed = T)
      returnmu    <- paste0("return ", "(",  funstring, ")")
      returnmu_d0 <- funstring
      returnmu_d1 <-
        "(a-b).*(rep_vector(0.5,N).*f.*c.*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*f.*d.*exp(f.*d.*(Xm-e)))./
      ((rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e)))^
      (rep_vector(1.0,N)./f).*f.*(rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e))))"
      
      returnmu_d2 <-
        "-(a-b).*(rep_vector(0.5,N).*f.*c.*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*f.*d.*exp(f.*d.*(Xm-e)))^2.0./
      ((rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e)))^
      (rep_vector(1.0,N)./f).*f^2.0.*(rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e)))^2.0)+
      (a-b).*(rep_vector(0.5,N).*f^2.0.*c^2.0.*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*f^2.0.*d^2.0.*exp(f.*d.*(Xm-e)))./
      ((rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e)))^
      (rep_vector(1.0,N)./f).*f.*(rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e))))-
      (a-b).*(rep_vector(0.5,N).*f.*c.*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*f.*d.*exp(f.*d.*(Xm-e)))^2.0./
      ((rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e)))^
      (rep_vector(1.0,N)./f).*f.*(rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e)))^2.0)"
      
      returnmu_d3 <-
        "(a-b).*(rep_vector(0.5,N).*f.*c.*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*f.*d.*exp(f.*d.*(Xm-e)))^
      3.0./((rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e)))^
      (rep_vector(1.0,N)./f).*f^3.0.*(rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e)))^3.0)-
      rep_vector(3.0,N).*(a-b).*(rep_vector(0.5,N).*f.*c.*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*f.*d.*exp(f.*d.*(Xm-e))).*(rep_vector(0.5,N).*f^
      2.0.*c^2.0.*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*f^2.0.*d^2.0.*exp(f.*d.*(Xm-e)))./
      ((rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e)))^
      (rep_vector(1.0,N)./f).*f^2.0.*(rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e)))^2.0)+
      rep_vector(3.0,N).*(a-b).*(rep_vector(0.5,N).*f.*c.*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*f.*d.*exp(f.*d.*(Xm-e)))^3.0./
      ((rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e)))^(rep_vector(1.0,N)./f).*f^
      2.0.*(rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e)))^3.0)+
      (a-b).*(rep_vector(0.5,N).*f^3.0.*c^
      3.0.*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*f^3.0.*d^3.0.*exp(f.*d.*(Xm-e)))./
      ((rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e)))^
      (rep_vector(1.0,N)./f).*f.*(rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e))))-
      rep_vector(3.0,N).*(a-b).*(rep_vector(0.5,N).*f^
      2.0.*c^2.0.*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*f^2.0.*d^
      2.0.*exp(f.*d.*(Xm-e))).*(rep_vector(0.5,N).*f.*c.*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*f.*d.*exp(f.*d.*(Xm-e)))./
      ((rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e)))^
      (rep_vector(1.0,N)./f).*f.*(rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e)))^2.0)+
      rep_vector(2.0,N).*(a-b).*(rep_vector(0.5,N).*f.*c.*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*f.*d.*exp(f.*d.*(Xm-e)))^3.0./
      ((rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e)))^
      (rep_vector(1.0,N)./f).*f.*(rep_vector(0.5,N).*exp(f.*c.*(Xm-e))+
      rep_vector(0.5,N).*exp(f.*d.*(Xm-e)))^3.0)"
      
      returnmu_d1 <- paste0(returnmu_d1, ";")
      returnmu_d2 <- paste0(returnmu_d2, ";")
      returnmu_d3 <- paste0(returnmu_d1, ";")
    } # if(select_model == 'pb2') {
    
    
    # a - asymtote
    # b - size at theta
    # c - s0
    # d - s1
    # e - time (theta)
    # f - gamma
    
    if (select_model == 'pb3') {
      funstring <- "a-((4.0*(a-b))./((exp(f.*(Xm-e))+
      exp(c.*(Xm-e))).*(1.0+exp(d.*(Xm-e)))))"
      if (utils::packageVersion('rstan') < 2.26)
        funstring <- gsub(".*", " .* ",
                          funstring,
                          fixed = T)
      returnmu    <- paste0("return ", "(",  funstring, ")")
      returnmu_d0 <- funstring
      returnmu_d1 <-
        "rep_vector(4.0,N).*(a-b).*(f.*exp(f.*(Xm-e))+
      c.*exp(c.*(Xm-e)))./((exp(f.*(Xm-e))+exp(c.*(Xm-e)))^
      2.0.*(rep_vector(1.0,N)+exp(d.*(Xm-e))))+
      rep_vector(4.0,N).*(a-b).*d.*exp(d.*(Xm-e))./
      ((exp(f.*(Xm-e))+exp(c.*(Xm-e))).*(rep_vector(1.0,N)+
      exp(d.*(Xm-e)))^2.0)"
      
      returnmu_d2 <-
        "-rep_vector(8.0,N).*(a-b).*(f.*exp(f.*(Xm-e))+
      c.*exp(c.*(Xm-e)))^2.0./((exp(f.*(Xm-e))+exp(c.*(Xm-e)))^
      3.0.*(rep_vector(1.0,N)+exp(d.*(Xm-e))))-
      rep_vector(8.0,N).*(a-b).*(f.*exp(f.*(Xm-e))+
      c.*exp(c.*(Xm-e))).*d.*exp(d.*(Xm-e))./((exp(f.*(Xm-e))+
      exp(c.*(Xm-e)))^2.0.*(rep_vector(1.0,N)+exp(d.*(Xm-e)))^2.0)+
      rep_vector(4.0,N).*(a-b).*(f^2.0.*exp(f.*(Xm-e))+c^2.0.*exp(c.*(Xm-e)))./
      ((exp(f.*(Xm-e))+exp(c.*(Xm-e)))^2.0.*(rep_vector(1.0,N)+
      exp(d.*(Xm-e))))-rep_vector(8.0,N).*(a-b).*d^
      2.0.*exp(d.*(Xm-e))^2.0./((exp(f.*(Xm-e))+
      exp(c.*(Xm-e))).*(rep_vector(1.0,N)+exp(d.*(Xm-e)))^3.0)+
      rep_vector(4.0,N).*(a-b).*d^2.0.*exp(d.*(Xm-e))./((exp(f.*(Xm-e))+
      exp(c.*(Xm-e))).*(rep_vector(1.0,N)+exp(d.*(Xm-e)))^2.0)"
      
      returnmu_d3 <-
        "rep_vector(24.0,N).*(a-b).*(f.*exp(f.*(Xm-e))+
      c.*exp(c.*(Xm-e)))^3.0./((exp(f.*(Xm-e))+exp(c.*(Xm-e)))^
      4.0.*(rep_vector(1.0,N)+exp(d.*(Xm-e))))+
      rep_vector(24.0,N).*(a-b).*(f.*exp(f.*(Xm-e))+c.*exp(c.*(Xm-e)))^
      2.0.*d.*exp(d.*(Xm-e))./((exp(f.*(Xm-e))+exp(c.*(Xm-e)))^
      3.0.*(rep_vector(1.0,N)+exp(d.*(Xm-e)))^2.0)-
      rep_vector(24.0,N).*(a-b).*(f.*exp(f.*(Xm-e))+
      c.*exp(c.*(Xm-e))).*(f^2.0.*exp(f.*(Xm-e))+c^2.0.*exp(c.*(Xm-e)))./
      ((exp(f.*(Xm-e))+exp(c.*(Xm-e)))^
      3.0.*(rep_vector(1.0,N)+exp(d.*(Xm-e))))+
      rep_vector(24.0,N).*(a-b).*(f.*exp(f.*(Xm-e))+
      c.*exp(c.*(Xm-e))).*d^2.0.*exp(d.*(Xm-e))^
      2.0./((exp(f.*(Xm-e))+exp(c.*(Xm-e)))^
      2.0.*(rep_vector(1.0,N)+exp(d.*(Xm-e)))^3.0)-
      rep_vector(12.0,N).*(a-b).*(f^
      2.0.*exp(f.*(Xm-e))+c^2.0.*exp(c.*(Xm-e))).*d.*exp(d.*(Xm-e))./
      ((exp(f.*(Xm-e))+exp(c.*(Xm-e)))^
      2.0.*(rep_vector(1.0,N)+exp(d.*(Xm-e)))^2.0)-
      rep_vector(12.0,N).*(a-b).*(f.*exp(f.*(Xm-e))+
      c.*exp(c.*(Xm-e))).*d^2.0.*exp(d.*(Xm-e))./((exp(f.*(Xm-e))+
      exp(c.*(Xm-e)))^2.0.*(rep_vector(1.0,N)+exp(d.*(Xm-e)))^2.0)+
      rep_vector(4.0,N).*(a-b).*(f^3.0.*exp(f.*(Xm-e))+c^
      3.0.*exp(c.*(Xm-e)))./((exp(f.*(Xm-e))+exp(c.*(Xm-e)))^
      2.0.*(rep_vector(1.0,N)+exp(d.*(Xm-e))))+
      rep_vector(24.0,N).*(a-b).*d^3.0.*exp(d.*(Xm-e))^3.0./
      ((exp(f.*(Xm-e))+exp(c.*(Xm-e))).*(rep_vector(1.0,N)+
      exp(d.*(Xm-e)))^4.0)-rep_vector(24.0,N).*(a-b).*d^3.0.*exp(d.*(Xm-e))^
      2.0./((exp(f.*(Xm-e))+
      exp(c.*(Xm-e))).*(rep_vector(1.0,N)+exp(d.*(Xm-e)))^3.0)+
      rep_vector(4.0,N).*(a-b).*d^3.0.*exp(d.*(Xm-e))./
      ((exp(f.*(Xm-e))+exp(c.*(Xm-e))).*(rep_vector(1.0,N)+exp(d.*(Xm-e)))^2.0)"
      
      returnmu_d1 <- paste0(returnmu_d1, ";")
      returnmu_d2 <- paste0(returnmu_d2, ";")
      returnmu_d3 <- paste0(returnmu_d3, ";")
    } # if(select_model == 'pb3') {
    
    
    
    # a - asymptote
    # b - rate constant
    # c - time at midpoint
    
    if (select_model == 'logistic1') {
      funstring <- "a./(1+exp(-b.*(Xm-c)))"
      if (utils::packageVersion('rstan') < 2.26)
        funstring <-
          gsub(".*", " .* ", funstring, fixed = T)
      returnmu    <- paste0("return ", "(",  funstring, ")")
      returnmu_d0 <- funstring
      returnmu_d1 <-
        "a.*b.*exp(-b.*(Xm - c))./(1 + exp(-b.*(Xm - c)))^2.0"
      
      returnmu_d2 <-
        "rep_vector(2.0,N).*a.*b^2.0.*exp(-b.*(Xm - c))^2.0./
        (1 + exp(-b.*(Xm - c)))^3.0 -
        a.*b^2.0.*exp(-b.*(Xm - c))./
        (1 + exp(-b.*(Xm - c)))^2.0"
      
      returnmu_d3 <-
        "rep_vector(6.0,N).*a.*b^3.0.*exp(-b.*(Xm - c))^3.0./
        (1 + exp(-b.*(Xm - c)))^4.0 -
        rep_vector(6.0,N).*a.*b^3.0.*exp(-b.*(Xm - c))^2.0./
        (1 + exp(-b.*(Xm - c)))^3.0 +
        a.*b^3.0.*exp(-b.*(Xm - c))./
        (1 + exp(-b.*(Xm - c)))^2.0"
      
      returnmu_d1 <- paste0(returnmu_d1, ";")
      returnmu_d2 <- paste0(returnmu_d2, ";")
      returnmu_d3 <- paste0(returnmu_d3, ";")
    } # if(select_model == 'logistic1') {
    
    
    
    # a - asymtote
    # b - size at theta
    # c - s0
    # d - theta1
    # e - s1
    # f - theta2
    
    # wolfram suggests -> (b/(1+exp(-c*(x-d)))) + ((a-b)/(1+exp(-e*(x-f))))
    # maple  -> ((a-b)/(1+exp(-e*(x-f)))) + (b/(1+exp(-c*(x-d))))
    
    # wolfram suggests
    # (a - b)/(exp(-e (x - f)) + 1) + b/(exp(-c (x - d)) + 1)
    
    if (select_model == 'logistic2') {
      funstring <-
        "((a-b)./(1+exp(-e.*(Xm-f)))) + (b./(1+exp(-c.*(Xm-d))))"
      if (utils::packageVersion('rstan') < 2.26)
        funstring <-
          gsub(".*", " .* ", funstring, fixed = T)
      returnmu    <- paste0("return ", "(",  funstring, ")")
      returnmu_d0 <- funstring
      returnmu_d1 <-
        "(e .* (a - b) .* exp(e .*(f + Xm)))./(exp(e .*f) + 
      exp(e .*Xm))^2.0 + (b .*c .* exp(c.* (d + Xm)))./(exp(c .*d) + 
      exp(c.* Xm))^2.0"
      
      
      returnmu_d2 <-
        "(a - b) .* ((2.0 .* e^2.0 .* exp(-2.0 .* e .* (Xm - f))) ./
      (exp(e .*(-(Xm - f))) + 1.0)^3.0- (e^2.0 .* exp(e .* (-(Xm - f))))./
      (exp(e .* (-(Xm - f))) + 1.0)^2.0) + b .* ((2.0 .* c^2.0 .* 
      exp(-2.0 .* c .* (Xm - d)))./(e^(-c .* (Xm - d)) + 1.0)^3.0 - 
      (c^2.0 .* exp(-c .*(Xm - d)))./(exp(-c .* (Xm - d)) + 1.0)^2.0)"
      
      
      returnmu_d3 <-
        "(a - b) .* ((e^3.0 .* exp(e .*(-(Xm - f))))./
      (exp(e .* (-(Xm - f))) + 1.0)^2.0 - (6.0 .* e^3.0 .* 
      exp(-2.0 .* e .* (Xm - f)))./(exp(e .* (-(Xm - f))) + 1.0)^3.0 + 
      (6.0 .* e^3.0 .* exp(-3.0 .* e .* (Xm - f)))./
      (exp(e .* (-(Xm - f))) + 1.0)^4.0.0) + b .* 
      ((c^3.0 .* exp(-c .* (Xm - d)))./(exp(-c .* (Xm - d)) + 1.0)^2.0 - 
      (6.0 .* c^3.0 .* exp(-2.0 .* c .* (Xm - d)))./
      (exp(-c .* (Xm - d)) + 1.0)^3.0 + (6.0 .* c^3.0 .* 
      exp(-3.0 .* c (Xm - d)))./(exp(-c .* (Xm - d)) + 1.0)^4.0)"
      
      
      returnmu_d1 <- paste0(returnmu_d1, ";")
      returnmu_d2 <- paste0(returnmu_d2, ";")
      returnmu_d3 <- paste0(returnmu_d3, ";")
    } # if(select_model == 'logistic2') {
    
    
    
    # a - size at infancy
    # b - rate at infancy
    # c - time at infancy
    # d - size at preadolescence
    # e - rate at preadolescence
    # f - time at preadolescence
    # g - size at adolescence
    # h - rate at adolescence
    # i - time at adolescence
    
    if (select_model == 'logistic3') {
      funstring <-
        "(a ./ (1 + exp(-b .* (Xm - c)))) +
        (d ./ (1 + exp(-e .* (Xm - f)))) +
        (g ./ (1 + exp(-h .* (Xm - i))))"
      if (utils::packageVersion('rstan') < 2.26)
        funstring <-
          gsub(".*", " .* ", funstring, fixed = T)
      returnmu    <- paste0("return ", "(",  funstring, ")")
      returnmu_d0 <- funstring
      
      
      returnmu_d1 <-
        "(a .* b .* exp(-b .* (Xm - c)))./(exp(-b .* (Xm - c)) + 1)^2.0 +
        (d .* e .* exp(-e .* (Xm - f)))./(exp(-e .* (Xm - f)) + 1)^2.0 +
        (g .* h .* exp(-h .* (Xm - i)))./(exp(-h .* (Xm - i)) + 1)^2.0"
      
      returnmu_d2 <-
        "(a .* ((rep_vector(2.0,N) .* b^2.0 .*
                  exp(-rep_vector(2.0,N) .* b .* (Xm - c))) ./
                 (exp(-b .* (Xm - c)) + 1)^3.0 -
                 (b^2.0 .* exp(-b .* (Xm - c))) ./
                 (exp(-b .* (Xm - c)) + 1.0)^2.0)) +
        (d .* ((rep_vector(2.0,N) .* e^2.0 .*
                  exp(-rep_vector(2.0,N) .* e .* (Xm - f))) ./
                 (exp(-e .* (Xm - f)) + 1)^3.0 -
                 (e^2.0 .* exp(-e .* (Xm - f))) ./
                 (exp(-e .* (Xm - f)) + 1.0)^2.0)) +
        (g .* ((rep_vector(2.0,N) .* h^2.0 .*
                  exp(-rep_vector(2.0,N) .* h .* (Xm - i))) ./
                 (exp(-h .* (Xm - i)) + 1)^3.0 -
                 (h^2.0 .* exp(-h .* (Xm - i))) ./
                 (exp(-h .* (Xm - i)) + 1.0)^2.0)) "
      
      returnmu_d3 <- returnmu_d2
      
      
      
      
      returnmu_d1 <- paste0(returnmu_d1, ";")
      returnmu_d2 <- paste0(returnmu_d2, ";")
      returnmu_d3 <- paste0(returnmu_d3, ";")
    } # if(select_model == 'logistic3') {
    
    
    
   
    
    
    insert_getX_name <-
      paste0("  vector[N] Xm = ", getxname, "(Xp);")
    start_fun <-
      paste0(
        "\nvector ",
        spfncname,
        "(vector ",
        vector_X_name,
        ", ",
        fullabcsnames_v,
        ") {" ,
        "\n",
        "  int N = num_elements(Xp);",
        "\n",
        insert_getX_name,
        collapse = " "
      )
    
    start_fun <- gsub(",)" , ")" , start_fun, fixed = TRUE)
    endof_fun <- paste0("\n    ", returnmu,
                        ";", "\n  } // end of spline function", sep = " ")
    
    
    rcsfun <- paste(start_fun, endof_fun)
    rcsfun_raw <- rcsfun
    
    # Create function d0
    fnameout <- paste0(spfncname, "_", "d0")
    
    spl_fun_ford <-
      paste0(fnameout,
             "(vector ",
             vector_X_name,
             ", ",
             fullabcsnames_v,
             ")")
    spl_fun_ford <- gsub("vector", "", spl_fun_ford, fixed = T)
    spl_fun_ford <- gsub("[[:space:]]", "", spl_fun_ford)
    spl_fun_ford <- gsub(",)", ")", spl_fun_ford, fixed = T)
    
    
    spl_d0 <- create_internal_function_nonsitar(
      y = y,
      function_str = rcsfun,
      fname = spfncname,
      fnameout = fnameout,
      returnmu = returnmu,
      xfunsi = xfunsi,
      yfunsi = yfunsi,
      setxoffset = setxoffset,
      gsub_out_unscaled = NULL,
      spl_fun_ford = spl_fun_ford,
      body = returnmu_d0,
      decomp = decomp,
      fixedsi = fixedsi
    )
    
    # Create function d1
    fnameout <- paste0(spfncname, "_", "d1")
    spl_d1 <- create_internal_function_nonsitar(
      y = y,
      function_str = rcsfun,
      fname = spfncname,
      fnameout = fnameout,
      returnmu = returnmu,
      xfunsi = xfunsi,
      yfunsi = yfunsi,
      setxoffset = setxoffset,
      # setxoffset setxoffset_d0_noqr
      gsub_out_unscaled = NULL,
      # gsub_out_unscaled = c('QR', 'Spl')
      spl_fun_ford = spl_fun_ford,
      body = returnmu_d1,
      decomp = decomp,
      fixedsi = fixedsi
    )
    
    # Create function d2
    fnameout <- paste0(spfncname, "_", "d2")
    spl_d2 <- create_internal_function_nonsitar(
      y = y,
      function_str = rcsfun,
      fname = spfncname,
      fnameout = fnameout,
      returnmu = returnmu,
      xfunsi = xfunsi,
      yfunsi = yfunsi,
      setxoffset = setxoffset,
      # setxoffset setxoffset_d0_noqr
      gsub_out_unscaled = NULL,
      # gsub_out_unscaled = c('QR', 'Spl')
      spl_fun_ford = spl_fun_ford,
      body = returnmu_d2,
      decomp = decomp,
      fixedsi = fixedsi
    )
    
    
    rcsfunmultadd <- NULL
    
    if (utils::packageVersion('rstan') > 2.26 & is.null(decomp)) {
      rcsfun <- paste0(getx_fun,
                       rcsfun,
                       rcsfunmultadd,
                       spl_d0,
                       spl_d1,
                       spl_d2,
                       sep = "\n")
    }
    
    if (utils::packageVersion('rstan') > 2.26 & !is.null(decomp)) {
      if (decomp == 'QR') {
        if (add_funmats) {
          rcsfun <- paste0(getx_fun,
                           funmats,
                           rcsfun,
                           rcsfunmultadd,
                           spl_d0,
                           spl_d1,
                           spl_d2,
                           sep = "\n")
        } else if (!add_funmats) {
          rcsfun <- paste0(getx_fun,
                           rcsfun,
                           rcsfunmultadd,
                           spl_d0,
                           spl_d1,
                           spl_d2,
                           sep = "\n")
        }
      }
    }
    
    
    
  } # if(select_model != 'sitar') { # pb models
  
  
  
  
  
  #################
  extract_r_fun_from_scode <-
    function(xstaring, what = NULL, decomp, spfncname) {
      xstaring <- gsub("[[:space:]]" , "", xstaring)
      xstaring <- gsub(";" , ";\n", xstaring)
      xstaring <- gsub("\\{" , "{\n", xstaring)
      xstaring <- gsub("}" , "}\n", xstaring)
      xstaring <- gsub("vector[N]" , "", xstaring, fixed = T)
      xstaring <- gsub("vector" , "", xstaring, fixed = T)
      xstaring <- gsub("int" , "", xstaring, fixed = T)
      xstaring <- gsub("real" , "", xstaring, fixed = T)
      xstaring <- gsub(paste0("jp1;", "\n"), "", xstaring, fixed = T)
      xstaring <- gsub("rep_vector" , "rep", xstaring, fixed = T)
      xstaring <- gsub("rep_" , "rep", xstaring, fixed = T)
      xstaring <-
        gsub(
          "Xx[ia,ja]=(X[ia]-knots[ja]>0?X[ia]-knots[ja]:0);" ,
          "Xx[ia,ja]=ifelse(X[ia]-knots[ja]>0,X[ia]-knots[ja],0);",
          xstaring,
          fixed = T
        )
      xstaring <- gsub("num_elements" , "length", xstaring, fixed = T)
      xstaring <-
        gsub("matrix[N,nknots-1]Spl" ,
             "Spl=matrix(0,N,nknots-1)",
             xstaring,
             fixed = T)
      xstaring <-
        gsub("matrix[nknots-1,N]rcs" ,
             "rcs=matrix(0,nknots-1,N)",
             xstaring,
             fixed = T)
      xstaring <-
        gsub("matrix[N,nknots]Xx" ,
             "Xx=matrix(0,N, nknots)",
             xstaring,
             fixed = T)
      xstaring <-
        gsub("for(iain1:N)" , "for(ia in 1:N)", xstaring, fixed = T)
      xstaring <- gsub("for(jain1:nknots)" ,
                       "for(ja in 1:nknots)",
                       xstaring,
                       fixed = T)
      xstaring <- gsub(".*" , "*", xstaring, fixed = T)
      xstaring <- gsub("./" , "/", xstaring, fixed = T)
      funame__ <- strsplit(xstaring, "\\(")[[1]][1]
      xstaring <- gsub(funame__ , paste0(funame__, "<-function"),
                       xstaring, fixed = T)
      xstaring <- sub("//[^//]+$", "", xstaring)
      # To remove stanadlon ";
      xstaring <-
        gsub(paste0(";\n;\n", ""), ";\n", xstaring, fixed = T)
      xstaring <- gsub("[nknots]knots" , "knots", xstaring, fixed = T)
      
      if (!is.null(what)) {
        if (what == 'getX') {
          xstaring <- gsub(paste0("x;", "\n"), "", xstaring)
        }
        if (what == 'getKnots') {
          xstaring <- gsub("\\[", "c\\(", xstaring)
          xstaring <- gsub("\\]'", "\\)", xstaring)
        }
      }
      
      if (!is.null(decomp)) {
        if (decomp == 'QR') {
          xstaring <-
            gsub(paste0("matrix[N,QK]XQ;", "\n") , "", xstaring, fixed = T)
          xstaring <- gsub("matrix[N,QK]" , "", xstaring, fixed = T)
          xstaring <-
            gsub(paste0("matrix[QK,QK]", XR_inv_name, ";", "\n") ,
                 "",
                 xstaring,
                 fixed = T)
          xstaring <-
            gsub(paste0("matrix[QK,QK]XR;", "\n"), "", xstaring, fixed = T)
          xstaring <- gsub("qr_thin_Q" , "qr", xstaring, fixed = T)
          xstaring <- gsub("qr_thin_R" , "qr.R", xstaring, fixed = T)
          xstaring <-
            gsub(XR_inv_name,
                 "=inverse" ,
                 XR_inv_name,
                 "=chol2inv",
                 xstaring,
                 fixed = T)
        }
      }
      
      xstaring
    } # extract_r_fun_from_scode
  
  
  rcsfun_raw_str   <- extract_r_fun_from_scode(rcsfun_raw,
                                               what = NULL,
                                               decomp = decomp,
                                               spfncname = spfncname)
  spl_d0_str   <- extract_r_fun_from_scode(spl_d0,
                                           what = NULL,
                                           decomp = decomp,
                                           spfncname = spfncname)
  spl_d1_str   <- extract_r_fun_from_scode(spl_d1,
                                           what = NULL,
                                           decomp = decomp,
                                           spfncname = spfncname)
  spl_d2_str   <- extract_r_fun_from_scode(spl_d2,
                                           what = NULL,
                                           decomp = decomp,
                                           spfncname = spfncname)
  getX_str     <- extract_r_fun_from_scode(
    getx_fun_raw,
    what = 'getX',
    decomp = decomp,
    spfncname = spfncname
  )
  getknots_str <- NULL
  if (select_model == 'sitar' | select_model == 'rcs') {
    getknots_str <- extract_r_fun_from_scode(
      getknots_fun_raw,
      what = 'getKnots',
      decomp = decomp,
      spfncname = spfncname
    )
  }
  
  all_raw_str <- c(rcsfun_raw_str,
                   spl_d0_str,
                   spl_d1_str,
                   spl_d2_str,
                   getX_str,
                   getknots_str)
  
  
  if (!add_rcsfunmatqrinv_genquant) {
    out <- list(rcsfun = rcsfun, r_funs = all_raw_str)
  } else if (add_rcsfunmatqrinv_genquant) {
    out <- list(rcsfun = rcsfun,
                r_funs = all_raw_str,
                gq_funs = rcsfunmatqrinv_genquant)
  }
  
  out
}






