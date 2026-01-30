#' generalized additive model
#'
#' @description
#' generalized additive model that captures non-linear relationships between a response variable and predictors using smooth functions, while allowing inclusion of random effects or complex structures.

#' @param data data containing all necessary columns to run the model
#' @param resp_scale Character. Specifies how the response variable is treated in the model.
#'   \itemize{
#'     \item \strong{"resp_gaussian"}: Uses the response on its original scale, assuming a Gaussian distribution with an identity link (no transformation applied).
#'     \item \strong{"resp_log"}: Log-transforms the response before modelling. The transformed response is then assumed to follow a Gaussian distribution with an identity link.
#'     \item \strong{"resp_gamma"}: Keeps the response on its original scale, fitted under a Gamma distribution with a log link. Suitable for strictly positive and right-skewed data.
#'   }
#' @param m.candidates the list of candidate equations.
#'
#'
#' @return list including model, fitting statistics, ptable, stable and prediction table
#' @details
#' This function models the generalized additive model using mcgv::gam.
#'
#' If users specify multiple candidate models through the m.candidates argument, the function will fit each candidate model using the maximum likelihood (ML) method.
#' The corrected Akaike Information Criterion (AICc) will then be compared to determine the best-fitting model. Once the optimal model is identified,
#' it will be refitted using the restricted maximum likelihood (REML) method and output the results.
#'
#' If users specify only 1 candidate model through the m.candidates argument, the model is fitted with "REML" method.

#' @export gam_mod
#' @examples
#'
#' # loading processed data
#' dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))

#' # pre-data for model
#' dt.samples_long <- prepare_samples_clim(dt.samples_trt)
#' dt.samples_long$uid_site.fac <- as.factor(as.character(dt.samples_long$uid_site))
#' dt.m <- dt.samples_long
#'
#' # gam_mod
#' m.gam <-gam_mod(data = dt.m, resp_scale = "resp_log",
#'                                        m.candidates = c( "rw_mm ~ s(year, by = uid_site.fac)",
#'                                                          "rw_mm ~ year:uid_site.fac"))

gam_mod <- function(data, resp_scale = "", m.candidates){

  main_model(data , resp_scale , m.option = 0, m.candidates)

}



#' detrending model on tree-ring width series

#' @description
#' models the biological growth trends in individual tree-ring width series using mgcv::gamm
#'
#'
#' @param data data containing all necessary columns to run the model
#' @param resp_scale Character. Specifies how the response variable is treated in the model.
#'   \itemize{
#'     \item \strong{"resp_gaussian"}: Uses the response on its original scale, assuming a Gaussian distribution with an identity link (no transformation applied).
#'     \item \strong{"resp_log"}: Log-transforms the response before modelling. The transformed response is then assumed to follow a Gaussian distribution with an identity link.
#'     \item \strong{"resp_gamma"}: Keeps the response on its original scale, fitted under a Gamma distribution with a log link. Suitable for strictly positive and right-skewed data.
#'   }
#' @param m.candidates the list of candidate equations.
#'
#'
#'
#' @return list including model, fitting statistics, ptable, stable and prediction table
#' @details
#' This function models the biological growth trends in individual tree-ring width series using mcgv::gamm.
#' By integrating a first-order autoregressive (AR1) component, it accounts for temporal autocorrelation.
#' This method can provide  “normalized” residuals, which are adjusted to reflect deviations after considering the AR1 correlation structure.
#' 'Normalized' residuals are valuable for further analyses, such as investigating relationships with climatic variables.

#' If users specify multiple candidate models through the m.candidates argument, the function will fit each candidate model using the maximum likelihood (ML) method.
#' The corrected Akaike Information Criterion (AICc) will then be compared to determine the best-fitting model. Once the optimal model is identified,
#' it will be refitted using the restricted maximum likelihood (REML) method and output the results.
#'
#' If users specify only 1 candidate model through the m.candidates argument, the model is fitted with "REML" method.

#' @export gamm_radius
#' @examples
#'
#' # loading processed data
#' dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))

#' # pre-data for model
#' dt.samples_long <- prepare_samples_clim(dt.samples_trt)
#'
#' dt.m <- dt.samples_long[uid_site == 1][ageC >1]
#'
#' # gamm_radius model
#' m.radius <-gamm_radius(data = dt.m, resp_scale = "resp_log",
#'                                        m.candidates = c( "rw_mm ~ s(year)",
#'                                                          "rw_mm ~ s(year)"))
gamm_radius <- function(data, resp_scale = "resp_gamma", m.candidates){

  main_model(data , resp_scale , m.option = 1, m.candidates)

}




#' growth model at site-level
#' @description
#' models the growth trend or climate-growth relationship per site.
#'
#' @param data data containing all necessary columns to run the model
#' @param resp_scale Character. Specifies how the response variable is treated in the model.
#'   \itemize{
#'     \item \strong{"resp_gaussian"}: Uses the response on its original scale, assuming a Gaussian distribution with an identity link (no transformation applied).
#'     \item \strong{"resp_log"}: Log-transforms the response before modelling. The transformed response is then assumed to follow a Gaussian distribution with an identity link.
#'     \item \strong{"resp_gamma"}: Keeps the response on its original scale, fitted under a Gamma distribution with a log link. Suitable for strictly positive and right-skewed data.
#'   }
#' @param m.candidates the list of candidate equations.
#'
#'
#'
#'
#' @return list including model, fitting statistics, ptable, stable and prediction table
#' @details
#' This function accounts for within-site variability and temporal autocorrelation by including series identity as random effects
#' and a first-order autoregressive (AR1) correlation structures, respectively. using mgcv::gamm()
#'
#' If users specify multiple candidate models through the m.candidates argument, the function will fit each candidate model using the maximum likelihood (ML) method.
#' The corrected Akaike Information Criterion (AICc) will then be compared to determine the best-fitting model.
#' Once the optimal model is identified, it will be refitted using the restricted maximum likelihood (REML) method and output the results.
#'
#' If users specify only 1 candidate model through the m.candidates argument, the model is fitted with "REML" method.


#' @export gamm_site
#'
#' @examples
#' #' @examples
#' # loading processed data
#' dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))
#' # climate
#' dt.clim <- data.table::fread(system.file("extdata", "dt.clim.csv", package = "growthTrendR"))
#' # pre-data for model
#' dt.samples_clim <- prepare_samples_clim(dt.samples_trt, dt.clim)
#'
#' dt.m <- dt.samples_clim[uid_site == 1][ageC >1]
#'
#' # gamm_site model
#' m.site <-gamm_site(data = dt.m, resp_scale = "resp_log",
#'          m.candidates = c(
#'          "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + s(FFD)",
#'          "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + FFD",
#'          "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC)"))
#'
#'
#'
gamm_site <- function(data, resp_scale = "resp_gamma", m.candidates){

  data$uid_radius.fac <- as.factor(as.character(data$uid_radius))
  main_model(data , resp_scale , m.option = 2, m.candidates )
}



#' spatial growth model at regional-level (multiple sites)
#' @description
#' models the growth trend or climate-growth relationship at regional-level with multiple sites
#'
#' @param data data containing all necessary columns to run the model
#' @param resp_scale Character. Specifies how the response variable is treated in the model.
#'   \itemize{
#'     \item \strong{"resp_gaussian"}: Uses the response on its original scale, assuming a Gaussian distribution with an identity link (no transformation applied).
#'     \item \strong{"resp_log"}: Log-transforms the response before modelling. The transformed response is then assumed to follow a Gaussian distribution with an identity link.
#'     \item \strong{"resp_gamma"}: Keeps the response on its original scale, fitted under a Gamma distribution with a log link. Suitable for strictly positive and right-skewed data.
#'   }
#' @param m.candidates the list of candidate equations.
#'
#'
#'
#' @return list including model, fitting statistics, ptable, stable, prediction table and spatial effect(moranI)
#' @details
#' This function accounts for within-site variability and temporal autocorrelation by including series identity as random effects
#' and a first-order autoregressive (AR1) correlation structures, respectively. Among-site variability and spatial effects are captured by incorporating site identity as random effects.
#' The model is refitted automatically by introducing a smooth term for latitude and longitude using the thin plate ("tp") basis if significant spatial autocorrelation persists.
#' “Normalized” residuals are provided for future analysis.
#'
#' If users specify multiple candidate models through the m.candidates argument, the function will fit each candidate model using the maximum likelihood (ML) method.
#' The corrected Akaike Information Criterion (AICc) will then be compared to determine the best-fitting model.
#' Once the optimal model is identified, it will be refitted using the restricted maximum likelihood (REML) method and output the results.
#'
#' If users specify only 1 candidate model through the m.candidates argument, the model is fitted with "REML" method.

#' @export gamm_spatial
#' @examples
#' \donttest{
#' # loading processed data
#' dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))
#' # climate
#' dt.clim <- data.table::fread(system.file("extdata", "dt.clim.csv", package = "growthTrendR"))
#' # pre-data for model
#' dt.samples_clim <- prepare_samples_clim(dt.samples_trt, dt.clim)

#' dt.m <- dt.samples_clim[ageC >1]

#' # gamm_spatial model
#' m.sp <-gamm_spatial(data = dt.m, resp_scale = "resp_gamma",
#'        m.candidates = c(
#'        "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + s(FFD)",
#'        "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + FFD",
#'        "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC)"))
#'}
#'

gamm_spatial <- function(data, resp_scale = "resp_gamma", m.candidates){
  data$uid_radius.fac <- as.factor(as.character(data$uid_radius))
  data$uid_site.fac <- as.factor(as.character(data$uid_site))
  m.candidates <- paste0(m.candidates, " + s(uid_site.fac, bs = 're')")
  main_model(data , resp_scale , m.option = 3, m.candidates)

}


#' spatial growth model for large dataset or vast geographical coverage
#' @description
#' To address the computational limitations of GAMMs for large datasets, this function offers a hybrid solution combining the efficiency of the mgcv::bam() function
#'

#' @param data data containing all necessary columns to run the model
#' @param resp_scale Character. Specifies how the response variable is treated in the model.
#'   \itemize{
#'     \item \strong{"resp_gaussian"}: Uses the response on its original scale, assuming a Gaussian distribution with an identity link (no transformation applied).
#'     \item \strong{"resp_log"}: Log-transforms the response before modelling. The transformed response is then assumed to follow a Gaussian distribution with an identity link.
#'     \item \strong{"resp_gamma"}: Keeps the response on its original scale, fitted under a Gamma distribution with a log link. Suitable for strictly positive and right-skewed data.
#'   }
#' @param m.candidates the list of candidate equations.
#'
# #' @import furrr


# #' @import sp
# #' @import spdep
#'
#'
#' @return list including model, fitting statistics, ptable, stable and prediction table
#' @details
#' This function accounts for within-site variability and temporal autocorrelation by including series identity as random effects
#' and a first-order autoregressive (AR1) correlation structures, respectively. Among-site variability and spatial effects are captured by incorporating site identity as random effects.
#' The model is refitted automatically by introducing a smooth term for latitude and longitude using the thin plate ("tp") basis if significant spatial autocorrelation persists.
#' “Normalized” residuals are provided for future analysis.
#'
#' This function supports parallel computation for the large-scale, geographically distributed datasets.
#'
#' If users specify multiple candidate models through the m.candidates argument, the function will fit each candidate model using the maximum likelihood (ML) method.
#' The corrected Akaike Information Criterion (AICc) will then be compared to determine the best-fitting model.
#' Once the optimal model is identified, it will be refitted using the restricted maximum likelihood (REML) method and output the results.
#'
#' If users specify only 1 candidate model through the m.candidates argument, the model is fitted with "REML" method.

#' @export bam_spatial
#' @examples
#' \donttest{
#' # loading processed data
#' dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))
#' # climate
#' dt.clim <- data.table::fread(system.file("extdata", "dt.clim.csv", package = "growthTrendR"))
#' # pre-data for model
#' dt.samples_clim <- prepare_samples_clim(dt.samples_trt, dt.clim)

#' dt.m <- dt.samples_clim[ageC >1]

#' # bam_spatial model
#' m.sp <-bam_spatial(data = dt.m, resp_scale = "resp_log",
#'        m.candidates = c(
#'        "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + s(FFD)",
#'        "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + FFD",
#'        "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC)"))
#'}
#'
#'

bam_spatial <- function(data, resp_scale = "resp_gamma", m.candidates){
  data$uid_radius.fac <- as.factor(as.character(data$uid_radius))
  data$uid_site.fac <- as.factor(as.character(data$uid_site))
  m.candidates <- paste0(m.candidates, " + s(uid_site.fac, bs = 're')")
  main_model(data , resp_scale , m.option = 4, m.candidates)

}


#' @keywords internal
#' @noRd
# main function
main_model <- function(data, resp_scale = "resp_gaussian", m.option, m.candidates){

  check_optional_deps()

  if (length(m.candidates) == 0) stop("must assign the equation(s) to m.candidates")
  if ( !(resp_scale %in% c("resp_log", "resp_gamma", "resp_gaussian"))) stop(paste0( "please check resp_scale, it allows 3 options: resp_log, on log-scale of resp; resp_gaussian, on response scale assuming gaussian distribution; resp_gamma, on response scale with family Gamma(log)"))


  if (resp_scale == "resp_log") famil = gaussian("identity")
  if (resp_scale == "resp_gaussian") famil = gaussian("identity")
  if (resp_scale == "resp_gamma") famil = Gamma("log")

  # setorder(data, uid_site, uid_radius, ageC)

  # for comparing and selecting model on AIC
  if (length(m.candidates) > 1){
    aic.all <- data.table()
    for (i in 1:length(m.candidates)){
      formul <- as.formula(m.candidates[i])
      # in log-scale, log-transfrom response variable
      if (resp_scale == "resp_log") formul <- update(formul, log(.) ~ .)

      if (m.option == 0){

        # m.tmp <- gam(formul,
        #               method = "ML",family = famil, data = data)

        m.tmp <- tryCatch({

          gam(formul, method = "ML",family = famil, data = data)

        }, warning = function(w) {
          # warning_message.c <<- conditionMessage(w)  # Store the warning message
          message("Warning captured : ", conditionMessage(w))
          return(NULL)  # Return NULL if a warning occurs
        }, error = function(e) {
          # error_message.c <<- conditionMessage(e)  # Store error message
          message("error captured : ", conditionMessage(e))
          return(NULL)  # Return NULL to prevent stopping execution
        })

      }
      if (m.option == 1){


        m.tmp <- tryCatch({

          gamm(formul,
               correlation = corCAR1(value = 0.5),
               method = "ML",family = famil, data = data)

        }, warning = function(w) {
          # warning_message.c <<- conditionMessage(w)  # Store the warning message
          message("Warning captured : ", conditionMessage(w))
          return(NULL)  # Return NULL if a warning occurs
        }, error = function(e) {
          # error_message.c <<- conditionMessage(e)  # Store error message
          message("error captured : ", conditionMessage(e))
          return(NULL)  # Return NULL to prevent stopping execution
        })

      }
      if (m.option %in% c(2,3)){
        # m.tmp <- gamm(formul,
        #               random = list(uid_radius.fac=~1), correlation = corCAR1(value = 0.5),
        #               method = "ML",family = famil, data = data)

        m.tmp <- tryCatch({

          gamm(formul,
               random = list(uid_radius.fac=~1), correlation = corCAR1(value = 0.5),
               method = "ML",family = famil, data = data)

        }, warning = function(w) {
          # warning_message.c <<- conditionMessage(w)  # Store the warning message
          message("Warning captured : ", conditionMessage(w))
          return(NULL)  # Return NULL if a warning occurs
        }, error = function(e) {
          # error_message.c <<- conditionMessage(e)  # Store error message
          message("error captured : ", conditionMessage(e))
          return(NULL)  # Return NULL to prevent stopping execution
        })

      }

      if (m.option == 4)  {
               # Detect available cores for parallel processing
        available_cores <- parallel::detectCores(logical = FALSE) - 1  # Adjusted cores based on system

        # Decide if parallel processing is supported
        if (available_cores > 1) {
          future::plan(future::multisession, workers = available_cores)
        } else {
          future::plan(future::sequential)
        }

        # m0.tmp <- bam(formul,
        #
        #               method = "ML",data = data)
        # setorder(data, uid_site, uid_radius, ageC)
        # data[, start.event := c(TRUE, rep(FALSE, .N - 1)), by = .(uid_site, uid_radius)]
        #

        m0.tmp <- tryCatch({

          bam(formul,

              method = "ML",data = data)

        }, warning = function(w) {
          # warning_message.c <<- conditionMessage(w)  # Store the warning message
          message("Warning captured : ", conditionMessage(w))
          return(NULL)  # Return NULL if a warning occurs
        }, error = function(e) {
          # error_message.c <<- conditionMessage(e)  # Store error message
          message("error captured : ", conditionMessage(e))
          return(NULL)  # Return NULL to prevent stopping execution
        })

        # r1 <- start_value_rho(m0, plot=TRUE)
        # print (paste0(Sys.time(), "          ar1"))
        # m.tmp <- bam(formul, data=data, rho=start_value_rho(m0.tmp, plot=FALSE), AR.start=data$start.event, method = "ML")
        setorder(data, uid_site, uid_radius, ageC)
        rho.start <- acf(resid(m0.tmp), plot = FALSE)$acf[2]
        rho.start <- max(min(rho.start, 0.9), -0.9)  # safety clamp

        # data[, start.event := c(TRUE, rep(FALSE, .N - 1)), by = .(uid_site, uid_radius)]
        data$start.event <- ave(
          seq_len(nrow(data)),
          data$uid_site,
          data$uid_radius,
          FUN = function(x) seq_along(x) == 1L
        )

        m.tmp <- tryCatch({

          bam(formul, data=data, rho=rho.start, AR.start=data$start.event, method = "ML")


        }, warning = function(w) {
          # warning_message.c <<- conditionMessage(w)  # Store the warning message
          message("Warning captured : ", conditionMessage(w))
          return(NULL)  # Return NULL if a warning occurs
        }, error = function(e) {
          # error_message.c <<- conditionMessage(e)  # Store error message
          message("error captured : ", conditionMessage(e))
          return(NULL)  # Return NULL to prevent stopping execution
        })

        rm(m0.tmp)
        # Reset to sequential
        future::plan(future::sequential)

      }

      if (is.null(m.tmp)) {
        next
      }
        aic.tmp <- data.table(i = i, form = gsub("\\\\", "", paste(deparse(formul), collapse = " ")), aic =  AIC(m.tmp), aicc = MuMIn::AICc(m.tmp), bic =  BIC(m.tmp), methd = "ML")
        if (m.option %in% c(1,2,3)) aic.tmp <- data.table(aic.tmp, R2 = summary(m.tmp$gam)$r.sq) else aic.tmp <- data.table(aic.tmp, R2 = summary(m.tmp)$r.sq)

        aic.all <- rbind(aic.all, aic.tmp)
    # }

      # if (i == 1) {
      #   # aicc.mn <- aic.tmp$aicc
      #   # m.sel <- m.tmp
      #   # i.sel <- i
      #   aic.all <- aic.tmp
      # } else{
      #   aic.all <- rbind(aic.all, aic.tmp)
      #   # if (aicc.mn > aic.tmp$aicc){
      #   #   aic.mn <- aic.tmp$aicc
      #   #   m.sel <- m.tmp
      #   #   i.sel <- i
      #   # }
      # }
      rm(aic.tmp, m.tmp)
    }
    if (nrow(aic.all) > 0) {
      aic.all[aicc == min(aicc), selected := "*"]
      form.sel <- as.formula(aic.all[selected == "*"]$form)
    }else form.sel <- character(0)
    # rm(m.sel)



  }else{
    # for 1 equation only
    form.sel <- as.formula(m.candidates)
    if (resp_scale == "resp_log") form.sel <- update(form.sel, log(.) ~ .)
  }

  # fitting REML for prediction
  if (length(form.sel) == 0) return(list(
  status  = "fail",
  model   = NULL,
  fitting = NA_real_,
  ptable  = NULL,
  stable  = NULL,
  pred    = NULL,
  error   = "Model did not converge"
))

  if (m.option == 0) {
    # m.sel <- gam(form.sel,
    #             method = "REML",family = famil, data = data)

    m.sel <- tryCatch({

      gam(form.sel,
          method = "REML",family = famil, data = data)

    }, warning = function(w) {
      # warning_message.c <<- conditionMessage(w)  # Store the warning message
      message("Warning captured : ", conditionMessage(w))
      return(NULL)  # Return NULL if a warning occurs
    }, error = function(e) {
      # error_message.c <<- conditionMessage(e)  # Store error message
      message("error captured : ", conditionMessage(e))
      return(NULL)  # Return NULL to prevent stopping execution
    })

  }

  if (m.option == 1) {
    # m.sel <- gamm(form.sel,
    #               correlation = corCAR1(value = 0.5),
    #               method = "REML",family = famil, data = data)

    m.sel <- tryCatch({

      gamm(form.sel,
           correlation = corCAR1(value = 0.5),
           method = "REML",family = famil, data = data)

    }, warning = function(w) {
      # warning_message.c <<- conditionMessage(w)  # Store the warning message
      message("Warning captured : ", conditionMessage(w))
      return(NULL)  # Return NULL if a warning occurs
    }, error = function(e) {
      # error_message.c <<- conditionMessage(e)  # Store error message
      message("error captured : ", conditionMessage(e))
      return(NULL)  # Return NULL to prevent stopping execution
    })


  }
   if (m.option %in% c(2,3)) {
     # m.sel <- gamm(form.sel,
     #              random = list(uid_radius.fac=~1), correlation = corCAR1(value = 0.5),
     #              method = "REML",family = famil, data = data)
     #

     m.sel <- tryCatch({

       gamm(form.sel,
            random = list(uid_radius.fac=~1), correlation = corCAR1(value = 0.5),
            method = "REML",family = famil, data = data)

     }, warning = function(w) {
       # warning_message.c <<- conditionMessage(w)  # Store the warning message
       message("Warning captured : ", conditionMessage(w))
       return(NULL)  # Return NULL if a warning occurs
     }, error = function(e) {
       # error_message.c <<- conditionMessage(e)  # Store error message
       message("error captured : ", conditionMessage(e))
       return(NULL)  # Return NULL to prevent stopping execution
     })




    data[, res.normalized:=residuals(m.sel$lme, type = "normalized")]
    # Extract the substring inside parentheses in case in log-scale
    y.char <- sub(".*\\((.*?)\\).*", "\\1", all.vars(m.sel$gam$formula)[1])

    }
  if (m.option == 4){


      # Detect available cores for parallel processing
      available_cores <- parallel::detectCores(logical = FALSE) - 1  # Adjusted cores based on system

      # Decide if parallel processing is supported
      if (available_cores > 1) {
        future::plan(future::multisession, workers = available_cores)
      } else {
        future::plan(future::sequential)
      }

      # m0.sel <- bam(form.sel,
      #
      #               method = "fREML",data = data)


      m0.sel <- tryCatch({

        bam(form.sel,

            method = "fREML",data = data)

      }, warning = function(w) {
        # warning_message.c <<- conditionMessage(w)  # Store the warning message
        message("Warning captured : ", conditionMessage(w))
        return(NULL)  # Return NULL if a warning occurs
      }, error = function(e) {
        # error_message.c <<- conditionMessage(e)  # Store error message
        message("error captured : ", conditionMessage(e))
        return(NULL)  # Return NULL to prevent stopping execution
      })



      setorder(data, uid_site, uid_radius, ageC)
      # data[, start.event := c(TRUE, rep(FALSE, .N - 1)), by = .(uid_site, uid_radius)]
      # avoid VECTOR_ELT (on macos)
      data$start.event <- ave(
        seq_len(nrow(data)),
        data$uid_site,
        data$uid_radius,
        FUN = function(x) seq_along(x) == 1L
      )

      # Step 3: Add rho and AR1 structure
      # rho.start <- start_value_rho(m0.sel, plot = FALSE)
      rho.start <- acf(resid(m0.sel), plot = FALSE)$acf[2]
      rho.start <- max(min(rho.start, 0.9), -0.9)  # safety clamp

      # m.sel <- bam(
      #   form.sel,
      #   data = data,
      #   rho = rho.start,
      #   AR.start = data$start.event,
      #   method = "fREML"
      # )

      m.sel <- tryCatch({

        bam(
          form.sel,
          data = data,
          rho = rho.start,
          AR.start = data$start.event,
          method = "fREML"
        )

      }, warning = function(w) {
        # warning_message.c <<- conditionMessage(w)  # Store the warning message
        message("Warning captured : ", conditionMessage(w))
        return(NULL)  # Return NULL if a warning occurs
      }, error = function(e) {
        # error_message.c <<- conditionMessage(e)  # Store error message
        message("error captured : ", conditionMessage(e))
        return(NULL)  # Return NULL to prevent stopping execution
      })


      data[, res.normalized:=residuals(m.sel, type = "scaled.pearson")]
      # Extract the substring inside parentheses in case in log-scale
      y.char <- sub(".*\\((.*?)\\).*", "\\1", all.vars(m.sel$formula)[1])

      # Reset to sequential
      future::plan(future::sequential)


  }

    # further for spatial effect
    if (m.option >= 3 & all(c("latitude", "longitude") %in% names(data))){

      data[, c("lon_use", "lat_use") := list(round(longitude,1), round(latitude,1) )]

      y.site <- data[!is.na(res.normalized), .(obs.med = median(get(y.char)), res.med = median(res.normalized)), by = .(lon_use, lat_use )]
      if (nrow(y.site) > 5) {
      sp::coordinates(y.site) <- ~ lon_use+lat_use
      knea <- spdep::knearneigh(sp::coordinates(y.site), longlat = TRUE)
      nb <- spdep::knn2nb(knea)
      lw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
      moran.I.o <- spdep::moran.test(y.site$obs.med,lw)
      moran.I.o <- data.frame(statistic = moran.I.o$estimate[1], expected = moran.I.o$estimate[2], Variance = moran.I.o$estimate[3],
                              p.value = moran.I.o$p.value)
      names(moran.I.o) <- paste0(names(moran.I.o), ".obs")

      moran.I.r <- spdep::moran.test(y.site$res.med,lw)
      moran.I.r <- data.frame(statistic = moran.I.r$estimate[1], expected = moran.I.r$estimate[2], Variance = moran.I.r$estimate[3],
                              p.value = moran.I.r$p.value)
      names(moran.I.r) <- paste0(names(moran.I.r), ".res")
      moranI <- cbind(moran.I.o, moran.I.r)
      rm(nb, knea, moran.I.o, moran.I.r)
      if (moranI$p.value.obs < 0.1 & moranI$p.value.res < 0.1 ) {
      if (m.option == 3)  {
        form.sel <- update(m.sel$gam$formula, . ~ . + s(latitude, longitude, bs = "tp"))
        m.sel <- gamm(form.sel,  data = data,
                      random = list(uid_radius.fac= ~1),correlation =  corCAR1(value = 0.5),
                      family = famil)
        data[, res.normalized.LL:=residuals(m.sel$lme, type = "normalized")]
      }
        if (m.option == 4)  {
          form.sel <- update(m.sel$formula, . ~ . + s(latitude, longitude, bs = "tp"))
          # Detect available cores for parallel processing
          available_cores <- parallel::detectCores(logical = FALSE) - 1  # Adjusted cores based on system

          # Decide if parallel processing is supported
          if (available_cores > 1) {
            future::plan(future::multisession, workers = available_cores)
          } else {
            future::plan(future::sequential)
          }

          m0.sel <- bam(form.sel,

                        method = "fREML",data = data)
          # setorder(data, uid_site, uid_radius, ageC)
          # data[, idrow:=seq_len(.N), by = .(uid_site, uid_radius)][,start.event := (idrow== 1)][, idrow:= NULL]
          # r1 <- start_value_rho(m0, plot=TRUE)
          # print (paste0(Sys.time(), "          ar1"))

          rho.start <- acf(resid(m0.sel), plot = FALSE)$acf[2]
          rho.start <- max(min(rho.start, 0.9), -0.9)  # safety clamp

          m.sel <- bam(form.sel, data=data, rho=rho.start, AR.start=data$start.event)

          data[, res.normalized.LL:=residuals(m.sel, type = "scaled.pearson")]

          # Reset to sequential
          future::plan(future::sequential)

        }

        y.site <- data[!is.na(res.normalized.LL), .(res.med.LL = median(res.normalized.LL)), by = .(lon_use, lat_use )]

        moran.I.r_LL <- spdep::moran.test(y.site$res.med.LL,lw)
        moran.I.r_LL <- data.frame(statistic = moran.I.r_LL$estimate[1], expected = moran.I.r_LL$estimate[2], Variance = moran.I.r_LL$estimate[3],
                                p.value = moran.I.r_LL$p.value)
        names(moran.I.r_LL) <- paste0(names(moran.I.r_LL), ".res_LL")
        moranI <- cbind(moranI, moran.I.r_LL)

      }
    }
    } # m.option >= 3, for testing spatial effect, and adding s(lat, lon) if necessary
  # }



  if (m.option < 4){
    if (m.option == 0) msel.gam <- m.sel else msel.gam <- m.sel$gam
  pred.terms  <-as.data.frame( predict(msel.gam, type="terms",se.fit=TRUE))
  rhs_terms <- attr(terms(msel.gam$formula), "term.labels")
  # to set column names for only 1 term,
  if (length(rhs_terms) == 1){
  if (length(names(pred.terms)) == 2) # only for 1 term with fit and se.fit
  names(pred.terms) <-paste0(c("fit.", "se.fit."), sub("s\\(([^,\\)]+).*", "\\1", rhs_terms))
  }

  pred.terms <- format_byterm(msel.gam, pred.terms)

  fit.y <- as.data.frame(predict(msel.gam, type = "response", se.fit = TRUE))
  names(fit.y) <- c("fit.resp", "se.fit.resp")
  tmp.y <- data.table(data, pred.terms, fit.y)
  tmp.y$res.resp <- residuals(msel.gam, type = "response")
  if (m.option > 0) tmp.y$res.resp_normalized <- residuals(m.sel$lme, type = "normalized")
  if ("res.normalized" %in% names(tmp.y)) tmp.y$res.normalized <- NULL
  # if (resp_scale == "resp_log") setnames(tmp.y, c("fit.resp", "se.fit.resp", "res.resp", "res.resp_normalized"),
  #                                   c("fit.log_resp", "se.fit.log_resp", "res.log_resp", "res.log_resp_normalized"))


  ptable <- data.table(Parameter = row.names(summary(msel.gam)$p.table), summary(msel.gam)$p.table )
  stable <- data.table(Parameter = row.names(summary(msel.gam)$s.table), summary(msel.gam)$s.table)
  aic.reml <- data.table(form = gsub("\\\\", "", paste(deparse(form.sel), collapse = " ")), aic =  AIC(m.sel), aicc = MuMIn::AICc(m.sel), bic =  BIC(m.sel), R2 = summary(msel.gam)$r.sq,  methd = "REML")


  }else{
    pred.terms  <-as.data.frame( predict(m.sel, type="terms",se.fit=TRUE))
    fit.y <- as.data.frame(predict(m.sel, type = "response", se.fit = TRUE))
    names(fit.y) <- c("fit.resp", "se.fit.resp")
    tmp.y <- data.table(data, pred.terms, fit.y)
    tmp.y$res.resp <- residuals(m.sel, type = "response")
    tmp.y[,res.resp_normalized := residuals(m.sel, type = "scaled.pearson")]




    ptable <- data.table(Parameter = row.names(summary(m.sel)$p.table), summary(m.sel)$p.table )
    stable <- data.table(Parameter = row.names(summary(m.sel)$s.table), summary(m.sel)$s.table)
    aic.reml <- data.table(form = gsub("\\\\", "", paste(deparse(form.sel), collapse = " ")), aic =  AIC(m.sel), aicc = MuMIn::AICc(m.sel), bic = BIC(m.sel), R2 = summary(m.sel)$r.sq, methd = "fREML")

  }
  # rename log-scale
  if (resp_scale == "resp_log") {
    setnames(tmp.y, c("fit.resp", "se.fit.resp", "res.resp"),
                                    c("fit.log_resp", "se.fit.log_resp", "res.log_resp"))
  if ("res.resp_normalized" %in% names(tmp.y)) setnames(tmp.y, "res.resp_normalized", "res.log_resp_normalized" )
    }
  # if (!is.null(out.csv)){
  #
  #   if (!(dir.exists(out.csv))) dir.create(out.csv, recursive = TRUE)
  #   utils::write.csv(ptable, file =  file.path(out.csv, paste0("ptable ", "REML" ,".csv")), row.names = FALSE, na = "")
  #   utils::write.csv(stable, file =  file.path(out.csv, paste0("stable ", "REML" ,".csv")), row.names = FALSE, na = "")
  #   utils::write.csv(tmp.y, file = file.path(out.csv, paste0("prediction ", "REML" ,".csv")), row.names = FALSE, na = "")
  #   utils::write.csv(aic.reml, file = file.path(out.csv, paste0("fitting ", "REML" ,".csv")), row.names = FALSE, na = "")
  # }
  m.sel$m.candidates <- m.candidates
  m.sel$resp_scale <- resp_scale
  m.sel$is_log_model <- resp_scale %in% c("resp_gamma", "resp_log")
  if (m.option %in% c(1,2,3)){
    m.sel$gam$resp_scale <- resp_scale
    m.sel$gam$is_log_model <- resp_scale %in% c("resp_gamma", "resp_log")
    }
  aic.reml$resp_scale <- resp_scale
  return.lst <- list(status  = "ok",model = m.sel, fitting = aic.reml, ptable = ptable, stable = stable, pred = tmp.y,
                     error   = NULL)
  if (m.option >= 3 & exists("moranI")) return.lst$moranI <- moranI
  if (length(m.candidates) > 1)  return.lst$fitting_ML <- aic.all
  class(return.lst) <- "cfs_model"
  return(return.lst)
}




#' format wide to long

#' @description
#' convert the format of term prediction with by from wide to long in model$pred
#'
#'
#' @param model a gam model
#' @param dt.pred prediction table from gam model
#'
#'
#' @return dt.pred, the term prediction by gam were formed as 1 term, for both fits and stand error
#'

#' @keywords internal
#' @noRd


format_byterm <- function(model, dt.pred){
  if (is.null(model) | is.null(dt.pred)) stop("please refit the model with package growthTrendR...")

  setDT(dt.pred)
  rhs_terms <-attr(terms(model$formula), "term.labels")

  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # Keep only terms with 's(' and 'by ='
  smooth_with_by <- rhs_terms[str_detect(rhs_terms, "s\\([^\\)]+by\\s*=")]

  # Extract the variable and the 'by' category
  matched <- str_match(smooth_with_by, "s\\(([^,]+),[^)]*?by\\s*=\\s*([^,\\)\\s]+)")
  # matched <-setDT (as.data.frame(matched))
  # # Lists:
  # x.lst <- matched[, 2]  # first argument to s()
  # by.lst <- matched[, 3]    # value after 'by ='
  dt.pred$byterm <- NA_character_

  # not necessary as only by term appeared in matched
  # ix <- 1
  # while (ix <= nrow(matched)) {
  #
  #
  #   fit.s.clim <- paste0("fit.s.", matched[, 2][ix], "..", matched[, 3][ix])
  #   if (any(str_detect(names(dt.pred), fit.s.clim)) == TRUE) ix <- ix + 1 else matched <- matched[-ix,]
  #
  # }


  # se extraction from gratia is on the term only, so slightly smaller than mgcv "term" use mgcv to keep consistence.
  if (nrow(matched) > 0){

    for (ix in 1: nrow(matched)){

      fit.s.clim <- paste0("fit.s.", matched[, 2][ix], "..", matched[, 3][ix])

      for (se in c("", "se.")){

        cols_to_sum <- grep(paste0("^", se, fit.s.clim), names(dt.pred), value = TRUE)

        dt.pred$new_column <- rowSums(dt.pred[, cols_to_sum, with = FALSE])

        setnames(dt.pred, "new_column", paste0( se, strsplit(fit.s.clim, paste0(".", matched[, 3][ix]) )))
        dt.pred <- dt.pred[, !names(dt.pred) %in% cols_to_sum, with = FALSE]

      }
      dt.pred$byterm <- paste(paste0(matched[, 2], "-", matched[, 3]), collapse = ", ")

    }
    }

  return(dt.pred)
}




#' variable importance of smooth terms in a GAM model

#' @description
#' Evaluates the relative influence of each smooth term in a GAM model by computing
#' its contribution to the fitted values using the linear predictor matrix
#' (\code{type = "lpmatrix"}). Three summary methods are available: sum of squares,
#' variance, and mean absolute value across all observations.
#' #'
#'
#' @param gam_model A GAM model object.
#' @param method A character string specifying the method to compute importance.
#'        One of \code{"ssq"}, \code{"var"}, or \code{"meanabs"}.
#'
#' @return A \code{data.table} with columns:
#' \describe{
#'   \item{var}{Name of the smooth term.}
#'   \item{importance_pct}{Relative importance as a percentage.}
#'   \item{method}{The method used for calculating the importance.}
#' }
#'
#' @export sterm_imp
#' @examples
#' # loading processed data
#' dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))
#' # climate
#' dt.clim <- data.table::fread(system.file("extdata", "dt.clim.csv", package = "growthTrendR"))
#' # pre-data for model
#' dt.samples_clim <- prepare_samples_clim(dt.samples_trt, dt.clim)

#' dt.m <- dt.samples_clim[ageC >1]

#' # using gamm_spatial model as an example
#' m.sp <-gamm_spatial(data = dt.m, resp_scale = "resp_log",
#'        m.candidates = "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + s(FFD)")
#'
#' dt.m[, uid_site.fac:= as.factor(as.character(uid_site))]

#' dt.imp <- sterm_imp(m.sp$model$gam)
#'

sterm_imp <- function(gam_model, method = c("ssq", "var", "meanabs")) {
  method <- match.arg(method)
  data <- gam_model$model

  # Ensure factor levels match
  for (v in names(data)) {
    if (is.factor(data[[v]])) {
      data[[v]] <- factor(data[[v]], levels = levels(gam_model$model[[v]]))
    }
  }

  # Predict lpmatrix and extract coefficients
  X <- predict(gam_model, newdata = data, type = "lpmatrix")
  beta <- coef(gam_model)

  # Map each smooth term to its coefficient indices
  term_map <- setNames(
    lapply(gam_model$smooth, function(sm) sm$first.para:sm$last.para),
    vapply(gam_model$smooth, `[[`, "", "label")
  )

  # Compute importance measure
  term_contrib <- sapply(term_map, function(cols) {
    values <- X[, cols, drop = FALSE] %*% beta[cols]
    switch(method,
           ssq     = sum(values^2),
           var     = var(as.vector(values)),
           meanabs = mean(abs(values)))
  })

  # Normalize and format output
  importance <- term_contrib / sum(term_contrib)
  data.table(
    term = names(importance),
    importance_pct = round(importance * 100, 1),
    method = method
  )[order(-importance_pct)]
}


#' Compute prediction and confidence intervals of smooth terms on response scale
#'
#' This function computes predicted values and confidence intervals from a fitted GAM model
#' with a log-link (or other link) and optionally back-transforms predictions to the response scale.
#' Five methods are supported:
#' 1. **delta_link**: classic delta method on the linear predictor (link) scale; back-transformed CI is asymmetric.
#' 2. **delta_resp**: delta method applied directly on the response scale using
#'  \eqn{\mathrm{Var}[\exp(\eta)] \approx \exp(2\eta)\,\mathrm{Var}(\eta)}.
#' 3. **bootstrap_link**: parametric bootstrap on the linear predictor; quantiles back-transformed.
#' 4. **bootstrap_resp**: parametric bootstrap on the response scale; quantiles computed after exponentiating.
#' 5. **posterior**: Bayesian posterior simulation using the model covariance matrix; quantiles on response scale.
#'
#' References:
#' - Wood, S.N. (2017) *Generalized Additive Models: An Introduction with R, 2nd Edition*. CRC Press.
#' - Efron, B., & Tibshirani, R. (1993) *An Introduction to the Bootstrap*. Chapman & Hall.
#' - Gelman, A., et al. (2013) *Bayesian Data Analysis, 3rd Edition*. CRC Press.
#'
#' @param model A fitted GAM object from \code{mgcv::gam}.
#' @param newdata A data.frame containing values at which to predict.
#' @param nboot Integer. Number of bootstrap or posterior samples. Default 1000.
#' @param method Character. One of \code{"delta_link"}, \code{"delta_resp"}, \code{"bootstrap_link"}, \code{"bootstrap_resp"}, \code{"posterior"}.
#' @param level Numeric. Confidence level, default 0.95.
#'
#' @return A \code{data.table} with columns: \code{fit}, \code{lwr}, \code{upr}.
#'

#' @export

#' @examples
#' # loading processed data
#' dt.samples_trt <- readRDS(system.file("extdata", "dt.samples_trt.rds", package = "growthTrendR"))
#' # climate
#' dt.clim <- data.table::fread(system.file("extdata", "dt.clim.csv", package = "growthTrendR"))
#' # pre-data for model
#' dt.samples_clim <- prepare_samples_clim(dt.samples_trt, dt.clim)

#' dt.m <- dt.samples_clim[ageC >1]

#' # using gamm_spatial model as an example
#' m.sp <-gamm_spatial(data = dt.m, resp_scale = "resp_log",
#'        m.candidates = "bai_cm2 ~ log(ba_cm2_t_1) + s(ageC) + s(FFD)")
#'
#' dt.m[, uid_site.fac:= as.factor(as.character(uid_site))]

#' dt.ci <- ci_resp(m.sp$model$gam, newdata = dt.m)
#'
ci_resp <- function(model, newdata, nboot = 100,
                    method = c("posterior", "delta_link", "delta_resp", "bootstrap_link", "bootstrap_resp"),
                    level = 0.95) {

  check_optional_deps()
  # Adaptive method selection for small samples
  if (nrow(newdata) < 30) {
    message("Small sample detected (n < ", "30",
            "). Switching CI method to 'posterior' for robustness.")
    method <- "posterior"
  } else {
    method <- match.arg(method)
  }

  alpha <- (1 - level)/2
  X <- predict(model, newdata = newdata, type = "lpmatrix")
  coefs <- coef(model)
  Vb <- vcov(model)
  eta_hat <- drop(X %*% coefs)
  fit_resp <- exp(eta_hat)
  out <- data.table::copy(newdata)
  setDT(out)
  is_log_model <- model$resp_scale %in% c("resp_gamma", "resp_log")
  if (length(model$resp_scale) == 0) stop("cannot pass resp_scale")
  if (length(model$is_log_model) == 0) stop("cannot pass is_log_model")

  # if (model$is_log_model == FALSE){
  if (is_log_model == FALSE){
    pred <- predict(model, newdata, type = "link", se.fit = TRUE)
    q_low  <- pred$fit - qnorm(1 - alpha) * pred$se.fit
    q_high <- pred$fit + qnorm(1 - alpha) * pred$se.fit
    out [, c("fit", "lwr", "upr"):= .(pred$fit, q_low, q_high)]
  }else{
  if (method == "delta_link") {
    pred <- predict(model, newdata, type = "link", se.fit = TRUE)
    q_low  <- pred$fit - qnorm(1 - alpha) * pred$se.fit
    q_high <- pred$fit + qnorm(1 - alpha) * pred$se.fit
    out [, c("fit", "lwr", "upr"):= .(exp(pred$fit), exp(q_low), exp(q_high) )]



  } else if (method == "delta_resp") {
    pred <- predict(model, newdata, type = "link", se.fit = TRUE)
    var_resp <- (exp(pred$fit))^2 * (pred$se.fit)^2
    se_resp <- sqrt(var_resp)
    out [, c("fit", "lwr", "upr"):= .(exp(pred$fit), fit_resp - qnorm(1 - alpha) * se_resp, fit_resp + qnorm(1 - alpha) * se_resp )]

  } else if (method %in% c("bootstrap_link", "bootstrap_resp")) {
    beta_star <- MASS::mvrnorm(nboot, coefs, Vb)
    eta_star <- X %*% t(beta_star)

    if (method == "bootstrap_link") {
      q_low  <- apply(eta_star, 1, quantile, probs = alpha)
      q_high <- apply(eta_star, 1, quantile, probs = 1 - alpha)
      out [, c("fit", "lwr", "upr"):= .(fit_resp, exp(q_low), exp(q_high) )]

    } else {
      y_star <- exp(eta_star)
      q_low  <- apply(y_star, 1, quantile, probs = alpha)
      q_high <- apply(y_star, 1, quantile, probs = 1 - alpha)
      out <- data.table::data.table(
        fit = fit_resp,
        lwr = q_low,
        upr = q_high
      )
      out [, c("fit", "lwr", "upr"):= .(fit_resp, q_low, q_high )]
    }

  } else if (method == "posterior") {
    beta_star <- mgcv::rmvn(nboot, coefs, Vb)
    eta_star <- X %*% t(beta_star)
    y_star <- exp(eta_star)
    q_low  <- apply(y_star, 1, quantile, probs = alpha)
    q_high <- apply(y_star, 1, quantile, probs = 1 - alpha)
    out [, c("fit", "lwr", "upr"):= .(fit_resp, q_low, q_high )]
  }
    }
  out$ci_method <- method
  return(out)
}
