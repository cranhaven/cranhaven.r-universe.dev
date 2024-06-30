#' @useDynLib mxsem, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#' mxsem
#'
#' Create an extended SEM with **OpenMx** (Boker et al., 2011) using a
#' **lavaan**-style (Rosseel, 2012) syntax.
#'
#' Setting up SEM can be tedious. The **lavaan** (Rosseel, 2012) package provides a great syntax to
#' make the process easier. The objective of **mxsem** is to provide a similar syntax
#' for **OpenMx**. **OpenMx** is a flexible R package for extended SEM. However, note that
#' **mxsem** only covers a small part of the **OpenMx** framework by focusing on "standard"
#' SEM. Similar to **lavaan**'s `sem()`-function, `mxsem` tries to set up parts
#' of the model automatically (e.g., adding variances automatically or scaling the
#' latent variables automatically). If you want to unlock
#' the full potential of **OpenMx**, **mxsem** may not be the best option.
#'
#' **Warning**: The syntax and settings of **mxsem** may differ from
#' **lavaan** in some cases. See `vignette("Syntax", package = "mxsem")` for more details
#' on the syntax and the default arguments.
#'
#' ## Alternatives
#'
#' You will find similar functions in the following packages:
#'
#' - [**metaSEM**](https://github.com/mikewlcheung/metasem) (Cheung, 2015) provides a `lavaan2RAM`
#' function that can be combined with the `create.mxModel` function. This combination
#' offers more features than **mxsem**. For instance, constraints of the form `a < b`
#' are supported. In **mxsem** such constraints require algebras (e.g., `!diff; a := b - exp(diff)`).
#' - [**umx**](https://github.com/tbates/umx) (Bates et al., 2019)
#' provides the `umxRAM` and `umxLav2RAM` functions that can parse single **lavaan**-style
#' statements (e.g., `eta =~ y1 + y2 + y3`)
#' or an entire **lavaan** models to **OpenMx** models.
#' - [**tidySEM**](https://github.com/cjvanlissa/tidySEM) (van Lissa, 2023) provides the
#' `as_ram` function to translate **lavaan** syntax to **OpenMx** and also implements a unified syntax to
#' specify both, **lavaan** and **OpenMx** models. Additionally, it works well with the
#' **tidyverse**.
#' - [**ezMx**](https://github.com/OpenMx/ezMx) (Bates, et al. 2014) simplifies fitting SEM with **OpenMx**
#'   and also provides a translation of **lavaan** models to **OpenMx** with the
#' `lavaan.to.OpenMx` function.
#'
#' Because **mxsem** implements the syntax parser from scratch, it can extend the
#' **lavaan** syntax to account for specific **OpenMx** features. This enables
#' implicit transformations with curly braces.
#'
#'
#' ## Citation
#'
#' Cite **OpenMx** (Boker et al., 2011) for the modeling and **lavaan** for the
#' syntax (Rosseel, 2012). **mxsem** itself is just a very small package and lets
#' **OpenMx** do all the heavy lifting.
#'
#' ## Defaults
#'
#' By default, **mxsem** scales latent variables by setting the loadings on the first
#' item to 1. This can be changed by setting `scale_loadings = FALSE` in the function
#' call. Setting `scale_latent_variances = TRUE` sets latent variances to 1 for
#' scaling.
#'
#' **mxsem** will add intercepts for all manifest variables as well as variances for
#' all manifest and latent variables. A lower bound of 1e-6 will be added to all
#' variances. Finally, covariances for all exogenous variables will be added.
#' All of these options can be changed when calling **mxsem**.
#'
#' ## Syntax
#'
#' The syntax is, for the most part, identical to that of **lavaan**. The following
#' specifies loadings of a latent variable `eta` on manifest variables `y1`-`y4`:
#' ```
#' eta =~ y1 + y2 + y3
#' ```
#' Regressions are specified with `~`:
#' ```
#' xi  =~ x1 + x2 + x3
#' eta =~ y1 + y2 + y3
#' # predict eta with xi:
#' eta ~  xi
#' ```
#' Add covariances with `~~`
#' ```
#' xi  =~ x1 + x2 + x3
#' eta =~ y1 + y2 + y3
#' # predict eta with xi:
#' eta ~  xi
#' x1 ~~ x2
#' ```
#' Intercepts are specified with `~1`
#' ```
#' xi  =~ x1 + x2 + x3
#' eta =~ y1 + y2 + y3
#' # predict eta with xi:
#' eta ~  xi
#' x1 ~~ x2
#'
#' eta ~ 1
#' ```
#'
#' ## Parameter labels and constraints
#'
#' Add labels to parameters as follows:
#' ```
#' xi  =~ l1*x1 + l2*x2 + l3*x3
#' eta =~ l4*y1 + l5*y2 + l6*y3
#' # predict eta with xi:
#' eta ~  b*xi
#' ```
#' Fix parameters by using numeric values instead of labels:
#' ```
#' xi  =~ 1*x1 + l2*x2 + l3*x3
#' eta =~ 1*y1 + l5*y2 + l6*y3
#' # predict eta with xi:
#' eta ~  b*xi
#' ```
#'
#' ## Bounds
#'
#' Lower and upper bounds allow for constraints on parameters. For instance,
#' a lower bound can prevent negative variances.
#' ```
#' xi  =~ 1*x1 + l2*x2 + l3*x3
#' eta =~ 1*y1 + l5*y2 + l6*y3
#' # predict eta with xi:
#' eta ~  b*xi
#' # residual variance for x1
#' x1 ~~ v*x1
#' # bound:
#' v > 0
#' ```
#' Upper bounds are specified with v < 10. Note that the parameter label must always
#' come first. The following is not allowed: `0 < v` or `10 > v`.
#'
#' ## (Non-)linear constraints
#'
#' Assume that latent construct `eta` was observed twice, where `eta1` is the first
#' observation and `eta2` the second. We want to define the loadings of `eta2`
#' on its observations as `l_1 + delta_l1`. If `delta_l1` is zero, we have measurement
#' invariance.
#'
#' ```
#' eta1 =~ l1*y1 + l2*y2 + l3*y3
#' eta2 =~ l4*y4 + l5*y5 + l6*y6
#' # define new delta-parameter
#' !delta_1; !delta_2; !delta_3
#' # redefine l4-l6
#' l4 := l1 + delta_1
#' l5 := l2 + delta_2
#' l6 := l3 + delta_3
#' ```
#'
#' Alternatively, implicit transformations can be used as follows:
#'
#' ```
#' eta1 =~ l1*y1 + l2*y2 + l3*y3
#' eta2 =~ {l1 + delta_1} * y4 + {l2 + delta_2} * y5 + {l3 + delta_3} * y6
#' ```
#'
#' Specific labels for the transformation results can also be provided:
#'
#' ```
#' eta1 =~ l1*y1 + l2*y2 + l3*y3
#' eta2 =~ {l4 := l1 + delta_1} * y4 + {l5 := l2 + delta_2} * y5 + {l6 := l3 + delta_3} * y6
#' ```
#'
#' This is inspired by the approach in **metaSEM** (Cheung, 2015).
#'
#'
#' ## Definition variables
#'
#' Definition variables allow for person-specific parameter constraints. Use the
#' `data.`-prefix to specify definition variables.
#' ```
#' I =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
#' S =~ data.t_1 * y1 + data.t_2 * y2 + data.t_3 * y3 + data.t_4 * y4 + data.t_5 * y5
#'
#' I ~ int*1
#' S ~ slp*1
#' ```
#'
#' ## Starting Values
#'
#' **mxsem** differs from **lavaan** in the specification of starting values. Instead
#' of providing starting values in the model syntax, the `set_starting_values`
#' function is used.
#'
#' ## References
#'
#' * Bates, T. C., Maes, H., & Neale, M. C. (2019). umx: Twin and Path-Based Structural Equation Modeling in R. Twin Research and Human Genetics, 22(1), 27–41. https://doi.org/10.1017/thg.2019.2
#' * Bates, T. C., Prindle, J. J. (2014). ezMx. https://github.com/OpenMx/ezMx
#' * Boker, S. M., Neale, M., Maes, H., Wilde, M., Spiegel, M., Brick, T., Spies, J., Estabrook, R., Kenny, S., Bates, T., Mehta, P., & Fox, J. (2011).
#' OpenMx: An Open Source Extended Structural Equation Modeling Framework. Psychometrika, 76(2), 306–317. https://doi.org/10.1007/s11336-010-9200-6
#' * Cheung, M. W.-L. (2015). metaSEM: An R package for meta-analysis using structural equation modeling. Frontiers in Psychology, 5. https://doi.org/10.3389/fpsyg.2014.01521
#' * Rosseel, Y. (2012). lavaan: An R package for structural equation modeling. Journal of Statistical Software, 48(2), 1–36. https://doi.org/10.18637/jss.v048.i02
#' * van Lissa, C. J. (2023). tidySEM: Tidy Structural Equation Modeling. R package version 0.2.4, https://cjvanlissa.github.io/tidySEM/.
#'
#' @encoding UTF-8
#' @param model model syntax similar to **lavaan**'s syntax
#' @param data raw data used to fit the model. Alternatively, an object created
#' with `OpenMx::mxData` can be used (e.g., `OpenMx::mxData(observed = cov(OpenMx::Bollen), means = colMeans(OpenMx::Bollen), numObs = nrow(OpenMx::Bollen), type = "cov")`).
#' @param scale_loadings should the first loading of each latent variable be used for scaling?
#' @param scale_latent_variances should the latent variances be used for scaling?
#' @param add_intercepts should intercepts for manifest variables be added automatically? If set to false, intercepts must be added manually. If no intercepts
#' are added, **mxsem** will automatically use just the observed covariances and not the observed means.
#' @param add_variances should variances for manifest and latent variables be added automatically?
#' @param add_exogenous_latent_covariances should covariances between exogenous latent variables be
#' added automatically?
#' @param add_exogenous_manifest_covariances should covariances between exogenous manifest variables be
#' added automatically?
#' @param lbound_variances should the lower bound for variances be set to 0.000001?
#' @param directed symbol used to indicate directed effects (regressions and loadings)
#' @param undirected symbol used to indicate undirected effects (variances and covariances)
#' @param return_parameter_table if set to TRUE, the internal parameter table is returend
#' together with the mxModel
#' @return mxModel object that can be fitted with mxRun or mxTryHard. If return_parameter_table
#' is TRUE, a list with the mxModel and the parameter table is returned.
#' @export
#' @import OpenMx
#' @importFrom stats cov
#' @importFrom methods is
#' @md
#' @examples
#' # THE FOLLOWING EXAMPLE IS ADAPTED FROM LAVAAN
#' library(mxsem)
#'
#' model <- '
#'   # latent variable definitions
#'      ind60 =~ x1 + x2 + x3
#'      dem60 =~ y1 + a1*y2 + b*y3 + c1*y4
#'      dem65 =~ y5 + a2*y6 + b*y7 + c2*y8
#'
#'   # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'
#'   # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8
#' '
#'
#' fit <- mxsem(model = model,
#'             data  = OpenMx::Bollen) |>
#'   mxTryHard()
#' omxGetParameters(fit)
#'
#'
#' model_transformations <- '
#'   # latent variable definitions
#'      ind60 =~ x1 + x2 + x3
#'      dem60 =~ y1 + a1*y2 + b1*y3 + c1*y4
#'      dem65 =~ y5 + {a2 := a1 + delta_a}*y6 + {b2 := b1 + delta_b}*y7 + c2*y8
#'
#'   # regressions
#'     dem60 ~ ind60
#'     dem65 ~ ind60 + dem60
#'
#'   # residual correlations
#'     y1 ~~ y5
#'     y2 ~~ y4 + y6
#'     y3 ~~ y7
#'     y4 ~~ y8
#'     y6 ~~ y8
#' '
#'
#' fit <- mxsem(model = model_transformations,
#'             data  = OpenMx::Bollen) |>
#'   mxTryHard()
#' omxGetParameters(fit)
mxsem <- function(model,
                  data,
                  scale_loadings = TRUE,
                  scale_latent_variances = FALSE,
                  add_intercepts = TRUE,
                  add_variances = TRUE,
                  add_exogenous_latent_covariances = TRUE,
                  add_exogenous_manifest_covariances = TRUE,
                  lbound_variances = TRUE,
                  directed = unicode_directed(),
                  undirected = unicode_undirected(),
                  return_parameter_table = FALSE){

  if(scale_loadings & scale_latent_variances)
    warning("Set either scale_loadings OR scale_latent_variances to TRUE. Setting both to TRUE is not necessary.")

  # split the string into pre-processing, model name, and model syntax
  splitted_syntax <- find_model_name(syntax = model)

  parameter_table <- parameter_table_rcpp(syntax = splitted_syntax$model_syntax,
                                          add_intercept = add_intercepts,
                                          add_variance = add_variances,
                                          add_exogenous_latent_covariances = add_exogenous_latent_covariances,
                                          add_exogenous_manifest_covariances = add_exogenous_manifest_covariances,
                                          scale_latent_variance = scale_latent_variances,
                                          scale_loading = scale_loadings)

  check_all_fields(parameter_table)
  parameter_table <- check_modifier_for_algebra(parameter_table = parameter_table,
                                                directed = directed,
                                                undirected = undirected)

  if(is(data, "MxDataStatic")){
    mx_data <- data
  }else{
    if(!add_intercepts){
      mx_data <- OpenMx::mxData(observed = stats::cov(as.matrix(data[,parameter_table$variables$manifests])),
                                type = "cov",
                                numObs = nrow(data))
    }else{
      mx_data <- OpenMx::mxData(data, type = "raw")
    }
  }

  mxMod <- OpenMx::mxModel(
    model = ifelse(test = splitted_syntax$model_name == "",
                   NA,
                   splitted_syntax$model_name),
    type = "RAM",
    manifestVars = parameter_table$variables$manifests,
    latentVars = parameter_table$variables$latents,
    mx_data)

  mxMod <- add_path(mxMod,
                    parameter_table,
                    lbound_variances,
                    directed,
                    undirected)
  mxMod <- add_algebra(mxMod,
                       parameter_table = parameter_table,
                       parameter_table$algebras,
                       parameter_table$new_parameters,
                       parameter_table$new_parameters_free)

  # add user defined elements
  if(length(parameter_table$user_defined) != 0)
    warning("User defined elements using curly braces can be problematic. Make sure to check the model thoroughly")
  for(user_def in parameter_table$user_defined)
    mxMod <- mxModel(
      mxMod,
      eval(parse(text = user_def))
    )

  if(!return_parameter_table)
    return(mxMod)

  return(
    list(
      model = mxMod,
      parameter_table = parameter_table
    )
  )

}

add_path <- function(mxMod,
                     parameter_table,
                     lbound_variances,
                     directed,
                     undirected){
  pt <- parameter_table$parameter_table
  for(i in 1:nrow(pt)){

    if(pt$op[i] %in% c("=~", "~~", "~")){
      # directed effect
      if(pt$op[i]  == "~"){
        from <- pt$rhs[i]
        to <- pt$lhs[i]

        if(from == "1"){
          from <- "one"
        }

      }else{
        from <- pt$lhs[i]
        to <- pt$rhs[i]
      }

      if(pt$op[i] == "~~"){
        arrows <- 2
      }else{
        arrows <- 1
      }

      parameter_setting <- parse_modifier(pt$modifier[i],
                                          pt$free[i])
      free <- parameter_setting$free
      value <- parameter_setting$value
      label <- parameter_setting$label
      lbound <- parse_bounds(pt$lbound[i])
      ubound <- parse_bounds(pt$ubound[i])

      if((from == to) &&
         (arrows == 2) &&
         free){
        # is variance
        # starting value for variances
        if(is.na(value))
          value <- .1

        if(lbound_variances && is.na(lbound))
          lbound <- 0.000001
      }

      if(is.na(label)){
        if(arrows == 1){
          label <- paste0(from, directed, to)
        }else{
          label <- paste0(from, undirected, to)
        }
      }

      mxMod <- OpenMx::mxModel(mxMod,
                               OpenMx::mxPath(from = from,
                                              to = to,
                                              arrows = arrows,
                                              free = free,
                                              values = value,
                                              labels = label,
                                              lbound = lbound,
                                              ubound = ubound
                               ))
    }

  }
  return(mxMod)
}

parse_modifier <- function(modifier, free){

  parameter_setting <- data.frame(label = NA,
                                  value = NA,
                                  free  = NA)
  if(free == "FALSE"){
    parameter_setting$free <- FALSE
  }else{
    parameter_setting$free <- TRUE
  }

  if(modifier == ""){
    return(parameter_setting)
  }
  if(grepl(pattern = "^[0-9\\.-]+$", x = modifier)){
    # the modifier is a parameter value
    parameter_setting$value <- as.numeric(modifier)
    parameter_setting$free  <- FALSE
    return(parameter_setting)
  }

  if(grepl(pattern = "^data\\.+[a-zA-Z0-9\\.-_]+$", x = modifier)){
    # the modifier is a definition variable
    parameter_setting$label <- modifier
    parameter_setting$free  <- FALSE
    return(parameter_setting)
  }

  if(grepl(pattern = "^[a-zA-Z]+[a-zA-Z0-9\\.-_]*$", x = modifier)){
    # the modifier is a label
    parameter_setting$label <- modifier
    return(parameter_setting)
  }

  stop("The following modifier is not allowed: ", modifier)
}

parse_bounds <- function(bound){
  if(bound == "")
    return(NA)

  if(grepl(pattern = "^[0-9\\.-]+$", x = bound)){
    # the modifier is a parameter value
    return(as.numeric(bound))
  }

  stop("Invalid bound", bound)
}


add_algebra <- function(mxMod,
                        parameter_table,
                        algebras,
                        new_parameters,
                        new_parameters_free){

  if(nrow(algebras) == 0)
    return(mxMod)

  if(length(new_parameters) > 0){
    # add the new parameters:
    labels <- new_parameters
    labels[new_parameters_free != "TRUE"] <- paste0(labels[new_parameters_free != "TRUE"], "[1,1]")
    mxMod <- OpenMx::mxModel(mxMod,
                             mxMatrix(type = "Full",
                                      values = rep(.01, length(new_parameters)),
                                      nrow = 1,
                                      ncol = length(new_parameters),
                                      free = new_parameters_free == "TRUE",
                                      labels = labels,
                                      name = "new_parameters"))

  }

  for(i in 1:nrow(algebras)){
    mxMod$A$labels[mxMod$A$labels == algebras$lhs[i]] <- paste0(algebras$lhs[i], "[1,1]")
    mxMod$S$labels[mxMod$S$labels == algebras$lhs[i]] <- paste0(algebras$lhs[i], "[1,1]")
    mxMod$M$labels[mxMod$M$labels == algebras$lhs[i]] <- paste0(algebras$lhs[i], "[1,1]")
    if(!is.null(mxMod$new_parameters)){
      mxMod$new_parameters$free[mxMod$new_parameters$labels == algebras$lhs[i]] <- FALSE
      mxMod$new_parameters$labels[mxMod$new_parameters$labels == algebras$lhs[i]] <- paste0(algebras$lhs[i], "[1,1]")
    }
    mxMod <- OpenMx::mxModel(mxMod,
                             OpenMx::mxAlgebraFromString(algString = algebras$rhs[i],
                                                         name = algebras$lhs[i]))
  }

  return(mxMod)
}
