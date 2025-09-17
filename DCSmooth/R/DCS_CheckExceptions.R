################################################################################
#                                                                              #
#                 DCSmooth Package: Checks for Exceptions                      #
#                                                                              #
################################################################################

# These functions check the data, the options and a given bandwidth for conform-
# ity with the requirements of the package. They stop the function and return an
# error, if something wrong.

  # exception.check.Y
  # exception.check.XT
  # exception.check.bndw
  # exception.check.args_list
  # exception.check.parallel
  # exception.check.options.input
  # exception.check.options
  # exception.check.model_order
  # exception.check.order_max
  # exception.check.model.sarma

#----------------------Check Matrix of Observations Y--------------------------#

exception.check.Y = function(Y)
{
  sys.call(-1)
  # check for missing values
  if (any(is.na(Y)))
  {
    stop("Y contains missing values (NAs).")
  }
  
  # check if Y is numeric
  if (!is.numeric(Y) || !is.matrix(Y))
  {
    stop("Y must be a numeric matrix.")
  }
  
  # check for correct dimension of Y
  if (any(dim(Y) < 5)) {
    stop("Y has to be at least of dimension 5 in each direction.")
  }
}

exception.check.XT = function(Y, X, T)
{
  sys.call(-1)
  # check for missing values
  if (any(is.na(X)) || any(is.na(T)))
  {
    stop("X and/or T contain missing values (NAs)")
  }
  
  # check if X, T is numeric
  if (!is.numeric(X) || !is.numeric(T))
  {
    stop("X and/or T be a numeric vector")
  }

  # check for correct dimension of X, T
  if (dim(Y)[1] != length(X))
  {
    stop("Rows of Y not matching X")
  }
  if (dim(Y)[2] != length(T))
  {
    stop("Columns of Y not matching T")
  }
}

#-----------------------Check for usable bandwidths----------------------------#

exception.check.bndw = function(bndw, dcs_options)
{
  if (bndw[1] != "auto")
  {
    if (!(is.numeric(bndw)) || (length(bndw) != 2))
    {
      stop("Bandwidth h must be a numeric vector of length 2.")
    }
      
    if (any(bndw < 0))
    {
      stop("Bandwidth h must be positive")
    }  
      
    if (any(bndw > 0.45) && dcs_options$type == "KR")
    {
      stop("Bandwidth h must be < 0.45 for kernel regression")
    } 
  }
}

#-------------------------------Check args_list--------------------------------#

exception.check.args_list = function(args_list)
{
  if (any(!(names(args_list) %in% dcs_list_args_dcs)))
  {
    index = which(!(names(args_list) %in% dcs_list_args_dcs))
    warning("Additional argument(s) \"", names(args_list)[index], 
            "\" are unsupported and will be ignored.")
  }
}

#--------------------------Check parallel Options------------------------------#

exception.check.parallel = function(parallel)
{
  if (!is.logical(parallel) || length(parallel) != 1)
  {
    stop("Unsupported value in argument \"parallel\".")
  }
}

#-----------------------Check for correct Options------------------------------#

# Check input for set.options()
exception.check.options.input = function(type, kerns, drv, var_model,
                                         IPI_options)
{
  if (length(type) != 1 || !(type %in% c("LP", "KR")))
  {
    stop("Unsupported values in argument \"type\".")
  }
  if (!(all(kerns %in% dcs_list_kernels)))
  {
    stop("Unsupported values in argument \"kerns\".")
  }
  if(!(is.numeric(drv)) || length(drv) != 2)
  {
    stop("Unsupported values in argument \"drv\".")
  }
  if (!(var_model %in% dcs_list_var_model) ||
      length(var_model) != 1)
  {
    stop("Unknown values in argument \"var_model\".")
  }
  
  # IPI_options
  unknown_IPI = names(IPI_options)[which(!(names(IPI_options)
                                           %in% dcs_list_IPI))]
  if (length(unknown_IPI) > 0)
  {
    warning("Unknown argument \"", unknown_IPI, "\" will be ignored.")
  }
  if (exists("infl_exp", IPI_options) && !(IPI_options$infl_exp[1] == "auto") &&
      (!is.numeric(IPI_options$infl_exp) || length(IPI_options$infl_exp) != 2))
  {
    stop("Unknown values in argument \"IPI_options$infl_exp\".")
  }
  if (exists("infl_par", IPI_options) && (!is.numeric(IPI_options$infl_par) ||
      length(IPI_options$infl_par) != 2))
  {
    stop("Unknown values in argument \"IPI_options$infl_par\".")
  }
  if (exists("trim", IPI_options) && (!is.numeric(IPI_options$trim) ||
      length(IPI_options$trim) != 2))
  {
    stop("Unknown values in argument \"IPI_options$trim\".")
  }
  if (exists("const_window", IPI_options) && 
      (!is.logical(IPI_options$const_window) ||
      length(IPI_options$const_window) != 1))
  {
    stop("Unknown values in argument \"IPI_options$const_window\".")
  }
}

# This function is used in the .setOptions() function for direct check as well
# as in the dcs() function.

exception.check.options = function(dcs_opt)
{
  sys.call(-1)
# check class
  if(!(class(dcs_opt) == "dcs_options"))
  {
    stop("Incorrect options specified, please use \"set.options()\".")
  }
  
# check for unknown or missing options
  unknown_name = names(dcs_opt)[which(!(names(dcs_opt) %in% dcs_list_options))]
  if (length(unknown_name) > 0)
  {
    warning("Option \"", unknown_name, "\" is unknown and will be ignored.")
  }
  unspec_name = dcs_list_options[which(!(dcs_list_options %in%
                                                  names(dcs_opt)))]
  if (length(unspec_name) > 0)
  {
    stop("Option \"", unspec_name, "\" not specified.")
  }
  
# check kernels
  if (!(dcs_opt$kerns[1] %in% dcs_list_kernels) || 
      !(dcs_opt$kerns[2] %in% dcs_list_kernels))
  {
    stop("Unsupported kernels specified.")
  }
  
# check regression type
  if (!(dcs_opt$type %in% c("LP", "KR")))
  {
    stop("Unsupported regression type. Choose \"KR\" or \"LP\"")
  }
  
# check derivative orders
  if (!is.numeric(dcs_opt$drv))
  {
    stop("Derivative order must be numeric.")
  }
  if (any(dcs_opt$drv < 0))
  {  
    stop("Derivative order must be at least 0.")
  }
  
# check trim orders
  if (!is.numeric(dcs_opt$IPI_options$trim))
  {
    stop("Shrink factor \"trim\" must be numeric.")
  }
  if (length(dcs_opt$IPI_options$trim) != 2)
  {
    stop("Shrink factor \"trim\" must be a numeric vector of length 2.")
  }
  if (any(dcs_opt$IPI_options$trim < 0) ||
      any(dcs_opt$IPI_options$trim > 0.5))
  {
    stop("Shrink factor \"trim\" must be between 0 and 0.5.")
  }
  
# Options for derivative estimation
  if (!is.numeric(dcs_opt$drv))
  {
    stop("Derivative order must be numeric.")
  }
  if (any(dcs_opt$drv != as.numeric(substr(dcs_opt$kerns,
                            nchar(dcs_opt$kerns), nchar(dcs_opt$kerns)))))
  {
    stop("Kernel orders not matching derivative orders.")
  }
  if (any(dcs_opt$drv > 2))
  {
    stop("Estimation of derivatives > 2 currently not supported.")
  }
  if (dcs_opt$type == "KR" && any(dcs_opt$drv > 0) &&
      any(dcs_opt$drv[which(substr(dcs_opt$kerns, 1, 1) == "T")] > 0))
  {
    stop("Estimation of derivatives > 0 currently not supported for ",
         "truncated kernels.")
  }

### Options for variance estimation method
  if (!(dcs_opt$var_model %in% dcs_list_var_model))
  {
    stop("unsupported method in \"var_model\".")
  }
  if (dcs_opt$var_model == "np")
  {
    warning("Nonparametric estimation of variance factor c_f currently in ",
            "experimental state (probably slow).")
  }
  
  ### Model selection options
  if (exists("model_order", dcs_opt$add_options))
  {
    exception.check.model_order(dcs_opt$add_options$model_order,
                                dcs_opt$var_model)
  }
  if (exists("order_max", dcs_opt$add_options))
  {
    exception.check.order_max(dcs_opt$add_options$order_max)
  }
}

#---------------------------Check additional Options---------------------------#

exception.check.model_order = function(model_order, var_model)
{
  if (var_model[1] == "iid")
  {
    message("argument \"model_order\" is unused for iid. errors.")
  }
  
  if (!is.list(model_order) && (length(model_order) == 1 &&
      model_order %in% c("aic", "bic", "gpac")))
  {
    # No GPAC estimation for SFARIMA possible
    if (model_order == "gpac" && var_model == "sfarima_RSS")
    {
      stop("Order selection of type \"gpac\" not supported for SFARIMA models.")
    } else {
      return(NULL)
    }
  }
  
  if (!is.list(model_order) && (length(model_order) != 1 ||
      !(model_order %in% c("aic", "bic", "gpac"))))
  {
    stop("unknown model selection criterion specified in \"model_order\".")
  }
  if (is.list(model_order) && !(exists("ar", model_order) &&
                                exists("ma", model_order)))
  {
    stop("\"model_order\" incorrectly specified.")
  }
  if (!is.numeric(model_order$ar) || length(model_order$ar) != 2 ||
      any(model_order$ar < 0))
  {
    stop("AR order of \"model_order\" incorrectly specified")
  }
  if (!is.numeric(model_order$ma) || length(model_order$ma) != 2 ||
      any(model_order$ma < 0))
  {
    stop("MA order of \"model_order\" incorrectly specified")
  }
}

exception.check.order_max = function(order_max)
{
  if (!(exists("ar", order_max) && exists("ma", order_max)))
  {
    stop("\"order_max\" incorrectly specified.")
  }
  if (!is.numeric(order_max$ar) || length(order_max$ar) != 2 ||
      any(order_max$ar < 0))
  {
    stop("AR order of \"order_max\" incorrectly specified")
  }
  if (!is.numeric(order_max$ma) || length(order_max$ma) != 2 ||
      any(order_max$ma < 0))
  {
    stop("MA order of \"model_order\" incorrectly specified")
  }
}

#------------------------Exceptions for SARMA/SFARIMA--------------------------#

exception.check.model.sarma = function(model)
{
  if (any(is.na(unlist(model))))
  {
    stop("Missing values in subelements of list \"model\".")
  }
  
  if (!is.numeric(unlist(model)))
  {
    stop("Only numerical values in subelements of list \"model\" allowed.")
  }

  if (as.matrix(model$ar)[1, 1] != 1)
  {
    warning("Entry in upper left of matrix \"$ar\" is not equal to 1.")
  }
  if (as.matrix(model$ma)[1, 1] != 1)
  {
    warning("Entry in upper left of matrix \"$ma\" is not equal to 1.")
  }
  
  if (length(model$sigma) != 1)
  {
    stop("Argument \"sigma\" should be a scalar value.")
  }
}
