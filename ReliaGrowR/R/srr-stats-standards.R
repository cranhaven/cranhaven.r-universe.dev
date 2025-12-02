#' NA_standards
#'
#' @srrstatsNA {G1.4a} There are no internal (non-exported) functions.
#' @srrstatsNA {G1.5} There are no performance claims made in an associated publication.
#' @srrstatsNA {G1.6} There are no performance claims compared to other R packages.
#' @srrstatsNA {G2.0a} The primary documentation covers all expectations on lengths of inputs.
#' @srrstatsNA {G2.1a} The primary documentation covers all expectations on data types of all vector inputs.
#' @srrstatsNA {G2.4} See sub-tags for responses.
#' @srrstatsNA {G2.4a} `integer` conversion is not required.
#' @srrstatsNA {G2.4c} `as.character()` conversion is not required.
#' @srrstatsNA {G2.4d} `as.factor()` conversion is not required.
#' @srrstatsNA {G2.4e} `as...()` conversion is not required.
#' @srrstatsNA {G2.5} There are no factor inputs.
#' @srrstatsNA {G2.9} No information is lost in type conversions.
#' @srrstatsNA {G2.12} list columns in `data.frame` objects are not used.
#' @srrstatsNA {G3.0} There are no comparisons made between floating point numbers.
#' @srrstatsNA {G3.1} Currently, there are no covariance calculations since all models
#' are univariate. However, this may change in future versions.
#' @srrstatsNA {G3.1a} Currently, there are no covariance calculations since all models
#' are univariate. However, this may change in future versions.
#' @srrstatsNA {G4.0} There are no local file outputs at this time.
#' @srrstatsNA {G5.3} Missing values return an error.
#' @srrstatsNA {G5.4a} There are new implementations of existing methods but no new methods.
#' @srrstatsNA {G5.4b} There are no known implementations of these methods in R.
#' @srrstatsNA {G5.6b} Parameter recovery tests do not include random components.
#' @srrstatsNA {G5.11} Unit tests do not require large amounts of data.
#' @srrstatsNA {G5.11a} Unit tests do not require downloads of additional data.
#' @srrstatsNA {G5.12} Unit tests do not require any special platform requirements,
#' memory, expected runtime, or artefacts.
#' @srrstatsNA {RE1.0} The dependent variable (failures) and independent
#' variable (times) are defined in the function arguments, not in a formula interface.
#' @srrstatsNA {RE1.1} The dependent variable (failures) and independent
#' variable (times) are defined in the function arguments, not in a formula interface.
#' @srrstatsNA {RE2.0} Input data is transformed using simple mathematical operations
#' (e.g., cumsum) that do not require special treatment.
#' @srrstatsNA {RE2.3} Data centering is not required.
#' @srrstatsNA {RE3.0} Algorithms are closed form solutions and do not require iterative
#' methods.
#' @srrstatsNA {RE3.1} Algorithms are closed form solutions and do not require iterative
#' methods.
#' @srrstatsNA {RE3.2} Algorithms are closed form solutions and do not require iterative
#' methods.
#' @srrstatsNA {RE3.3} Algorithms are closed form solutions and do not require iterative
#' methods.
#' @srrstatsNA {RE4.1} Software does not enable the ability to generate a model object
#' without actually fitting values.
#' @srrstatsNA {RE4.6} The current functionality is for univariate analysis only.
#' As such, the variance-covariance matrix of the model parameters is not applicable.
#' @srrstatsNA {RE4.7} Convergence is not applicable as the algorithms are closed form solutions.
#' @srrstatsNA {RE4.12} Input data is transformed using simple mathematical operations
#' (e.g., cumsum) that do not require special treatment.
#' @srrstatsNA {RE4.14} Model objects are not used to generate a forecast, though
#' prediction is possible in a future version.
#' @srrstatsNA {RE4.15} Model objects are not used to generate a forecast, though
#' prediction is possible in a future version.
#' @srrstatsNA {RE4.16} Model objects are not used to generate a forecast, though
#' prediction is possible in a future version.
#' @srrstatsNA {RE4.18} Summary statistics are trivial and do not require special
#' methods.
#' @srrstatsNA {RE6.1} Default plot method is a generic plot method dispatched on
#' the class of return objects.
#' @srrstatsNA {RE6.3} Model objects are not used to generate a forecast, though
#' prediction is possible in a future version.
#' @srrstatsNA {RE7.0} The current functionality is for univariate analysis only.
#' As such, tests involving relationships between predictor (independent) variables
#' are not applicable.
#' @srrstatsNA {RE7.0a} The current functionality is for univariate analysis only.
#' As such, tests involving relationships between predictor (independent) variables
#' are not applicable.
#' @srrstatsNA {RE7.4} Model objects are not used to generate a forecast, though
#' prediction is possible in a future version.
#' @noRd
NULL
