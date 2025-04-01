# Internal functions for parameter checking, as part of the clusterability R package

# Copyright (C) 2020  Zachariah Neville, Naomi Brownstein, Andreas Adolfsson, Margareta Ackerman
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

# Validates the data provided
validate_data <- function(d, dname) {

  isnullmiss <- tryCatch(isTRUE(is.null(d)), error = function(e) return(TRUE))
  if(isnullmiss) {
    stop(paste("The dataset", dname, "is NULL or is invalid."))
  }

  if(NROW(d) == 0) {
    stop(paste("The dataset", dname, "has 0 rows."))
  }

  if(NCOL(d) == 0) {
    stop(paste("The dataset", dname, "has 0 columns."))
  }
}


# Validates the 'test' parameter and returns an uppercase version of it.
validate_test <- function(test) {
  valid_test <- c("DIP", "SILVERMAN")

  isvalid <- tryCatch(isTRUE((!is.null(test) & (toupper(test) %in% valid_test))), error = function(e) return(FALSE))

  if(isvalid) {
    return(toupper(test))
  } else {
    stop("Invalid 'test' argument was specified. The 'test' must be either \"dip\" or \"silverman\"")
  }
}

# Validates the 'distance_metric' parameter and returns a lowercase version of it.
validate_metric <- function(metric, x) {
  isnullmiss <- tryCatch(isTRUE(is.null(metric)), error = function(e) return(TRUE))

  if (isnullmiss) {
    warning("Invalid distance metric was entered. The default metric (\"euclidean\") will be used. Please see the help file for a list of valid metrics.")
    return("euclidean")
  } else {
    lowermetric <- tolower(metric)

    validmetrics <- c("euclidean", "maximum", "manhattan", "canberra", "binary", "sqeuc", "sqcorr", "corr", "cov")

    # This is to check for a valid minkowski metric. Form must be "minkowski(p)", where p is a positive numeric value (not necessarily integer)
    minkowski_regex <- "minkowski\\([[:digit:]]*[.]?[[:digit:]]*\\)"
    is_minkowski <- grepl(minkowski_regex, lowermetric, ignore.case = TRUE)

    # If it's one of the preapproved metrics, return the lowercase version
    if (isTRUE(lowermetric %in% validmetrics)) {
      if (isTRUE(lowermetric %in% c("cov", "corr", "sqcorr") & identical(as.double(NCOL(x)), 1))) {
        stop("The 'cov', 'corr', and 'sqcorr' metrics are not available for 1-dimensional data.")
      } else {
        return(lowermetric)
      }
    } else if (is_minkowski) {
      # If it's a minkowski metric, then check the "p" value
      pattern1 <- "minkowski\\("
      pattern2 <- "\\)"

      out1 <- sub(pattern1, "", lowermetric)
      minkowski_p <- as.numeric(sub(pattern2, "", out1))

      # "p" must be positive numeric
      if (minkowski_p > 0) {
        return(lowermetric)
      } else {
        warning("Invalid value for p was entered when using the Minkowski metric. p must be a positive number. The default value of 2 will be used.")
        return("minkowski(2)")
      }

    } else {
      warning("Invalid distance metric was entered. The default metric (\"euclidean\") will be used. Please see the help file for a list of valid metrics.")
      return("euclidean")
    }
  }
}

# Validates the 'reduction' argument and returns an uppercase version of it
validate_reduction <- function(reduction) {
  valid_reduction <- c("PCA", "DISTANCE", "NONE")
  isvalid <- tryCatch(isTRUE((!is.null(reduction) & (toupper(reduction) %in% valid_reduction))),
                      error = function(e) return(FALSE))

  if(isvalid) {
    return(toupper(reduction))
  }
  else {
    warning("Invalid reduction method was used. No reduction was performed. The 'reduction' argument must be \"PCA\", \"DISTANCE\", or \"NONE\"")
    return("NONE")
  }
}

# Validates the 'is_dist_matrix' argument and returns its value
validate_isdistmatrix <- function(is_dist_matrix, reduction, data) {
  if(validate_boolean(is_dist_matrix, "is_dist_matrix", FALSE)) {
    if(!identical(reduction, "NONE")) {
      stop("When providing a value of TRUE for the 'is_dist_matrix' argument, the 'reduction' argument must be \"NONE\".")
    } else if (!isSymmetric(as.matrix(data), check.attributes = FALSE)){
		stop("When providing a value of TRUE for the 'is_dist_matrix' argument, the dataset must be a symmetric matrix.")
	} else {
      return(TRUE)
    }
  } else {
    return(FALSE)
  }
}


# Validates the 'distance_standardize' argument and returns an uppercase version of it
validate_standardize <- function(standard) {
  valid_stdize <- c("STD", "NONE", "MEAN", "MEDIAN")
  isvalid <- tryCatch(isTRUE((!is.null(standard) & (toupper(standard) %in% valid_stdize))),
                      error = function(e) return(FALSE))

  if (isvalid) {
    return(toupper(standard))
  } else {
    warning("Invalid standardization technique was used. No standardization was performed. Please see documentation for valid techniques.")
    return("NONE")
  }
}

# In the case of PCA, need the "pca_center" and "pca_scale" to be logical type
validate_pca_center <- function(center) {
  return(validate_boolean(center, "pca_center", TRUE))
}

validate_pca_scale <- function(scale) {
  return(validate_boolean(scale, "pca_scale", TRUE))
}

validate_completecase <- function(cc) {
  return(validate_boolean(cc, "completecase", FALSE))
}

validate_dsimulatepvalue <- function(spv) {
  return(validate_boolean(spv, "d_simulatepvalue", FALSE))
}

validate_sadjust <- function(sadj) {
  return(validate_boolean(sadj, "s_adjust", TRUE))
}

validate_soutseed <- function(souts) {
  return(validate_boolean(souts, "s_outseed", FALSE))
}

validate_dreps <- function(d) {
  isvalid <- tryCatch(isTRUE((!is.null(d) & is.numeric(d) & (d >= 1) & ((d %% 1) == 0))),
                      error = function(e) return(FALSE))

  if (isvalid) {
    return(d)
  } else {
    warning("The value of 'd_reps' must be a positive integer. The default value of 2000 will be used.")
    return(2000)
  }
}

validate_sk <- function(sk) {
  isvalid <- tryCatch(isTRUE((!is.null(sk) & is.numeric(sk) & (sk >= 1) & ((sk %% 1) == 0))),
                      error = function(e) return(FALSE))

  if (isvalid) {
    return(sk)
  } else {
    warning("The value of 's_k' must be a positive integer. The default value of 1 will be used.")
    return(1)
  }
}

validate_sm <- function(sm) {
  isvalid <- tryCatch(isTRUE((!is.null(sm) & is.numeric(sm) & (sm >= 1) & ((sm %% 1) == 0))),
                      error = function(e) return(FALSE))

  if (isvalid) {
    return(sm)
  } else {
    warning("The value of 's_m' must be a positive integer. The default value of 999 will be used.")
    return(999)
  }
}

validate_sdigits <- function(sd) {
  isvalid <- tryCatch(isTRUE((!is.null(sd) & is.numeric(sd) & (sd >= 1) & ((sd %% 1) == 0))),
                      error = function(e) return(FALSE))

  if (isvalid) {
    return(sd)
  } else {
    warning("The value of 's_digits' must be a positive integer. The default value of 6 will be used.")
    return(6)
  }
}

validate_ssetseed <- function(seed) {
  # Note that NULL is the default so it should still be allowed
  isinteger <- tryCatch(isTRUE((is.numeric(seed) & (seed %% 1 == 0))),
                      error = function(e) return(FALSE))

  isnull <- tryCatch(is.null(seed), error = function(e) return(FALSE))

  if (isinteger | isnull) {
    return(seed)
  } else {
    warning("The value of 's_setseed' must be an integer. The seed was not set.")
    return(NULL)
  }
}


# Used for validation of any boolean type. There are several so this reduces need for repetition
validate_boolean <- function(var, name, default) {
  islogical <- tryCatch(identical(typeof(var), "logical"),
                        error = function(errmsg) return(FALSE))

  if (islogical) {
    return(var)
  } else {
    warning(paste("The ", name, " argument must be boolean (either TRUE or FALSE). The default value (", default, ") will be used.", sep = ""))
    return(default)
  }
}
