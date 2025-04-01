# Internal functions for performing dimension reduction and/or standardization, as part of the clusterability R package.

# Copyright (C) 2020  Zachariah Neville, Naomi Brownstein, Andreas Adolfsson, Margareta Ackerman

# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.


# Compute and return the scores for the first principal component in PCA
performpca <- function(x, center, scale) {
  pcaresult <- stats::prcomp(x, center = center, scale. = scale, retx = TRUE)

  # Because different machines or implementations of PCA can yield
  # differently signed rotation matrices, and thus scores,
  # we multiply all scores by -1 if the first loading is negative.
  # This should ensure consistent results across machines and between SAS and R implementations,
  # assuming the variables are ordered the same.

  if(pcaresult$rotation[1, 1] < 0) {
   return(-1 * pcaresult$x[, 1])
  } else {
    return(pcaresult$x[, 1])
  }
}

# Compute pairwise distances and return a vector of distances
computedistances <- function(x, method) {
  ### Supported by default in dist() function ###
  # "minkowski(p)" = Minkowski metric, p
  # "euclidean" = Euclidean
  # "maximum" = maximum
  # "manhatten" = manhatten
  # "canberra" = canberra
  # "binary" = binary

  ### Custom metrics ###
  # http://documentation.sas.com/?cdcId=pgmsascdc&cdcVersion=9.4_3.3&docsetId=statug&docsetTarget=statug_distance_details01.htm&locale=en
  # "corr" = correlation
  # "sqeuc" = square euclidean
  # "sqcorr" = squared correlation metric
  # "cov" = covariance metric

  # Check if it's supported by built-in dist()
  distmethods <- c("euclidean", "maximum", "manhattan", "canberra", "binary")
  is_dist_default <- method %in% distmethods

  # Check if it's a Minkowski metric
  minkowski_regex <- "minkowski\\([[:digit:]]*[.]?[[:digit:]]*\\)"
  is_minkowski <- grepl(minkowski_regex, method, ignore.case = TRUE)

  if (is_dist_default) {
    distresult <- as.vector(stats::dist(x = x, method = method))
  } else if (is_minkowski) {

    # Strip out the unnecessary parts to get the "p"
    pattern1 <- "minkowski\\("
    pattern2 <- "\\)"

    out1 <- sub(pattern1, "", method)
    minkowski_p <- as.numeric(sub(pattern2, "", out1))

    distresult <- as.vector(stats::dist(x = x, method = "minkowski", p = minkowski_p))

  } else {
    distresult <- switch(method,
           "sqeuc" = dist_sqeuc(x),
           "corr" = dist_corr(x),
           "cov" = dist_cov(x),
           "sqcorr" = dist_sqcorr(x))
  }

  return(distresult)
}

# Standardize a data set and return the standardized data
standardizedata <- function(x, method) {
  # NONE = don't do anything
  # STD = mean 0, stdev 1
  # MEAN = subtract mean
  # MEDIAN = subtract median
  # These match with SAS results
  # http://documentation.sas.com/?cdcId=pgmsascdc&cdcVersion=9.4_3.3&docsetId=statug&docsetTarget=statug_stdize_details01.htm&locale=en

  result <- switch(method,
                   "STD" = scale(x),
                   "NONE" = x,
                   "MEAN" = apply(x, 2, function(x) (x - mean(x))),
                   "MEDIAN" = apply(x, 2, function(x) (x - stats::median(x))))

  if (any(is.nan(result))) {
    warning("NaN values occurred during standardization. One possible cause is that the data contains a variable which is constant. No standardization was performed.")
    return(x)
  } else {
    return(result)
  }
}

# Returns only complete cases from the original data set
getcompletecases <- function(x) {
  ccind <- stats::complete.cases(x)
  return(x[ccind, ])
}

countmiss <- function(x) {
  totalrows <- NROW(x)
  completerows <- NROW(x[stats::complete.cases(x), ])
  return((totalrows - completerows))
}

dist_corr <- function(x) {
  # Matches SAS
  # Validation handled in validate_metric(). Cannot have 1-dimensional data
  numerator <- ( (x - rowMeans(x)) %*% t(x - rowMeans(x)) )
  denominator <- rowSums( (x - rowMeans(x))^2) %*% t(rowSums((x - rowMeans(x))^2) )

  if (any(denominator == 0)) {
    stop("One or more constant observations were found in the data set. When using distance_metric = 'corr', the data must not contain constant observations. ")
  } else {
    final <- numerator / sqrt(denominator)
    # This is a vector
    return(final[lower.tri(final)])
  }
}

dist_cov <- function(x) {
  # Matches SAS
  # Validation handled in validate_metric(). Cannot have df = 0
  # This is a similarity metric, not a distance metric.
  df <- dim(x)[2] - 1
  fullmat <- 1/df * ((x - rowMeans(x)) %*% t(x - rowMeans(x)))
  return(fullmat[lower.tri(fullmat)])
}

dist_sqeuc <- function(x) {
  # Matches SAS
  return(as.vector(stats::dist(x, method = "euclidean"))^2)
}

dist_sqcorr <- function(x) {
  # Matches SAS
  # Validation handled in validate_metric(). Cannot have 1-dimensional data
  return(dist_corr(x)^2)
}

get_lower_triangle <- function(x) {
  # Return the lower triangular portion of a distance matrix
  if(NROW(x) != NCOL(x)){
    stop("When using the 'is_dist_matrix' argument, the 'data' argument must be a square matrix.")
  }
  return(x[lower.tri(x)])
}
