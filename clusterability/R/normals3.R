#' Data generated from a mixture of three multivariate Normal distributions, 2 dimensions.
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
#
#' A dataset containing 150 observations generated from a mixture of three multivariate
#' Normal distributions. 50 observations are from a distribution with mean vector
#' (3, 0), 50 observations from a distribution with mean vector (0, 3), and 50
#' observations from a distribution with mean vector (3, 6). For each of these three
#' distributions, the x and y variables have unit variance and are uncorrelated.
#' The dataset is clusterable.
#'
#' Remove the cluster variable before using the dataset in any tests.
#'
#' @format A data frame with 150 rows and 3 variables:
#'
#' \describe{
#'    \item{x}{x variable}
#'    \item{y}{y variable}
#'    \item{cluster}{Distribution from which the observation was sampled}
#' }
"normals3"
