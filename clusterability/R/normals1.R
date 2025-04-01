#' Data generated from a single multivariate Normal distribution, 2 dimensions.
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
#'
#' A dataset containing 150 observations generated from a multivariate Normal
#' distribution. The distribution has mean vector (0, 4), each variable has unit
#' variance, and the variables are uncorrelated. This dataset is not clusterable.
#'
#' The cluster variable is 1 for all observations because all were sampled from
#' the same distribution. Remove the variable before using the dataset in any
#' tests.
#'
#' @format A data frame with 150 rows and 3 variables:
#'
#' \describe{
#'    \item{x}{x variable}
#'    \item{y}{y variable}
#'    \item{cluster}{Distribution from which the observation was sampled}
#' }
"normals1"
