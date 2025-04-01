#' clusterability: a package to perform tests of clusterability
#'
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
#' The \code{\link{clusterabilitytest}} function can test for
#' clusterability of a dataset, and the \code{\link[=print.clusterability]{print}} function
#' to display output in the console. Below we include code to use with the provided example
#' datasets. Please see the \code{clusterabilitytest} function for documentation on
#' available parameters.
#'
#' @examples
#' \donttest{
#' # Normals1
#' data(normals1)
#' normals1 <- normals1[,-3]
#' norm1_dippca <- clusterabilitytest(normals1, "dip")
#' norm1_dipdist <- clusterabilitytest(normals1, "dip",  distance_standardize = "NONE",
#' reduction = "distance")
#' norm1_silvpca <- clusterabilitytest(normals1, "silverman", s_setseed = 123)
#' norm1_silvdist <- clusterabilitytest(normals1, "silverman", distance_standardize = "NONE",
#'  reduction = "distance", s_setseed = 123)
#'
#' print(norm1_dippca)
#' print(norm1_dipdist)
#' print(norm1_silvpca)
#' print(norm1_silvdist)
#'
#'
#' # Normals2
#' data(normals2)
#' normals2 <- normals2[,-3]
#' norm2_dippca <-
#' clusterabilitytest(normals2, "dip")
#' norm2_dipdist <-
#' clusterabilitytest(normals2, "dip", reduction = "distance", distance_standardize = "NONE")
#' norm2_silvpca <- clusterabilitytest(normals2, "silverman", s_setseed = 123)
#' norm2_silvdist <- clusterabilitytest(normals2, "silverman", reduction = "distance",
#' distance_standardize = "NONE", s_setseed = 123)
#'
#' print(norm2_dippca)
#' print(norm2_dipdist)
#' print(norm2_silvpca)
#' print(norm2_silvdist)
#'
#'
#' # Normals3
#' data(normals3)
#' normals3 <- normals3[,-3]
#' norm3_dippca <- clusterabilitytest(normals3, "dip")
#' norm3_dipdist <- clusterabilitytest(normals3, "dip", reduction = "distance",
#' distance_standardize = "NONE")
#' norm3_silvpca <- clusterabilitytest(normals3, "silverman", s_setseed = 123)
#' norm3_silvdist <- clusterabilitytest(normals3, "silverman", reduction = "distance",
#' distance_standardize = "NONE", s_setseed = 123)
#'
#' print(norm3_dippca)
#' print(norm3_dipdist)
#' print(norm3_silvpca)
#' print(norm3_silvdist)
#'
#'
#'
#' # Normals4
#' data(normals4)
#' normals4 <- normals4[,-4]
#' norm4_dippca <- clusterabilitytest(normals4, "dip")
#' norm4_dipdist <- clusterabilitytest(normals4, "dip", reduction = "distance",
#' distance_standardize = "NONE")
#' norm4_silvpca <- clusterabilitytest(normals4, "silverman", s_setseed = 123)
#' norm4_silvdist <- clusterabilitytest(normals4, "silverman", reduction = "distance",
#' distance_standardize = "NONE", s_setseed = 123)
#'
#' print(norm4_dippca)
#' print(norm4_dipdist)
#' print(norm4_silvpca)
#' print(norm4_silvdist)
#'
#'
#'
#'
#' # Normals5
#' data(normals5)
#' normals5 <- normals5[,-4]
#' norm5_dippca <- clusterabilitytest(normals5, "dip")
#' norm5_dipdist <- clusterabilitytest(normals5, "dip", reduction = "distance",
#' distance_standardize = "NONE")
#' norm5_silvpca <- clusterabilitytest(normals5, "silverman", s_setseed = 123)
#' norm5_silvdist <- clusterabilitytest(normals5, "silverman", reduction = "distance",
#' distance_standardize = "NONE", s_setseed = 123)
#'
#' print(norm5_dippca)
#' print(norm5_dipdist)
#' print(norm5_silvpca)
#' print(norm5_silvdist)
#'
#'
#' # iris
#' data(iris)
#' newiris <- iris[,c(1:4)]
#' iris_dippca <- clusterabilitytest(newiris, "dip")
#' iris_dipdist <- clusterabilitytest(newiris, "dip", reduction = "distance",
#' distance_standardize = "NONE")
#' iris_silvpca <- clusterabilitytest(newiris, "silverman", s_setseed = 123)
#' iris_silvdist <- clusterabilitytest(newiris, "silverman", reduction = "distance",
#'  distance_standardize = "NONE", s_setseed = 123)
#'
#' print(iris_dippca)
#' print(iris_dipdist)
#' print(iris_silvpca)
#' print(iris_silvdist)}
#'
#' # cars
#' data(cars)
#'
#' cars_dippca <- clusterabilitytest(cars, "dip")
#' cars_dipdist <- clusterabilitytest(cars, "dip", reduction = "distance",
#' distance_standardize = "NONE")
#' cars_silvpca <- clusterabilitytest(cars, "silverman", s_setseed = 123)
#' cars_silvdist <- clusterabilitytest(cars, "silverman", reduction = "distance",
#' distance_standardize = "NONE", s_setseed = 123)
#'
#' print(cars_dippca)
#' print(cars_dipdist)
#' print(cars_silvpca)
#' print(cars_silvdist)
#'
#'
#' @docType package
#' @name clusterability
NULL
