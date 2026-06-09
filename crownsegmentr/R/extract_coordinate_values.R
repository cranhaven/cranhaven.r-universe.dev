# This file is part of crownsegmentr, an R package for identifying tree crowns
# within 3D point clouds.
#
# Copyright (C) 2025 Leon Steinmeier, Timon Miesner, Nikolai Knapp
# Contact: timon.miesner@thuenen.de
#
# crownsegmentr is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# crownsegmentr is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with crownsegmentr in a file called "COPYING". If not,
# see <http://www.gnu.org/licenses/>.


#' Find all exact matches with at least one of the provided patterns
#'
#' @param patterns Objects which will be matched to `targets` via the `==`
#'   operator.
#' @param targets Objects which will be matched to each of the `patterns`.
#'
#' @returns A boolean vector of the same length as `targets`.
match_any <- function(patterns, targets) {
  assert_that(is.vector(patterns), is.vector(targets))

  matches <- list()

  # match each pattern
  for (pattern in patterns) {
    matches[[length(matches) + 1]] <- targets == pattern
  }

  res_matches <- matches[[1]]

  # combine all matches
  if (length(matches) > 1) {
    for (match in matches[2:length(matches)]) {
      res_matches <- res_matches | match
    }
  }

  return(res_matches)
}

#' Extract coordinate data from a data.frame-like object
#'
#' This function extracts three numeric columns from the input table. If
#' possible, columns which are named x/X, y/Y, or z/Z.
#'
#' @param coordinate_table An object which is valid according to
#'   `validate_coordinate_table()` (i.e. data.frame-like and contains at least
#'   three numeric columns).
#'
#' @returns A [base::data.frame()] with just three columns that are expected to
#'   hold the x-, y-, and z-coordinates in that order.
extract_coordinate_values <- function(coordinate_table) {
  # Define coordinate column names to search for
  xyz_chars <- list(x = c("x", "X"), y = c("y", "Y"), z = c("z", "Z"))

  # Find all numeric columns
  is_numeric_col <- sapply(coordinate_table, is.numeric)

  # Get their names
  numeric_col_names <- names(which(is_numeric_col))

  # Find numeric columns with names matching the search patterns
  xyz_numeric_matches <- lapply(xyz_chars, match_any, targets = numeric_col_names)

  # Get the inversely not-matching columns
  non_xyz_numeric_cols <-
    !xyz_numeric_matches[["x"]] &
      !xyz_numeric_matches[["y"]] &
      !xyz_numeric_matches[["z"]]

  # Variables for the following loop
  num_used_non_xyz_cols <- 0
  xyz_numeric_col_pos <- vector("integer")

  # Look for a matching column for each of the three dimensions and issue
  # warnings when there are none or more than one for any.
  for (coord_dim in c("x", "y", "z")) {
    numeric_match_pos <- which(xyz_numeric_matches[[coord_dim]])

    if (length(numeric_match_pos) == 1) {
      xyz_numeric_col_pos <- append(xyz_numeric_col_pos, numeric_match_pos)
    } else if (length(numeric_match_pos) > 1) {
      xyz_numeric_col_pos <- append(xyz_numeric_col_pos, numeric_match_pos[1])

      warning(paste0(
        "Found more than one numeric column named like it could hold ",
        coord_dim, " coordinates. Using column \"",
        numeric_col_names[numeric_match_pos[1]], "\". Ignoring columns \"",
        paste(
          numeric_col_names[numeric_match_pos[2:length(numeric_match_pos)]],
          sep = ", "
        ),
        "\"."
      ))
    } else {
      num_used_non_xyz_cols <- num_used_non_xyz_cols + 1
      xyz_numeric_col_pos <- append(
        xyz_numeric_col_pos,
        which(non_xyz_numeric_cols)[num_used_non_xyz_cols]
      )

      warning(paste0(
        "Found no numeric column named like it could hold ", coord_dim,
        " coordinates. Using next available numeric column \"",
        numeric_col_names[xyz_numeric_col_pos[length(xyz_numeric_col_pos)]], "\"."
      ))
    }
  }

  # Return the columns which are assumed to hold the coordinate values
  return(as.data.frame(coordinate_table)[is_numeric_col][xyz_numeric_col_pos])
}
