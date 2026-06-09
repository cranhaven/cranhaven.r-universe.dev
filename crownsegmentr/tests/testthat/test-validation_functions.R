# This file is part of crownsegmentr, an R package for identifying tree crowns
# within 3D point clouds.
#
# Copyright (C) 2025 Leon Steinmeier, Nikolai Knapp, UFZ Leipzig
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


test_that(
  "validate_coordinate_table throws error for non-data.frame-like objects",
  {
    numeric_scalar <- 1.0
    numeric_vector <- c(.0, 1.0, 5, 10.0)
    numeric_matrix <- matrix(data = numeric_vector, ncol = 2)

    expect_error(
      validate_coordinate_table(numeric_scalar),
      regexp = paste0(
        "^The coordinate data needs to be stored in a data\\.frame or another ",
        "data type which can be treated as one\\.$"
      )
    )
    expect_error(
      validate_coordinate_table(numeric_vector),
      regexp = paste0(
        "^The coordinate data needs to be stored in a data\\.frame or another ",
        "data type which can be treated as one\\.$"
      )
    )
    expect_error(
      validate_coordinate_table(numeric_matrix),
      regexp = paste0(
        "^The coordinate data needs to be stored in a data\\.frame or another ",
        "data type which can be treated as one\\.$"
      )
    )
  }
)

test_that(paste0(
  "validate_coordinate_table throws error for data.frames with less than three",
  " columns"
), {
  numeric_vector <- c(.0, 1.0, 5, 10.0)
  one_column_data_frame <- data.frame(a = numeric_vector)
  two_column_data_frame <- data.frame(a = numeric_vector, b = numeric_vector)

  expect_error(
    validate_coordinate_table(one_column_data_frame),
    regexp = paste0(
      "^The coordinate table needs to have at least three numeric columns ",
      "for x-, y-, and z-coordinates but there are only 1 numeric columns\\.$"
    )
  )
  expect_error(
    validate_coordinate_table(two_column_data_frame),
    regexp = paste0(
      "^The coordinate table needs to have at least three numeric columns ",
      "for x-, y-, and z-coordinates but there are only 2 numeric columns\\.$"
    )
  )
})
