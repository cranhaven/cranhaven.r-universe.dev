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
  "the any pattern matching works correctly with one pattern",
  {
    expect_equal(match_any("a", targets = "a"), TRUE)
    expect_equal(match_any("a", targets = "b"), FALSE)
    expect_equal(match_any("a", targets = "ab"), FALSE)
    expect_equal(match_any("a", targets = c("ab", "a")), c(F, T))
    expect_equal(match_any("a", targets = c("a", "ab")), c(T, F))
    expect_equal(match_any("a", targets = c("b", "c")), c(F, F))
    expect_equal(match_any("a", targets = c("b", "c", "d")), c(F, F, F))
    expect_equal(match_any("", targets = ""), TRUE)
    expect_equal(match_any("", targets = c("a", "")), c(F, T))
  }
)

test_that(
  "the any pattern matching works correctly with multiple patterns",
  {
    expect_equal(match_any(c("p1", "p2"), targets = ""), FALSE)
    expect_equal(match_any(c("p1", "p2"), targets = "a"), FALSE)
    expect_equal(match_any(c("p1", "p2"), targets = "p1"), TRUE)
    expect_equal(match_any(c("p1", "p2"), targets = "p2"), TRUE)
    expect_equal(match_any(c("p1", "p2"), targets = c("", "p2")), c(F, T))
    expect_equal(match_any(c("p1", "p2"), targets = c("p1", "", "p2")), c(T, F, T))
    expect_equal(match_any(c("p1", "p2"), targets = c("p2", "p2")), c(T, T))
    expect_equal(match_any(c("p1", "p2"), targets = c("a", "b", "")), c(F, F, F))
    expect_equal(match_any(c("p1", "p2", "p3"), targets = c("a", "p3", "")), c(F, T, F))
  }
)

test_that(
  "the any pattern matching works correctly with multiple equal patterns",
  {
    expect_equal(match_any(c("p", "p"), targets = c("p", "")), c(T, F))
  }
)

# TODO expand this test into multiple smaller ones.
test_that(
  "function returns a data.frame with just the x-, y-, and z-coordinates",
  {
    numeric_vector <- c(.0, 1.0, 5, 10.0)

    valid_data_frame_w_normal_colnames <- data.frame(
      x = numeric_vector, Y = numeric_vector, z = numeric_vector
    )
    valid_data_frame_w_normal_colnames_unordered <- data.frame(
      Y = numeric_vector, z = numeric_vector, x = numeric_vector
    )

    expect_identical(
      extract_coordinate_values(valid_data_frame_w_normal_colnames),
      expected = valid_data_frame_w_normal_colnames
    )

    expect_equal(
      extract_coordinate_values(
        valid_data_frame_w_normal_colnames_unordered
      ),
      expected = valid_data_frame_w_normal_colnames
    )

    valid_data_table_w_normal_colnames <- data.table::as.data.table(
      valid_data_frame_w_normal_colnames
    )
    valid_data_table_w_normal_colnames_unordered <- data.table::as.data.table(
      valid_data_frame_w_normal_colnames_unordered
    )

    expect_identical(
      extract_coordinate_values(valid_data_table_w_normal_colnames),
      expected = valid_data_frame_w_normal_colnames
    )

    expect_equal(
      extract_coordinate_values(
        valid_data_table_w_normal_colnames_unordered
      ),
      expected = valid_data_frame_w_normal_colnames
    )
  }
)
