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


#' Get the scale and offset values of all files referenced by a LAScatalog
#'
#' @returns A data.table with 7 columns. The first three columns hold the x, y,
#'   and z scale factors, the next three columns hold the x, y, and z offsets
#'   and the last column holds the file paths. There is one row for each
#'   referenced file.
#'
#' @keywords internal
#'
#' @export
collect_scale_n_offset_of_LAScatalog_files <- function(LAScatalog) {
  # Get the paths to the files accessed by the LAScatalog
  file_paths <- unique(unlist(lapply(
    X = lidR::engine_chunks(LAScatalog, plot = FALSE),
    FUN = function(chunk) {
      chunk@files
    }
  )))

  # Read the scale and offset data from the file headers and convert them into a
  # table
  return(data.table::rbindlist(lapply(
    X = file_paths,
    FUN = function(file_path) {
      c(
        lidR::readLASheader(file_path)@PHB[c(
          "X scale factor",
          "Y scale factor",
          "Z scale factor",
          "X offset",
          "Y offset",
          "Z offset"
        )],
        file_path = file_path
      )
    }
  )))
}
