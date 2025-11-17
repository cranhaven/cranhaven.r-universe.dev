# Copyright 2023 Observational Health Data Sciences and Informatics
#
# This file is part of CohortExplorer
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Copy shiny app files
#'
#' @description
#' Copy shiny app files.
#'
#' @param exportFolder                The folder where the output will be exported to. If this folder
#'                                    does not exist it will be created.
#' @returns                           Nothing is returned to the environment. The function creates the
#'                                    required files for the shiny application in the export folder.
#' @examples
#' \dontrun{
#' exportCohortExplorerAppFiles(
#'   exportFolder = "output"
#' )
#' }
#'
#' @export
exportCohortExplorerAppFiles <- function(exportFolder) {
  filesToCopys <- dplyr::tibble(
    fullPath = list.files(
      path = system.file("shiny", package = utils::packageName()),
      include.dirs = TRUE,
      all.files = TRUE,
      recursive = TRUE,
      full.names = TRUE
    )
  )
  filesToCopys$relativePath <-
    gsub(
      pattern = paste0(system.file("shiny", package = utils::packageName()), "/"),
      replacement = "",
      fixed = TRUE,
      x = filesToCopys$fullPath
    )

  for (i in (1:nrow(filesToCopys))) {
    dir.create(
      path = dirname(file.path(
        exportFolder,
        filesToCopys[i, ]$relativePath
      )),
      showWarnings = FALSE,
      recursive = TRUE
    )
    file.copy(
      from = filesToCopys[i, ]$fullPath,
      to = dirname(file.path(
        exportFolder,
        filesToCopys[i, ]$relativePath
      )),
      overwrite = TRUE,
      recursive = TRUE
    )
  }

  dir.create(
    path = (file.path(
      exportFolder,
      "data"
    )),
    showWarnings = FALSE,
    recursive = TRUE
  )
}
