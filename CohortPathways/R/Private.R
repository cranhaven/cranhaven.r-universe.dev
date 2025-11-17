# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of CohortPathways
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

createIfNotExist <-
  function(type,
           name,
           recursive = TRUE,
           errorMessage = NULL) {
    if (is.null(errorMessage) |
      !inherits(errorMessage, "AssertCollection")) {
      errorMessage <- checkmate::makeAssertCollection()
    }
    if (!is.null(type)) {
      if (length(name) == 0) {
        stop(message("Must specify ", name))
      }
      if (type %in% c("folder")) {
        if (!file.exists(gsub("/$", "", name))) {
          dir.create(
            path = name,
            showWarnings = FALSE,
            recursive = recursive
          )
          message("Created ", type, " at ", name)
        }
      }
      checkmate::assertDirectory(
        x = name,
        access = "x",
        add = errorMessage
      )
    }
    invisible(errorMessage)
  }

extractBitSum <- function(x) {
  lengthVar <- round(log(x = x, base = 2) + 2)

  series <- c(2^(0:lengthVar))

  remainder <- x
  combination <- c()

  while (remainder != 0) {
    component <- match(TRUE, series > remainder) - 1
    remainder <- remainder - series[component]
    combination[length(combination) + 1] <- component - 1
  }
  return(combination)
}
