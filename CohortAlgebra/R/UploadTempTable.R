# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of CohortAlgebra
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


uploadTempTable <- function(connection,
                            data,
                            tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                            bulkLoad = Sys.getenv("DATABASE_CONNECTOR_BULK_UPLOAD"),
                            camelCaseToSnakeCase = TRUE,
                            dropTableIfExists = TRUE) {
  tempTableName <-
    paste0("#", getUniqueString())

  invisible(utils::capture.output(
    DatabaseConnector::insertTable(
      connection = connection,
      tableName = tempTableName,
      dropTableIfExists = dropTableIfExists,
      tempTable = TRUE,
      tempEmulationSchema = tempEmulationSchema,
      data = data,
      camelCaseToSnakeCase = camelCaseToSnakeCase,
      bulkLoad = bulkLoad,
      progressBar = TRUE,
      createTable = TRUE
    ),
    file = nullfile()
  ))

  return(tempTableName)
}

getUniqueString <- function(n = 7) {
  # create a vector of all alphanumeric characters
  alphanumericChars <- c(letters, 0:9)

  # generate the first character from the set of letters only
  firstChar <- sample(c(letters), 1)

  # generate the remaining characters from the set of all alphanumeric characters
  remainingChars <- sample(alphanumericChars, n, replace = TRUE)

  # combine the first character with the remaining characters
  uniqueString <-
    paste0(firstChar, paste0(remainingChars, collapse = ""))

  return(tolower(uniqueString))
}
