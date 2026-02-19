# BSD 3-Clause License
# =====================
#
#   Copyright (c) 2019, Stan Developers and their Assignees
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#   1. Redistributions of source code must retain the above copyright notice, this
# list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
#          SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# Adapted from
# https://github.com/stan-dev/cmdstanr/blob/9f4e8a6b68593863b33a4a75430c2b77c853916a/R/install.R#L345
get_latest_version <- function() {
  dest_file <- base::tempfile(pattern = "releases-", fileext = ".json")

  # Aqui se cambio
  download_url <- "https://api.github.com/repos/RodrigoZepeda/covidmx/releases/latest"

  # Aqui se cambio
  download_rc <- tryCatch(
    utils::download.file(url = download_url, destfile = dest_file, quiet = TRUE),
    warning = function(e) {
      NULL
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(download_rc) || download_rc != 0) {
    NULL # Aqui se cambio
  } else {
    tryCatch(
      {
        release <- strsplit(readLines(dest_file, encoding = "UTF-8", warn = FALSE), ",")
        release <- stringr::str_subset(release[[1]], "tag_name")
        release <- stringr::str_remove_all(release, "tag_name|v|\\\\|:|\"|,|[:space:]")
        release
      },
      error = function(e) {
        NULL
      }
    )
  }
}
