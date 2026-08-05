# Copyright 2024. Jaime Salvador
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this proprietary software and associated documentation files (the "Software"),
# to use, publish, or distribute copies of the Software, and to permit persons to
# whom the Software is furnished to do so.
#
# Any other use, including modifying, adapting, reverse engineering, decompiling,
# or disassembling, is not permitted.
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

#' @title Open dictionary
#' @author Jaime Salvador
#' @description Open a REDATAM database.
#' This function returns an ID than can be used in functions to query data.
#' @param dictionary_name Dictionary filename
#' @return ID than can be used in functions to query data.
#' @export
#' @examples
#' \dontrun{
#' dic<-redatam_open("path/to/rxdb")
#' }
redatam_open <- function(dictionary_name) {
  .Call(`_minired_redatam_open`, dictionary_name)
}

#' @title Save dictionary
#' @author Jaime Salvador
#' @description Save a REDATAM database.
#' This function  can be used to save a dictionary.
#' @param dic Dictionary identifier
#' @param name Dictionary filename
#' @return {No return value.}
#' @export
#' @examples
#' \dontrun{
#' dic<-redatam_open("path/to/rxdb")
#' ...
#' redatam_save(dic,"path/to/new/dictionary.rxdb")
#' }
redatam_save <- function(dic, name = "") {
  invisible(.Call(`_minired_redatam_save`, dic, name))
}

#' @title Close dictionary
#' @author Jaime Salvador
#' @description Close a REDATAM database.
#' @param dic Dictionary ID (returned by 'redatam.open')
#' @return {No return value.}
#' @export
#' @examples
#' \dontrun{
#' dic<-redatam_open("path/to/rxdb")
#' # run some queries using 'query' or 'run'
#' redatam_close(dic);
#' }
redatam_close <- function(dic) {
  invisible(.Call(`_minired_redatam_close`, dic))
}

#' @title List entities
#' @author Jaime Salvador
#' @description List the entities in a databse.
#' @param dic Dictionary ID (returned by 'redatam.open')
#' @return Data frame that contains all the entities in the database.
#' @export
#' @examples
#' \dontrun{
#' dic<-redatam_open("path/to/rxdb")
#' redatam_entities(dic);
#' }
redatam_entities <- function(dic) {
  .Call(`_minired_redatam_entities`, dic)
}

#' @title List variables for entity
#' @author Jaime Salvador
#' @description List the variables in an entity.
#' @param dic Dictionary ID (returned by 'redatam.open')
#' @param entity_name Entity's name
#' @return Data frame that contains all the variables from the \sQuote{entity_name}.
#' @export
#' @examples
#' \dontrun{
#' dic<-redatam_open("path/to/rxdb")
#' redatam_variables(dic, "person")
#' }
redatam_variables <- function(dic, entity_name) {
  .Call(`_minired_redatam_variables`, dic, entity_name)
}

#' @title Get the Redatam API version
#' @author Jaime Salvador
#' @description Returns the current version of the native Redatam API.
#' @return String with the Redatam Engine version.
#' @export
#' @examples
#' redatam_version()
redatam_version <- function() {
  .Call(`_minired_redatam_version`)
}

#----------------------------------------------------------------------

#' @title Execute a Redatam command from text
#' @author Jaime Salvador
#' @description Execute a Redatam command: TABLE or AREALIST.
#' @param dic Dictionary identifier
#' @param spc Program text in SPC format
#' @return Raw dataset with al values: tot, na, mv, values.
#' @export
#' @examples
#' \dontrun{
#' dic<-redatam_open("path/to/rxdb")
#' df<-redatam_internal_query(dic,"freq person.sexo")
#' }
redatam_internal_query <- function(dic, spc) {
  .Call(`_minired_redatam_query`, dic, spc)
}

#' @title Execute a Redatam command from file
#' @author Jaime Salvador
#' @description Execute a Redatam command: TABLE or AREALIST.
#' @param dic Dictionary identifier
#' @param file_name Program file name
#' @return Raw dataset with all values: tot, na, mv, values.
#' @export
#' @examples
#' \dontrun{
#' dic<-redatam.open("rxdb")
#' df<-redatam.internal_run(dic,"/path/to/nmir_test.spc")
#' }
redatam_internal_run <- function(dic, file_name) {
  .Call(`_minired_redatam_run`, dic, file_name)
}





