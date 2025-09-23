#' @docType class
#' @title V2StachUtilities
#' @description The purpose of this class is to provide the helper methods
#'
#' @importFrom R6 R6Class
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom stringr str_locate
#' @export
#' @examples
#'\dontrun{
#' package <- 'Stach data which is converted into RowOrganized Package'
#' tableData <-
#' V2StachUtilities$public_methods$GetMappingObject(package$tables,
#' primaryTableId)$value
#' }

V2StachUtilities <- R6::R6Class(
  "V2StachUtilities",
  public = list(
    #' @description Helper method for getting the value corresponding to the given key from the object.
    #' @param objectMap Mapping object value
    #' @param key Mapping object corresponding key value
    #' @param keyname Mapping object corresponding key name
    #' @return Returns the value corresponding to the given key from the object
    GetMappingObject = function(objectMap, key, keyname = "key") {
      for (obj in objectMap) {
        if (obj[[keyname]] == key) {
          return(obj)
        }
      }
    },

    #' @description Helper method for converting value to string
    #' @param value Value that should be converted to string
    #' @return Returns the value by converting corresponding value to string
    ValueToString = function(value) {
      valueDatatype <- value$toString()
      # Get the index of character '_' from a string value
      charpos <-
        stringr::str_locate(valueDatatype, "_")[, 1]
      # Sub string of a value using above charpos
      valueDatatype <-
        substr(valueDatatype, 1, charpos - 1)
      convertedValue <-
        value$toJSON()
      if (tolower(valueDatatype) == "struct")
      {
        convertedValue <- jsonlite::fromJSON(convertedValue)
        convertedValue <-
          jsonlite::toJSON(convertedValue, auto_unbox = T)
      }
      else
      {
        convertedValue <- jsonlite::fromJSON(convertedValue)
        convertedValue <-
          toString(convertedValue)
      }
      return(convertedValue)
    }
  )
)
