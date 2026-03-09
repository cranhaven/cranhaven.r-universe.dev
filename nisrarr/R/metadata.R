#' Get metadata
#'
#' Retrieve metadata from a dataset, such as contact information, notes, and
#' official statistics status.
#'
#' Every dataset should have the following metadata fields:
#' * contact - contact name, email and phone
#' * copyright - copyright information
#' * experimental - a flag for experimental statistics
#' * note - any notes describing the data
#' * official - a flag for official statistics
#' * subject - a code and label for the subject
#' * updated - date
#'
#' @param x A [nisra_df] object created using [nisra_read_dataset()]
#' @param field The metadata field to read. See details for a list of fields.
#'
#' @return Metadata if the field is found, otherwise `NULL`
#' @export
#'
#' @examples
#' mye <- nisra_read_dataset("MYE01T09")
#' get_metadata(mye)
#' get_metadata_field(mye, "contact")
get_metadata_field <- function(x, field) {
  UseMethod("get_metadata_field")
}

#' @exportS3Method
get_metadata_field.default <- function(x, field) {
  x_type <- obj_type_friendly(x)
  stop(paste("`get_metadata_field` not implemented for", x_type))
}

#' @exportS3Method
get_metadata_field.nisra_df <- function(x, field) {
  attr(x, "meta", exact = TRUE)[[field]]
}

#' @rdname get_metadata_field
#' @export
get_metadata <- function(x) {
  UseMethod("get_metadata")
}

#' @exportS3Method
get_metadata.default <- function(x) {
  x_type <- obj_type_friendly(x)
  stop(paste("`get_metadata` not implemented for", x_type))
}

#' @exportS3Method
get_metadata.nisra_df <- function(x) {
  attr(x, "meta", exact = TRUE)
}
