#' Utility Function for Matching a Closing Brace
#'
#' Given positions of opening and closing braces \code{match_brace} identifies
#' the closing brace matching the first opening brace.
#'
#' @param opening integer: Vector with positions of opening braces.
#' @param closing integer: Vector with positions of closing braces.
#'
#' @return Integer with the posision of the matching brace.
match_brace <- function(opening, closing){
  # maximum index for the matching closing brace
  max_close <- max(closing)

  # "path" for mapping opening and closing breaces
  path <- numeric(max_close)

  # Set openings to 1, and closings to -1
  path[opening[opening < max_close]] <-  1
  path[closing] <- -1

  # Cumulate the path ...
  cumpath <- cumsum(path)

  # ... and the first 0 after the first opening identifies the match.
  min(which(1:max_close > min(which(cumpath == 1)) & cumpath == 0))
}


#' Match Expression Placeholders for String Interpolation
#'
#' Given a character string a set of expression placeholders are matched. They
#' are of the form \code{${...}} or optionally \code{$[f]{...}} where \code{f}
#' is a valid format for \code{\link{sprintf}}.
#'
#' @param string character: The string to be interpolated.
#'
#' @return list containing \code{indices} (regex match data) and \code{matches},
#'   the string representations of matched expressions.
match_placeholders <- function(string){
  # Find starting position of ${} or $[]{} placeholders.
  starts   <- gregexpr("\\$(\\[.*?\\])?\\{", string)[[1]]

  # Break up the string in parts
  parts <- substr(rep(string, length(starts)),
                  start = starts,
                  stop  = c(starts[-1L] - 1L, nchar(string)))

  # For each part, find the opening and closing braces.
  opens  <- lapply(strsplit(parts, ""), function(v) which(v == "{"))
  closes <- lapply(strsplit(parts, ""), function(v) which(v == "}"))

  # Identify the positions within the parts of the matching closing braces.
  # These are the lengths of the placeholder matches.
  lengths <- mapply(match_brace, opens, closes)

  # Update the `starts` match data with the
  attr(starts, "match.length") <- lengths

  # Return both the indices (regex match data) and the actual placeholder
  # matches (as strings.)
  list(indices = starts,
       matches = mapply(substr, starts, starts + lengths - 1, x = string))
}


#' Extract Expression Objects from String Interpolation Matches
#'
#' An interpolation match object will contain both its wrapping \code{${ }} part
#' and possibly a format. This extracts the expression parts and parses them to
#' prepare them for evaluation.
#'
#' @param matches Match data
#'
#' @return list of R expressions
extract_expressions <- function(matches){
  # Parse function for text argument as first argument.
  parse_text <- function(text) parse(text = text)

  # string representation of the expressions (without the possible formats).
  strings  <- gsub("\\$(\\[.+?\\])?\\{", "", matches)

  # Remove the trailing closing brace and parse.
  lapply(substr(strings, 1L, nchar(strings) - 1), parse_text)
}


#' Extract String Interpolation Formats from Matched Placeholders
#'
#' An expression placeholder for string interpolation may optionally contain a
#' format valid for \code{\link{sprintf}}. This function will extract such or
#' default to "s" the format for strings.
#'
#' @param matches Match data
#'
#' @return A character vector of format specifiers.
extract_formats <- function(matches)
{
  # Extract the optional format parts.
  formats <- gsub("\\$(\\[(.+?)\\])?.*", "\\2", matches)

  # Use string options "s" as default when not specified.
  paste0("%", ifelse(formats == "", "s", formats))
}

#' Evaluate String Interpolation Matches
#'
#' The expression part of string interpolation matches are evaluated in a
#' specified environment and formatted for replacement in the original string.
#'
#' @param matches Match data
#'
#' @param env The environment in which to evaluate the expressions.
#'
#' @return A character vector of replacement strings.
evaluate_matches <- function(matches, env){
  # Extract expressions from the matches
  expressions <- extract_expressions(matches)

  # Evaluate them in the given environment
  values <- lapply(expressions, eval, env = env,
                   enclos = if (is.environment(env)) env else environment(env))

  # Find the formats to be used
  formats <- extract_formats(matches)

  # Format the values and return.
  mapply(sprintf, formats, values)
}

#' String Interpolation
#'
#' String interpolation is a useful way of specifying a character string which
#' depends on values in a certain environment. It allows for string creation
#' which is easier to read and write when compared to using e.g.
#' \code{\link{paste}} or \code{\link{sprintf}}. The (template) string can
#' include expression placeholders of the form \code{${expression}} or
#' \code{$[format]{expression}}, where expressions are valid R expressions that
#' can be evaluated in the given environment, and \code{format} is a format
#' specification valid for use with \code{\link{sprintf}}.
#'
#' @param string A template character string.
#' @param env The environment in which to evaluate the expressions.
#'
#' @export
#'
#' @return An interpolated character string.
stringterpolate <- function(string, env = parent.frame()){
  # Find expression placeholders
  matches      <- match_placeholders(string)

  # Evaluate them to get the replacement strings.
  replacements <- evaluate_matches(matches$matches, env)

  # Replace the expressions by their values and return.
  `regmatches<-`(string, list(matches$indices), FALSE, list(replacements))
}


