# Extracts the next field (pattern) in a character vector 
.log_extract_next <- function(pattern, text, ...){
  # Pattern single string, text character vector
  if(!is.character(pattern))
    stop("Argument 'pattern' must be a character vector of length 1")
  if(length(pattern) != 1)
    stop("Argument 'pattern' must be a character vector of length 1")
  if(!is.character(text))
    stop("Argument 'text' must be a character vector")
  
  # Where does the regex match, and for how long
  matches <- regexpr(pattern, text, perl=TRUE, ...)
  match_lengths <- attr(matches, 'match.length')
  
  # If any of the records in 'text' don't match, raise a warning
  for(t in text[which(matches < 0)]){
    warning("Pattern ", pattern, " not found in text ", t)
  }
  
  # Return a formatted list of the extracted fields, and the remaining text
  list(
    formatter(pattern)(substr(text, matches, matches + match_lengths - 1)),
    substr(text, matches + match_lengths, nchar(text))
  )
}

# Convert a template string into a vector of regular expressions
.parse_template <- function(template, classes = list()){
  # Need a character template, and a named list of parser classes
  if(!is.character(template))
    stop("Argument 'template' must be a character vector")
  if((!is.list(classes) | is.null(names(classes))) & length(list) > 1)
    stop("Argument 'classes' must be a named list")
  # Convert the passed clases to parser objects
  classes <- c(lapply(classes, parser), default_classes())
  # Keep the provided parser classes, and any of the default classes that
  # haven't been overwritten
  classes <- classes[unique(names(classes))]
  
  # Escape \{ \} with the html escapes &#123; &125;
  template <- gsub('\\{', '&#123;', template, fixed = TRUE)
  template <- gsub('\\}', '&#125;', template, fixed = TRUE)
  
  # Split on double curly brackets
  template <- strsplit(template, '\\{(?=\\{)|(?<=\\})\\}', perl = T)
  
  # Loop over the split set of expressions
  lapply(template, function(fields){
    expressions <- lapply(fields, function(field){
      # If the expression is surrounded by curly brackets, then make a parser
      # from the regex defined in classes
      if(grepl("^\\{.*\\}$", field)){
        # Strip off curly brackets
        class = gsub('^\\{\\s*|\\s*\\}$', '', field, perl=TRUE)
        # Templates look like {{ class fieldName }}
        class = strsplit(class, '[ ]')[[1]][1]
        # The class for each parser must be defined in the list of classes
        if(! class %in% names(classes))
          stop("Class '", class, "' not defined.")
        # Turn it into a parser!
        parser(classes[[class]])
      }else{
        # Convert the html-escaped curly brackets into real curly brackets
        field <- gsub('&#123;', '{', field, fixed = TRUE)
        field <- gsub('&#125;', '}', field, fixed = TRUE)
        # Turn it into a parser! (But where the regex is a literal string)
        parser(sprintf('\\Q%s\\E', field))
      }
    })
    # Get the names for field
    names <- sapply(fields, function(field){
      # If the expression is surrounded by curly brackets, then extract the name,
      # otherwise make the name <NA>
      if(grepl("^\\{.*\\}$", field)){
        # Strip off the curly brackets
        name <- gsub('^\\{\\s*|\\s*\\}$', '', field, perl=TRUE)
        # Templates look like {{ class fieldName }}
        name <- strsplit(name, '[ ]')[[1]][2]
        # Raise an error if no field name is provided
        if(is.na(name))
          stop("Field {", field, "} must be named")
        name
      }else{
        # If the field is not surrouded by curly braces, then it is just filler
        # text, and has a name of <NA> since it will be excluded from final output.
        NA
      }
    })
    # Name the expressions the extracted names
    names(expressions) <- names
    expressions
  })
}

#' Parse Log Files
#'
#' Parse a log file with a provided template and a set of classes
#'
#' `\code{template} should only be a template string, such as
#'  '{{ip ip_address}} [{{date access_date}}]...'.
#'
#'  \code{config_file} should be a yaml file or connection with the following fields
#'  \itemize{
#'      \item template: Template String
#'      \item classes: Named list of regex strings for building classes
#'  }
#'
#'  \code{text} should be a character vector, with each element representing a
#'              a log record
#'
#'  \code{text_file} should be a file or connection that can be split (with readLines)
#'                  into a character vector of records
#'
#'  \code{classes} should be a named list of parser objects, where names
#'                 match names of classes in template string, or a similarly
#'                 named list of regex strings for coercing into parsers
#'
#'  \code{formatters} should be a named list of functions, where names
#'                 match names of classes in template string, for properly
#'                 formatting fields once they have been captured
#'
#' @param template Template string
#' @param text Character vector; each element a log record
#' @param classes A named list of parsers or regex strings for use within the
#'                template string
#' @param text_file Filename (or readable connection) containing log text
#' @param config_file Filename (or readable connection) containing template file
#' @param formatters Named list of formatter functions for use of formatting \code{classes}
#' @param ... Other arguments passed onto \code{regexpr} for matching regular expressions.
#'
#' @examples
#' # Template string with two fields
#' template <- '{{ip ipAddress}} - [{{date accessDate}}] {{int status }}'
#'
#' # Two simple log records
#' logs <- c(
#'   '192.168.1.10 - [26/Jul/2019:11:41:10 -0500] 200',
#'   '192.168.1.11 - [26/Jul/2019:11:41:21 -0500] 404'
#' )
#'
#' # A formatter for the date field
#' myFormatters <- list(date = function(x) lubridate::as_datetime(x, format = '%d/%b/%Y:%H:%M:%S %z'))
#' # A parser class for the date field
#' date_parser <- parser(
#'   '[0-3][0-9]\\/[A-Z][a-z]{2}\\/[0-9]{4}:[0-9]{2}:[0-9]{2}:[0-9]{2}[ ][\\+|\\-][0-9]{4}',
#'   myFormatters$date,
#'   'date'
#' )
#'
#' # Parse the logs from raw data
#' parse_logs(logs, template, list(date=date_parser))
#'
#' # Write the logs and to file and parse
#' logfile <- tempfile()
#' templatefile <- tempfile()
#' writeLines(logs, logfile)
#' yaml::write_yaml(list(template=template, classes=list(date=date_parser)), templatefile)
#' parse_logs_file(logfile, templatefile, myFormatters)
#' file.remove(logfile)
#' file.remove(templatefile)
#' 
#' @return A data.frame with each field identified in the template string as a column.
#'         For each record in the passed text, the fields were extracted and formatted
#'         using the parser objects in \code{default_classes()} and \code{classes}.
#'
#' @export
parse_logs <- function(text, template, classes = list(), ...){
  # .parse_template returns a list, so a vector of template strings can be passed.
  # For parse_logs we only want a template string of length 1 to be passed
  if(!is.character(template) | length(template) != 1)
    stop('Argument templates should be a character vector of length 1')
  template <- .parse_template(template, classes)[[1]]
  
  # Initialize empty list to store parsed output
  parsed <- list()
  
  # After a template is parsed, the elements with non-NA names are the fields
  # we are extracting, and the elements with NA names are simply the expressions
  # that sit between those fields (usually things like brackets and space)
  # Keep looping through looking for the next named field, and build a regex
  # string to find and extract it
  while(any(!is.na(names(template)))){
    # Strings to be looked for behind and ahead of our field
    lookbehind <- '^'
    lookahead  <- ''
    
    # Find the next named field. If our template isn't malformed, it should either
    # be the first or second element in the list. If it is the second, the first
    # element will be unnamed, and will be a literal match like \\Q - [ \\E.
    next_field <- which(!is.na(names(template)))[[1]]
    
    # Use everything up until our field in question as a lookbehind. If the field
    # after ours has name NA, use it as a lookahead
    if(next_field > 1)
      lookbehind <- sprintf("(?<=^%s)", paste0(unlist(template[1:(next_field-1)]), collapse=''))
    if(is.na(names(template)[next_field+1]) & next_field < length(template))
      lookahead <- sprintf("(?=%s)", template[[next_field+1]])
    
    # Tack on the lookahead, expression, and lookbehind and make a new parser
    # object, using the same formatter as before
    expression <- sprintf('%s%s%s', lookbehind, template[[next_field]], lookahead)
    expression <- parser(expression, formatter(template[[next_field]]))
    
    # Use our new parser to extract the next field. Remove everything up through
    # the extracted field from the text and template. Repeat
    extracted <- .log_extract_next(expression, text, ...)
    parsed[[names(template)[next_field]]] <- extracted[[1]]
    text <- extracted[[2]]
    template <- template[-c(1:next_field)]
  }
  # Our output is a named list of lists. Make it a dataframe.
  as.data.frame(parsed, stringsAsFactors = FALSE)
}

#' @rdname parse_logs
#' @importFrom yaml yaml.load_file
#' @export
parse_logs_file <- function(text_file, config_file, formatters = list(), ...){
  # Simply a wrapper for parse_logs. Read in the text from the connection
  # textFile, and read in the template from configFile. Any custom classes can
  # be defined in the configFile, with the matching formatters passed in the
  # named list formatters
  template <- yaml::yaml.load_file(config_file)
  text <- readLines(text_file)
  parse_logs(text, template$template, parser(template$classes, formatters), ...)
}
