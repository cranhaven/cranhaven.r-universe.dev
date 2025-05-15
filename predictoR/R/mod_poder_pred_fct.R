#Funciones tomadas del paquete STRINGR 

str_interp <- function (string, env = parent.frame()) 
{
  if (!is.character(string)) {
    stop("string argument is not character.", call. = FALSE)
  }
  string <- paste(string, collapse = "")
  matches <- interp_placeholders(string)
  if (matches$indices[1] <= 0) {
    string
  }
  else {
    replacements <- eval_interp_matches(matches$matches, 
                                        env)
    `regmatches<-`(string, list(matches$indices), FALSE, 
                   list(replacements))
  }
}
eval_interp_matches <- function (matches, env) 
{
  expressions <- extract_expressions(matches)
  values <- lapply(expressions, eval, envir = env, enclos = if (is.environment(env)) 
    env
    else environment(env))
  formats <- extract_formats(matches)
  mapply(sprintf, formats, values, SIMPLIFY = FALSE)
}

interp_placeholders <- function (string) 
{
  starts <- gregexpr("\\$(\\[.*?\\])?\\{", string)[[1]]
  if (starts[1] <= 0) 
    return(list(indices = starts))
  parts <- substr(rep(string, length(starts)), start = starts, 
                  stop = c(starts[-1L] - 1L, nchar(string)))
  if (any(!grepl("\\$(\\[.*?\\])?\\{.+\\}", parts))) 
    stop("Invalid template string for interpolation.", call. = FALSE)
  opens <- lapply(strsplit(parts, ""), function(v) which(v == 
                                                           "{"))
  closes <- lapply(strsplit(parts, ""), function(v) which(v == 
                                                            "}"))
  lengths <- mapply(match_brace, opens, closes)
  attr(starts, "match.length") <- lengths
  list(indices = starts, matches = mapply(substr, starts, starts + 
                                            lengths - 1, x = string))
}

extract_expressions <- function (matches) 
{
  parse_text <- function(text) {
    tryCatch(parse(text = text), error = function(e) stop(conditionMessage(e), 
                                                          call. = FALSE))
  }
  strings <- gsub("\\$(\\[.+?\\])?\\{", "", matches)
  lapply(substr(strings, 1L, nchar(strings) - 1), parse_text)
}

extract_formats <- function (matches) 
{
  formats <- gsub("\\$(\\[(.+?)\\])?.*", "\\2", matches)
  paste0("%", ifelse(formats == "", "s", formats))
}

#Funciones tomadas del paquete HTSSIP 
match_brace <- function (opening, closing) 
{
  max_close <- max(closing)
  path <- numeric(max_close)
  path[opening[opening < max_close]] <- 1
  path[closing] <- -1
  cumpath <- cumsum(path)
  min(which(1:max_close > min(which(cumpath == 1)) & cumpath == 
              0))
}
