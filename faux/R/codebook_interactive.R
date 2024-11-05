#' Interactive Codebook
#' 
#' Create a Psych-DS formatted codebook from data by answering questions interactively in the console.
#'
#' @param data The data frame to generate a codebook for
#' @param cb The codebook in list format if already generated
#'
#' @return codebook list
#' @keywords internal
#'
codebook_interactive <- function(data, cb = NULL) {
  if (is.null(cb)) {
    # run codebook function to get best guess ----
    name <- utils::capture.output(match.call()$data)
    cb <- codebook(data, name, return = "list")
  }
  
  types <- c(i = "int", s = "string", f = "float", b = "bool")
  type_q <- "  Is it an integer (i), string (s), float (f), or boolean (b)?"
  
  # check each column ----
  for (i in 1:length(cb$variableMeasured)) {
    v <- cb$variableMeasured[[i]]
    unique_vals <- unique(data[[v$name]])
    if (length(unique_vals) > 5) {
      if (is.numeric(unique_vals)) {
        range <- paste(min(unique_vals), "to", max(unique_vals))
      } else {
        range <- sprintf("e.g., %s", paste(unique_vals[1:5], collapse = ","))
      }
    } else {
      range <- paste(unique_vals, collapse = ", ")
    }
    
    # check type guess ----
    message(sprintf("%s has %d unique values (%s)",
                    v$name, length(unique_vals), range))
    
    t <- readline_check(
      type_q, "grep",
      pattern = "^(i|s|f|b)$",
      warning = "Enter only i, s, f or b",
      ignore.case = TRUE,
      default = substr(v$dataType, 1, 1)
    ) %>% tolower()
    type <- types[t]
    cb$variableMeasured[[i]]$dataType <- type
    
    # description ----
    desc <- readline_check("  Column description",
                           type = "length", min = 1,
                           default = v$description)
    cb$variableMeasured[[i]]$description <- desc
    
    # factor specifics ----
    is_factor <- "n"
    if (type != "float") {
      is_factor <- readline_check(
        "  Is this column a factor? [y/n]",
        "grep", pattern = "^(y|n)$",
        warning = "Enter only y or n",
        ignore.case = TRUE) %>% tolower()
    }
    
    if (is_factor == "n") {
      # only factors can have levels
      cb$variableMeasured[[i]]$levels <- NULL
    } else {
      # check levels in the codebook versus unique vals
      if (!is.null(v$levels)) {
        cb_levels <- names(v$levels)
        # missing from cb levels
        cb_missing <- setdiff(unique_vals, cb_levels)
        if (length(cb_missing) > 0) {
          message("Levels are undefined for: ",
                  paste(cb_missing, collapse = ", "))
        }
        
        for (m in cb_missing) {
          v$levels[m] <- m
        }
      } else {
        # set levels as all unique values
        v$levels <- unique_vals
        names(v$levels) <- unique_vals
      }
      
      lvl_n <- readline_check(
        "  How many levels does it have?", "integer", min = 0,
        default = length(v$levels)
      )
      
      # check level names and descriptions
      if (lvl_n > 20) {
        message("You can't interactively set descriptions for factors with more than 20 levels, but you can set them manually.")
        cb$variableMeasured[[i]]$levels<- v$levels
      } else {
        for (j in 1:lvl_n) {
          if (is.null(v$levels[j])) {
            lvl_name <- readline_check(
              paste("Level", j, "name"), "length", min = 1,
              default = paste0("V", j)
            )
          } else {
            lvl_name <- names(v$levels)[j]
          }
          
          cur_desc <- v$levels[[lvl_name]]
          cur_desc <- ifelse(is.null(cur_desc), lvl_name, cur_desc)
          lvl_desc <- readline_check(
            paste(lvl_name, "description"),
            "length", min = 1, default = cur_desc)
          
          if (lvl_desc == "") {
            # default accepted
            lvl_desc <- cur_desc
          }
          
          cb$variableMeasured[[i]]$levels[[lvl_name]] <- lvl_desc
        }
      }
    }
  }
  
  cb
}

