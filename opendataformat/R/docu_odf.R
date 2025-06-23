#' @title Get documentation from R data frame.
#'
#' @description
#' Get access to information about the dataset
#' and variables via the R-Studio Viewer or the web browser.
#'
#' @importFrom cli style_hyperlink
#'
#' @param input R data frame (df) or variable from an R data frame (df$var).
#'
#' @param languages Select the language in which the descriptions and labels of
#' the data will be displayed.
#' * By default the language that is set to current is displayed
#' (\code{languages = "current"}).
#' * The default-option chooses either the default language(if labels and
#' * descriptions without a language tag exist)Otherwise the current language
#' * is displayed.
#' (\code{languages = "default"}).
#' * You can choose to view all available language variants by selecting
#' (\code{languages = "all"}),
#' * or you can select the language by language code, e.g.
#' \code{languages = "en"}.
#'
#' @param style Selects where the output should be displayed (console ore
#' viewer).By default the metadata information is displayed in the viewer if the
#' viewer is available.
#' (\code{style = "console"})
#' (\code{style = "print"})
#' * You can choose to display the code in both the console and the viewer
#' (\code{style = "both"})
#' (\code{style = "all"})
#' * You can choose to display the code only in the viewer
#' (\code{style = "viewer"})
#' (\code{style = "html"})
#'
#' @param replace_missing_language If only one language is specified in
#' languages and replace_missing_language is set to TRUE. In case of a
#' missing label or description, the default or english label/description
#' is displayed additionally (if one of these is available).
#'
#' @param variables Indicate whether a list with all the variables should be
#' displayed with the dataset metadata.
#' If the input is a variable/column, the variables-argument will be ignored.
#' Set (\code{variables = "yes"}) to display the list of variables.
#'
#' @return Documentation.
#'
#' @examples
#' # get example data from the opendataformat package
#' df <- get(data("data_odf"))
#'
#' # view documentation about the dataset in the language that is currently set
#' docu_odf(df)
#'
#' # view information from a selected variable in language "en"
#' docu_odf(df$bap87, languages = "en")
#'
#' # view dataset information for all available languages
#' docu_odf(df, languages = "all")
#'
#' # print information to the R console
#' docu_odf(df$bap87, style = "print")
#'
#' # print information to the R viewer
#' docu_odf(df$bap87, style = "viewer")
#'
#' # Since the label for language de is missing, in this case the
#' # english label will be displayed additionally.
#' attributes(df$bap87)["label_de"] <- ""
#' docu_odf(df$bap87, languages = "de", replace_missing_language = TRUE)
#'
#' @export

docu_odf <- function(input,
                     languages = "current",
                     style = "viewer",
                     replace_missing_language = FALSE,
                     variables = "yes") {
  style = tolower(style)
  if (!(style %in% c("both", "all", "console", "viewer","html", "print"))){
    stop("style has to be one of following inputs: both, all, console, viewer, html and print")
  }
  if (!inherits(languages, "character")){
    stop("languages must be type character")
  }
  if (!inherits(replace_missing_language, "logical")){
    stop("replace_missing_language must be TRUE or FALSE")
  }
  if (!(variables %in% c("yes", "Yes", "T", "TRUE", TRUE))){
    stop("variables has to be one of following inputs: 'yes', 'Yes', 'T', 'TRUE', and TRUE")
  }

  if (("data.frame" %in% class(input) && !("odf_tbl" %in% class(input))) ||
      (!("lang" %in% names(attributes(input))) &&
       !("languages" %in% names(attributes(input))))) {
    stop("Input is not a dataframe or variable in the odf-format.")
  }

  #check whether input is dataset or variable
  if ("data.frame" %in% class(input)) {
    input_type <- "Dataset"
  } else {
    input_type <- "Variable"
  }

  #assign languages and currentlanguage
  input_languages <- attr(input, "languages")
  input_lang <- unlist(attr(input, "lang"))

  #Check if languages argument is valid
  if (!(languages %in% c(input_languages, "current", "default", "all"))){
    stop("Your language selection is not valid.")
  }
    


  #if languages is set to default, but no default language exists, the current
  # language is used
  if (languages == "default") {
    if (!("default" %in% input_languages)) {
      languages <- attr(input, "lang")
    }
  }
  #if languages is set to current, the current language is used
  if (languages == "current") {
    languages <- attr(input, "lang")
  }
  #transform "de" to "_de"
  if (languages != "all") languages <- paste0("_", languages)
  #if languages is set to all, all languages of the dataset ate assigned to
  # languages
  if (languages == "all") {
    languages <- input_languages
    languages <- paste0("_", languages)
  }
  #get dataset/variable name (and study name for dataset)
  name <- attr(input, "name")
  if (input_type == "Dataset") {
    study <- attr(input, "study")
    name <- paste0(study, ": ", name)
  }
  #get label and description for every language
  label <- list()
  label_console <- list()
  label_html <- list()
  description <- list()
  for (l in languages){
    label[l] <- attr(input, paste0("label", l))
    description[l] <- attr(input, paste0("description", l))
  }
  label_console <- label
  label_html <- label
  if (length(languages) == 1 && any(c(label[[l]] == "", is.null(label[[l]]))) &&
      replace_missing_language == TRUE) {
    if (!is.null(attr(input, "label_default"))) {
      if (attr(input, "label_default") != "") {
        label_console[languages] <- paste0("\n[default] ",
                                           attr(input, "label_default"))
        label_html[languages] <- paste0("<br>[default] ",
                                        attr(input, "label_default"))
      } else {
        if (!is.null(attr(input, "label_en"))) {
          if (attr(input, "label_en") != "") {
            label_console[languages] <- paste0("\n[en] ",
                                               attr(input, "label_en"))
            label_html[languages] <- paste0("<br>[en] ",
                                            attr(input, "label_en"))
          }
        }
      }
    } else {
      if (!is.null(attr(input, "label_en"))) {
        if (attr(input, "label_en") != "") {
          label_console[languages] <- paste0("\n[en] ", attr(input, "label_en"))
          label_html[languages] <- paste0("<br>[en] ", attr(input, "label_en"))
        }
      }
    }
  }
  description_console <- description
  description_html <- description
  if (length(languages) == 1 && any(c(description[[l]] == "",
                                      is.null(description[[l]])))
      && replace_missing_language == TRUE) {
    if (!is.null(attr(input, "description_default"))) {
      if (attr(input, "description_default") != "") {
        description_console[languages] <- paste0("\n[default] ",
                                                 attr(input,
                                                      "description_default"))
        description_html[languages] <- paste0("<br>[default] ",
                                              attr(input,
                                                   "description_default"))
      } else {
        if (!is.null(attr(input, "description_en"))) {
          if (attr(input, "description_en") != "") {
            description_console[languages] <- paste0("\n[en] ",
                                                     attr(input,
                                                          "description_en"))
            description_html[languages] <- paste0("<br>[en] ",
                                                  attr(input,
                                                       "description_en"))
          }
        }
      }
    } else {
      if (!is.null(attr(input, "description_en"))) {
        if (attr(input, "description_en") != "") {
          description_console[languages] <- paste0("\n[en] ",
                                                   attr(input,
                                                        "description_en"))
          description_html[languages] <- paste0("<br>[en] ",
                                                attr(input,
                                                     "description_en"))
        }
      }
    }
  }
  #get url
  url <- attr(input, "url")
  if (is.null(url)){
    url <- ""
  }
  if (url != "" && exists("style_hyperlink")) {
    interactive_url <- cli::style_hyperlink(
      text = url,
      url = url
    )
  } else {
    interactive_url <- url
  }

  #get value labels for each language
  if (input_type == "Variable") {
    valuelabels_tab <- data.frame()
    type <- attr(input, "type")
    for (l in languages) {
      labels <- attr(input, paste0("labels", l))
      labels_names <- names(attr(input, paste0("labels", l)))
      if (length(valuelabels_tab) == 0) {
        valuelabels_tab <- data.frame(Value = labels, Label = labels_names)
        if (nrow(valuelabels_tab) > 0) {
          colnames(valuelabels_tab)[2] <- sub("_", "", l)
        }
      } else {
        valuelabels_tab_new <- data.frame(Value = labels, Label = labels_names)
        colnames(valuelabels_tab_new)[2] <- sub("_", "", l)
        valuelabels_tab <- merge(valuelabels_tab, valuelabels_tab_new,
                                 by = "Value", all = TRUE)
      }
    }
    valuelabels_html <- paste0("<tr>", paste0("<th>", names(valuelabels_tab),
                                              "</th>", collapse = ""), "</tr>")
    for (i in seq(1, nrow(valuelabels_tab))){
      valuelabels_html <- paste0(valuelabels_html, "<tr>",
                                 paste0("<td>&#160;&#160;&#160;&#160;",
                                        valuelabels_tab[i, ],
                                        "</td>", collapse = ""), "</tr>")
    }
  }

  if (input_type == "Dataset" && variables %in% c("yes", "Yes", "T", "TRUE", TRUE)) {
    labels_vars <- list()
    varlist_html <- paste0("<tr><th>Variables&#160;&#160;&#160;&#160;</th>",
                           paste0("<th align = left>label", languages, "</th>",
                                  collapse = ""), "</tr>")
    var_list <- c()

    for (var in names(input)) {
      var_list <- c(var_list, var)
      labls_var <- c()
      for (lang in languages) {
        if( !is.null(attr(input[[var]], paste0("label", lang)))){
          labls_var <- c(labls_var, attr(input[[var]], paste0("label", lang)))
        } else {
          labls_var <- c(labls_var, "")
        }
        if (!is.null(attr(input[[var]], paste0("label", lang)))){
          labels_vars[[paste0("label", lang)]] <-
          as.character(c(unlist(labels_vars[paste0("label", lang)]),
                         attr(input[[var]], paste0("label", lang))))
        } else {
          labels_vars[[paste0("label", lang)]] <-
            as.character(c(unlist(labels_vars[paste0("label", lang)]),
                           ""))
        }
        
      }
      row_html <- paste0("<tr><td>", var, "</td>",
                         paste0("<td>", labls_var, "</td>",
                                collapse = ""), "</tr>")
      varlist_html <- paste0(varlist_html, row_html)
    }
    varlist <- data.frame("Variable" = var_list)
    for (lang in languages) {
      varlist[, paste0("Label ", gsub("_", "", lang))] <-
        labels_vars[[paste0("label", lang)]]
    }
  }

  #######  format output ######
  #name and url
  printing_output <- c(
    paste0("\033[4m\033[1m", input_type, ":\033[0m\033[0m"),
    paste0("  ", name, "\n")
  )
  html_output <- paste0(
    "<html><head><meta charset = 'utf-8'><style>body {
    word-break: break-word;
    overflow-wrap: break-word;
    }
</style></head><body>",
    "<h3>", input_type, ": ",  name, "</h3>"
  )
  

  #label
  printing_output <- c(
    paste0(printing_output),
    "\033[1mLabel:\033[0m\n"
  )
  html_output <- paste0(
    html_output,
    "<p><b>Label:</b>"
  )
  for (l in languages) {
    printing_output <- c(
      paste0(printing_output),
      "[", gsub("_", "", l), "]", label_console[[l]], "\n"
    )
    html_output <- paste0(
      html_output,
      "<br>[", gsub("_", "", l), "] ", label_html[[l]]
    )
  }
  html_output <- paste0(
    html_output, "</p>"
  )

  #Description
  #label
  printing_output <- c(
    paste0(printing_output),
    "\033[1mDescription:\033[0m\n"
  )
  html_output <- paste0(
    html_output,
    "<p><b>Description:</b>"
  )
  for (l in languages) {
    printing_output <- c(
      paste0(printing_output),
      "[", gsub("_", "", l), "]", description_console[[l]], "\n"
    )
    html_output <- paste0(
      html_output,
      "<br>[", gsub("_", "", l), "] ", description_html[[l]]
    )
  }
  html_output <- paste0(
    html_output, "</p>"
  )

  #Value Labels
  if (input_type == "Variable") {
    #Type
    printing_output <- c(
      paste0(printing_output),
      "\033[1mType:\033[0m\n",
      paste0("    ", type, "\n")
    )
    html_output <- paste0(
      html_output,
      "<p><b>Type:", "</b><br>", type, "</p>"
    )
  }

  #languages
  if (input_type == "Dataset") {
    printing_output <- c(
      paste0(printing_output),
      "\033[1mlanguages:\033[0m\n",
      paste0("    ", paste0(input_languages, collapse = " "), " (active: ",
             input_lang, ")", "\n")
    )
    html_output <- paste0(
      html_output,
      "<p><b>languages:", "</b><br>",
      paste0(paste0(input_languages, collapse = " "),
             " (active: ", input_lang, ")", "</p>")
    )
  }

  #url
  printing_output <- c(
    paste0(printing_output),
    "\033[1mURL:\033[0m\n",
    paste0("    ", interactive_url, "\n")
  )
  html_output <- paste0(
    html_output,
    "<p><b>URL:", "</b><br>", paste0("<a href = '", url, "'>", url, "</a></p>")
  )
  
  if (input_type == "Variable") {
    printing_output <- c(
      paste0(printing_output),
      "\033[1mValue Labels:\033[0m\n"
    )
    html_output <- paste0(
      html_output,
      "<p><b>Value Labels: </b><br>",
      "<table>", valuelabels_html, "</table></p>"
    )
  }



  #add variables information to dataset information
  if (input_type == "Dataset" && variables %in% c("yes", "Yes", "T", "TRUE", TRUE)) {
    printing_output <- c(
      paste0(printing_output),
      "\033[1mVariables:\033[0m\n"
    )
    html_output <- paste0(
      html_output,
      "<p><b>Variables: </b><br>",
      "<table>", varlist_html, "</table></p>"
    )

  }

  html_output <- paste0(html_output, "</html></body>")
  #print meta data in console
  if (style %in% c("both", "all", "print", "console")) {
    for (i in seq(1, length(printing_output))) {
      if (printing_output[i] != "valuelabels") {
        cat(printing_output[i])
      } else {
        print(valuelabels_tab, row.names = FALSE)
      }
    }
    if (input_type == "Variable") {
      if (nrow(valuelabels_tab > 0)) print(valuelabels_tab, row.names = FALSE)
      else print("No value labels")
    }
    if (input_type == "Dataset" && variables == "yes") {
      print(varlist)
    }
  }
  #print meta data in viewer
  if (style %in% c("both", "all", "html", "viewer")) {
    #create html tempfile and write html output
    htmlfile <- file.path(tempdir(), "docu.html")
    viewer <- getOption("viewer")
    writeLines(html_output, htmlfile)
    if (!is.null(viewer)) {
      viewer(htmlfile)
    } else {
      if (style %in% c("html", "viewer")) {
        for (i in seq(1, length(printing_output))) {
          if (printing_output[i] != "valuelabels") {
            cat(printing_output[i])
          } else {
            print(valuelabels_tab, row.names = FALSE)
          }
        }
        if (input_type == "Variable") {
          if (nrow(valuelabels_tab > 0)) print(valuelabels_tab,
                                               row.names = FALSE)
          else print("No value labels")
        }
        if (input_type == "Dataset" && variables == "yes") {
          print(varlist)
        }
      }
    }
  }
}
