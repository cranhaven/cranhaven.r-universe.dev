#' Convert a (pre)registration form to an R Markdown template
#'
#' This function creates an R Markdown template from a {preregr}
#' (pre)registrations form specification. Pass it the URL to a Google
#' Sheet holding the (pre)registration form specification (in {preregr}
#' format), see the
#' "[Creating a form from a spreadsheet](https://preregr.opens.science/articles/creating_form_from_spreadsheet.html)"
#' vignette), the path to a file with a spreadsheet holding such a
#' specification, or a loaded or imported {preregr} (pre)registration form.
#'
#' @param x The (pre)registration form (as produced by a call
#' to [preregr::form_create()] or [preregr::import_from_html()]) or
#' initialized `preregr` object (as produced by a call to
#' [preregr::prereg_initialize()] or [preregr::import_from_html()]); or, for
#' the printing method, the R Markdown template produced by a call to
#' [preregr::form_to_rmd_template()].
#' @param title The title to specify in the template's YAML front matter.
#' @param author The author to specify in the template's YAML front matter.
#' @param date The date to specify in the template's YAML front matter.
#' @param output The output format to specify in the template's YAML
#' front matter.
#' @param yaml It is also possible to specify the YAML front matter directly
#' using this argument. If used, it overrides anything specified in `title`,
#' `author`, `date` and `output`.
#' @param includeYAML Whether to include the YAML front matter or omit it.
#' @param file Optionally, a file to save the html to.
#' @param headingLevel The level of the top-most heading to use (the
#' title of the (pre)registration form).
#' @param showSpecification Whether to show the specification in the Rmd
#' output. When `FALSE`, the `preregr` option `silent` is set to `TRUE` at
#' the start of the Rmd template; otherwise, it is set to `FALSE`.
#' @param chunkOpts The chunk options to set for the chunks in the template.
#' @param justify Whether to use [preregr::prereg_specify()] as function for
#' specifying the (pre)registration content (if `FALSE`), or
#' [preregr::prereg_justify()] (if `TRUE`).
#' @param preventOverwriting Set to `FALSE` to override overwrite prevention.
#' @param silent Whether to be silent or chatty.
#' @param ... Additional argument are ignored.
#'
#' @return x, invisibly
#' @export
#' @rdname rmd_templates
#'
#' @examples preregr::form_create(
#'   title = "Example form",
#'   version = "0.1.0"
#' ) |>
#'   preregr::form_to_rmd_template();
form_to_rmd_template <- function(x,
                                 file = NULL,
                                 title = NULL,
                                 author = NULL,
                                 date = '`r format(Sys.time(), \'%Y-%m-%d at %H:%M:%S %Z (UTC%z)\')`',
                                 output = "html_document",
                                 yaml = list(title = title,
                                             author = author,
                                             date = date,
                                             output = output),
                                 includeYAML = TRUE,
                                 chunkOpts = "echo=FALSE, results='hide'",
                                 justify = FALSE,
                                 headingLevel = 2,
                                 showSpecification = FALSE,
                                 preventOverwriting = preregr::opts$get('preventOverwriting'),
                                 silent = preregr::opts$get('silent')) {

  formObjectName <- deparse(substitute(x));

  if (grepl("[^a-zA-Z0-9_]", formObjectName)) {
    formObjectName <- "preregrFormSpec";
  }

  if (inherits(x, "preregr") && inherits(x, "preregr_spec")) {
    initialText <- x$config$initialText;
  } else {
    initialText <- "Unspecified";
  }

  x <- retrieve_form(x);

  if (justify) {
    preregrFunc <- "prereg_justify";
  } else {
    preregrFunc <- "prereg_specify";
  }

  if (is.null(chunkOpts)) {
    chunkOpts <- "";
  } else {
    chunkOpts <- paste0(", ", chunkOpts);
  }

  if (is.null(yaml$title)) {
    yaml$title <- x$metadata$content[x$metadata$field == "title"];
  }

  if (is.null(yaml$author)) {
    yaml$author <- "Specify author here";
  }

  if (includeYAML) {
    res <-
      c("---",
        yaml::as.yaml(yaml),
        "---",
        ""
      );
  } else {
    res <- "";
  }

  ### Add metadata comment
  res <-
    c(
      res,
      htmlComment(
        c(
          "This preregistration template was written by `preregr` at",
          format(Sys.time(), '%Y-%m-%d at %H:%M:%S %Z (UTC%z)')
        )
      ),
      ""
    );

  formSpecificationChunkLabel <-
    paste0("preregr-formSpecification-",
           preregr::randomSlug(6));

  ### Include form specification chunk
  res <- c(
    res,
    paste0("```{r ", formSpecificationChunkLabel, chunkOpts, "}"),
    "```",
    ""
  );

  ### Add setup chunk
  res <- c(
    res,
    paste0("```{r preregr-setup-", preregr::randomSlug(6),
           chunkOpts, "}"),
    "",
    "###-------------------------------------------------------------------",
    "### Setup",
    "###-------------------------------------------------------------------",
    "",
    ifelse(showSpecification,
           "preregr::opts$set(silent = FALSE);",
           "preregr::opts$set(silent = TRUE);"),
    "",
    "preregrObject <-",
    paste0("  preregr::prereg_initialize(", formObjectName, ");"),
    "",
    "```"
  );

  ### Add instructions, if specified in the form

  if (nrow(x$instructions) > 0) {

    res <- c(
      res,
      "",
      "<!------------------------------------------------------------------->",
      "<!--------    GENERAL (PRE)REGISTRATION FORM INSTRUCTIONS    -------->",
      "<!------------------------------------------------------------------->",
      "",
      ""
    );

    for (i in 1:nrow(x$instructions)) {

      res <- c(
        res,
        htmlComment(x$instructions[i, "heading"]),
        ""
      );

      res <- c(
        res,
        htmlComment(x$instructions[i, "description"],
                    padding = " "),
        ""
      );

    }

  }

  ### Empty line for vertical padding
  res <- c(res, "");

  ### Process each section in turn

  for (section in x$sections$section_id) {

    res <- c(
      res,
      htmlComment(
        x$sections[
          x$sections$section_id==section,
          "section_label"
        ]
      ),
      ""
    );

    item_ids <- x$items$item_id[x$items$section_id == section];
    item_labels <- x$items$item_label[x$items$section_id == section];
    item_descriptions <- x$items$item_description[x$items$section_id == section];
    names(item_labels) <- item_ids;
    names(item_descriptions) <- item_ids;

    for (currentItemId in item_ids) {

      if (is.na(item_descriptions[currentItemId])) {
        descriptionBit <- "";
      } else {
        descriptionBit <-
          c("",
            rComment(item_descriptions[currentItemId]),
            "");
      }

      res <- c(
        res,
        paste0("```{r preregr-",
               section, "-", currentItemId, "-",
               preregr::randomSlug(6),
               chunkOpts, "}"),
        "",
        "###-------------------------------------------------------------------",
        rComment(item_labels[currentItemId]),
        "###-------------------------------------------------------------------",
        descriptionBit,
        "preregrObject <-",
        paste0("  preregr::", preregrFunc, "("),
        "    preregrObject,",
        ifelse(
          justify,
          paste0("    ", currentItemId, " = \"",
                 initialText,
                 "\",\n  decision=\"No decision specified\",",
                 "\n  justification=\"No justification specified\""),
          paste0("    ", currentItemId, " = \"",
                 initialText, "\"")
        ),
        ");",
        "",
        "```",
        ""
      );

    }

  }

  ### Show the form

  res <- c(
    res,
    paste0("```{r, preregr-show-form-contents-",
           preregr::randomSlug(6),
           gsub(",? ?results=['\"a-zA-Z0-9_]+", "", chunkOpts),
           "}"),
    "preregr::prereg_knit_item_content(",
    "  preregrObject,",
    paste0("  headingLevel = ", headingLevel),
    ");",
    "```",
    ""
  );

  ### Form specification

  res <- c(
    res,
    "",
    htmlComment(c("Here, the form specification is included. ",
                  "This chunk is executed earlier on though.")),
    "",
    paste0("```{r ",
           formSpecificationChunkLabel,
           ", echo=FALSE, eval=FALSE}"),
    "",
    paste0(formObjectName, " <- "),
    paste0("  ", utils::capture.output(dput(x, control="all"))),
    "",
    "```"
  )

  Encoding(res) <- "UTF-8";

  class(res) <- c("preregr", "preregr_rmd_template", "character");

  if (!is.null(file)) {

    if (dir.exists(dirname(file))) {

      if (file.exists(file) && preventOverwriting) {

        msg("File '", file, "' already exists, and `preventOverwriting` ",
            "is set to TRUE, so I have not written the file to disk.\n",
            silent = silent);

      } else {

        writeLines(res,
                   file);

        msg("Stored the (pre)registration form template in '",
            file, "'.\n",
            silent = silent);

      }

    } else {
      stop("The path that you specified to save the file in ('",
           dirname(file), "') does not exist.");
    }

    return(invisible(res));

  } else {

    return(res);
    #return(knitr::asis_output(res));

  }

}

#' @rdname rmd_templates
#' @export
#' @method print preregr_rmd_template
print.preregr_rmd_template <- function(x, ...) {
  cat(x, sep="\n");
  return(invisible(x));
}

rComment <- function(x,
                     length = 70,
                     commentPrefix = "### ") {

  maxLength <-
    length - nchar(commentPrefix);

  x <- unlist(strsplit(x, "\n", fixed=TRUE));

  x <- unlist(trimws(x, which="right"));

  x <- unlist(strwrap(x, width = maxLength));

  x <-
    paste0(
      commentPrefix,
      x
    );

  return(x);

}

htmlComment <- function(x,
                        padding = "~",
                        length = 70,
                        pre = "<!-- ",
                        after = " ",
                        post = "-->") {

  pad_to <-
    length - nchar(pre) - nchar(after) - nchar(post);

  x <- unlist(strsplit(x, "\n", fixed=TRUE));

  x <- unlist(trimws(x, which="right"));

  x <- unlist(strwrap(x, width = pad_to));

  x <-
    paste0(
      pre,
      x,
      rep(after, length(x)),
      unlist(lapply(nchar(x), function(i) {
        if (i < pad_to) {
          return(paste(rep(padding, pad_to - i), collapse=""));
        } else {
          return(" ");
        }
      })),
      post
    );

  return(x);

}
