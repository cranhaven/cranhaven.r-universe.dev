#' @export
#' @rdname export_to_yaml_or_json
form_to_json <- function(x,
                         file = NULL) {

  x <- retrieve_form(x);

  x <-
    list(
      form =
        list(
          instructions = serialize_df(x$instructions, idCol = "heading"),
          metadata = serialize_df(x$metadata, idCol = "field"),
          items = serialize_df(x$items, idCol = "item_id"),
          sections = serialize_df(x$sections, idCol = "section_id"),
          valueTemplates = serialize_df(x$valueTemplates, idCol = "identifier")
        )
    );

  if (is.null(file)) {
    res <-
      jsonlite::toJSON(
        x,
        pretty = TRUE,
        force = TRUE
      );
    class(res) <- c("preregr_json", class(res));
    return(
      res
    );
  } else {
    jsonlite::write_json(
      x,
      file,
      pretty = TRUE,
      force = TRUE
    );
    return(invisible(x));
  }
}

