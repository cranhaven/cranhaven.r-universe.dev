### Not exported, for internal use
knitTable <- function(x,
                      caption = NULL,
                      cols = NULL,
                      booktabs = TRUE,
                      longtable = FALSE,
                      ...) {

  if (is.null(caption)) {
    res <-
      kableExtra::kbl(
        x,
        booktabs = booktabs,
        longtable = longtable,
        ...
      );
  } else {
    res <-
      kableExtra::kbl(
        x,
        caption = caption,
        booktabs = booktabs,
        longtable = longtable,
        ...
      );
  }

  if (!is.null(cols)) {
    for (i in length(cols)) {

      res <-
        kableExtra::column_spec(
          res,
          column = i,
          width = cols[i]
        );

    }
  }

  res <- kableExtra::kable_styling(res);

  return(res);

}
