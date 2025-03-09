glue_df_list <- function(x,
                         glue = " ",
                         prefix = TRUE,
                         suffix = TRUE) {

  if (length(x) < 2) {
    if (is.data.frame(x)) {
      return(x);
    } else if (is.list(x)) {
      if (is.data.frame(x[[1]])) {
        return(x[[1]]);
      } else {
        warning("You passed a list that contained one object of class(es) ",
                vecTxtQ(class(x[[1]])), ". Returning that object.");
        return(x[[1]]);
      }
    }
  } else if (length(x) == 2) {
    if (!prefix && !suffix) {
      return(glue_dfs(x[[1]], x[[2]]));
    }
    if (prefix) {
      x <- c(list(create_glue_df(x[[1]])),
             x);
    }
    if (suffix) {
      x <- c(x,
             list(create_glue_df(x[[1]])));
    }
    return(glue_df_list(x, prefix = FALSE, suffix = FALSE));
  } else {

    glueDfs <-
      rep(
        list(
          create_glue_df(x[[1]])
        ),
        length(x)
      );

    ### Use mapply to interleave the lists of data frames
    res <-
      mapply(rbind_dfs, x, glueDfs, SIMPLIFY=FALSE);

    res <-
      rbind_df_list(res);

    if (prefix) {
      res <-
        rbind_dfs(
          create_glue_df(res),
          res
        );
    }

    if (!suffix) {
      res <-
        res[1:(nrow(res)-1), names(res), drop=FALSE];
    }

    return(res);
  }

}
