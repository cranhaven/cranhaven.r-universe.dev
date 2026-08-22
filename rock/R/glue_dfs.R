glue_dfs <- function(x,
                     y,
                     glue = "") {

  return(
    rbind_df_list(
      list(
        x,
        create_glue_df(x, glue=glue),
        y
      )
    )
  );

}
