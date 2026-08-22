create_glue_df <- function(x,
                           glue = "",
                           rows = 1) {

  return(
    stats::setNames(
      as.data.frame(
        matrix(
          rep(
            glue,
            rows * ncol(x)
          ),
          ncol = ncol(x)
        )
      ),
      names(x)
    )
  );

}
