#' In an object imported from YAML or JSON, convert some elements to dataframes
#'
#' @param x The just imported (pre)registration specification
#'
#' @return The restructured object
#' @export
serialized_data_to_dfs <- function(x) {

  res <- x;

  ### Convert all into data frames
  res$form <-
    stats::setNames(
      lapply(
        res$form,
        function(dfList) {
          res <-
            rbind_df_list(
              lapply(
                dfList,
                as.data.frame
              )
            );
          if (!is.data.frame(res)) {
            res <- res[[1]];
          }
          return(res);
        }
      ),
      nm = names(res$form)
    );

  ### Also set all columns to character
  res$form <-
    stats::setNames(
      lapply(
        res$form,
        function(currentDf) {
          return(
            data.frame(
              lapply(
                currentDf,
                as.character
              )
            )
          );
        }
      ),
      nm = names(res$form)
    );

  class(res$form) <- c("preregr", "preregr_form", "list");

  return(res);

}
