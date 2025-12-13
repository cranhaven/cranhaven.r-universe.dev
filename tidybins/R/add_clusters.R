#' add_clusters
#'
#' Wraps \code{\link[ClusterR]{KMeans_rcpp}} to create a column that is a cluster formed from select columns in the data frame.
#' Clusters names are specified by capital letters.
#'
#' @param .data dataframe
#' @param ... columns to cluster (tidyselect)
#' @param n_clusters integer
#' @param cluster_name column name
#' @importFrom framecleaner select_otherwise
#'
#' @return data frame
#' @export
#'
#' @examples
#'
#' iris %>%
#' tibble::as_tibble() %>%
#' add_clusters(Sepal.Width, Sepal.Length, n_clusters = 3, cluster_name = "Sepal_Cluster") -> iris1
#'
#' iris1
#'
#' iris1 %>%
#' numeric_summary(original_col = Sepal.Width, bucket_col = Sepal_Cluster)
add_clusters <- function(.data, ..., n_clusters = 4, cluster_name = "cluster"){

  .data %>%
    select_otherwise(..., otherwise = where(is.numeric), return_type = "df") -> df1

  ClusterR::KMeans_rcpp(df1, clusters = n_clusters, num_init = 5) -> c_out

  .data %>%
    dplyr::mutate("{cluster_name}" := factor(LETTERS[c_out$clusters])) -> .data1

  .data1

}

