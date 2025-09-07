#' Compute and plot multiple D-scores
#'
#' Compute and plot multiple \emph{D-score}s.
#'
#' @param data Dataframe of class \code{iat_clean}.
#' @param ds String. Indicates which D-score to compute. \code{built-in} compute
#'         only \emph{D-score} with the built-in error correction (D1 and D2),
#'         \code{error-inflation} compute the \emph{D-score}s without built-in
#'         correction (D3 to D6).
#'
#' @return A list. The first object is a dataframe containing all the computed
#'  \emph{D} scores. The second object is a \code{ggplot} object, depicting the
#'  distribution of the \emph{D} scores through boxplots (also displaying the mean and standard deviation).
#'
#'  @import tidyr
#' @export
#' @examples
#' \donttest{
#'  # Compute multiple IAT D-scores
#'   data("raw_data") # import data
#'   iat_cleandata <- clean_iat(raw_data, sbj_id = "Participant",
#'                           block_id = "blockcode",
#'                           mapA_practice = "practice.iat.Milkbad",
#'                           mapA_test = "test.iat.Milkbad",
#'                           mapB_practice = "practice.iat.Milkgood",
#'                           mapB_test = "test.iat.Milkgood",
#'                           latency_id = "latency",
#'                           accuracy_id = "correct",
#'                           trial_id = "trialcode",
#'                           trial_eliminate = c("reminder", "reminder1"),
#'                           demo_id = "blockcode",
#'                           trial_demo = "demo")
#'   iat_data <- iat_cleandata[[1]]
#'
#'   # compute the mulitple scores and prepare the graphs for the built-in
#'   # strategies
#'
#'   multiple_scores <- multi_dscore(iat_data, ds = "built-in")
#'
#'   data_multiple <- multiple_scores$dscores # store the D-score in a dataframe
#'
#'   # plot the results
#'   multiple_scores$graph
#'   }
multi_dscore <- function(data, ds = c("built-in", "error-inflation")){
  # check class --------------------------
  if (is.na(class(data)[2]) | class(data)[2] != "iat_clean"){
    stop('use the clean_iat function to prepare the dataset for the multi_dscore
         function')
  }
  ds <- match.arg(ds)
  # prepare d labels
  if (ds == "built-in") {
    label_d <- c(paste0("d", 1:2))
    type <- "D Built-in"
  }
  else if (ds == "error-inflation") {
    label_d <-  c(paste0("d", 3:6))
    type <- "D Error Inflation"
  }

  # initialize storing object for multiple Ds
  scores <- list()
  for(i in 1:length(label_d)) {
    scores[[i]] <- compute_iat(data, Dscore = label_d[i])
  }
  dscores <- data.frame(participant = scores[[1]]$participant)
  for (i in 1:length(scores)){
    name_col <- gsub("d", "dscore_d", label_d)
    dscores[, name_col[i]] <- scores[[i]][, name_col[[i]]]
  }
  data <- tidyr::gather(dscores, key = "type", value = "Dscore",
                              2:max(ncol(dscores)))
  mg <- ggplot(data,
               aes(y = .data$Dscore, x = .data$type)) +
        geom_boxplot() +
        stat_summary(fun.data=mean_sdl,
                     geom="pointrange",
                    color="firebrick") + theme_light()
  mg <- mg + labs(x = type, y = "D scores")
  mulitple_dscores <- list(dscores = dscores,
                           graph = mg)
  return(mulitple_dscores)
}
