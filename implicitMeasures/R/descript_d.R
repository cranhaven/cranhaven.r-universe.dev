#' Descriptive table of either the IAT D-score or the SC-IAT Ds
#'
#' Descriptive statistics for the IAT \emph{D-score} or the SC-IAT \emph{D}.
#'
#' @param data Dataframe with either class \code{dscore} or class
#'              \code{dsciat}.
#' @param latex Logical. If \code{TRUE}, the table for the descriptive statistics
#'               will be printed in latex format. Default is \code{FALSE}.
#'
#' @return Dataframe, containing the mean, s.d., minimum and maximum of the IAT
#'          (\code{D-score}, \code{D-practice}, and \code{D-test}) or the SC-IAT
#'          (\code{D-Sciat}, \code{RT.MappingA}, \code{RT.MappingB}).
#' @export
#' @examples
#' # compute D-score 2 for the IAT data ###
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
#' # calculate D-score
#'   iat_dscore <- compute_iat(iat_data,
#'                           Dscore =  "d2")
#'   descript_d(iat_dscore) # descriptive statistics for the IAT
#'
#'  # calculate D for the SCIAT
#'   data("raw_data") # load data
#' sciat_data <- clean_sciat(raw_data, sbj_id = "Participant",
#'                          block_id = "blockcode",
#'                          latency_id = "latency",
#'                          accuracy_id = "correct",
#'                          block_sciat_1 = c("test.sc_dark.Darkbad",
#'                                            "test.sc_dark.Darkgood"),
#'                          block_sciat_2 = c("test.sc_milk.Milkbad",
#'                                            "test.sc_milk.Milkgood"),
#'                          trial_id  = "trialcode",
#'                          trial_eliminate = c("reminder",
#'                                              "reminder1"))
#'
#'  sciat1 <- sciat_data[[1]] # compute D for the first SC-IAT
#'  d_sciat1 <- compute_sciat(sciat1,
#'                     mappingA = "test.sc_dark.Darkbad",
#'                     mappingB = "test.sc_dark.Darkgood",
#'                     non_response = "alert")
#'   descript_d(d_sciat1,
#'              latex = TRUE) # descriptive statistics for the SC-IAT in latex
#'                           # format

descript_d <- function(data, latex = FALSE){

  # check dataset class --------------------------
  if(is.na(class(data)[2])) {
    stop("data is not an object of class dscore or dsciat")
  } else if (class(data)[2] == "dscore" | class(data)[2] == "dsciat"){
    if (class(data)[2] == "dscore"){
      sel_var <- c(grep("dscore", colnames(data)),
                   grep("d_practice", colnames(data)),
                   grep("d_test", colnames(data)))
      names_table <- c("D-score", "D-practice", "D-test")
    } else if (class(data)[2] == "dsciat"){
      sel_var <- c(grep("d_sciat", colnames(data)),
                   grep("RT_mean.mappingA", colnames(data)),
                   grep("RT_mean.mappingB", colnames(data)))
      names_table <- c("D-Sciat", "RT.MappingA", "RT.MappingB")
    }
    # select variables from the original dataset
    data <- data[ , sel_var]

    # compute descriptive statistics --------------------------
    mean_all <- c(mean(data[,1]), mean(data[,2]), mean(data[,3]))
    sd_all <-  c(sd(data[,1]), sd(data[,2]), sd(data[,3]))
    min_all <-  c(min(data[,1]), min(data[,2]), min(data[,3]))
    max_all <- c(max(data[,1]), max(data[,2]), max(data[,3]))
    table_d <- data.frame(Mean = mean_all,
                          SD = sd_all,
                          Min = min_all,
                          Max = max_all)
    rownames(table_d) <- names_table
    table_d <- round(table_d, 2)

    # latex code --------------------------
    if (latex == TRUE){
      return(xtable::xtable(table_d))
    } else {
      return(table_d)
    }
  } else {stop("data is not an object of class dscore or dsciat")}

}
