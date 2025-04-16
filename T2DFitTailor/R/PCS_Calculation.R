#' This function calculates the Physical Component Summary (PCS) scores
#' based on a set of specific questionnaire items related to physical
#' and mental health domains. It requires a dataframe containing the
#' required columns with valid answer ranges, and outputs a dataframe with
#' PCS scores.
#'
#' @title Calculate PCS (Physical Component Summary) Scores
#'
#' @param df A dataframe containing the questionnaire items with answers.
#' The dataframe should have specific columns:
#' \itemize{
#'   \item \strong{EVGFP_rating}: In general, would you say your health is.
#'   \strong{Reference answers}: 1=Excellent, 2=Very good, 3=Good, 4=Fair, 5=Poor.
#'   \item \strong{Moderate_activities}: Moderate activities, such as moving a table, pushing a vacuum cleaner, bowling, or playing golf.
#'   \strong{Reference answers}: 1=Yes, Limited A Lot, 2=Yes, Limited A Little, 3=No, Not Limited At All.
#'   \item \strong{Climb_several_flights}: Climbing several flights of stairs.
#'   \strong{Reference answers}: 1=Yes, Limited A Lot, 2=Yes, Limited A Little, 3=No, Not Limited At All.
#'   \item \strong{Accomplished_less_physically}: Accomplished less than you would like as a result of your physical health.
#'   \strong{Reference answers}: 1=YES, 2=NO.
#'   \item \strong{Limited_in_kind_physically}: Were limited in the kind of work or other activities as a result of your physical health.
#'   \strong{Reference answers}: 1=YES, 2=NO.
#'   \item \strong{Accomplished_less_mentally}: Accomplished less than you would like as a result of any emotional problems (such as feeling depressed or anxious).
#'   \strong{Reference answers}: 1=YES, 2=NO.
#'   \item \strong{Not_careful_mentally}: Didn't do work or other activities as carefully as usual as a result of any emotional problems (such as feeling depressed or anxious).
#'   \strong{Reference answers}: 1=YES, 2=NO.
#'   \item \strong{Pain_interfere}: During the past 4 weeks, how much did pain interfere with your normal work (including both work outside the home and housework)?
#'   \strong{Reference answers}: 1=Not at all, 2=A little bit, 3=Moderately, 4=Quite a bit, 5=Extremely.
#'   \item \strong{Calm}: During the past 4 weeks, have you felt calm and peaceful?
#'   \strong{Reference answers}: 1=All of the Time, 2=Most of the Time, 3=A Good Bit of the Time, 4=Some of the Time, 5=A Little of the Time, 6=None of the Time.
#'   \item \strong{Energy}: During the past 4 weeks, did you have a lot of energy?
#'   \strong{Reference answers}: 1=All of the Time, 2=Most of the Time, 3=A Good Bit of the Time, 4=Some of the Time, 5=A Little of the Time, 6=None of the Time.
#'   \item \strong{Felt_down}: During the past 4 weeks, have you felt downhearted and blue?
#'   \strong{Reference answers}: 1=All of the Time, 2=Most of the Time, 3=A Good Bit of the Time, 4=Some of the Time, 5=A Little of the Time, 6=None of the Time.
#'   \item \strong{Social_time}: During the past 4 weeks, how much of the time has your physical health or emotional problems interfered with your social activities (like visiting with friends, relatives, etc)?
#'   \strong{Reference answers}: 1=All of the Time, 2=Most of the Time, 3=Some of the Time, 4=A Little of the Time, 5=None of the Time.
#' }
#'
#' @return A dataframe containing the calculated PCS scores.
#' @export
#'
#' @references
#' Tucker G, Adams R, Wilson D. New Australian population scoring coefficients
#' for the old version of the SF-36 and SF-12 health status questionnaires.
#' Qual Life Res. 2010 Sep;19(7):1069-76. doi: 10.1007/s11136-010-9658-9.
#' Epub 2010 May 4. Erratum in: Qual Life Res. 2010 Sep;19(7):1077. PMID: 20440565.
#'
#' @examples
#' set.seed(123)
#'
#' sample_names <- c("xiaoming", "xiaohong", "xiaohua")
#'
#' df <- data.frame(
#'   row.names = sample_names,
#'   EVGFP_rating = sample(1:5, 3, replace = TRUE),
#'   Moderate_activities = sample(1:3, 3, replace = TRUE),
#'   Climb_several_flights = sample(1:3, 3, replace = TRUE),
#'   Accomplished_less_physically = sample(1:2, 3, replace = TRUE),
#'   Limited_in_kind_physically = sample(1:2, 3, replace = TRUE),
#'   Accomplished_less_mentally = sample(1:2, 3, replace = TRUE),
#'   Not_careful_mentally = sample(1:2, 3, replace = TRUE),
#'   Pain_interfere = sample(1:5, 3, replace = TRUE),
#'   Calm = sample(1:6, 3, replace = TRUE),
#'   Energy = sample(1:6, 3, replace = TRUE),
#'   Felt_down = sample(1:6, 3, replace = TRUE),
#'   Social_time = sample(1:5, 3, replace = TRUE)
#' )
#'
#' PCS_output <- PCS_Calculation(df)
#' PCS_output
PCS_Calculation <- function(df) {
  # Check if the input is a dataframe
  if (!is.data.frame(df)) {
    stop("Please input data of type dataframe!")
  }

  # Check if all the data in the dataframe are numeric
  if (!all(sapply(df, is.numeric))) {
    stop("The input dataframe is not numeric!")
  }

  # Define required columns and their valid answer ranges
  required_columns <- c("EVGFP_rating", "Moderate_activities", "Climb_several_flights",
                        "Accomplished_less_physically", "Limited_in_kind_physically",
                        "Accomplished_less_mentally", "Not_careful_mentally", "Pain_interfere",
                        "Calm", "Energy", "Felt_down", "Social_time")

  valid_answers <- list(
    EVGFP_rating = 1:5,
    Moderate_activities = 1:3,
    Climb_several_flights = 1:3,
    Accomplished_less_physically = 1:2,
    Limited_in_kind_physically = 1:2,
    Accomplished_less_mentally = 1:2,
    Not_careful_mentally = 1:2,
    Pain_interfere = 1:5,
    Calm = 1:6,
    Energy = 1:6,
    Felt_down = 1:6,
    Social_time = 1:5
  )

  # Check if all required columns are present
  if (!all(required_columns %in% names(df))) {
    stop("Please provide a dataframe with the required column names formatted correctly!")
  }

  # Ensure the dataframe only includes the required columns
  if (length(names(df)) != length(required_columns)) {
    stop("The number of columns in the dataframe is incorrect, please ensure only the required columns are included!")
  }

  # Check the validity of the data in each column
  for (column in names(df)) {
    if (!all(df[[column]] %in% valid_answers[[column]])) {
      stop(paste("Column", column, "contains invalid answers, please ensure answers are within the specified range!"))
    }
  }

  # Weight mappings
  weights <- list(
    "EVGFP rating" = c(5, 4.4, 3.4, 2, 1),
    "Moderate activities" = c(1, 2, 3),
    "Climb several flights" = c(1, 2, 3),
    "Accomplished less physically" = c(1, 2),
    "Limited in kind physically" = c(1, 2),
    "Accomplished less mentally" = c(1, 2),
    "Not careful mentally" = c(1, 2),
    "Pain-interfere" = c(6, 4.75, 3.5, 2.25, 1),
    "Calm" = c(6, 5, 4, 3, 2, 1),
    "Energy" = c(6, 5, 4, 3, 2, 1),
    "Felt down" = c(1, 2, 3, 4, 5, 6),
    "Social-time" = c(2, 3, 4, 5, 6)
  )

  # PCS values
  pcs_values <- c(0.1031, 0.1166, 0.0383, 0.1516, 0.2833, 0.1832, 0.0569, 0.0129, 0.0193, 0.0569, 0.0281, 0.0763)

  # Replace the values in the dataframe with weights

  df_weighted <- df

  for (questionname in 1:ncol(df_weighted)) {
    for (k in 1:nrow(df_weighted)) {
      df_weighted[k,questionname] <- weights[[questionname]][df_weighted[k,questionname]]
    }
  }

  # Multiply each row by the corresponding PCS values and calculate row sums

  # Expand pcs_values into a matrix with the same number of rows as df_weighted
  pcs_matrix <- matrix(pcs_values, nrow = nrow(df_weighted), ncol = ncol(df_weighted), byrow = TRUE)

  SF12PCS <- rowSums(df_weighted * pcs_matrix)

  # Standardize the row sum results

  standardized_SF12PCS <- 50 + (SF12PCS - 3.4296) * 10 / 0.59965

  # Return a new dataframe containing only the standardized SF12PCS values
  return(data.frame(PCS = standardized_SF12PCS))
}
