#' This function generates a tailored exercise plan for T2D (Type 2 Diabetes)
#' @name VisualizeTailoredExercisePlan
#' @title Tailor Exercise Plan for T2D Patients
#' @param input_df A data frame containing patient data necessary for
#' generating a tailored exercise plan. Each column in the dataframe should
#' be as follows (All data must be numeric.):
#' \describe{
#'   \item{Age}{Patient's age in years.}
#'   \item{Sex}{Patient's sex, where 0 might denote female and 1 denotes male.}
#'   \item{BMI}{Body Mass Index, a measure of body fat based on height and weight.}
#'   \item{WHtR}{Patient's ratio of waist and height.}
#'   \item{PCS}{Patient's Physical Component Summary (PCS) score,
#'   a metric derived from the SF-12 (Short Form Health Survey) which measures health-related quality of life.
#'   The PCS score is calculated based on responses to questions in the SF-12 survey,
#'   reflecting physical functioning, bodily pain, general health perceptions, and physical roles.
#'   A higher score indicates better physical health. Users can refer to the \code{PCS_Calculation}
#'   function within this package for detailed calculations based on SF-12 survey responses.}
#'   \item{Duration_T2D (year)}{Duration of Type 2 Diabetes Mellitus in years.
#'   It reflects how long the patient has been living with Type 2 diabetes.}
#'   \item{Total cholesterol (mmol/L)}{Total cholesterol levels in mmol/L.
#'   This measurement includes LDL, HDL, and other lipid components. Higher overall
#'   cholesterol levels can increase the risk of cardiovascular disease, although the
#'   balance of different types of lipids (HDL, LDL, others) also plays a critical role.}
#'   \item{HDL (mmol/L)}{High-Density Lipoprotein cholesterol levels in mmol/L.
#'   HDL is considered the "good" cholesterol, and higher levels are often associated
#'   with a lower risk of heart disease.}
#'   \item{LDL (mmol/L)}{Low-Density Lipoprotein cholesterol levels in mmol/L.
#'   LDL is often referred to as the "bad" cholesterol because high levels can lead to
#'   plaque buildup in arteries and increase the risk of heart disease.}
#'   \item{VO2_Max}{Patient's maximum oxygen uptake.}
#'   \item{Lung_capacity}{Patient's lung volume or capacity.}
#'   \item{BACK_Scratch_Test}{Assessment of the patient's upper limb flexibility and shoulder range of motion.}
#' }
#' Note: It is crucial that the data for each of these columns is correctly formatted
#' and accurately represents the patient's health information for the exercise
#' plan to be effectively tailored.
#'
#' @return A list containing two data frames: `$RecommendedExercisePlan`: This data frame includes exercise plans that
#'          are recommended based on the criterion that the intervention leads to a positive reduction in HbA1c levels,
#'          and `$AllExercisePlan` which includes all received plans.
#' @export
#'
#' @importFrom httr POST
#' @importFrom httr content
#' @importFrom httr add_headers
#' @importFrom jsonlite toJSON
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' #Create a demo dataframe
#' set.seed(5)
#' df <- data.frame(
#'   Age = sample(39:77, 8, replace = TRUE),
#'   Sex = sample(0:1, 8, replace = TRUE),
#'   BMI = sample(18:31, 8, replace = TRUE),
#'   WHtR = sample(0.4:0.6, 8, replace = TRUE),
#'   PCS = sample(27:54, 8, replace = TRUE),
#'   Duration_T2D = sample(1:26, 8, replace = TRUE),
#'   Total_cholesterol = sample(7.4:14.1, 8, replace = TRUE),
#'   HDL = sample(1:1.7, 8, replace = TRUE),
#'   LDL = sample(2.2:4.7, 8, replace = TRUE),
#'   VO2_Max = sample(13:45, 8, replace = TRUE),
#'   Lung_capacity = sample(1900:4600, 8, replace = TRUE),
#'   Back_Scratch_Test = sample(-30:8, 8, replace = TRUE))
#'
#' names(df) <- c('Age', 'Sex', 'BMI', 'WHtR', 'PCS', 'Duration_T2D (year)',
#'                'Total cholesterol (mmol/L)', 'HDL (mmol/L)', 'LDL (mmol/L)',
#'                'VO2_Max (ml/kg/min)', 'Lung_capacity (ml)', 'Back_Scratch_Test (cm)')
#' rownames(df) <- c('Sample1', 'Sample2', 'Sample3', 'Sample4',
#'                   'Sample5', 'Sample6', 'Sample7', 'Sample8')
#'
#' # Run the TailorExercisePlan function
#' demo_result <- TailorExercisePlan(df)
#' # View the structure of the returned list
#' str(demo_result)
#'
library(httr)
library(jsonlite)
TailorExercisePlan <- function(input_df) {

  # define the required columns
  required_columns <- c('Age', 'Sex', 'BMI', 'WHtR', 'PCS', 'Duration_T2D (year)','Total cholesterol (mmol/L)', 'HDL (mmol/L)', 'LDL (mmol/L)', 'VO2_Max (ml/kg/min)', 'Lung_capacity (ml)', 'Back_Scratch_Test (cm)')

  # check the required columns
  if (!all(required_columns %in% names(input_df))) {
    return("The input DataFrame does not contain all the required columns. Please ensure it includes: 'Age', 'Sex', 'BMI', 'WHtR', 'PCS', 'Duration_T2D (year)','Total cholesterol (mmol/L)', 'HDL (mmol/L)', 'LDL (mmol/L)', 'VO2_Max (ml/kg/min)', 'Lung_capacity (ml)', 'Back_Scratch_Test (cm)'")
  }

  if (length(names(input_df)) != length(required_columns)) {
    extra_cols <- setdiff(names(input_df), required_columns)
    return(paste("The input DataFrame contains additional columns:", paste(extra_cols, collapse=', '), ". Please ensure it only includes the required columns: 'Age', 'Sex', 'BMI', 'WHtR', 'PCS', 'Duration_T2D (year)','Total cholesterol (mmol/L)', 'HDL (mmol/L)', 'LDL (mmol/L)', 'VO2_Max (ml/kg/min)', 'Lung_capacity (ml)', 'Back_Scratch_Test (cm)'"))
  }

  # check the number of row
  if (nrow(input_df) > 1000) {
    return("The number of rows in the input DataFrame exceeds the limit (1000 rows).")
  }

  # check if the content of the matrix is the numeric value or not
  if (!all(sapply(input_df, is.numeric))) {
    return("All the matrix data must be numeric!")
  }

  # print the note information
  message("Note: Sample_ID and the rownames of input data correspond on a one-to-one basis.")

  list_data <- list(columns = colnames(input_df), index = rownames(input_df), data = as.matrix(input_df))

  # transform the original data into JSON obeject to require POST
  json_string <- toJSON(list_data, pretty = TRUE, auto_unbox = TRUE)

  response <- POST('http://106.54.44.39:5002/calculate_recommendation',
                   body = json_string,
                   encode = 'json',
                   add_headers(`Content-Type` = 'application/json'))

  # transform the JSON object into original object
  response_content <- content(response, "text", encoding = "UTF-8")

  # use tryCatch to get the potential error
  df_result <- tryCatch({
    fromJSON(response_content)
  }, error = function(e) {
    return("Please check your data format!")
  })

  df_result <- fromJSON(df_result)

  # check the type of df_result
  if (is.list(df_result)) {
    result_df <- data.frame(df_result$data)
    colnames(result_df) <- df_result$columns
    rownames(result_df) <- df_result$index
    result_df <- list(RecommendedExercisePlan = subset(result_df,as.numeric(result_df$`Decreased_HbA1c (%)`)>0),AllExercisePlan = result_df)
    return(result_df)
  } else {
    return("Please check your data format!")
  }
}
