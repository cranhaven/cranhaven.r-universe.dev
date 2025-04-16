#' This function visualizes the exercise plan recommendations for Type 2 Diabetes (T2D) patients by generating radar charts.
#' @name TailorExercisePlan
#' @title Visualize Tailored Exercise Plan for T2D Patients
#'
#' @param demo_result A list containing two data frames (`$RecommendedExercisePlan` and `$AllExercisePlan`) as returned by the `TailorExercisePlan` function.
#' @param sample_selection Specifies which patient samples to visualize, accepting three types of inputs:
#' \itemize{
#'   \item{"all": Visualizes all samples.}
#'   \item{Given names (character vector): Names of specific patients for visualization.}
#'   \item{Numeric value: A numeric index indicating the number of samples to visualize.}
#' }
#' @param sort_by Determines the sorting orientation for the visualization, affecting which samples are prioritized:
#' \itemize{
#'   \item{"head": Prioritizes samples towards the beginning of the selection.}
#'   \item{"tail": Prioritizes samples towards the end of the selection.}
#' }
#' @param exercise_type Specifies the type(s) of exercises to include in the visualization. Accepts multiple input types:
#' \itemize{
#'   \item{"all": Includes all types of exercises.}
#'   \item{"best": Selects the exercise(s) with the highest predicted reduction in HbA1c.}
#'   \item{Given types (character vector): Specific exercises, including "Taiji", "Qigong", "Stretching", "Rugby", "Cycling", and "Walking".}
#' }
#' @param nrow The number of rows in the visualization grid, which can be:
#' \itemize{
#'   \item{"auto": Automatically calculates the number of rows based on the number of samples and other parameters.}
#'   \item{Numeric value: Specifies the exact number of rows for the layout.}
#' }
#' @param ncol The number of columns in the visualization grid, which can be:
#' \itemize{
#'   \item{"auto": Automatically calculates the number of columns based on the number of samples, the specified number of rows, and other parameters.}
#'   \item{Numeric value: Specifies the exact number of columns for the layout.}
#' }
#' @param show_legend A logical value indicating whether to display a legend in the plots. TRUE or FALSE.
#'
#' @return The function does not return a data structure, but rather invisibly produces radar charts.
#' These charts visualize the expected HbA1c decrease value and other relevant details for each recommended exercise plan for T2D patients.
#' @export
#'
#' @importFrom utils head tail
#' @importFrom stats setNames
#' @importFrom grDevices colors
#' @importFrom graphics par plot.new legend title text
#' @importFrom dplyr mutate across %>%
#' @importFrom fmsb radarchart
#'
#' @examples
#'
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
#'               'Total cholesterol (mmol/L)', 'HDL (mmol/L)', 'LDL (mmol/L)',
#'               'VO2_Max (ml/kg/min)', 'Lung_capacity (ml)', 'Back_Scratch_Test (cm)')
#' rownames(df) <- c('Sample1', 'Sample2', 'Sample3', 'Sample4',
#'                   'Sample5', 'Sample6', 'Sample7', 'Sample8')
#'
#' # Run the TailorExercisePlan function
#' demo_result <- TailorExercisePlan(df)
#'
#' # Visualize the outcome from 'TailorExercisePlan' function
#' VisualizeTailoredExercisePlan(demo_result,sample_selection="all",sort_by="head",
#'                               exercise_type="best",nrow="auto",ncol="auto",show_legend=TRUE)
#'
#' VisualizeTailoredExercisePlan(demo_result,sample_selection="Sample1",sort_by="head",
#'                               exercise_type="all",nrow="auto",ncol="auto",show_legend=TRUE)
#'
library(grDevices)
library(graphics)
library(dplyr)
library(fmsb)
VisualizeTailoredExercisePlan <- function(demo_result, sample_selection="all",
                                          sort_by="head", exercise_type="all",
                                          nrow="auto", ncol="auto", show_legend=TRUE) {

  opar <- par(no.readonly = TRUE)  # save the current user's figure parameters
  on.exit(par(opar), add = TRUE)  # recover the original figure parameters when exit

  # check if the input  has two dataframe object
  if (!is.list(demo_result) || length(demo_result) != 2) stop("Your input dataset is not the list object with two dataframes!")
  if (!all(sapply(demo_result, is.data.frame))) stop("Your input dataset is not the list object with two dataframes!")

  # check the list's content
  if (nrow(demo_result$RecommendedExercisePlan) == 0) {
    stop("In the recommended exercise plans, there are no samples meeting the criteria for subsequent visualization!")
  }

  # give the recommended exercise plan to a new object for visualization
  recommended_exercises <- demo_result$RecommendedExercisePlan
  recommended_exercises$Sample_ID <- factor(recommended_exercises$Sample_ID,levels = unique(recommended_exercises$Sample_ID))
  # sample_selection
  if(length(sample_selection) == 1){
    if (sample_selection != "all") {
      if (is.numeric(sample_selection)) {
        if (length(unique(recommended_exercises$Sample_ID)) < sample_selection) {
          stop("There are no enough samples for subsequent visualization!")
        }
        selected_samples <- unique(recommended_exercises$Sample_ID)
        selected_samples <- if(sort_by == "head") head(selected_samples, sample_selection) else tail(selected_samples, sample_selection)
      } else if (is.character(sample_selection)) {
        selected_samples <- sample_selection
      }

      recommended_exercises <- recommended_exercises[recommended_exercises$Sample_ID %in% selected_samples, ]
    }
  }else{
    if (!is.character(sample_selection)) {
      stop("sample_selection must be character type!")
    }
    selected_samples <- sample_selection
    recommended_exercises <- recommended_exercises[recommended_exercises$Sample_ID %in% selected_samples, ]
  }

  # check there is any sample or not!
  if (nrow(recommended_exercises) == 0) {
    stop("In the recommended exercise plans, there are no samples meeting the criteria for subsequent visualization!")
  }

  # exercise type selection
  if (!is.character(exercise_type)) {
    stop("exercise_type must be character type!")
  }

  if(length(exercise_type) == 1){
    if (exercise_type != "all") {
      if (exercise_type == "best") {
        recommended_exercises <- recommended_exercises[with(recommended_exercises, order(Sample_ID,decreasing = F, `Decreased_HbA1c (%)`)), ]
        recommended_exercises <- recommended_exercises[!duplicated(recommended_exercises$Sample_ID,fromLast = T), ]
      } else {
        recommended_exercises <- recommended_exercises[recommended_exercises$Exercise_type == exercise_type, ]
      }
    }
  }else{
    recommended_exercises <- recommended_exercises[recommended_exercises$Exercise_type %in% exercise_type, ]
  }

  # check there is any sample or not!
  if (nrow(recommended_exercises) == 0) {
    stop("In the recommended exercise plans, there are no samples meeting the criteria for subsequent visualization!")
  }


  # identify the nrows and ncols for further figure plot
  num_samples <- length(unique(recommended_exercises$Sample_ID))

  if(num_samples == 1){
    nrows <- 1
    ncols <- 1

    if(nrow != "auto" || ncol != "auto"){
      warning("The sample number of 1 cann't adjust the parameters of nrow and ncol after selection!")
    }
  }else{

    if(nrow == "auto" && ncol == "auto"){
      if(show_legend == TRUE){
        nrows <- ceiling(sqrt(num_samples+1))
        ncols <- nrows
      }else{
        nrows <- ceiling(sqrt(num_samples))
        ncols <- nrows
      }
    }

    if(is.numeric(nrow) && ncol == "auto"){
      if(show_legend == TRUE){
        nrows <- nrow
        ncols <- ceiling((num_samples+1)/nrow)
      }else{
        nrows <- nrow
        ncols <- ceiling((num_samples)/nrow)
      }
    }

    if(nrow == "auto" && is.numeric(ncol)){
      if(show_legend == TRUE){
        ncols <- ncol
        nrows <- ceiling((num_samples+1)/ncol)
      }else{
        ncols <- ncol
        nrows <- ceiling((num_samples)/ncol)
      }
    }

    if(is.numeric(nrow) && is.numeric(ncol)){
      if(nrow * ncol > num_samples){
        ncols <- ncol
        nrows <- nrow
      }

      if(nrow * ncol == num_samples){
        if(show_legend == TRUE){
          ncols <- ncol
          nrows <- nrow+1
        }else{
          ncols <- ncol
          nrows <- nrow
        }
      }

      if(nrow * ncol < num_samples){
        if(show_legend == TRUE){
          ncols <- ncol
          nrows <- nrow+1
        }else{
          ncols <- ncol
          nrows <- nrow
        }
        # select the target samples
        if(sort_by == "head"){
          recommended_exercises <- subset(recommended_exercises,recommended_exercises$Sample_ID %in% head(unique(recommended_exercises$Sample_ID),nrow * ncol))
        }else{
          recommended_exercises <- subset(recommended_exercises,recommended_exercises$Sample_ID %in% tail(unique(recommended_exercises$Sample_ID),nrow * ncol))
        }
      }

      if(nrow * ncol == 1){# adjust when the final showing number is 1
        ncols <- 1
        nrows <- 1
      }

    }


  }


  num_samples <- length(unique(recommended_exercises$Sample_ID))


  # set the visualization setting
  par(mfrow = c(nrows, ncols))
  par(mar=c(4, 3, 3, 5))

  colors <- c("#E64B35FF","#0571b0", "#fdae61", "#91cf60", "#ca0020", "#7b3294", "#92c5de" ,"#4DBBD5FF")

  names(colors) <- c("A&R","Taiji","Qigong","Stretching","Rugby","Cycling","Int-Walking","Con-Walking")

  df_all <- recommended_exercises

  #set the content as numeric value:"Exercise_type","Decreased_HbA1c (%)", "Recommendation_score", "Duration (w)", "Session (min)", "Frequency (per week)"
  for (k in 3:7) {
    df_all[[k]] <- as.numeric(df_all[[k]])
  }

  # visualize the radar chart according to the above framework
  if(nrows*ncols==1){


    for (current_sample in unique(df_all$Sample_ID)) {

      df_result <- subset(df_all,df_all$Sample_ID==current_sample)

      #df_result <- mutate(df, across(c(`Decreased_HbA1c (%)`, Recommendation_score, `Duration (w)`, `Session (min)`, `Frequency (per week)`), as.numeric))

      df_result <- df_result[c("Exercise_type","Decreased_HbA1c (%)", "Recommendation_score", "Duration (w)", "Session (min)", "Frequency (per week)")]

      rownames(df_result) <- df_result$Exercise_type

      df_result <- df_result[-1]

      # set the scope for each parameter
      radar_limits <- data.frame(rbind(c(4.7, 1, 12, 80, 7),#maximun
                                       c(0, 0, 0, 0, 0)  #minimun
      ))
      colnames(radar_limits) <- c("Decreased_HbA1c (%)", "Recommendation_score", "Duration (w)", "Session (min)", "Frequency (per week)")


      df_result <- rbind(radar_limits,df_result)


      radarchart(df_result,#data
                 pcol=colors[rownames(df_result)[c(-1,-2)]],#polygon：grid color
                 plwd=2,#polygon：grid width
                 plty=2,#polygon：grid type
                 cglcol='black',#grid:color
                 cglty=1,#grid:type
                 axistype=1,#axis type
                 axislabcol='grey',#grid:color
                 cglwd=0.8,#grid:width
                 vlcex=1)#size for group label

      title(main=current_sample, cex.main=2, col.main="black", line=1)

      text(x=0.05*cos(18/180*pi), y=0.05*sin(18/180*pi), labels = 0, pos = 4)
      text(x=0.05*cos(90/180*pi), y=0.05*sin(90/180*pi), labels = 0, pos = 3)
      text(x=0.05*cos(162/180*pi), y=0.05*sin(162/180*pi), labels = 0, pos = 2)
      text(x=0.12*cos(234/180*pi), y=0.12*sin(234/180*pi), labels = 0)
      text(x=0.12*cos(306/180*pi), y=0.12*sin(306/180*pi), labels = 0)

      text(x=0.6*cos(18/180*pi), y=0.6*sin(18/180*pi), labels = 3.5)
      text(x=0.6*cos(90/180*pi), y=0.6*sin(90/180*pi), labels = 2.35)
      text(x=0.6*cos(162/180*pi), y=0.6*sin(162/180*pi), labels = 0.5)
      text(x=0.6*cos(234/180*pi), y=0.6*sin(234/180*pi), labels = 6)
      text(x=0.6*cos(306/180*pi), y=0.6*sin(306/180*pi), labels = 40)

      text(x=1*cos(18/180*pi), y=1*sin(18/180*pi), labels = 7, pos = 4)
      text(x=1*cos(90/180*pi), y=1*sin(90/180*pi), labels = 4.7, pos = 3)
      text(x=1*cos(162/180*pi), y=1*sin(162/180*pi), labels = 1, pos = 2)
      text(x=1*cos(234/180*pi), y=1*sin(234/180*pi), labels = 12, pos = 2)
      text(x=1*cos(306/180*pi), y=1*sin(306/180*pi), labels = 80, pos = 4)


      if(show_legend == TRUE){
        legend(x="topright",inset=c(0,0),  legend = rownames(df_result)[c(-1,-2)],
               bty = "n", pch=20 , col=colors[rownames(df_result)[c(-1,-2)]],
               text.col = "black", cex=1.2, pt.cex=3)
      }

    }
  }else{

    for (current_sample in unique(df_all$Sample_ID)) {

      df_result <- subset(df_all,df_all$Sample_ID==current_sample)

      #df_result <- mutate(df, across(c(`Decreased_HbA1c (%)`, Recommendation_score, `Duration (w)`, `Session (min)`, `Frequency (per week)`), as.numeric))

      df_result <- df_result[c("Exercise_type","Decreased_HbA1c (%)", "Recommendation_score", "Duration (w)", "Session (min)", "Frequency (per week)")]

      rownames(df_result) <- df_result$Exercise_type

      df_result <- df_result[-1]

      colnames(df_result) <- c("Dec","Rec","Dur","Ses","Fre")

      # set the slope for the radar chart
      radar_limits <- data.frame(rbind(c(4.7, 1, 12, 80, 7),#maximum
                                       c(0, 0, 0, 0, 0)#minimum
      ))
      colnames(radar_limits) <- c("Dec","Rec","Dur","Ses","Fre")


      df_result <- rbind(radar_limits,df_result)


      radarchart(df_result,#data
                 pcol=colors[rownames(df_result)[c(-1,-2)]],#polygon：line color
                 plwd=2,#polygon:line width
                 plty=2,#polygon：line type
                 cglcol='black',#grid:color
                 cglty=1,#grid:type
                 axistype=1,#axis type
                 axislabcol='transparent',#grid:axis color
                 cglwd=0.8,#gride:width
                 vlcex=1)#label for group

      title(main=current_sample, cex.main=2, col.main="black", line=1)

      text(x=0.05*cos(18/180*pi), y=0.05*sin(18/180*pi), labels = 0, pos = 4)
      text(x=0.05*cos(90/180*pi), y=0.05*sin(90/180*pi), labels = 0, pos = 3)
      text(x=0.05*cos(162/180*pi), y=0.05*sin(162/180*pi), labels = 0, pos = 2)
      text(x=0.12*cos(234/180*pi), y=0.12*sin(234/180*pi), labels = 0)
      text(x=0.12*cos(306/180*pi), y=0.12*sin(306/180*pi), labels = 0)

      text(x=0.6*cos(18/180*pi), y=0.6*sin(18/180*pi), labels = 3.5)
      text(x=0.6*cos(90/180*pi), y=0.6*sin(90/180*pi), labels = 2.35)
      text(x=0.6*cos(162/180*pi), y=0.6*sin(162/180*pi), labels = 0.5)
      text(x=0.6*cos(234/180*pi), y=0.6*sin(234/180*pi), labels = 6)
      text(x=0.6*cos(306/180*pi), y=0.6*sin(306/180*pi), labels = 40)

    }
    # get a new place for the legend visualization
    if(show_legend == TRUE){
      plot.new()
      legend("center", legend = names(colors),
             bty = "n", pch=20, col=colors,
             text.col = "black", cex=1.2, pt.cex=3)
    }else{

    }


  }
  message("Already finish tailored exercise plan visualization!")
}
