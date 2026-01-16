
#' @importFrom rlang sym
#' @importFrom rlang :=
#' @importFrom dplyr ungroup
#' @importFrom testthat expect_equal


#' @name check_previous_events
#' @title Add a column and check any previous events identified for a particular antibiotic
#' @description
#'  This function helps to check any previous events identified or not (TRUE/FALSE)
#' @usage check_previous_events(df,
#'       cols,
#'       sort_by_col,
#'       patient_id_col,
#'       event_indi_value="R",
#'       new_col_prefix="pr_event_",
#'       time_period_in_days,
#'       minimum_prev_events,
#'       default_na_date='9999-12-31 00:00:00')
#' @param df A data frame containing microbiology events
#' @param cols Columns for each antibiotics which contains events
#' @param sort_by_col A date column to order the input data frame
#' @param patient_id_col Patient Id Column
#' @param event_indi_value (optional) Event value indicating Resistance (Default 'R' )
#' @param new_col_prefix (optional) Custom Prefix for new column(Default 'pr_event_' )
#' @param time_period_in_days (optional) to check any  previous events in last 'n' days or not
#' @param minimum_prev_events (optional) to check any 'n' number of previous events happened or not
#' @param default_na_date (optional) replacement date string for NA values in sort_by_col
#'                        eg: '9999-12-31 00:00:00'
#'
#' @return Data Frame
#' @examples
#'
#' #Example -1
#' test_data <- data.frame(subject_id = c(10000032,
#'                                        10000280,
#'                                        10000280,
#'                                        10000280,
#'                                        10000826,
#'                                        10000826),
#'                       chartdate = c('2150-10-12',
#'                                     '2150-10-12',
#'                                     '2151-03-17',
#'                                     '2146-12-08',
#'                                     '2187-09-26',
#'                                     '2188-07-01'),
#'                       AMIKACIN=c('R','R','S','S','S','R'))
#'
#' check_previous_events(test_data,
#'                       cols="AMIKACIN",
#'                       sort_by_col='chartdate',
#'                       patient_id_col='subject_id',
#'                       event_indi_value='R')
#' #Example -2
#'
#' test_data <- data.frame(subject_id=c('10016742', '10016742','10016742',
#'                                       '10016742','10016742','10038332',
#'                                       '10038332','10038332','10038332',
#'                                        '10038332','10038332'),
#'               chartdate= c('2178-07-03','2178-08-01','2178-07-22',
#'                           '2178-08-03','2178-09-25','2164-07-31',
#'                           '2164-12-22','2164-12-22','2165-01-07',
#'                           '2165-04-17','2165-05-05'),
#'                CEFEPIME=c('R','S','R','S','S','R','R','R','S','S','S'),
#'                CEFTAZIDIME=c('S','R','S','R','R','S','S','S','R','R','S'))
#'
#' check_previous_events(test_data,
#'                       cols = c('CEFEPIME','CEFTAZIDIME'),
#'                       sort_by_col = 'chartdate',
#'                       patient_id_col = 'subject_id',
#'                       time_period_in_days = 62,
#'                       minimum_prev_events = 2)
#'
#'

add_events <- function(event_list,event_indi_value){
  results <- {}
  for(event in as.list(event_list)){
    if(event == event_indi_value){
      results <- c(results, TRUE)
    }else{
      results <- c(results, FALSE)
    }
  }
  sum(results)
}

#this function expecting input data is ordered by date
check_events_in_a_period <- function(data, patient_id_col,event_col, event_date_col, time_period_in_days, minimum_prev_events=1,new_col_name, event_indi_value){

  current_id = data[[1,{{patient_id_col}}]]
  results <- c(FALSE)

  start_date_pointer=0

  for (i in 2:nrow(data)){
    if(data[[i, {{patient_id_col}}]] == current_id) {

     if(start_date_pointer == 0)
       start_date_pointer = 1 # use pointer to find range of results that fall within threshold

      while(as.integer(as.numeric(difftime(as.POSIXct(data[[i, {{event_date_col}}]]), as.POSIXct(data[[start_date_pointer, {{event_date_col}}]]), units='days')))  > time_period_in_days){
        start_date_pointer = start_date_pointer+1
      }

      end_date_pointer = i
      while(as.POSIXct(data[[i, {{event_date_col}}]]) == as.POSIXct(data[[(end_date_pointer-1), {{event_date_col}}]])){
        end_date_pointer= end_date_pointer-1
        if(end_date_pointer == 1){
          break
        }
      }

      events = ungroup(data) %>%  select(!!sym(event_col))
      if(i != end_date_pointer){
        event_sum= add_events(as.list(unlist(events[,1]))[start_date_pointer:end_date_pointer-1],event_indi_value) # sum the events within period
      }else{
        event_sum= add_events(as.list(unlist(events[,1]))[start_date_pointer:(i-1)],event_indi_value) # sum the events within period
      }

      # important: note that we use ALL the patient's results to come up with the sum
      if(event_sum >= minimum_prev_events & (i != start_date_pointer) )  {
        results <- c(results,TRUE)
      }else{
        results <- c(results,FALSE)
      }
    }
    if (data[[i, {{patient_id_col}}]] != current_id){
      current_id <- data[[i, {{patient_id_col}}]]
      results <- c(results, FALSE)
      #Reset to new pointer
      start_date_pointer=i
    }
  }
  newdata <- cbind(data, !!new_col_name := results)
  return(newdata)
}


add_prev_event_column <- function(data, col, new_col, event_indi_value, sort_by_col, patient_id_col, time_period_in_days, minimum_prev_events) {

  #Initialization of function variables
  events_mapped <- latest_event_date <- NULL

  #Input Validation
  stopifnot(all.equal(minimum_prev_events, as.integer(minimum_prev_events)))

  stopifnot(all.equal(time_period_in_days, as.integer(time_period_in_days)))

  #initialization
  new_data <- data

  #Calculating Any Previous Event happened or not
  if(time_period_in_days == 0 & minimum_prev_events == 0 ){
    new_data <- data %>%
      mutate(events_mapped := ifelse( (! is.na(dplyr::lead(!!sym(sort_by_col)))) & (dplyr::lead(!!sym(sort_by_col)) == !!sym(sort_by_col)),"NA", !!sym(col) ),
             !!new_col := ifelse(cumsum(ifelse(row_number() == 1,0,
                                               ifelse(dplyr::lag(events_mapped, 1) == sym(event_indi_value), 1, 0))) >= 1, TRUE, FALSE)) %>% select(-c('events_mapped'))

  }else{
    if(time_period_in_days > 0 & minimum_prev_events <= 1 ){
      new_data <- new_data %>%
        mutate(events_mapped := ifelse( (! is.na(dplyr::lead(!!sym(sort_by_col)))) & (dplyr::lead(!!sym(sort_by_col)) == !!sym(sort_by_col)),"NA", !!sym(col) ),
               latest_event_date := as.POSIXct(cummax(ifelse(dplyr::lag(events_mapped, 1)== sym(event_indi_value) & row_number() != 1,
                                                          as.numeric(as.POSIXct(dplyr::lag(!!sym(sort_by_col), 1))),0 ))),
               !!new_col := ifelse(row_number() == 1, FALSE,
                                   ifelse(as.integer(as.numeric(difftime(as.POSIXct(!!sym(sort_by_col)), as.POSIXct(latest_event_date), units='days')))  <= time_period_in_days,
                                          TRUE,
                                          FALSE) )) %>% select(-c('latest_event_date','events_mapped'))
      # new_data <- new_data %>%
      #   check_events_in_a_period(patient_id_col, event_col=col, event_date_col=sort_by_col, time_period_in_days, minimum_prev_events=1, new_col_name = new_col,event_indi_value)

    }else if(time_period_in_days == 0 & minimum_prev_events > 0 ){
      new_data <- new_data %>%
        mutate(events_mapped := ifelse( (! is.na(dplyr::lead(!!sym(sort_by_col)))) & (dplyr::lead(!!sym(sort_by_col)) == !!sym(sort_by_col)),"NA", !!sym(col) ),
               !!new_col := ifelse( cumsum(ifelse(row_number() == 1, 0,
                                                  ifelse(dplyr::lag(events_mapped, 1) == sym(event_indi_value),  1, 0 ))) >= minimum_prev_events,
                                    TRUE,
                                    FALSE)) %>% select(-c('events_mapped'))

    }else if(time_period_in_days > 0 & minimum_prev_events > 1 ){
      new_data <- new_data %>%
        check_events_in_a_period(patient_id_col, event_col=col, event_date_col = sort_by_col, time_period_in_days, minimum_prev_events, new_col_name = new_col,event_indi_value)

    }
  }
  return(new_data)
}

#' @export
check_previous_events<- function(df,cols,sort_by_col,patient_id_col, event_indi_value='R', new_col_prefix="pr_event_",
                                 time_period_in_days=0, minimum_prev_events=0, default_na_date='9999-12-31 00:00:00') {


  df <- df %>%
    arrange(!!sym(sort_by_col), !!sym(patient_id_col)) %>%
    group_by(!!sym(patient_id_col))

  #Bug fix to handle NA values
  stopifnot(!is.na(as.POSIXct(default_na_date,format='%Y-%m-%d %H:%M:%S')) | !is.na(as.POSIXct(default_na_date,format='%Y-%m-%d')) )
  df <- df %>% mutate({{sort_by_col}} := ifelse(is.na(!!sym(sort_by_col)),as.character(default_na_date),as.character(!!sym(sort_by_col))) )

  i=0
  message("Checking Previous Events for ")
  for(col in cols){
    message(col)
    new_col = paste0(new_col_prefix,col)
    i=i+1

    #Bug fix to handle NA values
    df <- df %>% mutate({{col}} := ifelse(is.na(!!sym(col)),'NA',as.character(!!sym(col))) )

    df <- df %>%
      add_prev_event_column({{col}},{{new_col}},event_indi_value, sort_by_col, patient_id_col, time_period_in_days, minimum_prev_events)

  }
  message(paste("Total Antibiotics Column (Events) Added : ",i))

  df <- df %>% dplyr::ungroup()

  return(df)
}
