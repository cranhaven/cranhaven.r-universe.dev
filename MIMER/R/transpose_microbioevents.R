
#' @importFrom stats as.formula
#' @importFrom dplyr group_by_at
#' @importFrom reshape2 dcast

#' @name transpose_microbioevents
#' @title Transpose microbiology events dataset
#' @description
#'  This function helps to transpose (rows to columns) microbiology events.
#' @usage  transpose_microbioevents(raw_df,
#'                                  key_columns,
#'                                  required_columns,
#'                                  transpose_key_column,
#'                                  transpose_value_column,
#'                                  fill="NA",
#'                                  non_empty_filter_column,
#'                                  remove_duplicates=TRUE)
#' @param raw_df A data frame containing microbiology events
#' @param key_columns (Optional) Primary Key/ Key columns for duplicate check
#'                   : Default Value = c('subject_id','micro_specimen_id','isolate_num',
#'                                                            'org_name','ab_itemid')
#' @param required_columns (Optional) columns should contain in final dataset
#'                : Default Value c('subject_id','hadm_id','micro_specimen_id',
#'                                  'order_provider_id', 'chartdate','charttime',
#'                                  'spec_itemid','spec_type_desc','storedate',
#'                                  'storetime','test_itemid','test_name',
#'                                   'org_itemid','isolate_num','org_name')
#' @param transpose_key_column (Optional) The column that should be transposed
#' (                        - distinct values of that column will become separate columns)
#'                          :Default 'ab_name'
#' @param transpose_value_column (optional) Values of 'transpose_key_column' column
#'                          :Default 'interpretation'
#' @param fill (optional) Fill character for empty columns- Default : "NA"
#' @param non_empty_filter_column (optional) Filter input dataframe
#'                                where 'non_empty_filter_column' is not empty
#'                                or na. Default :'ab_itemid'
#' @param remove_duplicates (optional) Default :TRUE
#'
#' @examples
#' test_data <- data.frame(subject_id=c('10016742','10016742','10016742',
#'                                      '10016742','10016742','10038332',
#'                                      '10038332','10038332','10038332',
#'                                      '10038332','10038332'),
#'                        chartdate= c('2178-07-03','2178-08-01','2178-08-01',
#'                                     '2178-08-01','2178-09-25','2164-07-31',
#'                                     '2164-12-22','2164-12-22','2165-01-07',
#'                                     '2165-04-17','2165-05-05'),
#'                        ab_name=c('CEFEPIME','CEFTAZIDIME','CEFEPIME',
#'                                 'CEFEPIME','CEFTAZIDIME','CEFTAZIDIME',
#'                                 'CEFEPIME','CEFEPIME','CEFTAZIDIME',
#'                                 'CEFTAZIDIME','CEFEPIME'),
#'                        interpretation=c('S','R','S','R','R','S','S','S','R','R','S'))
#'
#' transpose_microbioevents(test_data,
#'                          key_columns = c('subject_id','chartdate','ab_name'),
#'                          required_columns = c('subject_id','chartdate'),
#'                          transpose_key_column = 'ab_name',
#'                          transpose_value_column = 'interpretation',
#'                          fill = "N/A",
#'                          non_empty_filter_column = 'subject_id')

#' @return Data Frame

## #' @export
duplicated_microbioevents_records <- function(df, key_columns= c('subject_id','micro_specimen_id','isolate_num',
                                                                 'org_name','ab_itemid','test_name','test_seq')){
   n <- NULL
   df %>%
    group_by_at(key_columns) %>%
    mutate(n := row_number()) %>%
    filter(n > 1)
}

remove_duplicates <- function(df, duplicated_df, key_columns){
   dplyr::anti_join(df, duplicated_df, by =key_columns)
}

#' @export
transpose_microbioevents <- function(raw_df, key_columns= c('subject_id','micro_specimen_id','isolate_num','org_name','ab_itemid'),
                                     required_columns = c('subject_id','hadm_id','micro_specimen_id','order_provider_id','chartdate','charttime','spec_itemid',
                                                        'spec_type_desc','storedate','storetime','test_itemid','test_name','test_seq', 'org_itemid','isolate_num','org_name'),
                                     transpose_key_column='ab_name', transpose_value_column='interpretation', fill = "NA", non_empty_filter_column='ab_itemid', remove_duplicates=TRUE){


  if(any(nchar(non_empty_filter_column) > 0)){
    raw_df <- raw_df %>% filter(!is.na(!!sym(non_empty_filter_column)))
  }

  df_without_bad_records=raw_df
  if(remove_duplicates){
    df_bad_records <- duplicated_microbioevents_records(raw_df, key_columns)
    df_without_bad_records <- remove_duplicates(raw_df, df_bad_records, key_columns)
  }

  stopifnot(nrow(df_without_bad_records) > 0)

  required_columns = paste(required_columns, collapse = "+")

  reshape2::dcast(df_without_bad_records, formula = as.formula(paste(required_columns, "~", transpose_key_column)), value.var = transpose_value_column ,fill = fill)

}

## #' @export
transpose_data <-function(raw_df, key_columns, required_columns, transpose_key_column, transpose_value_column, fill = "NA", non_empty_filter_column=""){
  transpose_microbioevents(raw_df, key_columns, required_columns, transpose_key_column, transpose_value_column, fill, non_empty_filter_column)
}

