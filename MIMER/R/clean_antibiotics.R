
#' @importFrom dplyr %>%
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr row_number
#' @importFrom dplyr as_label
#' @importFrom dplyr if_else
#' @importFrom dplyr arrange
#' @importFrom rlang .data

globalVariables(c("drug"))

#Generic Function
#' @name clean_antibiotics
#' @title Clean  Antibiotics using Fuzzy Matching with AMR Packages's Antibiotics Data.
#' @description
#'  This utility helps to identify and clean-up antibiotics names passed to it.
#' Also helps to identify whether a medicine is antibiotic or not
#' @usage clean_antibiotics(x , ...)
#' @param x character vector or a dataframe containing medicine details
#' @param ...    column name with drug details (required only if first parameter is a dataframe)
#'
#' @return Character Vector or Data Frame
#' @examples
#'
#' clean_antibiotics(c("Amoxicilin","Amoxicillin","Paracetamol"))
#'
#'
#' df <- data.frame(medicine = c("Amoxicilin","Amoxicillin","Paracetamol"))
#' clean_antibiotics(df,drug_col=medicine)
#'
#'

#' @export
clean_antibiotics <- function(x, ...) {

  stopifnot(is.data.frame(x) | is.character(x))

  UseMethod("clean_antibiotics")
}


#' @export
clean_antibiotics.character <- function(x, custom_synonyms = NULL, ...) {
  temp_df <- data.frame(drug = x)
  clean_temp_df <- clean_antibiotics(temp_df, drug_col = drug, custom_synonyms = custom_synonyms)
  return(clean_temp_df$abx_name)
}

#' @export
clean_antibiotics.data.frame <- function(x, drug_col,
                                         fuzzy_matching_method = "osa",
                                         custom_synonyms = NULL, ...) {
  stopifnot(nargs()[[1]] >= 2)

  if("abx_name" %in% colnames(x) | "synonyms" %in% colnames(x) | "is_abx" %in% colnames(x))
  {
    stop(message("Please pass valid Data Frame, column names like abx_name/synonyms/is_abx are not allowed"))
  }

  #Creating a antibiotics-lookup table
  df_amr_antibiotics_lookup <- populate_amr_antibiotics()

  hard_coded_custom_rules <- c("Trimethoprim/sulfamethoxazole" = "Sulfameth/Trimethoprim",
                               "Piperacillin/tazobactam" = "Piperacillin/tazo",
                               "Amoxicillin/clavulanic acid"="Amoxicillin-Clavulanate",
                               "Trimethoprim/sulfamethoxazole" = "Sulfamethoxazole-Trimethoprim")
  combined_custom_rules <- c(hard_coded_custom_rules, custom_synonyms)

  #Add extra synonyms to Antibiotics lookup
  df_amr_antibiotics_lookup_final <- add_new_synonyms(df_amr_antibiotics_lookup,
                                                      new_synonyms = combined_custom_rules)

  medications_cleaned <- clean_medication(x, {{drug_col}})

  #Fuzzy Matching with AMR DataSet & Identifying AntiBiotics###########
  #DF, passing  to method 'get_antibiotics_in_medications' should have a column named 'medications_cleaned'
  df_antibiotics <- get_antibiotics_in_medications(medications_cleaned,df_amr_antibiotics_lookup_final,fuzzy_matching_method)

  #Removing duplicates for joining with row table
  df_antibiotics <- df_antibiotics %>%
    filter(strtoi(.data$distance_column) <=1) %>%
    distinct({{drug_col}}, .data$name, .keep_all = TRUE) %>%
    group_by({{drug_col}}) %>% arrange(.data$distance_column) %>% mutate(row_num = row_number()) %>%
    filter(.data$row_num == 1) %>% select(-c("row_num","distance_column"))

  ##Final Cleanup ####
  joined_data <- left_join(x, df_antibiotics, by = as_label(substitute(drug_col)))
  joined_data <- joined_data %>%
    rename(abx_name = "name") %>%
    mutate(is_abx = if_else(!is.na(.data$abx_name), TRUE, FALSE)) %>%
    select(-c("medication_cleaned", -"synonyms"))
  return(joined_data)
}

