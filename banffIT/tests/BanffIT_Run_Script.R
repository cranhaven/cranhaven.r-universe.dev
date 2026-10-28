# # # loading dataset
# library(banffIT)
# library(fs)
# library(dplyr)
# library(rlang)
# library(tools)
# # library(readr)
# library(stringr)
# library(lubridate)
# library(tidyr)
# library(fabR)
# library(madshapR)
# library(crayon)
# # library(Rmonize)
# # library(janitor)
# #
# # input_file = "tests/CHUQ_for_test_3.csv"
# # output_folder = 'tests'
# # language = 'label:en'
# # option_filter = quote(adequacy == 1)
# # detail = TRUE
# # include_banff_dictionary = TRUE
#
# library(banffIT)
#
# input_file = '../../example_errors.csv'
#
# # banff_launcher(
# #   input_file = 'inst/extdata/2022/banff_example.xlsx',
# #   output_folder = 'tests',
# #   version = 2022)
# #
# # banff_launcher(
# #   input_file = 'inst/extdata/2017/banff_example.xlsx',
# #   output_folder = 'tests',
# #   version = 2017)
#
#
#
# patch_banffIT <- function(input_file){
#
#   library(fs)
#   library(fabR)
#   library(tidyr)
#   library(stringr)
#
#   path_file <- input_file
#   if(path_ext(path_file) == 'xlsx') file <- read_excel_allsheets(path_file)
#   if(path_ext(path_file) == 'csv')  file <- read_csv_any_formats(path_file)
#
#   if(sum(c('sc_date_bx','date_tx') %in% names(file)) != 2)
#     stop(
# '`sc_date_bx` and/or `date_tx` is missing in your dataset. Please refer to online documentation')
#
#   if(sum(is.na(silently_run(as_any_date(file$sc_date_bx)))) >
#      sum(is.na(file$sc_date_bx)) |
#      sum(is.na(silently_run(as_any_date(file$date_tx)))) >
#      sum(is.na(file$date_tx))) stop(
# '\n\n
# In `sc_date_bx` and/or `date_tx` : Some values in your file cannot be coerced in
# dates. Open your file and check in these columns which values are not
# compatible with date formats. Please refer to online documentation')
#
#   file_corrected            <- file
#   file_corrected$sc_date_bx <- as_any_date(file_corrected$sc_date_bx)
#   file_corrected$date_tx    <- as_any_date(file_corrected$date_tx)
#
#   new_name <-
#     str_replace(path_file,
#                 basename(path_file),paste0('Copy - ',
#                 file_path_sans_ext(basename(path_file)),'.xlsx'))
#
#   write_excel_allsheets(file_corrected, new_name)
#
#   message(
# "
# A new file has been created in the same folder of the original file.
# Use this file path instead of your original file, or replace your
# dates columns with the one provided (All of the other columns are unchanged)
#
# input_file <- '", new_name,"'")
#
# }
#
# # patch_banffIT(input_file)
#
# input_file = 'inst/extdata/2022/banff_example.xlsx'
#
# differential_diagnoses <- function(input_file, between = NULL, and = NULL){
#
#   between <- suppressMessages(get_banff_version(between))
#   and <- suppressMessages(get_banff_version(and))
#
#   if(between == and){
#     return('Versions are identical')
#   }
#   input <- read_excel_allsheets(input_file)
#   banff_data_dict_1 <- suppressMessages(get_banff_dictionary(version = between,which = "output"))
#   banff_data_dict_2 <- suppressMessages(get_banff_dictionary(version = and,which = "output"))
#
#   diag_1 <- try(suppressMessages(add_diagnoses(input,version = between)),silent = TRUE)
#   if(class(diag_1)[1] == 'try-error') add_diagnoses(input,version = between)
#
#   diag_2 <- try(suppressMessages(add_diagnoses(input,version = and)),silent = TRUE)
#   if(class(diag_2)[1] == 'try-error') add_diagnoses(input,version = and)
#
#   diag_1 <-
#     diag_1 %>%
#     select(patient_id,starts_with('diag')) %>%
#     data_dict_match_dataset(banff_data_dict_1,output = 'dataset')
#
#   diag_2 <-
#     diag_2 %>%
#     select(patient_id,starts_with('diag')) %>%
#     data_dict_match_dataset(banff_data_dict_2,output = 'dataset')
#
#   message(paste0("\nVersion "),between,"\n")
#   diag_1 <- dataset_cat_as_labels(diag_1,banff_data_dict_1) %>% pivot_longer(cols=starts_with("diag"))
#
#   message(paste0("\nVersion "),and,"\n")
#   diag_2 <- dataset_cat_as_labels(diag_2,banff_data_dict_2) %>% pivot_longer(cols=starts_with("diag"))
#
#   diff <-
#     bind_cols(diag_1,value_2 =diag_2$value) %>%
#     # mutate(
#     #   value = str_squish(str_remove(value, "^[^:]+:")),
#     #   value_2 = str_squish(str_remove(value_2, "^[^:]+:"))) %>%
#     filter(value != value_2) %>%
#     rename_with(.cols = c("value","value_2"),~c(toString(between),toString(and)))
#
#   return(diff)
# }
# differential_diagnoses(input_file,between = 2017,and = 2022)
