#' @title
#' Evaluates the format and content of the input dataset
#'
#' @description
#' This function takes a dataset and evaluates its format and content based on
#' the accepted format specified in the data dictionary. It applies a series of
#' checks to make sure the dataset is ready to be processed by the
#' [add_diagnoses()] function which assigns diagnoses to each observation of the
#' dataset. The function evaluates whether:
#'
#' * The input file is a dataset
#' * All mandatory variables are present in the dataset
#' * Missing values (NA) are present in variables where they are not allowed
#' * Data types are correct.
#' * The combination of ID, center, and biopsy date is unique
#' * There are duplicated variable in the dataset
#' * Dates are valid
#' * Content values follow the category values as specified in the data dictionary
#' * Constraints specified in the data dictionary are respected
#'
#' @param banff_dataset A tibble object.
#' @inheritParams get_banff_version
#'
#' @return
#' A list of tibble objects giving information on the assessment of the dataset.
#'
#' @examples
#' {
#'
#' banff_dataset <- get_banff_template()
#' banff_dataset_evaluate(banff_dataset)
#'
#' }
#'
#' @import dplyr tidyr madshapR
#' @importFrom rlang .data
#' @export
banff_dataset_evaluate <- function(banff_dataset,version = NULL) {

  # check the version.
  version <- get_banff_version(version)

  banff_dict_input <- get_banff_dictionary(version,which = "input",detail = TRUE)

  banff_assessment <- list(
    `Data dictionary summary` =
      data_dict_collapse(banff_dict_input)$`Variables`,

    `Dataset assessment` =
      tibble(
        variable = as.character(),
        `Assessment comment` = as.character(),
        `Row number` = as.character())
  )

  ##### Test : if is a dataset #####

  if(!silently_run(is_dataset(banff_dataset))){

    attributes(banff_assessment)$`error` <- "

The dataset you provided does not complies with the package. It must be a
data frame object that must contain a minimal list of variables used in the
process."

    banff_assessment$`Dataset assessment` <-
      tibble(
        variable = '(all)',
        "Assessment comment" =
          "[ERR] - The dataset you provided does not complies with the package")

    message(attributes(banff_assessment)$`error`)
    return(banff_assessment)

  }

  ##### Test : if is all mandatory variables are present ####

  test_missing_cols <-
    banff_dict_input$`Variables`$`name`[!
                  banff_dict_input$`Variables`$`name` %in% names(banff_dataset)]

  # if no col at all
  if(length(test_missing_cols) == length(banff_dict_input$`Variables`$`name`)){

    attributes(banff_assessment)$`error` <- "

Your dataset contains no variables that matches the data dictionary."

    banff_assessment$`Dataset assessment` <-
      tibble(
        "variable" = '(all)',
        "Assessment comment" =
          "[ERR] - No variables that matches the data dictionary found")

    message(attributes(banff_assessment)$`error`)
    return(banff_assessment)

  }

  ###### if some cols are missing
  if(length(test_missing_cols) > 0){

    attributes(banff_assessment)$`error` <- paste0("

Some variables in your dataset are missing.
Missing variables in dataset : \n\n",
bold(toString(test_missing_cols) %>% str_replace_all(", ","\n")))

    banff_assessment$`Dataset assessment` <-
      tibble(
        "variable" = test_missing_cols,
        "Assessment comment" = "[ERR] - Missing variable")

    message(attributes(banff_assessment)$`error`)
    return(banff_assessment)

  }

  if(nrow(banff_dataset) == 0){

    attributes(banff_assessment)$`empty` <- "

Your input dataset is empty."

    banff_assessment$`Dataset assessment` <-
      tibble(
        "variable" = test_missing_cols,
        "Assessment comment" = "[INFO] - The dataset is empty")

    message(attributes(banff_assessment)$`error`)
    return(banff_assessment)

  }

  test_dataset <-
    data_dict_match_dataset(banff_dataset,banff_dict_input,output = 'dataset')

  # permute adequacy calculated and adequacy for testing
  adequacy_input_copy <- calculate_adequacy(test_dataset)

  test_dataset <-
    test_dataset %>%
    mutate(
      adequacy = adequacy_input_copy$`adequacy_calculated`)

  ##### Test : if NA are present where it is not allowed. #####

  no_na_accepted <-
    banff_dict_input$`Variables` %>%
    dplyr::filter(.data$`NA accepted or not` == "NO") %>%
    pull("name")

  contains_na <-
    test_dataset %>%
    summarise(across(all_of(no_na_accepted),~ any(is.na(.)))) %>%
    pivot_longer(everything()) %>%
    dplyr::filter(.data$`value` == TRUE)  %>%
    pull("name")

  if(length(contains_na) > 0){

    attributes(banff_assessment)$`error` = paste0("

Some variables in your dataset contain missing values (NA).
The definition in the Banff dictionary require that all the observations for
these variable must be filled with proper categories, and cannot contain NA.
Usually, you can replace all these NA by '0'.

Variables in the dataset that contain NA : \n\n",
bold(toString(contains_na) %>% str_replace_all(", ","\n")))

    for(i in contains_na){
      # stop()}

      banff_assessment$`Dataset assessment` <-
        banff_assessment$`Dataset assessment` %>%
        bind_rows(
          test_dataset %>%
            add_index() %>%
            select('index',all_of(i)) %>%
            filter(is.na(!! as.symbol(i))) %>%
            reframe(
              `Row number` = paste0(.data$`index`,collapse = ' ; ')) %>%
            mutate(`Row number` = str_trunc(.data$`Row number`, width = 50,
                                            ellipsis = '...')) %>%
            mutate(
              variable = i,
              `Assessment comment` = "[ERR] - Contain some missing values (NA)",
              `Row number` = str_trunc(.data$`Row number`, width = 50, ellipsis = '...')
            ) %>%
            select("variable","Assessment comment","Row number"))
    }

    message(attributes(banff_assessment)$`error`)
    return(banff_assessment)

  }

  ##### Test : general tests #####

  test_unique_id <-
    test_valueType <-
    test_unique_value <-
    test_duplicate_col <-
    test_all_na <-
    test_any_na <-
    test_valid_dates <-
    test_adequacy <-
    test_gs <-
    test_sptcbmml <-
    test_categories <-
    banff_assessment$`Dataset assessment`


  #### * test_valueType ####
  # non unique (patient_id, center, biopsy_id,sc_date_bx)

  test_valueType <-
    check_dataset_valueType(
      test_dataset %>%

        ## patch, fix error later
        add_row(test_dataset[1,] %>%
                  mutate(across(c("sc_date_bx","date_tx"),
                                ~ as_any_date("1983-07-19")))),

      banff_dict_input,valueType_guess = TRUE) %>%
    dplyr::filter(!str_detect(.data$`condition`,'\\[INFO\\]')) %>%
    mutate(
      variable = .data$`name_var`,
      `Assessment comment` =
        "[ERR] - The variable type is not compatible with the Banff dictionary",
      `Row number` = "(all)") %>%
    select("variable","Assessment comment","Row number")

  #### * test_unique_id ####
  # non unique (patient_id, center, biopsy_id,sc_date_bx)

  has_unique_id <-
    test_dataset %>%
    group_by(.data$`patient_id`, .data$`center`, .data$`biopsy_id`, .data$`sc_date_bx`) %>%
    count() %>% dplyr::filter(.data$`n`  >= 2) %>%
    inner_join(
      test_dataset %>%
        select('patient_id','center','biopsy_id','sc_date_bx') %>%
        add_index(),
      by = join_by('patient_id', 'center', 'biopsy_id', 'sc_date_bx')) %>%
    ungroup

  if(nrow(has_unique_id) > 1)
    test_unique_id <-
    has_unique_id %>%
    reframe(
      `Row number` = paste0(.data$`index`,collapse = ' ; ')) %>%
    mutate(
      variable = "Unique ID",
      `Assessment comment` = paste0(
        "[ERR] - Multiple observations (same patient_id/center/biopsy_id/sc_date_bx)"),
      `Row number` = str_trunc(.data$`Row number`, width = 50,ellipsis = '...')) %>%
    select("variable","Assessment comment","Row number")

  #### * test_unique_value ####
  # variable with unique value          WARNING

  if(nrow(test_dataset) > 1){
    test_unique_value <-
      get_unique_value_cols(test_dataset) %>%
      ungroup() %>%
      mutate(
        variable = .data$`col_name`,
        `Assessment comment` = "[INFO] - All the values are identical",
        `Row number` = "(all)") %>%
      select("variable","Assessment comment","Row number")
  }

  #### * test_duplicate_col ####
  # possible duplicated variable       WARNING
  # exclude already addressed unique values.

  test_duplicate_col <-
    test_dataset %>%
    select(-all_of(test_unique_value$`variable`)) %>%
    # mutate(adequacy = i_score) %>%
    get_duplicated_cols() %>%
    mutate(
      variable = .data$`col_name`,
      `Assessment comment` = paste0(
        "[INFO] - ",.data$`condition`),
      `Row number` = "(all)") %>%
    select("variable","Assessment comment","Row number")

  #### * test_all_na ####
  # all NA variable                     WARNING

  test_all_na <-
    test_dataset %>%
    summarize(across(-(c(all_of(no_na_accepted))), (~ all(is.na(.))))) %>%
    mutate(glomeruli = ifelse(.data$`adequacy` == FALSE, FALSE, .data$`glomeruli`)) %>%
    mutate(arteries = ifelse(.data$`adequacy` == FALSE, FALSE, .data$`arteries`)) %>%
    pivot_longer(everything(),names_to = "variable",values_to = "Assessment comment") %>%
    dplyr::filter(.data$`Assessment comment` == TRUE) %>%
    mutate(
      `Assessment comment` = "[INFO] - All the values are missing (NA)",
      `Row number` = "(all)") %>%
    select("variable","Assessment comment","Row number")

  #### * test_any_na ####
  # any NA variable                     WARNING

  has_any_na <-
    test_dataset %>%
    select(- all_of(test_all_na$`variable`)) %>%
    summarize(across(-(all_of(no_na_accepted)), (~ any(is.na(.))))) %>%
    mutate(glomeruli = ifelse(.data$`adequacy` == FALSE, FALSE, .data$`glomeruli`)) %>%
    mutate(arteries = ifelse(.data$`adequacy` == FALSE, FALSE, .data$`arteries`)) %>%
    pivot_longer(everything(),names_to = "variable",values_to = "Assessment comment") %>%
    dplyr::filter(.data$`Assessment comment` == TRUE) %>%
    pull(.data$`variable`)

  if(length(has_any_na) > 1){

    for(i in has_any_na){
      # stop()}

      test_any_na <-
        test_any_na %>%
        bind_rows(
          test_dataset %>%
            add_index() %>%
            select('index',all_of(i)) %>%
            filter(is.na(!! as.symbol(i))) %>%
            reframe(
              `Row number` = paste0(.data$`index`,collapse = ' ; ')) %>%
            mutate(`Row number` = str_trunc(.data$`Row number`, width = 50,
                                            ellipsis = '...')) %>%
            mutate(
              variable = i,
              `Assessment comment` = "[INFO] - The variable has missing values",
              `Row number` =
                str_trunc(.data$`Row number`, width = 50, ellipsis = '...')) %>%
            select("variable","Assessment comment","Row number"))
    }
  }

  #### * test_valid_dates ####
  # sc_date_bx date_tx: must be between 1900 and today ERR

  test_valid_dates <-
    test_dataset %>%
    add_index() %>%
    dplyr::filter(.data$`date_tx` > as_any_date(Sys.time()) | .data$`date_tx` < as.Date("1900-01-01")) %>%
    reframe(
      `Row number` = paste0(.data$`index`,collapse = ' ; ')) %>%
    mutate(
      variable = "date_tx",
      `Assessment comment` = "[ERR] - Some dates in 'date_tx' are not be between 1900-01-01 and today",
      `Row number` = str_trunc(.data$`Row number`, width = 50, ellipsis = '...')) %>%
    bind_rows(
      test_dataset %>%
        add_index() %>%
        dplyr::filter(.data$`sc_date_bx` > as_any_date(Sys.time()) | .data$`sc_date_bx` < as.Date("1900-01-01")) %>%
        reframe(
          `Row number` = paste0(.data$`index`,collapse = ' ; ')) %>%
        mutate(
          variable = "sc_date_bx",
          `Assessment comment` = "[ERR] - Some dates in 'sc_date_bx' are not be between 1900-01-01 and today",
          `Row number` = str_trunc(.data$`Row number`, width = 50, ellipsis = '...'))) %>%
    select("Row number","variable","Assessment comment") %>%
    filter(.data$`Row number` != '')

  #### * test_adequacy ####
  # test adequacy

  test_adequacy <-
    adequacy_input_copy %>%
    add_index() %>%
    dplyr::filter(is.na(.data$`adequacy_calculated`)) %>%
    reframe(
      `Row number` = paste0(.data$`index`,collapse = ' ; ')) %>%
    mutate(
      variable = "adequacy",
      `Assessment comment` = "[ERR] - Some information in adequacy, glomeruli or arteries are missing",
      `Row number` = str_trunc(.data$`Row number`, width = 50, ellipsis = '...')) %>%
    bind_rows(
      adequacy_input_copy %>%
        add_index() %>%
        dplyr::filter(
          !is.na(.data$`adequacy_input`) &
            .data$`adequacy_calculated` != .data$`adequacy_input`) %>%
        reframe(
          `Row number` = paste0(.data$`index`,collapse = ' ; ')) %>%
        mutate(
          variable = "adequacy",
          `Assessment comment` = "[INFO] - For some values, adequacy differ from adequacy_calculated",
          `Row number` = str_trunc(.data$`Row number`, width = 50, ellipsis = '...'))) %>%
    select("Row number","variable","Assessment comment") %>%
    filter(.data$`Row number` != '')

  #### * test_gs ####
  # gs entre 0 et 100

  test_gs <-
    test_dataset %>%
    add_index() %>%
    dplyr::filter(.data$`gs` < 0 | .data$`gs` > 100) %>%
    reframe(
      `Row number` = paste0(.data$`index`,collapse = ' ; ')) %>%
    mutate(
      variable = "gs",
      `Assessment comment` = "[ERR] - Some values in 'gs' are not be between 0 and 100",
      `Row number` = str_trunc(.data$`Row number`, width = 50, ellipsis = '...')) %>%
    select("Row number","variable","Assessment comment") %>%
    filter(.data$`Row number` != '')

  #### * test_sptcbmml ####
  # is.na(sptcbmml) == TRUE only if microscopy == 0 ERROR

  test_sptcbmml <-
    test_dataset %>%
    add_index() %>%
    dplyr::filter(is.na(.data$`sptcbmml`) & .data$`microscopy` != 0) %>%
    reframe(
      `Row number` = paste0(.data$`index`,collapse = ' ; ')) %>%
    mutate(
      variable = "sptcbmml",
      `Assessment comment` = "[ERR] - sptcbmml has some missing values when microscopy is 0",
      `Row number` = str_trunc(.data$`Row number`, width = 50, ellipsis = '...')) %>%
    select("Row number","variable","Assessment comment") %>%
    filter(.data$`Row number` != '')

  #### * test_categories ####
  # category in dd but not in dataset WARNING
  # category in dataset but not in dd ERROR
  categorical_variables <- unique(banff_dict_input$`Categorie`$`variable`)

  # exclude already adressed all_na
  categorical_variables <-
    categorical_variables[!categorical_variables %in% test_all_na$`variable`]

  # exclude glomerulies and arteries if adequacy OK
  if(sum(is.na(test_dataset$`adequacy`)) == 0){
    categorical_variables <-
      categorical_variables[!categorical_variables %in% c("arteries","glomeruli")]
  }

  if(nrow(test_dataset) == 1){
    test_categories <- tibble("variable" = as.character(),
                              "Assessment comment" = as.character(),
                              "Row number" = as.character())
  }else{
    for(i in categorical_variables){
      # stop()}

      dd_cat <- banff_dict_input$`Categories`[banff_dict_input$`Categorie`$`variable` == i,]$`name`
      ds_cat <- as.character(unique(test_dataset[!is.na(test_dataset[[i]]),i][[1]]))

      cat_in_dd_only <- dd_cat[!dd_cat %in% ds_cat]
      cat_in_ds_only <- ds_cat[!ds_cat %in% dd_cat]

      if(length(cat_in_dd_only) > 0){
        test_categories <-
          test_categories %>%
          bind_rows(
            tibble(
              variable = i,
              `Assessment comment` = "[INFO] - Some values are only present in the data dictionary",
              `Row number` = "(all)")
          )}

      if(length(cat_in_ds_only) > 0){
        test_categories <-
          test_categories %>%
          bind_rows(
            tibble(
              variable = i,
              `Assessment comment` = "[ERR] - Some values found in the dataset are not in the data dictionary",
              `Row number` = "(all)"))}
    }
  }

  # exclude already adressed unique values
  test_categories <-
    test_categories %>%
    mutate(`Assessment comment` =
             if_else(
               (.data$`variable` %in% test_unique_value$`variable` &
                  str_detect(.data$`Assessment comment`,"\\[INFO\\]")),NA_character_,.data$`Assessment comment`)) %>%
    dplyr::filter(!is.na(.data$`Assessment comment`))

  ##### gater all information #####

  all_tests <-
    bind_rows(
      test_valueType,
      test_unique_id,
      test_unique_value,
      test_duplicate_col,
      test_all_na,
      test_any_na,
      test_valid_dates,
      test_adequacy,
      test_gs,
      test_sptcbmml,
      test_categories)

  test_no_fail <-
    tibble(variable = names(test_dataset)) %>%
    dplyr::filter(!.data$`variable` %in% all_tests$`variable`) %>%
    mutate(`Assessment comment` = "No problem detected",
           `Row number` = "(all)")

  if("Unique ID" %in% all_tests$`variable`){
    test_no_fail <-
      test_no_fail  %>%
      dplyr::filter(!.data$`variable` %in%
                      c("patient_id","center","biopsy_id","sc_date_bx"))}

  banff_assessment$`Dataset assessment` <-
    bind_rows(test_no_fail,all_tests) %>%
    left_join(banff_dict_input$`Variables` %>%
                select("index",variable = "name"),join_by("variable")) %>%
    mutate(index = as.integer(.data$`index`),
           index = replace_na(.data$`index`,0L)) %>%
    arrange(.data$`index`) %>%
    select("variable","Row number","Assessment comment")

  if(sum(str_detect(
    banff_assessment$`Dataset assessment`$`Assessment comment`,"\\[ERR\\]")) > 0){

    attributes(banff_assessment)$`error` <- "

Some variables in your dataset contain errors. The definition of these variables in
the data dictionary require that all the observations for these variable must be
filled with proper values or categories, and some of them cannot be missing (NA)."

    message(attributes(banff_assessment)$`error`)
    return(banff_assessment)}

  if(sum(str_detect(
    banff_assessment$`Dataset assessment`$`Assessment comment`,"\\[INFO\\]")) > 1){

    attributes(banff_assessment)$`warn` <- "

Your dataset contains no error. Although, some variables may require your
attention because they have some information associated (such as all missing
variable (NA), or categories not present in the dataset, or possible duplicated
variables. This message can be ignored if you think your dataset is correct."

    message(attributes(banff_assessment)$`warn`)
    return(banff_assessment)}

  if(toString(unique(
    banff_assessment$`Dataset assessment`$`Assessment comment`)) ==
    "No problem detected"){

    attributes(banff_assessment)$`warn` <- "

Your dataset contains no error."

    banff_assessment$`Dataset assessment` <-
      tibble("variable" = '(all)', "Assessment comment" = "No problem detected")

    message(attributes(banff_assessment)$`warn`)
    return(banff_assessment)}

}

#  @title
#  summarise a dataset with its diagnoses
#
#  @description
#  Assesses and summarizes the content and structure of an input dataset (with
#  diagnoses) and generates reports of the results. This function can be used
#  to evaluate data structure, and to summarize additional information about
#  variable distributions and descriptive statistics.
#
#  @param banff_diagnoses A dataset object with diagnoses.
#  @param banff_assessment A tibble object.
#  @param banff_dict A list of tibble objects giving information on the
#  assessment of the dataset.
#  @param input_file_name A character string specifying the name of the dataset.
#
#  @return
#  A list of tibble objects giving information on the summary of the Banff
#  dataset.
#
#  @examples
#  {
#
#  library(fabR)
#  version <- get_banff_version()
#  input_file <- system.file("extdata",
#     paste0(version,"/banff_example.xlsx"), package = "banffIT")
#  banff_dataset <- read_excel_allsheets(input_file)[1,]
#  banff_dataset_evaluate(banff_dataset)
#
#  }
#
#  @import dplyr tidyr madshapR
#  @importFrom rlang .data
#  @export
# banff_dataset_summarize <- function(
#     banff_diagnoses,
#     banff_assessment = NULL,
#     banff_dict,
#     input_file_name) {
#
#   if(is.null(banff_assessment))
#     banff_assessment <- banff_dataset_evaluate(banff_diagnoses$codeset)
#
#   banff_report <-
#     banff_dataset_summarize(
#       dataset = banff_diagnoses$codeset,
#       data_dict = banff_dict,
#       dataset_name = input_file_name)
#
#   banff_report$`Dataset assessment - input`     <- banff_assessment$`Dataset assessment`
#   banff_report$`Dataset assessment - diagnoses` <- banff_report$`Dataset assessment`
#   banff_report$`Dataset assessment`             <- NULL
#   banff_report$`Variables summary (all)`        <- banff_assessment$`Data dictionary summary`
#
#   banff_report$`Dataset assessment - diagnoses` <-
#     banff_report$`Dataset assessment - diagnoses` %>%
#     select("variable" = "name","condition" = "Quality assessment comment", "value") %>%
#     dplyr::filter(!.data$`variable` %in% banff_report$`Dataset assessment - input`$variable*)
#
#   banff_report <- banff_report[unique(c(
#     "Overview","Dataset assessment - input",
#     "Dataset assessment - diagnoses", names(banff_report)))]
#
#   return(banff_report)
#
# }


