#' @importFrom rlang .data


#this is an internal function to help create mapping from LSA plate to ROI
translate_rows_for_sort <- function(x){
  ifelse (x == "A", 1,
          ifelse(x == "E", 2,
                 ifelse(x == "B", 3,
                        ifelse(x == "F", 4,
                               ifelse(x == "C", 5,
                                      ifelse(x == "G", 6,
                                             ifelse(x == "D", 7,
                                                    ifelse(x == "H", 8, 0))))))))

}

get_ROI <- function(sample_info){

  row_column <- tibble::tibble(Sample = stringr::str_c(sample_info$Row, sample_info$Column),
                               Block = sample_info$Block)

  ROI_map_path <- system.file("extdata", "LSA_to_ROI.xlsx",
                              package="htrSPRanalysis")
  ROI_map <- readxl::read_xlsx(ROI_map_path)

  ROI_df <- dplyr::left_join(row_column, ROI_map)

  ROI_df$`ROI #`

}
# Expand sample sheet so that replicates are each represented by one row

process_sample_sheet <- function(sample_sheet){
  # Process Sample Sheet

  # Position/Channel/Sensor has multiple wells. Need to expand to one row per well.

  test_names <- names(sample_sheet)

  # Bloc/Chip/Tray is old format

  if (!("Position/Channel/Sensor" %in% test_names)){
    idx <- which(test_names == "Block/Chip/Tray")
    test_names[idx] <- "Position/Channel/Sensor"
    names(sample_sheet) <- test_names

  }

  suppressWarnings(sample_sheet %>%
    tidyr::separate("Position/Channel/Sensor", into = c("BeginPosition", "EndPosition", sep = "-")) %>%
    tidyr::separate("BeginPosition", into = c("Row", "Column"), sep = 1) %>%
    tidyr::separate("EndPosition", into = c("EndRow", "EndColumn"), sep = 1) %>%
    dplyr::mutate("Column" = as.integer(.data$Column)) %>%
    dplyr::mutate("EndColumn" = as.integer(.data$EndColumn)) -> RowExpansionTemplate)

  check_row_number <- !(RowExpansionTemplate$Row %in% c("A", "B", "C", "D","E", "F","G","H"))

  bad_rows <- which(check_row_number)

  bad_rows <- stringr::str_flatten(paste0(bad_rows, ","))

  if (sum(check_row_number) > 0){
     stop(paste("The row position names in the sample sheet are incorrect in row(s):",
                bad_rows))
  }

  check_row_number <- !(RowExpansionTemplate$EndRow %in% c("A", "B", "C", "D","E", "F","G","H")) &
    !is.na(RowExpansionTemplate$EndRow)

  bad_rows <- which(check_row_number)

  bad_rows <- stringr::str_flatten(paste0(bad_rows, ","))

  if (sum(check_row_number) > 0){
    stop(paste("The row position names in the sample sheet are incorrect in row(s):",
               bad_rows))
  }

  check_col_number <- (RowExpansionTemplate$Column < 0 |
                         RowExpansionTemplate$Column > 12 |
                         is.na(RowExpansionTemplate$Column))

  bad_cols <- which(check_col_number)
  bad_cols <- stringr::str_flatten(paste0(bad_cols, ","))

    if (sum(check_col_number) > 0){
    stop(paste("The column position names in the sample sheet are incorrect in row(s):",
               bad_cols))
  }

  check_col_number <- ((RowExpansionTemplate$EndColumn < 1 |
                         RowExpansionTemplate$EndColumn > 12) &
                         !is.na(RowExpansionTemplate$EndColumn))

  bad_cols <- which(check_col_number)
  bad_cols <- stringr::str_flatten(paste0(bad_cols, ","))

  if (sum(check_col_number) > 0){
    stop(paste("The column position names in the sample sheet are incorrect in row(s):",
               bad_cols))
  }

  length_ss <- dim(RowExpansionTemplate)[1]
  ss_exp <- NULL

  for(i in 1:length_ss){

    old_row <- RowExpansionTemplate[i,]
    old_row$Row <- stringr::str_to_upper(old_row$Row)
    old_row$EndRow <- stringr::str_to_upper(old_row$EndRow)

    start_letter <- old_row$Row
    end_letter   <- old_row$EndRow

    if (is.na(end_letter))
      end_letter <- start_letter

    if(start_letter != end_letter) {
      start_idx <- which(LETTERS == start_letter)
      end_idx <- which(LETTERS == end_letter)
      for (j in start_idx+1:(end_idx-1)){
        new_row <- old_row
        new_row$Row <- LETTERS[j]
        if (j == start_idx+1)
          ss_exp <- rbind(old_row, new_row)
        else
          ss_exp <- rbind(ss_exp, new_row)
      }
    } else
      ss_exp <- rbind(ss_exp, old_row)
  }

  ### Now to expand columns if necessary
  RowExpansionTemplate <- ss_exp

  length_ss <- dim(RowExpansionTemplate)[1]
  ss_exp <- NULL

  for(i in 1:length_ss){
    old_row <- RowExpansionTemplate[i,]

    start_col <- old_row$Column
    end_col   <- old_row$EndColumn

    if (is.na(end_col))
      end_col <- start_col

    if (start_col != end_col){
      for (j in (start_col+1):end_col){
        new_row <- old_row
        new_row$Column++
          if (j == start_idx+1)
            ss_exp <- rbind(old_row, new_row)
        else
          ss_exp <- rbind(ss_exp, new_row)
      }
    } else
      ss_exp <- rbind(ss_exp, RowExpansionTemplate[i,])
  }

  ss_exp %>% dplyr::select(-"EndRow", -"EndColumn", -"-") -> ss_exp

  # Now expand blocks
  current_idx <- 0
  new_current_idx <- 0
  nsamples <- dim(ss_exp)[1]
  sample_info_expanded <- NULL

  for(i in 1:nsamples){

    num_blocks <- stringr::str_count(ss_exp[i,]$`Block/Chip/Tray`, ",") + 1

    blocks <- as.numeric(purrr::flatten(stringr::str_split(ss_exp[i,]$`Block/Chip/Tray`, ",")))

    bad_blocks <- ifelse(blocks %in% 1:4,0,1)
    if (sum(bad_blocks) > 0){
      stop(paste("There is an invalid block number in rows ", i))
    }

    # create an expanded sample sheet with one row for each block

    tmp_sample_info_expanded <- purrr::map_dfr(seq_len(num_blocks), ~ss_exp[i,])
    tmp_sample_info_expanded$Block <- blocks
    #  tmp_sample_info_expanded <- data.frame(tmp_sample_info_expanded)
    sample_info_expanded <- dplyr::bind_rows(sample_info_expanded, tmp_sample_info_expanded)

  }
  # Rows are sorted A,E,B,F,C,G,D,H

  sample_info_expanded %>%
    dplyr::mutate(RowSort = translate_rows_for_sort(.data$Row)) ->
    sample_info_expanded

  ROIs <- get_ROI(sample_info_expanded)

  if (!is.null(ROIs) & sum(is.na(ROIs) == 0) & length(ROIs == dim(sample_info_expanded)[1]))
     sample_info_expanded$ROI <- ROIs else
    sample_info_expanded$ROI <- 1:dim(sample_info_expanded)[1]

  sample_info_expanded %>%
    dplyr::arrange(.data$RowSort, .data$Column, .data$Block) -> sample_info_expanded

  sample_info_expanded
}

### keep_concentrations_all is wrong value. Fix to use ligand_ROI dataframe
### ROI at this point is the row # in sample_info. Just filter out ROI's from titration_data

select_samples_with_ROI <- function(sample_info, titration_data, ligand_and_ROI){

  remove_ligands <- which(sample_info$Incl. == "N")
  keep_ligands <- which(sample_info$Incl. == "Y") # which samples to keep
  keep_ROI <- sample_info[keep_ligands,]$ROI

  titration_data %>% dplyr::select(tidyselect::everything(), -tidyselect::starts_with("Y")) -> x_vals
  titration_data %>% dplyr::select(tidyselect::everything(), -tidyselect::starts_with("X")) -> y_vals

  n_time_points <- dim(x_vals)[1]
  nsamples <- dim(sample_info)[1]

  keep_concentrations_all <- which(ligand_and_ROI$ROI %in% keep_ROI)
  x_vals_select <- x_vals[ , keep_concentrations_all]
  y_vals_select <- y_vals[ , keep_concentrations_all]


  all_concentrations_ligand <- NULL
  all_concentrations_values <- NULL

  for(i in 1:nsamples){

    num_conc <- stringr::str_count(sample_info$`All Concentrations`[i], ",") + 1

    if (sample_info$Incl.[i] == "N")
      next


    concentrations <-
      as.numeric(purrr::flatten(stringr::str_split(sample_info$`All Concentrations`[i], ",")))
    all_concentrations_ligand <- c(all_concentrations_ligand, num_conc)
    all_concentrations_values <- c(all_concentrations_values, concentrations)

    #Use this to match to ligand_conc and to x_vals, y_vals
  }

  sample_info <- sample_info[keep_ligands, ]
  sample_info$NumConc <- all_concentrations_ligand
  ligand_and_ROI <- ligand_and_ROI[keep_concentrations_all, ]

  list(Time = x_vals_select, RU = y_vals_select,
       sample_info = sample_info,
       ligand_and_ROI = ligand_and_ROI,
       all_concentrations_values = all_concentrations_values,
       n_time_points = n_time_points)
}


select_samples <- function(sample_info, titration_data){
  remove_ligands <- which(sample_info$Incl. == "N")
  keep_ligands <- which(sample_info$Incl. == "Y")

  titration_data %>% dplyr::select(tidyselect::everything(), -tidyselect::starts_with("Y")) -> x_vals
  titration_data %>% dplyr::select(tidyselect::everything(), -tidyselect::starts_with("X")) -> y_vals

  n_time_points <- dim(x_vals)[1]
  nsamples <- dim(sample_info)[1]

  #loop over all samples to be included. Exclude concentrations not selected for analysis.

  displacement_in_titration_data <- 0
  keep_concentrations_all <- NULL
  all_concentrations_ligand <- NULL
  all_concentrations_values <- NULL

  for(i in 1:nsamples){

    num_conc <- stringr::str_count(sample_info$`All Concentrations`[i], ",") + 1

    if (sample_info$Incl.[i] == "N"){
      displacement_in_titration_data <- displacement_in_titration_data + num_conc
      next
    }

    concentrations <-
      as.numeric(purrr::flatten(stringr::str_split(sample_info$`All Concentrations`[i], ",")))
    all_concentrations_ligand <- c(all_concentrations_ligand, num_conc)
    all_concentrations_values <- c(all_concentrations_values, concentrations)

    #Use this to match to ligand_conc and to x_vals, y_vals
    keep_conc_idx <- 1:num_conc + displacement_in_titration_data

    keep_concentrations_all <- c(keep_concentrations_all, keep_conc_idx)

    displacement_in_titration_data <- displacement_in_titration_data + num_conc

  }

  # time and ru values for selected wells
  x_vals_select <- x_vals[ , keep_concentrations_all]
  y_vals_select <- y_vals[ , keep_concentrations_all]

  sample_info <- sample_info[keep_ligands, ]
  sample_info$NumConc <- all_concentrations_ligand

  list(Time = x_vals_select, RU = y_vals_select,
       sample_info = sample_info,
       all_concentrations_values = all_concentrations_values,
       n_time_points = n_time_points)
}

get_auto_bulkshift <- function(well_idx, sample_info, Time, RU){

  bulkshift <- sample_info[well_idx, ]$Bulkshift
  auto_bulkshift <- sample_info[well_idx, ]$`Automate Bulkshift`
  num_conc <- sample_info[well_idx, ]$NumInclConc

  if (bulkshift == "Y")
    return(bulkshift)
  if (auto_bulkshift != "Y")
    return(bulkshift)

  start_conc <- sample_info[well_idx, ]$FirstInclConcIdx
  end_conc <- start_conc + num_conc - 1

  Time <- Time[, start_conc:end_conc]
  RU <- RU[, start_conc:end_conc]

  association <- sample_info[well_idx, ]$Association
  baseline <- sample_info[well_idx, ]$Baseline
  baseline_start <- sample_info[well_idx, ]$`Bsl Start`

  # get last ten seconds of assoc and first 10 of dissoc

  end_assoc_frame <- association + baseline + baseline_start - 20
  # This means the end of the dissoc window to use
  begin_dissoc_frame <- end_assoc_frame + 40

  avgs <- purrr::map_dfc(1:num_conc, .f = get_averages, Time, RU, end_assoc_frame, begin_dissoc_frame)

  avg_assoc <- avgs[1,]
  avg_dissoc <- avgs[2,]

  sd_assoc <- as.vector(avgs[3,])
  sd_dissoc <- as.vector(avgs[4,])

  if (sum(ifelse(sd_assoc > 10, 1, 0)) > 0 |
      sum(ifelse(sd_dissoc > 10, 1, 0)) > 0){
    warning(paste("Too much noise to detect bulkshift for ROI",
                  sample_info$ROI[well_idx]))
    return("N")
  }


  test_bulkshift <- as.vector(abs(avg_assoc - avg_dissoc)/(avg_dissoc + avg_assoc))

  # fit bulkshift if the difference in response is more than 10 % of the total response

  if (sum(ifelse(test_bulkshift > 0.1, 1, 0)) > 0)
    return("Y")

  return("N")
}

get_averages <- function(conc_idx, Time, RU, end_assoc_frame, begin_dissoc_frame){

   df <- dplyr::bind_cols(Time = Time[, conc_idx], RU = RU[, conc_idx])
   names(df) <- c("Time", "RU")

  df %>%
    dplyr::filter(.data$Time >= end_assoc_frame & .data$Time <= (end_assoc_frame + 20)) %>%
    dplyr::select("RU") -> RU_assoc

  df %>%
    dplyr::filter(.data$Time >= end_assoc_frame + 20 & .data$Time <= begin_dissoc_frame) %>%
    dplyr::select("RU") -> RU_dissoc

  avg_assoc <- mean(RU_assoc$RU, na.rm = TRUE)
  avg_dissoc <- mean(RU_dissoc$RU, na.rm = TRUE)

  sd_assoc <- stats::sd(RU_assoc$RU, na.rm = TRUE)
  sd_dissoc <- stats::sd(RU_dissoc$RU, na.rm = TRUE)

  c(avg_assoc, avg_dissoc, sd_assoc, sd_dissoc)

}

select_concentrations <- function(sample_info, x_vals, y_vals){
  #after we select the wells, we select the concentrations out of the remaining time series

  n_time_points <- dim(x_vals)[1]
  nsamples <- dim(sample_info)[1]

  #loop over all samples to be included. Exclude concentrations not selected for analysis.

  displacement_in_titration_data <- 0
  keep_concentrations <- NULL
  incl_concentrations_ligand <- NULL
  incl_concentrations_values <- NULL
  error_idx <- NULL

  for(i in 1:nsamples){

    start_idx <- sample_info$FirstConcIdx[i]
    num_conc <- stringr::str_count(sample_info$`All Concentrations`[i], ",") + 1

    # if (sample_info$Incl.[i] == "N"){
    #   displacement_in_titration_data <- num_conc + displacement_in_titration_data
    #   next
    # }

    concentrations <-
      as.numeric(purrr::flatten(stringr::str_split(sample_info$`All Concentrations`[i], ",")))

    if (stringr::str_to_upper(sample_info$`Incl. Conc.`[i]) == "ALL"){
      incl_concentrations <- concentrations
    } else {
      incl_concentrations <-
        as.numeric(purrr::flatten(stringr::str_split(sample_info$`Incl. Conc.`[i], ",")))
    }
    num_incl <- length(incl_concentrations)

    if (num_incl > 5){
      incl_concentrations <-
        get_best_window(i, sample_info, x_vals, y_vals,
                        num_incl, incl_concentrations,
                        start_idx, n_time_points)
      num_incl <- length(incl_concentrations)

      if(is.null(num_incl) | num_incl == 0){
         msg <- paste("Could not determine the optimal concentrations for ROI", sample_info$ROI[i],
                      "\n Please check the sample information and the time series for issues or specify the concentrations to be fit explicitly in the sample information")
         stop(msg)
      }
    }

    #Use this to match to ligand_conc and to x_vals, y_vals

    # the following keeps all of the concentrations for each of the wells selected to include, but
    # excludes the wells not selected
    if (num_incl >= 1){

      incl_concentrations_ligand <- c(incl_concentrations_ligand, num_incl)
      incl_concentrations_values <- c(incl_concentrations_values, incl_concentrations)

      incl_idx <- which(concentrations %in% incl_concentrations) + displacement_in_titration_data
      keep_concentrations <- c(keep_concentrations, incl_idx)

    } else
      error_idx <- c(error_idx, i)

    displacement_in_titration_data <- num_conc + displacement_in_titration_data

  }


  list(keep_concentrations = keep_concentrations,
       sample_info = sample_info, incl_concentrations_values = incl_concentrations_values,
       incl_concentrations_ligand = incl_concentrations_ligand,
       error_idx = error_idx)

}

#check sample sheet
check_sample_sheet <- function(sample_sheet, sample_sheet_path) {

    if (dim(sample_sheet)[1] == 0) {
      check1 <- "empty data set :"
      flagspresent <- TRUE
      return(flagspresent)
  }
  columns <- names(sample_sheet)
  defaultcolumnnames <-
    c(
      "Incl.",
      "Block/Chip/Tray",
      "Position/Channel/Sensor",
      "Analyte",
      "Ligand",
      "Baseline",
      "Association",
      "Dissociation",
      "Asso. Skip",
      "Disso. Skip",
      "All Concentrations",
      "Incl. Conc.",
      "Bulkshift",
      "Regen.",
      "Bsl Start",
      "Base Corr",
      "Global Rmax",
      "Automate Dissoc. Window",
      "Automate Bulkshift"
    )

  columnspresent <-
    any(columns %in% defaultcolumnnames == FALSE) # if true, means that unidentified column is present
  if (columnspresent == TRUE) {
    columnspresent_note <-
      paste(setdiff(columns, defaultcolumnnames),
            setdiff(defaultcolumnnames, columns))
  } else {
    columnspresent_note <- ""
  }
  columnslength <-
    (length(columns) == length(defaultcolumnnames)) # if false, some column is missing or extra is added
  if (columnslength == FALSE) {
    columnslength_note <-
      paste("File columns",
            length(columns),
            ", expected",
            length(defaultcolumnnames))
  } else {
    columnslength_note = ""
  }
  columns_note <-
    unique(paste(columnspresent_note, columnslength_note, sep = ""))

  #check for "Incl." column !is.na & %in% c("Y", "N")
  incl_note <-
    (ifelse(any(
      !unique(sample_sheet$"Incl.") %in% c("Y", "N")
    ),  "Only Y and N values allowed", ""))
  #"Block/Chip/Tray" !is.na
  block_note <-
    ifelse(any(is.na(unique(
      sample_sheet$"Block/Chip/Tray"
    ))) == TRUE, "Empty rows", "")
  #"Position/Channel/Sensor" !is.na & letter and number
  position_note <-
    ifelse(any(is.na(
      unique(sample_sheet$"Position/Channel/Sensor")
    )) == TRUE, "Empty rows", "")
  position_note <-
    unique(ifelse(
      !grepl(
        "([A-Z])([0-9])",
        unique(sample_sheet$`Position/Channel/Sensor`)
      ),
      paste(position_note, "Incorrect format"),
      position_note
    ))
  #"Analyte" !is.na &character
  analyte_note <-
    ifelse(any(is.na(unique(
      sample_sheet$Analyte
    ))) == TRUE, "Empty rows", "")
  #"Ligand" !is.na &character
  ligand_note <-
    ifelse(any(is.na(unique(
      sample_sheet$Ligand
    ))) == TRUE, "Empty rows", "")
  #"Baseline"    !is.na & numeric
  baseline_note <-
    ifelse(any(is.na(unique(
      sample_sheet$Baseline
    ))) == TRUE, "Empty rows", "")
  baseline_note <-
    ifelse(typeof(unique(sample_sheet$Baseline)) != "double",
           "Incorrect data type",
           baseline_note)
  #"Association"  !is.na & numeric
  association_note <-
    ifelse(any(is.na(unique(
      sample_sheet$Association
    ))) == TRUE, "Empty rows", "")
  association_note <-
    ifelse(typeof(unique(sample_sheet$Association)) != "double",
           "Incorrect data type",
           association_note)
  #"Dissociation" !is.na & numeric
  dissociation_note <-
    ifelse(any(is.na(unique(
      sample_sheet$Dissociation
    ))) == TRUE, "Empty rows", "")
  dissociation_note <-
    ifelse(typeof(unique(sample_sheet$Dissociation)) != "double",
           "Incorrect data type",
           association_note)
  #"Asso. Skip" !is.na & numeric
  assoskip_note <-
    ifelse(any(is.na(unique(
      sample_sheet$`Asso. Skip`
    ))) == TRUE, "Empty rows", "")
  assoskip_note <-
    ifelse(typeof(unique(sample_sheet$`Asso. Skip`)) != "double",
           "Incorrect data type",
           assoskip_note)
  #"Disso. Skip" !is.na & numeric
  dissoskip_note <-
    ifelse(any(is.na(unique(
      sample_sheet$`Disso. Skip`
    ))) == TRUE, "Empty rows", "")
  dissoskip_note <-
    ifelse(typeof(unique(sample_sheet$`Disso. Skip`)) != "double",
           "Incorrect data type",
           dissoskip_note)
  #"All Concentrations" !is.na &character
  conc_note <-
    ifelse(any(is.na(
      unique(sample_sheet$`All Concentrations`)
    )) == TRUE, "Empty rows", "")
  conc_note <-
    ifelse(typeof(unique(sample_sheet$`All Concentrations`)) != "character", "Incorrect data type", conc_note)
  #"Incl. Conc." !is.na &character
  inclconc_note <-
    ifelse(any(is.na(
      unique(sample_sheet$`All Concentrations`)
    )) == TRUE, "Empty rows", "")
  inclconc_note <-
    ifelse(typeof(unique(sample_sheet$`All Concentrations`)) != "character",
           "Incorrect data type",
           inclconc_note)
  #"Bulkshift" !is.na & %in% c("Y", "N")
  bulkshift_note <-
    unique(ifelse((
      !unique(sample_sheet$Bulkshift) %in% c("Y", "N")
    ),  "Only Y and N values allowed",  ""))
  #"Regen." !is.na & %in% c("Y", "N")
  regen_note <-
    unique(ifelse((
      !unique(sample_sheet$Regen.) %in% c("Y", "N")
    ),  "Only Y and N values allowed",  ""))
  #"Bsl Start"  !is.na & numeric
  bslstart_note <-
    ifelse(any(is.na(unique(
      sample_sheet$`Bsl Start`
    ))) == TRUE, "Empty rows", "")
  bslstart_note <-
    ifelse(typeof(unique(sample_sheet$`Bsl Start`)) != "double",
           "Incorrect data type",
           bslstart_note)
  #"Base Corr"  !is.na & %in% c("Y", "N")
  basecorr_note <-
    unique(ifelse((
      !unique(sample_sheet$`Base Corr`) %in% c("Y", "N")
    ),  "Only Y and N values allowed",  ""))

  parameter <- c("Columns overview", defaultcolumnnames)
  flag = c(
    columns_note,
    incl_note,
    block_note,
    position_note,
    ligand_note,
    baseline_note,
    association_note,
    dissociation_note,
    assoskip_note,
    dissoskip_note,
    conc_note,
    inclconc_note,
    bulkshift_note,
    regen_note,
    bslstart_note,
    basecorr_note,
    "",
    "",
    "",
    ""
  )
  check1 <- data.frame(parameter, flag, sample_sheet_path)
  #change to txt
  if (any(check1$flag != "")) {
    flagspresent <- TRUE
    stop(check1$flag)
  } else {

    flagspresent <- FALSE
  }
  flagspresent
}

#check titration data
check_titration_data <- function(titration_data, data_file_path) {

  if (dim(titration_data)[1] == 0) {
    check2 <- "empty data set :"
    stop(check2)
    # flagspresent <- TRUE
    # print(paste(check2, data_file_path))
    # return(flagspresent)
  }
  columns_title <- names(titration_data)
  # this does not work for large number of columns (i.e. X1000 looks like an error)
  #  columns_note <- unique(ifelse(any(!grepl("([X-Y])(...)([0-9])", columns_title)), "Incorrect naming of the columns.", ""))
  columns_note <- ""

    ifelse(
      any(unique(sapply(
        titration_data, typeof
      )) != "double") == T,
      paste(columns_note, "Incorrect data type for some columns"),
      columns_note
    )

  if (columns_note != "") {
    check2 <- data.frame(columns_note, data_file_path)
    stop(check2$columns_note)
  } else {
    flagspresent <- FALSE
  }
  flagspresent
}

check_sample_and_data_match <- function(sample_info, ligand_and_ROI){
# At this point, the sample sheet is expanded, so it has 384 rows
# Also, the ith row corresponds to the ith ROI

  if (is.null(ligand_and_ROI))
    return(NULL)

  nrows <- dim(sample_info)[1]

  for (i in 1:nrows){

   #   if (sample_info$Incl.[i] == "N")
  #       next()

      num_conc <- stringr::str_count(sample_info$`All Concentrations`[i], ",") + 1

      ligand_and_ROI %>%
        dplyr::filter(.data$ROI == sample_info$ROI[i]) -> rows_for_this_spot

      num_conc_data <- dim(rows_for_this_spot)[1]

      if (num_conc != num_conc_data)
        return(paste("The number of columns in the titration data for spot ",
                     sample_info$ROI[i],
                     "does not match the number of concentrations in the sample sheet"))


      concentrations <-
        as.numeric(purrr::flatten(stringr::str_split(sample_info$`All Concentrations`[i], ",")))

      if (sum(is.na(concentrations)) > 0)
        return(paste("Invalid concentration value for spot", sample_info$ROI[i]))

      # initialize as all and change if this is false
      incl_concentrations <- concentrations
      num_incl_conc <- num_conc

      if (sample_info$`Incl. Conc.`[i] != "ALL" &
          sample_info$`Incl. Conc.`[i] != "All"){
        incl_concentrations <-
          as.numeric(purrr::flatten(stringr::str_split(sample_info$`Incl. Conc.`[i], ",")))

        num_incl_conc <- length(incl_concentrations)

        if(sum(is.na(incl_concentrations)) > 0)
          return(paste("Invalid include concentration value for spot", i))

       }

      if(sum(incl_concentrations %in% concentrations) != num_incl_conc)
        return(paste("Include concentration value is not a subset of all concentrations for spot", i))


  }

  return(NULL)

}
