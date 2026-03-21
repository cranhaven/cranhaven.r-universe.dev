#' Data Cleaning
#'
#'
#'The \code{data_cleansing} function is a simpler wrapper for data cleaning functions, such as
#' delete variables that values are all NAs;
#' checking dat and target format.
#' delete low variance variables
#' replace null or NULL or blank with NA;
#' encode variables which NAs &  miss value rate is more than 95% as 1,0 ;
#' encode variables which unique value  rate is  more than 95% as 1,0;
#' merge categories of character variables that  is more than 10;
#' transfer time variables to dateformation;
#' remove duplicated observations;
#' process outliers;
#' process NAs.
#' @param dat A data frame with x and target.
#' @param target The name of target variable.
#' @param miss_values  Other extreme value might be used to represent missing values, e.g: -9999, -9998. These miss_values will be encoded to -1 or "missing".
#' @param x_list  A list of x variables.
#' @param ex_cols  A list of excluded variables. Default is NULL.
#' @param obs_id  The name of ID of observations.Default is NULL.
#' @param occur_time  The name of occur time of observations.Default is NULL.
#' @param pos_flag The value of positive class of target variable, default: "1".
#' @param remove_dup  Logical, if TRUE, remove the duplicated observations.
#' @param outlier_proc  Logical, process outliers or not. Default is TRUE.
#' @param missing_proc If logical, process missing values or not. If "median", then Nas imputation with k neighbors median. If "avg_dist", the distance weighted average method is applied to determine the NAs imputation with k neighbors. If "default", assigning the missing values to -1 or "missing", otherwise ,processing the missing values according to the results of missing analysis.
#' @param low_var The maximum percent of unique values (including NAs) for filtering low variance variables.
#' @param missing_rate The maximum percent of missing values for recoding values to missing and non_missing.
#' @param merge_cat The minimum number of categories for merging categories of character variables.
#' @param parallel  Logical, parallel computing or not. Default is FALSE.
#' @param note Logical. Outputs info. Default is TRUE.
#' @param save_data  Logical, save the result or not. Default is FALSE.
#' @param file_name  The name for periodically saved data file. Default is NULL.
#' @param dir_path The path for periodically saved data file. Default is tempdir().
#'
#' @return A preprocessed data.frame
#' @seealso
#' \code{\link{remove_duplicated}},
#' \code{\link{null_blank_na}},
#' \code{\link{entry_rate_na}},
#' \code{\link{low_variance_filter}},
#' \code{\link{process_nas}},
#' \code{\link{process_outliers}}
#' @examples
#' #data cleaning
#' dat_cl = data_cleansing(dat = UCICreditCard[1:2000,],
#'                        target = "default.payment.next.month",
#'                        x_list = NULL,
#'                        obs_id = "ID",
#'                        occur_time = "apply_date",
#'                        ex_cols = c("PAY_6|BILL_"),
#'                        outlier_proc = TRUE,
#'                        missing_proc = TRUE,
#'                        low_var = TRUE,
#'                        save_data = FALSE)
#'
#' @importFrom dplyr group_by mutate summarize  summarise n  count %>% filter mutate_if
#' @importFrom data.table fwrite melt fread dcast
#' @export
data_cleansing = function (dat, target = NULL, obs_id = NULL, occur_time = NULL, 
                           pos_flag = NULL, x_list = NULL, ex_cols = NULL, miss_values = NULL, 
                           remove_dup = TRUE, outlier_proc = TRUE, missing_proc = "median", 
                           low_var = 0.999, missing_rate = 0.999, merge_cat = TRUE, note = TRUE, 
                           parallel = FALSE, save_data = FALSE, file_name = NULL, dir_path = tempdir()) 
{
  opt = options(scipen = 200, warn = -1, stringsAsFactors = FALSE, digits = 10)
  if (note)cat_line("-- Cleansing data", col = love_color("dark_green"))
  dat = checking_data(dat = dat, target = target, pos_flag = pos_flag, 
                      note = note)
  dat = dat[unique(names(dat))]
  if (save_data) {
    dir_path = ifelse(!is.character(dir_path), tempdir(), 
                      dir_path)
    if (!dir.exists(dir_path)) 
      dir.create(dir_path)
    if (!is.character(file_name)) 
      file_name = NULL
  }
  if (is.null(x_list)) {
    x_list = get_names(dat = dat, types = c("logical", 
                                            "factor", "character", "numeric", 
                                            "integer64", "integer", "double", 
                                            "Date", "POSIXlt", "POSIXct", "POSIXt"), 
                       ex_cols = ex_cols, get_ex = FALSE)
  }
  ex_x_cols = ex_x_cols2 = NULL
  ex_x_cols = get_names(dat = dat, types = c("logical", 
                                             "factor", "character", "numeric", "integer64", 
                                             "integer", "double", "Date", "POSIXlt", 
                                             "POSIXct", "POSIXt"), ex_cols = ex_cols, 
                        get_ex = TRUE)
  if (length(ex_x_cols) > 0) {
    cat_line("-- Following variables are excluded:", 
             col = love_color("dark_green"))
    cat_bullet(paste0(format(ex_x_cols)), col = "darkgrey")
  }
  if (length(dat) > 0) {
    if (!is.null(x_list)) {
      dat = dat[intersect(names(dat), unique(unlist(c(obs_id, 
                                                      target, occur_time, x_list))))]
    }
    dat = null_blank_na(dat = dat, miss_values = miss_values, 
                        note = note) %>% time_transfer(date_cols = NULL, 
                                                       ex_cols = c(obs_id, target, ex_cols), note = note)
    date_x_list = get_names(dat = dat, types = c("Date", 
                                                 "POSIXlt", "POSIXct", "POSIXt"), 
                            ex_cols = c(obs_id, target, occur_time, ex_cols), 
                            get_ex = FALSE)
    flag_list = c(obs_id, occur_time, target)
    if ((is.logical(low_var) && low_var) || (is.numeric(low_var) && 
                                             (low_var > 0 & low_var < 1))) {
      lvp = ifelse(is.numeric(low_var) && (low_var > 0 & 
                                             low_var < 1), low_var, 0.9999)
      dat = dat[!colAllnas(dat)] %>% low_variance_filter(lvp = lvp, 
                                                         note = note, ex_cols = c(date_x_list, flag_list, 
                                                                                  ex_x_cols))
      x_list_2 = get_names(dat = dat, types = c("logical", 
                                                "factor", "character", "numeric", 
                                                "integer64", "integer", "double", 
                                                "Date", "POSIXlt", "POSIXct", 
                                                "POSIXt"), ex_cols = NULL, get_ex = FALSE)
      ex_x_cols2 = setdiff(x_list, x_list_2)
      ex_x_cols = unique(c(ex_x_cols, ex_x_cols2))
      if (length(ex_x_cols2) > 0) {
        cat_line("-- Following variables are excluded by low variance:", 
                 col = love_color("dark_green"))
        cat_bullet(paste0(format(ex_x_cols2)), col = "darkgrey")
      }
    }

    if ((is.logical(missing_rate) && missing_rate) || (is.numeric(missing_rate) && 
                                                       (missing_rate > 0 & missing_rate < 1))) {
      nr = ifelse((is.numeric(missing_rate) && (missing_rate > 
                                                  0 & missing_rate < 1)), missing_rate, 0.9999)
      dat = entry_rate_na(dat = dat, nr = nr, note = note)
    }
    dat = char_to_num(dat = dat, ex_cols = c(date_x_list, 
                                             flag_list, ex_x_cols), note = note)
    if (remove_dup) {
      dat = remove_duplicated(dat = dat, obs_id = obs_id, 
                              occur_time = occur_time, note = note)
    }
    if ((is.logical(merge_cat) && merge_cat) || (is.numeric(merge_cat) && 
                                                 (merge_cat > 1))) {
      if (is.numeric(merge_cat) && merge_cat >= 1) {
        m = merge_cat
      }
      else {
        m = 10
      }
      dat = merge_category(dat = dat, ex_cols = c(date_x_list, 
                                                  flag_list, ex_x_cols), m = m, note = note)
    }
    
    date_x_list = get_names(dat = dat, types = c("Date", 
                                                 "POSIXlt", "POSIXct", "POSIXt"), 
                            ex_cols = c(obs_id, target, occur_time, ex_cols), 
                            get_ex = FALSE)
    char_x_list = get_names(dat = dat, types = c("factor", 
                                                 "character"), ex_cols = c(date_x_list, flag_list, 
                                                                           ex_x_cols), get_ex = FALSE)
    num_x_list = get_names(dat = dat, types = c("logical", 
                                                "numeric", "integer", "double", 
                                                "integer64"), ex_cols = c(date_x_list, flag_list, 
                                                                          ex_x_cols), get_ex = FALSE)
    flag_list = c(obs_id, occur_time, target)
    dat = dat[, c(flag_list, date_x_list, char_x_list, num_x_list)]
    
    if (outlier_proc) {
      dat = process_outliers(dat = dat, target = target, 
                             x_list = num_x_list, ex_cols = NULL, parallel = parallel, 
                             note = FALSE, save_data = save_data, file_name = file_name, 
                             dir_path = dir_path)
    }
    if ((is.logical(missing_proc) && missing_proc) || (is.character(missing_proc) && 
                                                       is.element(missing_proc, c("median", "avg_dist", 
                                                                                  "default")))) {
      if (is.character(missing_proc) && is.element(missing_proc, 
                                                   c("median", "avg_dist", "default"))) {
        method = missing_proc
        dat = process_nas(dat = dat, class_var = FALSE, 
                          x_list = c(num_x_list, char_x_list), ex_cols = NULL, 
                          parallel = parallel, method = method, note = FALSE, 
                          save_data = FALSE)
      } else {
        dat = process_nas(dat = dat, class_var = FALSE, 
                          x_list = c(num_x_list, char_x_list), ex_cols = NULL, 
                          parallel = parallel, method = "default", 
                          note = FALSE, save_data = FALSE)
      }
    }
    if (save_data) {
      save_data(dat, dir_path = dir_path, file_name = ifelse(is.null(file_name), 
                                                             "data_cleansing", paste(file_name, "data_cleansing", 
                                                                                     sep = ".")), append = FALSE, note = note)
    }
  }
  return(dat)
  options(opt)
}



#' Remove Duplicated Observations
#'
#' \code{remove_duplicated} is the function to remove duplicated observations
#'
#' @param dat A data frame with x and target.
#' @param target The name of target variable.
#' @param obs_id The name of ID of observations. Default is NULL.
#' @param occur_time The name of occur time of observations.Default is NULL.
#' @param note   Logical.Outputs info.Default is TRUE.
#'
#' @return  A data.frame
#'
#' @examples
#' datss = remove_duplicated(dat = UCICreditCard,
#' target = "default.payment.next.month",
#' obs_id = "ID", occur_time =  "apply_date")
#' @importFrom dplyr group_by mutate summarize  summarise n  count %>% filter mutate_if distinct ungroup
#' @export


remove_duplicated = function(dat = dat, obs_id = NULL, occur_time = NULL,
                              target = NULL , note = FALSE) {
    if(note)cat_line("-- Removing duplicated observations", col = love_color("dark_green"))

    y_1 = id_1 = apply_date_1 = NULL
    if (!is.null(obs_id)) {
        if (!is.null(target)) {
            if (!is.null(occur_time)) {
                dat[c("id_1", "apply_date_1", "y_1")] = dat[c(obs_id, occur_time, target)]
                if (is.numeric(dat$y_1)) {
                    dat = dat %>% dplyr::group_by(id_1) %>%
                    dplyr::filter( y_1 == max(y_1) & apply_date_1 == max(apply_date_1)) %>%
                    dplyr::distinct(id_1, .keep_all = TRUE) %>%
                    dplyr::ungroup()
                } else {
                    dat = dat %>% dplyr::group_by(id_1) %>% base :: as.factor(y_1) %>%
                        dplyr::filter(y_1 == levels(y_1)[which.min(table(y_1))] & apply_date_1 == max(apply_date_1)) %>%
                        dplyr::distinct(id_1, .keep_all = TRUE) %>%
                        dplyr::ungroup()
                }
            } else {
                dat[c("id_1", "y_1")] = dat[c(obs_id, target)]
                if (is.numeric(dat$y_1)) {
                    dat = dat %>% dplyr::group_by(id_1) %>%
                    dplyr::filter(y_1 == max(y_1) )%>%
                    dplyr::distinct(id_1, .keep_all = TRUE) %>%
                    dplyr::ungroup()
                } else {
                    dat = dat %>% dplyr::group_by(id_1) %>% as.factor(y_1) %>%
                        dplyr::filter(y_1 == levels(y_1)[which.min(table(y_1))]) %>%
                        dplyr::distinct(id_1, .keep_all = TRUE) %>%
                        dplyr::ungroup()
                }
            }
        } else {
            if (!is.null(occur_time)) {
                dat[c("id_1", "apply_date_1")] = dat[c(obs_id, occur_time)]
                dat = dat %>% dplyr::group_by(id_1) %>%
                dplyr::filter(apply_date_1 == max(apply_date_1)) %>%
                dplyr::distinct(id_1, .keep_all = TRUE) %>% dplyr::ungroup()
            } else {
                dat[c("id_1")] = dat[c(obs_id)]
                dat = dat %>% dplyr::group_by(id_1) %>%
                dplyr::distinct(id_1, .keep_all = TRUE) %>% dplyr::ungroup()
            }
        }
        dat[c("id_1", "apply_date_1", "y_1")] = NULL
    }else{
	 dat =  dplyr::distinct(dat, .keep_all = TRUE) %>% dplyr::ungroup()
	}
    quick_as_df(dat)
}



#' Encode NAs
#'
#' \code{null_blank_na} is the function to  replace null ,NULL, blank or other missing vaules with NA.
#'
#' @param dat A data frame with x and target.
#' @param miss_values  Other extreme value might be used to represent missing values, e.g: -9999, -9998. These miss_values will be encoded to -1 or "missing".
#' @param note   Logical.Outputs info.Default is TRUE.
#'
#' @return  A data.frame
#'
#' @examples
#' datss = null_blank_na(dat = UCICreditCard[1:1000, ], miss_values =list(-1,-2))
#' @export
#' @importFrom dplyr group_by mutate summarize  summarise n  count %>% filter mutate_if
null_blank_na = function(dat, miss_values = NULL, note = FALSE) {
    if(note)cat_line("-- Replacing null or blank or miss_values with NA", col = love_color("dark_green"))
  dat = checking_data(dat)
    dat %>% mutate_if(is.character, function(x) {
        ifelse(is.null(x) | x == "" | x == "null" | x == "NULL" | x == " " | x %in% miss_values, NA, x)}) %>%
      mutate_if(is.numeric, function(x) {
        ifelse(x %in% miss_values, NA, x)
        })
}


#' Max Percent of missing Value
#'
#' \code{entry_rate_na} is the function to recode variables with missing values up to a certain percentage with missing and non_missing.
#'
#' @param dat A data frame with x and target.
#' @param nr  The maximum percent of NAs.
#' @param note   Logical.Outputs info.Default is TRUE.
#'
#' @return  A data.frame
#'
#' @examples
#' datss = entry_rate_na(dat = lendingclub[1:1000, ], nr = 0.98)
#' @export


entry_rate_na = function(dat, nr = 0.98, note = FALSE) {
    if(note)cat_line(paste("-- Processing NAs & special value rate is more than", nr), col = love_color("dark_green"))
    n_row = nrow(dat)
    NAs_rate = vapply(dat, function(x) {
        sum(is.na(x)) / n_row
    }, FUN.VALUE = numeric(1))
    dat[which(NAs_rate > nr)] = lapply(dat[which(NAs_rate > nr)], function(x) {
        if (is.element(class(x), c('numeric', 'integer', 'double'))) {
            ifelse( is.na(x), "missing", "non_missing")
        } else {
                x
        }
    })
    return(dat)
}



#' Filtering Low Variance Variables
#'
#' \code{low_variance_filter} is for removing variables with repeated values up to a certain percentage.
#'
#' @param dat A data frame with x and target.
#' @param lvp  The maximum percent of unique values (including NAs).
#' @param only_NA Logical, only process variables which NA's rate are more than lvp.
#' @param note Logical.Outputs info.Default is TRUE.
#' @param ex_cols  A list of excluded variables. Default is NULL.
#' @return  A data.frame
#'
#' @examples
#' dat = low_variance_filter(lendingclub[1:1000, ], lvp = 0.9)
#'
#' @export

low_variance_filter = function(dat, lvp = 0.97, only_NA = FALSE, note = FALSE, ex_cols = NULL) {
  if (note) {
    cat_line(paste("-- Deleting low variance variables"), col = love_color("dark_green"))
  }
  dat = dat[which(colSums(is.na(dat)) != nrow(dat) | names(dat) %in% ex_cols)]
  is_na = as.data.frame(abs(is.na(dat)))
  dat = dat[which(apply(is_na, 2, function(x) sum(is.na(x)) / nrow(is_na)) < lvp | names(dat) %in% ex_cols)]
  dat = as.data.table(dat)
  
  if (!only_NA) {
    n_row = nrow(dat)
    low_variance = vapply(names(dat), function(x) max(dat[, .N / n_row, x][, "V1"]),
                          FUN.VALUE = numeric(1))
    dat = dat[,low_variance < lvp | names(dat) %in% ex_cols,with = FALSE]
  }
  dat = quick_as_df(dat)
  return(dat)
}