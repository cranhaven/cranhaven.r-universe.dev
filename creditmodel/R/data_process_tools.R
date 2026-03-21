#' Train-Test-Split
#'
#' \code{train_test_split} Functions for partition of data.
#' @param dat A data.frame with independent variables and target variable.
#' @param prop The percentage of train data samples after the partition.
#' @param split_type  Methods for partition.
#' \itemize{
#'   \item "Random" is to split train & test set randomly.
#'   \item "OOT" is to split by time for observation over time test.
#'   \item "byRow" is to split by rownumbers.
#' }
#' @param occur_time The name of the variable that represents the time at which each observation takes place. It is used for "OOT" split.
#' @param cut_date Time points for spliting data sets, e.g. : spliting Actual and Expected data sets.
#' @param start_date The earliest occurrence time of observations.
#' @param save_data Logical, save results in locally specified folder. Default is FALSE.
#' @param file_name The name for periodically saved data file. Default is "dat".
#' @param dir_path The path for periodically saved data file. Default is "./data".
#' @param seed  Random number seed. Default is 46.
#' @param note Logical. Outputs info. Default is TRUE.
#' @return A list of indices (train-test)
#' @examples
#' train_test = train_test_split(lendingclub,
#' split_type = "OOT", prop = 0.7,
#' occur_time = "issue_d", seed = 12, save_data = FALSE)
#' dat_train = train_test$train
#' dat_test = train_test$test
#' @importFrom stats quantile ecdf
#' @importFrom cli cat_rule cat_line cat_bullet
#' @export
train_test_split = function( dat, prop = 0.7, split_type = "Random",occur_time = NULL,
                              cut_date = NULL, start_date = NULL, save_data = FALSE,
                              dir_path = tempdir(), file_name = NULL, note = FALSE, seed = 43) {

  if (prop > 1 || !is.numeric(prop)) {
    warning("-- prop is not a numeric or more than 1,  reset to 0.7.\n")
    prop = 0.7
  }
  if (!any(is.element(split_type, c("OOT", "Random", "byRow")))) {
    stop("split_type must be either 'OOT' or 'Random' or 'byRow'.\n")
  }
  if (length(split_type) > 1) {
    warning("-- your split_type is more than one and only the first one is selected.\n")
  }
  if (length(split_type) == 0) {
    warning("-- split_type is missing, set 'Random' by default.\n")
    split_type = "Random"
  }
  if (split_type[1] == "OOT" & !is.null(occur_time) && any(names(dat) == occur_time)) {
    dat = time_transfer(dat, date_cols = occur_time)
    if (is_date(dat[, occur_time])) {
      if (is.null(cut_date)) {
        cut_date = date_cut(dat_time = dat[, occur_time], pct = prop)
      }
      if (is.null(start_date)) {
        start_date = date_cut(dat_time = dat[, occur_time], pct = 0)
      }
      dat[, occur_time] = as.Date(dat[, occur_time])
      test = dat[which(dat[, occur_time] >= cut_date),]
      train = dat[which(dat[, occur_time] >= start_date & dat[, occur_time] < cut_date),]
      if (note){
        cat_line("-- train_test_split:", col = love_color("dark_green"))
        split_out = list(Total = nrow(dat), Train = nrow(train), Test = nrow(test))
        cat_bullet(paste0(format(names(split_out)), ": ", unname(split_out),
                          " (", sapply(split_out,function(x)as_percent(x/nrow(dat),2)),")"), col = "darkgrey")
      }
    } else {
      if (!is.null(seed)) set.seed(seed) else set.seed(46)
      sub = sample(1:nrow(dat), round(nrow(dat) * prop))
      train = dat[sub,]
      test = dat[-sub,]
      if (note){
        cat_line("-- train_test_split:", col = love_color("dark_green"))
        split_out = list(Total = nrow(dat), Train = nrow(train), Test = nrow(test))
        cat_bullet(paste0(format(names(split_out)), ": ", unname(split_out),
                          " (", sapply(split_out,function(x)as_percent(x/nrow(dat),2)),")"), col = "darkgrey")
      }
      warning(paste(occur_time, "is  not date or time, unable to use OOT , split random.\n"))
    }

  }else{
    if (split_type[1] == "byRow") {
        sub = 1:round(nrow(dat) * prop)
        train = dat[sub,]
        test = dat[-sub,]
        if (note){
          cat_line("-- train_test_split:", col = love_color("dark_green"))
          split_out = list(Total = nrow(dat), Train = nrow(train), Test = nrow(test))
          cat_bullet(paste0(format(names(split_out)), ": ", unname(split_out),
                            " (", sapply(split_out,function(x)as_percent(x/nrow(dat),2)),")"), col = "darkgrey")
        }
	 }else{

      if (!is.null(seed)) set.seed(seed) else set.seed(46)
      sub = sample(1:nrow(dat), round(nrow(dat) * prop))
      train = dat[sub,]
      test = dat[-sub,]
      if (note){
        cat_line("-- train_test_split:", col = love_color("dark_green"))
        split_out = list(Total = nrow(dat), Train = nrow(train), Test = nrow(test))
        cat_bullet(paste0(format(names(split_out)), ": ", unname(split_out),
                          " (", sapply(split_out,function(x)as_percent(x/nrow(dat),2)),")"), col = "darkgrey")
      }
    }
  }

  if (save_data) {
    dir_path = ifelse(!is.character(dir_path),
                      tempdir(), dir_path)
    if (!dir.exists(dir_path)) dir.create(dir_path)
    if (!is.character(file_name)) file_name = NULL
    save_data(train, file_name = ifelse(is.null(file_name), "train", paste0(file_name, "_train")), dir_path = dir_path)
    save_data(test, file_name = ifelse(is.null(file_name), "test", paste0(file_name, "_test")), dir_path = dir_path)
  }
  return(list(test = test, train = train))
}



#' Stratified Folds
#'
#' this function creates stratified folds for cross validation.
#'
#' @param dat  A data.frame.
#' @param k  k is an integer specifying the number of folds.
#' @param occur_time time variable for creating OOT folds. Default is NULL.
#' @param seed A seed. Default is 46.
#' @return a list of indices
#' @examples
#' sub = cv_split(UCICreditCard, k = 30)[[1]]
#' dat = UCICreditCard[sub,]
#' @importFrom stats quantile ecdf
#' @export

cv_split = function(dat, k = 5, occur_time = NULL, seed = 46) {
    cv_list = list()
    dat = checking_data(dat = dat, occur_time = occur_time)
    if (!is.null(seed)) set.seed(seed) else set.seed(46)
    if (!is.null(occur_time) && is.element(occur_time, names(dat)) &&
            is_date(dat[, occur_time])) {
        date_n = quantile(ecdf(dat[, occur_time]), seq(0, 1, by = 0.01))
        date_q = as.double(sub("%", "", names(date_n))) / 100
        prop = round(1 / k,  2)
        date_temp = date_n[which(date_q == prop)]
        if (length(date_temp) >0 && n_char(date_temp) <= 7) {
            for (i in 1:k) {
                cv_list[[i]] = which(dat[, occur_time] >= min(as.Date(date_n[which(date_q >= prop * (k - i))],
                                                                      origin = "1970-01-01")) &
                                       dat[, occur_time] < min(as.Date(date_n[which(date_q >= prop * (k - i + 1))],
                                                                       origin = "1970-01-01")))
            }
        } else {
            for (i in 1:k) {
                cv_list[[i]] = which(dat[, occur_time] >= min(as.Date.POSIXct(date_n[which(date_q >= prop * (k - i))],
                                                                              origin = "1970-01-01")) &
                                       dat[, occur_time] < min(as.Date.POSIXct(date_n[which(date_q >= prop * (k - i + 1))],
                                                                               origin = "1970-01-01")))
            }
        }
        null_cv = which(sapply(cv_list, function(i) length(i) == 0))
        if (length(null_cv) > 0) {
            not_in_cv = which(!1:nrow(dat) %in% unlist(cv_list))
            for (i in null_cv[which(null_cv < k)]) {
                cv_list[[i]] = sample(not_in_cv, ceiling(length(not_in_cv) / length(null_cv)))
            }
            cv_list[[k]] = which(!1:nrow(dat) %in% unlist(cv_list))
        }

        cv_list = cv_list[which(sapply(cv_list, function(i) length(i) > 0))]

    } else {
        nr = nrow(dat)
        k = ceiling(k)
        if (!is.null(seed)) set.seed(seed) else set.seed(46)
        chaos_n = sample(rep(1:k, ceiling(nr / k))[1:nr], nr)
        dat_seq = 1:nr
        cv_list = lapply(1:k, function(x) dat_seq[chaos_n == x])
    }
    return(cv_list)
}



#' Packages required and intallment
#'
#' \code{require_packages} is function for librarying required packages and  installing missing packages if needed.
#' @param pkg A list or vector of names of required packages.
#' @param ... Packages need loaded
#' @return  packages installed and library.
#' @examples
#' \dontrun{
#' require_packages(data.table, ggplot2, dplyr)
#' }
#' @importFrom cli cat_rule cat_line cat_bullet
#' @importFrom utils  capture.output  data  install.packages  installed.packages
#' @export
require_packages = function(..., pkg = as.character(substitute(list(...)))) {
    opt = options("warn" = -1)
    new_pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new_pkg) > 0) {
        cat_rule("Installing missing packages if needed", col = love_color("light_cyan"))
        install.packages(new_pkg, dependencies = TRUE)
        cat_line("-- Following packages  are installed:", col = love_color("dark_green"))
        cat_bullet(paste0(format(new_pkg)[-1]), col = "darkgrey")
    }
    cat_line("-- Following packages  are loaded:", col = love_color("dark_green"))
    cat_bullet(paste0(format(pkg)[-1]), col = "darkgrey")
    sapply(pkg, require, ch = TRUE)
    options(opt)
}


#' List as data.frame quickly
#'
#' \code{quick_as_df} is function for fast dat frame  transfromation.
#' @param df_list A list of data.
#' @return  packages installed and library,
#'
#' @examples
#'
#' UCICreditCard = quick_as_df(UCICreditCard)
#'
#' @export
quick_as_df = function(df_list){
    class(df_list) = "data.frame"
    attr(df_list, "row.names") = .set_row_names(length(df_list[[1]]))
    df_list
}


#' Save data
#'
#' \code{save_data} is for saving a data.frame or a list fast.
#' @param ... datasets
#' @param files A dataset or a list of datasets.
#' @param file_name The file name of data.
#' @param dir_path  A string. The dir path to save breaks_list.
#' @param note  Logical. Outputs info.Default is TRUE.
#' @param as_list  Logical. List format or data.frame format to save. Default is FALSE.
#' @param row_names  Logical,retain rownames.
#' @param append Logical, append newdata to old.
#' @examples
#' save_data(UCICreditCard,"UCICreditCard", tempdir())
#' @importFrom data.table fwrite fread dcast melt as.data.table
#' @export


save_data = function(..., files = list(...), file_name = as.character(substitute(list(...))), dir_path = getwd(),
                     note = FALSE, as_list = FALSE, row_names = FALSE, append = FALSE) {

  ind_n = sapply(files, function(x) {
    ifelse( length(x)==1,  length(unlist(x))== 1 & is.character(x[1]) & !grepl(":|\\/|\\\\|\\//", x[1]),FALSE )

  })

  ind_p = sapply(files, function(x) {
    ifelse(length(x)==1, length(unlist(x))== 1 & is.character(x[1]) & grepl(":|\\/|\\\\|\\//", x[1]),FALSE )
  })
  if (any(ind_n)) {
    file_name = unlist(files[ind_n])
  }

  if (any(ind_p)) {
    dir_path = unlist(files[ind_p])[1]
  }
  if (!dir.exists(dir_path)) dir.create(dir_path)

  ind_f = sapply(files, function(x) {
    ifelse(length(x)==1, length(unlist(x))== 1 & is.character(x[1]),FALSE )
  })
  if(all(ind_f)){
    files = files[[1]]
    as_list = TRUE
  }else{
    files = files[!ind_f]
  }
  if (length(file_name) < length(files)) {
    file_name = as.character(substitute(list(...)))[2:length(files) + length(file_name)]
    warning("The number of files is greater than the number of filenames,
            using the environment variable name of the file as the filename.\n")
  } else {
    if (is.element("list", file_name[1])) {
      file_name = file_name[-1]
    }

  }

  for (file_n in file_name) {
    if (dir.exists(paste0(dir_path, '/', file_n, ".csv"))) file.remove(list.files(paste0(dir_path, '/', file_n, ".csv"),
                                                                                  recursive = TRUE, full.names = TRUE))
  }
  if (note) {
    cat_line(paste("-- Saving", paste(file_name, collapse = " "), "to:"), col = love_color("dark_green"))
    cat_bullet(paste0(format(paste0(dir_path, '/', file_name, ".csv"))), col = "darkgrey")

  }

  if (as_list) {
    for (i in seq_along(files)) {
	  
      if (dir.exists(paste0(dir_path, '/', file_name[i], ".csv"))) file.remove(list.files(paste0(dir_path, '/', file_name[i], ".csv"),
                                                                                          recursive = TRUE, full.names = TRUE))
      if(length(files[[i]])>0){
	  data.table::fwrite(list(files[[i]]), paste0(dir_path, '/', file_name[i], ".csv"), append = append, col.names = FALSE)
	  }
    }

  } else {
    for (i in seq_along(files)) {
      if (dir.exists(paste0(dir_path, '/', file_name[i], ".csv"))) file.remove(list.files(paste0(dir_path, '/', file_name[i], ".csv"),
	                                                                                            recursive = TRUE, full.names = TRUE))
      if(length(files[[i]])>0){
	  data.table::fwrite(as.data.table(files[[i]]), paste0(dir_path, '/', file_name[i], ".csv"), append = append,col.names = TRUE)
	  }
    }
  }
}

#' Read data
#'
#' \code{read_data} is for loading data, formats like csv, txt,data and so on.
#' @param path Path to file or file name in working directory & path to file.
#' @param encoding Default is "unknown". Other possible options are "UTF-8" and "Latin-1".
#' @param header Does the first data line contain column names?
#' @param pattern An optional regular expression. Only file names which match the regular expression will be returned.
#' @param sep The separator between columns.
#' @param stringsAsFactors  Logical. Convert all character columns to factors?
#' @param select  A vector of column names or numbers to keep, drop the rest. 
#' @param drop A vector of column names or numbers to drop, keep the rest. 
#' @param nrows The maximum number of rows to read.
#' @importFrom data.table fwrite fread dcast melt
#' @importFrom dplyr distinct
#' @importFrom cli cat_rule cat_line cat_bullet
#' @importFrom parallel detectCores
#' @export



read_data = function(path, pattern = NULL, encoding = "unknown", header = TRUE, sep = "auto", stringsAsFactors = FALSE, select = NULL, drop = NULL, nrows = Inf) {
	file_names = sort(list.files(path, pattern = pattern))
	numCores = parallel::detectCores() - 1
	if (length(file_names) > 0) {
		file_names = file_names[grepl("[.]", file_names)]

		file_format = c()
		for (i in file_names) {
			path_file = paste(path, i, sep = "/")
			file_format[i] = check_data_format(path_file)
		}

		file_format_tx = file_format[!is.na(file_format) && !grepl('xl\\S{1,2}$', file_format)]

		dt_list = list()
		if (length(file_format_tx) > 0) {
			for (file_name in names(file_format_tx)) {
				file_n = gsub('.csv$|.txt$|.CSV$', "", file_name)
				dt_list[[file_n]] = dplyr::distinct(data.table::fread(paste(path, file_name, sep = "/"),
																		  encoding = encoding,
																		  header = header,
																		  sep = sep,
																		  data.table = getOption("datatable.fread.datatable", FALSE),
																		  integer64 = getOption("datatable.integer64", "character"),
																		  nThread = numCores, drop = drop,
																		  fill = TRUE, select = select, nrows = nrows,
																		  stringsAsFactors = stringsAsFactors))
			}
		}
		cat_line("-- Input files:", col = love_color("dark_green"))
		cat_bullet(paste0(format(names(file_format_tx))), col = "darkgrey")
		return(dt_list)
	} else {

		file_format = check_data_format(path)
		file_format_tx = file_format[!is.na(file_format) && !grepl('xl\\S{1,2}$', file_format)]
		#file_format_tc = file_format[!is.na(file_format)& file_format %alike% 'csv$|txt$|CSV$']
		if (length(file_format_tx) > 0) {
			data.table::fread(paste(path, sep = "/"),
									encoding = encoding,
									header = header,
									sep = sep,
									data.table = getOption("datatable.fread.datatable", FALSE),
									integer64 = getOption("datatable.integer64", "character"),
									nThread = numCores, drop = drop,
									fill = TRUE, select = select, nrows = nrows,
									stringsAsFactors = stringsAsFactors)
		} else {
			stop(paste0("Cannot open file '", path, "'"))
		}
	}
}

#' @rdname read_data
check_data_format = function(path) {
  if (file.exists(path)) {
    sig = readBin(path, n = 10, what = "raw")
    xlsx_sig = as.raw(c(
      "0x50", "0x4B", "0x03", "0x04"
    ))
    xls_sig = as.raw(c(
      "0xD0", "0xCF", "0x11", "0xE0", "0xA1", "0xB1", "0x1A", "0xE1"
    ))
    data_format = NA
    if (identical(sig[1:4], xlsx_sig)) {
      data_format = "xlsx"
    } else if (identical(sig[1:4], xls_sig)) {
      data_format = "xls"
    } else if (grepl(".txt$", path)) {
      data_format = "txt"
    } else if (grepl(".csv$|.CVS$", path)) {
      data_format = "csv"
    }

  } else {
    stop(paste0("Cannot open file '", path, "': no such file or directory"))
  }
  data_format
}

#' multi_left_join
#'
#' \code{multi_left_join} is for left jion a list of datasets fast.
#' @param ... Datasets need join
#' @param df_list A list of datasets.
#' @param key_dt Name or index of Key table to left join.
#' @param by  Name of Key columns to join.
#' @examples
#' multi_left_join(UCICreditCard[1:10, 1:10], UCICreditCard[1:10, c(1,8:14)],
#' UCICreditCard[1:10, c(1,20:25)], by = "ID")
#' @export

multi_left_join = function(..., df_list = list(...), key_dt = NULL, by = NULL) {

	if (is.null(by)) {
		for (i in 1:length(df_list)) {
			intesect_name[[i]] = names(quick_as_df(df_list[[i]]))
		}
		by = unique(Reduce('intersect', intesect_name))
		if (length(by) > 0) {
			cat(paste("by is missing, Joining by", "(", paste(by,collapse = ','), ")", ".\n ", collapse = ''))
		} else {
			stop("Either key columns 'by' is missing or dataset list has no intersect names.\n ")
		}
	}
	if (length(names(by)) > 0) {
		by = names(by)
		for (i in 1:length(df_list)) {
			join_ind = which(names(df_list[[i]]) %in% by)
			names(df_list[[i]])[join_ind] = by[1:length(join_ind)]
		}
		by = unique(by)
	}

	if (!is.null(key_dt)) {
		key_id = df_list[key_dt]
		df_list[key_dt] = NULL
		df_list = append(key_id, df_list)
	}
	for (i in 1:length(df_list)) {
		df_list[[i]] = quick_as_df(df_list[[i]])
		by_id = c(df_list[[i]][by])
		if (length(by_id) == 0) {
			warning(paste(paste0("No column called '",
				by, "'in"), names(df_list[i]), ". Drop it...\n"))
			df_list[[i]] = NULL
		} 
	}
	dat_merge = Reduce(function(x, y) {
		i = 1
		intersect_name = list()
		while (TRUE) {
			intersect_name[[i]] = intersect(colnames(x), colnames(y))
			intersect_name[[i]] = intersect_name[[i]][-which(intersect_name[[i]] %in% by)]
			if (length(intersect_name[[i]]) == 0) break
			names(y)[which(colnames(y) %in% intersect_name[[i]])] = paste(intersect_name[[1]], i, sep = '_')
			i = i + 1
		}
		merge(x, y, by = by, all.x = TRUE)
	}, df_list)

	return(quick_as_df(dat_merge))
}

#' Number of digits
#'
#' \code{digits_num} is for caculating optimal digits number for numeric variables.
#' @param dat_x  A numeric variable.
#' @return  A number of digits
#' @examples
#' \dontrun{
#' digits_num(lendingclub[,"dti"])
#' # 7
#' }
#' @export


digits_num =function(dat_x){
  digits1 = digits2 = 16
  dat_x = unique(unlist(dat_x[!is.na(dat_x)&!dat_x %in% c(-Inf,Inf)]))
  if (length(dat_x) > 0 && any(is.element(class(dat_x), c("integer", "numeric",
                                                          "double")))&& max(dat_x,na.rm = TRUE) > 1e-16) {
    digits1 = vapply(dat_x, function(num) {
      char_num = as.character(gsub("-", "",
                                   num))
      n_num = as.numeric(char_num) %% 1
      if (!is.null(n_num) && !is.na(n_num) && is.numeric(n_num)) {
        if (n_num == 0) {
          t_lens = n_char(char_num)
          left_comma = t_lens
          right_comma = t_lens - left_comma
        } else {
          comma_p = gregexpr("[.]", char_num)[[1]][1]
          t_lens = n_char(char_num)
          left_comma = comma_p - 1
          right_comma = t_lens - 1 - left_comma
        }
        right_comma
      }
    }, FUN.VALUE = numeric(1))
    digits2 = max(digits1)
  }
  digits2 = ifelse(digits2 > 16, 16, digits2)
  return(digits2)
}





#' is_date
#'
#' \code{is_date} is a small function for distinguishing time formats
#' @param x  list or vectors
#' @return  A Date.
#' @examples
#' is_date(lendingclub$issue_d)
#' @export
is_date = function(x){
    any(class(x) %in% c("Date", "POSIXlt", "POSIXct", "POSIXt"))
}

#' Date Time Cut Point
#'
#' \code{date_cut} is  a small function to get date point.
#' @param dat_time  time vectors.
#' @param pct  the percent of cutting. Default: 0.7.
#' @param g  Number of cuts.
#' @return  A Date.
#' @examples
#' date_cut(dat_time = lendingclub$issue_d, pct = 0.8)
#' #"2018-08-01"
#' @importFrom stats quantile ecdf
#' @export

date_cut = function(dat_time, pct = 0.7,g = 100){
  dat_time = as.Date(dat_time)
  if (is_date(dat_time)) {
    date_n = quantile(ecdf(dat_time), seq(0, 1, by = 1/g))
    date_q = round(as.double(sub("%", "", names(date_n))) / 100,3)
    date_temp = date_n[which(date_q == round(pct,3))]
    if (n_char(date_temp) > 7) {
      cut_date= min(as.Date.POSIXct(date_n[which(date_q >= pct)], origin = "1970-01-01"))
    } else {
      cut_date = min(as.Date(date_n[which(date_q >= pct)], origin = "1970-01-01"))
    }
    return(cut_date)
  } else {
    stop(paste("Not Date or Time.\n"))
  }
}


#' Percent Format
#'
#' \code{as_percent} is  a small function for making percent format..
#' @param x  A numeric vector or  list.
#' @param digits  Number of digits.Default: 2.
#' @return  x with percent format.
#' @examples
#' as_percent(0.2363, digits = 2)
#' as_percent(1)
#' @export
as_percent = function(x, digits = 2) {
    x = as.numeric(x)
    pct = round(x, digits) * 100
    x_pct = paste0(pct, ifelse(is.finite(x), "%", ""))
    return(x_pct)
}

#' %islike%
#' Fuzzy String matching
#'
#' @param x  A string.
#' @param y  A string.
#' @return  Logical.
#' @examples
#'  "xyz"  %islike% "yz$"
#' @export

'%islike%' = function(x, y) {
    grx = FALSE
    x = gsub("[^\u4e00-\u9fa5,^a-zA-Z,^0-9,^.,^_,^;^-]", "", x)
    y = gsub("\\{|\\}", "", y)

    if (any(x != '') & any(y != '') & any(!is.null(x)) & any(!is.null(y)) & any(!is.na(x)) & any(!is.na(y))) {
        y = unique(unlist(y))
        y = y[which(y != '')]
        if (length(y[!grepl("\\$|\\|", y)]) > 0) {
            grx = Reduce(function(u, v) paste(u, v, sep = '|'), paste0("^", y[!grepl("\\|", y)], sep = "$"))
        }
        if (length(y[grepl("\\$|\\|", y)]) > 0) {
            grx = paste(grx, y[grepl("\\|", y)], sep = '|')
        }
    }
    grepl(grx, x)
}


#' %alike%
#' Fuzzy String matching
#' @param x  A string.
#' @param y  A string.
#' @return  Logical.
#' @examples
#' "xyz"  %alike% "xy"
#' @export

`%alike%` = function(x, y) {
	x = gsub("[^\u4e00-\u9fa5,^a-zA-Z,^0-9,^.,^_,^;^-]", "", x)
	if (any(x != '') & any(y != '') & any(!is.null(x)) & any(!is.null(y)) & any(!is.na(x)) & any(!is.na(y))) {
		y = unlist(y)
		y = y[which(y != '')]
		x = unlist(x)
		x[which(x != '')]
		grx1 = Reduce(function(u, v) paste(u, v, sep = '|'), y)
		if (any(grepl(grx1, x))) {
			grpl2 = grepl(grx1, x)
		} else {
			grx2 = Reduce(function(u, v) paste(u, v, sep = '|'), x)
			grpl2 = grepl(grx2, y)
		}
	} else {
		grpl2 = grepl(FALSE, x)
	}
	return(grpl2)
}



#'  Rename
#'
#' \code{re_name} is  for renaming variables.
#' @param dat A data frame with vairables to rename.
#' @param newname  New names of vairables.
#' @param oldname  Old names of vairables.
#' @return  data with new variable names.
#' @examples
#' dt = re_name(dat = UCICreditCard, "default.payment.next.month" , "target")
#' names(dt['target'])
#' @export

re_name = function(dat, oldname = c(), newname = c()) {
	if (length(oldname) == length(newname)) {
		name_merge = merge(data.frame(oldname, newname), data.frame(oldname = names(dat), ind = 1:length(dat)), by = "oldname", all.x = TRUE)
		name_merge = name_merge[!is.na(name_merge[, "ind"]),]
		ind = name_merge[, "ind"]
		names(dat)[ind] = as.character(name_merge[, "newname"])
	}
	dat
}


#' Functions for vector operation.
#'
#' @param x  A data.frame or Matrix.
#' @param na.rm  Logical, remove NAs.
#' @return  A data.frame or Matrix.
#' @examples
#' #any row has missing values
#' row_amy =  rowAny(UCICreditCard[8:10])
#' #rows which is all missing values
#' row_na =  rowAllnas(UCICreditCard[8:10])
#' #cols which is all missing values
#' col_na =  colAllnas(UCICreditCard[8:10])
#' #cols which is all zeros
#' row_zero =  colAllzeros(UCICreditCard[8:10])
#' #sum all numbers of a row
#' row_all =  rowAll(UCICreditCard[8:10])
#' #caculate cv of a row
#' row_cv =  rowCVs(UCICreditCard[8:10])
#' #caculate sd of a row
#' row_sd =  rowSds(UCICreditCard[8:10])
#' #caculate sd of a column
#' col_sd =  colSds(UCICreditCard[8:10])
#' @export


rowAny = function(x) rowSums(x, na.rm = TRUE) > 0

#' @rdname rowAny
#' @export
rowAllnas = function(x) rowSums(is.na(x)) == length(x)

#' @rdname rowAny
#' @export
colAllnas = function(x) colSums(is.na(x)) == nrow(x)


#' @rdname rowAny
#' @export
colAllzeros = function(x) colSums(x) == 0

#' @rdname rowAny
#' @export
rowAll = function(x) rowSums(x, na.rm = TRUE) == ncol(x)


#' @rdname rowAny
#' @export

rowCVs = function(x, na.rm = FALSE) {
    ifelse(rowAllnas(x), NA,
            ifelse(rowAny(x) > 0,
            sqrt(rowSums((x - rowMeans(x, na.rm = na.rm)) ^ 2, na.rm = na.rm) / length(x)) / rowMeans(x, na.rm = na.rm), 0))
}

#' @rdname rowAny
#' @export
rowSds = function(x, na.rm = FALSE) sqrt(rowSums((x - rowMeans(x, na.rm = na.rm)) ^ 2, na.rm = na.rm) / length(x))

#' @rdname rowAny
#' @export

colSds = function(x, na.rm = TRUE) {
    lapply(x, function(i)sd(i,na.rm = TRUE))
}

#' @rdname rowAny
#' @export
rowMaxs = function(x, na.rm = FALSE) {
    maxs = apply(x, 1, function(i) ifelse(length(i) > 1, i[which.max(i)], i))
    as.numeric(maxs)
}

#' @rdname rowAny
#' @export
rowMins = function(x, na.rm = FALSE) {
    mins = apply(x, 1, function(i) i[which.min(i)])
    as.numeric(mins)
}

#' @rdname rowAny
#' @export
rowMaxMins = function(x, na.rm = FALSE) {
    max_mins = apply(x, 1, function(i) i[which.max(i)] - i[which.min(i)])
    as.numeric(max_mins)
}

#' @rdname rowAny
#' @export
colMaxMins = function(x, na.rm = FALSE) {
    max_mins = apply(x, 2, function(i) i[which.max(i)] - i[which.min(i)])
    as.numeric(max_mins)
}

#' @rdname rowAny
#' @export
cnt_x = function(x) {
  length(x)
}
#' @rdname rowAny
#' @export
sum_x = function(x) {
  sum_x = sum(x, na.rm = TRUE)
  sum_x[which(sum_x == -Inf || sum_x == Inf || is.na(sum_x))] = NA
  sum_x
}
#' @rdname rowAny
#' @export
max_x = function(x) {
  max_x = max(x, na.rm = TRUE)
  max_x[which(max_x == -Inf || max_x == Inf || is.na(max_x))] = NA
  max_x
}
#' @rdname rowAny
#' @export
min_x = function(x) {
  min_x = min(x, na.rm = TRUE)
  min_x[which(min_x == -Inf || min_x == Inf || is.na(min_x))] = NA
  min_x
}
#' @rdname rowAny
#' @export
avg_x = function(x) {
  mean_x = mean(x, na.rm = TRUE)
  mean_x[which(mean_x == -Inf || mean_x == Inf || is.na(mean_x))] = NA
  mean_x
}
#'  Get Variable Names
#'
#' \code{get_names} is  for getting names of particular classes of variables
#' @param dat A data.frame with independent variables and target variable.
#' @param types  The class or types of variables which names to get. Default: c('numeric', 'integer', 'double')
#' @param ex_cols A list of excluded variables. Regular expressions can also be used to match variable names. Default is NULL.
#' @param get_ex  Logical ,if TRUE, return a list contains names of excluded variables.
#' @return  A list contains names of variables
#' @seealso \code{\link{get_x_list}}
#' @examples
#' x_list = get_names(dat = UCICreditCard, types = c('factor', 'character'),
#' ex_cols = c("default.payment.next.month","ID$|_date$"), get_ex = FALSE)
#' x_list = get_names(dat = UCICreditCard, types = c('numeric', 'character', "integer"),
 #' ex_cols = c("default.payment.next.month", "ID$|SEX "), get_ex = FALSE)
#' @export


get_names = function(dat, types = c('logical', 'factor', 'character', 'numeric', 'integer64',
									  'integer', 'double', "Date", "POSIXlt", "POSIXct", "POSIXt"),
					   ex_cols = NULL, get_ex = FALSE) {
	if (is.null(types)) {
		stop("types is missing!")
	}
	if (is.null(ex_cols)) {
		sel_names = unique(names(dat)[sapply(dat, function(x) any(is.element(class(x), types)))])
		ex_names = unique(names(dat)[!(sapply(dat, function(x) any(is.element(class(x), types))))])
	} else {
		var_names = names(dat)[sapply(dat, function(x) any(is.element(class(x), types)))]
		if (length(ex_cols) == 1 & !any(grepl("\\$|\\*|\\+|\\?|\\[|\\^|\\{|\\}|\\\\|\\|\\)|\\]", ex_cols))) {
			ex_vars = names(dat)[colnames(dat) %in% ex_cols]
		} else {
			if (any(grepl("\\$|\\*|\\+|\\?|\\[|\\^|\\{|\\}|\\\\|\\|\\)|\\]", ex_cols))) {
				ex_vars = names(dat)[colnames(dat) %alike% ex_cols]
			} else {
				ex_vars = names(dat)[colnames(dat) %in% ex_cols]
			}
		}
		ex_types = names(dat)[!(sapply(dat, function(x) any(is.element(class(x), types))))]
		ex_names = unique(c(ex_vars, ex_types))
		sel_names = setdiff(var_names, ex_names)
	}
	if (get_ex) {
		dat = dat[ex_names]
	} else {
		dat = dat[sel_names]
	}
	var_names = names(dat)
	return(var_names)
}




#' Get X List.
#'
#' \code{get_x_list} is  for getting intersect names of x_list, train and test.
#' @param dat_train  A data.frame with independent variables.
#' @param dat_test  Another data.frame.
#' @param x_list Names of independent variables.
#' @param ex_cols A list of excluded variables. Regular expressions can also be used to match variable names. Default is NULL.
#' @param note Logical. Outputs info. Default is TRUE.
#' @return  A list contains names of variables
#' @seealso \code{\link{get_names}}
#' @examples
#' x_list = get_x_list(x_list = NULL,dat_train = UCICreditCard,
#' ex_cols = c("default.payment.next.month","ID$|_date$"))
#' @export


get_x_list = function(dat_train = NULL, dat_test = NULL,x_list = NULL, ex_cols = NULL,note = FALSE) {
    if (!is.null(dat_train)) {
        if (is.null(x_list) | length(x_list) <1 ) {
            if (is.null(dat_test)) {
                x_list_retain = get_names(dat = dat_train,
                                          types = c('character', 'factor', 'numeric', 'integer', 'double'),
                                          ex_cols = ex_cols, get_ex = FALSE)
            } else {
                x_list_t = get_names(dat = dat_train,
                                     types = c('character', 'factor', 'numeric', 'integer', 'double'),
                                     ex_cols = ex_cols, get_ex = FALSE)
                x_list_s = get_names(dat = dat_test,
                                     types = c('character', 'factor', 'numeric', 'integer', 'double'),
                                     ex_cols = ex_cols, get_ex = FALSE)
                x_list_retain = unique(intersect(x_list_t, x_list_s))
            }
        } else {
            if (is.null(dat_test)) {
                x_list_ts = get_names(dat = dat_train,
                                      types = c('character', 'factor', 'numeric', 'integer', 'double'),
                                      ex_cols = ex_cols, get_ex = FALSE)
                x_input = x_list %in% x_list_ts
            } else {
                x_list_t = get_names(dat = dat_train,
                                     types = c('character', 'factor', 'numeric', 'integer', 'double'),
                                     ex_cols = ex_cols, get_ex = FALSE)
                x_list_s = get_names(dat = dat_test,
                                     types = c('character', 'factor', 'numeric', 'integer', 'double'),
                                     ex_cols = ex_cols, get_ex = FALSE)
                x_list_ts = unique(intersect(x_list_t, x_list_s))
                x_excluded_ts = setdiff(x_list_t, x_list_s)
                if (note & length(x_excluded_ts) > 0) {
					cat_line("-- Following variables are not both in train & test:", col = love_color("dark_green"))
                    cat_bullet(paste0(format(x_excluded_ts)), col = "darkgrey")
                }
                x_input = x_list %in% x_list_ts
            }
            if (!all(x_input)) {
                x_retain = x_list[which(x_input)]
                x_excluded = x_list[which(!x_input)]

			if(note){
			cat_line("-- Following variables are excluded:", col = love_color("dark_green"))
            cat_bullet(paste0(format(x_excluded)), col = "darkgrey")
            }
            } else {
                x_retain = x_list
            }
            x_type = sapply(dat_train[, x_retain],
                            class) %in% c('character', 'factor', 'numeric', 'integer', 'double')

            if (!all(x_type)) {
                x_list_retain = x_retain[which(x_type)]
                x_excluded = x_retain[which(!x_type)]
				if(note){
				cat_line("-- Following variables are not numeric or character or factor:", col = love_color("dark_green"))
                cat_bullet(paste0(format(x_excluded)), col = "darkgrey")
				}

            } else {
                x_list_retain = x_retain
            }
        }
    }
    return(x_list_retain)
}


#'Checking Data
#'
#'
#' \code{checking_data}  cheking dat before processing.
#' @param dat A data.frame with independent variables and target variable.
#' @param target The name of target variable. Default is NULL.
#' @param occur_time The name of the variable that represents the time at which each observation takes place.
#' @param pos_flag The value of positive class of target variable, default: "1".
#' @param note Logical.Outputs info.Default is TRUE.
#' @return data.frame
#' @examples
#' dat = checking_data(dat = UCICreditCard, target = "default.payment.next.month")
#' @export

checking_data = function(dat = NULL, target = NULL, occur_time = NULL,
						  note = FALSE, pos_flag = NULL) {
	if (note) cat_line("-- Checking data and target format...", col = love_color("dark_green"))
	if (is.null(dat)) {
		warning("dat is null.\n")
	} else {
		if (!(class(dat)[1] == "data.frame")) {
			if (any(is.element(class(dat), c("data.table", "list", "tbl_df", "tbl", "matrix")))) {
				if (note) cat_line(paste("-- Convert", class(dat)[is.element(class(dat), c("data.table", "list", "tbl_df", "tbl", "matrix"))], "to data.frame."), col = love_color("light_green"))
				dat = quick_as_df(dat)
				} else {
					warning("dat is not two-dimensional.\n")
				}
		}
	}
	if (!is.null(target)) {
		if (!is.character(target) || length(target) > 1) {
			warning(paste("target is not a string or a name.\n", sep = "\t"))
		} else {
			if (length(unique(dat[, target])) < 2) {
				warning(paste("Unique values of target is only one.\n", sep = "\t"))
			} else {
				if (is.null(pos_flag)) {
					pos_flag = list("1", "bad", 1, "Bad", "positive", "pos", "Positive", "Pos")
				}

				if (length(which(dat[, target] %in% pos_flag)) != 0) {
					if (!all(sort(unique(dat[, target])) == c(0, 1))) {
						pos = unique(dat[, target])[which(unique(dat[, target]) %in% pos_flag)]
						dat[, target] = ifelse(dat[, target] %in% pos_flag, 1, 0)
						warning(paste("The  values in of target has been encoded", pos, "= 1 and others = 0", " \n"))
					}
				} else {
					warning(paste("Positive values of", target, "is not in pos_flag:", paste(pos_flag, collapse = ","), "\nplease set pos_flag. \n", sep = "\t"))
				}
				if(!is.numeric(dat[, target])){dat[, target]= as.numeric(as.character(dat[, target]))}

			}
		}
	}
	if (!is.null(occur_time)) {
		if (is.element(occur_time, names(dat))) {
			dat = time_transfer(dat, date_cols = occur_time)
			if (!is_date(dat[, occur_time])) {
				warning(paste("occur_time:", occur_time, "is not time or date.\n", sep = "\t"))
			}
		} else {
			warning(paste("occur_time:", occur_time, "is not in data.\n", sep = "\t"))
		}
	}
	return(dat)
}

#' Parallel computing and export variables to global Env.
#'
#' This function  is not intended to be used by end user.
#' @param parallel  A logical, default is TRUE.
#'
#' @return  parallel works.
#' @importFrom parallel detectCores  clusterExport clusterCall makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach %dopar% %do%  registerDoSEQ
#' @export
start_parallel_computing = function(parallel = TRUE) {

  parallelType = if (.Platform$OS.type == "windows")
    "snow" else "multicore"

  numCores = parallel::detectCores()-1
  attr(parallel, "type") = parallelType
  attr(parallel, "cores") = numCores

  if (parallel) {
    if (parallelType == "snow") {

      cl = parallel::makeCluster(numCores, type = "PSOCK")
      attr(parallel, "cluster") = cl

      varlist = ls(envir = parent.frame(), all.names = TRUE)
      varlist = varlist[varlist != "..."]
      parallel::clusterExport(cl, varlist = varlist, envir = parent.frame())
      parallel::clusterExport(cl, varlist = ls(envir = globalenv(), all.names = TRUE), envir = globalenv())
      pkgs = .packages()
      lapply(pkgs, function(pkg)
        parallel::clusterCall(cl, library, package = pkg,
                              character.only = TRUE))
      doParallel::registerDoParallel(cl, cores = numCores)
    }
    else if (parallelType == "multicore") {
      cl = parallel::makeCluster(numCores, type = "FORK")

      doParallel::registerDoParallel(cl, cores = numCores[1])

      attr(parallel, "cluster") = cl
    }
    else { stop("Only 'snow' and 'multicore' clusters allowed!") }
  }
  return(parallel)
}

#' Stop parallel computing
#'
#' This function  is not intended to be used by end user.
#' @param cluster  Parallel works.
#'
#' @return  stop clusters.
#'
#' @importFrom parallel  stopCluster
#' @importFrom foreach  registerDoSEQ
#' @export
stop_parallel_computing = function(cluster) {
    parallel::stopCluster(cluster)
    foreach::registerDoSEQ()
    invisible()
}



#'  Loop Function.
#' #' \code{loop_function} is an iterator to loop through
#' @param func  A function.
#' @param args  A list of argauments required by function.
#' @param x_list  Names of objects to loop through.
#' @param bind  Complie results, "rbind" & "cbind" are available.
#' @param parallel  Logical, parallel computing.
#' @param as_list  Logical, whether outputs  to be a list.
#'
#' @return   A data.frame or list
#'
#' @examples
#' dat = UCICreditCard[24:26]
#' num_x_list = get_names(dat = dat, types = c('numeric', 'integer', 'double'),
#'                       ex_cols = NULL, get_ex = FALSE)
#' dat[ ,num_x_list] = loop_function(func = outliers_kmeans_lof, x_list = num_x_list,
#'                                    args = list(dat = dat),
#'                                    bind = "cbind", as_list = FALSE,
#'                                  parallel = FALSE)
#' @importFrom foreach foreach %dopar% %do%
#' @export

loop_function = function(func = NULL, args = list(data = NULL), x_list = NULL,
                          bind = "rbind", parallel = TRUE, as_list = FALSE) {
    opt = options(scipen = 200, stringsAsFactors = FALSE, "warn" = -1) # suppress warnings
    df_list = df_tbl = NULL
    if (parallel) {
        parallel = start_parallel_computing(parallel)
        stopCluster = TRUE
    } else {
        parallel = stopCluster = FALSE
    }
    on.exit(if (parallel & stopCluster) stop_parallel_computing(attr(parallel, "cluster")))

    if (is.null(func)) {
        stop("The function is missing. Please input a split_type function.\n")
    }
    if (is.null(x_list)) {
        x_list = get_names(data, types = c("character", "factor", "numeric", "integer", "double"))
        warning(paste("x_list is NULL, use all variables.\n"))
    }
    i. = NULL
    if (!parallel) {
        funct = function(i.) {
            try(do.call(func, c(args, x = i.)), silent = FALSE)
        }
        df_list = lapply(x_list, funct)
        if (as_list) {
            df_tbl = df_list
            names(df_tbl) = x_list
        } else {
            df_tbl = as.data.frame(Reduce(bind, df_list))
        }
    } else {
        df_list = foreach(i. = x_list, .errorhandling = c('pass')) %dopar% {
            try(do.call(func, args = c(args, x = i.)), silent = FALSE)
        }
        if (as_list) {
            df_tbl = df_list
            names(df_tbl) = x_list
        } else {
            df_tbl = as.data.frame(Reduce(bind, df_list))
        }
    }
    options(opt) # reset warnings
    return(df_tbl)
}



#' string match
#' #' \code{str_match} search for matches to argument pattern within each element of a character vector:
#' @param pattern character string containing a regular expression (or character string for fixed = TRUE) to be matched in the given character vector. Coerced by as.character to a character string if possible. If a character vector of length 2 or more is supplied, the first element is used with a warning. missing values are allowed except for regexpr and gregexpr.
#' @param str_r	a character vector where matches are sought, or an object which can be coerced by as.character to a character vector. Long vectors are supported.
#'
#' @examples
#' orignal_nam = c("12mdd","11mdd","10mdd")
#' str_match(str_r = orignal_nam,pattern= "\\d+")
#' @export

str_match = function(pattern, str_r) {
    ind = regexpr(pattern, str_r)
    substr(str_r, ind, ind + attr(ind, "match.length"))
}


#' re_code
#' \code{re_code} search for matches to argument pattern within each element of a character vector:
#' @param x Variable to recode.
#' @param codes	A data.frame of original value & recode value
#' @examples
#' SEX  = sample(c("F","M"),1000,replace = TRUE)
#' codes= data.frame(ori_value = c('F','M'), code = c(0,1) )
#' SEX_re = re_code(SEX,codes)
#' @export
re_code = function(x,codes){
    for (i in 1:nrow(codes)) {
    x[which(x == codes[i, 1])] = codes[i, 2]
    }
    return(x)
}


#' Min Max Normalization
#'
#' \code{min_max_norm} is for normalizing each column vector of matrix 'x' using min_max normalization
#' @param x  Vector
#' @return Normalized vector
#' @examples
#' dat_s = apply(UCICreditCard[,12:14], 2, min_max_norm)
#' @export

min_max_norm = function(x) {
  min_x = min(x[x!=-Inf], na.rm = TRUE)
  max_x =  max(x[x!=Inf], na.rm = TRUE)
  if(any(x %in% c(-Inf,Inf))){
    x[x==-Inf] = min_x
    x[x==Inf] = max_x
    cat_line("Warning: Function call min_max_norm(x) cannot have -Inf/Inf,turn Inf to maximum,and turn -Inf to minimum.", 
             col = love_color("dark_red"))
  }
  x = (x - min_x) / (max_x - min_x)
  return(x)
}

#' Max Min Normalization
#'
#' \code{max_min_norm} is for normalizing each column vector of matrix 'x' using max_min normalization
#' @param x  Vector
#' @return Normalized vector
#' @examples
#' dat_s = apply(UCICreditCard[,12:14], 2, max_min_norm)
#' @export

max_min_norm = function(x) {
  min_x = min(x[x!=-Inf], na.rm = TRUE)
  max_x =  max(x[x!=Inf], na.rm = TRUE)
  if(any(x %in% c(-Inf,Inf))){
    x[x==-Inf] = min_x
    x[x==Inf] = max_x
    cat_line("Warning: Function call max_min_norm(x) cannot have -Inf/Inf,turn Inf to maximum,and turn -Inf to minimum.", 
             col = love_color("dark_red"))
  }
  x =  (max_x - x) / (max_x - min_x)
  return(x)
}


#' The length of a string.
#'
#' Returns the number of "code points", in a string.
#' @param string A string.
#' @return A numeric vector giving number of characters (code points) in each
#'    element of the character vector. Missing string have missing length.
#' @export
#' @examples
#' n_char(letters)
#' n_char(NA)

n_char = function(string) {
     nchar(string,allowNA = TRUE,keepNA =FALSE,type = 'bytes')
}


#' Automatic production of hive SQL
#'
#' Returns text parse of hive SQL
#' @param sql_dt The data dictionary has three columns: table, map and feature.
#' @param key_sql You can write your own SQL for the main table.
#' @param key_table Key table.
#' @param key_id Primary key id.
#' @param key_where Key table conditions.
#' @param only_key Only key table.
#' @param left_id  Right table's key id.
#' @param left_where Right table conditions.
#' @param new_name A string, Rename all variables except primary key with suffix 'new_name'.
#' @param ... Other params.
#' @return Text parse of hive SQL
#' @export
#' @examples
#' #sql_dt:table, map and feature
#' sql_dt = data.frame(table = c("table_1", "table_1",  "table_1", "table_1","table_1",
#'                                "table_2", "table_2","table_2",
#'                               "table_2","table_2","table_2","table_2",
#'                                "table_2","table_2","table_2","table_2",
#'                               "table_2","table_2","table_2","table_3","table_3",
#'                                "table_3","table_3","table_3"), 
#'                    map =  c("all","all", "all","all","all","all","all","all","all","all",
#'                             "all", "all","all","id_card_info",
#'                             "id_card_info","id_card_info", "mobile_info","mobile_info",
#'                             "mobile_info","all", "all","all", "all","all"), 
#'                    feature =c( "user_id","real_name","id_card_encode","mobile_encode","dt",
#'                               "user_id","type_code","first_channel",
#'                                "second_channel","user_name","user_sex","user_birthday",
#'                                  "user_age","card_province","card_zone",
#'                                "card_city","city","province","carrier","user_id",
#'                               "biz_id","biz_code","apply_time","dt"))
#' #sample 1
#' sql_hive_text_parse(sql_dt = sql_dt,
#'           key_sql = NULL,
#'                key_table = "table_2",
#'                key_where =  c("user_sex = 'male",
#'                               "user_age > 20"),
#'                only_key = FALSE,
#'                key_id = "user_id",
#'                left_id = "user_id",
#'                left_where = c("dt = date_add(current_date(),-1)",
#'                               "apply_time >= '2020-05-01' "
#'                ), new_name ="basic"
#'           )
#' 
#' #sample 2
#' sql_hive_text_parse(sql_dt = subset(sql_dt),
#'                key_sql = "SELECT 
#'        user_id,
#'        max(apply_time) as max_apply_time
#'        FROM table_3
#'        WHERE dt = date_add(current_date(),-1)
#'                GROUP BY user_id",
#'                key_id = "user_id",
#'                left_id = "user_id",
#'                left_where = c("dt = date_add(current_date(),-1)"
#'                               ),
#'                new_name =  NULL)

sql_hive_text_parse = function(sql_dt,
                          key_sql = NULL,
                          key_table = NULL,
                          key_id = NULL,
                          key_where =  c("dt = date_add(current_date(),-1)"),
                          only_key = FALSE,
                          left_id = NULL,
                          left_where = c("dt = date_add(current_date(),-1)"),
                          new_name = NULL,...){
  if(is.null(key_sql)){
    key_dt =  sql_dt[which(sql_dt$table == key_table),]
    key_dt[,'feature'] = gsub(" ","",key_dt[,'feature'])
    key_map =  unique(key_dt["map"])
    dup_feature_map = sapply(unlist(key_map),function(x)ifelse(x != 'all' & any(unique(key_dt[which(key_dt$map == x),"feature"])
                                                                                %in%
                                                                                  unique(key_dt[which(key_dt$map != 'all' &key_dt$map != x),"feature"])),x,NA))

    dup_feature_map = dup_feature_map[!is.na(dup_feature_map)]
    key_sql_tuple = list()

    for( i in unlist(key_map[which(key_map$map != "all"),"map"])){
      dup_name = ifelse(is.element(i,dup_feature_map),paste0(i,"_"),"")
      key_tuple_feature = unique(key_dt[which(key_dt$map == i),"feature"])
      key_tuple_feature_2 = unique(key_dt[which(key_dt$map == i),"feature"])
      key_tuple_feature_2 = gsub("-","_",tolower(key_tuple_feature_2))

      key_sql_tuple[[i]] = paste(paste0("lateral view json_tuple(udf.MapToJson(",
                                         unique(key_dt[which(key_dt$map == i),"map"]),"),"),
                                  paste0("'",key_tuple_feature,"'",
                                         collapse = ","),")"
                                  ,unique(key_dt[which(key_dt$map == i),"map"]),"AS",
                                  paste0(dup_name,key_tuple_feature_2,
                                         collapse = ","))


    }
    key_feature = list()
    for( i in unlist(key_map[,"map"])){
      dup_name = ifelse(is.element(i,dup_feature_map),paste0(i,"_"),"")
      key_feature[[i]] = paste0(dup_name,unique(key_dt[which(key_dt$map == i),"feature"]))
    }
    key_feature = unlist(key_feature,use.names = FALSE)
    key_where_x = sapply(key_where, function(s)as.character(strsplit(s,split = "\t| |<=|>=|>|<|in|=")[[1]][1]))
    if(any(!key_where_x %in% key_feature) ){
      key_where_t_no = names(key_where_x)[which(!key_where_x %in% key_feature)]
      warnings(paste("invalid condition of key_where:",key_where_t_no))
    }
    if(length(which(key_id %in% key_feature))==0){
      stop(paste(key_id,"does not exist in the Features of", key_table))
    }
    key_where_t = names(key_where_x)[which(key_where_x %in% key_feature)]
    if(length(key_where_t)>0){
      key_where_sql = paste("WHERE",paste(key_where_t,collapse = " AND "))
    }else{
      key_where_sql = ""
    }
    key_feature_3 = gsub("-","_",tolower(key_feature))

    if(!is.null(new_name) && length(new_name) >0){
      if(!is.null(key_id)){
        key_feature_2 = key_feature

        key_feature_2[which( key_id != key_feature_2)] = paste(unlist(key_feature_2[which( key_id != key_feature_2)],
                                                                      use.names = FALSE),
                                                               new_name,sep = "_")
        key_feature_2 = gsub("-","_",tolower(key_feature_2))
        key_feature_text =  paste(paste(unlist(key_feature_3),"AS",key_feature_2),
                                  collapse = ",",sep = ",")
      }else{
        key_feature_2 = key_feature
        key_feature_2 = paste(unlist(key_feature_2,use.names = FALSE),new_name,sep = "_")
        key_feature_2 = gsub("-","_",tolower(key_feature_2))
        key_feature_text =  paste(paste(unlist(key_feature_3),"AS",key_feature_2),
                                  collapse = ",",sep = ",")
      }

    }else{
      key_feature_2 = key_feature
      key_feature_2 = gsub("-","_",tolower(key_feature_2))
      key_feature_text =  paste(paste(unlist(key_feature_3),"AS",key_feature_2),
                                collapse = ",",sep = ",")

    }
    key_sql_text = paste(paste(paste0("SELECT ",key_feature_text)," FROM ",
                               key_table
    ), paste(unlist(key_sql_tuple),collapse = "  "),
    key_where_sql)

  }else{
    key_sql_text = gsub("\n"," ",key_sql)
  }
  if(is.null(key_sql)){
    left_table = unique(sql_dt[which(sql_dt$table != key_table),"table"])
  }else{
    left_table = unique(sql_dt[,"table"])
  }

  if(!is.null(key_id) & !only_key & length(left_table)>0){
    left_sql_text = list()
    left_id_t = c()

    for( t in left_table){
      left_dt =  sql_dt[which(sql_dt$table == t),]
      left_dt[,'feature'] = gsub(" ","",left_dt[,'feature'])
      left_map =  unique(left_dt["map"])
      dup_feature_left_map = sapply(unlist(left_map),function(x)ifelse(x != 'all' & any(unique(left_dt[which(left_dt$map == x),"feature"])
                                                                                  %in%
                                                                                    unique(left_dt[which(left_dt$map != 'all' &
                                                                                                           left_dt$map != x),
                                                                                                   "feature"])),x,NA))

      dup_feature_left_map = dup_feature_left_map[!is.na(dup_feature_left_map)]

      left_feature = list()
      for( i in unlist(left_map[,"map"])){
        left_dup_name = ifelse(is.element(i,dup_feature_left_map),paste0(i,"_"),"")
        left_feature[[i]] = paste0(left_dup_name,unique(left_dt[which(left_dt$map == i),"feature"]))
      }

      left_sql_tuple = list()
      for( i in unlist(left_map[which(left_map$map != "all"),"map"])){
        left_dup_name = ifelse(is.element(i,dup_feature_left_map),paste0(i,"_"),"")
        left_tuple_feature = unique(left_dt[which(left_dt$map == i),"feature"])
        #left_tuple_feature[grepl("-",left_tuple_feature)] = paste("`",left_tuple_feature[grepl("-",left_tuple_feature)],"`",sep = "")
        left_tuple_feature_2 = unique(left_dt[which(left_dt$map == i),"feature"])

        left_tuple_feature_2 = gsub("-","_",tolower(left_tuple_feature_2))

        left_sql_tuple[[i]] = paste(paste0("lateral view json_tuple(udf.MapToJson(",
                                           unique(left_dt[which(left_dt$map == i),"map"]),"),"),
                                    paste0("'",left_tuple_feature,"'",
                                           collapse = ","),")"
                                    ,unique(left_dt[which(left_dt$map == i),"map"]),"AS",
                                    paste0(left_dup_name,left_tuple_feature_2,
                                           collapse = ","))
      }
      left_feature = unlist(left_feature,use.names = FALSE)
      left_where_x = sapply(left_where, function(s)as.character(strsplit(s,split = "\t| |<=|>=|>|<|in|=")[[1]][1]))
      left_where_t = names(left_where_x)[which(left_where_x %in% left_feature)]
      if(length(left_where_t)>0){
        left_where_sql = paste("WHERE",paste(left_where_t,collapse = " AND "))
      }else{
        left_where_sql = ""
      }
  
      if(length(which(left_id %in% left_feature))>0){
        left_id_t[t] =  left_id[which(left_id %in% left_feature)]
      }else{
        stop(paste(left_id,"does not exist in the Features of", t))
      }
      left_feature_3 =  gsub("-","_",tolower(left_feature))
      if(!is.null(new_name) && length(new_name) >0){
        if(!is.null(left_id)){
          left_feature_2 = left_feature

          left_feature_2[which( left_id != left_feature_2)] = paste(unlist(left_feature_2[which( left_id != left_feature_2)],
                                                                        use.names = FALSE),
                                                                 new_name,sep = "_")
          left_feature_2 = gsub("-","_",tolower(left_feature_2))

          left_feature_text =  paste(paste(unlist(left_feature_3),"AS",left_feature_2),
                                    collapse = ",",sep = ",")
        }else{
          left_feature_2 = left_feature
          left_feature_2 = paste(unlist(left_feature_2,use.names = FALSE),new_name,sep = "_")
          left_feature_2 = gsub("-","_",tolower(left_feature_2))
          left_feature_text =  paste(paste(unlist(left_feature_3),"AS",left_feature_2),
                                    collapse = ",",sep = ",")
        }

      }else{
        left_feature_2 = left_feature
        left_feature_2 = gsub("-","_",tolower(left_feature_2))
        left_feature_text =  paste(paste(unlist(left_feature_3),"AS",left_feature_2),
                                  collapse = ",",sep = ",")

      }

      left_sql_text[[t]] = paste(paste(paste0("SELECT ",left_feature_text)," FROM ",t),
                                 paste(unlist(left_sql_tuple),collapse = "  "),left_where_sql)
    }
    key_letters = letters[1:(length(left_table)+1)]
    left_join_sql_text = c()
    for( len_dt in 1:(length(key_letters)-1)){
      left_join_sql_text[len_dt] =  paste("LEFT JOIN",
                                          paste("(",left_sql_text[[len_dt]],")",
                                                key_letters[len_dt+1]),
                                          paste("ON",paste0(key_letters[1],
                                                            ".",key_id),"=" ,
                                                paste0(key_letters[len_dt+1],".",
                                                       left_id_t[which(names(left_id_t) == left_table[len_dt])])))
    }
    sql_text = paste( "SELECT", paste0(paste0(key_letters,".*"), collapse = " , "),
                      "FROM",
                      paste("(",key_sql_text,")",key_letters[1]),
                      paste(left_join_sql_text,collapse = "  "))
  }else{
    sql_text = key_sql_text
  }
  return(sql_text)
}


