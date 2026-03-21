#' Customer Segmentation
#'
#' \code{customer_segmentation} is  a function for clustering and find the best segment variable.
#' @param  dat  A data.frame contained only predict variables.
#' @param x_list A list of x variables.
#' @param ex_cols A list of excluded variables. Default is NULL.
#' @param  cluster_control  A list controls cluster. kc is the number of cluster center (default is 2), nstart is the number of random groups (default is 1), max_iter max iteration number(default is 100) .
#' \itemize{
#'   \item \code{meth} Method of clustering. Provides two mehods,"Kmeans" and "FCM(Fuzzy Cluster Means)"(default is "Kmeans").
#'   \item \code{kc}  Number of cluster center (default is 2).
#'   \item \code{nstart} Number of random groups (default is 1).
#'   \item \code{max_iter}  Max iteration number(default is 100).
#' }
#' @param  tree_control  A list of controls for desison tree to find the best segment variable.
#' \itemize{
#'   \item \code{cv_folds}  Number of cross-validations(default is 5).
#'   \item \code{maxdepth} Maximum depth of a tree(default is kc +1).
#'   \item \code{minbucket}  Minimum percent of observations in any terminal <leaf> node (default is nrow(dat) / (kc + 1)).
#' }
#' @param save_data Logical. If TRUE, save outliers analysis file to the specified folder at \code{dir_path}
#' @param file_name  The name for periodically saved segmentation file. Default is NULL.
#' @param dir_path The path for periodically saved segmentation file.
#' @return  A "data.frame" object contains cluster results.
#' @references
#' Bezdek, James C. "FCM: The fuzzy c-means clustering algorithm".
#' Computers & Geosciences (0098-3004),\doi{10.1016/0098-3004(84)90020-7}
#' @examples
#' clust = customer_segmentation(dat = lendingclub[1:10000,20:30],
#'                               x_list = NULL, ex_cols = "id$|loan_status",
#'                               cluster_control = list(meth = "FCM", kc = 2),  save_data = FALSE,
#'                               tree_control = list(minbucket = round(nrow(lendingclub) / 10)),
#'                               file_name = NULL, dir_path = tempdir())
#' @importFrom rpart rpart rpart.control
#' @export


customer_segmentation = function(dat, x_list = NULL, ex_cols = NULL,
                                  cluster_control = list(meth = "Kmeans", kc = 2,
                                                         nstart = 1, epsm = 1e-06,
                                                         sf = 2, max_iter = 100),
                                  tree_control = list(cv_folds = 5, maxdepth = kc + 1,
                                                      minbucket = nrow(dat) / (kc + 1)),
                                  save_data = FALSE,
                                  file_name = NULL, dir_path = tempdir()) {
  opt = options(scipen = 200, stringsAsFactors = FALSE, digits = 6) #
    x_list = get_x_list(x_list = x_list, dat_train = dat, ex_cols = ex_cols)
    dir_path = ifelse(is.null(dir_path) | !is.character(dir_path) || !grepl(".|/", dir_path),
                      "./segmentation", dir_path)
    file_name = ifelse(is.null(file_name) | !is.character(file_name), "customer_seg", file_name)
    if (!dir.exists(dir_path)) dir.create(dir_path)
	if (any(is.na(dat[x_list]))) {
        dat = process_nas(dat = dat, x_list = x_list,  ex_cols =  ex_cols, parallel = FALSE,
                          method = "median",note = FALSE)
    }
    dat = one_hot_encoding(dat[x_list])
    dat_s = quick_as_df(lapply(dat, min_max_norm))
	if (any(is.na(dat_s))) {
        dat_s = process_nas(dat = dat_s, x_list = x_list, ex_cols =  ex_cols,
                            parallel = FALSE, method = "median",note = FALSE)
    }
    meth = ifelse(!is.null(cluster_control[["meth"]]), cluster_control[["meth"]], "FCM")
    nstart = ifelse(!is.null(cluster_control[["nstart"]]), cluster_control[["nstart"]], 1)
    epsm = ifelse(!is.null(cluster_control[["epsm"]]), cluster_control[["epsm"]], 1e-06)
    sf = ifelse(!is.null(cluster_control[["sf"]]), cluster_control[["sf"]], 2)
    max_iter = ifelse(!is.null(cluster_control[["max_iter"]]), cluster_control[["max_iter"]], 100)
    kc = ifelse(!is.null(cluster_control[["kc"]]), cluster_control[["kc"]], 2)
    #Clustering
    if (meth == "FCM") {
        cluster_res = fuzzy_cluster_means(dat = dat_s, kc = kc,
                                          nstart = nstart, max_iter = max_iter,
                                          sf = sf, epsm = epsm)
    } else {
        cluster_res = kmeans(x = dat_s, centers = kc, nstart = nstart, iter.max = max_iter)
    }

    #The result of cluster analysis
    dt_cluster_res = data.frame(dat, cluster_id = cluster_res$cluster)
    if(save_data)save_data(dt_cluster_res, file_name = file_name, dir_path = dir_path)
    dt_cluster_res$cluster_id = as.factor(as.character(dt_cluster_res$cluster_id))
    #fund the best Clustering variable by desision tree.
    tree_formula = as.formula(paste("cluster_id",
                                     paste(names(dt_cluster_res[, - length(dt_cluster_res)]), collapse = "+"),
                                     sep = ' ~ '))
    cv_folds = ifelse(!is.null(tree_control[["cv_folds"]]), tree_control[["cv_folds"]], 5)
    maxdepth = ifelse(!is.null(tree_control[["maxdepth"]]), tree_control[["maxdepth"]], kc + 1)
    minbucket = max(min(table(dt_cluster_res$cluster_id)) / (kc + 1), tree_control[["minbucket"]])
    trcontrol = rpart.control(minbucket = minbucket, xval = cv_folds, maxdepth = maxdepth)
    set.seed(46)
    fit = rpart(data = dt_cluster_res,
                 formula = tree_formula,
                 control = trcontrol,
                 parms = list(split = "information"))
    if (save_data) {
        summary(fit, digits = getOption("digits"),
            file = paste0(dir_path, "/", ifelse(is.null(file_name), "segment.tree.txt", paste(file_name, "segment.tree.txt", sep = "."))))
    }

    return(list(fit,cluster_res))
	options(opt) # reset
}



#' Table of Binning
#'
#' \code{get_bins_table}  is used to generates summary information of varaibles.
#' \code{get_bins_table_all} can generates bins table for all specified independent variables.
#' @param dat A data.frame with independent variables and target variable.
#' @param target The name of target variable.
#' @param x_list Names of independent variables.
#' @param ex_cols A list of excluded variables. Regular expressions can also be used to match variable names. Default is NULL.
#' @param pos_flag Value of positive class, Default is "1".
#' @param breaks_list A table containing a list of splitting points for each independent variable. Default is NULL.
#' @param breaks Splitting points for an independent variable. Default is NULL.
#' @param bins_total Logical, total sum for each columns.
#' @param dat_test  A data.frame of test data. Default is NULL.
#' @param parallel Logical, parallel computing. Default is FALSE.
#' @param note   Logical, outputs info. Default is TRUE.
#' @param save_data Logical, save results in locally specified folder. Default is FALSE.
#' @param file_name  The name for periodically saved bins table file. Default is "bins_table".
#' @param dir_path The path for periodically saved bins table file. Default is "./variable".
#' @seealso
#' \code{\link{get_iv}},
#' \code{\link{get_iv_all}},
#' \code{\link{get_psi}},
#' \code{\link{get_psi_all}}
#' @examples
#' breaks_list = get_breaks_all(dat = UCICreditCard, x_list = names(UCICreditCard)[3:4],
#' target = "default.payment.next.month", equal_bins =TRUE,best = FALSE,g=5,
#' ex_cols = "ID|apply_date", save_data = FALSE)
#' get_bins_table_all(dat = UCICreditCard, breaks_list = breaks_list,
#' target = "default.payment.next.month")
#' @importFrom data.table fwrite melt fread dcast
#' @export

get_bins_table_all = function(dat, x_list = NULL, target = NULL, pos_flag = NULL,dat_test = NULL,
                               ex_cols = NULL, breaks_list = NULL, parallel = FALSE,
                               note = FALSE, bins_total = TRUE,
                               save_data = FALSE, file_name = NULL, dir_path = tempdir()) {
    if (note)cat_line("-- Processing bins table", col = love_color("deep_green"))

    opt = options(scipen = 200, stringsAsFactors = FALSE) #
    if (is.null(x_list)) {
        if (!is.null(breaks_list)) {
            x_list = unique(as.character(breaks_list[, "Feature"]))
        } else {
            x_list = get_x_list(x_list = x_list, dat_train = dat,
                                dat_test = dat_test, ex_cols = c(target,  ex_cols))
        }
    }
    bins_list = loop_function(func = get_bins_table, x_list = x_list,
                              args = list(dat = dat, target = target,
                                          breaks = NULL, breaks_list = breaks_list,
                                          pos_flag = pos_flag, dat_test = dat_test,
                                          bins_total = bins_total, note = note),
                              bind = "rbind", parallel = parallel, as_list = FALSE)
    bins_list[which(bins_list$cuts == 'NULL'),"cuts"] = "missing"
    if (save_data) {
        dir_path = ifelse(!is.character(dir_path),
                      tempdir(), dir_path)
        if (!dir.exists(dir_path)) dir.create(dir_path)
        if (!is.character(file_name)) file_name = NULL
        save_data(bins_list, file_name = ifelse(is.null(file_name), "bins_table", paste(file_name, "bins_table", sep = ".")), dir_path = dir_path, note = FALSE)
    }
    return(bins_list)
	options(opt) # reset
}

#' @param x  The name of an independent variable.
#' @rdname get_bins_table_all
#' @export

get_bins_table = function(dat, x, target = NULL, pos_flag = NULL, dat_test = NULL,
          breaks = NULL, breaks_list = NULL,bins_total = TRUE,
          note = FALSE) {
   
   opt = options(scipen = 200, stringsAsFactors = FALSE) #
   good = bad = NULL
   if (is.null(breaks)|length(breaks)<=0) {
     if (!is.null(breaks_list)|length(breaks_list) <=0) {
       breaks = breaks_list[which(as.character(breaks_list[, "Feature"]) == names(dat[x])), "cuts"]
     }

     if (length(breaks)<=0) {
       breaks = get_breaks(dat = dat,x = x, target = target, equal_bins = TRUE, best = FALSE,g = 10,bins_no = TRUE)
     }
   }

   if (!is.null(target) | length(target) >0) {
     if (length(unique(dat[, target])) > 1) {
       if (is.null(pos_flag)) {
         dat[, target] = ifelse(dat[, target] %in% list("1", "bad", 1), 1, 0)
       } else {
         if (any(is.element(pos_flag, list("1", "bad", 1)))) {
           dat[, target] = ifelse(dat[, target] %in% pos_flag, 1, 0)
         } else {
           dat[, target] = ifelse(dat[, target] %in% pos_flag, 0, 1)
         }
         
         if (length(unique(dat$target)) == 1) {
           stop(paste("The value in pos_flag is not one of the value of  target.\n"))
         }
       }
     } else {
       stop(paste("The value of  target is unique.\n"))
     }
   } else {
     stop(paste("The target variable is missing.\n"))
   }
   
   best_bins = split_bins(dat = dat, x = x, breaks = breaks, bins_no = TRUE)
   dt_bins = table(best_bins, dat[, target])
   rm(best_bins)
   
   dt_bins = data.frame(unclass(dt_bins))
   if (all(names(dt_bins) == c("X0", "X1"))) {
     names(dt_bins) = c("good", "bad")
   } else {
     if (all(names(dt_bins) == c("X1", "X0"))) {
       names(dt_bins) = c("bad", "good")
     } else {
       stop(paste(target, "is neither 1 nor 0./n"))
     }
   }
   #bins table
   df_bins = within(dt_bins, {
     bins = row.names(dt_bins)
     Feature = names(dat[x])
     cuts = breaks[1:nrow(dt_bins)]
     total = good + bad
     `%total` = as_percent((good + bad) / (sum(good,na.rm = TRUE) + sum(bad,na.rm = TRUE)), digits = 3)
     `%good` = as_percent(good / sum(good), 2)
     `%bad` = as_percent(bad / sum(bad), 2)
     bad_rate = as_percent(bad / (ifelse(good>0, good,1) + bad), digits = 3)
     `GB_index` = round(((ifelse(good > 0,good,1) / ifelse(bad >0, bad, 1)) / (sum(good,na.rm = TRUE) / sum(bad,na.rm = TRUE))) * 100, 0)
     woe = round(log((ifelse(good > 0, good,1) / sum(good,na.rm = TRUE)) / (ifelse(bad >0 ,bad,1) / sum(bad,na.rm = TRUE))), 4)
     iv = round(((ifelse(good >0, good,1) / sum(good,na.rm = TRUE)) - (ifelse(bad>0,bad,1) / sum(bad,na.rm = TRUE))) * woe, 4)
   })
   rm(dt_bins)
   
   if (!is.null(dat_test)) {
     dt_psi = get_psi(dat = dat, x = x, breaks = breaks,
                      pos_flag = pos_flag,
                      dat_test = dat_test,
                      as_table = TRUE, note = FALSE)
     df_bins = merge(df_bins, dt_psi[, c("Bins", "PSI_i")], by.x = "bins", by.y = "Bins", all.x = TRUE)
     df_bins[is.na(df_bins)] = 0
     df_bins = re_name(df_bins, "PSI_i", "psi")
   } else {
     df_bins$psi = rep(-1, nrow(df_bins))
   }
   df_bins = df_bins[c("Feature", "bins", "cuts", "total", "good", "bad",
                       "%total", "%good", "%bad", "bad_rate", "woe", "GB_index", "iv", "psi")]
   
   
   if (note){
     cat_bullet(paste(x, "IV:", round(sum(df_bins$iv,na.rm = TRUE), 3), "PSI:",
                      ifelse(all(df_bins$psi > -1),round(sum(df_bins$psi,na.rm = TRUE), 3), -1),
                      collapse = "\t"), col = "darkgrey")
   }
   if (bins_total) {
     total_sum = c("Total", "--", "--", sum(df_bins$total),
                   sum(df_bins$good), sum(df_bins$bad),
                   as_percent(1, digits = 2), as_percent(1, digits = 2),
                   as_percent(1, digits = 2),
                   as_percent(sum(df_bins$bad) / sum(df_bins$total), 2), 0, 100,
                   round(sum(df_bins$iv,na.rm = TRUE), 3), ifelse(all(df_bins$psi > -1),round(sum(df_bins$psi,na.rm = TRUE), 3), -1))
     df_bins = rbind(df_bins, total_sum)
     
   }
   rownames(df_bins) = NULL
   return(df_bins)
   options(opt) # reset
 }



#' Calculate IV & PSI
#'
#'
#' \code{get_iv_psi}  is used to calculate Information Value (IV)  and Population Stability Index (PSI) of an independent variable.
#' \code{get_iv_psi_all} can loop through IV & PSI for all specified independent variables.
#' @param dat A data.frame with independent variables and target variable.
#' @param dat_test  A data.frame of test data. Default is NULL.
#' @param target The name of target variable.
#' @param x_list Names of independent variables.
#' @param x  The name of an independent variable.
#' @param ex_cols A list of excluded variables. Regular expressions can also be used to match variable names. Default is NULL.
#' @param pos_flag The value of positive class of target variable, default: "1".
#' @param breaks_list A table containing a list of splitting points for each independent variable. Default is NULL.
#' @param breaks Splitting points for an independent variable. Default is NULL.
#' @param bins_total Logical, total sum for each variable.
#' @param occur_time The name of the variable that represents the time at which each observation takes place.
#' @param oot_pct  Percentage of observations retained for overtime test (especially to calculate PSI). Defualt is 0.7
#' @param best  Logical, merge initial breaks to get optimal breaks for binning.
#' @param equal_bins  Logical, generates initial breaks for equal frequency or width binning.
#' @param cut_bin A string, if equal_bins is TRUE, 'equal_depth' or 'equal_width', default is 'equal_depth'.
#' @param g  Number of initial breakpoints for equal frequency binning.
#' @param tree_control  Parameters of using Decision Tree to segment initial breaks. See detials: \code{\link{get_tree_breaks}}
#' @param bins_control  Parameters  used to control binning.  See detials: \code{\link{select_best_class}}, \code{\link{select_best_breaks}}
#' @param as_table Logical, output results in a table. Default is TRUE.
#' @param bins_no Logical, add serial numbers to bins. Default is FALSE.
#' @param parallel Logical, parallel computing. Default is FALSE.
#' @param note Logical, outputs info. Default is TRUE.
#' @seealso \code{\link{get_iv}},\code{\link{get_iv_all}},\code{\link{get_psi}},\code{\link{get_psi_all}}
#' @examples
#' iv_list = get_psi_iv_all(dat = UCICreditCard[1:1000, ],
#' x_list = names(UCICreditCard)[3:5], equal_bins = TRUE,
#' target = "default.payment.next.month", ex_cols = "ID|apply_date")
#' get_psi_iv(UCICreditCard, x = "PAY_3",
#' target = "default.payment.next.month",bins_total = TRUE)
#' @importFrom data.table fwrite melt fread dcast
#' @export

get_psi_iv_all = function(dat, dat_test = NULL, x_list = NULL, target, ex_cols = NULL, pos_flag = NULL,
breaks_list = NULL, occur_time = NULL, oot_pct = 0.7, equal_bins = FALSE,cut_bin = 'equal_depth', tree_control = NULL, bins_control = NULL,
bins_total = FALSE, best = TRUE, g = 10, as_table = TRUE, note = FALSE, parallel = FALSE, bins_no = TRUE) {
    dat = checking_data(dat = dat, target = target, pos_flag = pos_flag)
    opt = options(scipen = 200, stringsAsFactors = FALSE) #
    if (note)cat_line("-- Calculating IV & PSI", col = love_color("deep_green"))
    dat = checking_data(dat = dat, target = target, pos_flag = pos_flag)
    if (is.null(x_list)) {
        if (is.null(x_list)) {
            if (!is.null(breaks_list)) {
                x_list = unique(as.character(breaks_list[, "Feature"]))
            } else {
                x_list = get_x_list(x_list = x_list,
                                    dat_train = dat,
                                    dat_test = dat_test,
                                    ex_cols = c(target, occur_time, ex_cols))
            }
        }
    } else {
        x_list = gsub("_woe$|_pred$", "", x_list)
    }
    psi_iv_list = loop_function(func = get_psi_iv, x_list = x_list, args = list(dat = dat, dat_test = dat_test, breaks = NULL,
    breaks_list = breaks_list, target = target, pos_flag = pos_flag, best = best, equal_bins = equal_bins, cut_bin = cut_bin,tree_control = tree_control,
    oot_pct = oot_pct, occur_time = occur_time, bins_control = bins_control, g = g, note = note, bins_total = bins_total,
    as_table = as_table, bins_no = bins_no), bind = "rbind", parallel = parallel)
    return(psi_iv_list)
	options(opt) # reset
}

#' @rdname get_psi_iv_all
#' @export

get_psi_iv = function(dat, dat_test = NULL, x, target, pos_flag = NULL, breaks = NULL, breaks_list = NULL,
					   occur_time = NULL, oot_pct = 0.7, equal_bins = FALSE, cut_bin = 'equal_depth', tree_control = NULL, bins_control = NULL,
					   bins_total = FALSE, best = TRUE, g = 10, as_table = TRUE, note = FALSE, bins_no = TRUE) {
	opt = options(scipen = 200, stringsAsFactors = FALSE, digits = 6) #
	if (is.null(target)) {
		stop("target is missing!")
	}
	if (is.null(breaks)) {
		if (!is.null(breaks_list)) {
			breaks = breaks_list[which(as.character(breaks_list[, "Feature"]) == names(dat[x])), "cuts"]
		}
		if (is.null(breaks)) {
			breaks = get_breaks(dat = dat, x = x, target = target, pos_flag = pos_flag, bins_control = bins_control,
						  occur_time = occur_time, oot_pct = oot_pct, equal_bins = equal_bins, cut_bin = cut_bin,
						  best = best, tree_control = tree_control, g = g, note = FALSE)
		}
	}
	if (all(unique(dat[, target]) != c("0", "1"))) {
		if (!is.null(pos_flag)) {
			dat$target = ifelse(dat[, target] %in% pos_flag, "1", "0")
		} else {
			pos_flag = list("1", 1, "bad", "positive")
			dat$target = ifelse(dat[, target] %in% pos_flag, "1", "0")
		}
		if (length(unique(dat$target)) == 1) {
			stop("pos_flag is missing.\n")
		}
	} else {
		dat$target = dat[, target]
	}
	if (is.null(dat_test)) {
		df_bins = split_bins(dat = dat, x = x, breaks = breaks, bins_no = bins_no)
		df_bins = as.data.frame(cbind(dat[occur_time], bins = df_bins, target = dat$target))
		train_test = train_test_split(dat = df_bins, prop = oot_pct, split_type = "OOT",
								  occur_time = occur_time, save_data = FALSE, note = FALSE)
		dfe = train_test$train
		dfa = train_test$test
		dfa$ae = "actual"
		dfe$ae = "expected"

		df_ae = rbind(dfa, dfe)
	} else {
		if (all(unique(dat_test[, target]) != c("0", "1"))) {
			if (!is.null(pos_flag)) {
				dat_test$target = ifelse(dat_test[, target] %in% pos_flag, "1", "0")
			} else {
				pos_flag = list("1", 1, "bad", "positive")
				dat_test$target = ifelse(dat_test[, target] %in% pos_flag, "1", "0")
			}
			if (length(unique(dat_test$target)) == 1) {
				stop("pos_flag is missing.\n")
			}
		} else {
			dat_test$target = as.character(dat_test[, target])
		}
		dfe_bins = split_bins(dat = dat, x = x, breaks = breaks, bins_no = bins_no)
		dfe = as.data.frame(cbind(bins = dfe_bins, target = dat$target))
		dfa_bins = split_bins(dat = dat_test, x = x, breaks = breaks, bins_no = bins_no)
		dfa = as.data.frame(cbind(bins = dfa_bins, target = dat_test$target))

		dfa$ae = "actual"
		dfe$ae = "expected"
		df_ae = rbind(dfa, dfe)
	}
	df_ae = as.data.table(df_ae)
	bins_psi_iv = data.table::dcast(df_ae, bins ~ ae + target, fun.aggregate = length, value.var = "ae")
	bins_psi_iv = quick_as_df(bins_psi_iv)
	bins = actual_1 = actual_0 = expected_0 = expected_1 = 0

	bins_psi_iv = within(bins_psi_iv, {
		cuts = breaks[1:nrow(bins_psi_iv)]
		actual_0 = as.numeric(actual_0)
		expected_0 = as.numeric(expected_0)
		actual_1 = as.numeric(actual_1)
		expected_1 = as.numeric(expected_1)
		Feature = names(dat[x])
		`#total` = actual_0 + expected_0 + actual_1 + expected_1
		`%total` = round((actual_0 + expected_0 + actual_1 + expected_1) / sum(`#total`), 2)
		`%total_1` = round((actual_1 + expected_1) / `#total`, 2)
		total_0_pct = (actual_0 + expected_0) / sum(actual_0 + expected_0)
		total_1_pct = (actual_1 + expected_1) / sum(actual_1 + expected_1)
		`#actual` = actual_1 + actual_0
		`%actual` = round((actual_1 + actual_0) / sum(`#actual`), 2)
		`#expected` = expected_1 + expected_0
		`%expected` = round((expected_1 + expected_0) / sum(`#expected`), 2)
		actual_pct_1 = (actual_1) / ifelse(sum(actual_1) == 0, 1, sum(actual_1))
		expected_pct_1 = (expected_1) / ifelse(sum(expected_1) == 0, 1, sum(expected_1))
		`%expected_1` = ifelse(`#expected` > 0, round(expected_1 / (expected_1 + expected_0), 2), 0)
		`%actual_1` = ifelse(`#actual` > 0, round(actual_1 / (actual_1 + actual_0), 2), 0)
		odds_ratio = round(((actual_0 + expected_0) / ifelse(actual_1 + expected_1 > 0, actual_1 + expected_1, 1)) /
						 (sum(actual_0 + expected_0) /
							sum(ifelse(actual_1 + expected_1 > 0, actual_1 + expected_1, 1))), 3)
		odds_rotio_e = (ifelse(expected_0 > 0, expected_0, 1) / ifelse(expected_1 > 0, expected_1, 1)) / (sum(ifelse(expected_0 > 0, expected_0, 1)) / sum(ifelse(expected_1 > 0, expected_1, 1)))
		odds_rotio_a = (ifelse(actual_0 > 0, actual_0, 1) / ifelse(actual_1 > 0, actual_1, 1)) / (sum(ifelse(actual_0 > 0, actual_0, 1)) / sum(ifelse(actual_1 > 0, actual_1, 1)))
		odds_ratio_s = round((odds_rotio_e - odds_rotio_a) *
						   log(odds_rotio_e / odds_rotio_a), 3)
		PSIi = round((ifelse(`#actual` > 0, `#actual`, 1) / sum(ifelse(`#actual` > 0, `#actual`, 1)) - ifelse(`#expected` > 0, `#expected`, 1) / sum(ifelse(`#expected` > 0, `#expected`, 1))) *
				   log((ifelse(`#actual` > 0, `#actual`, 1) / sum(ifelse(`#actual` > 0, `#actual`, 1))) / (ifelse(`#expected` > 0, `#expected`, 1) / sum(ifelse(`#expected` > 0, `#expected`, 1)))), 3)
		WOEi = log(total_0_pct / ifelse(total_1_pct > 0, total_1_pct, 1))
		IVi = round((total_0_pct - total_1_pct) * WOEi, 3)
	})
	if (as_table) {
		df_psi_iv = bins_psi_iv[c("Feature", "bins", "cuts", "#total", "#expected", "expected_0", "expected_1",
							  "#actual", "actual_0", "actual_1", "%total", "%expected", "%actual", "%total_1",
							  "%expected_1", "%actual_1", "odds_ratio", "odds_ratio_s", "PSIi", "IVi")]
		if (bins_total) {
			total = c("Total",
				 "--", "--",
				 sum(bins_psi_iv$`#total`, na.rm = TRUE),
				 sum(bins_psi_iv$`#expected`, na.rm = TRUE),
				 sum(bins_psi_iv$expected_0, na.rm = TRUE),
				 sum(bins_psi_iv$expected_1, na.rm = TRUE),
				 sum(bins_psi_iv$`#actual`, na.rm = TRUE),
				 sum(bins_psi_iv$actual_0, na.rm = TRUE),
				 sum(bins_psi_iv$actual_1, na.rm = TRUE),
				 1, 1, 1,
				 round(sum(bins_psi_iv$expected_1 + bins_psi_iv$actual_1) / sum(bins_psi_iv$`#total`), 2),
				 round(sum(bins_psi_iv$expected_1) / sum(bins_psi_iv$`#expected`), 2),
				 round(sum(bins_psi_iv$actual_1) / sum(bins_psi_iv$`#actual`), 2),
				 1,
				 sum(bins_psi_iv$odds_ratio_s),
				 sum(bins_psi_iv$PSIi),
				 sum(bins_psi_iv$IVi))
			df_psi_iv = rbind(df_psi_iv, total)
		}
	} else {
		df_psi_iv = data.frame(Feature = x, IV = as.numeric(sum(bins_psi_iv$IVi)), PSI = as.numeric(sum(bins_psi_iv$PSIi)))
	}
	if (note) {
		cat_bullet(paste(x, "IV:", as.numeric(sum(bins_psi_iv$IVi)), "PSI: ",
			  as.numeric(sum(bins_psi_iv$PSIi)), sep = " "), col = "darkgrey")
	}

	return(df_psi_iv)
	options(opt) # reset
}



#' Calculate Information Value (IV)
#' \code{get_iv}  is used to calculate Information Value (IV) of an independent variable.
#' \code{get_iv_all} can loop through IV for all specified independent variables.
#' @param dat A data.frame with independent variables and target variable.
#' @param target The name of target variable.
#' @param x_list Names of independent variables.
#' @param x  The name of an independent variable.
#' @param ex_cols A list of excluded variables. Regular expressions can also be used to match variable names. Default is NULL.
#' @param pos_flag Value of positive class, Default is "1".
#' @param breaks_list A table containing a list of splitting points for each independent variable. Default is NULL.
#' @param best  Logical, merge initial breaks to get optimal breaks for binning.
#' @param equal_bins  Logical, generates initial breaks for equal frequency binning.
#' @param g  Number of initial breakpoints for equal frequency binning.
#' @param tree_control  Parameters of using Decision Tree to segment initial breaks. See detials: \code{\link{get_tree_breaks}}
#' @param bins_control  Parameters  used to control binning.  See detials: \code{\link{select_best_class}}, \code{\link{select_best_breaks}}
#' @param parallel Logical, parallel computing. Default is FALSE.
#' @param note  Logical, outputs info. Default is TRUE.
#'
#' @seealso \code{\link{get_iv}},\code{\link{get_iv_all}},\code{\link{get_psi}},\code{\link{get_psi_all}}
#' @references Information Value Statistic:Bruce Lund, Magnify Analytics Solutions, a Division of Marketing Associates, Detroit, MI(Paper AA - 14 - 2013)
#' @details
#' IV Rules of Thumb for evaluating the strength a predictor
#' Less than 0.02:unpredictive
#' 0.02 to 0.1:weak
#' 0.1 to 0.3:medium
#' 0.3 + :strong
#' @examples
#' get_iv_all(dat = UCICreditCard,
#'  x_list = names(UCICreditCard)[3:10],
#'  equal_bins = TRUE, best = FALSE,
#'  target = "default.payment.next.month",
#'  ex_cols = "ID|apply_date")
#' get_iv(UCICreditCard, x = "PAY_3",
#'        equal_bins = TRUE, best = FALSE,
#'  target = "default.payment.next.month")
#' @export


get_iv_all = function(dat, x_list = NULL, ex_cols = NULL, breaks_list = NULL,
                       target = NULL, pos_flag = NULL, best = TRUE,
					   equal_bins = FALSE, tree_control = NULL, bins_control = NULL,
					   g = 10, parallel = FALSE, note = FALSE) {
    dat = checking_data(dat = dat, target = target, pos_flag = pos_flag)
    opt = options(scipen = 200, stringsAsFactors = FALSE) #
    if (note)cat_line("-- Calculating IV", col = love_color("dark_green"))
    x_list = get_x_list(x_list = x_list, dat_train = dat, dat_test = NULL, ex_cols = c(target, ex_cols))
    iv_list = loop_function(func = get_iv, x_list = x_list, args = list(dat = dat, breaks = NULL,
    breaks_list = breaks_list, target = target, pos_flag = pos_flag, best = best,
    equal_bins = equal_bins, tree_control = tree_control, bins_control = bins_control,
    g = g, note = note), bind = "rbind", parallel = parallel)

    return(iv_list)
	options(opt) # reset
}


#' @param breaks Splitting points for an independent variable. Default is NULL.
#' @rdname get_iv_all
#' @export


get_iv = function(dat, x, target = NULL, pos_flag = NULL, breaks = NULL, breaks_list = NULL,
 best = TRUE, equal_bins = FALSE, tree_control = NULL, bins_control = NULL, g = 10, note = FALSE) {

    IV = G = B = NULL

    if (is.null(target)) {
        stop("target is missing!")
    }
    if (is.null(breaks)) {
        if (!is.null(breaks_list)) {
            breaks = breaks_list[which(as.character(breaks_list[, "Feature"]) == names(dat[x])), "cuts"]
        }
        if (is.null(breaks)) {
            breaks = get_breaks(dat = dat, x = x, target = target, pos_flag = pos_flag,
            bins_control = bins_control, equal_bins = equal_bins, best = best,
            tree_control = tree_control, g = g, note = FALSE)
        }
    }
    if (!is.null(target)) {
        if (length(unique(dat[, target])) > 1) {
            if (is.null(pos_flag)) {
                dat[, target] = ifelse(dat[, target] %in% list("1", "bad", 1), 1, 0)
            } else {
                if (any(is.element(pos_flag, list("1", "bad", 1)))) {
                    dat[, target] = ifelse(dat[, target] %in% pos_flag, 1, 0)
                } else {
                    dat[, target] = ifelse(dat[, target] %in% pos_flag, 0, 1)
                }

                if (length(unique(dat$target)) == 1) {
                    stop(paste("The value in pos_flag is not one of the value of  target.\n"))
                }
            }
        } else {
            stop(paste("The value of  target is unique.\n"))
        }
    } else {
        stop(paste("The target variable is missing.\n"))
    }

    best_bins = split_bins(dat = dat, x = x, breaks = breaks, bins_no = TRUE)
    dt_bins = table(best_bins, dat[, target])
    rm(best_bins)

    bins_df = data.frame(unclass(dt_bins))
    rm(dt_bins)
    if (all(names(bins_df) == c("X0", "X1"))) {
        names(bins_df) = c("G", "B")
    } else {
        if (all(names(bins_df) == c("X1", "X0"))) {
            names(bins_df) = c("B", "G")
        } else {
            stop(paste(target, "is neither 1 nor 0./n"))
        }
    }
    #IV
    bins_df = within(bins_df, {
        `%totalG` = ifelse(G >0 ,G,1) / sum(G,na.rm = TRUE)
        `%totalB` = ifelse(B>0,B ,1)/ sum(B,na.rm = TRUE)
        IVi = round((`%totalG` - `%totalB`) * log(`%totalG` / `%totalB`), 3)
    })
    iv_df = data.frame(Feature = x, IV = as.numeric(sum(bins_df$IVi)))
    rm(bins_df)
    iv_df = within(iv_df, {
        strength = "Suspicious"
        strength[IV <= 0.01] = "Unpredictive"
        strength[IV > 0.01 & IV <= 0.02] = "Very Weak"
        strength[IV > 0.02 & IV <= 0.05] = "Weak"
        strength[IV > 0.05 & IV <= 0.1] = "Medium"
        strength[IV > 0.1 & IV <= 0.3] = "Strong"
        strength[IV <= 3 & IV > 0.3] = "Very Strong"
    })
    if (note) {
    cat_line(paste0("--",x), col = love_color("water_blue"))
	cat_bullet(paste("IV:", iv_df$IV," --> ", iv_df$strength), col = "darkgrey")
    }

    iv_df
}


#' Calculate Population Stability Index (PSI)
#' \code{get_psi} is used to calculate Population Stability Index (PSI)  of an independent variable.
#' \code{get_psi_all} can loop through PSI for all specified independent variables.
#' @param dat A data.frame with independent variables and target variable.
#' @param dat_test  A data.frame of test data. Default is NULL.
#' @param x_list Names of independent variables.
#' @param target The name of target variable.
#' @param x  The name of an independent variable.
#' @param ex_cols Names of excluded variables. Regular expressions can also be used to match variable names. Default is NULL.
#' @param pos_flag Value of positive class, Default is "1".
#' @param breaks_list A table containing a list of splitting points for each independent variable. Default is NULL.
#' @param breaks Splitting points for an independent variable. Default is NULL.
#' @param g  Number of initial breakpoints for equal frequency binning.
#' @param occur_time The name of the variable that represents the time at which each observation takes place.
#' @param start_date The earliest occurrence time of observations.
#' @param cut_date Time points for spliting data sets, e.g. : spliting Actual and Expected data sets.
#' @param oot_pct  Percentage of observations retained for overtime test (especially to calculate PSI). Defualt is 0.7
#' @param as_table Logical, output results in a table. Default is TRUE.
#' @param bins_no Logical, add serial numbers to bins. Default is TRUE.
#' @param parallel Logical, parallel computing. Default is FALSE.
#' @param note   Logical, outputs info. Default is TRUE.
#' @seealso \code{\link{get_iv}},\code{\link{get_iv_all}},\code{\link{get_psi}},\code{\link{get_psi_all}}
#' @details
#' PSI Rules for evaluating the stability of a predictor
#' Less than 0.02: Very stable
#' 0.02 to 0.1: Stable
#' 0.1 to 0.2: Unstable
#' 0.2 to 0.5] : Change
#' more than 0.5: Great change
#' @examples
#' #  dat_test is null
#' get_psi(dat = UCICreditCard, x = "PAY_3", occur_time = "apply_date")
#' # dat_test is not all
#' # train_test split
#' train_test = train_test_split(dat = UCICreditCard, prop = 0.7, split_type = "OOT",
#'                              occur_time = "apply_date", start_date = NULL, cut_date = NULL,
#'                             save_data = FALSE, note = FALSE)
#' dat_ex = train_test$train
#' dat_ac = train_test$test
#' # generate psi table
#' get_psi(dat = dat_ex, dat_test = dat_ac, x = "PAY_3",
#'        occur_time = "apply_date", bins_no = TRUE)
#' @export



get_psi_all = function(dat, x_list = NULL,target = NULL, dat_test = NULL, breaks_list = NULL, occur_time = NULL,
                        start_date = NULL, cut_date = NULL, oot_pct = 0.7, pos_flag = NULL,
                        parallel = FALSE, ex_cols = NULL, as_table = FALSE, g = 10, bins_no = TRUE, note = FALSE) {
  if (note)cat_line("-- Calculating PSI", col = love_color("dark_green"))
  opt = options(scipen = 200, stringsAsFactors = FALSE) #
  if (is.null(x_list)) {
    if (!is.null(breaks_list)) {
      x_list = unique(as.character(breaks_list[, "Feature"]))
    } else {
      x_list = get_x_list(x_list = x_list, dat_train = dat, dat_test = dat_test, ex_cols = c(target, occur_time, ex_cols))
    }
  }
  if (is.null(dat_test) && !is.null(occur_time) && any(names(dat) == occur_time)) {
    if (!is_date(dat[, occur_time])) {
      dat = time_transfer(dat, date_cols = occur_time, note = FALSE)
    }
    if (is_date(dat[, occur_time])) {
      if (is.null(cut_date)) {
        cut_date = date_cut(dat_time = dat[, occur_time], pct = oot_pct)
      }
      if (is.null(start_date)) {
        start_date = date_cut(dat_time = dat[, occur_time], pct = 0)
      }
    } else {
      stop(paste(occur_time, "is not Date or Time"))
    }
  }
  psi_list = loop_function(func = get_psi, x_list = x_list,
                           args = list(dat = dat, dat_test = dat_test,
                                       breaks = NULL, breaks_list = breaks_list,
                                       occur_time = occur_time, start_date = start_date,
                                       cut_date = cut_date,oot_pct = oot_pct,
                                       target = target, pos_flag = pos_flag,
                                       as_table = as_table, g = g, note = note,
                                       bins_no = bins_no), bind = "rbind",
                           parallel = parallel)
  return(psi_list)
  options(opt) # reset
}


#' @rdname get_psi_all
#' @export


get_psi = function(dat, x, target = NULL, dat_test = NULL, occur_time = NULL, start_date = NULL, cut_date = NULL,
         pos_flag = NULL, breaks = NULL, breaks_list = NULL, oot_pct = 0.7, g = 10,
         as_table = TRUE, note = FALSE, bins_no = TRUE) {
  bins = PSI = actual = expected = actual_1= actual_0= expected_1= expected_0 = NULL
  if (!is.null(breaks_list)) {
    breaks = breaks_list[which(as.character(breaks_list[, "Feature"]) == names(dat[x])), "cuts"]
  }
  if (is.null(breaks)) {
    breaks = get_breaks(dat = dat, x = x,target = target, pos_flag = pos_flag, equal_bins = TRUE, best = FALSE, g = g, note = FALSE)
  }

  if (is.null(dat_test)) {
    dat$bins = split_bins(dat = dat, x = x, breaks = breaks, bins_no = bins_no)
    if(!is.null(target)){
      dat$target = dat[,target]
    }
    df_ae = train_test_split(dat = dat, prop = oot_pct, split_type = "OOT", occur_time = occur_time,
                             start_date = start_date, cut_date = cut_date, save_data = FALSE, note = FALSE)
    dfe = df_ae$train
    dfa = df_ae$test
    dfa$ae = "actual"
    dfe$ae = "expected"
    df_ae = rbind(dfa, dfe)
  } else {
    dat$ae = "expected"
    dat_test$ae = "actual"
    if(!is.null(target)){
      dat$target = dat[,target]
      dat_test$target = dat_test[,target]
    }
    dat$bins = split_bins(dat = dat, x = x, breaks = breaks, bins_no = bins_no)
    dat_test$bins = split_bins(dat = dat_test, x = x, breaks = breaks, bins_no = bins_no)
    dfe = dat[, c("ae", "bins",target)]
    dfa = dat_test[, c("ae", "bins",target)]

    df_ae = rbind(dfa, dfe)
  }
  df_ae = as.data.table(df_ae)
  if(!is.null(target)){
    df_ae$target =df_ae[,target,with = FALSE]
    df_psi = data.table::dcast(df_ae, bins~ ae + target, fun.aggregate = length, value.var = "ae")
	df_psi = quick_as_df(df_psi)
    df_psi = within(df_psi, {
      actual = actual_1 + actual_0
      expected = expected_1 + expected_0
      Ac_pct_1 = actual_1/ifelse(actual > 0, actual, 1)
      Ex_pct_1 = expected_1/ifelse(expected > 0,expected, 1)
      cos_s = round(cos_sim(Ex_pct_1, Ac_pct_1, cos_margin = 2 ),3)
      Ac_pct = actual / sum(actual,na.rm = TRUE)
      Ex_pct = expected/ sum(expected, na.rm = TRUE)
      PSI_i = round((Ac_pct - Ex_pct) * log(Ac_pct / Ex_pct), 3)
    })

    if (as_table) {
      dat_psi = data.frame(Feature = x, Bins = df_psi$bins,
                           actual = df_psi$actual,
                           expected = df_psi$expected,
                           Ac_pct = as_percent(df_psi$Ac_pct, digits = 3),
                           Ex_pct = as_percent(df_psi$Ex_pct, digits = 3),
                           PSI_i = df_psi$PSI_i,
                           PSI = as.numeric(sum(df_psi$PSI_i,na.rm = TRUE)),
                           COS = df_psi$cos_s )
    } else {
      dat_psi = data.frame(Feature = x, PSI = as.numeric(sum(df_psi$PSI_i,na.rm = TRUE)),
                           COS = max(df_psi$cos_s,na.rm = TRUE))
    }
    dt_psi = within(dat_psi, {
      stability = "Great change"
      stability[PSI <= 0.02] = "Very stable"
      stability[PSI > 0.02 & PSI <= 0.1] = "Stable"
      stability[PSI > 0.1 & PSI <= 0.2] = "Unstable"
      stability[PSI > 0.2 & PSI <= 0.5] = "Change"
      stability[PSI > 0.5] = "Great change"
    })
    if (note) {
	 cat_line(paste0("--",x), col = love_color("water_blue"))
     cat_bullet(paste("PSI:", as.numeric(dt_psi$PSI[1]), " --> ", dt_psi$stability[1]), col = "darkgrey")
    }
  }else{
    df_psi = data.table::dcast(df_ae, bins~ ae, fun.aggregate = length, value.var = "ae")
	df_psi = quick_as_df(df_psi)
    df_psi = within(df_psi, {
      Ac_pct = ifelse(actual > 0, actual, 1) / sum(actual)
      Ex_pct = ifelse(expected > 0,expected, 1) / sum(expected)
      PSI_i = round((Ac_pct - Ex_pct) * log(Ac_pct / Ex_pct), 3)
    })
    if (as_table) {
      dat_psi = data.frame(Feature = x, Bins = df_psi$bins, actual = df_psi$actual, expected = df_psi$expected,
                           Ac_pct = as_percent(df_psi$Ac_pct, digits = 3), Ex_pct = as_percent(df_psi$Ex_pct, digits = 3),
                           PSI_i = df_psi$PSI_i, PSI = as.numeric(sum(df_psi$PSI_i)))
    } else {
      dat_psi = data.frame(Feature = x, PSI = as.numeric(sum(df_psi$PSI_i)))
    }
    dt_psi = within(dat_psi, {
      stability = "Great change"
      stability[PSI <= 0.02] = "Very stable"
      stability[PSI > 0.02 & PSI <= 0.1] = "Stable"
      stability[PSI > 0.1 & PSI <= 0.2] = "Unstable"
      stability[PSI > 0.2 & PSI <= 0.5] = "Change"
      stability[PSI > 0.5] = "Great change"
    })
    if (note) {
	cat_line(paste0("--",x), col = love_color("water_blue"))
     cat_bullet(paste("PSI:", as.numeric(dt_psi$PSI[1]), " --> ", dt_psi$stability[1]), col = "darkgrey")
    }
  }

  rm(df_psi, dt_psi)
  return(dat_psi)
}



