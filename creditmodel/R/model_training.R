#'Training model
#'
#' \code{training_model} Model builder
#' @param model_name  A string, name of the project. Default is "mymodel"
#' @param model_path The path for periodically saved data file. Default is \code{tempdir()}.
#' @param dat A data.frame with independent variables and target variable.
#' @param dat_test  A data.frame of test data. Default is NULL.
#' @param target The name of target variable.
#' @param x_list Names of independent variables. Default is NULL.
#' @param occur_time The name of the variable that represents the time at which each observation takes place.Default is NULL.
#' @param pos_flag The value of positive class of target variable, default: "1".
#' @param ex_cols Names of excluded variables. Regular expressions can also be used to match variable names. Default is NULL.
#' @param prop Percentage of train-data after the partition. Default: 0.7.
#' @param split_type  Methods for partition. See details at :  \code{\link{train_test_split}}.
#' @param obs_id  The name of ID of observations or key variable of data. Default is NULL.
#' @param preproc Logical. Preprocess data. Default is TRUE.
#' @param outlier_proc Logical, process outliers or not. Default is TRUE.
#' @param missing_proc If logical, process missing values or not. If "median", then Nas imputation with k neighbors median. If "avg_dist", the distance weighted average method is applied to determine the NAs imputation with k neighbors. If "default", assigning the missing values to -1 or "missing", otherwise ,processing the missing values according to the results of missing analysis.
#' @param miss_values  Other extreme value might be used to represent missing values, e.g: -9999, -9998. These miss_values will be encoded to -1 or "missing".
#' @param default_miss  Default value of missing data imputation, Defualt is list(-1,'missing').
#' @param missing_rate The maximum percent of missing values for recoding values to missing and non_missing.
#' @param one_hot  Logical. If TRUE, one-hot_encoding  of category variables. Default is FASLE.
#' @param trans_log  Logical, Logarithmic transformation. Default is FALSE.
#' @param low_var  Logical, delete low variance variables or not. Default is TRUE.
#' @param merge_cat merge categories of character variables that  is more than m.
#' @param remove_dup  Logical, if TRUE, remove the duplicated observations.
#' @param feature_filter  Parameters for selecting important and stable features.See details at: \code{\link{feature_selector}}
#' @param algorithm  Algorithms for training a model. list("LR", "XGB", "GBDT", "RF") are available.
#' @param LR.params  Parameters of logistic regression & scorecard. See details at :  \code{\link{lr_params}}.
#' @param XGB.params Parameters of xgboost. See details at :  \code{\link{xgb_params}}.
#' @param GBM.params Parameters of GBM. See details at :  \code{\link{gbm_params}}.
#' @param RF.params  Parameters of Random Forest. See details at :  \code{\link{rf_params}}.
#' @param breaks_list A table containing a list of splitting points for each independent variable. Default is NULL.
#' @param parallel  Default is FALSE.
#' @param cores_num The number of CPU cores to use.
#' @param save_pmml Logical, save model in PMML format. Default is TRUE.
#' @param vars_plot Logical, if TRUE, plot distribution ,correlation or partial dependence of model input variables . Default is TRUE.
#' @param plot_show Logical, show model performance in current graphic device. Default is FALSE.
#' @param ...  Other parameters.
#' @param seed  Random number seed. Default is 46.
#' @return A list containing Model Objects.
#' @seealso   \code{\link{train_test_split}},\code{\link{data_cleansing}}, \code{\link{feature_selector}},   \code{\link{lr_params}}, \code{\link{xgb_params}}, \code{\link{gbm_params}}, \code{\link{rf_params}},\code{\link{fast_high_cor_filter}},\code{\link{get_breaks_all}},\code{\link{lasso_filter}}, \code{\link{woe_trans_all}}, \code{\link{get_logistic_coef}}, \code{\link{score_transfer}},\code{\link{get_score_card}}, \code{\link{model_key_index}},\code{\link{ks_psi_plot}},\code{\link{ks_table_plot}}
#' @examples
#' sub = cv_split(UCICreditCard, k = 30)[[1]]
#' dat = UCICreditCard[sub,]
#' x_list = c("LIMIT_BAL")
#' B_model = training_model(dat = dat,
#'                          model_name = "UCICreditCard",
#'                          target = "default.payment.next.month",
#' 							x_list = x_list,
#'                          occur_time =NULL,
#'                          obs_id =NULL,
#' 							dat_test = NULL,
#'                          preproc = FALSE,
#'                          outlier_proc = FALSE,
#'                          missing_proc = FALSE,
#'                          feature_filter = NULL,
#'                          algorithm = list("LR"),
#'                          LR.params = lr_params(lasso = FALSE,
#'                                                step_wise = FALSE,
#'                                                  score_card = FALSE),
#'                          breaks_list = NULL,
#'                          parallel = FALSE,
#'                          cores_num = NULL,
#'                          save_pmml = FALSE,
#'                          plot_show = FALSE,
#'                          vars_plot = FALSE,
#'                          model_path = tempdir(),
#'                          seed = 46)
#'
#' @import ggplot2
#' @importFrom foreach foreach
#' @importFrom grid gTree
#' @importFrom xgboost xgb.importance xgb.train xgb.DMatrix xgb.dump xgb.save xgb.cv getinfo xgb.load
#' @importFrom dplyr group_by mutate summarize  summarise n  count %>% filter left_join distinct
#' @importFrom data.table fwrite fread dcast melt fifelse
#' @importFrom glmnet cv.glmnet glmnet
#' @importFrom grDevices dev.print png rgb
#' @importFrom graphics par plot text
#' @importFrom stats  ar as.formula binomial chisq.test coef complete.cases cor cov glm  kmeans median  na.omit  na.pass predict reorder runif sd ts
#' @importFrom cli cat_rule cat_line cat_bullet
#' @export

training_model = function(model_name = "mymodel",
						   dat,
						   dat_test = NULL,
						   target = NULL,
						   occur_time = NULL,
						   obs_id = NULL,
						   x_list = NULL,
						   ex_cols = NULL,
						   pos_flag = NULL,
						   prop = 0.7,
						   split_type = if (!is.null(occur_time)) "OOT" else "Random",
						   preproc = TRUE,
						   low_var = 0.99,
						   missing_rate = 0.98,
						   merge_cat = 30,
						   remove_dup = TRUE,
						   outlier_proc = TRUE,
						   missing_proc = 'median',
						   default_miss = list(-1,'missing'),
						   miss_values = NULL,
						   one_hot = FALSE,
						   trans_log = FALSE,
						   feature_filter = list(filter = c("IV", "PSI", "COR", "XGB"),
												 iv_cp = 0.02, psi_cp = 0.1, xgb_cp = 0,
												 cv_folds = 1, hopper = FALSE),
						   algorithm = list("LR", "XGB", "GBM", "RF"),
						   LR.params = lr_params(),
						   XGB.params = xgb_params(),
						   GBM.params = gbm_params(),
						   RF.params = rf_params(),
						   breaks_list = NULL,
						   parallel = FALSE, cores_num = NULL,
						   save_pmml = FALSE, plot_show = FALSE,
						   vars_plot = TRUE,
						   model_path = tempdir(),
						   seed = 46, ...) {

	opt = options(scipen = 200, stringsAsFactors = FALSE, digits = 10)
	cat_rule(left = "Building", right = model_name, col = "cyan")
	if (is.null(algorithm)) {
		stop(paste("algorithm is missing.\n"))
	}
	if (!any(is.element(algorithm, c("LR", "XGB", "GBM", "RF")))) {
		stop("In algorithm, only LR, XGB, GBM, RF are supported.\n")
	}
	if (length(x_list) > 0 && any(x_list == target)) {
		stop(paste("x_list  contains", target, ".\n"))
	}
	cat_rule("Creating the model output file path", col = love_color("light_cyan"))
	if (!dir.exists(model_path)) dir.create(model_path)
	if (!is.character(model_name)) model_name = "my_model"
	model_path = ifelse(!is.character(model_path),
					  paste(tempdir(), model_name, sep = "/"), paste(model_path, model_name, sep = "/"))
	if (!dir.exists(model_path)) dir.create(model_path)
	model_dir_path = paste(model_path, "model", sep = "/")
	data_dir_path = paste(model_path, "data", sep = "/")
	var_dir_path = paste(model_path, "variable", sep = "/")
	perf_dir_path = paste(model_path, "performance", sep = "/")
	pred_dir_path = paste(model_path, "predict", sep = "/")
	if (!dir.exists(model_dir_path)) dir.create(model_dir_path)
	if (!dir.exists(data_dir_path)) dir.create(data_dir_path)
	if (!dir.exists(var_dir_path)) dir.create(var_dir_path)
	if (!dir.exists(perf_dir_path)) dir.create(perf_dir_path)
	if (!dir.exists(pred_dir_path)) dir.create(pred_dir_path)
	paths = list(model = model_dir_path, data = data_dir_path,
			   variable = var_dir_path, performance = perf_dir_path,
			   predict = pred_dir_path)
	cat_line("-- Seting model output file path:", col = love_color("dark_green"))
	cat_bullet(paste0(format(names(paths)), ": ", unname(paths)), col = "darkgrey")
	#prepare parallel computing
	if (parallel) {
		parallel = start_parallel_computing(parallel)
		stopCluster = TRUE
	} else {
		parallel = stopCluster = FALSE
	}
	if (is.null(cores_num)) {
		cores_num = parallel::detectCores()
	}
	if (parallel) cat_line(paste("--", cores_num, "cores will be used for parallel computing.\n"), col = love_color("sky_blue"))
	on.exit(if (parallel & stopCluster) stop_parallel_computing(attr(parallel, "cluster")))

	cat_rule("Checking datasets and target", col = love_color("light_cyan"))
	if (!is.null(dat_test)) {
		dat = checking_data(dat = dat, target = target, pos_flag = pos_flag)
		dat_test = checking_data(dat = dat_test, target = target, pos_flag = pos_flag)
		x_list = get_x_list(x_list = x_list, note = TRUE,
						dat_train = dat, dat_test = dat_test,
						ex_cols = c(target, obs_id, occur_time, ex_cols))
		nr = nrow(dat)
		com_list = unique(c(obs_id, occur_time, target, x_list))
		dat = dat[, com_list]
		dat_test = dat_test[, com_list]
		dat = rbind(dat, dat_test)
	} else {
		dat = checking_data(dat = dat, target = target, pos_flag = pos_flag)
		x_list = get_x_list(x_list = x_list, note = TRUE,
						dat_train = dat, dat_test = dat_test,
						ex_cols = c(target, obs_id, occur_time, ex_cols))
	}
	if (!is.null(x_list)) {
		dat = dat[unique(c(obs_id, target, occur_time, x_list))]
	}


	if (preproc) {
		cat_rule("Cleansing & Prepocessing data", col = love_color("light_cyan"))
		dat = data_cleansing(dat = dat, x_list = x_list,
						 target = target, obs_id = obs_id, occur_time = occur_time,
						 ex_cols = ex_cols, remove_dup = remove_dup,
						outlier_proc = FALSE, missing_proc = FALSE,
						 miss_values = miss_values, missing_rate = missing_rate,
						 low_var = low_var, merge_cat = merge_cat, parallel = parallel, note = TRUE,
						 save_data = TRUE, dir_path = data_dir_path, file_name = NULL)
	}
	char_x_list = get_names(dat = dat,
						  types = c('factor', 'character'),
						  ex_cols = c(obs_id, target, occur_time, ex_cols),
						  get_ex = FALSE)
	num_x_list = get_names(dat = dat,
						 types = c('numeric', 'integer', 'double', 'integer64'),
						 ex_cols = c(obs_id, target, occur_time, ex_cols),
						 get_ex = FALSE)


	if (trans_log & length(num_x_list) > 0 & !is.null(target)) {
		dat = log_trans(dat = dat, target = target, x_list = num_x_list, cor_dif = 0.01,
		ex_cols = ex_cols, note = TRUE)
		num_x_list = get_names(dat = dat,
							   types = c('numeric', 'integer', 'double', 'integer64'),
							   ex_cols = c(obs_id, target, occur_time, ex_cols),
							   get_ex = FALSE)

	}

	if (one_hot & length(char_x_list) > 0) {
		dat = one_hot_encoding(dat = dat, cat_vars = char_x_list, na_act = FALSE, note = TRUE)
		char_x_list = get_names(dat = dat,
						  types = c('factor', 'character'),
						  ex_cols = c(obs_id, target, occur_time, ex_cols),
						  get_ex = FALSE)
		num_x_list = get_names(dat = dat,
						 types = c('numeric', 'integer', 'double', 'integer64'),
						 ex_cols = c(obs_id, target, occur_time, ex_cols),
						 get_ex = FALSE)
	}
	x_list = c(char_x_list, num_x_list)
	#train test spliting

	if (is.null(dat_test)) {
		cat_rule("Spliting train & test", col = love_color("light_cyan"))
		train_test = train_test_split(dat, split_type = split_type,
								   prop = prop, occur_time = occur_time, note = TRUE,
								   save_data = TRUE, dir_path = data_dir_path,
								   file_name = NULL,
								   seed = seed)
		dat_train = train_test$train
		dat_test = train_test$test
	} else {
		train_test = train_test_split(dat, split_type = "byRow", prop = nr / nrow(dat),
								  occur_time = occur_time, seed = seed, note = TRUE,
								  save_data = TRUE, dir_path = data_dir_path, file_name = NULL)
		dat_train = train_test$train
		dat_test = train_test$test
	}

	if (outlier_proc) {
		dat_train = process_outliers(dat = dat_train, target = target,
								 x_list = num_x_list, ex_cols = c(obs_id, occur_time, target),
								 parallel = parallel, note = TRUE, save_data = TRUE,
								 file_name = NULL, dir_path = data_dir_path)
	}

	if ((is.logical(missing_proc) && missing_proc) || (is.character(missing_proc) && is.element(missing_proc,
	c("median", "avg_dist", "default")))) {
		if (is.character(missing_proc) && is.element(missing_proc, c("median", "avg_dist", "default"))) {
			method = missing_proc
			dat_train = process_nas(dat = dat_train, class_var = FALSE, x_list = x_list,
							ex_cols = c(obs_id, occur_time, target),default_miss = default_miss,
							miss_values = unique(append(miss_values, list("missing"))),
							parallel = parallel,
							method = method, note = TRUE, save_data = TRUE,
							file_name = NULL, dir_path = data_dir_path)
			dat_test = process_nas(dat = dat_test, class_var = FALSE, x_list = x_list,
							ex_cols = c(obs_id, occur_time, target),default_miss = default_miss,
							miss_values = unique(append(miss_values, list("missing"))),
							parallel = parallel,
							method = method, note = FALSE, save_data = FALSE,
							file_name = NULL, dir_path = data_dir_path)
		} else {
			dat_train = process_nas(dat = dat_train, class_var = FALSE, x_list = x_list,
							ex_cols = c(obs_id, occur_time, target),default_miss = default_miss,
							miss_values = unique(append(miss_values, list("missing"))),
							parallel = parallel,
							method = "median", note = FALSE, save_data = FALSE,
							file_name = NULL, dir_path = data_dir_path)
			dat_test = process_nas(dat = dat_test, class_var = FALSE, x_list = x_list,
							ex_cols = c(obs_id, occur_time, target),default_miss = default_miss,
							miss_values = unique(append(miss_values, list("missing"))),
							parallel = parallel,
							method = "median", note = FALSE, save_data = FALSE,
							file_name = NULL, dir_path = data_dir_path)
		}
	} else {
		dat_train = process_nas(dat = dat_train, class_var = FALSE, x_list = x_list,
							ex_cols = c(obs_id, occur_time, target),default_miss = default_miss,
							miss_values = unique(append(miss_values, list("missing"))),
							parallel = parallel,
							method = "default", note = FALSE, save_data = FALSE,
							file_name = NULL, dir_path = data_dir_path)
		dat_test = process_nas(dat = dat_test, class_var = FALSE, x_list = x_list,
							ex_cols = c(obs_id, occur_time, target),default_miss = default_miss,
							miss_values = unique(append(miss_values, list("missing"))),
							parallel = parallel,
							method = "default", note = FALSE, save_data = FALSE,
							file_name = NULL, dir_path = data_dir_path)
	}


	if (!is.null(feature_filter) && is.list(feature_filter)) {
		sel_list = NULL
		if (!is.null(feature_filter[["filter"]]) &&
			any(is.element(feature_filter[["filter"]], c("IV", "PSI", "XGB", "COR")))) {
			filter = feature_filter[["filter"]]
		} else {
			filter = c("IV", "PSI", "XGB", "COR")
		}
		cat_rule("Filtering features", col = love_color("light_cyan"))
		cv_folds = ifelse(!is.null(feature_filter[["cv_folds"]]), feature_filter[["cv_folds"]], 1)
		iv_cp = ifelse(!is.null(feature_filter[["iv_cp"]]), feature_filter[["iv_cp"]], 0.01)
		psi_cp = ifelse(!is.null(feature_filter[["psi_cp"]]), feature_filter[["psi_cp"]], 0.1)
		xgb_cp = ifelse(!is.null(feature_filter[["xgb_cp"]]), feature_filter[["xgb_cp"]], 0)
		cor_cp = ifelse(!is.null(feature_filter[["cor_cp"]]), feature_filter[["cor_cp"]], 0.98)
		hopper = ifelse(!is.null(feature_filter[["hopper"]]), feature_filter[["hopper"]], TRUE)
		sel_list = feature_selector(dat_train = dat_train,
									  target = target,
									  x_list = x_list, occur_time = occur_time,
									  ex_cols = c(obs_id, occur_time, target, ex_cols),
									  filter = filter, cv_folds = cv_folds,
									  iv_cp = iv_cp, psi_cp = psi_cp,
									  cor_cp = cor_cp, xgb_cp = xgb_cp, hopper = hopper,
									  vars_name = TRUE, save_data = TRUE, parallel = parallel,
									  cores_num = cores_num, seed = seed,
									  file_name = NULL, note = TRUE,
									  dir_path = var_dir_path)
		if (length(sel_list) > 0) {
			x_list = get_x_list(x_list = sel_list,
						  dat_train = dat_train, dat_test = dat_test,
						  ex_cols = c(target, obs_id, occur_time, ex_cols))
		} else {
			warning("No feature satisfies the criteria for feature selection, use the previous x_list.\n")
		}
	}
	model_new = lr_model_new = xgb_model_new = gbdt_model_new = rf_model_new = NULL
	# Train Models
	if (length(unique(dat_train[, target])) == 2) {
		pmml = NULL
		if (any(algorithm == "LR")) {
			cat_rule("Training logistic regression model/scorecard", col = "cyan")

			LR_model_dir_path = paste(model_dir_path, "LR", sep = "/")
			LR_var_dir_path = paste(var_dir_path, "LR", sep = "/")
			LR_perf_dir_path = paste(perf_dir_path, "LR", sep = "/")
			LR_pred_dir_path = paste(pred_dir_path, "LR", sep = "/")
			LR_data_dir_path = paste(data_dir_path, "LR", sep = "/")

			if (!dir.exists(LR_model_dir_path)) dir.create(LR_model_dir_path)
			if (!dir.exists(LR_var_dir_path)) dir.create(LR_var_dir_path)
			if (!dir.exists(LR_perf_dir_path)) dir.create(LR_perf_dir_path)
			if (!dir.exists(LR_pred_dir_path)) dir.create(LR_pred_dir_path)
			if (!dir.exists(LR_data_dir_path)) dir.create(LR_data_dir_path)
			if (is.null(LR.params)) {
				LR.params = lr_params()
			}
			obsweight = if (!is.null(LR.params[["obsweight"]])) LR.params[["obsweight"]] else 1
			step_wise = ifelse(!is.null(LR.params[["step_wise"]]), LR.params[["step_wise"]], TRUE)
			lasso = ifelse(!is.null(LR.params[["lasso"]]), LR.params[["lasso"]], TRUE)
			score_card = ifelse(!is.null(LR.params[["score_card"]]), LR.params[["score_card"]], TRUE)
			sp_values = LR.params[["sp_values"]]
			forced_in = LR.params[["forced_in"]]
			iters = ifelse(!is.null(LR.params[["iters"]]), LR.params[["iters"]], 10)
			thresholds = LR.params[["thresholds"]]

			f_eval = ifelse(!is.null(LR.params[["f_eval"]]), LR.params[["f_eval"]], "ks")
			method = ifelse(!is.null(LR.params[["method"]]), LR.params[["method"]], "random_search")
			best_lambda = ifelse(!is.null(LR.params[["best_lambda"]]), LR.params[["best_lambda"]], "lambda.ks")

			thresholds = if (!is.null(LR.params["thresholds"])) {
				LR.params[["thresholds"]]
			} else {
				list(cor_p = 0.7, iv_i = 0.01, psi_i = 0.2, cos_i = 0.5)
			}
			cor_p = thresholds$cor_p
			iv_i = thresholds$iv_i
			psi_i = thresholds$psi_i
			cos_i = thresholds$cos_i

			tree_control = if (!is.null(LR.params["tree_control"])) {
				LR.params[["tree_control"]]
			} else {
				list(p = 0.02, cp = 0.00000001, xval = 5, maxdepth = 10)
			}

			bins_control = if (!is.null(LR.params["bins_control"])) {
				LR.params[["bins_control"]]
			} else {
				list(bins_num = 10, bins_pct = 0.05,
			 b_chi = 0.02, b_odds = 0.1, b_psi = 0.03,
			 b_or = 0.15, mono = 0.2, odds_psi = 0.15, kc = 1)
			}
			if (length(obsweight) > 1 && is.vector(obsweight, mode = "numeric")) {
				obsweighted = ifelse(dat_train[, target] == 0, obsweight[1], obsweight[2])
			} else {
				obsweighted = NULL
			}
			bins_params = bins_control

			if (any(sapply(tree_control, length) > 1, sapply(bins_control, length) > 1, sapply(thresholds, length) > 1)) {
				cat_rule("Searching optimal binning & feature selection parameters", col = love_color("light_cyan"))
				lr.params.search = lr_params_search(method = method, dat_train = dat_train, target = target,
										   iters = iters, x_list = x_list,
											occur_time = occur_time,
											tree_control = tree_control,
											bins_control = bins_control,
											thresholds = thresholds,
											lasso = lasso, step_wise = step_wise,
											f_eval = f_eval)
				bins_control = lr.params.search$bins_control
				tree_control = lr.params.search$tree_control
				thresholds = lr.params.search$thresholds
				cor_p = thresholds$cor_p
				iv_i = thresholds$iv_i
				psi_i = thresholds$psi_i
				cos_i = thresholds$cos_i
				x_list = lr.params.search$x_list
				seed = lr.params.search$best_seed
			}
			#bins breaks
			x_list = unique(c(forced_in, unlist(x_list)))
			if (is.null(breaks_list) || length(breaks_list) < 1) {
				cat_rule("Constrained optimal binning of varibles", col = love_color("light_cyan"))

				breaks_list = get_breaks_all(dat = dat_train,
									 x_list = x_list,
									 ex_cols = c(obs_id, occur_time, target, ex_cols),
									 occur_time = occur_time, oot_pct = prop,
									 target = target,
									 tree_control = tree_control,
									 bins_control = bins_control,
									 best = TRUE,
									 sp_values = sp_values, parallel = parallel,
									 note = TRUE,
									 save_data = TRUE,
									 file_name = "breaks_list",
									 dir_path = LR_var_dir_path)
			}

			#psi_iv_filter
			cat_rule("Filtering variables by IV & PSI", col = love_color("light_cyan"))

			iv_psi_list = psi_iv_filter(dat = dat_train, dat_test = dat_test,
								  x_list = x_list, target = target,
								  occur_time = occur_time, oot_pct = prop,
								  parallel = parallel,
								  ex_cols = c(obs_id, occur_time, target, ex_cols),
								  breaks_list = breaks_list,
								  psi_i = psi_i, iv_i = iv_i, cos_i = cos_i,
								  note = TRUE,
								  save_data = TRUE,
								  file_name = NULL,
								  dir_path = LR_var_dir_path)
			if (length(iv_psi_list) > 0 && nrow(iv_psi_list) >= 1) {
				select_vars = as.character(iv_psi_list[, "Feature"])
			} else {
				select_vars = x_list
				cat_rule(paste("No variable satisfies the psi & iv condition.\n"), col = love_color("deep_orange"))
			}


			select_vars = unique(c(forced_in, unlist(select_vars)))
			save_data(select_vars, as_list = TRUE, row_names = FALSE, note = TRUE,
			  file_name = "LR.IV_PSI_features",
			  dir_path = LR_var_dir_path)
			cat_rule("Transforming WOE", col = love_color("light_cyan"))

			train_woe = woe_trans_all(dat = dat_train,
								x_list = select_vars,
								target = target,
								ex_cols = c(target, occur_time, obs_id),
								bins_table = NULL,
								breaks_list = breaks_list,
								woe_name = FALSE, note = TRUE, parallel = parallel,
								save_data = TRUE, file_name = "lr_train",
								dir_path = LR_data_dir_path)

			cat_rule("Filtering variables by correlation", col = love_color("light_cyan"))

			if (length(select_vars) > 1) {
				select_vars = fast_high_cor_filter(dat = train_woe,
										 x_list = select_vars,
										 com_list = iv_psi_list,
										 ex_cols = c(target, occur_time, obs_id, ex_cols),
										 p = cor_p,
										 note = FALSE,
										 save_data = TRUE,
										 file_name = model_name,
										 dir_path = LR_var_dir_path)
				select_vars = unique(c(forced_in, unlist(select_vars)))
			}

			bins_table = get_bins_table_all(dat = dat_train, target = target,
									  ex_cols = c(target, occur_time, obs_id, ex_cols),
									  x_list = select_vars, breaks_list = breaks_list,
									  dat_test = dat_test,
									  note = TRUE, save_data = TRUE,
									  file_name = model_name,
									  dir_path = LR_perf_dir_path)

			test_woe = woe_trans_all(dat = dat_test,
							   x_list = unlist(select_vars),
							   target = target,
							   ex_cols = c(target, occur_time, obs_id),
							   bins_table = bins_table,
							   breaks_list = breaks_list,
							   note = FALSE, woe_name = FALSE, parallel = parallel,
							   save_data = TRUE, file_name = "lr_test",
							   dir_path = LR_data_dir_path)

			if (lasso) {
				cat_rule("Filtering variables by LASSO", col = love_color("light_cyan"))

				select_vars = lasso_filter(dat_train = train_woe, dat_test = test_woe,
								   x_list = select_vars,
								   target = target,
								   ex_cols = c(target, occur_time, obs_id, ex_cols),
								   sim_sign = "negtive",
								   best_lambda = best_lambda,
								   plot.it = FALSE, seed = seed,
								   save_data = TRUE, file_name = "LR", dir_path = LR_var_dir_path)
			}
			select_vars = unique(c(forced_in, select_vars))
			save_data(select_vars, as_list = TRUE, row_names = FALSE, note = TRUE,
			  file_name = "lr_premodel_features", dir_path = LR_var_dir_path)

			cat_rule("Start training lr model", col = love_color("light_cyan"))
			Formula = as.formula(paste(target, paste(unique(c(forced_in, select_vars)), collapse = ' + '),
								 sep = ' ~ '))
			if (!is.null(seed)) set.seed(seed) else set.seed(46)
			lr_model = glm(Formula,
					 data = train_woe[, c(target, unique(c(forced_in, select_vars)))],
					 family = binomial(logit),
					 weights = obsweighted)
			dt_coef = data.frame(summary(lr_model)$coefficients)
			lg_coef = subset(dt_coef, abs(dt_coef$Estimate) > 0)
			glm_vars = row.names(lg_coef)[-1]
			Formula = as.formula(paste(target, paste(unique(c(forced_in, glm_vars)), collapse = ' + '), sep = ' ~ '))
			lr_model_new = glm(Formula,
						 data = train_woe[, c(target, unique(c(forced_in, glm_vars)))],
						 family = binomial(logit),
						 weights = obsweighted)
			#step wise
			if (step_wise) {
				lr_model_step = stats::step(lr_model_new, dir_pathection = "both", trace = TRUE)
				dt_step_coef = data.frame(summary(lr_model_step)$coefficients)
				step_vars = row.names(dt_step_coef)[-1]
				Formula = as.formula(paste(target, paste(unique(c(forced_in, step_vars)), collapse = ' + '), sep = ' ~ '))
				lr_model_new = glm(Formula,
						   data = train_woe[, c(target, unique(c(forced_in, step_vars)))],
						   family = binomial(logit),
						   weights = obsweighted)
			}
			# get lr coef
			dt_imp_LR = get_logistic_coef(lg_model = lr_model_new, file_name = NULL,
									dir_path = LR_perf_dir_path, save_data = FALSE)
			lr_vars = dt_imp_LR[-1, "Feature"]
			save_data(lr_vars, as_list = TRUE,
			  file_name = "lr_model_features",
			  dir_path = LR_var_dir_path, note = TRUE)
			if (length(iv_psi_list) > 0 && nrow(iv_psi_list) > 1) {
				LR_iv_psi = subset(iv_psi_list, iv_psi_list$Feature %in% lr_vars)[1:3]
				LR_iv_psi = rbind(c("(Intercept)", 0, 0), LR_iv_psi)
				dt_imp_LR = merge(dt_imp_LR, LR_iv_psi)
			}
			imp_vars = dt_imp_LR[-1, "Feature"]
			save_data(dt_imp_LR,
			  file_name = paste0(model_name, ".lr_coef"),
			  dir_path = LR_perf_dir_path, note = TRUE)
			#correlation matrix plot of input variables
			if (vars_plot & length(imp_vars) > 1) {
				cat_line("-- Ploting correlation matrix of input variables", col = love_color("deep_green"))

				cor_plot(dat = train_woe, x_list = imp_vars,
				 dir_path = LR_var_dir_path, save_data = TRUE,
				 gtitle = "lr.")
				cat_line("-- Ploting distribution of input variables", col = love_color("deep_green"))


			}

			if (score_card) {
				# standard socre card
				cat_rule("Generating standard socrecard", col = love_color("light_cyan"))

				LR_score_card = get_score_card(lg_model = lr_model_new,
										bins_table, target = target,
										file_name = model_name,
										dir_path = LR_perf_dir_path,
										save_data = TRUE)
				cat_line("-- Using scorecard to predict the train and test", col = love_color("deep_green"))

				train_score = dat_train[c(obs_id, occur_time, target)]
				test_score = dat_test[c(obs_id, occur_time, target)]
				train_score$score_LR = score_transfer(model = lr_model_new,
											  tbl_woe = train_woe,
											  save_data = TRUE)[, "score"]
				test_score$score_LR = score_transfer(model = lr_model_new,
											 tbl_woe = test_woe,
											 save_data = TRUE)[, "score"]
				save_data(train_score, file_name = "lr_train_score",
				dir_path = LR_pred_dir_path, note = TRUE)
				save_data(test_score, file_name = "lr_test_score",
				dir_path = LR_pred_dir_path, note = TRUE)
			}


			train_pred = dat_train[c(obs_id, occur_time, target)]
			test_pred = dat_test[c(obs_id, occur_time, target)]
			train_pred$prob_LR = round(predict(lr_model_new, train_woe[imp_vars], type = "response"), 5)
			test_pred$prob_LR = round(predict(lr_model_new, test_woe[imp_vars], type = "response"), 5)
			save_data(train_pred, file_name = "lr_train_prob",
			  dir_path = LR_pred_dir_path, note = TRUE)
			save_data(test_pred, file_name = "lr_test_prob",
			  dir_path = LR_pred_dir_path, note = TRUE)
			#plot the model results

			if (score_card) {
				cat_line("-- Producing plots that characterize performance of scorecard", col = love_color("deep_green"))
				perf_tb = model_result_plot(train_pred = train_score, test_pred = test_score, target = target,
									score = "score_LR", gtitle = paste0(model_name, ".LR"), perf_dir_path = LR_perf_dir_path,
									save_data = TRUE, plot_show = plot_show, total = TRUE)
			} else {
				cat_line("-- Producing plots that characterize the performance of Logistic Regression", col = love_color("deep_green"))
				perf_tb = model_result_plot(train_pred = train_pred, test_pred = test_pred, target = target,
									score = "prob_LR", gtitle = paste0(model_name, ".LR"), perf_dir_path = LR_perf_dir_path,
									save_data = TRUE, plot_show = plot_show, total = TRUE)
			}

			key_index = model_key_index(perf_tb)
			params_key_index = data.frame(tree_control, bins_control, thresholds, key_index)
			save_data(params_key_index,
			  file_name = "LR.params",
			  dir_path = LR_perf_dir_path,
			  append = TRUE, note = TRUE)

			#transfer to pmml
			if (save_pmml) {

				cat_rule("Converting LR model to pmml", col = love_color("light_cyan"))
				if (!requireNamespace("pmml", quietly = TRUE) & !requireNamespace("XML", quietly = TRUE)) {
					cat_rule("Package `pmml`,`XML` needed for PMML transfering to work. Use 'require_packages(pmml,XML)' to install and load it.\n", col = love_color("deep_red"))
				} else {
					model_pmml = pmml::pmml(lr_model_new,
						   model.name = "Logistic_Regression_Model",
						   description = "Logistic Regression Model",
						   copyright = NULL,
						   transforms = NULL,
						   unknownValue = NULL,
						   weights = NULL)
					XML::saveXML(model_pmml, file = paste0(LR_model_dir_path, "/lr_model.pmml"))
				}
			}

			save(lr_model_new, file = paste0(LR_model_dir_path, "/lg_model.RData")) #save model
			model_new = list(lr_model = lr_model_new)
		}

		if (any(algorithm == "XGB")) {
			cat_rule("Training XGboost Model", col = "cyan")

			XGB_model_dir_path = paste(model_dir_path, "XGB", sep = "/")
			XGB_var_dir_path = paste(var_dir_path, "XGB", sep = "/")
			XGB_perf_dir_path = paste(perf_dir_path, "XGB", sep = "/")
			XGB_pred_dir_path = paste(pred_dir_path, "XGB", sep = "/")
			XGB_data_dir_path = paste(data_dir_path, "XGB", sep = "/")
			if (!dir.exists(XGB_model_dir_path)) dir.create(XGB_model_dir_path)
			if (!dir.exists(XGB_var_dir_path)) dir.create(XGB_var_dir_path)
			if (!dir.exists(XGB_perf_dir_path)) dir.create(XGB_perf_dir_path)
			if (!dir.exists(XGB_pred_dir_path)) dir.create(XGB_pred_dir_path)
			if (!dir.exists(XGB_data_dir_path)) dir.create(XGB_data_dir_path)

			#get parameters
			if (is.null(XGB.params)) {
				XGB.params = xgb_params()
			}
			nrounds = ifelse(!is.null(XGB.params[["nrounds"]]),
					   XGB.params[["nrounds"]], 1000)
			if (!is.null(XGB.params[["params"]])) {
				params = XGB.params[["params"]]
			} else {
				params = list(max_depth = 6,
					  eta = 0.01,
					  gamma = 0.01,
					  min_child_weight = 1,
					  subsample = 1,
					  colsample_bytree = 1,
					  scale_pos_weight = 1)
			}
			early_stopping_rounds = ifelse(!is.null(XGB.params[["early_stopping_rounds"]]),
									 XGB.params[["early_stopping_rounds"]], 100)
			method = ifelse(!is.null(XGB.params[["method"]]),
					  XGB.params[["method"]], 'random_search')
			iters = ifelse(!is.null(XGB.params[["iters"]]),
					 XGB.params[["iters"]], 10)
			f_eval = ifelse(!is.null(XGB.params[["f_eval"]]),
					XGB.params[["f_eval"]], 'auc')
			nthread = ifelse(!is.null(XGB.params[["nthread"]]),
					   XGB.params[["nthread"]], 2)
			nfold = ifelse(!is.null(XGB.params[["nfold"]]),
					 XGB.params[["nfold"]], 1)
			seed_number = ifelse(!is.null(seed), seed, 46)
			if (any(sapply(params, length) > 1)) {

				cat_rule("Searching optimal parameters of XGboost", col = love_color("light_cyan"))

				xgb.params.search = xgb_params_search(dat_train = dat_train, target = target, x_list = x_list, prop = prop,
											  method = method, iters = iters,
											  nrounds = nrounds, occur_time = occur_time,
											  early_stopping_rounds = early_stopping_rounds,
											  params = params,
											  f_eval = f_eval, nfold = nfold, nthread = nthread)



				params = xgb.params.search$params
				early_stopping_rounds = xgb.params.search$early_stopping_rounds

				seed_number = xgb.params.search$seed_number
			}
			xgb_list = xgb_data(dat_train = dat_train, target = target, dat_test = dat_test, x_list = x_list, prop = prop)
			dtrain = xgb_list$dtrain
			dtest = xgb_list$dtest
			watchlist = xgb_list$watchlist
			xgb_x_list = xgb_list$x_list
			x_train = xgb_list$x_train
			x_test = xgb_list$x_test
			y_train = xgb_list$y_train
			y_test = xgb_list$y_test
			save_data(x_train, file_name = "XGB.x_train", dir_path = XGB_data_dir_path, note = TRUE)
			save_data(x_test, file_name = "XGB.x_test", dir_path = XGB_data_dir_path, note = TRUE)
			save_data(y_train, file_name = "XGB.y_train", dir_path = XGB_data_dir_path, note = TRUE)
			save_data(y_test, file_name = "XGB.y_test", dir_path = XGB_data_dir_path, note = TRUE)
			# Train a model
			train_iter = train_xgb(seed_number = seed_number, dtrain = dtrain,
							 nthread = nthread,
							 nfold = nfold,
							 watchlist = watchlist,
							 nrounds = nrounds, f_eval = f_eval,
							 early_stopping_rounds = early_stopping_rounds,
							 verbose = 1,
							 params = params)
			max_iter = train_iter$max_iter
			max_iter_index = train_iter$max_iter_index
			xgb_model_new = train_iter$xgb_model
			if (length(unlist(xgb_model_new$feature_names)) > 0) {
				input_vars = data.frame(feature_order = 1:xgb_model_new$nfeatures,
							  feature_names = unlist(xgb_model_new$feature_names))
				save_data(input_vars, file_name = paste(model_name, "XGB_input_vars", sep = "."),
			  dir_path = XGB_model_dir_path, note = TRUE)
			}
			# feature importance1111
			dat_names = dimnames(x_train)[[2]]
			imp_XGB = xgb.importance(dat_names, model = xgb_model_new)
			imp_XGB = as.data.frame(imp_XGB)
			dt_imp_XGB = data.frame(Feature = imp_XGB[, "Feature"],
							  Importance = round(imp_XGB[, 'Gain'], 5),
							  stringsAsFactors = FALSE)

			save_data(dt_imp_XGB, file_name = "XGB_feature_importance",
			  dir_path = XGB_var_dir_path, note = TRUE)

			if (vars_plot & length(as.character(dt_imp_XGB[, 1])) > 1) {
				cat_line("-- Ploting partial dependence of input variables", col = love_color("deep_green"))
				pd_list = get_partial_dependence_plots(model = xgb_model_new, x_list = unlist(dt_imp_XGB[, 1]), parallel = parallel,
											   x_train = x_train, save_data = TRUE, plot_show = FALSE, dir_path = XGB_var_dir_path)
			}

			train_pred = dat_train[c(obs_id, occur_time, target)]
			test_pred = dat_test[c(obs_id, occur_time, target)]
			train_pred$prob_XGB = round(predict(xgb_model_new,
										  x_train, type = "response"), 5)
			test_pred$prob_XGB = round(predict(xgb_model_new,
										 x_test, type = "response"), 5)
			save_data(train_pred, file_name = "XGB.train_prob", dir_path = XGB_pred_dir_path, note = TRUE)
			save_data(test_pred, file_name = "XGB.test_prob", dir_path = XGB_pred_dir_path, note = TRUE)
			cat_line("-- Producing plots that characterize the performance of XGboost", col = love_color("deep_green"))
			perf_tb = model_result_plot(train_pred = train_pred, test_pred = test_pred, target = target,
								  score = "prob_XGB", gtitle = paste0(model_name, ".XGB"), perf_dir_path = XGB_perf_dir_path,
								  save_data = TRUE, plot_show = plot_show, total = TRUE)
			key_index = model_key_index(perf_tb)
			params_key_index = data.frame(c(params), key_index)
			save_data(params_key_index,
			  file_name = "XGB.params",
			  dir_path = XGB_perf_dir_path, append = TRUE, note = TRUE)

			if (save_pmml) {
				cat_rule("Converting  XGboost model to pmml", col = love_color("light_cyan"))
				if (!requireNamespace("pmml", quietly = TRUE) & !requireNamespace("XML", quietly = TRUE)) {
					cat_rule("Package `pmml`,`XML` needed for PMML transfering to work. Use 'require_packages(pmml,XML)' to install and load it.\n", col = love_color("deep_red"))
				} else {
					# save the tree information file
					xgb.dump(xgb_model_new, paste0(XGB_model_dir_path, "/xgb_model.dumped.trees"))
					# Export the model to PMML.
					xgb_model_pmml = pmml::pmml(xgb_model_new,
							  inputFeatureNames = unlist(xgb_model_new$feature_names),
							  outputLabelName = target,
							  outputCategories = c(0, 1),
							  xgbDumpFile = paste0(XGB_model_dir_path, "/xgb_model.dumped.trees"),
							  model.name = "xgboost_Model",
							  app.name = "XGB-PMML",
							  description = "Extreme Gradient Boosting Model",
							  copyright = NULL, transforms = NULL,
							  unknownValue = NULL,
							  parentInvalidValueTreatment = "returnInvalid",
							  childInvalidValueTreatment = "asIs"
		)
					XML::saveXML(xgb_model_pmml, file = paste0(XGB_model_dir_path, "/xgb_model.pmml"))
				}
			}
			xgb.save(xgb_model_new, paste0(XGB_model_dir_path, paste0("/", model_name, "_xgb.model")))
			save(xgb_model_new, file = paste0(XGB_model_dir_path, "/xgb_model.RData")) #save model
			model_new = append(model_new, list(xgb_model = xgb_model_new), 1)
			rm(x_train, y_train, xgb_model_new)
		}

		if (any(algorithm == "GBM")) {
			cat_rule("Training GBM model", col = "cyan")
			if (!requireNamespace("gbm", quietly = TRUE)) {
				cat_rule("Package `gbm` needed for training gbm. Use 'require_packages(gbm)' to install and load it.\n", col = love_color("deep_red"))
			} else {
				GBM_model_dir_path = paste(model_dir_path, "GBM", sep = "/")
				GBM_var_dir_path = paste(var_dir_path, "GBM", sep = "/")
				GBM_perf_dir_path = paste(perf_dir_path, "GBM", sep = "/")
				GBM_pred_dir_path = paste(pred_dir_path, "GBM", sep = "/")
				GBM_data_dir_path = paste(data_dir_path, "GBM", sep = "/")
				if (!dir.exists(GBM_model_dir_path)) dir.create(GBM_model_dir_path)
				if (!dir.exists(GBM_var_dir_path)) dir.create(GBM_var_dir_path)
				if (!dir.exists(GBM_perf_dir_path)) dir.create(GBM_perf_dir_path)
				if (!dir.exists(GBM_pred_dir_path)) dir.create(GBM_pred_dir_path)
				if (!dir.exists(GBM_data_dir_path)) dir.create(GBM_data_dir_path)
				if (is.null(GBM.params)) {
					GBM.params = gbm_params()
				}
				n.trees = ifelse(!is.null(GBM.params[["n.trees"]]),
					   GBM.params[["n.trees"]], 100)
				interaction.depth = ifelse(!is.null(GBM.params[["interaction.depth"]]),
								 GBM.params[["interaction.depth"]], 6)
				shrinkage = ifelse(!is.null(GBM.params[["shrinkage"]]),
						 GBM.params[["shrinkage"]], 0.01)
				n.minobsinnode = ifelse(!is.null(GBM.params[["n.minobsinnode"]]),
							  GBM.params[["n.minobsinnode"]], 30)
				bag.fraction = ifelse(!is.null(GBM.params[["bag.fraction"]]),
							GBM.params[["bag.fraction"]], 0.5)
				train.fraction = ifelse(!is.null(GBM.params[["train.fraction"]]),
							  GBM.params[["train.fraction"]], 1)
				cv.folds = ifelse(!is.null(GBM.params[["cv.folds"]]),
						GBM.params[["cv.folds"]], 5)

				char_x_list = get_names(dat = dat_train[, x_list],
							  types = c('character', 'factor'),
							  ex_cols = c(target, obs_id, occur_time, ex_cols),
							  get_ex = FALSE)
				if (length(char_x_list) > 0) {
					nr = nrow(dat_train)
					var_list = unique(c(target, obs_id, occur_time, x_list))
					dat_ts = rbind(dat_train[, var_list], dat_test[, var_list])
					dat_ts = one_hot_encoding(dat = dat_ts, cat_vars = char_x_list, na_act = FALSE)
					dat_train = dat_ts[1:nr,]
					dat_test = dat_ts[-c(1:nr),]
					x_list = get_x_list(x_list = NULL,
							dat_train = dat_train, dat_test = dat_test,
							ex_cols = c(target, obs_id, occur_time, ex_cols))
				}
				save_data(dat_train, file_name = "GBM.train", dir_path = GBM_data_dir_path, note = TRUE)
				save_data(dat_test, file_name = "GBM.test", dir_path = GBM_data_dir_path, note = TRUE)
				Formula = as.formula(paste(target, paste(x_list, collapse = ' + '), sep = ' ~ '))
				if (!is.null(seed)) set.seed(seed) else set.seed(46)
				gbm_model_new = gbm::gbm(
		Formula,
		data = dat_train,
		distribution = "bernoulli",
		n.trees = n.trees,
		shrinkage = shrinkage,
		interaction.depth = interaction.depth,
		bag.fraction = bag.fraction,
		train.fraction = train.fraction,
		n.minobsinnode = n.minobsinnode,
		cv.folds = cv.folds,
		class.stratify.cv = TRUE,
		keep.data = FALSE,
		verbose = TRUE,
		n.cores = cores_num
	  )
				# check performance using cross-validation.
				best.iter = gbm::gbm.perf(gbm_model_new, method = "cv", plot.it = FALSE, oobag.curve = FALSE)
				imp_gbm = as.data.frame(summary(gbm_model_new, best.iter, plot = FALSE))
				dt_imp_GBM = data.frame(Feature = imp_gbm[, "var"],
							  Importance = round(imp_gbm[, 'rel.inf'], 5),
							  stringsAsFactors = FALSE)
				imp_vars_gbm = subset(dt_imp_GBM, dt_imp_GBM$Importance > 0)[, "Feature"]
				save_data(dt_imp_GBM, file_name = "GBM.feature_importance",
			  dir_path = GBM_var_dir_path, note = TRUE)

				if (vars_plot & length(imp_vars_gbm) > 1) {
					cat_line("-- Ploting partial dependence of input variables", col = love_color("deep_green"))
					pd_list = get_partial_dependence_plots(model = gbm_model_new, x_list = imp_vars_gbm, parallel = parallel,
											   x_train = dat_train, n.trees = best.iter, save_data = TRUE, plot_show = FALSE, dir_path = GBM_var_dir_path)
				}
				train_pred = dat_train[c(obs_id, occur_time, target)]
				test_pred = dat_test[c(obs_id, occur_time, target)]
				train_pred$prob_GBM = round(predict(gbm_model_new, dat_train[, x_list],
										  best.iter, type = "response"), 5)
				test_pred$prob_GBM = round(predict(gbm_model_new, dat_test[, x_list],
										 best.iter, type = "response"), 5)
				save_data(train_pred, file_name = "GBM.train_prob",
			  dir_path = GBM_pred_dir_path, note = TRUE)
				save_data(test_pred, file_name = "GBM.test_prob",
			  dir_path = GBM_pred_dir_path, note = TRUE)

				cat_line("-- Producing plots that characterize the performance of GBM", col = love_color("deep_green"))
				perf_tb = model_result_plot(train_pred = train_pred, test_pred = test_pred, target = target,
								  score = "prob_GBM", gtitle = paste0(model_name, ".GBM"), perf_dir_path = GBM_perf_dir_path,
								  save_data = TRUE, plot_show = plot_show, total = TRUE)

				key_index = model_key_index(perf_tb)
				params_key_index = data.frame(c(GBM.params), key_index)
				save_data(params_key_index, file_name = "GBM.params",
			  dir_path = GBM_perf_dir_path, append = TRUE, note = TRUE)
				if (save_pmml) {
					cat_rule("Converting  GBM model to pmml", col = love_color("light_cyan"))
					if (!requireNamespace("pmml", quietly = TRUE) & !requireNamespace("XML", quietly = TRUE)) {
						cat_rule("Package `pmml`,`XML` needed for PMML transfering to work. Use 'require_packages(pmml,XML)' to install and load it.\n", col = love_color("deep_red"))
					} else {
						gbm_model_pmml = pmml::pmml(gbm_model_new,
							  model.name = "GBM_Model",
							  app.name = "GBM-PMML",
							  description = "Gradient Boosting Decision Tree Model",
							  copyright = NULL,
							  transforms = NULL,
							  unknownValue = NULL,
							  parentInvalidValueTreatment = "returnInvalid",
							  childInvalidValueTreatment = "asIs"
					 )
						XML::saveXML(gbm_model_pmml, file = paste0(GBM_model_dir_path, "/gbm_model.pmml"))
					}
				}
				#save model
				save(gbm_model_new, file = paste0(GBM_model_dir_path, "/gbm_model.RData"))
				model_new = append(model_new, list(gbm_model = gbm_model_new), 1)
				rm(gbm_model_new)
			}

		}
		if (any(algorithm == "RF")) {
			cat_rule("Training RandomForest Model", col = "cyan")

			if (!requireNamespace("randomForest", quietly = TRUE)) {
				cat_rule("Package `randomForest` needed for training randomForest. Use 'require_packages(randomForest)' to install and load it, .\n", col = love_color("deep_red"))
			} else {
				RF_model_dir_path = paste(model_dir_path, "RF", sep = "/")
				RF_var_dir_path = paste(var_dir_path, "RF", sep = "/")
				RF_perf_dir_path = paste(perf_dir_path, "RF", sep = "/")
				RF_pred_dir_path = paste(pred_dir_path, "RF", sep = "/")
				RF_data_dir_path = paste(data_dir_path, "RF", sep = "/")
				if (!dir.exists(RF_model_dir_path)) dir.create(RF_model_dir_path)
				if (!dir.exists(RF_var_dir_path)) dir.create(RF_var_dir_path)
				if (!dir.exists(RF_perf_dir_path)) dir.create(RF_perf_dir_path)
				if (!dir.exists(RF_pred_dir_path)) dir.create(RF_pred_dir_path)
				if (!dir.exists(RF_data_dir_path)) dir.create(RF_data_dir_path)
				`%DO%` = if (parallel) `%dopar%` else `%do%`
				if (is.null(RF.params)) {
					RF.params = rf_params()
				}
				ntree = ifelse(!is.null(RF.params[["ntree"]]),
					 RF.params[["ntree"]], 100)
				nodesize = ifelse(!is.null(RF.params[["nodesize"]]),
						RF.params[["nodesize"]], 30)
				samp_rate = ifelse(!is.null(RF.params[["samp_rate"]]),
						 RF.params[["samp_rate"]], 0.1)
				tune_rf = ifelse(!is.null(RF.params[["tune_rf"]]),
					   RF.params[["tune_rf"]], TRUE)

				tmp = as.vector(table(dat_train[, target]));
				num_clases = length(tmp);
				min_size = tmp[order(tmp, decreasing = FALSE)[1]] * samp_rate
				vector_for_sampsize = rep(min_size, num_clases);

				char_x_list = get_names(dat = dat_train[, x_list],
							  types = c('character', 'factor'),
							  ex_cols = c(target, obs_id, occur_time, ex_cols),
							  get_ex = FALSE)
				if (length(char_x_list) > 0) {
					nr = nrow(dat_train)
					var_list = unique(c(target, obs_id, occur_time, x_list))
					dat_ts = rbind(dat_train[, var_list], dat_test[, var_list])
					dat_ts = one_hot_encoding(dat = dat_ts, cat_vars = char_x_list, na_act = FALSE)
					dat_train = dat_ts[1:nr,]
					dat_test = dat_ts[-c(1:nr),]
					x_list = get_names(dat = dat_train,
						   types = c('numeric', 'integer', 'double'),
						   ex_cols = c(target, obs_id, occur_time, ex_cols),
						   get_ex = FALSE)
				}

				dat_train[, target] = as.factor(as.character(dat_train[, target]))
				if (!is.null(seed)) set.seed(seed) else set.seed(46)
				if (tune_rf) {
					#Tune Random Forest Model
					tRF = foreach(n_tree = rep(round(10 / cores_num), cores_num),
					  .combine = randomForest::combine,
					  .packages = "randomForest") %DO% {
						randomForest::tuneRF(x = as.matrix(dat_train[, x_list]),
							   y = dat_train[, target],
							   stepFactor = 0.5,
							   plot = FALSE,
							   ntreeTry = n_tree,
							   trace = TRUE,
							   improve = 0.05,
							   doBest = TRUE,
							   sampsize = vector_for_sampsize,
							   nodesize = nodesize,
							   importance = TRUE,
							   proximity = FALSE)
  					}

					n_tree = tRF$ntree
					mtry = tRF$mtry
					imp_rf = as.data.frame(randomForest::importance(tRF))
					vars_rf = rownames(imp_rf[which(imp_rf$MeanDecreaseAccuracy > 0),])
				} else {
					n_tree = ntree
					mtry = floor(sqrt(length(x_list)))
					vars_rf = x_list
				}

				#Fit the Random Forest Model After Tuning
				Formula = as.formula(paste(target, paste(vars_rf, collapse = ' + '), sep = ' ~ '))
				if (!is.null(seed)) set.seed(seed) else set.seed(46)
				rf_model_new = foreach(n_tree = rep(round(ntree / cores_num), cores_num),
							  .combine = randomForest::combine,
							  .packages = "randomForest") %DO% {
								randomForest::randomForest(Formula,
											 data = dat_train,
											 sampsize = vector_for_sampsize,
											 ntree = n_tree,
											 nodesize = nodesize,
											 importance = TRUE,
											 proximity = FALSE,
											 mtry = mtry)
			  				}
				imp_rf = randomForest::importance(rf_model_new)
				dt_imp_RF = data.frame(Feature = row.names(imp_rf),
							 Importance = round(imp_rf[, 'MeanDecreaseAccuracy'], 5),
							 stringsAsFactors = FALSE)
				imp_vars_rf = subset(dt_imp_RF, dt_imp_RF$Importance > 0)[, "Feature"]

				save_data(dt_imp_RF, file_name = "RF.feature_importance",
			  dir_path = RF_var_dir_path, note = TRUE)
				dat_train[, target] = as.numeric(as.character(dat_train[, target]))


				if (vars_plot & length(imp_vars_rf) > 1) {
					cat_line("-- Ploting partial dependence of input variables", col = love_color("deep_green"))
					pd_list = get_partial_dependence_plots(model = rf_model_new, x_list = imp_vars_rf, parallel = parallel,
											   x_train = dat_train, save_data = TRUE, plot_show = FALSE, dir_path = RF_var_dir_path)
				}

				train_pred = dat_train[c(obs_id, occur_time, target)]
				test_pred = dat_test[c(obs_id, occur_time, target)]
				train_pred$prob_RF = round(predict(rf_model_new,
										 dat_train[, vars_rf],
										 type = c("prob"))[, 2], 5)
				test_pred$prob_RF = round(predict(rf_model_new,
										dat_test[, vars_rf],
										type = c("prob"))[, 2], 5)
				save_data(train_pred, file_name = "RF.train_prob", dir_path = RF_pred_dir_path, note = TRUE)
				save_data(test_pred, file_name = "RF.test_prob", dir_path = RF_pred_dir_path, note = TRUE)
				cat_line("-- Producing plots that characterize the performance of RandomForest", col = love_color("deep_green"))
				perf_tb = model_result_plot(train_pred = train_pred, test_pred = test_pred, target = target,
								  score = "prob_RF", gtitle = paste0(model_name, ".RF"), perf_dir_path = RF_perf_dir_path,
								  save_data = TRUE, plot_show = plot_show, total = TRUE)
				key_index = model_key_index(perf_tb)
				params_key_index = data.frame(c(RF.params), key_index)
				save_data(params_key_index, file_name = "RF.params",
			  dir_path = RF_perf_dir_path,
			  append = TRUE, note = TRUE)

				if (save_pmml) {

					cat_rule("Converting  RandomForest model to pmml", col = love_color("light_cyan"))
					if (!requireNamespace("pmml", quietly = TRUE) & !requireNamespace("XML", quietly = TRUE)) {
						cat_rule("Package `pmml`,`XML` needed for PMML transfering to work. Use 'require_packages(pmml,XML)' to install and load it.\n", col = love_color("deep_red"))
					} else {
						rf_model_pmml = pmml::pmml(tRF,
							 model.name = "randomForest_Model",
							 app.name = "RF-PMML",
							 description = "Random Forest Tree Model",
							 copyright = NULL,
							 transforms = NULL,
							 unknownValue = NULL,
							 parentInvalidValueTreatment = "returnInvalid",
							 childInvalidValueTreatment = "asIs")
						XML::saveXML(rf_model_pmml, file = paste0(RF_model_dir_path, "/rf_model.pmml"))
					}
				}
				save(rf_model_new, file = paste0(RF_model_dir_path, "/rf_model.RData")) #save model
				model_new = append(model_new, list(rf_model = rf_model_new), 1)
				rm(rf_model_new)
			}
		}
	} else {
		stop(paste("target must be binomial.\n"))
	}
	return(model_new)
	options(opt) # reset
}




#'Logistic Regression & Scorecard Parameters
#'
#' \code{lr_params} is the list of parameters to train a LR model or Scorecard using in  \code{\link{training_model}}.
#' \code{lr_params_search} is for searching the optimal parameters of logistic regression,if any parameters of params in \code{\link{lr_params}} is more than one.
#' @param tree_control the list of parameters to control cutting initial breaks by decision tree. See details at: \code{\link{get_tree_breaks}}
#' @param bins_control  the list of parameters to control merging initial breaks. See details at: \code{\link{select_best_breaks}},\code{\link{select_best_class}}
#' @param best_lambda  Metheds of best lanmbda stardards using to filter variables by LASSO. There are 3 methods: ("lambda.auc", "lambda.ks", "lambda.sim_sign") . Default is  "lambda.auc".
#' @param obsweight An optional vector of 'prior weights' to be used in the fitting process. Should be NULL or a numeric vector. If you oversample or cluster diffrent datasets to training the LR model, you need to set this parameter to ensure that the probability of logistic regression output is the same as that before oversampling or segmentation. e.g.:There are 10,000 0 obs and 500 1 obs before oversampling or under-sampling, 5,000 0 obs and 3,000 1 obs after oversampling. Then this parameter should be set to c(10000/5000, 500/3000). Default is NULL..
#' @param forced_in Names of forced input variables. Default is NULL.
#' @param sp_values  Vaules will be in separate bins.e.g. list(-1, "missing")  means that -1 & missing as special values.Default is NULL.
#' @param lasso  Logical, if TRUE, variables filtering by LASSO. Default is TRUE.
#' @param step_wise  Logical, stepwise method. Default is TRUE.
#' @param score_card  Logical, transfer woe to a standard scorecard. If TRUE, Output scorecard, and score prediction, otherwise output probability. Default is TRUE.
#' @param thresholds Thresholds for selecting variables.
#' \itemize{
#'   \item \code{cor_p} The maximum threshold of correlation. Default: 0.8.
#'   \item \code{iv_i} The minimum threshold of IV. 0.01 to 0.1 usually work. Default: 0.02
#'   \item \code{psi_i} The maximum threshold of PSI. 0.1 to 0.3 usually work. Default: 0.1.
#'   \item \code{cos_i} cos_similarity of posive rate of train and test. 0.7 to 0.9 usually work.Default: 0.5.
#' }
#' @param method Method of searching optimal parameters. "random_search","grid_search","local_search" are available.
#' @param iters Number of iterations of "random_search" optimal parameters.
#' @param f_eval 	Custimized evaluation function, "ks" & "auc" are available.
#' @param ... Other parameters
#' @return A list of parameters.
#' @seealso  \code{\link{training_model}}, \code{\link{xgb_params}}, \code{\link{gbm_params}}, \code{\link{rf_params}}
#' @export

lr_params = function(tree_control = list(p = 0.02, cp = 0.00000001, xval = 5, maxdepth = 10),
                     bins_control = list(bins_num = 10, bins_pct = 0.05, b_chi = 0.02,
                                         b_odds = 0.1, b_psi = 0.03, b_or = 0.15,
                                         mono = 0.2, odds_psi = 0.15, kc = 1),
                     f_eval = 'ks',best_lambda = "lambda.ks",method = "random_search",iters = 10,
                     lasso = TRUE,step_wise = TRUE, score_card = TRUE, sp_values = NULL,forced_in = NULL, obsweight = c(1, 1),
                     thresholds = list(cor_p = 0.8,iv_i = 0.02,
                                       psi_i = 0.1,
                                       cos_i = 0.5), ...) {
  structure(list(tree_control = tree_control, bins_control = bins_control,
                 f_eval = f_eval,best_lambda = best_lambda, method = method,iters= iters,
                 sp_values = sp_values,
                 forced_in = forced_in, obsweight = obsweight, lasso = lasso,
                 step_wise = step_wise, score_card = score_card,
                 thresholds = thresholds))
}


#' @rdname lr_params
#' @param dat_train  data.frame of train data. Default is NULL.
#' @param dat_test  data.frame of test data. Default is NULL.
#' @param target name of target variable.
#' @param x_list names of independent variables. Default is NULL.
#' @param prop Percentage of train-data after the partition. Default: 0.7.
#' @param occur_time The name of the variable that represents the time at which each observation takes place.Default is NULL.
#' @export

lr_params_search = function( method = "random_search",dat_train, target,
                             dat_test = NULL,occur_time= NULL,
                             x_list = NULL , prop = 0.7,iters = 10,
                             tree_control = list(p = 0.02,
                                                 cp = 0,
                                                 xval = 1,
                                                 maxdepth = 10),
                             bins_control = list(bins_num = 10,
                                                 bins_pct = 0.02,
                                                 b_chi = 0.02,
                                                 b_odds = 0.1,
                                                 b_psi = 0.05,
                                                 b_or = 0.1,
                                                 mono = 0.1,
                                                 odds_psi = 0.03,
                                                 kc = 1),
                             thresholds = list(cor_p = 0.8,
                                               iv_i = 0.02,
                                               psi_i = 0.1,
                                               cos_i = 0.6),
                             step_wise = FALSE,
                             lasso = FALSE,
                             f_eval = 'ks') {


  if (length(method) > 1) stop("only one method can be provided to select best parameters.\n")
  if(is.null(dat_test)) {
    train_test = train_test_split(dat = dat_train,split_type = 'OOT',prop = prop,
                                  occur_time = occur_time)
    dat_train = train_test$train
    dat_test = train_test$test
  }
  if(f_eval == 'ks'){
    best_lambda ='lambda.ks'
  }else{
    best_lambda ='lambda.auc'
  }
  p = if(!is.null(tree_control[["p"]]))tree_control[["p"]] else 0.05
  cp = if(!is.null(tree_control[["cp"]]))tree_control[["cp"]] else 0.0000001
  xval = if(!is.null(tree_control[["xval"]]))tree_control[["xval"]] else 5
  maxdepth =if(!is.null(tree_control[["maxdepth"]]))tree_control[["maxdepth"]] else 10
  bins_num = if(!is.null(bins_control[["bins_num"]]))bins_control[["bins_num"]] else 10
  bins_pct = if(!is.null(bins_control[["bins_pct"]]))bins_control[["bins_pct"]] else 0.02
  b_chi = if(!is.null(bins_control[["b_chi"]]))bins_control[["b_chi"]] else 0.02
  b_odds = if(!is.null(bins_control[["b_odds"]]))bins_control[["b_odds"]] else 0.05
  b_psi = if(!is.null(bins_control[["b_psi"]]))bins_control[["b_psi"]] else 0.05
  b_or = if(!is.null(bins_control[["b_or"]]))bins_control[["b_or"]] else 0.15
  mono = if(!is.null(bins_control[["mono"]]))bins_control[["mono"]] else 0.3
  odds_psi = if(!is.null(bins_control[["odds_psi"]]))bins_control[["odds_psi"]] else 0.2
  kc = if(!is.null(bins_control[["kc"]]))bins_control[["kc"]] else 1
  cor_p = if(!is.null(thresholds[["cor_p"]]))thresholds[["cor_p"]] else 0.7
  iv_i= if(!is.null(thresholds[["iv_i"]]))thresholds[["iv_i"]] else 0.01
  psi_i = if(!is.null(thresholds[["psi_i"]]))thresholds[["psi_i"]] else 0.1
  cos_i = if(!is.null(thresholds[["cos_i"]]))thresholds[["cos_i"]] else 0.5
  best_x_list=c()
  best_tree_control = list()
  best_bins_control = list()
  best_thresholds = list()
  best_iter = c(0,0)
  best_seed = 1234
  best_lr_model = NULL
  iter_no = 1
  if (any(sapply(tree_control,length) >1 ,sapply(bins_control,length)>1 ,sapply(thresholds,length)>1 ) &
      any(is.element(method, c("random_search", "grid_search", "local_search")))){
    if (method == "random_search"){
      for (iter in 1:iters) {
        tree_control= list(
          p = ifelse(length(p) > 1, sample(p, 1), p),
          cp =  ifelse(length(cp) > 1, sample(cp,  1), cp),
          xval =  ifelse(length(xval) > 1, sample(xval, 1), xval),
          maxdepth = ifelse(length(maxdepth) > 1, sample(maxdepth,  1), maxdepth)

        )

        bins_control = list(bins_num = ifelse(length(bins_num) > 1, sample(bins_num, 1), bins_num),
                            bins_pct = ifelse(length(bins_pct) > 1, sample(bins_pct,  1), bins_pct),
                            b_chi = ifelse(length(b_chi) > 1, sample(b_chi,1), b_chi),
                            b_odds = ifelse(length(b_odds) > 1, sample(b_odds,1), b_odds),
                            b_psi = ifelse(length(b_psi) > 1, sample(b_psi, 1), b_psi),
                            b_or =ifelse(length(b_or) > 1, sample(b_or, 1), b_or),
                            mono = ifelse(length(mono) > 1, sample(mono, 1), mono),
                            odds_psi = ifelse(length(odds_psi) > 1, sample(odds_psi, 1), odds_psi),
                            kc = ifelse(length(kc) > 1, sample(kc,  1), kc))

        thresholds = list(cor_p = ifelse(length(cor_p) > 1, sample(cor_p, 1), cor_p),
                          iv_i =ifelse(length(iv_i) > 1, sample(iv_i,  1), iv_i),
                          psi_i = ifelse(length(psi_i) > 1, sample(psi_i, 1), psi_i),
                          cos_i = ifelse(length(cos_i) > 1, sample(cos_i, 1), cos_i))
        seed_number = sample.int(10000, 1)[[1]]
        train_iter = train_lr(dat_train = dat_train,dat_test = dat_test, target = target,
                              x_list= x_list, occur_time = occur_time,prop = prop,
                              tree_control = tree_control,
                              bins_control = bins_control,
                              thresholds = thresholds,
                              lasso = lasso,step_wise = step_wise,
                              best_lambda = best_lambda,
                              seed = seed_number)
        max_iter = train_iter$max_iter
        lr_model = train_iter$lr_model
        rm(train_iter)

        cat_line(paste(paste0("[", iter, "] "),
                       paste(names(max_iter), unlist(max_iter), collapse  = "  " ,sep  = ":")),col = love_color("deep_green"))

        cat_bullet( c(
          paste(
            "tree_control:{", paste(names(tree_control),tree_control,collapse = ", ", sep = ":"),"}"),
          paste(
            "bins_control:{", paste(names(bins_control),bins_control,collapse = ", ", sep = ":"),"}"),
          paste(
            "thresholds:{", paste(names(thresholds),thresholds,collapse = ", ", sep = ":"),"}")),col = "darkgrey")

        if (max_iter[[2]] > best_iter[[2]]) {
          best_iter = max_iter
          iter_no = iter
          best_lr_model = lr_model
          best_tree_control = tree_control
          best_bins_control = bins_control
          best_thresholds = thresholds
          best_seed = seed_number
        }
      }

      cat_rule(paste("[best iter]"),col = love_color("deep_purple"))
      cat_line(paste(paste0("[", iter_no, "] "),
                     paste(names(best_iter), unlist(best_iter), collapse  = "\t" ,sep  = ":")),col = love_color("deep_green"))

      cat_bullet( c(
        paste(
          "tree_control:{", paste(names(best_tree_control),best_tree_control,collapse = ", ", sep = ":"),"}"),
        paste(
          "bins_control:{", paste(names(best_bins_control),best_bins_control,collapse = ", ", sep = ":"),"}"),
        paste(
          "thresholds:{", paste(names(best_thresholds),best_thresholds,collapse = ", ", sep = ":"),"}")),col = "darkgrey")
    } else {
      if (method == "grid_search") {
        iter = 1
        for (p_i in p) {
          for (cp_i in cp) {
            for (xval_i in xval) {
              for (maxdepth_i in maxdepth) {
                for(bins_num_i in bins_num){
                  for (bins_pct_i in bins_pct) {
                    for (b_chi_i in b_chi) {
                      for (b_odds_i in b_odds) {
                        for (b_psi_i in b_psi) {
                          for (b_or_i in b_or) {
                            for (mono_i in mono) {
                              for (odds_psi_i in odds_psi) {
                                for (kc_i in kc) {
                                  for (cor_p_i in cor_p) {
                                    for (iv_i_i in iv_i) {
                                      for (psi_i_i in psi_i) {
                                        for (cos_i_i in cos_i) {
                                          tree_control= list(
                                            p = p_i,
                                            cp = cp_i,
                                            xval = xval_i,
                                            maxdepth = maxdepth_i
                                          )
                                          bins_control = list(bins_num = bins_num_i,
                                                              bins_pct = bins_pct_i,
                                                              b_chi =b_chi_i,
                                                              b_odds = b_odds_i,
                                                              b_psi =b_psi_i,
                                                              b_or = b_or_i,
                                                              mono = mono_i,
                                                              odds_psi = odds_psi_i,
                                                              kc = kc_i)

                                          thresholds = list(cor_p =cor_p_i ,
                                                            iv_i = iv_i_i,
                                                            psi_i = psi_i_i,
                                                            cos_i = cos_i_i)
                                          seed_number = sample.int(10000, 1)[[1]]
                                          train_iter = train_lr(dat_train = dat_train,dat_test = dat_test, target = target,
                                                                x_list= x_list, occur_time = occur_time,prop = prop,
                                                                tree_control = tree_control,
                                                                bins_control = bins_control,
                                                                thresholds = thresholds,
                                                                lasso = lasso,step_wise = step_wise,
                                                                best_lambda = best_lambda,
                                                                seed = seed_number)
                                          max_iter = train_iter$max_iter
                                          lr_model = train_iter$lr_model
                                          rm(train_iter)

                                          cat_line(paste(paste0("[", iter, "] "),
                                                         paste(names(max_iter), unlist(max_iter), collapse  = "  " ,sep  = ":")),col = love_color("deep_green"))

                                          cat_bullet( c(
                                            paste(
                                              "tree_control:{", paste(names(tree_control),tree_control,collapse = ", ", sep = ":"),"}"),
                                            paste(
                                              "bins_control:{", paste(names(bins_control),bins_control,collapse = ", ", sep = ":"),"}"),
                                            paste(
                                              "thresholds:{", paste(names(thresholds),thresholds,collapse = ", ", sep = ":"),"}")),col = "darkgrey")

                                          if (max_iter[[2]] > best_iter[[2]]) {
                                            best_iter = max_iter
                                            iter_no = iter
                                            best_lr_model = lr_model
                                            best_tree_control = tree_control
                                            best_bins_control = bins_control
                                            best_thresholds = thresholds
                                            best_seed = seed_number
                                          }
                                          iter = iter + 1
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
        cat_rule(paste("[best iter]"),col = love_color("deep_purple"))
        cat_line(paste(paste0("[", iter_no, "] "),
                       paste(names(best_iter), unlist(best_iter), collapse  = "\t" ,sep  = ":")),col = love_color("deep_green"))

        cat_bullet( c(
          paste(
            "tree_control:{", paste(names(best_tree_control),best_tree_control,collapse = ", ", sep = ":"),"}"),
          paste(
            "bins_control:{", paste(names(best_bins_control),best_bins_control,collapse = ", ", sep = ":"),"}"),
          paste(
            "thresholds:{", paste(names(best_thresholds),best_thresholds,collapse = ", ", sep = ":"),"}")),col = "darkgrey")
      } else {
        if (method == "local_search") {
          iter = 1
          if(any(sapply(tree_control,length)>1)){
            best_iter = c(0,0)
            for (p_i in p) {
              for (cp_i in cp) {
                for (xval_i in xval) {
                  for (maxdepth_i in maxdepth) {
                    tree_control= list(
                      p = p_i,
                      cp = cp_i,
                      xval = xval_i,
                      maxdepth = maxdepth_i
                    )
                    bins_control = list(bins_num = bins_num[1], bins_pct = bins_pct[1],
                                        b_chi =b_chi[1],b_odds = b_odds[1],
                                        b_psi = b_psi[1], b_or = b_or[1],
                                        mono = mono[1], odds_psi = odds_psi[1],
                                        kc = kc[1])
                    thresholds =list(cor_p =cor_p[1] ,
                                     iv_i = iv_i[1],
                                     psi_i = psi_i[1],
                                     cos_i = cos_i[1])
                    seed_number = sample.int(10000, 1)[[1]]
                    train_iter = train_lr(dat_train = dat_train,dat_test = dat_test, target = target,
                                          x_list= x_list, occur_time = occur_time,prop = prop,
                                          tree_control = tree_control,
                                          bins_control = bins_control,
                                          thresholds = thresholds,
                                          lasso = lasso,step_wise = step_wise,
                                          best_lambda = best_lambda,
                                          seed = seed_number)
                    max_iter = train_iter$max_iter
                    lr_model = train_iter$lr_model
                    rm(train_iter)

                    cat_line(paste(paste0("[", iter, "] "),
                                   paste(names(max_iter), unlist(max_iter), collapse  = "  " ,sep  = ":")),col = love_color("deep_green"))

                    cat_bullet( c(
                      paste(
                        "tree_control:{", paste(names(tree_control),tree_control,collapse = ", ", sep = ":"),"}"),
                      paste(
                        "bins_control:{", paste(names(bins_control),bins_control,collapse = ", ", sep = ":"),"}"),
                      paste(
                        "thresholds:{", paste(names(thresholds),thresholds,collapse = ", ", sep = ":"),"}")),col = "darkgrey")

                    if (max_iter[[2]] > best_iter[[2]]) {
                      best_iter = max_iter
                      iter_no = iter
                      best_lr_model = lr_model
                      best_tree_control = tree_control
                      best_bins_control = bins_control
                      best_thresholds = thresholds
                      best_seed = seed_number
                    }
                    iter = iter + 1
                  }
                }
              }
            }
          }else{

            best_tree_control = tree_control

          }
          if(any(sapply(bins_control,length)>1)){
            best_iter = c(0,0)
            for(bins_num_i in bins_num){
              for (bins_pct_i in bins_pct) {
                for (b_chi_i in b_chi) {
                  for (b_odds_i in b_odds) {
                    for (b_psi_i in b_psi) {
                      for (b_or_i in b_or) {
                        for (mono_i in mono) {
                          for (odds_psi_i in odds_psi) {
                            for (kc_i in kc) {
                              tree_control= best_tree_control
                              bins_control = list(bins_num = bins_num_i,
                                                  bins_pct = bins_pct_i,
                                                  b_chi =b_chi_i,
                                                  b_odds = b_odds_i,
                                                  b_psi =b_psi_i,
                                                  b_or = b_or_i,
                                                  mono = mono_i,
                                                  odds_psi = odds_psi_i,
                                                  kc = kc_i)

                              thresholds = list(cor_p = cor_p[1] ,
                                                iv_i = iv_i[1],
                                                psi_i = psi_i[1],
                                                cos_i =psi_i[1])
                               seed_number = sample.int(10000, 1)[[1]]
                              train_iter = train_lr(dat_train = dat_train,dat_test = dat_test, target = target,
                                                    x_list= x_list, occur_time = occur_time,prop = prop,
                                                    tree_control = tree_control,
                                                    bins_control = bins_control,
                                                    thresholds = thresholds,
                                                    lasso = lasso,step_wise = step_wise,
                                                    best_lambda = best_lambda,
                                                    seed = seed_number)
                              max_iter = train_iter$max_iter
                              lr_model = train_iter$lr_model
                              rm(train_iter)

                              cat_line(paste(paste0("[", iter, "] "),
                                             paste(names(max_iter), unlist(max_iter), collapse  = "  " ,sep  = ":")),col = love_color("deep_green"))

                              cat_bullet( c(
                                paste(
                                  "tree_control:{", paste(names(tree_control),tree_control,collapse = ", ", sep = ":"),"}"),
                                paste(
                                  "bins_control:{", paste(names(bins_control),bins_control,collapse = ", ", sep = ":"),"}"),
                                paste(
                                  "thresholds:{", paste(names(thresholds),thresholds,collapse = ", ", sep = ":"),"}")),col = "darkgrey")

                              if (max_iter[[2]] > best_iter[[2]]) {
                                best_iter = max_iter
                                iter_no = iter
                                best_lr_model = lr_model
                                best_tree_control = tree_control
                                best_bins_control = bins_control
                                best_thresholds = thresholds
                                best_seed = seed_number
                              }
                              iter = iter + 1

                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }else{
            best_bins_control = bins_control
          }
          if(any(sapply(thresholds,length)>1)){
            best_iter = c(0,0)
            for (cor_p_i in cor_p) {
              for (iv_i_i in iv_i) {
                for (psi_i_i in psi_i) {
                  for (cos_i_i in cos_i) {
                    tree_control= best_tree_control
                    bins_control = best_bins_control
                    thresholds = list(cor_p =cor_p_i ,
                                      iv_i = iv_i_i,
                                      psi_i = psi_i_i,
                                      cos_i = cos_i_i)

                    train_iter = train_lr(dat_train = dat_train,dat_test = dat_test, target = target,
                                          x_list= x_list, occur_time = occur_time,prop = prop,
                                          tree_control = tree_control,
                                          bins_control = bins_control,
                                          thresholds = thresholds,
                                          lasso = lasso,step_wise = step_wise,
                                          best_lambda = best_lambda,
                                          seed = seed_number)
                    max_iter = train_iter$max_iter
                    lr_model = train_iter$lr_model
                    rm(train_iter)

                    cat_line(paste(paste0("[", iter, "] "),
                                   paste(names(max_iter), unlist(max_iter), collapse  = "  " ,sep  = ":")),col = love_color("deep_green"))

                    cat_bullet( c(
                      paste(
                        "tree_control:{", paste(names(tree_control),tree_control,collapse = ", ", sep = ":"),"}"),
                      paste(
                        "bins_control:{", paste(names(bins_control),bins_control,collapse = ", ", sep = ":"),"}"),
                      paste(
                        "thresholds:{", paste(names(thresholds),thresholds,collapse = ", ", sep = ":"),"}")),col = "darkgrey")

                    if (max_iter[[2]] > best_iter[[2]]) {
                      best_iter = max_iter
                      iter_no = iter
                      best_lr_model = lr_model
                      best_tree_control = tree_control
                      best_bins_control = bins_control
                      best_thresholds = thresholds
                      best_seed = seed_number
                    }
                    iter = iter + 1
                  }
                }
              }
            }
          }

          cat_rule(paste("[best iter]"),col = love_color("deep_purple"))
          cat_line(paste(paste0("[", iter_no, "] "),
                         paste(names(best_iter), unlist(best_iter), collapse  = "\t" ,sep  = ":")),col = love_color("deep_green"))

          cat_bullet( c(
            paste(
              "tree_control:{", paste(names(best_tree_control),best_tree_control,collapse = ", ", sep = ":"),"}"),
            paste(
              "bins_control:{", paste(names(best_bins_control),best_bins_control,collapse = ", ", sep = ":"),"}"),
            paste(
              "thresholds:{", paste(names(best_thresholds),best_thresholds,collapse = ", ", sep = ":"),"}")),col = "darkgrey")




        }
      }
    }

  }else {
    iter = 1
    train_iter = train_lr(dat_train = dat_train,dat_test = dat_test, target = target,
                          x_list= x_list, occur_time = occur_time,prop = prop,
                          tree_control = tree_control,
                          bins_control = bins_control,
                          thresholds = thresholds,
                          lasso = lasso,step_wise = step_wise,
                          best_lambda = best_lambda,
                          seed = seed_number)
    max_iter = train_iter$max_iter
    lr_model = train_iter$lr_model
    rm(train_iter)

    cat_line(paste(paste0("[", iter, "] "),
                   paste(names(max_iter), unlist(max_iter), collapse  = "  " ,sep  = ":")),col = love_color("deep_green"))

    cat_bullet( c(
      paste(
        "tree_control:{", paste(names(tree_control),tree_control,collapse = ", ", sep = ":"),"}"),
      paste(
        "bins_control:{", paste(names(bins_control),bins_control,collapse = ", ", sep = ":"),"}"),
      paste(
        "thresholds:{", paste(names(thresholds),thresholds,collapse = ", ", sep = ":"),"}")),col = "darkgrey")

    if (max_iter[[2]] > best_iter[[2]]) {
      best_iter = max_iter
      iter_no = iter
      best_lr_model = lr_model
      best_tree_control = tree_control
      best_bins_control = bins_control
      best_thresholds = thresholds
      best_seed = seed_number
    }
    cat_rule(paste("[best iter]"),col = love_color("deep_purple"))
    cat_line(paste(paste0("[", iter_no, "] "),
                   paste(names(best_iter), unlist(best_iter), collapse  = "\t" ,sep  = ":")),col = love_color("deep_green"))

    cat_bullet( c(
      paste(
        "tree_control:{", paste(names(best_tree_control),best_tree_control,collapse = ", ", sep = ":"),"}"),
      paste(
        "bins_control:{", paste(names(best_bins_control),best_bins_control,collapse = ", ", sep = ":"),"}"),
      paste(
        "thresholds:{", paste(names(best_thresholds),best_thresholds,collapse = ", ", sep = ":"),"}")),col = "darkgrey")
  }

  dt_imp_LR = get_logistic_coef(lg_model = best_lr_model,save_data = FALSE)
  lr_vars = dt_imp_LR[-1, "Feature"]


  structure(list( iter = best_iter,
                  x_list = lr_vars,
                  tree_control = best_tree_control,
                  bins_control = best_bins_control,
                  thresholds = best_thresholds,
                  best_seed = best_seed))
}

#' Trainig LR model
#'
#'
#' \code{train_lr} is for training the logistic regression model using in \code{\link{training_model}}.
#' @param dat_train  data.frame of train data. Default is NULL.
#' @param dat_test  data.frame of test data. Default is NULL.
#' @param target name of target variable.
#' @param x_list names of independent variables. Default is NULL.
#' @param prop Percentage of train-data after the partition. Default: 0.7.
#' @param occur_time The name of the variable that represents the time at which each observation takes place.Default is NULL.
#' @param seed Random number seed. Default is 1234.
#' @param tree_control the list of parameters to control cutting initial breaks by decision tree. See details at: \code{\link{get_tree_breaks}}
#' @param bins_control  the list of parameters to control merging initial breaks. See details at: \code{\link{select_best_breaks}},\code{\link{select_best_class}}
#' @param best_lambda  Metheds of best lanmbda stardards using to filter variables by LASSO. There are 3 methods: ("lambda.auc", "lambda.ks", "lambda.sim_sign") . Default is  "lambda.auc".
#' @param lasso  Logical, if TRUE, variables filtering by LASSO. Default is TRUE.
#' @param step_wise  Logical, stepwise method. Default is TRUE.
#' @param thresholds Thresholds for selecting variables.
#' \itemize{
#'   \item \code{cor_p} The maximum threshold of correlation. Default: 0.8.
#'   \item \code{iv_i} The minimum threshold of IV. 0.01 to 0.1 usually work. Default: 0.02
#'   \item \code{psi_i} The maximum threshold of PSI. 0.1 to 0.3 usually work. Default: 0.1.
#'   \item \code{cos_i} cos_similarity of posive rate of train and test. 0.7 to 0.9 usually work.Default: 0.5.
#' }
#' @param ... Other parameters
#' @export


train_lr = function(dat_train,dat_test = NULL, target,x_list= NULL, occur_time = NULL,prop = 0.7,
                    tree_control = list(p = 0.02, cp = 0.00000001,
                                        xval = 5, maxdepth = 10),
                    bins_control = list(bins_num = 10, bins_pct = 0.05,
                                        b_chi = 0.02,b_odds = 0.1,
                                        b_psi = 0.03, b_or = 0.15,
                                        mono = 0.2, odds_psi = 0.15,
                                        kc = 1),
                    thresholds = list(cor_p = 0.8, iv_i = 0.02, psi_i = 0.1,cos_i = 0.6),
                    lasso = TRUE,step_wise = TRUE,
                    best_lambda = "lambda.auc",seed = 1234,
                    ...){
  if(is.null(seed)){seed = sample.int(10000, 1)[[1]]}
  if(is.null(dat_test)) {
    train_test = train_test_split(dat_train,split_type = 'OOT',prop = prop,
                                  occur_time = occur_time,seed = seed)
    dat_train = train_test$train
    dat_test = train_test$test
  }

  cor_p = ifelse(!is.null(thresholds$cor_p),thresholds$cor_p,0.7)
  iv_i= ifelse(!is.null(thresholds$iv_i),thresholds$iv_i,0.01)
  psi_i = ifelse(!is.null(thresholds$psi_i),thresholds$psi_i,0.2)
  cos_i = ifelse(!is.null(thresholds$cos_i),thresholds$cos_i,0.5)
  x_list = get_x_list(dat_train = dat_train, dat_test = dat_test,
                      x_list = x_list,ex_cols = c(target,occur_time))
  if(is.null(tree_control)){

    tree_control = list(
      p = 0.01, cp = 0,
      xval = 1, maxdepth = 15
    )
  }

  if(is.null(bins_control)){

    bins_control = list(bins_num = 10, bins_pct = 0.01,
                        b_chi = 0,b_odds = 0,
                        b_psi = 1, b_or = 0,
                        mono = 0.5, odds_psi = 1,
                        kc = 1)
  }
  breaks_list = get_breaks_all(dat = dat_train,
                               x_list = x_list,
                               occur_time = occur_time,
                               oot_pct = prop,
                               target = target,
                               tree_control = tree_control,
                               bins_control = bins_control,
                               best = TRUE,
                               note = FALSE,
                               save_data = FALSE)

  #psi_iv_filter
  iv_psi_list = psi_iv_filter(dat = dat_train, dat_test = dat_test,
                              x_list = x_list, target = target,
                              occur_time = occur_time, oot_pct = prop,
                              breaks_list = breaks_list,
                              psi_i = psi_i, iv_i = iv_i,cos_i= cos_i,
                              note = FALSE,
                              save_data = FALSE)
  if (length(iv_psi_list) < 1) {
    stop(paste("No variable satisfies the psi & iv condition."))
  }

  select_vars = as.character(iv_psi_list[,'Feature'])

  train_woe = woe_trans_all(dat = dat_train,
                            x_list = select_vars,
                            target = target,
                            bins_table = NULL,
                            breaks_list = breaks_list,
                            woe_name = FALSE, note = FALSE,
                            save_data = FALSE)
   if(length(select_vars)>1){
    select_vars = fast_high_cor_filter(dat = train_woe,
                                     x_list = select_vars,
                                     com_list = iv_psi_list,
                                     p = cor_p,
                                     note = FALSE,
                                     save_data = FALSE)

    }

  bins_table = get_bins_table_all(dat = dat_train, target = target,
                                  x_list = select_vars, breaks_list = breaks_list,
                                  dat_test = dat_test,
                                  note = FALSE, save_data = FALSE)

  test_woe = woe_trans_all(dat = dat_test,
                           x_list = unlist(select_vars),
                           target = target,
                           ex_cols = c(target, occur_time),
                           bins_table = bins_table,
                           breaks_list = breaks_list,
                           note = FALSE, woe_name = FALSE,
                           save_data = FALSE)
  if (lasso){
    select_vars = lasso_filter(dat_train = train_woe, dat_test = test_woe,
                               x_list = select_vars,
                               target = target,
                               sim_sign = "negtive",
                               best_lambda = best_lambda,
                               plot.it = FALSE, seed = seed,
                               save_data = FALSE)
  }

  Formula = as.formula(paste(target, paste(unique(select_vars), collapse = ' + '),
                             sep = ' ~ '))
  if (!is.null(seed)) set.seed(seed) else set.seed(46)
  lr_model = glm(Formula,
                 data = train_woe[, c(target, unique(select_vars))],
                 family = binomial(logit))
  dt_coef = data.frame(summary(lr_model)$coefficients)
  lg_coef = subset(dt_coef, abs(dt_coef$Estimate) > 0)
  glm_vars = row.names(lg_coef)[-1]
  Formula = as.formula(paste(target, paste(unique(glm_vars), collapse = ' + '), sep = ' ~ '))
  lr_model_new = glm(Formula,
                     data = train_woe[, c(target, unique(glm_vars))],
                     family = binomial(logit))
  #step wise
  if (step_wise) {
    lr_model_step = stats::step(lr_model_new, dir_pathection = "both", trace = FALSE)
    dt_step_coef = data.frame(summary(lr_model_step)$coefficients)
    step_vars = row.names(dt_step_coef)[-1]
    Formula = as.formula(paste(target, paste(unique( step_vars), collapse = ' + '), sep = ' ~ '))
    lr_model_new = glm(Formula,
                       data = train_woe[, c(target, unique(step_vars))],
                       family = binomial(logit))
  }

  train_pred = dat_train[c(occur_time, target)]
  test_pred = dat_test[c(occur_time, target)]
  train_pred$prob_LR = round(predict(lr_model_new, train_woe, type = "response"), 5)
  test_pred$prob_LR = round(predict(lr_model_new, test_woe, type = "response"), 5)

  max_iter = list()
  if (best_lambda == 'lambda.ks') {
    max_iter$train_ks = round(ks_value(target = train_pred[,target], prob = train_pred$prob_LR),4)
    max_iter$test_ks = round(ks_value(target = test_pred[,target], prob = test_pred$prob_LR),4)
  } else {
    max_iter$train_auc = round(auc_value(target = train_pred[,target], prob = train_pred$prob_LR),4)
    max_iter$test_auc = round(auc_value(target = test_pred[,target], prob = test_pred$prob_LR),4)
  }
  max_iter$psi = get_psi(dat = train_pred, dat_test = test_pred,x = "prob_LR",as_table = FALSE)$PSI
  return(list(lr_model = lr_model_new, max_iter = max_iter,seed = seed))

}


#' XGboost Parameters
#'
#'
#' \code{xgb_params} is the list of parameters to train a XGB model using in \code{\link{training_model}}.
#' \code{xgb_params_search} is for searching the optimal parameters of xgboost,if any parameters of params in \code{\link{xgb_params}} is more than one.
#' @param nrounds Max number of boosting iterations.
#' @param params  List of contains parameters of xgboost. The complete list of parameters is available at: \url{ http://xgboost.readthedocs.io/en/latest/parameter.html}
#' @param early_stopping_rounds  If NULL, the early stopping function is not triggered. If set to an integer k, training with a validation set will stop if the performance doesn't improve for k rounds.
#' @param f_eval Custimized evaluation function,"ks" & "auc" are available.
#' @param nfold Number of the cross validation of xgboost
#' @param nthread Number of threads
#' @param method Method of searching optimal parameters."random_search","grid_search","local_search" are available.
#' @param iters Number of iterations of "random_search" optimal parameters.
#' @param ... Other parameters
#' @return A list of parameters.
#' @importFrom xgboost xgb.importance xgb.train xgb.DMatrix xgb.dump xgb.save xgb.cv getinfo
#' @seealso \code{\link{training_model}}, \code{\link{lr_params}},\code{\link{gbm_params}}, \code{\link{rf_params}}
#' @export

xgb_params = function(nrounds = 1000,
                      params = list(max_depth = 6, eta = 0.01, gamma = 0,min_child_weight = 1,
                                    subsample = 1,colsample_bytree = 1,
                                    scale_pos_weight = 1),
                      early_stopping_rounds = 100, method = 'random_search', iters = 10, f_eval = "auc",
                      nfold = 1, nthread = 2,...) {
  structure(list(nrounds = nrounds, params = params,f_eval = f_eval, method = method,iters = iters,
                 nfold = nfold, nthread = nthread,
                 early_stopping_rounds = early_stopping_rounds))
}


#' @rdname xgb_params
#' @param dat_train  A data.frame of train data. Default is NULL.
#' @param dat_test  A data.frame of test data. Default is NULL.
#' @param target Name of target variable.
#' @param x_list Names of independent variables. Default is NULL.
#' @param prop Percentage of train-data after the partition. Default: 0.7.
#' @param occur_time The name of the variable that represents the time at which each observation takes place.Default is NULL.
#' @export

xgb_params_search = function(dat_train, target,dat_test = NULL, x_list = NULL, prop = 0.7,occur_time = NULL,
                             method = "random_search", iters = 10,
                             nrounds = 100,
                             early_stopping_rounds = 10,
                             params = list(max_depth = 6,
                                           eta = 0.01,
                                           gamma =0,
                                           min_child_weight = 1,
                                           subsample = 1,
                                           colsample_bytree = 1,
                                           scale_pos_weight = 1),
                             f_eval = 'auc', nfold = 1, nthread = 2,
                             ...) {


  if (length(method) > 1) stop("only one method can be provided to select best parameters.\n")
  xgb_list =  xgb_data(dat_train = dat_train, target = target,dat_test = dat_test, x_list = x_list, prop = prop,occur_time = occur_time)
  dtrain = xgb_list$dtrain
  dtest = xgb_list$dtest
  watchlist = xgb_list$watchlist
	max_depth = if(!is.null(params$max_depth))params$max_depth else 6
	eta = if (!is.null(params$eta)) params$eta else 0.01
	gamma = if (!is.null(params$gamma)) params$gamma else 0
	min_child_weight = if (!is.null(params$min_child_weight)) params$min_child_weight else 1
	subsample = if (!is.null(params$subsample))params$subsample else 1
	colsample_bytree = if (!is.null(params$colsample_bytree))params$colsample_bytree else 1
	scale_pos_weight = if (!is.null(params$scale_pos_weight)) params$scale_pos_weight else 1
  best_x_list=c()
  verbose = 0
  best_param = list()
  best_iter = c(0,0)
  best_iter_index = 0
  best_seed = 1234
  best_xgb_model = NULL
  iter_no = NULL
  if (any(sapply(params,length) >1) &
      any(is.element(method, c("random_search", "grid_search", "local_search")))){


    if (method == "random_search") {
      for (iter in 1:iters) {
        params = list(
          max_depth = ifelse(length(max_depth) > 1, sample(max_depth, 1), max_depth),
          eta = ifelse(length(eta) > 1, sample(eta, 1), eta),
          gamma = ifelse(length(gamma) > 1, sample(gamma, 1), gamma),
          min_child_weight = ifelse(length(min_child_weight) > 1, sample(min_child_weight, 1), min_child_weight),
          subsample = ifelse(length(subsample) > 1, sample(subsample,1), subsample),
          colsample_bytree = ifelse(length(colsample_bytree) > 1, sample(colsample_bytree, 1), colsample_bytree),
          scale_pos_weight = ifelse(length(scale_pos_weight) > 1, sample(scale_pos_weight, 1), scale_pos_weight)
        )

        seed_number = sample.int(10000, 1)[[1]]
        train_iter = train_xgb(seed_number = seed_number, dtrain = dtrain,
                               nthread = nthread,
                               nfold = nfold,
                               watchlist = watchlist,
                               nrounds = nrounds, f_eval = f_eval,
                               early_stopping_rounds = early_stopping_rounds,
                               verbose = verbose,
                               params = params)
        max_iter = train_iter$max_iter
        max_iter_index = train_iter$max_iter_index
        xgb_model = train_iter$xgb_model
        rm(train_iter)

        cat_line(paste(paste0("[", iter, "] "),
                       paste(names(max_iter), unlist(max_iter), collapse  = "  " ,sep  = ":")),col = love_color("deep_green"))
        cat_bullet(paste0("params:{",paste(names(params),
                                           params,collapse  = ", " ,sep  = ":" ), "}"), col = "darkgrey")
        if (max_iter[[2]] > best_iter[[2]]) {
          best_iter = max_iter
          best_iter_index = max_iter_index
          best_param = params
          best_xgb_model = xgb_model
          best_seed = seed_number
          iter_no = iter
        }

      }

      cat_rule(paste("[best iter]"),col = love_color("deep_purple"))
      cat_line(paste(paste0("[", iter_no, "] "),
                     paste(names(best_iter), unlist(best_iter), collapse  = "\t" ,sep  = ":")),col = love_color("deep_green"))

      cat_bullet(paste0("params:{",paste(names(best_param),
                                         best_param,collapse  = ", " ,sep  = ":" ), "}"), col = "darkgrey")

    } else {
      if (method == "grid_search") {
        iter = 1
        for (max_d in max_depth) {
          for (et in eta) {
            for (ga in gamma) {
              for (min_child in min_child_weight) {
                for (subs in subsample) {
                  for (cols in colsample_bytree) {
                    for (scale_pos in scale_pos_weight) {
                      params = list(
                        max_depth = max_d,
                        eta = et,
                        gamma = ga,
                        min_child_weight = min_child,
                        subsample = subs,
                        colsample_bytree = cols,
                        scale_pos_weight = scale_pos
                      )
                      seed_number = sample.int(10000, 1)[[1]]
                      train_iter = train_xgb(seed_number = seed_number, dtrain = dtrain,
                                             nthread = nthread,
                                             nfold = nfold,
                                             watchlist = watchlist,
                                             nrounds = nrounds, f_eval = f_eval,
                                             early_stopping_rounds = early_stopping_rounds,
                                             verbose = verbose,
                                             params = params)
                      max_iter = train_iter$max_iter
                      max_iter_index = train_iter$max_iter_index
                      xgb_model = train_iter$xgb_model
                      rm(train_iter)

                      cat_line(paste(paste0("[", iter, "] "),
                                     paste(names(max_iter), unlist(max_iter), collapse  = "  " ,sep  = ":")),col = love_color("deep_green"))
                      cat_bullet(paste0("params:{",paste(names(params),
                                                         params,collapse  = ", " ,sep  = ":" ), "}"), col = "darkgrey")
                      if (max_iter[[2]] > best_iter[[2]]) {
                        best_iter = max_iter
                        best_iter_index = max_iter_index
                        best_param = params
                        best_xgb_model = xgb_model
                        best_seed = seed_number
                        iter_no = iter
                      }
                      iter = iter + 1


                    }

                  }

                }

              }

            }

          }

        }
        cat_rule(paste("[best iter]"),col = love_color("deep_purple"))
        cat_line(paste(paste0("[", iter_no, "] "),
                       paste(names(best_iter), unlist(best_iter), collapse  = "\t" ,sep  = ":")),col = love_color("deep_green"))

        cat_bullet(paste0("params:{",paste(names(best_param),
                                           best_param,collapse  = ", " ,sep  = ":" ), "}"), col = "darkgrey")
      } else {
        if (method == "local_search") {
          iter = 1
          for (max_d in max_depth) {
            params = list(
              max_depth = max_d,
              eta = ifelse(length(eta) > 1, eta[1], eta),
              gamma = ifelse(length(gamma) > 1, gamma[1], gamma),
              min_child_weight = ifelse(length(min_child_weight) > 1, min_child_weight[1], min_child_weight),
              subsample = ifelse(length(subsample) > 1, subsample[1], subsample),
              colsample_bytree = ifelse(length(colsample_bytree) > 1, colsample_bytree[1], colsample_bytree),
              scale_pos_weight = ifelse(length(scale_pos_weight) > 1, scale_pos_weight[1], scale_pos_weight)
            )
            seed_number = sample.int(10000, 1)[[1]]
            train_iter = train_xgb(seed_number = seed_number, dtrain = dtrain,
                                   nthread = nthread,
                                   nfold = nfold,
                                   watchlist = watchlist,
                                   nrounds = nrounds, f_eval = f_eval,
                                   early_stopping_rounds = early_stopping_rounds,
                                   verbose = verbose,
                                   params = params)
            max_iter = train_iter$max_iter
            max_iter_index = train_iter$max_iter_index
            xgb_model = train_iter$xgb_model
            rm(train_iter)

            cat_line(paste(paste0("[", iter, "] "),
                           paste(names(max_iter), unlist(max_iter), collapse  = "  " ,sep  = ":")),col = love_color("deep_green"))
            cat_bullet(paste0("params:{",paste(names(params),
                                               params,collapse  = ", " ,sep  = ":" ), "}"), col = "darkgrey")
            if (max_iter[[2]] > best_iter[[2]]) {
              best_iter = max_iter
              best_iter_index = max_iter_index
              best_param = params
              best_xgb_model = xgb_model
              best_seed = seed_number
              iter_no = iter
            }
            iter = iter + 1
          }

          if (length(eta) > 1) {
            for (et in eta) {
              params = best_param
              params$eta = et
              seed_number = sample.int(10000, 1)[[1]]
              train_iter = train_xgb(seed_number = seed_number, dtrain = dtrain,
                                     nthread = nthread,
                                     nfold = nfold,
                                     watchlist = watchlist,
                                     nrounds = nrounds, f_eval = f_eval,
                                     early_stopping_rounds = early_stopping_rounds,
                                     verbose = verbose,
                                     params = params)
              max_iter = train_iter$max_iter
              max_iter_index = train_iter$max_iter_index
              xgb_model = train_iter$xgb_model
              rm(train_iter)

              cat_line(paste(paste0("[", iter, "] "),
                             paste(names(max_iter), unlist(max_iter), collapse  = "  " ,sep  = ":")),col = love_color("deep_green"))
              cat_bullet(paste0("params:{",paste(names(params),
                                                 params,collapse  = ", " ,sep  = ":" ), "}"), col = "darkgrey")
              if (max_iter[[2]] > best_iter[[2]]) {
                best_iter = max_iter
                best_iter_index = max_iter_index
                best_param = params
                best_xgb_model = xgb_model
                best_seed = seed_number
                iter_no = iter
              }
              iter = iter + 1

            }
          }
          if (length(gamma) > 1) {
            for (ga in gamma) {
              params = best_param
              params$gamma = ga
              seed_number = sample.int(10000, 1)[[1]]
              train_iter = train_xgb(seed_number = seed_number, dtrain = dtrain,
                                     nthread = nthread,
                                     nfold = nfold,
                                     watchlist = watchlist,
                                     nrounds = nrounds, f_eval = f_eval,
                                     early_stopping_rounds = early_stopping_rounds,
                                     verbose = verbose,
                                     params = params)
              max_iter = train_iter$max_iter
              max_iter_index = train_iter$max_iter_index
              xgb_model = train_iter$xgb_model
              rm(train_iter)

              cat_line(paste(paste0("[", iter, "] "),
                             paste(names(max_iter), unlist(max_iter), collapse  = "  " ,sep  = ":")),col = love_color("deep_green"))
              cat_bullet(paste0("params:{",paste(names(params),
                                                 params,collapse  = ", " ,sep  = ":" ), "}"), col = "darkgrey")
              if (max_iter[[2]] > best_iter[[2]]) {
                best_iter = max_iter
                best_iter_index = max_iter_index
                best_param = params
                best_xgb_model = xgb_model
                best_seed = seed_number
                iter_no = iter
              }
              iter = iter + 1

            }
          }
          if (length(min_child_weight) > 1) {
            for (min_child in min_child_weight) {
              params = best_param
              params$min_child_weight = min_child
              seed_number = sample.int(10000, 1)[[1]]
              train_iter = train_xgb(seed_number = seed_number, dtrain = dtrain,
                                     nthread = nthread,
                                     nfold = nfold,
                                     watchlist = watchlist,
                                     nrounds = nrounds, f_eval = f_eval,
                                     early_stopping_rounds = early_stopping_rounds,
                                     verbose = verbose,
                                     params = params)
              max_iter = train_iter$max_iter
              max_iter_index = train_iter$max_iter_index
              xgb_model = train_iter$xgb_model
              rm(train_iter)

              cat_line(paste(paste0("[", iter, "] "),
                             paste(names(max_iter), unlist(max_iter), collapse  = "  " ,sep  = ":")),col = love_color("deep_green"))
              cat_bullet(paste0("params:{",paste(names(params),
                                                 params,collapse  = ", " ,sep  = ":" ), "}"), col = "darkgrey")
              if (max_iter[[2]] > best_iter[[2]]) {
                best_iter = max_iter
                best_iter_index = max_iter_index
                best_param = params
                best_xgb_model = xgb_model
                best_seed = seed_number
                iter_no = iter
              }
              iter = iter + 1

            }
          }

          if (length(subsample) > 1) {
            for (subs in subsample) {
              params = best_param
              params$subsample = subs
              seed_number = sample.int(10000, 1)[[1]]
              train_iter = train_xgb(seed_number = seed_number, dtrain = dtrain,
                                     nthread = nthread,
                                     nfold = nfold,
                                     watchlist = watchlist,
                                     nrounds = nrounds, f_eval = f_eval,
                                     early_stopping_rounds = early_stopping_rounds,
                                     verbose = verbose,
                                     params = params)
              max_iter = train_iter$max_iter
              max_iter_index = train_iter$max_iter_index
              xgb_model = train_iter$xgb_model
              rm(train_iter)

              cat_line(paste(paste0("[", iter, "] "),
                             paste(names(max_iter), unlist(max_iter), collapse  = "  " ,sep  = ":")),col = love_color("deep_green"))
              cat_bullet(paste0("params:{",paste(names(params),
                                                 params,collapse  = ", " ,sep  = ":" ), "}"), col = "darkgrey")
              if (max_iter[[2]] > best_iter[[2]]) {
                best_iter = max_iter
                best_iter_index = max_iter_index
                best_param = params
                best_xgb_model = xgb_model
                best_seed = seed_number
                iter_no = iter
              }
              iter = iter + 1

            }
          }
          if (length(colsample_bytree) > 1) {
            for (cols in colsample_bytree) {
              params = best_param
              params$colsample_bytree = cols
              seed_number = sample.int(10000, 1)[[1]]
              train_iter = train_xgb(seed_number = seed_number, dtrain = dtrain,
                                     nthread = nthread,
                                     nfold = nfold,
                                     watchlist = watchlist,
                                     nrounds = nrounds, f_eval = f_eval,
                                     early_stopping_rounds = early_stopping_rounds,
                                     verbose = verbose,
                                     params = params)
              max_iter = train_iter$max_iter
              max_iter_index = train_iter$max_iter_index
              xgb_model = train_iter$xgb_model
              rm(train_iter)

              cat_line(paste(paste0("[", iter, "] "),
                             paste(names(max_iter), unlist(max_iter), collapse  = "  " ,sep  = ":")),col = love_color("deep_green"))
              cat_bullet(paste0("params:{",paste(names(params),
                                                 params,collapse  = ", " ,sep  = ":" ), "}"), col = "darkgrey")
              if (max_iter[[2]] > best_iter[[2]]) {
                best_iter = max_iter
                best_iter_index = max_iter_index
                best_param = params
                best_xgb_model = xgb_model
                best_seed = seed_number
                iter_no = iter
              }
              iter = iter + 1

            }
          }
          if (length(scale_pos_weight) > 1) {
            for (scale_pos in scale_pos_weight) {
              params = best_param
              params$scale_pos_weight = scale_pos
              seed_number = sample.int(10000, 1)[[1]]
              train_iter = train_xgb(seed_number = seed_number, dtrain = dtrain,
                                     nthread = nthread,
                                     nfold = nfold,
                                     watchlist = watchlist,
                                     nrounds = nrounds, f_eval = f_eval,
                                     early_stopping_rounds = early_stopping_rounds,
                                     verbose = verbose,
                                     params = params)
              max_iter = train_iter$max_iter
              max_iter_index = train_iter$max_iter_index
              xgb_model = train_iter$xgb_model
              rm(train_iter)

              cat_line(paste(paste0("[", iter, "] "),
                             paste(names(max_iter), unlist(max_iter), collapse  = "  " ,sep  = ":")),col = love_color("deep_green"))
              cat_bullet(paste0("params:{",paste(names(params),
                                                 params,collapse  = ", " ,sep  = ":" ), "}"), col = "darkgrey")
              if (max_iter[[2]] > best_iter[[2]]) {
                best_iter = max_iter
                best_iter_index = max_iter_index
                best_param = params
                best_xgb_model = xgb_model
                best_seed = seed_number
                iter_no = iter
              }
              iter = iter + 1

            }
          }
          cat_rule(paste("[best iter]"),col = love_color("deep_purple"))
          cat_line(paste(paste0("[", iter_no, "] "),
                         paste(names(best_iter), unlist(best_iter), collapse  = "\t" ,sep  = ":")),col = love_color("deep_green"))

          cat_bullet(paste0("params:{",paste(names(best_param),
                                             best_param,collapse  = ", " ,sep  = ":" ), "}"), col = "darkgrey")
        }
      }
    }

  }else {
    iter = 1
    params = list(

      max_depth = max_depth,
      eta = ifelse(length(eta) > 1, eta[1], eta),
      gamma = ifelse(length(gamma) > 1, gamma[1], gamma),
      min_child_weight = ifelse(length(min_child_weight) > 1, min_child_weight[1], min_child_weight),
      subsample = ifelse(length(subsample) > 1, subsample[1], subsample),
      colsample_bytree = ifelse(length(colsample_bytree) > 1, colsample_bytree[1], colsample_bytree),
      scale_pos_weight = ifelse(length(scale_pos_weight) > 1, scale_pos_weight[1], scale_pos_weight)
    )
    seed_number = sample.int(10000, 1)[[1]]
    train_iter = train_xgb(seed_number = seed_number, dtrain = dtrain,
                           nthread = nthread,
                           nfold = nfold,
                           watchlist = watchlist,
                           nrounds = nrounds, f_eval = f_eval,
                           early_stopping_rounds = early_stopping_rounds,
                           verbose = verbose,
                           params = params)
    max_iter = train_iter$max_iter
    max_iter_index = train_iter$max_iter_index
    xgb_model = train_iter$xgb_model
    rm(train_iter)

    cat_line(paste(paste0("[", iter, "] "),
                   paste(names(max_iter), unlist(max_iter), collapse  = "  " ,sep  = ":")),col = love_color("deep_green"))
    cat_bullet(paste0("params:{",paste(names(params),
                                       params,collapse  = ", " ,sep  = ":" ), "}"), col = "darkgrey")
    if (max_iter[[2]] > best_iter[[2]]) {
      best_iter = max_iter
      best_iter_index = max_iter_index
      best_param = params
      best_xgb_model = xgb_model
      best_seed = seed_number
      iter_no = iter
    }


    cat_rule(paste("[best iter]"),col = love_color("deep_purple"))
    cat_line(paste(paste0("[", iter_no, "] "),
                   paste(names(best_iter), unlist(best_iter), collapse  = "\t" ,sep  = ":")),col = love_color("deep_green"))

    cat_bullet(paste0("params:{",paste(names(best_param),
                                       best_param,collapse  = ", " ,sep  = ":" ), "}"), col = "darkgrey")
  }

  structure(list(xgb_model = best_xgb_model, nrounds = best_iter_index, params = best_param,
                 early_stopping_rounds = early_stopping_rounds, seed_number = best_seed))
}

#' XGboost data
#'
#'
#' \code{xgb_data} is for prepare data using in \code{\link{training_model}}.
#' @param dat_train  data.frame of train data. Default is NULL.
#' @param dat_test  data.frame of test data. Default is NULL.
#' @param target name of target variable.
#' @param x_list names of independent variables of raw data. Default is NULL.
#' @param prop Percentage of train-data after the partition. Default: 0.7.
#' @param occur_time The name of the variable that represents the time at which each observation takes place.Default is NULL.
#' @importFrom xgboost xgb.importance xgb.train xgb.DMatrix xgb.dump xgb.save xgb.cv getinfo
#' @export

xgb_data = function(dat_train, target, dat_test = NULL,x_list = NULL,prop = 0.7,occur_time = NULL){
  if (is.null(dat_test)) {
    train_test = train_test_split(dat_train,split_type = "OOT",prop = prop,occur_time = occur_time)
    dat_train = train_test$train
    dat_test = train_test$test
  }
  x_list = get_x_list(dat_train = dat_train, dat_test = dat_test, x_list = x_list)
  char_x_list = get_names(dat = dat_train[, x_list],
                          types = c('character', 'factor'),
                          ex_cols = c(target),
                          get_ex = FALSE)

  if (length(char_x_list) > 0) {
    nr = nrow(dat_train)
    var_list = unique(c(target, x_list))
    dat_ts = rbind(dat_train[, var_list], dat_test[, var_list])
    dat_ts = one_hot_encoding(dat = dat_ts, cat_vars = char_x_list, na_act = FALSE)
    dat_ts = low_variance_filter(dat = dat_ts, lvp = 1, note = FALSE)
    dat_train = dat_ts[1:nr,]
    dat_test = dat_ts[-c(1:nr),]
    x_list = get_x_list(dat_train = dat_train, dat_test = dat_test,
                        ex_cols = c(target))
    rm(dat_ts)
  }
  # Generate XGBoost DMatrix
  x_train = as.matrix(dat_train[, x_list])
  y_train = as.numeric(as.character(dat_train[, target]))
  xgb_train = list(data = x_train, label = y_train)
  dtrain = xgb.DMatrix(data = xgb_train$data, label = xgb_train$label)
  x_test = as.matrix(dat_test[, x_list])
  y_test = as.numeric(as.character(dat_test[, target]))
  xgb_test = list(data = x_test, label = y_test)
  dtest = xgb.DMatrix(data = xgb_test$data, label = xgb_test$label)
  watchlist = list(train = dtrain, eval = dtest)
  return(list(dtrain = dtrain, dtest = dtest,watchlist = watchlist,x_list = x_list,x_train = x_train,x_test = x_test, y_train = y_train,y_test = y_test))
}



#' Training XGboost
#'
#' \code{train_xgb} is for training a xgb model using in \code{\link{training_model}}.
#' @param nrounds Max number of boosting iterations.
#' @param params  List of contains parameters of xgboost. The complete list of parameters is available at: \url{ http://xgboost.readthedocs.io/en/latest/parameter.html}
#' @param early_stopping_rounds  If NULL, the early stopping function is not triggered. If set to an integer k, training with a validation set will stop if the performance doesn't improve for k rounds.
#' @param f_eval 	Custimized evaluation function,"ks" & "auc" are available.
#' @param nfold Number of the cross validation of xgboost
#' @param nthread Number of threads
#' @param dtrain train-data of xgb.DMatrix datasets.
#' @param seed_number Random number seed. Default is 1234.
#' @param verbose If 0, xgboost will stay silent. If 1, it will print information about performance.
#' @param watchlist named list of xgb.DMatrix datasets to use for evaluating model performance.generating by \code{\link{xgb_data}}
#' @param ... Other parameters
#' @importFrom xgboost xgb.importance xgb.train xgb.DMatrix xgb.dump xgb.save xgb.cv getinfo
#' @export

train_xgb = function(seed_number = 1234, dtrain,
                     nthread = 2,
                     nfold = 1,
                     watchlist = NULL,
                     nrounds = 100, f_eval = 'ks',
                     early_stopping_rounds = 10,
                     verbose = 0,
                     params = NULL,...) {

  if (f_eval == 'ks') {
  feval = eval_ks
  if (!is.null(nfold) && nfold > 1) {
    eval_log = c('train_ks_mean','test_ks_mean')
  } else {
    eval_log = c("train_ks","eval_ks")
  }
  } else {
  if(f_eval == 'auc'){
    feval = eval_auc
    if (!is.null(nfold) && nfold > 1) {
      eval_log = c('train_auc_mean','test_auc_mean')
    } else {
      eval_log = c("train_auc","eval_auc")
    }
  }else{
    if(f_eval == 'tnr'){
      feval = eval_tnr
      if (!is.null(nfold) && nfold > 1) {
        eval_log = c('train_tnr_mean','test_tnr_mean')
      } else {
        eval_log = c("train_tnr","eval_tnr")
      }
    }else{
	   if(f_eval == 'lift'){
      feval = eval_lift
      if (!is.null(nfold) && nfold > 1) {
        eval_log = c('train_lift_mean','test_lift_mean')
      } else {
        eval_log = c("train_lift","eval_lift")
      }
    }else{

      feval = f_eval
      if (!is.null(nfold) && nfold > 1) {
        eval_log = c('train_mean','test_mean')
      } else {
        eval_log = c("train","f_eval")
      }
    }
	}
  }
  }

  set.seed(seed_number)
  if (!is.null(nfold) && nfold > 1) {
    xgb_model = xgb.cv(data = dtrain, nthread = nthread, nfold = nfold, watchlist = watchlist,
                       nrounds = nrounds,
                       params = params,
                       objective = "binary:logistic",
                       feval = feval,
                       early_stopping_rounds = early_stopping_rounds,
                       verbose = verbose,
                       maximize = TRUE)

  } else {
    xgb_model = xgb.train(data = dtrain, nthread = nthread, watchlist = watchlist,
                          nrounds = nrounds,
                          params = params,
                          objective = "binary:logistic",
                          feval = feval,
                          early_stopping_rounds = early_stopping_rounds,
                          verbose = verbose,
                          maximize = TRUE)

  }
  evaluation_log = as.data.frame(xgb_model$evaluation_log)
  max_iter_index = which.max(unlist(evaluation_log[, eval_log[2]]))
  max_iter = evaluation_log[max_iter_index, eval_log]
  return(list(xgb_model = xgb_model,max_iter = max_iter,max_iter_index = as.integer(max_iter_index)))
}



#' GBM Parameters
#'
#' \code{gbm_params} is the list of parameters to train a GBM using in  \code{\link{training_model}}.
#' @param n.trees Integer specifying the total number of trees to fit. This is equivalent to the number of iterations and the number of basis functions in the additive expansion. Default is 100.
#' @param interaction.depth Integer specifying the maximum depth of each tree(i.e., the highest level of variable interactions allowed) . A value of 1 implies an additive model, a value of 2 implies a model with up to 2 - way interactions, etc. Default is 1.
#' @param n.minobsinnode  Integer specifying the minimum number of observations in the terminal nodes of the trees. Note that this is the actual number of observations, not the total weight.
#' @param shrinkage a shrinkage parameter applied to each tree in the expansion. Also known as the learning rate or step - size reduction; 0.001 to 0.1 usually work, but a smaller learning rate typically requires more trees. Default is 0.1 .
#' @param bag.fraction  the fraction of the training set observations randomly selected to propose the next tree in the expansion. This introduces randomnesses into the model fit. If bag.fraction < 1 then running the same model twice will result in similar but different fits. gbm uses the R random number generator so set.seed can ensure that the model can be reconstructed. Preferably, the user can save the returned gbm.object using save. Default is 0.5 .
#' @param train.fraction The first train.fraction * nrows(data) observations are used to fit the gbm and the remainder are used for computing out-of-sample estimates of the loss function.
#' @param cv.folds  Number of cross - validation folds to perform. If cv.folds > 1 then gbm, in addition to the usual fit, will perform a cross - validation, calculate an estimate of generalization error returned in cv.error.
#' @param ... Other parameters
#' @return A list of parameters.
#' @details See details at: \code{gbm}
#' @seealso \code{\link{training_model}}, \code{\link{lr_params}}, \code{\link{xgb_params}}, \code{\link{rf_params}}
#' @export


gbm_params = function(n.trees = 1000, interaction.depth = 6, shrinkage = 0.01,
                      bag.fraction = 0.5, train.fraction = 0.7, n.minobsinnode = 30,
                      cv.folds = 5, ...) {
  structure(list(n.trees = n.trees, interaction.depth = interaction.depth,
                 shrinkage = shrinkage, bag.fraction = bag.fraction,
                 train.fraction = train.fraction, n.minobsinnode = n.minobsinnode,
                 cv.folds = cv.folds))
}

#' Random Forest Parameters
#'
#' \code{rf_params} is the list of parameters to train a Random Forest using in  \code{\link{training_model}}.
#' @param ntree Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
#' @param nodesize  Minimum size of terminal nodes. Setting this number larger causes smaller trees to be grown (and thus take less time). Note that the default values are different for classification (1) and regression (5).
#' @param samp_rate   Percentage of sample to draw. Default is 0.2.
#' @param tune_rf A logical.If TRUE, then tune Random Forest model.Default is FALSE.
#' @param ... Other parameters
#' @return A list of parameters.
#' @details See details at : \url{https://www.stat.berkeley.edu/~breiman/Using_random_forests_V3.1.pdf}
#' @seealso  \code{\link{training_model}}, \code{\link{lr_params}}, \code{\link{gbm_params}}, \code{\link{xgb_params}}
#' @export

rf_params = function(ntree = 100, nodesize = 30, samp_rate = 0.5, tune_rf = FALSE, ...) {
  structure(list(ntree = ntree, nodesize = nodesize,
                 samp_rate = samp_rate, tune_rf = tune_rf))
}




#' Score Transformation
#'
#' \code{score_transfer} is  for transfer woe to score.
#' @param model A data frame with x and target.
#' @param tbl_woe a data.frame with woe variables.
#' @param a  Base line of score.
#' @param b  Numeric.Increased scores from doubling Odds.
#' @param save_data Logical, save results in locally specified folder. Default is FALSE.
#' @param file_name The name for periodically saved score file. Default is "dat_score".
#' @param dir_path  The path for periodically saved score file.  Default is "./data"
#' @return  A data.frame with variables which values transfered to score.
#' @examples
#' # dataset spliting
#' sub = cv_split(UCICreditCard, k = 30)[[1]]
#' dat = UCICreditCard[sub,]
#' #rename the target variable
#' dat = re_name(dat, "default.payment.next.month", "target")
#' dat = data_cleansing(dat, target = "target", obs_id = "ID",
#' occur_time = "apply_date", miss_values =  list("", -1))
#' #train_ test pliting
#' train_test = train_test_split(dat, split_type = "OOT", prop = 0.7,
#'                                 occur_time = "apply_date")
#' dat_train = train_test$train
#' dat_test = train_test$test
#' #get breaks of all predictive variables
#' x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "EDUCATION", "PAY_3", "PAY_2")
#' breaks_list = get_breaks_all(dat = dat_train, target = "target",
#'                               x_list = x_list, occur_time = "apply_date", ex_cols = "ID",
#' save_data = FALSE, note = FALSE)
#' #woe transforming
#' train_woe = woe_trans_all(dat = dat_train,
#'                           target = "target",
#'                           breaks_list = breaks_list,
#'                           woe_name = FALSE)
#' test_woe = woe_trans_all(dat = dat_test,
#'                        target = "target",
#'                          breaks_list = breaks_list,
#'                          note = FALSE)
#' Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))
#' set.seed(46)
#' lr_model = glm(Formula, data = train_woe[, c("target", x_list)], family = binomial(logit))
#' #get LR coefficient
#' dt_imp_LR = get_logistic_coef(lg_model = lr_model, save_data = FALSE)
#' bins_table = get_bins_table_all(dat = dat_train, target = "target",
#'                                 x_list = x_list,dat_test = dat_test,
#'                                breaks_list = breaks_list, note = FALSE)
#' #score card
#' LR_score_card = get_score_card(lg_model = lr_model, bins_table, target = "target")
#' #scoring
#' train_pred = dat_train[, c("ID", "apply_date", "target")]
#' test_pred = dat_test[, c("ID", "apply_date", "target")]
#' train_pred$pred_LR = score_transfer(model = lr_model,
#'                                                     tbl_woe = train_woe,
#'                                                     save_data = FALSE)[, "score"]
#'
#' test_pred$pred_LR = score_transfer(model = lr_model,
#' tbl_woe = test_woe, save_data = FALSE)[, "score"]
#' @export

score_transfer = function(model, tbl_woe, a = 600, b = 50,
                           file_name = NULL, dir_path = tempdir(),
                           save_data = FALSE) {
  coef = model$coefficients
  glm_vars = names(coef)[-1]
  A = a
  B = b / log(2)
  base_score = A - B * coef[1]
  tbl_woe = tbl_woe[c(glm_vars)]
  score_name = c()
  for (i in glm_vars) {
    tbl_woe[i] = (-1) * B * tbl_woe[i] * coef[i]
    score_name[i] = gsub("_woe", "_score", i)
  }
  names(tbl_woe) = score_name
  tbl_woe$score = apply(tbl_woe[1:length(tbl_woe)], MARGIN = 1, function(x) sum(x))
  tbl_woe$score = round(tbl_woe$score + base_score, 2)
  if (save_data) {
    dir_path = ifelse(!is.character(dir_path),
                      tempdir(), dir_path)
    if (!dir.exists(dir_path)) dir.create(dir_path)
    if (!is.character(file_name)) file_name = NULL

    save_data(tbl_woe, file_name = ifelse(is.null(file_name), "lr_score", paste(file_name, "lr_score", sep = ".")), dir_path = dir_path, note = FALSE)
  }
  tbl_woe
}

#' Score Card
#'
#' \code{get_score_card} is  for generating a stardard scorecard
#' @param lg_model An object of glm model.
#' @param target The name of target variable.
#' @param bins_table a data.frame generated by \code{\link{get_bins_table}}
#' @param a  Base line of score.
#' @param b  Numeric.Increased scores from doubling Odds.
#' @param save_data Logical, save results in locally specified folder. Default is FALSE.
#' @param file_name  The name for periodically saved scorecard file. Default is "LR_Score_Card".
#' @param dir_path  The path for periodically saved scorecard file. Default is "./model"
#' @return  scorecard
#' @export
#' @examples
#' # dataset spliting
#' sub = cv_split(UCICreditCard, k = 30)[[1]]
#' dat = UCICreditCard[sub,]
#' #rename the target variable
#' dat = re_name(dat, "default.payment.next.month", "target")
#' dat = data_cleansing(dat, target = "target", obs_id = "ID",
#' occur_time = "apply_date", miss_values =  list("", -1))
#' #train_ test pliting
#' train_test = train_test_split(dat, split_type = "OOT", prop = 0.7,
#'                                 occur_time = "apply_date")
#' dat_train = train_test$train
#' dat_test = train_test$test
#' #get breaks of all predictive variables
#' x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "EDUCATION", "PAY_3", "PAY_2")
#' breaks_list = get_breaks_all(dat = dat_train, target = "target",
#'                               x_list = x_list, occur_time = "apply_date", ex_cols = "ID",
#' save_data = FALSE, note = FALSE)
#' #woe transforming
#' train_woe = woe_trans_all(dat = dat_train,
#'                           target = "target",
#'                           breaks_list = breaks_list,
#'                           woe_name = FALSE)
#' test_woe = woe_trans_all(dat = dat_test,
#'                        target = "target",
#'                          breaks_list = breaks_list,
#'                          note = FALSE)
#' Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))
#' set.seed(46)
#' lr_model = glm(Formula, data = train_woe[, c("target", x_list)], family = binomial(logit))
#' #get LR coefficient
#' dt_imp_LR = get_logistic_coef(lg_model = lr_model, save_data = FALSE)
#' bins_table = get_bins_table_all(dat = dat_train, target = "target",
#'                                  dat_test = dat_test,
#'                                 x_list = x_list,
#'                                breaks_list = breaks_list, note = FALSE)
#' #score card
#' LR_score_card = get_score_card(lg_model = lr_model, bins_table, target = "target")
#' #scoring
#' train_pred = dat_train[, c("ID", "apply_date", "target")]
#' test_pred = dat_test[, c("ID", "apply_date", "target")]
#' train_pred$pred_LR = score_transfer(model = lr_model,
#'                                                     tbl_woe = train_woe,
#'                                                     save_data = FALSE)[, "score"]
#'
#' test_pred$pred_LR = score_transfer(model = lr_model,
#' tbl_woe = test_woe, save_data = FALSE)[, "score"]

get_score_card = function(lg_model, target, bins_table, a = 600, b = 50,
                           file_name = NULL, dir_path = tempdir(),
                           save_data = FALSE) {
  coef = lg_model$coefficients
  glm_vars = gsub("_woe", "", names(coef))
  names(coef) = glm_vars
  A = a
  B = b / log(2)
  base_score = A - B * coef[1]
  dt_score_card = bins_table[which(as.character(bins_table[, "Feature"]) %in% glm_vars),
                              c("Feature", "cuts", "bins", "woe")]

  for (i in glm_vars) {
    dt_score_card[which(as.character(dt_score_card[, "Feature"]) == i), "coefficient"] = round(coef[i], 5)
  }

  for (i in glm_vars) {
    dt_score_card[which(as.character(dt_score_card[, "Feature"]) == i), "score"] =
      round((-1) * B * as.numeric(dt_score_card[which(as.character(dt_score_card[, "Feature"]) == i), "woe"]) * coef[i], 2)
  }
  Intercept = c("Intercept", "", "", "", round(coef[1], 5), paste("Base:", round(base_score)))
  dt_score_card = rbind(Intercept, dt_score_card)
  if (save_data) {
    dir_path = ifelse(!is.character(dir_path),
                      tempdir(), dir_path)
    if (!dir.exists(dir_path)) dir.create(dir_path)
    if (!is.character(file_name)) file_name = NULL
    save_data(dt_score_card, file_name = ifelse(is.null(file_name), "scorecard", paste(file_name, "scorecard", sep = ".")), dir_path = dir_path, note = FALSE)
  }
  dt_score_card
}



#' get logistic coef
#'
#' \code{get_logistic_coef} is  for geting logistic coefficients.
#' @param lg_model  An object of logistic model.
#' @param save_data Logical, save the result or not. Default is FALSE.
#' @param file_name  The name for periodically saved coefficient file.  Default is "LR_coef".
#' @param dir_path  The Path for periodically saved coefficient file. Default is "./model".
#' @return  A data.frame with logistic coefficients.
#' @examples
#' # dataset spliting
#' sub = cv_split(UCICreditCard, k = 30)[[1]]
#' dat = UCICreditCard[sub,]
#' #rename the target variable
#' dat = re_name(dat, "default.payment.next.month", "target")
#' dat = data_cleansing(dat, target = "target", obs_id = "ID",
#' occur_time = "apply_date", miss_values =  list("", -1))
#' #train_ test pliting
#' train_test = train_test_split(dat, split_type = "OOT", prop = 0.7,
#'                                 occur_time = "apply_date")
#' dat_train = train_test$train
#' dat_test = train_test$test
#' #get breaks of all predictive variables
#' x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "EDUCATION", "PAY_3", "PAY_2")
#' breaks_list = get_breaks_all(dat = dat_train, target = "target",
#'                               x_list = x_list, occur_time = "apply_date", ex_cols = "ID",
#' save_data = FALSE, note = FALSE)
#' #woe transforming
#' train_woe = woe_trans_all(dat = dat_train,
#'                           target = "target",
#'                           breaks_list = breaks_list,
#'                           woe_name = FALSE)
#' test_woe = woe_trans_all(dat = dat_test,
#'                        target = "target",
#'                          breaks_list = breaks_list,
#'                          note = FALSE)
#' Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))
#' set.seed(46)
#' lr_model = glm(Formula, data = train_woe[, c("target", x_list)], family = binomial(logit))
#' #get LR coefficient
#' dt_imp_LR = get_logistic_coef(lg_model = lr_model, save_data = FALSE)
#' bins_table = get_bins_table_all(dat = dat_train, target = "target",
#'                                 x_list = x_list,dat_test = dat_test,
#'                                breaks_list = breaks_list, note = FALSE)
#' #score card
#' LR_score_card = get_score_card(lg_model = lr_model, bins_table, target = "target")
#' #scoring
#' train_pred = dat_train[, c("ID", "apply_date", "target")]
#' test_pred = dat_test[, c("ID", "apply_date", "target")]
#' train_pred$pred_LR = score_transfer(model = lr_model,
#'                                                     tbl_woe = train_woe,
#'                                                     save_data = TRUE)[, "score"]
#'
#' test_pred$pred_LR = score_transfer(model = lr_model,
#' tbl_woe = test_woe, save_data = FALSE)[, "score"]
#' @export
get_logistic_coef = function(lg_model, file_name = NULL,
                             dir_path = tempdir(), save_data = FALSE) {
  lg_coef = data.frame(summary(lg_model)$coefficients)
  lg_coef[4] = round(lg_coef[4], 5)
  lg_coef[, "Feature"] = row.names(lg_coef)
  if (length(row.names(lg_coef)) > 2) {
    lg_coef[-1, "vif"] = lr_vif(lg_model)
  } else {
    lg_coef[-1, "vif"] = 0
  }
  names(lg_coef) = c("estimate", "std.error", "Z_value", "P_value", "Feature", "vif")
  lg_coef = lg_coef[c("Feature", "estimate", "std.error", "Z_value", "P_value", "vif")]
  if (save_data) {
    dir_path = ifelse(!is.character(dir_path), tempdir(), dir_path)
    if (!dir.exists(dir_path)) dir.create(dir_path)
    if (!is.character(file_name)) file_name = NULL
    save_data(lg_coef, file_name = ifelse(is.null(file_name), "logistic.coef", paste(file_name, "logistic.coef", sep = ".")), dir_path = dir_path, note = FALSE)
  }
  return(lg_coef)
}


#' Variance-Inflation Factors
#'
#' \code{lr_vif} is  for calculating Variance-Inflation Factors.
#' @param lr_model  An object of logistic model.
#' @examples
#' sub = cv_split(UCICreditCard, k = 30)[[1]]
#' x_list = c("PAY_0", "LIMIT_BAL", "PAY_AMT5", "PAY_3", "PAY_2")
#' dat = re_name(UCICreditCard[sub,], "default.payment.next.month", "target")
#' dat = dat[,c("target",x_list)]
#'
#' dat = data_cleansing(dat, miss_values = list("", -1))
#'
#' train_test = train_test_split(dat,  prop = 0.7)
#' dat_train = train_test$train
#' dat_test = train_test$test
#'
#' Formula = as.formula(paste("target", paste(x_list, collapse = ' + '), sep = ' ~ '))
#' set.seed(46)
#' lr_model = glm(Formula, data = dat_train[, c("target", x_list)], family = binomial(logit))
#' lr_vif(lr_model)
#' get_logistic_coef(lr_model)
#' class(dat)
#' mod = lr_model
#' lr_vif(lr_model)
#' @importFrom stats coefficients cov2cor vcov
#' @export
lr_vif = function(lr_model) {
  if (any(is.na(coef(lr_model)))){
    stop ("Some coefficients is NA in the model.\n")
  }
  if(length(coef(lr_model)[which(names(coefficients(lr_model)) != "(Intercept)")]) < 2){
    stop("There are only one predictor in  the model.\n")
  }
  cov_coef = vcov(lr_model)[-1,-1]
  cor_coef = cov2cor(cov_coef)
  det_R = det(cor_coef)
  vif_res = c()
  for(i in 1:ncol(cor_coef)){
    coef_name = colnames(cor_coef)[i]
    vif_res[coef_name] = det(as.matrix(cor_coef[i, i])) * det(as.matrix(cor_coef[-i, -i])) / det_R
  }
  vif_res
}

#' pred_score
#'
#' \code{pred_score} is for using logistic regression model model to predict new data.
#' @param model  Logistic Regression Model generated by \code{\link{training_model}}.
#' @param dat Dataframe of new data.
#' @param x_list  Into the model variables.
#' @param bins_table a data.frame generated by \code{\link{get_bins_table}}
#' @param obs_id  The name of ID of observations or key variable of data. Default is NULL.
#' @param miss_values Special values.
#' @param woe_name Logical. Whether woe variable's name contains 'woe'.Default is FALSE.
#' @return new scores.
#' @seealso \code{\link{training_model}}, \code{\link{lr_params}}, \code{\link{xgb_params}}, \code{\link{rf_params}}
#' @export


pred_score = function(model, dat, x_list = NULL, bins_table = NULL, obs_id = NULL,
 miss_values = list(-1,"-1","NULL","-1","-9999","-9996","-9997","-9995","-9998", -9999,-9998,-9997,-9996,-9995),
                      woe_name = FALSE) {
  dat = data_cleansing(dat,low_var = FALSE, miss_values = miss_values, merge_cat = FALSE,remove_dup = FALSE)
  dat = process_nas(dat = dat, class_var = FALSE, x_list = x_list, ex_cols = c(obs_id), default_miss = TRUE)
  dat_woe = woe_trans_all(dat = dat, x_list = x_list, target = NULL, ex_cols = NULL, bins_table = bins_table,
                          breaks_list = NULL, sp_values = list(-1, "missing"), file_name = 'woe', note = FALSE, save_data = FALSE, woe_name = woe_name)
  dat_pred = dat[c(obs_id)]
  dat_pred$pred_score = score_transfer(model = model, tbl_woe = dat_woe, save_data = FALSE)[, "score"]
  return(dat_pred)
}

#' prob to socre
#'
#' \code{p_to_score} is for transforming probability to score.
#' @param p  Probability.
#' @param PDO Point-to-Double Odds.
#' @param base  Base Point.
#' @param ratio The corresponding odds when the score is base.
#' @return new prob.
#' @seealso \code{\link{training_model}}, \code{\link{pred_score}}
#' @export


p_to_score = function(p, PDO=20, base=600, ratio = 1){
  B = PDO/log(2)
  A = base + B * log(ratio)
  y = log(p / (1 - p))
  score = A - B *y
  return(round(score,0))
}




