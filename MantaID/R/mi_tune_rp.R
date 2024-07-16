#' Tune decision tree model by hyperband.
#'
#' @param data A tibble.All are numeric except the first column is a factor.
#' @param resampling R6/Resampling.
#' @param measure Model evaluation method.Use `mlr_measures` and `msr()` to view and choose metrics.
#' @param eta The percent parameter configurations discarded.
#' @importFrom dplyr slice mutate pull group_by
#' @importFrom mlr3tuning tnr tune
#' @importFrom paradox ps p_dbl p_int
#' @importFrom data.table as.data.table
#' @importFrom mlr3 as_task_classif lrn rsmp
#' @importFrom ggplot2 geom_line geom_point guides scale_x_continuous scale_y_continuous
#' @return A list of tuning instance and stage plot.
#' @export
mi_tune_rp <- function(data, resampling = rsmp("bootstrap", ratio = 0.8, repeats = 5), measure = msr("classif.acc"), eta = 3) {
	search_space <- ps(
		cp = p_dbl(lower = 0.0005, upper = 0.01),
		maxdepth = p_int(lower = 10, upper = 30),
		minsplit = p_int(lower = 1, upper = 40, tags = "budget"),
		maxcompete = p_int(lower = 1, upper = 10)
	)
	data %<>% slice(sample(nrow(.), nrow(.)))
	learner <- lrn("classif.rpart", keep_model = F)
	task <- data %>%
		as.data.table() %>%
		as_task_classif(target = "class", feature = -c("class"))
	instance <- tune(
		method = tnr("hyperband", eta = 3),
		task = task,
		learner = learner,
		resampling = resampling,
		measures = measure,
		search_space = search_space
	)
	result = instance$archive$data
	hyperband_group <- result %<>% bind_cols(hyperband = str_c(result$maxdepth,result$maxcompete,result$cp)) %>% mutate("stage" = .data[["stage"]] + 1)
	fct <- hyperband_group %>%
		pull(.data[['hyperband']]) %>%
		factor()
	result <- hyperband_group %>% split(fct)
	p <- ggplot(data = hyperband_group, mapping = aes(x = .data[["stage"]], y = .data[["classif.acc"]], group =  .data[["hyperband"]], colour = factor(.data[['hyperband']]))) +
		scale_x_continuous(limits = c(0.5, max(hyperband_group$stage) + 0.5)) +
		scale_y_continuous(limits = c(min(hyperband_group$classif.acc), max(hyperband_group$classif.acc))) +
		theme_bw() +
		guides(color = FALSE) +
		geom_point() +
		geom_line()
	return(list(instance, p))
}