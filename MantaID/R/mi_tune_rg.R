#' Tune Random Forest model by hyperband.
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
mi_tune_rg <- function(data, resampling = rsmp("cv", folds = 5), measure = msr("classif.acc"), eta = 3) {
	search_space <- ps(
		regularization.factor = p_dbl(lower = 0.01, upper = 1),
		minprop = p_dbl(lower = 0.005, upper = 0.15, tags = "budget"),
		num.trees = p_int(lower = 100, upper = 600),
		max.depth = p_int(lower = 20, upper = 400)
	)
	data %<>% slice(sample(nrow(.), nrow(.)))
	learner <- lrn("classif.ranger", importance = "impurity", save.memory = F, oob.error = F, num.threads = 12)
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
	hyperband_group <- result %<>% bind_cols(hyperband = str_c(result$regularization.factor,result$num.trees,result$max.depth)) %>% mutate("stage" = .data[["stage"]] + 1)
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