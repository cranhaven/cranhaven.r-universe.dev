calculate_compare_with_known <- function(env_obj) {

	if (! is.data.frame(env_obj$known_regular_step_ds)) { known_regular_step_ds <- as.data.frame(env_obj$known_regular_step_ds) }

	env_obj$known_regular_step_ds <- env_obj$known_regular_step_ds[ order(env_obj$known_regular_step_ds$date_as_sec),]
	prev_rows <- nrow(env_obj$known_regular_step_ds)
	env_obj$known_regular_step_ds <- env_obj$known_regular_step_ds[ env_obj$known_regular_step_ds$date_as_sec >= env_obj$first_time & env_obj$known_regular_step_ds$date_as_sec < max(env_obj$t_reg), ]	
	
	if (env_obj$show_prints) {
		print(env_obj$t_reg)
		print(env_obj$included_intervals)
		print(length(env_obj$t_reg))
		print(length(env_obj$included_intervals))
	}
	
	env_obj$known_regular_step_ds$t_intervals <- as.numeric(as.character(cut(x=env_obj$known_regular_step_ds$date_as_sec, breaks=env_obj$t_reg, labels=1:(env_obj$N-1), right=TRUE)))

	#cut off at the end of what we have
	env_obj$known_regular_step_ds <- env_obj$known_regular_step_ds[ ! is.na(env_obj$known_regular_step_ds$t_intervals),]
	env_obj$known_regular_step_ds <- env_obj$known_regular_step_ds[ env_obj$known_regular_step_ds$t_intervals < env_obj$N,]
	env_obj$known_regular_step_ds$tag <- factor(env_obj$known_regular_step_ds$tag)
	
	if (env_obj$show_prints) {

		print("true")
		print(head(env_obj$known_regular_step_ds))
		print("obs")
		print(head(env_obj$d))
	}
	
	#recalculate if anything changed
	
	if (env_obj$nstates > 1 & (prev_rows != nrow(env_obj$known_regular_step_ds))) {
	
				
		tmp0 <- true_behavior_probabilities(reg_step_ds=env_obj$known_regular_step_ds, 
											shark_names=env_obj$shark_names, nstates=env_obj$nstates,
											nregions=env_obj$nregions)
											
		if (is.null(env_obj$known_foraging_prob)) {
			env_obj$known_foraging_prob <- tmp0$true_foraging_prob
		}

		if (is.null(env_obj$known_trans_prob)) {
			env_obj$known_transition_prob <- tmp0$true_transition_prob
		}
	
	}

	invisible(NULL)

}