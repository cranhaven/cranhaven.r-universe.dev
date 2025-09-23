#' Inference for A Sequential Design
#' 
#' @description
#' An R6 Class that estimates, tests and provides intervals for a treatment effect in a sequential design.
#' This class takes a \code{SeqDesign} object as an input where this object
#' contains data for a fully completed sequential experiment (i.e. all treatment
#' assignments were allocated and all responses were collected). Then the user
#' specifies the type of estimation (difference-in-means or OLS) and the type
#' of sampling assumption (i.e. the superpopulation assumption leading to normal-based inference or 
#' the finite population assumption implying randomization-exact-based inference) and then can query the
#' estimate and pval for the test. If the test is normal-theory based it is 
#' testing the population H_0: beta_T = 0 and if the test is a randomization test, 
#' it is testing the sharp null that H_0: Y_T_i = Y_C_i for all subjects. Confidence
#' interval construction is available for normal-theory based test type as well.
#' 
#' @export
SeqDesignInference = R6::R6Class("SeqDesignInference",
	public = list(
		#' @field estimate_type		The type of estimate to compute (either "difference-in-means" or "OLS").
		estimate_type = NULL,
		#' @field test_type			The type of test to run (either "normal-based" or "randomization-exact").
		test_type = NULL,
		#' @field num_cores			The number of CPU cores to employr during sampling within randomization inference
		num_cores = NULL,
		#' @field verbose			A flag that indicates whether messages should be displayed to the user
		verbose = NULL,
		
		#' @description
		#' Initialize a sequential experimental design estimation and test object after the sequential design is completed.
		#' 
		#' 
		#' @param seq_des_obj		A SeqDesign object whose entire n subjects are assigned and response y is recorded within.
		#' @param estimate_type		The type of estimate to compute (either "difference-in-means" or "OLS"). Default is "OLS"
		#' 							as this provided higher power in our simulations.
		#' @param test_type			The type of test to run (either "normal-based" implying your subject entrant sampling 
		#' 							assumption is from a superpopulation or "randomization-exact" implying a finite sampling
		#' 							assumption). The default option is "randomization-exact" as it provided properly-sized 
		#' 							tests in our simulations.
		#' @param num_cores			The number of CPU cores to use to parallelize the sampling during randomization-based inference 
		#' 							(which is very slow). The default is 1 for serial computation. This parameter is ignored
		#' 							for \code{test_type = "normal-based"}.
		#' @param verbose			A flag indicating whether messages should be displayed to the user. Default is \code{TRUE}
		#' 
		#' @examples
		#' seq_des = SeqDesign$new(n = 6, p = 10, design = "CRD")
		#' seq_des$add_subject_to_experiment(c(1, 38, 142, 71, 5.3, 0, 0, 0, 1, 0))
		#' seq_des$add_subject_to_experiment(c(0, 27, 127, 60, 5.5, 0, 0, 0, 1, 0))
		#' seq_des$add_subject_to_experiment(c(1, 42, 169, 74, 5.1, 0, 1, 0, 0, 0))
		#' seq_des$add_subject_to_experiment(c(0, 59, 105, 62, 5.9, 0, 0, 0, 1, 0))
		#' seq_des$add_subject_to_experiment(c(1, 32, 186, 66, 5.6, 1, 0, 0, 0, 0))
		#' seq_des$add_subject_to_experiment(c(1, 37, 178, 75, 6.5, 0, 0, 0, 0, 1))
		#' seq_des$add_all_subject_responses(c(4.71, 1.23, 4.78, 6.11, 5.95, 8.43))
		#' 
		#' seq_des_inf = SeqDesignInference$new(seq_des)
		#'  
		#' @return A new `SeqDesignTest` object.
		initialize = function(seq_des_obj, estimate_type = "OLS", test_type = "randomization-exact", num_cores = 1, verbose = TRUE){
			assertClass(seq_des_obj, "SeqDesign")
			seq_des_obj$assert_experiment_completed()
			assertChoice(estimate_type, c("difference-in-means", "OLS"))
			assertChoice(test_type, c("normal-based", "randomization-exact"))
			assertCount(num_cores, positive = TRUE)
			assertFlag(verbose)
			
			private$seq_des_obj = seq_des_obj
			private$isKK = seq_des_obj$.__enclos_env__$private$isKK
			self$estimate_type = estimate_type
			self$test_type = test_type
			self$num_cores = num_cores
			self$verbose = verbose
			
			if (self$verbose){
				cat(paste0("Intialized inference methods for a ", seq_des_obj$design, " design using estimation type: ", estimate_type, " and test type: ", test_type, ".\n"))
			}	
		},
		
		#' @description
		#' Computes either the classic different-in-means estimate of the additive treatment effect, 
		#' i.e. ybar_T - ybar_C or the OLS estimate of the additive treatment effect linearly i.e. 
		#' the treatment different adjusted linearly for the p covariates.
		#' 
		#' @return 	The numeric estimate of the treatment effect
		#' 
		#' @examples
		#' seq_des = SeqDesign$new(n = 6, p = 10, design = "CRD")
		#' seq_des$add_subject_to_experiment(c(1, 38, 142, 71, 5.3, 0, 0, 0, 1, 0))
		#' seq_des$add_subject_to_experiment(c(0, 27, 127, 60, 5.5, 0, 0, 0, 1, 0))
		#' seq_des$add_subject_to_experiment(c(1, 42, 169, 74, 5.1, 0, 1, 0, 0, 0))
		#' seq_des$add_subject_to_experiment(c(0, 59, 105, 62, 5.9, 0, 0, 0, 1, 0))
		#' seq_des$add_subject_to_experiment(c(1, 32, 186, 66, 5.6, 1, 0, 0, 0, 0))
		#' seq_des$add_subject_to_experiment(c(1, 37, 178, 75, 6.5, 0, 0, 0, 0, 1))
		#' seq_des$add_all_subject_responses(c(4.71, 1.23, 4.78, 6.11, 5.95, 8.43))
		#' 
		#' seq_des_inf = SeqDesignInference$new(seq_des)
		#' seq_des_inf$compute_treatment_estimate()
		#' 		
		compute_treatment_estimate = function(){
			if (self$estimate_type == "difference-in-means" & private$isKK){
				private$compute_normal_based_inference_difference_in_means_KK()$beta_hat_T
			} else if (self$estimate_type == "difference-in-means" & !private$isKK){
				private$compute_normal_based_inference_difference_in_means(estimate_only = TRUE)$beta_hat_T
			} else if (self$estimate_type == "OLS" & private$isKK){
				private$compute_normal_based_inference_ols_KK()$beta_hat_T
			} else if (self$estimate_type == "OLS" & !private$isKK){
				private$compute_normal_based_inference_ols(estimate_only = TRUE)$beta_hat_T
			}
		},
		
		#' @description
		#' Computes either the classic different-in-means estimate of the additive treatment effect, 
		#' i.e. ybar_T - ybar_C or the OLS estimate of the additive treatment effect linearly i.e. 
		#' the treatment different adjusted linearly for the p covariates.
		#' @param nsim_exact_test		The number of randomization vectors to use in the randomization test (ignored if \code{test_type}
		#' 								is not "randomization-exact"). The default is 501 providing pvalue resolution to a fifth of a percent.
		#' 
		#' @return 	The frequentist p-val for the test of nonzero treatment effect
		#' 
		#' @examples
		#' seq_des = SeqDesign$new(n = 6, p = 10, design = "CRD")
		#' seq_des$add_subject_to_experiment(c(1, 38, 142, 71, 5.3, 0, 0, 0, 1, 0))
		#' seq_des$add_subject_to_experiment(c(0, 27, 127, 60, 5.5, 0, 0, 0, 1, 0))
		#' seq_des$add_subject_to_experiment(c(1, 42, 169, 74, 5.1, 0, 1, 0, 0, 0))
		#' seq_des$add_subject_to_experiment(c(0, 59, 105, 62, 5.9, 0, 0, 0, 1, 0))
		#' seq_des$add_subject_to_experiment(c(1, 32, 186, 66, 5.6, 1, 0, 0, 0, 0))
		#' seq_des$add_subject_to_experiment(c(1, 37, 178, 75, 6.5, 0, 0, 0, 0, 1))
		#' seq_des$add_all_subject_responses(c(4.71, 1.23, 4.78, 6.11, 5.95, 8.43))
		#' 
		#' seq_des_inf = SeqDesignInference$new(seq_des)
		#' seq_des_inf$compute_pval_for_no_treatment_effect()
		#' 		
		compute_pval_for_no_treatment_effect = function(nsim_exact_test = 501){
			if (self$estimate_type == "difference-in-means" & self$test_type == "normal-based" & private$isKK){
				private$compute_normal_based_inference_difference_in_means_KK()$p_val
			} else if (self$estimate_type == "difference-in-means" & self$test_type == "normal-based" & !private$isKK){
				private$compute_normal_based_inference_difference_in_means()$p_val
			} else if (self$estimate_type == "OLS" & self$test_type == "normal-based" & private$isKK){
				private$compute_normal_based_inference_ols_KK()$p_val
			} else if (self$estimate_type == "OLS" & self$test_type == "normal-based" & !private$isKK){
				private$compute_normal_based_inference_ols()$p_val
			} else if (self$test_type == "randomization-exact"){
				assertCount(nsim_exact_test, positive = TRUE)
				if (is.null(private$rand_inf_cache)){
					private$rand_inf_cache = private$conduct_randomization_inference(nsim_exact_test)
				}
				private$rand_inf_cache$p_val
			}
		},
		
		#' @description
		#' Computes many randomization samples of either the classic different-in-means estimate of 
		#' the additive treatment effect, i.e. ybar_T - ybar_C or the OLS estimate of the additive 
		#' treatment effect linearly i.e. the treatment different adjusted linearly for the p covariates.
		#' This function is useful if you wish to run your own, custom hypothesis tests.
		#' 
		#' @param nsim_exact_test		The number of randomization vectors. 
		#' 								The default is 501 providing pvalue resolution to a fifth of a percent.
		#' 
		#' @return 	The \code{nsim_exact_test} samples of the treatment effect under the null hypothesis of no treatment effect
		#' 			where each sample is estimated from a different assignment vector for the prespecified design
		#' 
		#' @examples
		#' seq_des = SeqDesign$new(n = 6, p = 10, design = "CRD")
		#' seq_des$add_subject_to_experiment(c(1, 38, 142, 71, 5.3, 0, 0, 0, 1, 0))
		#' seq_des$add_subject_to_experiment(c(0, 27, 127, 60, 5.5, 0, 0, 0, 1, 0))
		#' seq_des$add_subject_to_experiment(c(1, 42, 169, 74, 5.1, 0, 1, 0, 0, 0))
		#' seq_des$add_subject_to_experiment(c(0, 59, 105, 62, 5.9, 0, 0, 0, 1, 0))
		#' seq_des$add_subject_to_experiment(c(1, 32, 186, 66, 5.6, 1, 0, 0, 0, 0))
		#' seq_des$add_subject_to_experiment(c(1, 37, 178, 75, 6.5, 0, 0, 0, 0, 1))
		#' seq_des$add_all_subject_responses(c(4.71, 1.23, 4.78, 6.11, 5.95, 8.43))
		#' 
		#' seq_des_inf = SeqDesignInference$new(seq_des)
		#' samps = seq_des_inf$randomization_inference_samples_for_no_treatment_effect()
		#' summary(samps)	
		#' 
		randomization_inference_samples_for_no_treatment_effect = function(nsim_exact_test = 501){
			assertTRUE(self$test_type == "randomization-exact")
			assertCount(nsim_exact_test, positive = TRUE)
			private$conduct_randomization_inference(nsim_exact_test)$b_T_sims
		},
		
		#' @description
		#' Computes either a:
		#' 
		#' 1. classic frequentist confidence interval (CI) of the additive treatment effect
		#' employing the normal theory approximation for both the
		#' (a) difference in means estimator i.e. [ybar_T - ybar_C +/- t_{alpha/2, n_T + n_C - 2} s_{ybar_T - ybar_C}] or
		#' (b) the OLS estimator i.e. [beta_hat_T +/- t_{alpha/2, n + p - 2} s_{beta_hat_T}]
		#' where the z approximation is employed in lieu of the t is the design is a KK design or
		#' 
		#' 2. a randomization-based CI of an additive shift effect of the potential outcomes under treatment and control
		#' by an inversion of the randomization test at level alpha (this feature is incomplete).
		#' 
		#' @param alpha					The confidence level in the computed confidence interval is 1 - \code{alpha}. The default is 0.05.
		#' @param nsim_exact_test		The number of randomization vectors. 
		#' 								The default is 1000 providing good resolutions to confidence intervals.
		#' 
		#' @return 	A 1 - alpha sized frequentist confidence interval for the treatment effect
		#' 
		#' @examples
		#' seq_des = SeqDesign$new(n = 6, p = 10, design = "CRD")
		#' seq_des$add_subject_to_experiment(c(1, 38, 142, 71, 5.3, 0, 0, 0, 1, 0))
		#' seq_des$add_subject_to_experiment(c(0, 27, 127, 60, 5.5, 0, 0, 0, 1, 0))
		#' seq_des$add_subject_to_experiment(c(1, 42, 169, 74, 5.1, 0, 1, 0, 0, 0))
		#' seq_des$add_subject_to_experiment(c(0, 59, 105, 62, 5.9, 0, 0, 0, 1, 0))
		#' seq_des$add_subject_to_experiment(c(1, 32, 186, 66, 5.6, 1, 0, 0, 0, 0))
		#' seq_des$add_subject_to_experiment(c(1, 37, 178, 75, 6.5, 0, 0, 0, 0, 1))
		#' seq_des$add_all_subject_responses(c(4.71, 1.23, 4.78, 6.11, 5.95, 8.43))
		#' 
		#' seq_des_inf = SeqDesignInference$new(seq_des, test_type = "normal-based")
		#' seq_des_inf$compute_confidence_interval()
		#' 		
		compute_confidence_interval = function(alpha = 0.05, nsim_exact_test = 501){
			assertNumeric(alpha, lower = .Machine$double.xmin, upper = 1 - .Machine$double.xmin)
			assertCount(nsim_exact_test, positive = TRUE)
			
			if (self$estimate_type == "difference-in-means" & self$test_type == "normal-based" & private$isKK){
				private$common_normal_based_ci(private$compute_normal_based_inference_difference_in_means_KK(), alpha, use_Z = TRUE)
			} else if (self$estimate_type == "difference-in-means" & self$test_type == "normal-based" & !private$isKK){
				private$common_normal_based_ci(private$compute_normal_based_inference_difference_in_means(), alpha, use_Z = FALSE)
			} else if (self$estimate_type == "OLS" & self$test_type == "normal-based" & private$isKK){
				private$common_normal_based_ci(private$compute_normal_based_inference_ols_KK(), alpha, use_Z = TRUE)	
			} else if (self$estimate_type == "OLS" & self$test_type == "normal-based" & !private$isKK){
				private$common_normal_based_ci(private$compute_normal_based_inference_ols(), alpha, use_Z = FALSE)
			} else if (self$estimate_type == "difference-in-means" & self$test_type == "randomization-exact"){
				stop("This computation is not implemented yet.")
				if (is.null(private$rand_inf_cache)){
					private$rand_inf_cache = private$conduct_randomization_inference(nsim_exact_test)
				} 
				#TODO
			} else if (self$estimate_type == "OLS" & self$test_type == "randomization-exact"){
				stop("This computation is not implemented yet.")
				if (is.null(private$rand_inf_cache)){
					private$rand_inf_cache = private$conduct_randomization_inference(nsim_exact_test)
				} 
				#TODO
			}
		}

	),
	private = list(
		seq_des_obj = NULL,
		isKK = NULL,
		rand_inf_cache = NULL,
		
		############# TESTING HELPER FUNCTIONS
			
		conduct_randomization_inference = function(nsim_exact_test, num_cores){
			cl = makeCluster(self$num_cores)
			#ensure to get all variables copied into scope (saves cycles)
			X = private$seq_des_obj$X
			y = private$seq_des_obj$y
			n = private$seq_des_obj$.__enclos_env__$private$n
			p = private$seq_des_obj$.__enclos_env__$private$p
			design = private$seq_des_obj$design
			estimate_type = self$estimate_type
			if (self$num_cores == 1){
				#easier on the OS I think...
				b_T_sims = array(NA, nsim_exact_test)
				for (r in 1 : nsim_exact_test){
					#cat("		r =", r, "/", nsim_exact_test, "\n")
					#do a fake run of the experiment to come up with a different w based on the design
					seq_des_r = SeqDesign$new(n = n, p = p, design = design, verbose = FALSE)
					for (i in 1 : n){
						seq_des_r$add_subject_to_experiment(X[i, ])
						seq_des_r$add_current_subject_response(y[i])
					}				
					b_T_sims[r] = SeqDesignInference$new(seq_des_r, estimate_type = estimate_type, verbose = FALSE)$compute_treatment_estimate()
				}
			} else {
				registerDoParallel(cl)			
				#now copy them to each core's memory
				clusterExport(cl, list("X", "y", "n", "p", "design", "estimate_type"), envir = environment())
				#now do the parallelization
				b_T_sims = foreach(r = 1 : nsim_exact_test, .inorder = FALSE, .combine = c) %dopar% {
				#cat("		r =", r, "/", nsim_exact_test, "\n")
					#do a fake run of the experiment to come up with a different w based on the design
					seq_des_r = SeqDesign$new(n = n, p = p, design = design, verbose = FALSE)
					for (i in 1 : n){
						seq_des_r$add_subject_to_experiment(X[i, ])
						seq_des_r$add_current_subject_response(y[i])
					}				
					SeqDesignInference$new(seq_des_r, estimate_type = estimate_type, verbose = FALSE)$compute_treatment_estimate()				
				}
				stopCluster(cl)
			}
			
			list(
				b_T_sims = b_T_sims,
				#where does the estimate from the actual w_experimental fall among the H0 estimates?
				p_val = sum(abs(self$compute_treatment_estimate()) < abs(b_T_sims)) / nsim_exact_test
			)		
		},
		
		############# CONFIDENCE INTERVALS HELPER FUNCTIONS
		
		common_normal_based_ci = function(inference_obj, alpha, use_Z){
			qu = 1 - alpha / 2
			moe = ifelse(use_Z, qnorm(qu), qt(qu, inference_obj$t_df)) * inference_obj$s_beta_hat_T
			inference_obj$beta_hat_T + c(-moe, moe)
		},
		
		######### ALL INFERENCE HELPER FUNCTIONS	
		
		compute_normal_based_inference_difference_in_means = function(estimate_only = FALSE){
			yTs = private$seq_des_obj$y[private$seq_des_obj$w == 1]
			yCs = private$seq_des_obj$y[private$seq_des_obj$w == 0]
			beta_hat_T = mean(yTs) - mean(yCs)
			if (estimate_only){
				list(beta_hat_T = beta_hat_T)
			} else {
				nT = length(yTs)
				nC = length(yCs)
				s_beta_hat_T = sqrt(var(yTs) / nT + var(yCs) / nC)
				t_df = nT + nC - 2
				list(
					beta_hat_T = beta_hat_T,
					s_beta_hat_T = s_beta_hat_T,
					t_df = t_df,
					p_val = 2 * pt(-abs(beta_hat_T / s_beta_hat_T), t_df)
				)				
			}
		},
		
		compute_normal_based_inference_difference_in_means_KK = function(){
			KKstats = private$compute_post_matching_data_KK()
			#sometimes the reservoir just isn't large enough
			if (KKstats$nRT <= 1 || KKstats$nRC <= 1){
				KKstats$beta_hat_T = KKstats$d_bar	
				KKstats$s_beta_hat_T = sqrt(KKstats$ssqD_bar)
			} else if (KKstats$m == 0){ #sometimes there's no matches
				KKstats$beta_hat_T = KKstats$r_bar		
				KKstats$s_beta_hat_T = sqrt(KKstats$ssqR)		
			} else {
				KKstats$beta_hat_T = KKstats$w_star * KKstats$d_bar + (1 - KKstats$w_star) * KKstats$r_bar #proper weighting
				KKstats$s_beta_hat_T = sqrt(KKstats$ssqR * KKstats$ssqD_bar / (KKstats$ssqR + KKstats$ssqD_bar))
			}
			KKstats$p_val = 2 * (pnorm(-abs(KKstats$beta_hat_T / KKstats$s_beta_hat_T))) #approximate by using real Z
			KKstats
		},
		
		compute_normal_based_inference_ols = function(estimate_only = FALSE){
			if (estimate_only){
				list(
					beta_hat_T = lm.fit(
						cbind(1, private$seq_des_obj$w, private$seq_des_obj$X), 
						private$seq_des_obj$y
					)$coefficients[2]
				)
			} else {
				mod_results = coef(summary(lm(private$seq_des_obj$y ~ ., data.frame(cbind(private$seq_des_obj$w, private$seq_des_obj$X)))))
				list(
					beta_hat_T = mod_results[2, 1],
					s_beta_hat_T = mod_results[2, 2],
					t_df = private$seq_des_obj$.__enclos_env__$private$n - private$seq_des_obj$.__enclos_env__$private$p - 1,
					p_val = mod_results[2, 4]
				)				
			}			
		},
		
		compute_normal_based_inference_ols_KK = function(){
			KKstats = private$compute_post_matching_data_KK()
			
			if (KKstats$nRT <= 2 || KKstats$nRC <= 2 || (KKstats$nRT + KKstats$nRC <= private$seq_des_obj$.__enclos_env__$private$p + 2)){
				coefs_matched = coef(summary(lm(KKstats$y_matched_diffs ~ KKstats$X_matched_diffs)))
				
				KKstats$beta_hat_T = coefs_matched[1, 1]
				KKstats$s_beta_hat_T = coefs_matched[1, 2]
				KKstats$p_val = coefs_matched[1, 4]
				
			#and sometimes there's no matches	
			} else if (KKstats$m == 0){			
				coefs_reservoir = coef(summary(lm(KKstats$y_reservoir ~ cbind(KKstats$w_reservoir, KKstats$X_reservoir))))		
				KKstats$beta_hat_T = coefs_reservoir[2, 1]
				KKstats$s_beta_hat_T = coefs[2, 2]
				KKstats$p_val = coefs[2, 4]
				
			#but most of the time... we have matches and a nice-sized reservoir
			} else {
				#compute estimator from matched pairs by regression
				coefs_matched = coef(summary(lm(KKstats$y_matched_diffs ~ KKstats$X_matched_diffs)))
				beta_match_regression = coefs_matched[1, 1]
				ssqd_match_regression = coefs_matched[1, 2]^2 #lin mod returns SE not VAR, so square it				
				
				#compute estimator reservoir sample std error
				coefs_reservoir = coef(summary(lm(KKstats$y_reservoir ~ cbind(KKstats$w_reservoir, KKstats$X_reservoir))))
				beta_reservoir_regression = coefs_reservoir[2, 1]
				ssqd_reservoir_regression = coefs_reservoir[2, 2]^2 #lin mod returns SE not VAR, so square it
				
				w_star = ssqd_reservoir_regression / (ssqd_reservoir_regression + ssqd_match_regression) #just a convenience for faster runtime	
				KKstats$beta_hat_T = w_star * beta_match_regression + (1 - w_star) * beta_reservoir_regression #proper weighting
				KKstats$s_beta_hat_T = sqrt(ssqd_match_regression * ssqd_reservoir_regression / (ssqd_match_regression + ssqd_reservoir_regression)) #analagous eq's
				KKstats$p_val = 2 * (pnorm(-abs(KKstats$beta_hat_T / KKstats$s_beta_hat_T))) #approximate by using N(0, 1) distribution
			}
			KKstats
		},
		
		compute_post_matching_data_KK = function(){			
			#get matched data
			match_indic = private$seq_des_obj$.__enclos_env__$private$match_indic
			m = max(match_indic)
			y_matched_diffs = array(NA, m)
			X_matched_diffs = matrix(NA, nrow = m, ncol = ncol(private$seq_des_obj$X))
			if (m > 0){
				for (match_id in 1 : m){ #we want to just calculate the diffs inside matches and ignore the reservoir
					yT = private$seq_des_obj$y[private$seq_des_obj$w == 1 & match_indic == match_id]
					yC = private$seq_des_obj$y[private$seq_des_obj$w == 0 & match_indic == match_id]
					y_matched_diffs[match_id] = yT - yC
					
					xmT = private$seq_des_obj$X[private$seq_des_obj$w == 1 & match_indic == match_id, ]
					xmC = private$seq_des_obj$X[private$seq_des_obj$w == 0 & match_indic == match_id, ]
					X_matched_diffs[match_id, ] = xmT - xmC
				}
			}			
			
			#get reservoir data
			X_reservoir = 	private$seq_des_obj$X[match_indic == 0, ]
			y_reservoir = 	private$seq_des_obj$y[match_indic == 0]
			w_reservoir = 	private$seq_des_obj$w[match_indic == 0]
			y_reservoir_T = y_reservoir[w_reservoir == 1] #get the reservoir responses from the treatment
			y_reservoir_C = y_reservoir[w_reservoir == 0] #get the reservoir responses from the control
			r_bar = mean(y_reservoir_T) - mean(y_reservoir_C) #compute the classic estimator from the reservoir: ybar_T - ybar_C
			
			#get reservoir sample sizes
			nRT = length(y_reservoir_T) #how many treatment observations are there in the reservoir?
			nRC = length(y_reservoir_C) #how many control observations are there in the reservoir?
			nR = nRT + nRC #how many observations are there in the reservoir?
			
			ssqR = (var(y_reservoir_T) * (nRT - 1) + var(y_reservoir_C) * (nRC - 1)) / (nR - 2) * (1 / nRT + 1 / nRC)
			ssqD_bar = var(y_matched_diffs) / m
			
			w_star = ssqR / (ssqR + ssqD_bar)
			
			list(
				X_matched_diffs = X_matched_diffs,
				y_matched_diffs = y_matched_diffs,
				X_reservoir = X_reservoir,
				y_reservoir = y_reservoir,
				w_reservoir = w_reservoir,
				nRT = nRT,
				nRC = nRC,
				m = m,
				d_bar = mean(y_matched_diffs),
				ssqD_bar = ssqD_bar,
				r_bar = r_bar,
				ssqR = ssqR,
				w_star = w_star
			)
		}
	)
)