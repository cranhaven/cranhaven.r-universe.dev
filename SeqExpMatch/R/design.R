#' A Sequential Design
#' 
#' @description
#' An R6 Class encapsulating the data and functionality for a sequential experimental design.
#' This class takes care of data intialization and sequential assignments. The class object
#' should be saved securely after each assignment e.g. on an encrypted cloud server.
#' 
#' @export
SeqDesign = R6::R6Class("SeqDesign",
		public = list(
				#' @field t			The current number of subjects in this sequential experiment (begins at zero).
				t = 0,
				#' @field design	The type of sequential experimental design (see constructor's documentation).
				design = NULL,
				#' @field X			A numeric matrix of subject data with number of rows n (the number of subjects) and number of 
				#' 					columns p (the number of characteristics measured for each subject). This matrix is filled in
				#' 					sequentially and thus will have data present for rows 1...t (i.e. the number of subjects in the
				#' 					experiment currently) but otherwise will be missing.
				X = NULL,
				#' @field y			A numeric vector of subject responses with number of entries n (the number of subjects). During
				#' 					the KK21 designs this must be filled in sequentially (similar to X) and will have data present for
				#' 					entries 1...t (i.e. the number of subjects in the experiment currently) but otherwise will be missing.
				#' 					For non-KK21 designs, this vector can be set at anytime (but must be set before inference is desired).
				y = NULL,
				#' @field w			A binary vector of subject assignments with number of entries n (the number of subjects). 
				#' 					This vector is filled in sequentially (similar to X) and will have assignments present for
				#' 					entries 1...t (i.e. the number of subjects in the experiment currently) but otherwise will be missing.			
				w = NULL,
				#' @field verbose	A flag that indicates whether messages should be displayed to the user
				verbose = NULL,
				#' 				
				#' @description
				#' Initialize a sequential experimental design
				#' 
				#' @param n 		Number of subjects fixed beforehand. A future version of this software will allow
				#' 					for sequential stopping and thus n will not need to be prespecified.
				#' @param p 		Number of characteristics measured for each subject. If measurement j are 
				#' 					categorical with L_j levels, you must select a reference level and convert this information
				#' 					to L_j-1 dummies. Thus p := # of numeric variables + sum_j (L_j - 1).
				#' @param design	The type of sequential experimental design. This must be one of the following
				#' 					"CRD" for the completely randomized design / Bernoulli design, 
				#' 					"BCRD" for the balanaced completely randomized design with n/2 T's and n/2 C's,
				#' 					"Efron" for Efron's (1971) Biased Coin Design
				#' 					"Atkinson" for Atkinson's (1982) Covariate-Adjusted Biased Coin Design
				#' 					"KK14" for Kapelner and Krieger's (2014) Covariate-Adjusted Matching on the Fly Design
				#' 					"KK21" for Kapelner and Krieger's (2021) CARA Matching on the Fly with Differential Covariate Weights Design
				#' 					"KK21stepwise" for Kapelner and Krieger's (2021) CARA Matching on the Fly with Differential Covariate Weights Stepwise Design
				#' @param verbose	A flag indicating whether messages should be displayed to the user. Default is \code{TRUE}.
				#' @param ...		Design-specific parameters:
				#' 					"Efron" requires "weighted_coin_prob" which is the probability of the weighted coin for assignment. If unspecified, default is 2/3.
				#' 					All "KK" designs require "lambda", the quantile cutoff of the subject distance distribution for determining matches. If unspecified, default is 10%.
				#' 					All "KK" designs require "t_0_pct", the percentage of total sample size n where matching begins. If unspecified, default is 35%.
				#' 					All "KK21" designs further require "num_boot" which is the number of bootstrap samples taken to approximate the subject-distance distribution. If unspecified, default is 500.
				#' 
				#' @return A new `SeqDesign` object.
				#' 
				#' @examples
				#' seq_des = SeqDesign$new(n = 100, p = 10, design = "KK21stepwise")
				#'  
				initialize = function(n, p, design, verbose = TRUE, ...) {
					assertCount(n, positive = TRUE)
					assertCount(p, positive = TRUE)
					assertChoice(design, c("CRD", "BCRD", "Efron", "Atkinson", "KK14", "KK21", "KK21stepwise"))
					assertFlag(verbose)
					
					if (n %% 2 != 0 & design == "BCRD"){
						stop("Design BCRD requires an even number of subjects.")
					}
					private$n = n
					private$p = p
					self$design = design
					self$verbose = verbose
					
					#create data derived from n, p
					self$X = matrix(NA, nrow = n, ncol = p)
					self$y = array(NA, n)
					self$w = array(NA, n)
					
					#now deal with design-specific hyperparameters
					other_params = list(...)
					if (design == "Efron"){
						if (is.null(other_params$weighted_coin_prob)){
							private$weighted_coin_prob = 2 / 3
						} else {
							assertNumeric(other_params$weighted_coin_prob, lower = 0, upper = 1)
							private$weighted_coin_prob = other_params$weighted_coin_prob
						}
					}
					if (grepl("KK", design)){
						private$isKK = TRUE
						private$match_indic = array(NA, n)
						if (is.null(other_params$lambda)){
							private$lambda = 0.1
						} else {
							assertNumeric(other_params$lambda, lower = 0, upper = 1)
							private$lambda = other_params$lambda
						}
						if (is.null(other_params$t_0_pct)){
							private$t_0 = round(0.35 * n)
						} else {
							assertNumeric(other_params$t_0_pct, lower = (p + 2) / n, upper = 1)
							private$t_0 = round(other_params$t_0_pct * n)
						}
						if (grepl("KK21", design)){
							private$isKK21 = TRUE
							self$y = array(NA, n)
							if (is.null(other_params$num_boot)){
								private$num_boot = 500
							} else {
								assertCount(other_params$num_boot, positive = TRUE)
								private$num_boot = other_params$num_boot
							}						
						}
					}
					if (self$verbose){
						cat(paste0("Intialized a ", design, " design for ", n, " subjects each with ", p, " characteristics measured per subject.\n"))
					}					
				},
				
				#' @description
				#' Add subject-specific measurements for the next subject entrant
				#' 
				#' @param x_vec A p-length numeric vector
				#' 
				#' @examples
				#' seq_des = SeqDesign$new(n = 100, p = 10, design = "CRD")
				#' seq_des$add_subject_to_experiment(c(1, 38, 142, 71, 5.3, 0, 0, 0, 1, 0))
				#' 
				add_subject_to_experiment = function(x_vec) {					
					assertNumeric(x_vec, len = private$p, any.missing = FALSE)
					
					if (private$isKK21){
						#we cannot add subject covariates unless the previous y was added
						if (self$t > 0){
							if (is.na(self$y[self$t])){
								stop("For KK21 designs, you cannot add a new subject unless you have added the previous subject's response.\n")
							}
						}
					}
					if (self$t + 1 > private$n){
						stop(paste("You cannot add any new subjects as all n =", private$n, "have already been added."))
					}
					
					#iterate t
					self$t = self$t + 1
					#add subject to data frame
					self$X[self$t, ] = x_vec
					#now make the assignment
					self$w[self$t] = private[[paste0("assign_wt_", self$design)]]() #do.call(what = paste0("assign_wt_", self$design), args = list())
				},
				
				#' @description
				#' Prints the current assignment to screen. Should be called after \code{add_subject_to_experiment}.
				#' 
				#' @examples
				#' seq_des = SeqDesign$new(n = 100, p = 10, design = "CRD")
				#' 
				#' seq_des$add_subject_to_experiment(c(1, 38, 142, 71, 5.3, 0, 0, 0, 1, 0))
				#' seq_des$print_current_subject_assignment()
				#' 
				print_current_subject_assignment = function(){
					cat("Subject number", self$t, "is assigned to", ifelse(self$w[self$t] == 1, "TREATMENT", "CONTROL"), "via design", self$design, "\n")
				},
				
				#' @description
				#' For CARA designs, add subject response for the current subject entrant
				#' 
				#' @param y The response as a numeric scalar
				#' @examples
				#' seq_des = SeqDesign$new(n = 100, p = 10, design = "KK21")
				#' 
				#' seq_des$add_subject_to_experiment(c(1, 38, 142, 71, 5.3, 0, 0, 0, 1, 0))
				#' 
				#' seq_des$add_current_subject_response(4.71)
				#' 
				add_current_subject_response = function(y) {
					assertNumeric(y, len = 1, any.missing = FALSE)
					self$y[self$t] = y
				},
				
				#' @description
				#' For non-CARA designs, add all subject responses
				#' 
				#' @param y The responses as a numeric vector of length n
				#' @examples
				#' seq_des = SeqDesign$new(n = 6, p = 10, design = "CRD")
				#' 
				#' seq_des$add_subject_to_experiment(c(1, 38, 142, 71, 5.3, 0, 0, 0, 1, 0))
				#' seq_des$add_subject_to_experiment(c(0, 27, 127, 60, 5.5, 0, 0, 0, 1, 0))
				#' seq_des$add_subject_to_experiment(c(1, 42, 169, 74, 5.1, 0, 1, 0, 0, 0))
				#' seq_des$add_subject_to_experiment(c(0, 59, 105, 62, 5.9, 0, 0, 0, 1, 0))
				#' seq_des$add_subject_to_experiment(c(1, 32, 186, 66, 5.6, 1, 0, 0, 0, 0))
				#' seq_des$add_subject_to_experiment(c(1, 37, 178, 75, 6.5, 0, 0, 0, 0, 1))
				#' 
				#' seq_des$add_all_subject_responses(c(4.71, 1.23, 4.78, 6.11, 5.95, 8.43))
				#' 				
				add_all_subject_responses = function(y) {
					assertNumeric(y, len = private$n, any.missing = FALSE)
					self$y = y
				},
				
				#' @description
				#' For KK designs only, this returns a list with useful matching statistics.
				#' 
				#' @return 	A list with the following data: \code{num_matches}, \code{prop_subjects_matched}, 
				#' 			\code{num_subjects_remaining_in_reservoir}, \code{prop_subjects_remaining_in_reservoir}.
				#' 
				#' @examples
				#' seq_des = SeqDesign$new(n = 6, p = 10, design = "KK14")
				#' 
				#' seq_des$add_subject_to_experiment(c(1, 38, 142, 71, 5.3, 0, 0, 0, 1, 0))
				#' seq_des$add_subject_to_experiment(c(0, 27, 127, 60, 5.5, 0, 0, 0, 1, 0))
				#' seq_des$add_subject_to_experiment(c(1, 42, 169, 74, 5.1, 0, 1, 0, 0, 0))
				#' seq_des$add_subject_to_experiment(c(0, 59, 105, 62, 5.9, 0, 0, 0, 1, 0))
				#' seq_des$add_subject_to_experiment(c(1, 32, 186, 66, 5.6, 1, 0, 0, 0, 0))
				#' seq_des$add_subject_to_experiment(c(1, 37, 178, 75, 6.5, 0, 0, 0, 0, 1))
				#' 
				#' seq_des$add_all_subject_responses(c(4.71, 1.23, 4.78, 6.11, 5.95, 8.43))
				#' 
				#' seq_des$matching_statistics()
				#'
				matching_statistics = function(){
					if (!private$isKK){
						stop("Matching statistics are only available for KK designs")
					}
					if (self$t == 0){
						stop("The experiment has not begun yet")
					}
					num_subjects_matched = sum(private$match_indic != 0, na.rm = TRUE)
					num_subjects_remaining_in_reservoir = self$t - num_subjects_matched
					list(
						num_matches = num_subjects_matched / 2,
						prop_subjects_matched = num_subjects_matched / self$t,
						num_subjects_remaining_in_reservoir = num_subjects_remaining_in_reservoir,
						prop_subjects_remaining_in_reservoir = num_subjects_remaining_in_reservoir / self$t
					)					
				},
				
				#' @description
				#' Asserts if the experiment is completed (all n assignments are assigned
				#' in the w vector and all n responses in the y vector are recorded), i.e. throws 
				#' descriptive error if the experiment is incomplete.
				#' 
				#' @examples
				#' seq_des = SeqDesign$new(n = 6, p = 10, design = "CRD")
				#' seq_des$add_subject_to_experiment(c(1, 38, 142, 71, 5.3, 0, 0, 0, 1, 0))
				#' 
				#' #if run, it would throw an error since all of the covariate vectors are not yet recorded
				#' #seq_des$assert_experiment_completed() 
				#' 
				#' seq_des$add_subject_to_experiment(c(0, 27, 127, 60, 5.5, 0, 0, 0, 1, 0))
				#' seq_des$add_subject_to_experiment(c(1, 42, 169, 74, 5.1, 0, 1, 0, 0, 0))
				#' seq_des$add_subject_to_experiment(c(0, 59, 105, 62, 5.9, 0, 0, 0, 1, 0))
				#' seq_des$add_subject_to_experiment(c(1, 32, 186, 66, 5.6, 1, 0, 0, 0, 0))
				#' seq_des$add_subject_to_experiment(c(1, 37, 178, 75, 6.5, 0, 0, 0, 0, 1))
				#' 
				#' #if run, it would throw an error since the responses are not yet recorded
				#' #seq_des$assert_experiment_completed() 
				#' 
				#' seq_des$add_all_subject_responses(c(4.71, 1.23, 4.78, 6.11, 5.95, 8.43))
				#' 
				#' seq_des$assert_experiment_completed() #no response means the assert is true
				assert_experiment_completed = function(){
					if (private$all_assignments_not_yet_allocated()){
						stop("This experiment is incomplete as the assignments aren't all allocated yet.")
					}
					if (private$all_responses_not_yet_recorded()){
						stop("This experiment is incomplete as the responses aren't all recorded yet.")
					}
				},
				
				#' @description
				#' Checks if the experiment is completed (all n assignments are assigned
				#' in the w vector and all n responses in the y vector are recorded).
				#' 
				#' @return	\code{TRUE} if experiment is complete, \code{FALSE} otherwise.
				#' 
				#' @examples
				#' seq_des = SeqDesign$new(n = 6, p = 10, design = "CRD")
				#' seq_des$add_subject_to_experiment(c(1, 38, 142, 71, 5.3, 0, 0, 0, 1, 0))
				#' 
				#' #returns FALSE since all of the covariate vectors are not yet recorded
				#' seq_des$check_experiment_completed() 
				#' 
				#' seq_des$add_subject_to_experiment(c(0, 27, 127, 60, 5.5, 0, 0, 0, 1, 0))
				#' seq_des$add_subject_to_experiment(c(1, 42, 169, 74, 5.1, 0, 1, 0, 0, 0))
				#' seq_des$add_subject_to_experiment(c(0, 59, 105, 62, 5.9, 0, 0, 0, 1, 0))
				#' seq_des$add_subject_to_experiment(c(1, 32, 186, 66, 5.6, 1, 0, 0, 0, 0))
				#' seq_des$add_subject_to_experiment(c(1, 37, 178, 75, 6.5, 0, 0, 0, 0, 1))
				#' 
				#' #returns FALSE since the responses are not yet recorded
				#' seq_des$check_experiment_completed() 
				#' 
				#' seq_des$add_all_subject_responses(c(4.71, 1.23, 4.78, 6.11, 5.95, 8.43))
				#' 
				#' seq_des$check_experiment_completed() #returns TRUE
				#' 
				check_experiment_completed = function(){
					if (private$all_assignments_not_yet_allocated()){
						FALSE
					} else if (private$all_responses_not_yet_recorded()){
						FALSE
					} else {
						TRUE
					}
				}
		),
		
		
		private = list(
			n = NULL,
			p = NULL,
			
			#design specific parameters
			isKK = FALSE,
			isKK21 = FALSE,
			#Efron
			weighted_coin_prob = NULL,
			#KK parameters
			lambda = NULL,
			t_0 = NULL,
			match_indic = NULL,
			#KK21 parameters
			num_boot = NULL,
			
			all_assignments_not_yet_allocated = function(){
				self$t != private$n
			},
			
			all_responses_not_yet_recorded = function(){
				sum(!is.na(self$y)) != private$n
			},

			assign_wt_CRD = function(){
				rbinom(1, 1, 0.5)
			},
			
			assign_wt_BCRD = function(){
				n_over_two = private$n / 2
				nT = sum(self$w == 1, na.rm = TRUE)
				nC = sum(self$w == 0, na.rm = TRUE)
				sample(c(rep(1, n_over_two - nT), rep(0, n_over_two - nC)), 1)
			},
			
			assign_wt_Efron = function(){
				n_T = sum(private$w, na.rm = TRUE)
				n_C = private$n - n_T
				if (n_T > n_C){
					rbinom(1, 1, 1 - private$weighted_coin_prob)
				} else if (n_T < n_C){
					rbinom(1, 1, private$weighted_coin_prob)
				} else {
					private$assign_wt_CRD()
				}				
			},
			
			assign_wt_Atkinson = function(){
				#if it's too early in the trial or if all the assignments are the same, then randomize
				if (self$t <= private$p + 2 + 1 | length(unique(self$t)) == 1){
					private$assign_wt_CRD()
				} else {	
					t_minus_1 = self$t - 1
					tryCatch({
						#this matrix is [w | 1 | X]
						X_t_min_1 = cbind(self$w[1 : t_minus_1], 1, self$X[1 : t_minus_1, ])
						XtX = t(X_t_min_1) %*% X_t_min_1						
						M = t_minus_1 * solve(XtX)
						A = M[1, 2 : (private$p + 2)] %*% c(1, self$X[self$t, ]) 
						s_over_A_plus_one_sq = (M[1, 1] / A + 1)^2
						#assign the Atkinson weighted biased coin
						rbinom(1, 1, s_over_A_plus_one_sq / (s_over_A_plus_one_sq + 1))	
					}, error = function(e){ #sometimes XtX is still not invertible... so in that case... just randomize
						private$assign_wt_CRD()
					})
				}
			},
			
			assign_wt_KK14 = function(){
				alloc = NULL
				if (length(private$match_indic[private$match_indic == 0]) == 0 | self$t <= private$p | self$t <= private$t_0){
					#we're early, so randomize
					private$match_indic[self$t] = 0
					alloc = private$assign_wt_CRD()
				} else {
					# cat("else\n")
					#first calculate the threshold we're operating at
					xs_to_date = self$X[1 : self$t, ]
					S_xs_inv = solve(var(xs_to_date))
					F_crit =  qf(private$lambda, p, self$t - private$p)
					T_cutoff_sq = private$p * (private$n - 1) / (private$n - private$p) * F_crit
					#now iterate over all items in reservoir and take the minimum distance x
					reservoir_indices = which(private$match_indic == 0)
					x_star = self$X[self$t, ]
					sqd_distances = array(NA, length(reservoir_indices))
					for (r in 1 : length(reservoir_indices)){
						sqd_distances[r] = 1 / 2 * 
								t(x_star - self$X[reservoir_indices[r], ]) %*%
								S_xs_inv %*%
								(x_star - self$X[reservoir_indices[r], ])			
					}					
					#find minimum distance index
					min_sqd_dist_index = which(sqd_distances == min(sqd_distances))
					if (length(sqd_distances[min_sqd_dist_index]) > 1 || length(T_cutoff_sq) > 1){
						min_sqd_dist_index = min_sqd_dist_index[1] #if there's a tie, just take the first one
					}
					#if it's smaller than the threshold, we're in business: match it
					if (sqd_distances[min_sqd_dist_index] < T_cutoff_sq){
						match_num = max(private$match_indic, na.rm = TRUE) + 1
						private$match_indic[reservoir_indices[min_sqd_dist_index]] = match_num
						private$match_indic[self$t] = match_num
						#assign opposite
						alloc = 1 - self$w[reservoir_indices[min_sqd_dist_index]]
					} else { #otherwise, randomize and add it to the reservoir
						private$match_indic[self$t] = 0
						alloc = private$assign_wt_CRD()
					}
				}
				if (is.na(private$match_indic[self$t])){
					stop("no match data recorded")
				}
				alloc
			},
			
			assign_wt_KK21 = function(){
				alloc = NULL
				if (length(private$match_indic[private$match_indic == 0]) == 0 | self$t <= private$p | self$t <= private$t_0){
					#we're early, so randomize
					private$match_indic[self$t] = 0
					alloc = private$assign_wt_CRD()
				} else {
					#1) need to calculate the weights
					# (a) get old x's and y's (we assume that x's are orthogonal for now)
					xs_to_date = self$X[1 : (self$t - 1), ]
					ys_to_date = self$y[1 : (self$t - 1)]
					# (b) run simple correlations to get Rsq's and use them as the relative weights
					weights = array(NA, private$p)
					for (j in 1 : private$p){
						weights[j] = cor(xs_to_date[, j], ys_to_date)^2
					}
					# (c) now we need to scale them
					weights = weights / sum(weights)
					# cat("nsim", nsim, "t", t, "weights", weights, "\n")
					
					#2) now iterate over all items in reservoir and calculate the weighted sqd distiance vs new guy 
					reservoir_indices = which(private$match_indic == 0)
					x_star = self$X[self$t, ]
					weighted_sqd_distances = array(NA, length(reservoir_indices))
					for (r in 1 : length(reservoir_indices)){
						delta_x = x_star - self$X[reservoir_indices[r], ]
						weighted_sqd_distances[r] = delta_x^2 %*% weights			
					}
					#3) find minimum weighted sqd distiance index
					min_weighted_sqd_dist_index = which(weighted_sqd_distances == min(weighted_sqd_distances))
					
					#generate a cutoff for the weighted minimum distance squared based on bootstrap
					bootstrapped_weighted_sqd_distances = array(NA, private$num_boot)
					for (b in 1 : private$num_boot){
						two_xs  = self$X[sample.int(self$t, 2), ] #self$X[sample_int_ccrank(self$t, 2, rep(1, (self$t))), ] #
						delta_x = two_xs[1, ] - two_xs[2, ]
						bootstrapped_weighted_sqd_distances[b] = delta_x^2 %*% weights
					}
					
					min_weighted_dsqd_cutoff_sq = quantile(bootstrapped_weighted_sqd_distances, private$lambda)
					
					#5) Now, does the minimum make the cut?
					if (length(weighted_sqd_distances[min_weighted_sqd_dist_index]) > 1 || length(min_weighted_dsqd_cutoff_sq) > 1){
						min_weighted_sqd_dist_index = min_weighted_sqd_dist_index[1] #if there's a tie, just take the first one
					}
					#  (a) if it's smaller than the threshold, we're in business: match it
					if (weighted_sqd_distances[min_weighted_sqd_dist_index] < min_weighted_dsqd_cutoff_sq){
						match_num = max(private$match_indic, na.rm = TRUE) + 1
						private$match_indic[reservoir_indices[min_weighted_sqd_dist_index]] = match_num
						private$match_indic[self$t] = match_num
						#assign opposite
						alloc = 1 - self$w[reservoir_indices[min_weighted_sqd_dist_index]]
					# (b) otherwise, randomize and add it to the reservoir
					} else { 
						private$match_indic[self$t] = 0	
						alloc = private$assign_wt_CRD()
					}
				}
				if (is.na(private$match_indic[self$t])){
					stop("no match data recorded")
				}
				alloc
			},
			
			assign_wt_KK21stepwise = function(){
				alloc = NULL
				if (length(private$match_indic[private$match_indic == 0]) == 0 | self$t <= private$p | self$t <= private$t_0){
					#we're early, so randomize
					private$match_indic[self$t] = 0
					alloc = private$assign_wt_CRD()
				} else {				
					#1) need to calculate the weights
					# (a) get old x's and y's (we assume that x's are orthogonal for now) 
					t_minus_1 = self$t - 1
					xs_to_date = self$X[1 : self$t, ]
					ys_seen_previously = self$y[1 : t_minus_1]
					
					# (b) we need to standardize the xs - puts them all on equal footing
					xs_to_date = apply(xs_to_date, 2, scale)
					
					# (c) we need to standardize the ys
					ys_seen_previously = scale(ys_seen_previously)
					
					# (d) extract out the relevant time portion and adjust for the effect of betaT for the eventual rand. test
					ys_seen_previously_adj_for_trt = lm.fit(as.matrix(self$w[1 : t_minus_1]), ys_seen_previously)$residuals
					xs_seen_previously = data.matrix(xs_to_date[1 : t_minus_1, ])
					
					# (e) we need to now run the stepwise procedure to get the weights
					weights = private$stepwise_weights(xs_seen_previously, ys_seen_previously_adj_for_trt)

					# cat("nsim", nsim, "t", t, "weights", weights, "\n")
					#2) now iterate over all items in reservoir and calculate the weighted sqd distiance vs new guy 
					reservoir_indices = which(private$match_indic == 0)
					x_star = xs_to_date[self$t, ]
					weighted_sqd_distances = array(NA, length(reservoir_indices))
					for (r in 1 : length(reservoir_indices)){
						delta_x = x_star - xs_to_date[reservoir_indices[r], ]
						weighted_sqd_distances[r] = delta_x^2 %*% weights			
					}
					#3) find minimum weighted sqd distiance index
					min_weighted_sqd_dist_index = which(weighted_sqd_distances == min(weighted_sqd_distances))
					
					#generate a cutoff for the weighted minimum distance squared based on bootstrap
					bootstrapped_weighted_sqd_distances = array(NA, private$num_boot)
					for (b in 1 : private$num_boot){
						two_xs  = self$X[sample.int(self$t, 2), ] #self$X[sample_int_ccrank(self$t, 2, rep(1, (self$t))), ] 
						delta_x = two_xs[1, ] - two_xs[2, ]
						bootstrapped_weighted_sqd_distances[b] = delta_x^2 %*% weights
					}
					
					min_weighted_dsqd_cutoff_sq = quantile(bootstrapped_weighted_sqd_distances, private$lambda)
					
					#5) Now, does the minimum make the cut?
					if (length(weighted_sqd_distances[min_weighted_sqd_dist_index]) > 1 || length(min_weighted_dsqd_cutoff_sq) > 1){
						min_weighted_sqd_dist_index = min_weighted_sqd_dist_index[1] #if there's a tie, just take the first one
					}
					#  (a) if it's smaller than the threshold, we're in business: match it
					if (weighted_sqd_distances[min_weighted_sqd_dist_index] < min_weighted_dsqd_cutoff_sq){
						match_num = max(private$match_indic, na.rm = TRUE) + 1
						private$match_indic[reservoir_indices[min_weighted_sqd_dist_index]] = match_num
						private$match_indic[self$t] = match_num
						alloc = 1 - self$w[reservoir_indices[min_weighted_sqd_dist_index]]
						# (b) otherwise, randomize and add it to the reservoir
					} else { 
						private$match_indic[self$t] = 0	
						alloc = private$assign_wt_CRD()	
					}
				}
				if (is.na(private$match_indic[self$t])){
					stop("no match data recorded")
				}
				alloc
			},
			
			stepwise_weights = function(X_stepwise, y_stepwise){	
				p = ncol(X_stepwise)
				weights = array(NA, p)
				
				j_droppeds = c()
				#initialize cols to be all cols
				cols = setdiff(1 : p, j_droppeds)
				
				#iteratively add all variables and extract weights for matching later
				repeat {
					#now get all the Rsqs and record the highest one i.e. the "best" covariate
					rsqs = private$find_rsqs(X_stepwise, y_stepwise, cols)
					j_max = which.max(rsqs)
					weights[j_max] = rsqs[j_max]
					j_droppeds = c(j_droppeds, j_max)
					
					#register the remaining cols
					cols = setdiff(1 : p, j_droppeds)
					#if there's none left, we jet
					if (length(cols) == 0){
						break
					}
					
					#we now need to adjust the other covariates for the "best" covariate
					x_best = X_stepwise[, j_max]
					X_st_w = cbind(1, x_best)
					X_st_w_tr = t(X_st_w)
					XtXinvXt_w_tr = X_st_w %*% solve(X_st_w_tr %*% X_st_w) %*% X_st_w_tr
					for (j in cols){
#						#predict x_j using x_best
#						mod = lm(X_stepwise[, j] ~ x_best)
#						#then set x_j equal to the residuals
#						X_stepwise[, j] = summary(mod)$residuals
						
						#predict x_j using x_best
						yhat_st_w_j = XtXinvXt_w_tr %*% X_stepwise[, j]
						#then set x_j equal to the residuals
						X_stepwise[, j] = X_stepwise[, j] - yhat_st_w_j
					}
				}
				
				#return normalized weights
				weights / sum(weights)
			},
			
			find_rsqs = function(X_stepwise, y_stepwise, cols){
				p = ncol(X_stepwise)
				Rsqs = array(NA, p)
				for (j in cols){
					Rsqs[j] = cor(y_stepwise, X_stepwise[, j])^2 #summary(lm(y_stepwise ~ X_stepwise[, j]))$r.squared
				}
				Rsqs
			}
		)
)