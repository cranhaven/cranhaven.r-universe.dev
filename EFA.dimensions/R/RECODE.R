


RECODE <- function(data, old = NULL, new = NULL, type = 'reverse', max_value = NULL,
                   real_min = NULL, real_max = NULL, new_min = NULL, new_max = NULL ) {
	
	# data <- MISSING_DROP(data)
	
	# numeric only notice
	if (!is.numeric(data))  message('\n Not all values of "data" are numeric. Expect errors.')

	data_recoded <- data

	# when old & new are specified
	if (!is.null(old) & !is.null(new)) {
			
		# display the recode requests	
		for (lupe in 1:length(old)) {
			
			if (length(old) != length(new)) 
				message('\nThe specified old and new values have different lengths. Expect errors.\n')
			
				message('\n', old[lupe], ' will be recoded to ', new[lupe] )
			
				data_recoded <- ifelse(data == old[lupe], new[lupe], data_recoded)							
		}
	}	

	# when type is specified
	if (!is.null(type) & is.null(old) & is.null(new)) {
		
		if (type == 'reverse') {
			
			if (is.null(max_value))  max_value <- max(data, na.rm = TRUE)

			data_recoded <- (min(data, na.rm = TRUE) + max(data, na.rm = TRUE)) - data			
		}

		if (type == 'new_range') {
			
			# Sometimes the items in a pool have different response option ranges, e.g., some
			# on a 5-pt scale and others on a 6-pt scale. This option changes the metric/range of
			# a specified item to a desired metric (e.g., so that scales scores based on
			# all of the items in the pool can be computed). This alters item scores and
			# the new item values may not be integers.
	
			# For each item response, compute the percent value on the real/used item, & then find
			# the corresponding value on the desired new item metric for the same percentage.

		  # check if the specified real_min & real_max are true for the data
		  if (min(data) < real_min) {
		    real_min <- min(data)
		    message('\nreal_min has been reset to the smallest value in the data: ', min(data))
		  }
		  if (max(data) > real_max) {
		    real_max <- max(data)
		    message('\nreal_max has been reset to the largest value in the data: ', max(data))
		  }
		  
			# get the percentage value for the old/real response options
			# percent <- (real_min:real_max - real_min) / (real_max - real_min)
			percent <- (seq (real_min, real_max, length.out = 500) - real_min) / (real_max - real_min)
			  
			# compute the corresponding new possible response option values 
			newscore <- (percent * (new_max - new_min)) + new_min
			  
			# now go through the items, converting the old/real responses to the new values
      #	data_recoded <- newscore[data]  # simple, but problem = doesn't work with 0s
			# data_recoded <- c(newscore, data)[match(data, c(percent, data))]  # just gives the indices
			data_percent <- data / real_max
			locs <- apply(as.matrix(data_percent), 1, function(x) which.min(abs( percent - x)))
			data_recoded <- newscore[locs]
			# head(newscore);  head(data_recoded)
  	}
	}
		
	return(invisible(data_recoded))
}	
	


