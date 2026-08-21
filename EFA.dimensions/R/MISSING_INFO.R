


MISSING_INFO <- function(data, verbose = TRUE) {
	
	count_na_func <- function(x) sum(is.na(x)) 
	
	num_case_NAs <- apply(data, 1, count_na_func)
	
	prop_case_NAs <- num_case_NAs / dim(data)[2]
	
	count_table <- table(num_case_NAs)
	
	N_cases <- as.numeric(count_table)
	
	N_missing <-  as.numeric(names(count_table))
	
	prop_table <- table(prop_case_NAs)
	
	Proportion <- as.numeric(names(prop_table))
	
	Cum_Proportion = cumsum(count_table) / sum(count_table)
	
	Cum_N = cumsum(count_table) 
	
	Output <- data.frame(N_cases = N_cases, N_missing = N_missing, Proportion = Proportion, 
	                     Cum_Proportion=Cum_Proportion, Cum_N=Cum_N)

	if (verbose) {	
		
		cat('\n\nMissing value statistics:')

		# total # of NAs
		totNAs <- sum(is.na(data))
		
		# number of rows (cases) with an NA
		nrowsNAs <- sum(apply(data, 1, anyNA))
				
		cat('\n\nThere are ', nrow(data), ' cases (rows) and ', ncol(data), 
		        ' variables (columns) in the data file.')

		cat('\n\nThere are ', nrowsNAs, ' cases with missing values, and ', 
		        totNAs, ' missing values in total.\n\n')		

	 	print(round(Output,2), print.gap=4, row.names = FALSE)			
	}
	
	return(invisible(Output)) 
}



