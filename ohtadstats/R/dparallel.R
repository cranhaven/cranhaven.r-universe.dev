#' Compute Ohta's D Statistics in a manner optimized for parallelization
#' 
#' 
#' Infers the comparisons that this instance of the function is supposed to perform given job_id and comparisons_per_job.
#' Returns the results of those comparisons to an SQL database.
#' 
#' 
#' @param data_set The data set that is to be analysed.
#' @param tot_maf Minimum minor allele frequency across the total population for a marker to be included in the analysis.
#' @param pop_maf Minimum minor allele frequency across a subpopulation for that subpopulation to be included in analysis.
#' @param comparisons_per_job The number of comparisons that each instance of dparallel will compute.
#' @param job_id A number indicating that this is the nth instance of this function.
#' @param outfile Prefix for the file name that results will be written to. May be a path. Do not include extension.
#' 
#' 
#' @examples 
#' \dontrun{
#' 
#' data(beissinger_data)
#' dparallel(data_set = beissinger_data,
#'                      comparisons_per_job = 300,
#'                      job_id = 1,
#'                      outfile = "beissinger_comparison")
#' 
#' }
#' @export
dparallel <- function(data_set, tot_maf = 0.1, pop_maf = 0.05, comparisons_per_job, job_id, outfile = "Ohta"){
	# data_set will need to be an rds that is loaded in
	comparisons <- matrix(NA, nrow = comparisons_per_job, ncol = 2)
	comparisons[1,] <- determinejob(r = job_id * comparisons_per_job - (comparisons_per_job - 1), n = ncol(data_set))
	for (i in 2:nrow(comparisons)){      # Fill in the rest of the comparison matrix.
		a <- comparisons[i-1,][1]
		b <- comparisons[i-1,][2] + 1
		if (b > ncol(data_set)){               # Resets a and b to the next position when b names the last locus
			b <- a + 1
			a <- a + 1
		}
		if (a > ncol(data_set)){               # Truncates the list of comparisons if it goes beyond the possible comparisons.
			comparisons <- stats::na.omit(comparisons)
			break
		}
		comparisons[i,] <- c(a,b)
	}
	results <- t(apply(comparisons, MARGIN = 1, dstat, data_set = data_set, tot_maf = tot_maf, pop_maf = pop_maf))
	results <- cbind(comparisons, results)
	colnames(results) <- c('Marker1', 'Marker2', 'nPops', 'D2it', 'D2is', 'D2st', 'Dp2st', 'Dp2is')
	#database <- DBI::dbConnect(drv = RSQLite::SQLite(), dbname = paste(outfile, '.sqlite', sep = ''))     #Open database connection
	#DBI::dbWriteTable(conn = database, name = "OhtasD", value = as.data.frame(results), append = TRUE)    #Dump results into the database
	#DBI::dbDisconnect(database)                                                                           #Disconnect from database
	utils::write.csv(results, paste(outfile, "_", as.character(job_id), ".csv", sep = ""), row.names = FALSE)
}
