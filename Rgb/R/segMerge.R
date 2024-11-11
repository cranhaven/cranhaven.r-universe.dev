# Merges consecutive segments with identical values in a data.frame
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

segMerge = function(
		segTable,
		on = names(segTable),
		fun = list(unique, start=min, end=max),
		group = NULL
		)
	{
	# Arg checks
	if(!is.data.frame(segTable))                              stop("'segTable' must be a data.frame")
	if(any(!c("chrom", "start", "end") %in% names(segTable))) stop("'segTable' must have 'chrom', 'start' and 'end' columns")
	
	# 'on' fixed values
	if(!"chrom" %in% on) on <- c(on, "chrom")
	if("start" %in% on)  on <- setdiff(on, "start")
	if("end" %in% on)    on <- setdiff(on, "end")
	
	if(nrow(segTable) > 1) {
		# Ordering
		segTable <- segTable[ order(segTable$chrom, segTable$start) ,]
		
		# Attributes (removing cell names)
		attrList <- list()
		for(k in names(segTable)) {
			names(segTable[[k]]) <- NULL
			attrList[[k]] <- attributes(segTable[[k]])
		}
		
		# Computing groups [time consuming]
		if(is.null(group)) {
			group <- integer(nrow(segTable))
			j <- 0L
			for(i in 2:nrow(segTable)) {
				if(!identical(segTable[i-1L, on, drop=TRUE], segTable[i, on, drop=TRUE])) j <- j + 1L
				group[i] <- j
			}
		}
		
		# Grouping rows
		newSeg <- list()
		for(k in names(segTable)) {
			# Function to apply
			if(is.list(fun)) {
				if(k %in% names(fun)) {
					# Function dedicated to this column
					if(is.function(fun[[k]])) { FUN <- fun[[k]]
					} else                    { stop("fun[[", k, "]] is not a function")
					}
				} else if("" %in% names(fun)) {
					# Default function
					if(is.function(fun[[ match("", names(fun)) ]])) { FUN <- fun[[ match("", names(fun)) ]]
					} else                                          { stop("The unnamed element of fun is not a function")
					}
				} else { stop("'fun' must contain a function for each column, or at least a default one")
				}
			} else { stop("'fun' must be a list of functions")
			}

			# Apply
			newSeg[[k]] <- tapply(X=segTable[[k]], INDEX=group, FUN=FUN)
			
			# From array to vector
			type <- class(newSeg[[k]][1L])
			if(type == "list") { stop("'fun' does not return single values for column \"", k, "\"")
			} else             { newSeg[[k]] <- as(newSeg[[k]], type)
			}
		}
		
		# Retrieve attributes (factor levels ...)
		for(k in names(segTable)) attributes(newSeg[[k]]) <- attrList[[k]]
		
		# Back to data.frame
		attr(newSeg, "row.names") <- .set_row_names(j+1L)
		attr(newSeg, "class") <- "data.frame"
	} else {
		# Nothing to do
		newSeg <- segTable
	}
	
	return(newSeg)
}
