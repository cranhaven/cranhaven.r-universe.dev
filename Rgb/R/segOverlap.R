# Merges overlapping segments in a data.frame
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

segOverlap = function(
		segTable,
		fun = list(unique.default, start=min, end=max),
		factorsAsIntegers = TRUE
		)
	{
	# Arg checks
	if(!is.data.frame(segTable))                              stop("'segTable' must be a data.frame")
	if(any(!c("chrom", "start", "end") %in% names(segTable))) stop("'segTable' must have 'chrom', 'start' and 'end' columns")
	
	if(nrow(segTable) > 1) {
		# Ordering
		if(is.factor(segTable$chrom) && isTRUE(factorsAsIntegers)) { segTable <- segTable[ order(as.integer(segTable$chrom), segTable$start) ,]
		} else                                                     { segTable <- segTable[ order(segTable$chrom, segTable$start) ,]
		}
		
		# Computing groups
		if(is.factor(segTable$chrom) && isTRUE(factorsAsIntegers)) { chroms <- as.integer(segTable$chrom)
		} else                                                     { chroms <- segTable$chrom
		}
		starts <- segTable$start
		ends <- segTable$end
		opened <- 1L
		group <- integer(nrow(segTable))
		group[1L] <- 1L
		for(i in 2:nrow(segTable)) {
			if(chroms[i] == chroms[opened] && starts[i] <= ends[opened]) { ends[opened] <- max(ends[c(i,opened)])
			} else                                                       { opened <- i
			}
			group[i] <- opened
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
			if(is.factor(segTable[[k]]) && isTRUE(factorsAsIntegers)) {
				# Factor
				lev <- levels(segTable[[k]])
				newSeg[[k]] <- tapply(X=as.integer(segTable[[k]]), INDEX=group, FUN=FUN)
				
				# From array to factor
				type <- class(newSeg[[k]][1L])
				if(type == "list") { stop("'fun' does not return single values for factor column \"", k, "\"")
				} else {
					newSeg[[k]] <- as.integer(newSeg[[k]])
					class(newSeg[[k]]) <- "factor"
					levels(newSeg[[k]]) <- lev
				}
			} else {
				# Other vector
				newSeg[[k]] <- tapply(X=segTable[[k]], INDEX=group, FUN=FUN)
				
				# From array to vector
				type <- class(newSeg[[k]][1L])
				if(type == "list") { stop("'fun' does not return single values for column \"", k, "\"")
				} else             { newSeg[[k]] <- as(newSeg[[k]], type)
				}
			}
		}
		
		# Back to data.frame
		attr(newSeg, "row.names") <- .set_row_names(length(unique(group)))
		attr(newSeg, "class") <- "data.frame"
	} else {
		# Nothing to do
		newSeg <- segTable
	}
	
	return(newSeg)
}
