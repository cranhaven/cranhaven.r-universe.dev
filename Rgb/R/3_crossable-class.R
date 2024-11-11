# Reference class for objects that can be crossed (compute genomic overlaps between content)
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

# A random object with a specific slice() method, and thus a cross() method based on it (virtual)
setRefClass(
	Class = "crossable",
	contains = c("sliceable", "VIRTUAL"),
	methods = list(
		
		### Inherits sliceable "slice" (virtual) method, but requires it to return a data.frame with at least "chrom", "start" and "end" columns
		
cross = function(annotation, colname=NULL, type=c("cover", "count", "cytoband"), fuzziness=0L, maxElements=30, location=c("chrom", "start", "end"), precision=2, quiet=TRUE) {
"Add a new column computed from overlaps with an other crossable object.
- annotation    : other crossable object to compute overlap with.
- colname       : single character value, the name of the new column to add to .self. If NULL or NA, the result will be returned rather than added to .self.
- type          : single character value, either :
                  'cover', to compute coverage of 'annotation' elements for each .self element
                  'count', to count 'annotation' elements overlapping each .self element
                  'cytoband', to get cytogenetic coordinates from a cytoband annotation track
                  an 'annotation' column name, to list 'annotation' elements overlapping each .self element
- fuzziness     : single integer value, to be added on each side of .self elements when computing overlaps.
- maxElements   : single integer value, when more overlaps are found, lists are replaced by counts. Can be NA to disable this behavior.
- location      : character vector, the 'chrom' / 'start' / 'end' .self columns to use for annotation.
- precision     : single integer value from 1 to 4, amount of digits to consider in banding (type='cytoband').
- quiet         : single logical value, wether to throw progression messages or not."
	
	# Checks
	if(!is(annotation, "crossable")) stop("'annotation' must be an other 'crossable' object")
	
	# Processing function
	if(identical(type, "cover")) {
		# Coverage
		newColumn <- double(.self$getRowCount())
		processing <- function(chrom, start, end) {
			slice <- annotation$slice(chrom, start-fuzziness, end+fuzziness)
			if(nrow(slice) > 0) { return(sum(pmin(slice$end, end) - pmax(slice$start, start)) / (end - start))
			} else              { return(0)
			}
		}
	} else if(identical(type, "count")) {
		# Element count
		newColumn <- integer(.self$getRowCount())
		processing <- function(chrom, start, end) {
			out <- annotation$size(chrom, start-fuzziness, end+fuzziness)
			return(out)
		}
	} else if(identical(type, "cytoband")) {
		# Cytogenetic coordinates
		newColumn <- character(.self$getRowCount())
		processing <- function(chrom, start, end) {
			# Coordinates of boundaries
			slice <- annotation$slice(chrom, start-fuzziness, end+fuzziness)
			slice <- slice[ c(1, nrow(slice)) , "name" ]
			
			# Band precision
			slice <- switch(
				as.character(as.integer(precision)),
				"1" = sub("[0-9](\\.[0-9]+)?$", "0", slice),
				"2" = sub("\\.[0-9]+$", "", slice),
				"3" = sub("(?<=\\.[0-9])[0-9]$", "", slice, perl=TRUE),
				"4" = slice,
				stop("Invalid precision (from 1 to 4)")
			)
			
			# Multiple bands
			slice <- unique(slice)
			if(length(slice) != 1) { 
				slice <- sub(sprintf("^%s", chrom), "", slice)
				slice <- sprintf("%s(%s)", chrom, paste(slice, collapse=";"))
			}
			
			return(slice)
		}
	} else if(is.character(type) && length(type) == 1 && type %in% annotation$getColNames()) {
		# Column concatenation
		newColumn <- character(.self$getRowCount())
		processing <- function(chrom, start, end) {
			slice <- annotation$slice(chrom, start-fuzziness, end+fuzziness)
			elements <- slice[[ type ]]
			if(!is.na(maxElements) && length(elements) > maxElements) { return(sprintf("(%i elements)", length(elements)))
			} else                                                    { return(paste(elements, collapse=", "))
			}
		}
	} else stop("Invalid 'type'")
	
	# Fast access to coordinates
	chroms <- as.character(.self$extract(, location[1]))
	starts <- .self$extract(, location[2])
	ends <- .self$extract(, location[3])
	
	# Filling
	n <- .self$getRowCount()
	i <- 0L
	if(!isTRUE(quiet)) message(i, "/", n)
	while(i < n) {
		i <- i + 1L
		if(!isTRUE(quiet) && i %% 1000 == 0) message(i, "/", n)
		newColumn[i] <- processing(chrom=chroms[i], start=starts[i], end=ends[i])
	}
	if(!isTRUE(quiet)) message(i, "/", n)
	
	# Output
	if(!is.null(colname) && !is.na(colname)) {
		.self$addColumn(content=newColumn, name=colname)
	} else {
		return(newColumn)
	}
}
		
	)
)
