# Reference class for data.frame-like objects
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

### expression() shortcut ###
e = expression

### R5 class definition ###
setRefClass(
	Class = "refTable",
	fields = list(
		rowCount = "integer",
		rowNames = "character",
		rowNamed = "logical",
		colCount = "integer",
		colNames = "character",
		colReferences = "character",
		colIterator = "integer",
		values = "environment"
	),
	methods = list(

addColumn = function(content, name, after=colCount) {
"Adds a column in the table
- content   : values to fill in the new column, as a vector.
- name      : name of the new column, as character.
- after     : where to add the column, as the index (numeric) or name (character) of the column on its left"
	
	# Position (0 is OK)
	if(length(after) !=1 || after != 0) {
		after <- (1:colCount)[ indexes(after, "column") ]
		if(length(after) != 1)  stop("'after' must be a single value")
	}
	
	# Name
	if(length(name) != 1)   stop("'name' must be a single value")
	if(!is.character(name)) stop("'name' must be a character")
	if(is.na(name))         stop("'name' must not be NA")
	if(name %in% colNames)  stop("'name' is already in use")
	
	# Content
	if(!is.atomic(content)) stop("'content' must be atomic")
	if(is.array(content))   stop("'content' must be a vector, not an array")
	if(rowCount == 0)                      { rowCount <<- length(content)
	} else if(length(content) != rowCount) { stop("'content' must have 'rowCount' elements")
	}
	
	# New reference
	colCount <<- colCount + 1L
	colIterator <<- colIterator + 1L
	colReferences <<- append(x=colReferences, values=as.character(colIterator), after=after)
	colNames <<- append(x=colNames, values=name, after=after)
	
	# Binding column name in values (for expression-based extraction)
	makeActiveBinding(
		sym = name,
		fun = eval(
			parse(text=sprintf("function(v) get(\"%s\")", colIterator)),
			envir = values
		),
		env = values
	)
	
	# Forget value names
	names(content) <- NULL
	
	# Filling
	values[[ as.character(colIterator) ]] <<- content
},

addDataFrame = function(dataFrame) {
"Adds a data.frame content to the refTable
- dataFrame   : the data to add."
	
	# Checks
	if(!is.data.frame(dataFrame)) stop("Input must be a data.frame")
	if(ncol(dataFrame) == 0)      invisible(TRUE)
	
	# Relocation
	invisible(.self$addList(dataFrame, row.names=row.names(dataFrame)))
},

addEmptyRows = function(amount, newNames) {
"Add rows filled with NA at the bottom of the table.
- amount     : single integer value, the amount of rows to add.
- newNames   : character vector, the names of the new rows. Ignored if the table is not row named."
	
	# Checks
	if(amount == 0)                        return(TRUE)
	if(!is.integer(amount) || amount < 1)  stop("'amount' must be a positive integer")
	if(isTRUE(rowNamed)) {
		if(!is.character(newNames))       stop("'newNames' must be a character vector")
		if(any(is.na(newNames)))          stop("'newNames' must not contain NA values")
		if(length(newNames) != amount)    stop("'newNames' must have 'amount' elements")
		if(any(newNames %in% rowNames))   stop("'newNames' contains row names already in use")
	} else {
		if(!missing(newNames))            warning("'newNames' was ignored as the table is not row named")
	}
	
	# Counter update
	rowCount <<- rowCount + amount
	
	# Row names update
	if(isTRUE(rowNamed)) {
		names(newNames) <- NULL
		rowNames <<- c(rowNames, newNames)
	}
	
	# Value update
	toAppend <- rep(NA, amount)
	for(colReference in colReferences) {
		content <- c(values[[ colReference ]], toAppend)
		if(is.factor(values[[ colReference ]])) attributes(content) <- attributes(values[[ colReference ]])
		values[[ colReference ]] <<- content
	}
},

addList = function(dataList, row.names) {
"Adds a list content to the refTable
- dataList    : the data to add.
- row.names   : character vector with the names of the enw rows."
	
	# Checks
	if(!is.list(dataList))       stop("Input must be a list")
	if(length(dataList) == 0)    invisible(TRUE)
	if(is.null(names(dataList))) stop("Input must have named elements")
	
	# Strict class check for existing columns
	for(k in intersect(names(dataList), .self$getColNames())) {
		if(!identical(class(dataList[[k]]), class(.self$extract(0,k))))                               stop("New values class does not match the existing column class for column \"", k, "\"")
		if(is.factor(dataList[[k]]) && !identical(levels(dataList[[k]]), levels(.self$extract(0,k)))) stop("New values levels do not match the existing column levels for column \"", k, "\"")
	}
	
	# Rows to add
	newRows <- max(sapply(dataList, length))
	oldRows <- .self$getRowCount()
	
	# Row coordinates
	if(.self$getRowCount() > 0) { rows <- .self$getRowCount() + 1:newRows
	} else                      { rows <- NULL
	}
	
	# Rows
	if(.self$rowNamed) {
		# Checks
		if(missing(row.names))                      stop("'row.names' must be provided for row named tables")
		if(length(row.names) != newRows)            stop("'row.names' must contain a value for every new row")
		if(any(row.names %in% .self$getRowNames())) stop("'row.names' must not contain names already in use")
		
		# New rows
		.self$addEmptyRows(newRows, row.names)
	} else {
		# New rows
		.self$addEmptyRows(newRows)
	}
	
	# Filling (recycling)
	for(colName in names(dataList)) {
		if(colName %in% .self$getColNames()) {
			# Fill existing column
			.self$fill(
				i = rows,
				j = colName,
				newValues = if(newRows == length(dataList[[colName]])) { dataList[[colName]] }
				            else                                       { rep(dataList[[colName]], length.out=newRows) }
			)
		} else {
			if(oldRows > 0) {
				# New column content, appending
				content <- c(
					rep(NA, length.out=oldRows),
					if(newRows == length(dataList[[colName]])) { dataList[[colName]] }
			          else                                       { rep(dataList[[colName]], length.out=newRows) }
				)
				if(is.factor(dataList[[colName]])) attributes(content) <- attributes(dataList[[colName]])
				
				# Update
				.self$addColumn(
					content = content,
					name = colName
				)
			} else {
				# New column in empty table
				.self$addColumn(
					name = colName,
					content = if(newRows == length(dataList[[colName]])) { dataList[[colName]] }
				               else                                       { rep(dataList[[colName]], length.out=newRows) }
				)
			}
		}
	}
	
	invisible(TRUE)
},

addVectors = function(..., row.names) {
"Adds vectors to the refTable
- ...   : named vectors to add."
	
	# Relocation
	invisible(.self$addList(list(...), row.names=row.names))
},

check = function(warn=TRUE) {
"Raises an error if the object is not valid, else returns TRUE"
	
	# Unique values
	if(length(rowCount) != 1)                                stop("'rowCount' must be a single value")
	if(length(colCount) != 1)                                stop("'colCount' must be a single value")
	if(length(rowNamed) != 1)                                stop("'rowNamed' must be a single value")
	if(length(colIterator) != 1)                             stop("'colIterator' must be a single value")
	
	# Rows
	if(isTRUE(rowNamed)) {
		if(length(rowNames) != rowCount)                     stop("'rowCount' and 'rowNames' does not match")
		if(any(duplicated(rowNames)))                        stop("All 'rowNames' must be unique")
	} else {
		if(length(rowNames) > 0)                             stop("'rowNames' must be empty if 'rowNamed' is FALSE")
	}
	
	# Columns
	if(length(colNames) != colCount)                         stop("'colCount' and 'colNames' does not match")
	if(length(colReferences) != colCount)                    stop("'colCount' and 'colReferences' does not match")
	if(length(ls(values)) != colCount*2)                     stop("'values' length and 'colCount' does not match")
	
	# Values
	k = 1
	while(k <= colCount) {
		if(!exists(colReferences[k], values))                stop("'colReferences' must all exist in 'values'")
		if(length(values[[ colReferences[k] ]]) != rowCount) stop("'values' element lengths and 'rowCount' does not match")
		k = k + 1
	}
	
	return(TRUE)
},

coerce = function(j=NULL, class, levels, ...) {
"Coerces a single column to a different class
- j        : column index (numeric) or name (character).
- class    : name of the class to coerce 'j' to.
- levels   : if 'class' is factor, the levels to use.
- ...      : further arguments to be passed to the 'as' method (for atomics) or function (for other classes)."
	
	# Reference
	j <- indexes(j, "column")
	
	# Single column
	colRefSubset <- colReferences[j]
	if(length(colRefSubset) != 1) stop("'j' must be defined and unique")
	
	# Coercion
	if(class == "factor") {
		if(missing(levels))        { values[[ colRefSubset ]] <<- factor(values[[ colRefSubset ]])
		} else                     { values[[ colRefSubset ]] <<- factor(values[[ colRefSubset ]], levels=levels)
		}
	} else if(class == "logical")   { values[[ colRefSubset ]] <<- as.logical(values[[ colRefSubset ]], ...)
	} else if(class == "integer")   { values[[ colRefSubset ]] <<- as.integer(values[[ colRefSubset ]], ...)
	} else if(class == "numeric")   { values[[ colRefSubset ]] <<- as.numeric(values[[ colRefSubset ]], ...)
	} else if(class == "double")    { values[[ colRefSubset ]] <<- as.double(values[[ colRefSubset ]], ...)
	} else if(class == "complex")   { values[[ colRefSubset ]] <<- as.complex(values[[ colRefSubset ]], ...)
	} else if(class == "character") { values[[ colRefSubset ]] <<- as.character(values[[ colRefSubset ]], ...)
	} else                          { values[[ colRefSubset ]] <<- as(object=values[[ colRefSubset ]], class=class, ...)
	}
},

colOrder = function(newOrder, na.last=TRUE, decreasing=FALSE) {
"Reorder the columns of the tables (duplication / subsetting are NOT handled)
- newOrder   : new order to apply, as an integer vector of character vector of column names."
	
	# Index conversion
	newOrder <- indexes(newOrder, type="column")
	
	# Security
	if(any(sort(newOrder) != 1:colCount)) stop("Column duplication and subsetting are not handled")
	
	# Apply
	colReferences <<- colReferences[ newOrder ]
	colNames <<- colNames[ newOrder ]
},

copy = function(shallow=FALSE) {
"Overloads 'envRefClass' copy method to force 'values' duplication in deep copy"
	
	if(shallow) {
		# Copy 'values' reference for shallow copy is fine
		return(callSuper(shallow=TRUE))
	} else {
		# Copy other fields
		value <- callSuper(shallow=FALSE)
		
		# New empty 'values' environment
		value$values <- new.env()
		
		# Copy content
		for(content in value$colReferences) {
			assign(content, get(content, envir=values), envir=value$values)
		}
		
		# Rebuild bindings
		for(i in 1:colCount) {
			makeActiveBinding(
				sym = value$colNames[i],
				fun = eval(
					parse(text=sprintf("function(v) get(\"%s\")", value$colReferences[i])),
					envir = value$values
				),
				env = value$values
			)
		}
		
		return(value)
	}
},

delColumns = function(targets) {
"Deletes a column from the table
- targets   : character vector, the name(s) of the column(s) to delete."
	
	# Arg checks
	if(!is.character(targets) || length(targets) == 0) stop("'targets' must be a non empty character vector")
	if(any(!targets %in% colNames))                    stop("'targets' must refer to existing column(s)")
	
	for(name in targets) {
		# Target position
		colPos = (colNames == name)
		
		# Binding and content
		rm(list=name, pos=values)
		rm(list=colReferences[ colPos ], pos=values)
		
		# Column dereferencement
		colReferences <<- colReferences[ !colPos ]
		colNames <<- colNames[ !colPos ]
		
		# Column count
		colCount <<- colCount - 1L
	}
},

erase = function() {
"Remove all the rows and columns in the table."
	
	# Empty environment
	rm(list=c(colReferences, colNames), envir=values)
	
	# Reset parameters
	rowCount <<- 0L
	rowNames <<- character(0)
	colCount <<- 0L
	colNames <<- character(0)
	colReferences <<- character(0)
	colIterator <<- 0L
},

extract = function(i=NULL, j=NULL, drop=TRUE, asObject=FALSE) {
"Extracts values into a data.frame or vector
- i          : row selection, see indexes() for further details.
- j          : column selection, see indexes() for further details.
- drop       : if TRUE and querying a single column, will return a vector instead of a data.frame.
- asObject   : if TRUE results will be served in the same class as the current object."
	
	# References
	i = indexes(i, "row")
	j = indexes(j, "column")
	
	colRefSubset <- colReferences[j]
	if(isTRUE(drop) && length(colRefSubset) == 1) {
		# Vector results
		if(is.null(i)) {
			results <- values[[ colRefSubset ]]
			if(rowNamed) names(results) <- rowNames
		} else {
			results <- values[[ colRefSubset ]][i]
			if(rowNamed) names(results) <- rowNames[i]
		}
	} else {
		# Empty data.frame
		results <- data.frame()
		if(rowNamed) {
			if(is.null(i)) attr(results, "row.names") <- rowNames
			else           attr(results, "row.names") <- rowNames[i]
		} else {
			if(is.null(i)) attr(results, "row.names") <- 1:rowCount
			else           attr(results, "row.names") <- (1:rowCount)[i]
		}
		
		# Filling data.frame
		if(is.null(j)) {
			if(is.null(i)) {
				# No subset
				k = 1
				while(k <= colCount) {
					results[[ colNames[k] ]] <- values[[ colReferences[k] ]]
					k = k + 1
				}
			} else {
				# Row subset
				k = 1
				while(k <= colCount) {
					results[[ colNames[k] ]] <- values[[ colReferences[k] ]][i]
					k = k + 1
				}
			}
		} else {
			if(is.null(i)) {
				# Col subset
				for(k in (1:colCount)[j]) results[[ colNames[k] ]] <- values[[ colReferences[k] ]]
			} else {
				# Both subset
				for(k in (1:colCount)[j]) results[[ colNames[k] ]] <- values[[ colReferences[k] ]][i]
			}
		}
	}
	
	if(isTRUE(asObject)) {
		# New object with same meta-data
		output <- new(class(.self))
		for(fieldName in .self$metaFields()) output[[fieldName]] <- .self$field(fieldName)
		output$addDataFrame(results)
		results <- output
	}
	
	return(results)
},

fill = function(i=NULL, j=NULL, newValues) {
"Replaces values in a single column
- i           : row indexes (numeric) or names (character). NULL or missing for all rows.
- j           : column index (numeric) or name (character).
- newValues   : vector of values to put in the object"
	
	# References
	i = indexes(i, "row")
	j = indexes(j, "column")
	
	# Single column
	colRefSubset <- colReferences[j]
	if(length(colRefSubset) != 1) stop("'j' must be defined and unique")
	
	# Strict class check
	if(!identical(class(newValues), class(values[[ colRefSubset ]])))                           stop("New values class does not match the existing column class")
	if(is.factor(newValues) && !identical(levels(newValues), levels(values[[ colRefSubset ]]))) stop("New values levels do not match the existing column levels")
	
	# Forget value names
	names(newValues) <- NULL
	
	# Replacing in a single column
	if(is.null(i)) {
		values[[ colRefSubset ]] <<- newValues
	} else {
		values[[ colRefSubset ]][i] <<- newValues
	}
},

getColCount = function() {
	"'colCount' field accessor."
	return(colCount)
},

getColNames = function() {
	"'colNames' field accessor."
	return(colNames)
},

getLevels = function(j=NULL) {
"Get levels of a factor column
- j   : column index (numeric) or name (character)."
	
	# Reference
	j <- indexes(j, "column")
	
	# Single column
	colRefSubset <- colReferences[j]
	if(length(colRefSubset) != 1) stop("'j' must be defined and unique")
	
	if(is.factor(values[[ colRefSubset ]])) {
		return(levels(values[[ colRefSubset ]]))
	} else stop("'j' must refer to a column of class 'factor'")
},

getRowCount = function() {
	"'rowCount' field accessor."
	return(rowCount)
},

getRowNames = function() {
	"'rowNames' field accessor."
	return(rowNames)
},

indexes = function(i, type=c("row", "column")) {
"Checks row or column references and return numeric indexes
- i   : reference to the rows or columns to select (NA not allowed), as :
- missing or NULL (all rows or columns)
- vector of numeric indexes to select
- vector of character indexes to select (if the dimension is named)
- vector of logical with TRUE on each value to select, FALSE otherwise
- expression object (as returned by e(...)), to be evaluated in the 'values' environment"
	
	# Pass through
	if(is.null(i)) {
		return(i)
	}
	if(is.numeric(i)) {
		if(any(is.na(i))) stop("NA reference not allowed")
		return(i)
	}
	if(is.logical(i)) {
		if(any(is.na(i))) stop("NA reference not allowed")
		return(i)
	}
	
	# Expression evaluation
	if(is.expression(i)) {
		return(
			indexes(
				eval(i, envir=new.env(parent=values)),
				type
			)
		)
	}
	
	# Character matching
	if(is.character(i)) {
		# Dimension to query
		type <- match.arg(type)
		if(type == "column") {
			names <- colNames
		} else if(type == "row") {
			if(!rowNamed) stop("No names to match in")
			names <- rowNames
		} else {
			stop("Invalid 'type'")
		}
		
		# Matching
		notFound <- ! i %in% names
		if(any(notFound)) stop("Name(s) not found : ", paste(sprintf("\"%s\"", i[notFound]), collapse=", "))
		return(match(i, names))
	}
	
	# Unknown
	stop("Invalid 'i' type")
},

initialize = function(
		rowCount = as.integer(0),
		rowNames = character(0),
		rowNamed = FALSE,
		colCount = as.integer(0),
		colNames = character(0),
		colReferences = character(0),
		colIterator = as.integer(0),
		values = new.env(),
		...
	) {
	# Initialize an empty table
	.self$initFields(
		rowCount = rowCount,
		rowNames = rowNames,
		rowNamed = rowNamed,
		colCount = colCount,
		colNames = colNames,
		colReferences = colReferences,
		colIterator = colIterator,
		values = values
	)
	.self
},

metaFields = function() {
"Returns a character vector of fields that do not directly depend on the tabular content, for clonage."
	
	out <- names(.self$getRefClass()$fields())
	out <- setdiff(out, names(getRefClass("refTable")$fields()))
	return(out)
},

rowOrder = function(newOrder, na.last=TRUE, decreasing=FALSE) {
"Reorder the rows of the tables (duplication / subsetting are handled)
- newOrder     : new order to apply, as an integer vector of row indexes or a character vector of column names.
- na.last      : to be passed to order(), if 'newOrder' is a column name vector.
- decreasing   : to be passed to order(), if 'newOrder' is a column name vector."
	
	# Build order
	if(is.character(newOrder)) {
		# Queried columns
		if(any(!newOrder %in% colNames)) stop("'newOrder' must refer to existing columns")
		columns <- sprintf("`%s`", colReferences[ match(newOrder, colNames) ])
		
		# order() call
		expr <- sprintf("order(%s, na.last=%s, decreasing=%s)", paste(columns, collapse=", "), na.last, decreasing)
		newOrder <- eval(parse(text=expr), values)
	} else if(!is.integer(newOrder)) {
		stop("'newOrder' must be an integer order or a character vector of columns")
	}
	
	# Apply order to columns
	for(colRef in colReferences) {
		values[[ colRef ]] <<- values[[ colRef ]][ newOrder ]
	}
	
	# Apply order to row names
	if(isTRUE(rowNamed)) {
		rowNames <<- rowNames[ newOrder ]
	}
	
	# Change row count if necessary
	if(rowCount != length(newOrder)) {
		rowCount <<- length(newOrder)
	}
},

setColNames = function(j, value) {
"Replaces one or many column names.
- j       : subset of columns to rename.
- value   : new column names to use, as a character vector."
	
	# References
	j <- indexes(j, "column")
	
	# Forget value names
	names(value) <- NULL
	
	# Remove old bindings
	rm(list=colNames[j], pos=values)
	
	# Update colNames vector
	if(is.null(j)) {
		colNames <<- value
	} else {
		colNames[j] <<- value
	}
	
	# Create new bindings
	for(index in j) {
		makeActiveBinding(
			sym = colNames[index],
			fun = eval(
				parse(text=sprintf("function(v) get(\"%s\")", colReferences[index])),
				envir = values
			),
			env = values
		)
	}
},

setLevels = function(j=NULL, newLevels) {
"Get or replace levels of a factor column
- j           : column index (numeric) or name (character).
- newLevels   : new levels to use, as a character vector."
	
	# Reference
	j <- indexes(j, "column")
	
	# Single column
	colRefSubset <- colReferences[j]
	if(length(colRefSubset) != 1) stop("'j' must be defined and unique")
	
	if(is.factor(values[[ colRefSubset ]])) {
		levels(values[[ colRefSubset ]]) <<- newLevels
	} else stop("'j' must refer to a column of class 'factor'")
},

setRowNames = function(value) {
"Replaces the entire row names set.
- value   : new row names to use in the table, as a character vector. NULL will disable row naming."
	
	# Forget value names
	names(value) <- NULL
	
	if(is.null(value)) {
		# No row names
		rowNamed <<- FALSE
		rowNames <<- character(0)
	} else {
		# New row names
		if(!is.character(value))      stop("'value' must be of type 'character'")
		if(length(value) != rowCount) stop("'value' must have 'rowCount' elements")
		if(any(is.na(value)))         stop("'value' must not contain NA values")
		if(any(duplicated(value)))    stop("'value' must not contain duplicated values")
		
		rowNamed <<- TRUE
		rowNames <<- value
	}
},

show = function(include=FALSE, fieldWidth=10) {
"Interactive printing
- include   : single logical value, if TRUE class name will not be printed."
	
	# Class name
	if(!isTRUE(include)) {
		cat("\n  \"refTable\" reference class object\n")
	} else {
		cat("\n  Extends \"refTable\"\n")
	}
	
	cat("\n")
	if(rowCount > 6) {
		# Content summary
		content <- as.matrix(format.data.frame(.self$extract(c(1:3, rowCount-(2:0))), na.encode=FALSE))
		content <- rbind(
			content[1:3,],
			rep("...", ncol(content)),
			content[4:6,],
			rep(" ", ncol(content))
		)
		rownames(content)[4] = "..."
		print(content, quote=FALSE, right=TRUE)
	} else if(rowCount == 0) {
		# No content
		cat("   ", paste(colNames, collapse="   "), "\n\n")
	} else {
		# Full content
		content <- as.matrix(format.data.frame(extract(), na.encode=FALSE))
		content <- rbind(
			content,
			rep(" ", ncol(content))
		)
		print(content, quote=FALSE, right=TRUE)
	}
},

types = function(j=NULL) {
"Returns classes of selected columns
- j   : column indexes or names (NULL for all columns)"
	
	# References
	j = indexes(j, "column")
	
	# Apply class()
	results = character(0)
	if(is.null(j)) {
		k = 1
		while(k <= colCount) {
			results[ colNames[k] ] = class(values[[ colReferences[k] ]])
			k = k + 1
		}
	} else {
		for(k in (1:colCount)[j]) {
			results[ colNames[k] ] = class(values[[ colReferences[k] ]])
		}
	}
	
	return(results)
}

	)
)

### Constructor ###
refTable = function(..., row.names, warn=TRUE) {
	# Arguments	
	argList = list(...)
	
	# Dispatch
	if(length(argList) == 0) {
		# Empty table
		object = new("refTable")
	} else if(length(argList) == 1 && is.data.frame(argList[[1]])) {
		# Data.frame
		object = new("refTable")
		object$rowNamed = !is.integer(attr(argList[[1]], "row.names"))
		if(!missing(row.names)) warning("'row.names' ignored")
		object$addDataFrame(argList[[1]])
	} else if(length(argList) == 1 && is.list(argList[[1]])) {
		# List
		object = new("refTable")
		if(!missing(row.names)) object$rowNamed = TRUE
		object$addList(argList[[1]], row.names=row.names)
	} else if(length(argList) > 0 && all(sapply(argList, is.vector) | sapply(argList, is.factor))) {
		# Vectors
		object = new("refTable")
		if(!missing(row.names)) object$rowNamed = TRUE
		object$addList(argList, row.names=row.names)
	} else {
		stop("Provide a single data.frame or multiple vectors")
	}
	
	# Check
	object$check(warn=warn)
	
	return(object)
}
