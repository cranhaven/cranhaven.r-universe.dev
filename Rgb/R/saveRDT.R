# Efficient storage of refTable inheriting objects
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

saveRDT <- function(
		object,
		file,
		compress = "gzip",
		compression_level = 6
		)
	{
	# Checks
	if(!is(object, "refTable")) stop("'object' must be a 'refTable' inheriting object")
	
	# Collect slots
	slots <- list()
	slotNames <- setdiff(names(object$getRefClass()$fields()), "values")
	for(slotName in slotNames) slots[[slotName]] <- object$field(slotName)
	
	# Remove environment from functions in collected slots
	noEnv <- function(x) {
		environment(x) <- .GlobalEnv
		return(x)
	}
	slots <- rapply(slots, classes="function", f=noEnv, how="replace")
	
	
	
	# Temporary storage of the flag element
	assign(".saveRDT.rdt", value=as.character(utils::packageVersion("Rgb")), envir=object$values)
	on.exit(rm(list=".saveRDT.rdt", envir=object$values), add=TRUE)
	
	# Temporary storage of the class name
	assign(".saveRDT.class", value=class(object), envir=object$values)
	on.exit(rm(list=".saveRDT.class", envir=object$values), add=TRUE)
	
	# Temporary storage of the slot contents
	assign(".saveRDT.slots", value=slots, envir=object$values)
	on.exit(rm(list=".saveRDT.slots", envir=object$values), add=TRUE)
	
	# Save from object$values to file
	save(list=c(".saveRDT.rdt", ".saveRDT.class", ".saveRDT.slots", object$colReferences), envir=object$values, file=file, compress=compress, compression_level=compression_level)
}

