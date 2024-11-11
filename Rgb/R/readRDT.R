# Efficient storage of refTable inheriting objects
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

readRDT <- function(file, version=FALSE) {
	# Loading
	values <- new.env()
	load(file=file, envir=values)
	
	# Checks
	if(!exists(".saveRDT.rdt", envir=values))   stop("'file' is not a valid .rdt (no .saveRDT.rdt element)")
	if(!exists(".saveRDT.slots", envir=values)) stop("'file' is not a valid .rdt (no .saveRDT.slots element)")
	.slots <- get(".saveRDT.slots", envir=values)
	if(!exists(".saveRDT.class", envir=values)) stop("'file' is not a valid .rdt (no .saveRDT.class element)")
	.class <- get(".saveRDT.class", envir=values)
	
	# Version only
	if(isTRUE(version)) return(values$.saveRDT.rdt)
	
	# Clean
	rm(".saveRDT.rdt", envir=values)
	rm(".saveRDT.slots", envir=values)
	rm(".saveRDT.class", envir=values)
	
	# New object
	object <- new(getClass(.class, where=attr(.class, "package")))
	object$values <- values
	
	# Slots
	for(slotName in names(.slots)) {
		object$field(slotName, .slots[[slotName]])
	}
	
	# Bindings
	for(i in 1:object$colCount) {
		makeActiveBinding(
			sym = object$colNames[i],
			fun = eval(
				parse(text=sprintf("function(v) get(\"%s\")", object$colReferences[i])),
				envir = object$values
			),
			env = object$values
		)
	}
	
	if(inherits(object, "track.table")) {
		# Dynamic slots in track.table
		object$buildCalls()
		object$buildIndex()
	}
	
	return(object)
}
