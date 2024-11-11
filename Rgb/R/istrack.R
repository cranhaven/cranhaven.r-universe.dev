# Check if a combination of arguments consists in a valid track
# Author : Sylvain Mareschal <maressyl@gmail.com>
# License : GPL3 http://www.gnu.org/licenses/gpl.html

istrack = function(...) {
	result <- try(.External("checktrack", PACKAGE="Rgb", ...), silent=TRUE)
	if(is(result, "try-error")) {
		output <- FALSE
		attr(output, "why") <- conditionMessage(attr(result, "condition"))
	} else {
		output <- TRUE
	}
	return(output);
}
