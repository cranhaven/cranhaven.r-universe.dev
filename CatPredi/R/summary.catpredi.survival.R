summary.catpredi.survival <-
function(object, digits = 4, ...) {
	object$digits <- digits
	class(object) <- "summary.catpredi.survival"
	object
}
