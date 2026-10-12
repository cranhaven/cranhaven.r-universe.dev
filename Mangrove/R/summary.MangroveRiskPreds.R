summary.MangroveRiskPreds <-
function(object,...){
	class(object) <- "numeric"
	summary.default(object,...)
	}

