#' @export
"print.HOF.list" <-  function (
		x,
		test = 'AICc',
		selectMethod = 'bootselect.lower',
		...) {
	if(selectMethod == 'bootselect' & is.null(x[[1]]$bootstrapmodels)) {
		message('Bootstrap results missing. Using selectMethod "IC.weight" instead.\n')
		selectMethod <- 'IC.weight'
	}
    cat("Deviances:\n")
    sapply(sapply(x, function(m) m$models), '[[', 'deviance')
    printCoefmat(sapply(x, function(x) deviance(x)), na.print="", has.Pvalue=FALSE, ...)
    cat(paste("\nSuggested best models (",test, ", ", selectMethod, "):", sep=''))
    tmp <- sapply(x, pick.model, test=test, selectMethod = selectMethod, silent = TRUE, ...)
    names(tmp) <- names(x)
    cat('\n')
    print(noquote(tmp))
#      }
    invisible(x)
}
