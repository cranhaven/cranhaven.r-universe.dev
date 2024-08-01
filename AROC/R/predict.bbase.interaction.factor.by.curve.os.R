predict.bbase.interaction.factor.by.curve.os <-
function(object, newx, newfactor) {
	newfactor <- droplevels(newfactor)
	factor.levels <- levels(newfactor)

	# Parametric part
	if(length(factor.levels) > 1) {
		#mf <- model.frame("~ factor", data = data.frame(factor = newfactor), drop.unused.levels = TRUE)
		#mt <- terms(mf)   
		#param.part <- model.matrix(mt, mf)[,-1, drop = FALSE]
		ord <- NULL
		for(i in 1:length(factor.levels)) {
			ord <- c(ord, which(newfactor == factor.levels[i]))
		}
	} else {
		#param.part <- NULL
		ord <- 1:length(newx)
	}
	interaction.smooth.part.pred <- list()
	interaction.smooth.part <- attr(object,"interaction.smooth.part")
	for(i in 1:length(factor.levels)) {
			Baux <- suppressWarnings(predict.bbase.os(interaction.smooth.part[[factor.levels[i]]], newx[newfactor == factor.levels[i]]))
			attributes(Baux) <- attributes(Baux)["dim"]
			interaction.smooth.part.pred[[i]] <- Baux
	}
	# Join parametric and smooth
	aux <- as.matrix(bdiag(interaction.smooth.part.pred))
	#B <- cbind(param.part, aux[order(ord),])
	B <- aux[order(ord),]
	B
}
