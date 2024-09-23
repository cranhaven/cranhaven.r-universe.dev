Id <- " "

bhpm.pointmass.weights = function(cluster.data) {

	if (is.character(cluster.data)) {
		file = cluster.data
		cluster.data <- read.table(file, header=TRUE, stringsAsFactors = FALSE)
	}

	facs <- sapply(cluster.data, is.factor)
	cluster.data[facs] <- sapply(cluster.data[facs], as.character)

	cluster.data = cluster.data[cluster.data$Trt.Grp > 1,]
	comp.grps <- unique(cluster.data$Trt.Grp)

	weights <- NULL

	if ("Cluster" %in% names(cluster.data)) {
		for (i in 1:length(comp.grps)) {
			t.d <- cluster.data[ cluster.data$Trt.Grp == comp.grps[i],]
			n <- nrow(t.d)
			weight.pm <- data.frame(Cluster = t.d$Cluster,
				Outcome.Grp = t.d$Outcome.Grp, Outcome = t.d$Outcome, Trt.Grp = rep(comp.grps[i], n),
				weight_pm = rep(0.5,n), stringsAsFactors = FALSE)

			if (is.null(weights)) {
				weights <- weight.pm
			}
			else {
				weights <- rbind(weights, weight.pm)
			}
		}
	}

	weights
}
