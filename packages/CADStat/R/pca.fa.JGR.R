#' @export
pca.fa.JGR = function(my.data, numFactors=0, variables=NULL,
						subset1.name=NULL, subset1.val=NULL, subset2.name=NULL, subset2.val=NULL,
						iScreePlot=FALSE, iLoadingPlot=FALSE, center=TRUE, scale=TRUE, retx=TRUE,
						rotation="varimax", scores="none")
{


	#perfoms a principal components analysis if numFactors = 0, else performs a factor analysis

	#my.data		data.frame
	#numFactors	number of factors for a factor analysis (if 0, then perform a principal components analysis)
	#variables		vector of numeric variable names from my.data to be used in the analysis
	#subset1.name	column name of the first subsetting variable (panel)
	#subset1.val	values of the first subsetting variable
	#subset2.name	column name of the second subsetting variable (group)
	#subset2.val	values of the second subsetting variable
	#iScreePlot		indicates whether a scree plot should be created (PCA)
	#iLoadingPlot	indicates whether a loading/score plot should be created (PCA)
	#center			indicates whether variables should be shifted to be centered about zero (PCA)
	#scale			indicates whether variables should be scaled to unit variance (PCA)
	#retx			indicates whether the rotated variables should be returned (PCA)
	#rotation		indicates the type of rotation for factor analysis ("varimax" or "promax")
	#scores			indicates the type of factor analysis scored that should be returned ("none", "regression", "Bartlett")

	#NA handling
	#use options to set na.action to omit NAs which is used by lm
	options(na.action = "na.omit")

	#error checking
	nVar = length(variables)
	if (nVar < 3) stop("The number of variables must be at least three.")
	dof = 0.5 * ((nVar - numFactors)^2 - nVar - numFactors)
	if (dof < 0) stop("The degrees of freedom must be at least 0. The number of factors\nchosen is too large for the number of variables selected.")

	#find proper data subset
	ind = rep(TRUE, nrow(my.data))
	if (length(subset1.val) > 0) ind = ind & my.data[,subset1.name]%in%subset1.val
	if (length(subset2.val) > 0) ind = ind & my.data[,subset2.name]%in%subset2.val
	my.subset = my.data[ind,]

        # omit missing values
        d1 <- nrow(my.subset)
        selvec <- apply(my.subset[, variables], 1, function(x) all(! is.na(x)))
        my.subset <- my.subset[selvec,]
        d2 <- nrow(my.subset)
        if (d1 != d2) cat("\n",d1-d2, "records with missing fields have been omitted from the original data set!\n\n")

	#create formula
	myFormula = as.formula(paste("~", paste(variables, collapse="+")))

	#Fix to set the environment and pass it in. Found in datamerge/bioinfer/glm/pca.fa
	pos <- 1
	envir = as.environment(pos)

	#perform the analysis
	if (numFactors == 0) {
		#principal components analysis
		pca = prcomp(formula=myFormula, data=my.subset, center=center, scale=scale, retx=iLoadingPlot | retx)

		#summary of pca (eignevalues and proportion of variance)
		cat("Summary of Principal Components Analysis\n")
		print(summary(pca))

		#loadings (eigenvectors)
		cat("Variable Loadings\n")
		print(pca$rotation)

		#rotated variables
		if (retx) {
                    PCA.scores <- cbind(my.subset, pca$x)
                    assign("PCA.scores", PCA.scores, envir = envir)
                    cat("\nRotated variables appended to original data set and saved in PCA.scores.\n")
		}

		#scree plot
		if (iScreePlot) {
			JavaGD(name="ScreePlot", width=600, height=400, ps=14)
			#par(mar=c(4,4,2,1))
			plot(pca, main="Scree Plot")
		}

		#loading plot and score plot
		if (iLoadingPlot) {
			JavaGD(name="LoadingPlot", width=600, height=600, ps=14)
			#par(mar=c(4,4,2,1))
			biplot(pca, main="Loading and Score Plots")
		}
	} else {
		#factor analysis
		fa = factanal(x=myFormula, factors=numFactors, data=my.subset, scores=scores, rotation=rotation)

		#summary of pca (eignevalues and proportion of variance)
		cat("Summary of Factor Analysis\n")
		print(fa)

		#scores from the factor analysis
		if (scores!="none") {
                       FA.scores <- cbind(my.subset, fa$scores)
                       assign("FA.scores", FA.scores, envir = envir)
                       cat("\nFactor Analysis scores appended to original data frame and saved in FA.scores\n")
			#print(fa$scores)
		}

	}

	return(invisible())
}

