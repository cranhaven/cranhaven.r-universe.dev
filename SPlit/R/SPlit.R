compute_sp = function(n, p, dist.samp, num.subsamp, iter.max=500, tol=1e-10, nThreads)
{
	rnd.flg = FALSE
	if(num.subsamp < nrow(dist.samp))
	{
		rnd.flg = TRUE
	}

	wts = rep(1.0, nrow(dist.samp))
	n0 = n * p

	if(missing(nThreads))
	{
		num.cores = parallel::detectCores()
	}
	else
	{
		if(nThreads < 1)
		{
			stop("nThreads should be at least 1.")
		}

		threads = parallel::detectCores()
		if(nThreads > threads)
		{
			num.cores = threads
		}
		else
		{
			num.cores = nThreads
		}
	}

	bd = matrix(NA, nrow=p, ncol=2, byrow=T)
	for(i in 1:p)
	{
        bd[i, ] = range(dist.samp[, i])
    }

    if (anyDuplicated(dist.samp))
    {
	      dist.samp = jitter(dist.samp)
	      for(i in 1:p)
	      { 
	        dist.samp[, i] = pmin(pmax(dist.samp[, i], bd[i, 1]), bd[i, 2])
	      }
    }
    
	ini = matrix(jitter(dist.samp[sample(1:nrow(dist.samp), n, F), ]), ncol=p)
	for(i in 1:p)
	{ 
		ini[, i] = pmin(pmax(ini[, i], bd[i, 1]), bd[i, 2])
	}	

    if(p == 1)
    {
		ini = matrix(ini, ncol=1)
    }

    sp_ = sp_cpp(n, p, ini, dist.samp, TRUE, bd, num.subsamp, iter.max, tol, num.cores, n0, wts, rnd.flg)
    return(sp_);
}


data_format = function(data)
{
	if(anyNA(data))
	{
		stop("Dataset contains missing value(s).")
	}

	D = matrix(, nrow=nrow(data))
	for(j in 1:ncol(data))
	{
		if(is.factor(data[, j]))
		{
			factor = unique(data[, j])
			if(length(factor) == 1)
			{
				next
			}

			factor_helm = contr.helmert(length(factor))
			helm = factor_helm[match(data[, j], factor), ]
			D = cbind(D, helm)
		}
		else
		{
			if(is.numeric(data[, j]))
			{
				if(sum(data[1, j] == data[, j]) == nrow(data))
				{
					next
				}

				D = cbind(D, data[, j])
			}
			else
			{
				stop("Dataset constains non-numeric non-factor column(s).")
			}
		}
	}

	D = D[, -1]
	D = scale(D)
	return(D)
}


#' Split a dataset for training and testing
#' 
#' \code{SPlit()} implements the optimal data splitting procedure described in Joseph and Vakayil (2021). \code{SPlit} can be applied to both regression and classification problems, and is model-independent. As a preprocessing step, the nominal categorical columns in the dataset must be declared as factors, and the ordinal categorical columns must be converted to numeric using scoring.
#' 
#' @param data The dataset including both the predictors and response(s); should not contain missing values, and only numeric and/or factor column(s) are allowed.  
#' @param splitRatio The ratio in which the dataset is to be split; should be in (0, 1) e.g. for an 80-20 split, the \code{splitRatio} is either 0.8 or 0.2.
#' @param kappa If provided, stochastic majorization-minimization is used for computing support points using a random sample from the dataset of size = \code{ceiling(kappa * splitRatio * nrow(data))}, in every iteration.
#' @param maxIterations The maximum number of iterations before the tolerance level is reached during support points optimization.
#' @param tolerance The tolerance level for support points optimization; measured in terms of the maximum point-wise difference in distance between successive solutions.
#' @param nThreads Number of threads to be used for parallel computation; if not supplied, \code{nThreads} defaults to maximum available threads.
#'
#' @return Indices of the smaller subset in the split.
#'
#' @details Support points are defined only for continuous variables. The categorical variables are handled as follows. \code{SPlit()} will automatically convert a nominal categorical variable with \eqn{m} levels to \eqn{m-1} continuous variables using Helmert coding. Ordinal categorical variables should be converted to numerical columns using a scoring method before using \code{SPlit()}. 
#' For example, if the three levels of an ordinal variable are poor, good, and excellent, then the user may choose 1, 2, and 5 to represent the three levels. These values depend on the problem and data collection method, and therefore, \code{SPlit()} will not do it automatically. The columns of the resulting numeric dataset are then standardized to have mean zero and variance one. 
#' \code{SPlit()} then computes the support points and calls the provided \code{subsample()} function to perform a nearest neighbor subsampling. The indices of this subsample are returned.
#'
#' \code{SPlit} can be time consuming for large datasets. The computational time can be reduced by using the stochastic majorization-minimization technique with a trade-off in the quality of the split. For example, setting \code{kappa = 2} will use a random sample, twice the size of the smaller subset in the split, instead of using the whole dataset in every iteration of the support points optimization. Another option for large datasets is to use data twinning (Vakayil and Joseph, 2022) implemented in the \code{R} package \href{https://CRAN.R-project.org/package=twinning}{\code{twinning}}. \code{Twinning} is extremely fast, but for small datasets, the results may not be as good as \code{SPlit}.
#'
#' @export
#' @examples
#' ## 1. An 80-20 split of a numeric dataset
#' X = rnorm(n = 100, mean = 0, sd = 1)
#' Y = rnorm(n = 100, mean = X^2, sd = 1)
#' data = cbind(X, Y)
#' SPlitIndices = SPlit(data, tolerance = 1e-6, nThreads = 2) 
#' dataTest = data[SPlitIndices, ]
#' dataTrain = data[-SPlitIndices, ]
#' plot(data, main = "SPlit testing set")
#' points(dataTest, col = 'green', cex = 2)
#'
#' ## 2. An 80-20 split of the iris dataset
#' SPlitIndices = SPlit(iris, nThreads = 2)
#' irisTest = iris[SPlitIndices, ]
#' irisTrain = iris[-SPlitIndices, ]
#'
#' @references
#' Joseph, V. R., & Vakayil, A. (2021). SPlit: An Optimal Method for Data Splitting. Technometrics, 1-11. doi:10.1080/00401706.2021.1921037.
#'
#' Vakayil, A., & Joseph, V. R. (2022). Data Twinning. Statistical Analysis and Data Mining: The ASA Data Science Journal. https://doi.org/10.1002/sam.11574.
#' 
#' Mak, S., & Joseph, V. R. (2018). Support points. The Annals of Statistics, 46(6A), 2562-2592.


SPlit = function(data, splitRatio=0.2, kappa=NULL, maxIterations=500, tolerance=1e-10, nThreads)
{

	if(splitRatio <= 0 | splitRatio >= 1)
	{
		stop("splitRatio should be in (0, 1).")
	}

	data_ = data_format(data)
	n = round(min(splitRatio, 1 - splitRatio) * nrow(data_))

	if(is.null(kappa))
	{
		kappa = nrow(data_)
	}
	else
	{
		if(kappa <= 0)
		{
			stop("kappa should be positive.")
		}

		kappa = min(nrow(data_), ceiling(kappa * n))
	}

	sp_ = compute_sp(n, ncol(data_), dist.samp=data_, num.subsamp=kappa, iter.max=maxIterations, tol=tolerance, nThreads=nThreads)
	return(subsample(data_, sp_))
}


#' Optimal splitting ratio
#' 
#' \code{splitratio()} finds the optimal splitting ratio by assuming a polynomial regression model with interactions can approximate the true model. The number of parameters in the model is estimated from the full data using stepwise regression. A simpler solution is to choose the number of parameters to be square root of the number of unique rows in the input matrix of the dataset. Please see Joseph (2022) for details.
#' 
#' @param x Input matrix
#' @param y Response (output variable)
#' @param method This could be “simple” or “regression”. The default method “simple” uses the square root of the number of unique rows in \code{x} as the number of parameters, whereas “regression” estimates the number of parameters using stepwise regression. The “regression” method works only with continuous output variable.
#' @param degree This specifies the degree of the polynomial to be fitted, which is needed only if \code{method}=“regression” is used. Default is 2.
#' 
#' @return Splitting ratio, which is the fraction of the dataset to be used for testing.
#'
#' @export
#' @examples
#' X = rnorm(n=100, mean=0, sd=1) 
#' Y = rnorm(n=100, mean=X^2, sd=1)
#' splitratio(x=X, y=Y)
#' splitratio(x=X, y=Y, method="regression")
#'
#' @references
#' Joseph, V. R. (2022). Optimal Ratio for Data Splitting. Statistical Analysis & Data Mining: The ASA Data Science Journal, to appear.


splitratio = function(x, y, method="simple", degree=2)
{
	if(method == "regression")
	{
	    x = as.data.frame(polym(as.matrix(x), degree=degree))
	    names(x) = paste("x", 1:dim(x)[2], sep="")
	    if(is.numeric(y))
	    {
	      a = lm(y~., data=data.frame(x, y))
	      a.step = step(a, trace=0)
	      p = length(a.step$coefficients)
	    } else 
	    {
	      print("using method=simple for nonnumeric response")
	      p = sqrt(dim(unique(as.matrix(x)))[1])
	    }
	} else 
  	{
    	p = sqrt(dim(unique(as.matrix(x)))[1])
  	}

  gamma = 1 / (sqrt(p) + 1)
  return(gamma)
}




















