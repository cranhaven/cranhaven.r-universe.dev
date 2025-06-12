#' Inferring Shared Modules from Multiple Gene Expression Datasets with Partially Overlapping Gene Sets
#' 
#' Takes a list of data matrices, with potentially different number of genes, number of modules, and a penalty parameter,
#' and returns the final assignment of the data points in each dataset to the modules, the values of the module latent variables, 
#' and the conditional dependency network among the module latent variables.
#' @param datasetlist A list of gene expression matrices of size n_i x p_i where rows represent samples and columns represent genes for each dataset i.  This can be created by using the list() command, e.g., list(dataset1, dataset2, dataset3)
#' @param mcnt A positive integer representing the number of modules to learn from the data
#' @param lambda A penalty parameter that regularizes the estimated precision matrix representing the conditional dependencies among the modules
#' @param printoutput 0 or 1 representing whether the progress of the algorithm should be displayed (0 means no display which is the default)
#' @param maxinitKMiter Maximum number of K-means iterations performed to initialize the parameters (the default is 100 iterations)
#' @param maxiter Maximum number of INSPIRE iterations performed to update the parameters (the default is 100 iterations)
#' @param threshold Convergence threshold measured as the relative change in the sum of the elements of the estimated precision matrices in two consecutive iterations (the default is 10^-2)
#' @param initseed The random seed set right before the K-means call which is performed to initialize the parameters
#' @return \item{L}{A matrix of size (sum_n_i) x mcnt representing the inferred latent variables (the low-dimensional representation - or LDR - of the data)}
#' @return \item{Z}{A list of vectors of size p_i representing the learned assignment of each of the genes in each dataset i to one of mcnt modules}
#' @return \item{theta}{Estimated precision matrix of size mcnt x mcnt representing the conditional dependencies among the modules}
#' @useDynLib INSPIRE, .registration = TRUE
#' @import stats
#' @import missMDA
#' @examples
#' \dontrun{
#' library(INSPIRE)
#' mcnt = 90 #module size
#' lambda = .1 #penalty parameter to induce sparsity
#' # download two real gene expression datasets, where the rows are genes and columns are samples
#' data('two_example_datasets')
#' # log-normalize, and standardize each dataset
#' res = INSPIRE(list(scale(log(exmp_dataset1)), scale(log(exmp_dataset2))), mcnt, lambda)
#' }
#' @export

INSPIRE = function(datasetlist, mcnt, lambda, printoutput=0, maxinitKMiter=100, maxiter=100, threshold=1e-2, initseed=123){
	if (requireNamespace('missMDA', quietly = TRUE)) {
		if(length(datasetlist) > 0) {
			for (i in 1:length(datasetlist)) {
				datasetcurr = datasetlist[[i]]
				if(i==1) {
					allgenes = colnames(datasetcurr)
					allsamples = rownames(datasetcurr)
				} else {
					allgenes = union(allgenes, colnames(datasetcurr))
					allsamples = union(allsamples, rownames(datasetcurr)) # this union is equal to concatenation since we don't expect any intersection in the samples of different datasets
				}
			}
			alldata = matrix(nrow=length(allsamples), ncol=length(allgenes))
			colnames(alldata) = allgenes
			rownames(alldata) = allsamples
			for (i in 1:length(datasetlist)) {
				datasetcurr = datasetlist[[i]]
				alldata[rownames(datasetcurr), colnames(datasetcurr)] = datasetcurr
			}
			if(sum(is.na(alldata)) > 0) { # we need that because if there is no missing data, there is no attribute called $completeObs
				dataimputed = imputePCA(alldata, method='EM', maxiter = 100, threshold=1e-2)$completeObs
			} else {
				dataimputed = alldata
			}
			
			set.seed(initseed)
			resKMimp = kmeans(t(dataimputed), mcnt, algorithm = 'Lloyd', iter.max = maxinitKMiter)
			L = t(resKMimp$centers)

			miss = .Machine$integer.max
			alldata[is.na(alldata)] = miss
			sz = nrow(alldata)
			p = ncol(alldata)
			mcnt = ncol(L)
			thetaout = numeric(mcnt*mcnt)
			Zout = numeric(length(allgenes))
			# below two lines are required because R passes matrices to C column by column
			alldata = t(alldata)
			L = t(L)
			storage.mode(alldata) = 'double'
			storage.mode(L) = 'double'
			storage.mode(Zout) = 'integer'
			storage.mode(thetaout) = 'double'
			res = .C('INSPIRE', alldata, Lout=L, as.integer(sz), as.integer(p), as.integer(mcnt), as.double(lambda), as.integer(maxiter), as.double(threshold), as.integer(printoutput), as.integer(miss), thetaout=thetaout, Zout=Zout)
			
			L = t(matrix(res$Lout, nrow=mcnt, ncol=length(allsamples)))
			rownames(L) = allsamples
			colnames(L) = paste0('M', 1:mcnt)
			
			Zall = res$Zout+1
			names(Zall) = allgenes
			
			Z = list()
			for (i in 1:length(datasetlist)) {
				Z[[i]] = Zall[colnames(datasetlist[[i]])]
			}
			
			theta = matrix(res$thetaout, nrow=mcnt, ncol=mcnt)
			rownames(theta) = paste0('M', 1:mcnt)
			colnames(theta) = paste0('M', 1:mcnt)
			
			return(list('L'=L, 'Z'=Z, 'theta'=theta))
		} else {
			print('No data')
		}
	} else {
		print('Error: missMDA package is required.')
	}
}

#' @name exmp_dataset1
#' @title Example Gene Expression Dataset-1
#' @description This example ovarian cancer dataset contains expression of random half of the genes
#' on the 28 samples from the GSE19829.GPL570 accession in Gene Expression Omnibus.
#' Contains 28 samples (as rows) and 9056 genes (as columns).
#' 4117 of the genes are overlapping with the genes in exmp_dataset2.
#' @docType data
NULL

#' @name exmp_dataset2
#' @title Example Gene Expression Dataset-2
#' @description This example ovarian cancer dataset contains expression of random half of the genes
#' on the 42 samples from the GSE19829.GPL8300 accession in Gene Expression Omnibus.
#' Contains 42 samples (as rows) and 4165 genes (as columns).
#' 4117 of the genes are overlapping with the genes in exmp_dataset1.
#' @docType data
NULL
