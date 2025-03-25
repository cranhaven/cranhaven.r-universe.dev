#' Pareto Enrichment Analysis for Combining Heterogeneous datasets
#'
#' This function is for pathway enrichment meta-analysis with Pareto dominance based method. The input of this function is the gene level test-statistics (e.g. t statistics) from multiple datasets on which meta analysis will be performed, and a pathway (or geneset) list. It outputs the pathway p-values for each individual dataset as well as the pareto combined pathway p-values in a data frame.
#' @importFrom metap sumlog
#' @importFrom mnormt sadmvn
#' @importFrom MASS mvrnorm
#' @importFrom stats cor pnorm qnorm
#' @param input.data The test statistics of each gene from multiple datasets (the test statistics is from the case versus control statistical test, e.g. t-test). The rows are genes where the rownames are gene names (official gene symbols). The columns are the individual datasets.
#' @param nsample The number of random sampling times for Pareto meta-analysis p-value calculation. As Pareto based meta-analysis is a non-parametric method, this parameter decides the NULL distribution size of meta-pathway p-value computing.
#' @param input.pathway the pathways or genesets in the format of lists. The pathways or genesets should be defined by official gene symbols. An example KEGG pathway can be obtained with data('KEGG'). (The pathway input format is the same with the output from the 'gmtPathways' function from fgsea package.)
#' @param direction "up" or "down" denoting if the pathway p-value is calculated by accounting for pathway up-regulation or down-regulation. The default is "up", which means the peach function calculated combined p-value indicates if a pathway is up-regulated across the datasets being combined.
#' @param is.Fisher.Stouffer Logical indication. If TRUE, peach function will output the combined meta-pathway-p-value from non-parametric Fisher's and Stouffer's method. The combined p-value will not be the same with the original Fisher's or Stouffer's method, as this version has the Monte Carlo implementation of these two methods that accounts for the correlation from the input dataset.

#' @keywords peach function
#' @export
#'
#' @examples
#' ## load example input data (TCGA cancer versus control test t statistics)
#' data('TCGA.input')
#' ## load the KEGG pathways
#' data('KEGG')
#' ## Run peach
#'\donttest{
#'res = peach(input.data=TCGA.input,input.pathway=KEGG,direction ="up",is.Fisher.Stouffer = TRUE)
#'}

peach = function(input.data = NULL, nsample=1000, input.pathway = NULL,
                 direction = "up", is.Fisher.Stouffer = TRUE){
  idx = which(rowSums(is.na(input.data))==0)
  input.data = input.data[idx,]
  N = dim(input.data)[1]
  K = dim(input.data)[2]

  pathways.raw = input.pathway

  ## only keep genes with gene id in the TCGA data
  ## the query gene is also removed
  pathways=pathways.raw
  pathway.sizes = rep(NaN,length(pathways))
  for (i in 1:length(pathways.raw)){
    pathways[[i]] = intersect(rownames(input.data),pathways[[i]])
    pathway.sizes[[i]] = length(pathways[[i]])
  }
  pathways = pathways[c(which(pathway.sizes > 1))] ## remove pathways that have only one gene
  pathway.names = names(pathways)
  pathway.sizes = pathway.sizes[pathway.sizes > 1]
  rm(pathways.raw)

  ### p-value combination.
  fisher.pval.mc = rep(NaN,length(pathway.names))
  stouffer.pval.mc = rep(NaN,length(pathway.names))
  pareto.pval = rep(NaN,length(pathway.names))

  cat('\n')
  message("PEACH is calculating meta-p-value. This may take 1-2 minutes. Please be patient...\n")
  cat('\n')

  mu = rep(0,ncol(input.data))
  sigma = cor(input.data)
  ## construct null based on data mu and sigma ****
  z.null = mvrnorm(n = nsample, mu = mu, Sigma = sigma)
  x.pval.null = pnorm(z.null, lower.tail = F)
  ## pareto null pda calculation
  pda.null = rep(NaN,nsample)
  for(j in 1:nsample){
    pda.null[j] = sadmvn(lower = z.null[j,],
                         upper = rep(Inf,K),
                         mean = mu,
                         varcov = sigma)
  }

  path.pval = matrix(NaN,length(pathways), ncol(input.data))
  rownames(path.pval) = pathway.names
  colnames(path.pval) = colnames(input.data)

  for (i in 1:length(pathway.names)){
    tmp.pathway = pathways[[i]]
    tmp.pid = match(tmp.pathway,rownames(input.data))
    n = length(tmp.pid)
    x = colSums(input.data[tmp.pid,])
    x.null = matrix(NaN,nsample,K)
    for(j in 1:nsample){
      x.null[j,] = colSums(input.data[sample(N,n),])
    }
    x.pval = rep(NaN,K)
    if(direction == "up"){
      for(k in 1:K){
        x.pval[k] = length(which(x.null[,k]>=x[k]))/nsample
      }
    } else if(direction == "down"){
      for(k in 1:K){
        x.pval[k] = length(which(x.null[,k]<x[k]))/nsample
      }
    }

    x.pval[which(x.pval==0)] = 1/nsample
    x.pval[which(x.pval==1)] = 1-1/nsample
    path.pval[i,] = x.pval

    z = qnorm(x.pval,lower.tail = FALSE)

    ## Monte Carlo Fisher
    chi2 = -sum(log(x.pval))
    chi2.null = -rowSums(log(x.pval.null))
    fisher.pval.mc[i] = length(which(chi2.null>=chi2))/nsample

    ## Monte Carlo Stouffer
    sumz = sum(z)
    sumz.null = rowSums(z.null)
    stouffer.pval.mc[i] = length(which(sumz.null>=sumz))/nsample

    ## pareto analysis
    pda = sadmvn(lower = z,
                 upper = rep(Inf,K),
                 mean = mu,
                 varcov = sigma)

    pareto.pval[i] = length(which(pda.null<=pda))/nsample
  }

  ## assign 0 and 1 pvalues to 1/nsample and 1-1/nsample to avoid numerical problems when taking log
  pareto.pval[which(pareto.pval==0)] = 1/nsample
  pareto.pval[which(pareto.pval==1)] = 1-1/nsample
  fisher.pval.mc[which(fisher.pval.mc==0)] = 1/nsample
  fisher.pval.mc[which(fisher.pval.mc==1)] = 1-1/nsample
  stouffer.pval.mc[which(stouffer.pval.mc==0)] = 1/nsample
  stouffer.pval.mc[which(stouffer.pval.mc==1)] = 1-1/nsample

  if(is.Fisher.Stouffer){
    meta.pval = data.frame("pareto.meta.pval" = pareto.pval,
                            "fisher.meta.pval" = fisher.pval.mc,
                            "stouffer.meta.pval" = stouffer.pval.mc,
                           stringsAsFactors = F)
    rownames(meta.pval) = rownames(path.pval) = pathway.names
  } else{
    meta.pval = as.data.frame(pareto.pval)
    rownames(meta.pval) = rownames(path.pval) = pathway.names
    colnames(meta.pval) = "pareto.meta.pval"
  }

  colnames(path.pval) = colnames(input.data)
  path.pval = as.data.frame(path.pval)

  output = cbind(path.pval, meta.pval)

  return(output)
}
