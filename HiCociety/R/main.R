#' Check if a package is installed
#'
#' This function checks whether a package is installed. If the package is not installed, it informs the user that the package is missing.
#'
#' @author Sora Yoon, PhD
#' @description This function checks whether a package is installed. If the package is not installed, it informs the user that the package is missing.
#' @param package The name of the package to check.
#' @return A logical value: TRUE if the package is installed, FALSE otherwise.
#' @importFrom utils getFromNamespace
#' @examples
#' check_package('dplyr')
#' @export
check_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    message(sprintf("The package '%s' is not installed. Please install it.", package))
    return(FALSE)
  } else {
    message(sprintf("The package '%s' is successfully installed.", package))
    return(TRUE)
  }
}


#' Get contact Frequency
#'
#' Get Contact Frequncy from .hic file
#'
#' @author Sora Yoon, PhD
#' @description Retrieve contact frequency from .hic file using strawR package.
#' @param fname .hic data for any types of genome conformation capture data
#' @param chr Chromosome number of network extraction
#' @param resol DNA basepair Resolution. Default=10000
#' @return A \code{data.frame} containing three columns: \code{x} and \code{y} (genomic coordinate pairs), and \code{counts} (the contact frequency between them).
#' @import strawr
#' @import HiCocietyExample
#' @examples
#' myhic=system.file('extdata', 'example.hic', package ='HiCocietyExample')
#' A = getContactFrequency(myhic,19,5000)
#' head(print(A))
#' @export
getContactFrequency <- function(fname, chr, resol){
  chr <- as.character(chr)
  cf <- straw("NONE", fname, chr, chr, "BP", resol)
  return(cf)
}

#' Contact probability
#'
#' Get Contact probablity
#'
#' @author Sora Yoon, PhD
#' @description It estimates contact probability based on the distance of a pair of a loci.
#' @param tab Output from getContactFrequency function.
#' @param farthest Maximum 1-D distance to search. Default=2Mb
#' @param resol Hi-C resolution for test. Default = 10000
#' @param prob Significance cutoff for negative binomial distribution. Default =0.975
#' @param n_cores The number of cores used for parallel computing. If set as NULL, n_cores is automatically set to the number of cores in the computer if it is not exceed 30. If it is more than 30, it is set as 30. Default = NULL
#' @return A \code{list} containing three objects: \code{AREA}, \code{original}, and \code{len1}, representing the statistical significance of each chromatin interaction pair.
#' @import parallel
#' @import doParallel
#' @import foreach
#' @import HiCocietyExample
#' @importFrom Rcpp sourceCpp
#' @import fitdistrplus
#' @importFrom stats pnbinom
#' @examples
#' # This example might take a long time to run, so we wrap it in donttest{}
#' \donttest{
#' myhic = system.file('extdata','example.hic',package = 'HiCocietyExample')
#' mydf=getContactFrequency(myhic, 19, 5000);
#' myprob=getContactProbability(mydf,farthest=2000000, resol=5000,prob=0.975,
#' n_cores=2);
#' }
#' @export
getContactProbability <- function(tab, farthest = 2000000, resol = 10000, prob, n_cores = NULL) {
  bin1 = bin2 = cnt = c()
  AREA <- list()
  
  # Detect number of cores
  if(is.null(n_cores))
  {
    n_cores <- parallel::detectCores() - 1
    if (n_cores > 30) n_cores <- 30
    if (n_cores <= 0) n_cores <- 1
  }
  
  # Create cluster and register parallel backend
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  
  # Define the function for unique pairs
  UniqueTwo <- function(vec1, vec2) {
    a <- paste(vec1, vec2, sep = "_")
    b <- unique(a)
    ab <- sapply(b, function(x) unlist(strsplit(x, split = "_")))
    res1 <- ab[1, ]
    res2 <- ab[2, ]
    return(list(uniVec1 = res1, uniVec2 = res2))
  }
  
  # Run the parallel processing
  A <- foreach::foreach(mylen = seq(0, farthest, resol), .combine = 'c', .init = list(list()),
                        .packages = c('fitdistrplus', 'stats')) %dopar% {
                          sameD <- which(tab$y - tab$x == mylen)
                          allCounts <- tab$counts[sameD]
                          fd <- fitdistrplus::fitdist(allCounts, distr = 'nbinom')
                          sz <- fd$estimate[1]
                          mu <- fd$estimate[2]
                          area <- stats::pnbinom(q = allCounts, size = sz, mu = mu, lower.tail = TRUE)
                          sigarea <- which(area > prob)
                          ABD <- UniqueTwo(allCounts, area)
                          AA <- list(list(contactfreq = ABD$uniVec1, prob = ABD$uniVec2))
                          list(Area = AA, bin1 = tab$x[sameD][sigarea], bin2 = tab$y[sameD][sigarea], cnt = area[sigarea])
                        }
  
  
  # Stop the cluster
  parallel::stopCluster(cl)
  sizeA = (length(A)-1)/4
  AREAindex = (0:(sizeA-1))*4+2
  AREA = A[AREAindex]
  names(AREA) <- seq(0,farthest,resol)
  
  bin1index = (0:(sizeA-1))*4+3
  bin1 = unlist(A[bin1index], use.names = F)
  bin2index = (0:(sizeA-1))*4+4
  bin2 = unlist(A[bin2index], use.names = F)
  cntindex = (0:(sizeA-1))*4+5
  cnt = unlist(A[cntindex],use.names=F)
  #names(AREA) = seq(0,farthest,resol)
  
  LB = length(bin1)
  
  return(list(AREA=AREA, original=data.frame(x=bin1,y=bin2,counts=cnt), len1=LB))
}

#' HiC to network data format
#'
#' Convert HiC to network data format
#'
#' @author Sora Yoon, PhD
#' @description It converts Hi-C dataframe to network object.
#' @param ftab three-column data composed of locus1, locus2 and value
#' @return An \code{igraph} object representing statistically significant chromatin interactions.
#' @import HiCocietyExample
#' @examples
#' # This example might take a long time to run, so we wrap it in donttest{}
#' \donttest{
#' myhic=system.file('extdata', 'example.hic', package ='HiCocietyExample')
#' ftab=getContactFrequency(myhic,19,5000);
#' net = hic2network(ftab[1:100,]);
#' plot(net)
#' }
#' @useDynLib HiCociety
#' @export
hic2network <- function(ftab){
  g = graph_from_data_frame(ftab, directed = FALSE, vertices = NULL)
  return(g)
}

#' Calculate Average Count within 5-pixel Padding
#'
#' This function calculates the average count within a 25kb padding
#' around each (x, y) coordinate pair.
#'
#' @param x Numeric vector of x-coordinates of contact frequency data frame.
#' @param y Numeric vector of y-coordinates of contact frequency data frame.
#' @param counts Numeric vector of contact frequency counts.
#' @param resol Integer specifying the HiC resolution.
#' @return A numeric vector of average counts.
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(1, 2, 3, 4, 5)
#' counts <- c(10, 20, 30, 40, 50)
#' resol <- 10000
#' calculate_avg_count(x, y, counts, resol)
#' @export
calculate_avg_count <- function(x, y, counts, resol) {
  .Call('_HiCociety_calculate_avg_count', x, y, counts, resol)
}



#' Retrieve chromosome names from .hic file
#'
#' To extract all chromosome names from .hic file
#'
#' @author Sora Yoon, PhD
#' @description It retrieves all chromosome names having longer than 2.5Mbp.
#' @param fname Path to .hic file
#' @import HiCocietyExample
#' @return A character vector containing the names of chromosomes whose genomic lengths exceed 2.5 Mbp.
#' @examples
#' myhic=system.file('extdata', 'example.hic', package ='HiCocietyExample')
#' get_all_chr_names(myhic)
#' @export
get_all_chr_names = function(fname)
{
  chromtab = strawr::readHicChroms(fname)
  filterChrom = chromtab$name[which(chromtab$length>5000*500)]
  filterChrom = sort(filterChrom)
  filterChrom = filterChrom[which(filterChrom!="ALL")]
  return(filterChrom)
}

#' Create module objects from the Hi-C data
#'
#' It generates a list of graph of significant interactions, module table and module elements.
#'
#' @author Sora Yoon, PhD
#' @description It generates a list of graph of significant interactions, module table and module elements.
#' @param fname  Path to .hic file
#' @param chr    chromosome numbers to run.
#' @param resol  Resolution of Hi-C data
#' @param nbprob  Negative binomial probability. Higher value gives smaller number of stronger interaction.
#' @param farthest  The maximum searching distance between two nodes
#' @param par.noise Parameter for noise removal. Default is 1, higher value gives more filtered interactions.
#' @param network.cluster.method Can select between 'louvain' as default and 'label_prop' which means the label propagation method.
#' @param n_cores The number of cores used for parallel computing. If set as NULL, n_cores is automatically set to the number of cores in the computer if it is not exceed 30. If it is more than 30, it is set as 30. Default = NULL
#' @return A \code{list} containing three elements: \code{Graphs} (an \code{igraph} object representing significant chromatin interactions for each chromosome), \code{ModuleSummary} (a \code{data.frame} containing information about chromatin interaction modules), and \code{ModuleElements} (a \code{list} of nodes forming significant chromatin interactions within each module).
#' @import igraph
#' @import HiCocietyExample
#' @examples
#'# This example might take a long time to run, so we wrap it in donttest{}
#' \donttest{
#' myhic=system.file('extdata', 'example.hic', package ='HiCocietyExample')
#' mycom = hic2community(myhic, "19", 5000, 0.975, 2000000,
#' par.noise=1, 'louvain', n_cores=2)
#' }
#' @export
hic2community <- function(fname, chr, resol, nbprob, farthest, par.noise = 1, network.cluster.method = 'louvain', n_cores=NULL){
  old_opts = options("scipen")
  on.exit(options(old_opts), add = TRUE)
  options(scipen = 999)
  chr = as.character(chr)
  totalNet = list()
  totalInfo  = data.frame(chr=character(), module_start = character(), module_end = character())
  totalModule = list()
  for(ch in chr)
  {
    message("Chromosome: ",ch,"\n")
    tab <- getContactFrequency(fname, ch, resol)
    
    message("--------Contact frequency table has been loaded (1/5)-------\n")
    fetab <- getContactProbability(tab, farthest, resol=resol, prob=nbprob, n_cores = n_cores)
    ftab_orig<-fetab$original
    message("--------Contact probability estimation is completed (2/5)-------\n")
    ftab_orig_cnt= merge(tab, ftab_orig, by=c('x','y'))
    ftab_orig_cnt$counts = ftab_orig_cnt$counts.x
    ftab_orig_cnt$avg_count <- calculate_avg_count(ftab_orig_cnt$x, ftab_orig_cnt$y, ftab_orig_cnt$counts, resol = resol)
    
    min_c = min(tab$x)
    max_c = max(tab$y)
    width_hic = ((max_c - min_c)/resol) + 1
    height = farthest / resol
    area_hic = width_hic * height
    c_area = sum(tab$counts[which(abs(tab$x-tab$y) < farthest)])
    acf1 = c_area / area_hic
    df = ftab_orig_cnt[which(ftab_orig_cnt$avg_count> par.noise*acf1),]
    message("--------Noise filtering is completed (3/5)--------\n")
    net <- hic2network(df)
    message("--------Significant interaction network is generated (4/5)--------\n")
    if(network.cluster.method == 'louvain'){A = cluster_louvain(as.undirected(net))
    }else if(network.cluster.method== 'label_prop'){ A = cluster_label_prop(as.undirected(net))
    }else{stop("Choose network.cluster.method between 'louvain' and 'label_prop'.")}
    message("--------Clustering data have been generated (5/5)--------\n")
    modu=sapply(1:max(A$membership), function(x) sort(as.numeric(A$names[which(A$membership==x)])))
    modu = modu[which(lapply(modu,length)>3)]
    modu2 <- lapply(modu, function(x){x = as.character(x); g1=subgraph(net,x);return(names(V(g1)))})
    EE = lapply(modu2, function(x){return(length(E(subgraph(net, vids = x))))})
    modu2 = modu2[order(unlist(EE),decreasing = T)]
    modu2 = lapply(modu2,function(x) sort(as.numeric(as.character(x))))
    totalNet = append(totalNet, list(net))
    totalInfo = rbind(totalInfo, data.frame(chr=ch, module_start = unlist(lapply(modu2, function(x) as.character(min(as.numeric(x))))),
                                            module_end = unlist(lapply(modu2, function(x) as.character(max(as.numeric(x)))))))
    totalModule = append(totalModule, modu2)
  }
  names(totalNet) = paste0("graph_",chr)
  
  # Gene name - add_Genes
  # Connectivity
  message('Start to estimate connectivity, transitivity and centrality node.\n')
  pt = proc.time()
  CON = c()
  TRA = c()
  EV  = c()
  for(i in 1:nrow(totalInfo))
  {
    chrom = totalInfo$chr[i]
    idx = which(names(totalNet) == paste0('graph_',chrom))
    grp = totalNet[[idx]]
    ele = totalModule[[i]]
    subg = subgraph(grp, as.character(ele))
    conne = length(E(subg))
    trans = transitivity(subg, type='global')
    ev= eigen_centrality(subg)
    ev = names(ev$vector)[which.max(ev$vector)]
    CON = append(CON, conne)
    TRA = append(TRA, trans)
    EV  = append(EV , ev)
  }
  timetaken=proc.time() - pt
  message('Elapsed time : ', timetaken[3], 's', sep="")
  totalInfo$connectivity = CON
  totalInfo$transitivity = TRA
  totalInfo$centrality_node=EV
  
  ord = order(totalInfo$connectivity, decreasing = T)
  
  totalInfo=totalInfo[ord,]
  totalModule=totalModule[ord]
  return(list(Graphs=totalNet, ModuleSummary = totalInfo, ModuleElements =totalModule))
}

#' All available Txdb
#'
#' Check all available Txdb package
#'
#' @author Sora Yoon, PhD
#' @description It finds all available Txdb packages used in add_Genes function.
#' @return A character vector containing the names of all available TxDb packages.
#' @examples
#' get_txdb()
#' @importFrom AnnotationDbi mapIds
#' @export
get_txdb = function()
{
  available_packages <- BiocManager::available()
  txdb_packages <- available_packages[grep("TxDb", available_packages)]
  return(txdb_packages)
}

#' Add gene information
#'
#' Adding gene list to ModuleSummary data frame obtained from hic2community function.
#'
#' @author Sora Yoon, PhD
#' @description This function adds a column with a list of genes included in each locus to the ModuleSummary data frame of the hic2community function.
#' @param df The ModuleSummary data frame obtained by running hic2community function
#' @param speciesObj Any Txdb package name corresponding
#' @return A \code{data.frame} identical to the input, with an additional \code{"Genes"} column. Each entry in this column lists the gene(s) that overlap with the corresponding genomic region. If multiple genes are present, they are concatenated with commas.
#' @importFrom IRanges IRanges
#' @importFrom GenomicRanges GRanges findOverlaps pintersect makeGRangesFromDataFrame width
#' @importFrom GenomicFeatures genes
#' @import HiCocietyExample
#' @examples
#' modulefile = system.file('extdata','mouse_naiveCD4T_Vahedi_short.rds',
#' package = 'HiCocietyExample')
#' mycom = readRDS(modulefile)
#' mycom$ModuleSummary = add_Genes(mycom$ModuleSummary,
#' 'TxDb.Mmusculus.UCSC.mm10.knownGene')
#' @importFrom AnnotationDbi mapIds
#' @importFrom S4Vectors queryHits subjectHits
#' @export
add_Genes <- function(df, speciesObj) {
  # Prefix 'chr' to chromosome names if not present
  if (substr(df$chr[1], 1, 1) != "c") {
    df$chr <- paste0("chr", df$chr)
  }
  
  # Create a GRanges object
  gr <- GRanges(
    seqnames = df$chr,
    ranges = IRanges(start = as.numeric(df$module_start), end = as.numeric(df$module_end))
  )
  
  # Load the TxDb package dynamically
  if (!requireNamespace(speciesObj, quietly = TRUE)) {
    stop(paste("Package", speciesObj, "not found"))
  }
  txdb <- getFromNamespace(speciesObj, asNamespace(speciesObj))
  
  # Define a mapping between TxDb and org packages
  org_mapping <- list(
    "TxDb.Hsapiens.UCSC.hg38.knownGene" = "org.Hs.eg.db",
    "TxDb.Mmusculus.UCSC.mm10.knownGene" = "org.Mm.eg.db",
    "TxDb.Rnorvegicus.UCSC.rn6.knownGene" = "org.Rn.eg.db",
    "TxDb.Dmelanogaster.UCSC.dm6.ensGene" = "org.Dm.eg.db",
    "TxDb.Scerevisiae.UCSC.sacCer3.sgdGene" = "org.Sc.sgd.db",
    "TxDb.Athaliana.BioMart.plantsmart28" = "org.At.tair.db",
    "TxDb.Btaurus.UCSC.bosTau9.refGene" = "org.Bt.eg.db",
    "TxDb.Cfamiliaris.UCSC.canFam3.refGene" = "org.Cf.eg.db",
    "TxDb.Celegans.UCSC.ce11.refGene" = "org.Ce.eg.db",
    "TxDb.Drerio.UCSC.danRer10.refGene" = "org.Dr.eg.db",
    "TxDb.EcoliK12.UCSC.ecoliK12.refGene" = "org.EcK12.eg.db",
    "TxDb.Ggallus.UCSC.galGal5.refGene" = "org.Gg.eg.db",
    "TxDb.Pfalcifarum.PlasmoDB.v46" = "org.Pf.plasmo.db",
    "TxDb.Sscrofa.UCSC.susScr11.refGene" = "org.Ss.eg.db",
    "TxDb.Xlaevis.UCSC.xenTro9.refGene" = "org.Xl.eg.db"
  )
  
  # Get the corresponding org package name
  orgPkg <- org_mapping[[speciesObj]]
  
  # Load the org package dynamically
  orgdb <- NULL
  packagecheck <- check_package(orgPkg)
  
  if (packagecheck) {
    # Try to get the OrgDb object from the package namespace
    orgdb <- tryCatch({
      getExportedValue(orgPkg, orgPkg)
    }, error = function(e) {
      warning(sprintf("Package '%s' is installed, but the OrgDb object could not be loaded. Gene symbols will not be annotated.", orgPkg))
      return(NULL)
    })
  } else {
    warning(sprintf("Package '%s' not found. Please install and load a proper package. Gene symbols will not be annotated.", orgPkg))
  }
  
  Genes <- genes(txdb)
  overlaps <- findOverlaps(gr, Genes)
  
  # Get gene IDs
  if (length(overlaps) == 0) {
    warning("No overlaps found. No gene annotations will be added.")
    df$Genes <- NA
  } else {
    gene_ids <- Genes$gene_id[subjectHits(overlaps)]
    
    if (!is.null(orgdb) && length(gene_ids) > 0) {
      gene_symbols <- mapIds(orgdb, keys = gene_ids, column = "SYMBOL", keytype = "ENTREZID", multiVals = "first")
    } else {
      gene_symbols <- gene_ids
    }
    
    # Add Genes column
    df$Genes <- sapply(seq_len(nrow(df)), function(i) {
      overlapping_genes <- gene_symbols[queryHits(overlaps) == i]
      if (length(overlapping_genes) > 0) paste(overlapping_genes, collapse = ",") else NA
    })
  }
  return(df)
}


#' Connectivity difference between two conditions
#'
#' Connectivity difference between two conditions
#'
#' @author Sora Yoon, PhD
#' @description output table of connectivity difference of modules between cell types is generated.
#' @param wt hic2community result from condition 1
#' @param ko hic2community result from condition 2
#' @param prefix.wt Prefix for wt to be presented in the column names
#' @param prefix.ko Prefix for ko to be presented in the column names
#' @return A \code{list} of two \code{data.frame} objects, each representing the network connectivity differences of modules in condition 1 or condition 2 when compared to the counterpart cell type. Each \code{data.frame} contains the following columns: \code{"chr"}, \code{"module_start"}, \code{"module_end"}, \code{"connectivity"}, \code{"transitivity"}, \code{"centrality_node"}, \code{"idx"} (the row index of the module in the input module object), \code{"connectivity_in_(counterpart_cell_type)"}, \code{"connectivity_difference"}, and \code{"connectivity_foldchange"}.
#' @param resolution Resolution of Hi-C dataset
#' @importFrom GenomicRanges GRanges
#' @importFrom GenomicRanges pintersect
#' @importFrom GenomicRanges width
#' @import HiCocietyExample
#' @import igraph
#' @examples
#' modulefile1 = system.file('extdata','mouse_naiveCD4T_Vahedi_short.rds',
#' package = 'HiCocietyExample')
#' modulefile2 = system.file('extdata','mouse_Th1_Vahedi_short.rds',
#' package = 'HiCocietyExample')
#' mycom1 = readRDS(modulefile1)
#' mycom2 = readRDS(modulefile2)
#' result = ConnectivityDiff(mycom1, mycom2, 'NaiveCD4T', 'Th1',
#' resolution = 5000)
#' head(print(result))
#' @export
ConnectivityDiff <- function(wt, ko, prefix.wt, prefix.ko, resolution = 5000)
{
  old_opts = options("scipen")
  on.exit(options(old_opts), add = TRUE)
  options(scipen = 999)
  wt.dat = wt$ModuleSummary
  ko.dat = ko$ModuleSummary
  
  wt.graph = wt$Graphs
  ko.graph = ko$Graphs
  
  # for modules in condition 1 (wt)
  chrlist = wt.dat$chr
  strlist = wt.dat$module_start
  endlist = wt.dat$module_end
  con.ko = c()
  for(i in 1:nrow(wt.dat))
  {
    ch = chrlist[i]
    st = strlist[i]
    en = endlist[i]
    
    ko.graph.sel = ko.graph[[paste0('graph_', ch)]]
    allNodes = seq(st,en,resolution)
    selNodes = intersect(as.character(allNodes), names(V(ko.graph.sel)))
    sg = subgraph(ko.graph.sel, vids = selNodes)
    con.ko = c(con.ko, length(E(sg)))
  }
  # for modules in condition 2 (ko)
  chrlist = ko.dat$chr
  strlist = ko.dat$module_start
  endlist = ko.dat$module_end
  con.wt = c()
  for(i in 1:nrow(ko.dat))
  {
    ch = chrlist[i]
    st = strlist[i]
    en = endlist[i]
    
    wt.graph.sel = wt.graph[[paste0('graph_', ch)]]
    allNodes = seq(st,en,resolution)
    selNodes = intersect(as.character(allNodes), names(V(wt.graph.sel)))
    sg = subgraph(wt.graph.sel, vids = selNodes)
    con.wt = c(con.wt, length(E(sg)))
  }
  
  wt.dat[['idx']] = 1:nrow(wt.dat)
  ko.dat[['idx']] = 1:nrow(ko.dat)
  
  wt.dat[[paste0('connectivity_in_', prefix.ko)]] = con.ko
  ko.dat[[paste0('connectivity_in_', prefix.wt)]] = con.wt
  
  wt.dat$connectivity_difference = wt.dat$connectivity - con.ko
  wt.dat$connectivity_foldchange = (wt.dat$connectivity+1)/(con.ko+1)
  
  ko.dat$connectivity_difference = ko.dat$connectivity - con.wt
  ko.dat$connectivity_foldchange = (ko.dat$connectivity+1) / (con.wt+1)
  
  wt.dat = wt.dat[order(wt.dat$connectivity_difference, decreasing=T),]
  ko.dat = ko.dat[order(ko.dat$connectivity_difference, decreasing=T),]
  
  return(list(wt = wt.dat, ko = ko.dat))
}


#' Estimation of elbow point from J-shaped-curve
#'
#' Estimation of elbow point from J-shaped curve
#'
#' @author Sora Yoon, PhD
#' @description It provides a point of the highest curvature from a J-shaped-plot
#' @param numbers Numeric vector
#' @return A \code{list} containing two elements: \code{index}, the index of a point in a sorted vector of numbers in descending order, representing the point where the tangent is closest to one, and \code{ConnectivityCutoff}, the corresponding value at that point.
#' @importFrom pracma gradient
#' @importFrom signal sgolayfilt
#' @import HiCocietyExample
#' @examples
#' modulefile = system.file('extdata','mouse_naiveCD4T_Vahedi_short.rds',
#' package = 'HiCocietyExample')
#' mycom = readRDS(modulefile)
#' connec = mycom$ModuleSummary$connectivity
#' getElbowPoint(connec)
#'@export
getElbowPoint = function(numbers)
{
  df <- data.frame(x=1:length(numbers), y=sort(numbers, decreasing=T))
  x <- df$x
  y <- df$y
  min_max_normalize <- function(x) {
    (x - min(x)) / (max(x) - min(x))
  }
  x_norm <- min_max_normalize(x)
  y_norm <- min_max_normalize(y)
  y_smooth <- sgolayfilt(y_norm, p=5, n=11)
  dy_smooth <- gradient(y_smooth, x_norm)
  abs_dy_smooth = sgolayfilt(abs(dy_smooth),p=3,n=11)
  target_idx = which.min(abs(1-abs_dy_smooth))
  return(list(index = df$x[target_idx], ConnectivityCutoff = df$y[target_idx]))
}

#' Visualization of module
#'
#' Visualization of module
#'
#' @author Sora Yoon, PhD
#' @description It draws a triangle heatmap and arcplot of a module
#' @param hicpath Path to the .hic file
#' @param HC.object The object name from hic2community result
#' @param moduleNum The row index of module to draw
#' @param resolution Resolution of HiC data
#' @param hic.norm Normalization method. If not, set 'NONE'
#' @param heatmap.color.range Min and max value of contact frequency, e.g., c(0,10)
#' @param heatmap.color Color for heatmap. For example, colorRampPalette(c("white","red))
#' @param arc.depth Height of arc plot
#' @param arc.color Arc color
#' @param nbnom.param Negative binomial probability cutoff. Higher cutoff gives less number of arcs.
#' @param txdb Character. One of Txdb list obtained from get_txdb().
#' @param gene.strand.arrow.lwd Numeric. Line width of arrowhead indicating the strands of genes. Same as arr.lwd option in Arrows function in shape package.
#' @param gene.strand.lwd Numeric. Line width of arrow body indicating the strands of genes. Same as lwd option in Arros function in shape package.
#' @param col.forward.gene Character. Color of arrows within gene track for forward genes.
#' @param col.reverse.gene Character. Color of arrows within gene track for reverse genes
#' @param highlight.centrality Boolean input to set if highlight eigenvector centrality node.
#' @param highlight.cent.col The color of arcs stemming from the centrality node.
#' @param highlight.node The coordiante of a node of which the user will highlight the arcs stemming from this node. Default=NULL
#' @param highlight.node.col The color of arcs stemming from the node which the user highlight.
#' @param show.sig.int Boolean. If TRUE, it marks significant contact on the triangle heatmap.
#' @param netinfo Boolean. If TRUE, it shows network information of the module as text in the plot.
#' @return No return value; the function generates a plot.
#' @import shape
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics abline lines par points polygon segments text
#' @importFrom stats end start
#' @import GenomicFeatures
#' @import strawr
#' @import igraph
#' @import HiCocietyExample
#' @examples
#' # A slow example that takes too long to run, wrapped in donttest{}
#' \donttest{
#' myhic = system.file('extdata','example.hic',package = 'HiCocietyExample')
#' HC.object = hic2community(myhic, "19", 5000, 0.975, 2000000, par.noise=1,
#' 'louvain', n_cores=2)
#' mNum = 1
#' visualizeModule(hicpath = myhic, HC.object = HC.object, moduleNum = mNum,
#' resolution = 5000,
#' hic.norm = 'NONE', heatmap.color.range=c(0,10),
#' heatmap.color = colorRampPalette(c('white','red')),
#' arc.depth=10, arc.color = "gray80", nbnom.param=0.99,
#' txdb = 'TxDb.Mmusculus.UCSC.mm10.knownGene',
#' gene.strand.arrow.lwd = 3, gene.strand.lwd = 3,
#' col.forward.gene = 'purple', col.reverse.gene = 'pink',
#' highlight.centrality=FALSE, highlight.cent.col=FALSE,
#' highlight.node=NULL, highlight.node.col=NULL,
#' show.sig.int=FALSE, netinfo=FALSE)
#' }
#' @export
visualizeModule <- function(hicpath, HC.object, moduleNum, resolution, hic.norm, heatmap.color.range=NULL, heatmap.color = colorRampPalette(c('white','red')),
                            arc.depth=10, arc.color = "gray80", nbnom.param=0.99, txdb = 'TxDb.Mmusculus.UCSC.mm10.knownGene', gene.strand.arrow.lwd = 3,
                            gene.strand.lwd = 6, col.forward.gene = 'purple', col.reverse.gene = 'pink', highlight.centrality=FALSE, highlight.cent.col=FALSE, highlight.node=NULL, highlight.node.col=NULL, show.sig.int=TRUE, netinfo){
  
  old_par = par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  old_opts = options("scipen")
  on.exit(options(old_opts), add = TRUE)
  options(scipen = 999)
  maptocolors <- function(vec,col,num=100,range=NULL)
  {
    if (is.null(range) == TRUE)
    {
      breaks <- seq(min(vec), max(vec),length.out=num)
    }
    if (is.null(range) == FALSE)
    {
      vec[which(vec < range[1])] = range[1]
      vec[which(vec > range[2])] = range[2]
      breaks <- seq(range[1], range[2],length.out=num)
    }
    
    cols <- col(length(breaks) + 1)
    colvec = as.character(cut(vec, c(-Inf, breaks, Inf), labels=cols))
    return(colvec)
  }
  
  N = as.numeric(moduleNum)
  r = resolution
  chr = HC.object$ModuleSummary$chr[N]
  chr = gsub("chr","",chr)
  chromstart = as.numeric(as.character(HC.object$ModuleSummary$module_start[N]))
  chromend = as.numeric(as.character(HC.object$ModuleSummary$module_end[N]))
  H = arc.depth
  max_y = (chromend - chromstart) / 2 / r + H + 20
  stepsize = r / 2
  hic = straw(norm = hic.norm, fname = hicpath, chr1loc = as.character(chr), chr2loc= as.character(chr), unit = "BP", binsize = r)
  myidx = which(hic$x >= chromstart & hic$y <= chromend)
  myhic = hic[myidx,]
  min_val = min(myhic$x)
  max_val = max(myhic$y)
  cols = rows = seq(min_val, max_val, r)
  
  graph.names = names(HC.object$Graphs)
  my.graph.name = paste('graph',chr,sep="_")
  graph.idx = which(graph.names == my.graph.name)
  tot.graph = HC.object$Graphs[[graph.idx]]
  my.compo = HC.object$ModuleElements[[N]]
  my.graph = subgraph(tot.graph, as.character(my.compo))
  my.graph.dat = igraph::as_data_frame(my.graph)
  filt.dat = my.graph.dat[which(my.graph.dat$counts.y > nbnom.param),]
  
  
  mymat = matrix(0, length(rows), length(cols))
  sigmat = matrix(FALSE, length(rows), length(cols))
  for(i in 1:nrow(myhic))
  {
    idx1 = which(cols == myhic$x[i])
    idx2 = which(rows == myhic$y[i])
    mymat[idx1,idx2] = mymat[idx2, idx1] = myhic$counts[i]
  }
  for(i in 1:nrow(filt.dat))
  {
    idx1 = which(cols == filt.dat$from[i])
    idx2 = which(rows == filt.dat$to[i])
    sigmat[idx1,idx2] = sigmat[idx2, idx1] = TRUE
  }
  hicm = mymat
  if(!is.null(heatmap.color.range)){
    max_z = heatmap.color.range[2]
    min_z = heatmap.color.range[1]
  }else{
    max_z = max(hicm)
    min_z = min(hicm)
  }
  hicmcol = matrix(maptocolors(hicm, heatmap.color, num=100, range = c(min_z, max_z)), nrow = nrow(hicm))
  
  # Empty plot
  plot(1,1,xlim=c(chromstart,chromend), ylim=c(0, max_y), type = 'n', xaxs = 'i', yaxs ='i', bty='n', xaxt='n', yaxt='n', xlab="",ylab="")
  # Fill plot
  for(rownum in 1:nrow(hicm)){
    y = H + 15 - 0.5
    x = chromstart + (rownum * 2 * stepsize) - (stepsize * 2)
    for(colnum in rownum:ncol(hicm))
    {
      x = x+stepsize
      y = y+.5
      xs = c(x-stepsize, x, x+stepsize, x, x-stepsize)
      ys = c(y, y+.5, y, y-.5, y)
      highlight.sig.int = sigmat[rownum,colnum]
      if(highlight.sig.int & show.sig.int){
        polygon(xs,ys, border='black', col = hicmcol[rownum,colnum])
      }else{
        polygon(xs,ys, border=NA, col = hicmcol[rownum,colnum])
      }
    }
  }
  # arcplot
  graph.names = names(HC.object$Graphs)
  my.graph.name = paste('graph',chr,sep="_")
  graph.idx = which(graph.names == my.graph.name)
  tot.graph = HC.object$Graphs[[graph.idx]]
  my.compo = HC.object$ModuleElements[[N]]
  my.graph = subgraph(tot.graph, as.character(my.compo))
  my.graph.dat = igraph::as_data_frame(my.graph)
  filt.dat = my.graph.dat[which(my.graph.dat$counts.y > nbnom.param),]
  message("size of filtered data is:", nrow(filt.dat))
  start_points = list()
  end_points = list()
  for(i in 1:nrow(filt.dat))
  {
    start_points = append(start_points, list(c(as.numeric(as.character(my.graph.dat$from[i])), H+2)))
    end_points = append(end_points, list(c(as.numeric(as.character(my.graph.dat$to[i])), H+2)))
  }
  # Function to calculate the radius given the height H and chord length
  calculate_radius <- function(H, chord_length) {
    return((H^2 + (chord_length / 2)^2) / (2 * H))
  }
  # Calculate the radius for each arc and generate points
  arcs <- lapply(1:nrow(filt.dat), function(i) {
    start <- start_points[[i]]
    end <- end_points[[i]]
    # Calculate the chord length
    chord_length <- sqrt((end[1] - start[1])^2 + (end[2] - start[2])^2)
    # Calculate the radius
    radius <- calculate_radius(H, chord_length)
    # Calculate the midpoint of the chord
    mid_x <- (start[1] + end[1]) / 2
    mid_y <- (start[2] + end[2]) / 2
    # Calculate the center of the circle
    if (start[2] == end[2]) {
      center_x <- mid_x
      center_y <- mid_y + sqrt(radius^2 - (chord_length / 2)^2)
    } else if (start[1] == end[1]) {
      center_x <- mid_x + sqrt(radius^2 - (chord_length / 2)^2)
      center_y <- mid_y
    } else {
      slope <- (end[2] - start[2]) / (end[1] - start[1])
      perp_slope <- -1 / slope
      dx <- sqrt(radius^2 - (chord_length / 2)^2) / sqrt(1 + perp_slope^2)
      dy <- perp_slope * dx
      center_x <- mid_x + sign(H) * dx
      center_y <- mid_y + sign(H) * dy
    }
    # Calculate angles
    start_angle <- atan2(start[2] - center_y, start[1] - center_x)
    end_angle <- atan2(end[2] - center_y, end[1] - center_x)
    # Generate points for the arc
    theta <- seq(start_angle, end_angle, length.out = 100)
    x <- center_x + radius * cos(theta)
    y <- center_y + radius * sin(theta)
    list(x = x, y = y)
  })
  
  par(new=T)
  for (i in 1:nrow(filt.dat)) {
    graphics::lines(arcs[[i]]$x, arcs[[i]]$y, col = arc.color, lwd = 1)
  }
  # Optionally, add the start and end points
  for (i in 1:nrow(filt.dat)) {
    graphics::points(start_points[[i]][1], start_points[[i]][2], pch = 16, col = arc.color)
    graphics::points(end_points[[i]][1], end_points[[i]][2], pch = 16, col = arc.color)
  }
  
  if(highlight.centrality)
  {
    centra = HC.object$ModuleSummary$centrality_node[N]
    idx = which(filt.dat$from == centra | filt.dat$to == centra)
    for(i in idx){
      lines(arcs[[i]]$x, arcs[[i]]$y, col = highlight.cent.col, lwd = 1)
      points(start_points[[i]][1], start_points[[i]][2], pch = 16, col = highlight.cent.col)
      points(end_points[[i]][1], end_points[[i]][2], pch = 16, col = highlight.cent.col)
    }
  }
  
  if(!is.null(highlight.node))
  {
    hl = highlight.node
    idx = which(filt.dat$from == hl | filt.dat$to == hl)
    if(length(idx)==0){warning("No such nodes in the module interaction list.")
    }else{
      for(i in idx){
        graphics::lines(arcs[[i]]$x, arcs[[i]]$y, col = highlight.node.col, lwd = 1)
        graphics::points(start_points[[i]][1], start_points[[i]][2], pch = 16, col = highlight.node.col)
        graphics::points(end_points[[i]][1], end_points[[i]][2], pch = 16, col = highlight.node.col)
      }
    }
  }
  # add gene track
  if(substr(chr,1,1) !="c"){chr = paste0("chr",chr)}
  gr = GRanges(
    seqnames = chr,
    ranges = IRanges(start = as.numeric(chromstart), end = as.numeric(chromend))
  )
  if(is.null(txdb)){stop("Provide a proper txdb object.")}
  org_mapping <- list(
    "TxDb.Hsapiens.UCSC.hg38.knownGene" = "org.Hs.eg.db",
    "TxDb.Mmusculus.UCSC.mm10.knownGene" = "org.Mm.eg.db",
    "TxDb.Rnorvegicus.UCSC.rn6.knownGene" = "org.Rn.eg.db",
    "TxDb.Dmelanogaster.UCSC.dm6.ensGene" = "org.Dm.eg.db",
    "TxDb.Scerevisiae.UCSC.sacCer3.sgdGene" = "org.Sc.sgd.db",
    "TxDb.Athaliana.BioMart.plantsmart28" = "org.At.tair.db",
    "TxDb.Btaurus.UCSC.bosTau9.refGene" = "org.Bt.eg.db",
    "TxDb.Cfamiliaris.UCSC.canFam3.refGene" = "org.Cf.eg.db",
    "TxDb.Celegans.UCSC.ce11.refGene" = "org.Ce.eg.db",
    "TxDb.Drerio.UCSC.danRer10.refGene" = "org.Dr.eg.db",
    "TxDb.EcoliK12.UCSC.ecoliK12.refGene" = "org.EcK12.eg.db",
    "TxDb.Ggallus.UCSC.galGal5.refGene" = "org.Gg.eg.db",
    "TxDb.Pfalcifarum.PlasmoDB.v46" = "org.Pf.plasmo.db",
    "TxDb.Sscrofa.UCSC.susScr11.refGene" = "org.Ss.eg.db",
    "TxDb.Xlaevis.UCSC.xenTro9.refGene" = "org.Xl.eg.db"
  )
  
  # Get the corresponding org package name
  orgPkg <- org_mapping[[txdb]]
  
  # Load the org package dynamically
  orgdb <- NULL
  packagecheck <- check_package(orgPkg)
  
  if (packagecheck) {
    # Try to get the OrgDb object from the package namespace
    orgdb <- tryCatch({
      getExportedValue(orgPkg, orgPkg)
    }, error = function(e) {
      warning(sprintf("Package '%s' is installed, but the OrgDb object could not be loaded. Gene symbols will not be annotated.", orgPkg))
      return(NULL)
    })
  } else {
    warning(sprintf("Package '%s' not found. Please install and load a proper package. Gene symbols will not be annotated.", orgPkg))
  }
  
  txdb <- getFromNamespace(txdb, asNamespace(txdb))
  Genes = genes(txdb)
  overlaps = findOverlaps(gr, Genes)
  gene_ids = Genes$gene_id[subjectHits(overlaps)]
  gene_rg = Genes[subjectHits(overlaps)]
  gene_symbols = mapIds(orgdb, keys = gene_ids , column = "SYMBOL", keytype="ENTREZID", multiVals = "first")
  gene_rg$gene_symbol = gene_symbols
  abline(h=c(H + 2))
  coord_write = round(seq(chromstart/1000000, chromend/1000000, .1),1)
  coord_orig  = coord_write * 1000000
  segments(x0 = c(chromstart,coord_orig,chromend), y0 = H+1.5, y1 = H+2.5)
  text(x = coord_orig, y=H+4.5, labels = coord_write)
  text(x = chromstart+r, y = H+5.5, labels = chr)
  text(x = chromend-r, y = H+5.5, labels = "Mb")
  
  for(g in 1:length(gene_rg))
  {
    tra = gene_rg[g]
    st = GenomicRanges::start(tra)
    en = GenomicRanges::end(tra)
    gsy = tra$gene_symbol
    mystrands = as.character(tra@strand@values)
    
    if(mystrands == "+"){shape::Arrows(x0 = st, x1 = en, y0 = H+8, y1 = H+8, arr.type = 'triangle', arr.lwd=gene.strand.arrow.lwd, lwd=gene.strand.lwd, col=col.forward.gene)}
    if(mystrands == "-"){shape::Arrows(x0 = en, x1 = st, y0 = H+8, y1 = H+8, arr.type = 'triangle', arr.lwd=gene.strand.arrow.lwd, lwd=gene.strand.lwd, col=col.reverse.gene)}
    text(x = ((st + en)/2)-10000, y = H+11, labels = gsy)
  }
  if(netinfo)
  {
    x0 = chromstart+r*10
    y0 = max_y
    text(x0, y0-3, paste0("Rank:",N), pos=4)
    text(x0, y0-9, paste0("Connectivity:",HC.object$ModuleSummary$connectivity[N]), pos=4)
    text(x0, y0-15, paste0("Transitivity:",signif(HC.object$ModuleSummary$transitivity[N],3)), pos=4)
    text(x0, y0-21, paste0("Centrality node:",HC.object$ModuleSummary$centrality_node[N]), pos=4)
  }
}
