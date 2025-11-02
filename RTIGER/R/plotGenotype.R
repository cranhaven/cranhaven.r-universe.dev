#### Plot functions for each chromosome and sample
#### 27/04/2019
#### by Rafael Campos Martin


#'
#' This funciton creates an RViterbi object.
#'
#' @param object an RTIGER or RViterbi object with the filtered data
#' @param samp character with the sample name as specified in the experimentalDesign
#' @param chr character with the chromosome to plot
#' @param col color to define segments in P1, P2 and heterozygous regions.
#' @param size size of each  panel (see Gviz documentation)
#' @param main an overall title for the plot. If null, the sample and chromosome are the default.
#' @param showGenAxis boolean parameter whether or not show the genome axis.
#' @param ratio boolean parameter wether or not to plot the ratio counts
#' @param window if ratio TRUE, what is the window for the sliding window to be computed of the ratios.
#' @param from If specified, which to start plotting. Default is one.
#' @param to If specified, till which position to plot. Default is final chromosomoe.
#'
#' @return plot with the genomic segmentation for chromosome chr in sample samp.
#' @usage plotGenotype(object, samp, chr,
#' col = c("red", "blue", "mediumorchid4"),
#' size = c(1,1), main = NULL,
#' showGenAxis = TRUE, ratio = FALSE, window = 10)
#'
#' @examples
#'
#' data("fittedExample")
#' info = myDat@info
#' plotGenotype(myDat, samp = info$sample_names[1],chr = info$part_names[1])
#'
#' @keywords internal
#' @noRd
#'
#'


plotGenotype = function(object,
         samp,
         chr,
         col = c("red", "blue",  "mediumorchid4"),
         size = c(1,1),
         main = NULL,
         showGenAxis = TRUE,
         ratio = FALSE,
         window = 10,
         from = NULL,
         to = NULL){

  # if(sum(c("Gviz") %in% rownames(installed.packages())) != 1) stop("To generate this plot you need to have installed Gviz Bioconductor package.\n
  #                                                                  https://bioconductor.org/packages/release/bioc/html/Gviz.html")
  requireNamespace("Gviz")

  DataViterbi_GR = object@Viterbi

  FinalRes_chr = DataViterbi_GR[[samp]]
  FinalRes_chr = FinalRes_chr[seqnames(FinalRes_chr) == chr]

  if(ratio){
    ratio_V = FinalRes_chr$P1.Allele.Count/FinalRes_chr$total
    ratio_V = ((ratio_V - .5)/.5)*100
    ratio_V = myrunningMean(ratio_V, window)
    posRatio = ratio_V
    posRatio[posRatio < 0] = NA
    negRatio = ratio_V
    negRatio[negRatio >= 0 ] = NA
    # "P1.Allele.Count"
    # rat io[is.na(ratio)] = 0
    # FinalRes_chr$ratio = ratio_V

    myratio = FinalRes_chr
    myratio$P1.Allele.Count = posRatio
    myratio$P2.Allele.Count = negRatio
    myratio = myratio[,c("P2.Allele.Count", "P1.Allele.Count")]
    ratlim = c(-100,100)
    ratgviz = Gviz::DataTrack(myratio, type = "l", col = col[1:2], name = "Allele frequency Ratio", ylim = ratlim)
    size = c(1,1,1)

  }

  Viterbi = Vit2GrangesGen(FinalRes_chr, value = "Viterbi")


  dat = FinalRes_chr[,c("P1.Allele.Count", "total")]
  dat$P2.Allele.Count = - (dat$total - dat$P1.Allele.Count)
  dat = dat[,c("P1.Allele.Count", "P2.Allele.Count")]
  colnames(mcols(dat)) = c( "Reference", "Alternate")
  lim = max(abs(as.matrix(mcols(dat))), na.rm = TRUE) + 2

  coldata = col[1:2]
  names(coldata) = c("Alternate", "Reference")
  mcols(dat) = mcols(dat)[, c("Alternate", "Reference")]
  datgviz = Gviz::DataTrack(dat, type = "h", name = "Allele frequency", col = coldata, ylim = c(-lim, lim))

  names(col) = c("mat", "pat", "het")

  myclass = "Genotype"

  vitGviz = Gviz::AnnotationTrack(Viterbi, name = myclass,
                            fill = col[Viterbi$Viterbi],
                            col = "transparent", stacking = "dense")

  mySize = size
  mytracks =  c(datgviz, vitGviz)
  if(ratio){
    mytracks = c(datgviz, ratgviz, vitGviz)
  }

  if(showGenAxis){
    gtrack <- Gviz::GenomeAxisTrack()
    mytracks = c(gtrack, mytracks)
    mySize = c(.5,size)

  }

  if(is.null(main)){
    sample.name = samp
    if("OName" %in%  colnames(object@info$expDesign)){
      rev.newn = object@info$expDesign$OName
      names(rev.newn) = object@info$expDesign$name
      sample.name = rev.newn[samp]
    }
    main = paste(sample.name, "\n", chr)
  }
  to = ifelse(is.null(to), seqlengths(FinalRes_chr)[chr], to)
  from = ifelse(is.null(from), 1, from)
  Gviz::plotTracks(mytracks, groups = colnames(mcols(dat)),
             background.title="darkgrey", lwd=2,
             from = from,
             to=  to,
             main = main,
             sizes=mySize,
             legend = TRUE,
             showFeatureId=FALSE,
             fontcolor.feature="black",  background.title="darkgrey",
             showId=TRUE, cex.main = 0.7)

}


myrunningMean = function(x, winHalfSize = 2)
{
  out = x
  for (i in 1:length(x)) {
    start = (max(c(1, i - winHalfSize)))
    end = (min(c(length(x), i + winHalfSize)))
    out[i] = mean(x[start:end], na.rm = TRUE)
  }
  out
}


Vit2GrangesGen = function (FinalRes_chr, value)
{
  chr = seqnames(FinalRes_chr)[1]
  myRle = rle(mcols(FinalRes_chr)[[value]])
  len = myRle$lengths
  vals = myRle$values
  cum = len[1]
  if(length(len) > 1){
    for(i in 2:length(len)) cum[i] = cum[i-1] + len[i]
  }
  starts = start(FinalRes_chr)
  ends = end(FinalRes_chr)
  good.ends = ends[cum]
  good.start = vector(length = length(len))
  good.start[1] = starts[1]
  if(length(len) > 1) good.start[2:length(good.start)] = starts[cum[-length(cum)] + 1]
  myGR = data.frame(seqnames = rep(chr, length(good.start)),
                    start = good.start,
                    end = good.ends)
  myGR[[value]] = vals
  myGR = GRanges(myGR)
  return(myGR)
}
