#'
#' Obtain number of Cross-Over events per sample and chromosome.
#'
#' @param object a RViterbi object.
#' @return Matrix m x n. M number of samples and N chromosomes.
#'
#' #' @return a matrix with n chromosomes and m samples (n x m) and the number of CO events.
#' @usage calcCOnumber(object)
#'
#' @examples
#'
#' data("fittedExample")
#' co.num = calcCOnumber(myDat)
#'
#' @export calcCOnumber
#'


calcCOnumber = function(object){
  numCO = sapply(object@Viterbi, function(samp){
    sapply(seqlevels(samp), function(chr){
      newsamp = samp[seqnames(samp) == chr]
      sampGR = try(Vit2GrangesGen(newsamp, "Viterbi"), silent = TRUE)
      COs = length(sampGR)-1
      return(COs)
    })
  })
  return(numCO)

}

#'
#' Obtain number of Cross-Over events per sample and chromosome.
#'
#' @param object a RViterbi object.
#' @param file file where to save the plot for CO numbers
#' @return a plot
#'
#'
#' @usage plotCOs(object, file = NULL)
#'
#' @examples
#'
#' data("fittedExample")
#' co.num = calcCOnumber(myDat)
#'
#' @export plotCOs
#'

plotCOs = function(object, file = NULL){
  Cos = calcCOnumber(object = object)
  Cos = melt(Cos)
  rev.newn = object@info$expDesign$OName
  names(rev.newn) = object@info$expDesign$name
  colnames(Cos) = c("Chr", "Sample", "value")
  Cos$Sample = rev.newn[Cos$Sample]
  p <- ggplot(Cos, aes( x = factor(Chr), y = value)) +
    geom_boxplot() +
    # theme(legend.position = "none")+
    xlab("chromosome") +
    ylab("Number of COs")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  if(!is.null(file)) pdf(file)
  print(p)
  if(!is.null(file)) dev.off()
}
