#'
#' Exports the count and viterbi code to a folder in order to visualize it in IGV.
#'
#' @param object an RTIGER or RViterbi object with the filtered data
#' @param sample character with the sample name as specified in the experimentalDesign
#' @param dir character with the chromosome to plot. If left null, it will create a folder with the name of the sample.
#' @param ratio boolean variable. Whether the ratio P1/total should be created as well.
#' @param newn named vector with the new and shorter names for the samples.
#'
#' @return a folder with files to visualize results in igv.
#' @usage export2IGV(object, sample, dir = NULL, ratio = FALSE, newn = NULL)
#' @keywords internal
#' @noRd
#'
#'
export2IGV = function( object, sample, dir = NULL, ratio = FALSE, newn = NULL){
  if(!is.null(dir)){
    if(!dir.exists(dir)) dir.create(dir)

    compdir = file.path(dir, sample)
  }
  else{
    compdir = sample
  }

  # if(dir.exists(dir)) stop("A new directory will be created with the results. \n Please change the name of the directory or delete the existing one.")
  if(!dir.exists(compdir)) dir.create(compdir)
  old_samp = sample

  if(!is.null(newn)) sample = newn[sample]
  requireNamespace("rtracklayer")
  # if(sum(c("rtracklayer") %in% rownames(installed.packages())) != 1) stop("To export to IGV you need to have installed rtracklayer.\n
                                                                          # https://bioconductor.org/packages/release/bioc/html/rtracklayer.html")

  P1.statesfile = file.path(compdir, paste("P1-state-", old_samp, ".bed", sep = ""))
  P2.statesfile = file.path(compdir, paste("P2-state-", old_samp, ".bed", sep = ""))
  Het.statesfile = file.path(compdir, paste("Het-state-", old_samp, ".bed", sep = ""))
  Comp.statesfile = file.path(compdir, paste("CompleteBlock-state-", old_samp, ".bed", sep = ""))

  P1.countsfile = file.path(compdir, paste("P1-countperbin-", old_samp, ".bw", sep = ""))
  P2.countsfile = file.path(compdir, paste("P2-countperbin-", old_samp, ".bw", sep = ""))

  Viterbi = object@Viterbi[[sample]]
  mycomp = sapply(object@info$part_names, function(x){
    myx = Viterbi[seqnames(Viterbi) == x]
    myx = Vit2GrangesGen(myx, "Viterbi")
    return(myx)
  })
  gr = mycomp[[1]]
  for(i in 2:length(mycomp)){
    gr = c(gr,mycomp[[i]])
  }
  goodanno = c("AA", "AB", "BB")
  names(goodanno) = c("pat", "het", "mat")
  df <- data.frame(seqnames=seqnames(gr),
                   starts=start(gr)-1,
                   ends=end(gr),
                   names=c(rep(".", length(gr))),
                   scores=goodanno[elementMetadata(gr)$Viterbi],
                   strands=strand(gr))
  df = df[,-c(4,6)]


  P1.states = Viterbi[Viterbi$Viterbi == "pat"][, "Viterbi"]
  P2.states = Viterbi[Viterbi$Viterbi == "mat"][, "Viterbi"]
  Het.states = Viterbi[Viterbi$Viterbi == "het"][, "Viterbi"]

  P1.count = Viterbi[,"P1.Allele.Count"]
  P1.count$score = P1.count$P1.Allele.Count
  P1.count$score[is.na(P1.count$score)] = 0
  P2.count = Viterbi[,"total"]
  P2.count$score = Viterbi$total - Viterbi$P1.Allele.Count
  # P2.count = Viterbi[,"P2.Allele.Count"]
  # P2.count$score = P2.count$P2.Allele.Count
  P2.count$score[is.na(P2.count$score)] = 0
  if(ratio){
    ratiofile = file.path(compdir, paste("Count-ratio-", old_samp, ".bw", sep = ""))
    ratio = Viterbi$P1.Allele.Count/Viterbi$total
    ratio = (ratio - .5)/.5
    ratio[is.na(ratio)] = 0
    Viterbi$score = ratio

    rtracklayer::export.bw(Viterbi[,"score"], ratiofile)
  }
  write.table(df, file= Comp.statesfile, quote=FALSE, sep="\t", row.names=FALSE, col.names=FALSE)
  rtracklayer::export.bed(P1.states, P1.statesfile)
  rtracklayer::export.bed(P2.states, P2.statesfile)
  rtracklayer::export.bed(Het.states, Het.statesfile)

  rtracklayer::export.bw(P1.count, P1.countsfile)
  rtracklayer::export.bw(P2.count, P2.countsfile)
}
