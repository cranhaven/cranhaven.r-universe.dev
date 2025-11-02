#'
#' This funciton computes the mean raatio per state and sample
#'
#' @param rtigerobj an RTIGER or RViterbi object with the filtered data
#'
#' @keywords internal
#' @noRd
#'


state_ratios = function(rtigerobj){
  r = rtigerobj@params$rigidity
  viterbi = rtigerobj@Viterbi
  nstates = rtigerobj@params$nstates

  myres = lapply(viterbi, function(samp){
    myx =lapply(seqlevels(samp), function(chr){
      FinalRes_chr = samp[seqnames(samp) == chr]
      states = Vit2GrangesGen(FinalRes_chr = FinalRes_chr, "Viterbi")
    })
    names(myx) = seqlevels(samp)
    return(myx)
  })
  ratios = vector("list", length(viterbi))
  names(ratios) = names(viterbi)
  ratios = lapply(ratios, function(x){

    st = vector("list", 3)
    names(st) = c("pat", "mat", "het")
    return(st)

  } )

  for(samp in names(myres)){
    # cat("in sample: ", samp, "\n")
    for(chr in names(myres[[samp]])){
      # cat("In chromossome: ", chr, "\n")
      myobj = myres[[samp]][[chr]]
      mydat = viterbi[[samp]]
      mydat = mydat[seqnames(mydat) == chr]

      for(st in c("pat", "mat", "het")){
        # cat("in state: ", st, "\n")
        myobjst = myobj[myobj$Viterbi == st]
        if(length(myobjst) > 0){
          for(i in 1:length(myobjst)){
            beg = start(myobjst)[i]
            en = end(myobjst)[i]
            count= mydat[start(mydat) > beg & end(mydat) < en]
            max.l = length(count)
            down = 1
            up = r
            while(up < max.l){
              myr = sum(count$P1.Allele.Count[down:up])/sum(count$total[down:up])
              ratios[[samp]][[st]] = c(ratios[[samp]][[st]], myr)
              down = up + 1
              up = min(up + r +1, max.l)
            }

          }
        }

      }



    }

  }
  ratios = sapply(ratios, function(samp){
    sapply(samp, function(st){mean(st)})
  })
  ratiosvar = sapply(ratios, function(samp){
    sapply(samp, function(st){var(st)})
  })
  ratios = t(ratios)
  ratiosvar = t(ratiosvar)
  ratios = ratios[order(ratios[,1]),]
  return(list(ratios, ratiosvar))

}

