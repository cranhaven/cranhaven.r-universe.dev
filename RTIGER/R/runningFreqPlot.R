running.freq = function(x, tiles = 4e5, info){

  listgr = vector("list", info$part_nr)
  names(listgr) = names(x[[1]])
  for(chr in names(listgr)){
    myt = tileGenome( tilewidth = tiles,
                      cut.last.tile.in.chrom=TRUE, seqlengths = seqlengths(x[[1]][[chr]])[chr])
    for(samp in names(x)){
      hits = findOverlaps(x[[samp]][[chr]], myt)
      tp = table(hits@to)
      mcols(myt)[,samp ] = 0
      mcols(myt)[[samp]][as.numeric(names(tp))] = tp
    }
    myt$co.freq = apply(mcols(myt), 1, function(x) sum(x)/(tiles/1e3))
    listgr[[chr]] = myt
  }
  return(listgr)
}

