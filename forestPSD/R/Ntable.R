Ntable<-function(ax){
  rank=1:length(ax)
  ageclass=as.character(utils::as.roman(rank))
  ax=ax
  return(data.frame(rank,ageclass,ax))
}
