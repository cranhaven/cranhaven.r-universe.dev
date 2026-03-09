featureRep=function(fgrid,df){
 if(is(fgrid,"RasterLayer")){
  df$y=extract(fgrid,df)#x.over[,1]
  options(digits=1)
  dist.histbb <- histbackback(df$y, values(fgrid), font.lab=5 ,probability=TRUE, xlab=c("Sample Points","Feature Map"), ylab = "Data range")

 }
  else if(is(fgrid,"SpatialGridDataFrame")){
    x.over=over(df,fgrid)
    df$y=x.over[,1]
    dist.histbb <- histbackback(df$y, fgrid@data[,1], font.lab=5 ,probability=TRUE, xlab=c("Sample Points","Feature Map"), ylab = "Data range")
  }
  else {
    dist.histbb <- histbackback(fgrid, df, font.lab=5 ,probability=TRUE, xlab=c("Sample Points","Feature Map"), ylab = "Data range")

  }
  barplot(-dist.histbb$left, col="dark grey" , horiz=TRUE, space=0, add=TRUE,axes=FALSE)
  barplot(dist.histbb$right, col="blue", horiz=TRUE, space=0, add=TRUE, axes=FALSE)
  ks.test(dist.histbb$left, dist.histbb$right, exact = FALSE)
}
