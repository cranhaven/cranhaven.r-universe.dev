createTexturedata=function(clay, silt, sand){
  total <- 20
  pb = txtProgressBar(min = 0, max = total, style = 3)
  if(is(clay,"SpatialGridDataFrame")){
    clay=as(clay,"RasterLayer")
    silt=as(silt, "RasterLayer")
    sand=as(sand, "RasterLayer")
    soilc=stack(clay,silt,sand)
    soilp =as(soilc, 'SpatialPixelsDataFrame')
    names(soilp@data) =c('CLAY', 'SILT', 'SAND')
    soilp@data =round(soilp@data, 2)
    soilp@data$raw_totals =rowSums(soilp@data[, 1:3])
    soilp_norm =TT.normalise.sum(tri.data = soilp@data, residuals = T)
    colnames(soilp_norm)[1:3] =paste0(colnames(soilp_norm)[1:3], "_n")
    soilp@data =cbind(soilp@data, round(soilp_norm, 2))
    for(i in 1:10){
      Sys.sleep(0.5)
      setTxtProgressBar(pb, i)}
  }

  else if(is(clay,"RasterLayer")){
    soilc=stack(clay,silt,sand)
    soilp =as(soilc, 'SpatialPixelsDataFrame')
    names(soilp@data) =c('CLAY', 'SILT', 'SAND')
    soilp@data =round(soilp@data, 2)
    soilp@data$raw_totals =rowSums(soilp@data[, 1:3])
    soilp_norm =TT.normalise.sum(tri.data = soilp@data, residuals = T)
    colnames(soilp_norm)[1:3] =paste0(colnames(soilp_norm)[1:3], "_n")
    soilp@data =cbind(soilp@data, round(soilp_norm, 2))

    for(i in 10:total){
      Sys.sleep(0.5)
      setTxtProgressBar(pb, i)}
  }
  else {

    soilp=as.data.frame(cbind(clay,silt,sand))
    names(soilp)=c("CLAY","SILT","SAND")
    soilp=round(soilp,2)
    soilp$raw_totals <- rowSums(soilp[, 1:3])
    SSCP_norm = TT.normalise.sum(tri.data = soilp, residuals = T)
    colnames(SSCP_norm)[1:3] <- paste0(colnames(SSCP_norm)[1:3], "_n")
    soilp = cbind(soilp, round(SSCP_norm, 2))
    for(i in 10:total){
      Sys.sleep(0.5)
      setTxtProgressBar(pb, i)}

  }
  return(soilp)
}
