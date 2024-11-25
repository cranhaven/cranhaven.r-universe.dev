appendTextureclass=function(df, method="USDA"){
  if(is((df)[1],"SpatialPointsDataFrame")){
    soilp=df
    total <- 20
    pb = txtProgressBar(min = 0, max = total, style = 3)

    if(method=="USDA"){soilp@data =cbind(soilp@data, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp@data[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "USDA.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp@data$TEXCLASS)}
    else if(method=="European"){soilp@data =cbind(soilp@data, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp@data[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "HYPRES.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp@data$TEXCLASS)}
    else if(method=="French"){ soilp@data =cbind(soilp@data, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp@data[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "FR.AISNE.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp@data$TEXCLASS)}
    else if(method=="German"){ soilp@data =cbind(soilp@data, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp@data[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "DE.BK94.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp@data$TEXCLASS)}
    else if(method=="UK"){ soilp@data =cbind(soilp@data, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp@data[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "UK.SSEW.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp@data$TEXCLASS)}
    else if(method=="Australia"){ soilp@data =cbind(soilp@data, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp@data[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "AU2.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp@data$TEXCLASS)}
    else if(method=="Belgian"){ soilp@data =cbind(soilp@data, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp@data[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "BE.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp@data$TEXCLASS)}
    else if(method=="Canadian"){ soilp@data =cbind(soilp@data, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp@data[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "CA.ET.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp@data$TEXCLASS)}
    else if(method=="FAO"){ soilp@data =cbind(soilp@data, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp@data[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "ISSS.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp@data$TEXCLASS)}
    else if(method=="Romania"){ soilp@data =cbind(soilp@data, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp@data[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "ROM.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp@data$TEXCLASS)}
    else if(method=="Polish"){ soilp@data =cbind(soilp@data, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp@data[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "PL.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp@data$TEXCLASS)}
    else if(method=="Brazil"){ soilp@data =cbind(soilp@data, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp@data[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "BRASIL.TT", PiC.type  = "t",collapse  = ', '))
    }
    for(i in 1:total){
      Sys.sleep(0.5)
      setTxtProgressBar(pb, i)
    }
  }
  else{
    soilp=df
    total <- 20
    pb = txtProgressBar(min = 0, max = total, style = 3)#pb <- winProgressBar(title = "Model processing", min = 0, max = total, width = 300)

    if(method=="USDA"){soilp =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "USDA.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp$TEXCLASS)}
    else if(method=="European"){soilp =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "HYPRES.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp$TEXCLASS)}
    else if(method=="French"){ soilp =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "FR.AISNE.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp$TEXCLASS)}
    else if(method=="German"){ soilp =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "DE.BK94.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp$TEXCLASS)}
    else if(method=="UK"){ soilp =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "UK.SSEW.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp$TEXCLASS)}
    else if(method=="Australia"){ soilp =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "AU2.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp$TEXCLASS)}
    else if(method=="Belgian"){ soilp =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "BE.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp$TEXCLASS)}
    else if(method=="Canadian"){ soilp =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "CA.ET.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp$TEXCLASS)}
    else if(method=="FAO"){ soilp =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "ISSS.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp$TEXCLASS)}
    else if(method=="Romania"){ soilp =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "ROM.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp$TEXCLASS)}
    else if(method=="Polish"){ soilp =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "PL.TT", PiC.type  = "t",collapse  = ', '))
    texclass=levels(soilp$TEXCLASS)}
    else if(method=="Brazil"){ soilp =cbind(soilp, "TEXCLASS" = TT.points.in.classes(tri.data  = soilp[, c('CLAY_n', 'SILT_n',  'SAND_n')],css.names = c('CLAY_n', 'SILT_n', 'SAND_n'),class.sys = "BRASIL.TT", PiC.type  = "t",collapse  = ', '))
    }
    for(i in 1:total){
      Sys.sleep(0.5)
      setTxtProgressBar(pb, i)   #setWinProgressBar(pb, i, title=paste( round(i/total*100, 0),"% done"))
    }
  }
  return(soilp)
}
