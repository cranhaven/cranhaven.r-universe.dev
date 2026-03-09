classLUT=function(fgrid, indicator="salinity"){
  total <- 20
  if(indicator=="saltseverity"){

    fgrid@data$value=as.character(fgrid@data[,1])
    fgrid@data$mapvalue=as.factor(fgrid@data$value)
    fgrid@data$classcode=as.numeric(fgrid@data[,1])
    for(i in 1:12){
      Sys.sleep(0.5)
      setTxtProgressBar(txtProgressBar(min = 0, max = total, style = 3), i)
    }
    LUT = data.frame(unique(fgrid@data[, c('classcode', 'mapvalue')]))

    for(i in 12:total){
      Sys.sleep(0.5)
      setTxtProgressBar((txtProgressBar(min = 0, max = total, style = 3)), i)
    }

  }
  else if(indicator=="texture"){

    fgrid@data$value=as.character(fgrid@data[,1])
    fgrid@data$mapvalue=as.factor(fgrid@data$value)
    fgrid@data$classcode=as.numeric(fgrid@data[,1])
    for(i in 1:12){
      Sys.sleep(0.5)
      setTxtProgressBar((txtProgressBar(min = 0, max = total, style = 3)), i)
    }
    LUT = data.frame(unique(fgrid@data[, c('classcode', 'mapvalue')]))

    for(i in 12:total){
      Sys.sleep(0.5)
      setTxtProgressBar((txtProgressBar(min = 0, max = total, style = 3)), i)
    }

  }
  else if(indicator=="suitability"){

    fgrid@data$value=as.character(fgrid@data[,1])
    fgrid@data$mapvalue=as.factor(fgrid@data$value)
    fgrid@data$classcode=as.numeric(fgrid@data[,1])
    for(i in 1:12){
      Sys.sleep(0.5)
      setTxtProgressBar((txtProgressBar(min = 0, max = total, style = 3)), i)
    }
    LUT = data.frame(unique(fgrid@data[, c('classcode', 'mapvalue')]))

    for(i in 12:total){
      Sys.sleep(0.5)
      setTxtProgressBar((txtProgressBar(min = 0, max = total, style = 3)), i)
    }

  }
  else if(indicator=="fertility"){

    fgrid@data$value=as.character(fgrid@data[,1])
    fgrid@data$mapvalue=as.factor(fgrid@data$value)
    fgrid@data$classcode=as.numeric(fgrid@data[,1])
    for(i in 1:12){
      Sys.sleep(0.5)
      setTxtProgressBar((txtProgressBar(min = 0, max = total, style = 3)), i)
    }
    LUT = data.frame(unique(fgrid@data[, c('classcode', 'mapvalue')]))

    for(i in 12:total){
      Sys.sleep(0.5)
      setTxtProgressBar((txtProgressBar(min = 0, max = total, style = 3)), i)
    }

  }
  else if(indicator=="drainage"){

    fgrid@data$value=as.character(fgrid@data[,1])
    fgrid@data$mapvalue=as.factor(fgrid@data$value)
    fgrid@data$classcode=as.numeric(fgrid@data[,1])
    for(i in 1:12){
      Sys.sleep(0.5)
      setTxtProgressBar((txtProgressBar(min = 0, max = total, style = 3)), i)
    }
    LUT = data.frame(unique(fgrid@data[, c('classcode', 'mapvalue')]))

    for(i in 12:total){
      Sys.sleep(0.5)
      setTxtProgressBar((txtProgressBar(min = 0, max = total, style = 3)), i)
    }

  }
  else if(indicator=="erodibility"){

    fgrid@data$value=as.character(fgrid@data[,1])
    fgrid@data$mapvalue=as.factor(fgrid@data$value)
    fgrid@data$classcode=as.numeric(fgrid@data[,1])
    for(i in 1:12){
      Sys.sleep(0.5)
      setTxtProgressBar((txtProgressBar(min = 0, max = total, style = 3)), i)
    }
    LUT = data.frame(unique(fgrid@data[, c('classcode', 'mapvalue')]))

    for(i in 12:total){
      Sys.sleep(0.5)
      setTxtProgressBar((txtProgressBar(min = 0, max = total, style = 3)), i)
    }

  }
  return(LUT)
}
