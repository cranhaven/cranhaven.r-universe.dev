MakeSnapshots <-
function(info, outputdir = './anim', prefix = 'snapshot', nums = 1:info$N, dt = 0.1, makesec1 = TRUE, makesec2 = TRUE, makesurf = TRUE, res = 96, fn_topomap = 'topomap.png', width = 480, height = NaN, asp = 1, pointsize = 12){


prettytitle = function(x, m1, m2){
  ## function to print the time as a plot title with the right formatting (spaces before first non-zero, leading zero before decimal, right number of decimals, right width)
  ## x: number to print
  ## m1: smallest possible increment between numbers
  ## m2: largest possible number

  digits = getOption('digits')
  options(digits = 22)
  nd2 = floor(log(m2, 10)) + 1 ## number of digits before decimal in largest number
  nd1 = nchar(as.character(m1)) - regexpr('\\.', m1)

  ## make a number with the right number of decimal places and leading zeroes
  start = 7 - (nd2-1)
  end = 7 + nd1 + (m1 < 1)
  y=substr(as.character(x+1e6+1e-6), start, end)

  ## substitute spaces for leading zeroes when appropriate
  if(nd2 >= 2){
    for(i in 1:(nd2-1)){
      if(x < 10^(nd2-i)){
        substr(y, i, i) = ' '
      }
    }
  }
  options(digits = digits)
  y
}



  N = info$N
  topo = info$topo
  coords_sec1 = info$coords_sec1
  coords_sec2 = info$coords_sec2
  coords_surf = info$coords_surf
  surfline_sec1 = info$surfline_sec1
  surfline_sec2 = info$surfline_sec2
  mai=c(0.8,0.8,0.2,0.2)

  zz_sec1 = unique(coords_sec1[,3])
  zz_sec2 = unique(coords_sec2[,3])
  dist_sec2 = surfline_sec2$h
  
  if(is.na(height)){
    width_plot = width - res * (mai[2]+mai[4])
    if(makesec1){
      height_sec1 = diff(range(zz_sec1))/max(surfline_sec1$h) * width_plot + res * (mai[1]+mai[3])
    }
    if(makesec2){
      height_sec2 = diff(range(zz_sec2))/max(dist_sec2) * width_plot + res * (mai[1]+mai[3])
    }
    height_surf = diff(range(topo$y))/diff(range(topo$x)) * width_plot + res * (mai[1]+mai[3])
  }
  
  ## color scheme
  mycols = colorRampPalette(c('darkblue','blue', 'white', 'orange', 'red'),bias=1,space=c('rgb'))

  ## Image map for surface topography
  png(filename=fn_topomap,width=4,height=length(topo$y)/length(topo$x)*4,units='in',res=200)
  par(omi=c(0,0,0,0),mai=mai,mgp=c(2,0.5,0),tck=-0.01)
  par(font.lab=2,font.axis=2,cex.lab=1.5,cex.axis=1.2)
  image(topo,col=topo.colors(50),xlab='X (m)',ylab='Y (m)', useRaster = TRUE)
  contour(topo,nlevels=10,lwd=1,add=TRUE)
  dev.off()


  ## read the snapshots and make pngs of them
  for(j in nums){
    ndigits_fn = 1 + floor(log(N, 10))
    png_num = formatC(j, flag = 0, width = ndigits_fn)
    ## formatC is a pain. use custom function prettytitle instead.
    ##    nd = (dt < 1) + ceiling(log(N*dt, 10)) - floor(log(dt, 10)) 
    ##    main = paste(formatC(j*dt, width = nd), 's')
    main = prettytitle(j*dt, dt, N*dt)
    print(paste(j, 'of', N, ': t =', main))
    
    fn_sec1 = info$fn_sec1list[j]
    fn_sec2 = info$fn_sec2list[j]
    fn_sur = info$fn_surlist[j]
    
    if(makesec1){
      ## Read vertical section 1
      con=file(fn_sec1,"rb")
      PP0=readBin(con,what=numeric(),n=dim(coords_sec1)[1],size=4)
      close(con)
      PP=matrix(PP0,ncol=length(zz_sec1))/info$MASKsec1

      ## write to a png file
      png(filename=paste(outputdir, '/', prefix, '_sec1_', png_num, '.png',sep=''),width=width, height=height_sec1, res=res, pointsize = pointsize)
      par(omi=c(0,0,0,0),mai=mai,mgp=c(2,0.5,0),tck=-0.01)
      par(font.lab=2,font.axis=2,cex.lab=1.5,cex.axis=1.2)
      image(surfline_sec1$h,sort(zz_sec1),PP[,order(zz_sec1)],xlab='Distance (m)',ylab='Elevation (m)',col=mycols(100), breaks = info$breaks_sec1, main = main, useRaster = TRUE, asp = asp)
      lines(surfline_sec1$h,surfline_sec1$z,col='black',lwd=2) # draw surface line
      dev.off()
    }#if makesec1

    if(makesec2){
      ##Read vertical section 2
      con=file(fn_sec2,"rb")
      PP0=readBin(con,what=numeric(),n=dim(coords_sec2)[1],size=4)
      close(con)
      PP=matrix(PP0,ncol=length(zz_sec2))/info$MASKsec2
      mai=c(0.8,0.8,0.2,0.2)

      ## write to a png file
      png(filename=paste(outputdir, '/', prefix, '_sec2_', png_num, '.png',sep=''),width=width, height=height_sec2, res=res, pointsize = pointsize)
      par(omi=c(0,0,0,0),mai=mai,mgp=c(2,0.5,0),tck=-0.01)
      par(font.lab=2,font.axis=2,cex.lab=1.5,cex.axis=1.2)
      image(surfline_sec2$h, sort(zz_sec2),PP[,order(zz_sec2)],xlab='Distance (m)',ylab='Elevation (m)',col=mycols(100), breaks = info$breaks_sec2, main = main, useRaster = TRUE, asp = asp)
      lines(surfline_sec2$h, surfline_sec2$z,col='black',lwd=2) # draw surface line
      dev.off()
    }#if makesec2

    if(makesurf){
      ##Read surface pressure
      con=file(fn_sur,"rb")
      PP0=readBin(con,what=numeric(),n=nrow(coords_surf),size=4)
      close(con)
      PP=matrix(PP0,ncol=ncol(info$MASKsurf))/info$MASKsurf
      mai=c(0.8,0.8,0.2,0.2)

      ## write to png file
      png(filename=paste(outputdir, '/', prefix, '_surf_', png_num, '.png',sep=''),width=width, height=height_surf, res=res, pointsize = pointsize)
      par(omi=c(0,0,0,0),mai=mai,mgp=c(2,0.5,0),tck=-0.01)
      par(font.lab=2,font.axis=2,cex.lab=1.5,cex.axis=1.2)
      image(topo$x,topo$y,PP,xlab='X (m)',ylab='Y (m)',col=mycols(100), breaks = info$breaks_surf, main = main, useRaster = TRUE, asp = asp)
      contour(topo,nlevels=10,lwd=1,add=TRUE) ## overlay topo map
      dev.off()
    }# if makesurf
  } #for k
}


