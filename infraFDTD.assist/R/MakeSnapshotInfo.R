MakeSnapshotInfo <-
function(inputdir = './snapshots', dframe = 20, N = 400, xs, ys, zs, makesec1 = TRUE, makesec2 = TRUE, makesurf = TRUE){
  ## dframe: snapshot interval in samples
  ## dt: snapshot interval in s
  ## N: number of snapshots
  ## xs, ys, zs: source coordinates (can be vectors)

  ## following code was originally written by Keehoon Kim and adapted by Jake Anderson.
  
  ## dt = 0.1
  ## N = 400 ## change this, depending on the number of images you have
  ## dframe = 20 ## snapshot interval in samples
  ## inputdir = './snapshots'
  ## outputdir = './anim'

  n = dframe*(1:N)
  fn_sec1list = paste(inputdir, '/pfield_sec1_', n, '.dat', sep = '')
  fn_sec2list = paste(inputdir, '/pfield_sec2_', n, '.dat', sep = '')
  fn_surlist = paste(inputdir, '/pfield_sur1_', n, '.dat', sep = '')

  ## read coordinate files
  print('Reading coord_sec1.txt')
  coords_sec1=read.table(paste(inputdir, '/coord_sec1.txt', sep=''), col.names = c('x', 'y', 'z'))
  print('Reading coord_sec2.txt')
  coords_sec2=read.table(paste(inputdir, '/coord_sec2.txt', sep = ''), col.names = c('x', 'y', 'z'))
  print('Reading coord_sur.txt')
  coords_surf=read.table(paste(inputdir, '/coord_sur.txt', sep = ''), col.names = c('x', 'y', 'z'))

  ## calculate surface linesM
  if(makesec1){
    print('Calculating surface line 1')
    surfline_sec1 = unique(coords_sec1[,1:2]) ## calculates the unique x-y pairs that are repeated for a range of z
    pb = txtProgressBar(min = 0, max = length(surfline_sec1[,1]), initial = 0, width = 20)
    print('                   | 100%')
    for(i in 1:length(surfline_sec1[,1])){
      surfline_sec1[i,3] = (coords_surf[,3])[coords_surf[,1] == surfline_sec1[i,1] & coords_surf[,2] == surfline_sec1[i,2]]
      setTxtProgressBar(pb, i)
    }
    names(surfline_sec1) = c('x', 'y', 'z')
    surfline_sec1$h = sqrt((surfline_sec1$x-surfline_sec1$x[1])^2 + (surfline_sec1$y-surfline_sec1$y[1])^2)
  }else{
    surfline_sec1 = list(h = numeric(), x = numeric(), y = numeric(), z = numeric())
  }

  if(makesec2){
    print('Calculating surface line 2')
    surfline_sec2 = unique(coords_sec2[,1:2]) ## calculates the unique x-y pairs that are repeated for a range of z
    pb = txtProgressBar(min = 0, max = length(surfline_sec2[,1]), initial = 0, width = 20)
    print('                   | 100%')
    for(i in 1:length(surfline_sec2[,1])){
      surfline_sec2[i,3] = (coords_surf[,3])[coords_surf[,1] == surfline_sec2[i,1] & coords_surf[,2] == surfline_sec2[i,2]]
      setTxtProgressBar(pb, i)
    }
    names(surfline_sec2) = c('x', 'y', 'z')
    surfline_sec2$h = sqrt((surfline_sec2$x-surfline_sec2$x[1])^2 + (surfline_sec2$y-surfline_sec2$y[1])^2)
  }else{
    surfline_sec2 = list(h = numeric(), x = numeric(), y = numeric(), z = numeric())
  }

  ## Get surface elevations from coords_surf
  topo = list()
  topo$x = unique(coords_surf$x)
  topo$y = unique(coords_surf$y)
  topo$z = matrix(coords_surf$z, nrow=length(topo$x))
  topo$XX = matrix(coords_surf$x, nrow=length(topo$x))
  topo$YY = matrix(coords_surf$y, nrow=length(topo$x))

  dev.new()
  image(topo, col = topo.colors(30), asp = 1, xlab = 'Easting', ylab = 'Northing', main = 'Section Lines', useRaster = TRUE)
  contour(topo, add = TRUE)
  lines(surfline_sec1, lwd = 2)
  lines(surfline_sec2, col=2, lwd = 2)
  legend(x = 'topleft', legend = c('Sec 1', 'Sec 2'), lwd = c(2,2), col = c(1,2), bg = 'white')
  

  ## Make the "masks" for scaling pressure 
  ## sec1 mask
  if(makesec1){
    print('Making section 1 mask')
    hn = length(surfline_sec1[,1])
    vn = length(coords_sec1[,1])/hn
    XX = matrix(coords_sec1[,1], hn, vn)
    YY = matrix(coords_sec1[,2], hn, vn)
    ZZ = matrix(coords_sec1[,3], hn, vn)
    z = unique(as.vector(ZZ))
    MASKtmp = 0
    for(i in 1:length(xs)){ # sum the expected energy from all sources
      D2 = (XX - xs[i])^2 + (YY - ys[i])^2 + (ZZ - zs[i])^2 + 1e-12 # 1e-12 makes the distance at least 1 micron
      MASKtmp = MASKtmp + 1/D2
    }
    MASKsec1 = sqrt(MASKtmp) # convert energy to amplitude. need to DIVIDE by this matrix
    dev.new()
    image(surfline_sec1$h, sort(z), log(MASKsec1[,order(z)]), col = topo.colors(20), asp = 1, useRaster = TRUE)
    lines(surfline_sec1$h, surfline_sec1$z)
  }
  
  ## sec2 mask
  if(makesec2){
    print('Making section 2 mask')
    hn = length(surfline_sec2[,1])
    vn = length(coords_sec2[,1])/hn
    XX = matrix(coords_sec2[,1], hn, vn)
    YY = matrix(coords_sec2[,2], hn, vn)
    ZZ = matrix(coords_sec2[,3], hn, vn)
    z = unique(as.vector(ZZ))
    MASKtmp = 0
    for(i in 1:length(xs)){ # sum the expected energy from all sources
      D2 = (XX - xs[i])^2 + (YY - ys[i])^2 + (ZZ - zs[i])^2 + 1e-4
      MASKtmp = MASKtmp + 1/D2
    }
    MASKsec2 = sqrt(MASKtmp) # convert energy to amplitude. need to DIVIDE by this matrix
    dev.new()
    image(surfline_sec2$h, sort(z), log(MASKsec2[,order(z)]), col = topo.colors(20), asp = 1, useRaster = TRUE)
    lines(surfline_sec2$h, surfline_sec2$z)
  }
  
  ## surface mask
  if(makesurf){
    print('Making surface mask')
    xdim = length(unique(coords_surf[,1]))
    ydim = length(unique(coords_surf[,2]))
#    XX = matrix(coords_surf[,1], xdim, ydim)
#    YY = matrix(coords_surf[,2], xdim, ydim)
    MASKtmp = 0
    for(i in 1:length(xs)){ # sum the expected energy from all sources
      D2 = (topo$XX - xs[i])^2 + (topo$YY - ys[i])^2 + (topo$z - zs[i])^2 + 1e-4
      MASKtmp = MASKtmp + 1/D2
    }
    MASKsurf = sqrt(MASKtmp) # convert energy to amplitude. need to DIVIDE by this matrix
    dev.new()
    image(topo$x, topo$y, log(MASKsurf), asp = 1, col = topo.colors(20), useRaster = TRUE)
    contour(topo, add=TRUE)
  }
  
  ## calculate range for entire animation by loading each image
  print('Calculating image plot range, snapshot-by-snapshot')
  maxlist1 = maxlist2 = maxlistsur = NULL
  for(j in 1:N){
    if((j %% 10) == 0){
      print(paste(j, 'of', N))
    }
    fn_sec1=fn_sec1list[j]
    fn_sec2=fn_sec2list[j]
    fn_sur=fn_surlist[j]

    if(makesec1){
      con=file(fn_sec1,"rb")
      PP0=readBin(con,what=numeric(),n=nrow(coords_sec1),size=4)
      PP=matrix(PP0,nrow=nrow(MASKsec1))/MASKsec1
      close(con)
      maxlist1[j] = max(abs(PP))
    }
    if(makesec2){
      con=file(fn_sec2,"rb")
      PP0=readBin(con,what=numeric(),n=nrow(coords_surf),size=4)
      PP=matrix(PP0,nrow=nrow(MASKsec2))/MASKsec2
      close(con)
      maxlist2[j] = max(abs(PP))
    }
    if(makesurf){
      con=file(fn_sur,"rb")
      PP0=readBin(con,what=numeric(),n=nrow(coords_surf),size=4)
      close(con)
      PP=matrix(PP0,ncol=length(topo$y))/MASKsurf
      maxlistsur[j] = max(abs(PP))
    }
  }

  info = list(
    makesec1 = makesec1,
    makesec2 = makesec2,
    makesurf = makesurf,
    surfline_sec1 = surfline_sec1,
    surfline_sec2 = surfline_sec2,
    coords_sec1 = coords_sec1,
    coords_sec2 = coords_sec2,
    coords_surf = coords_surf,
    topo = topo,
    N = N
    
  )
  
  ## functions to define the breakpoints
  ff = function(x)sign(x)*abs(x)^0.5 # d/d(abs(x)) should be positive
  fi = function(x)sign(x)*abs(x)^2
  if(makesec1){
    info$breaks_sec1 = fi(seq(ff(-max(maxlist1)), ff(max(maxlist1)), length.out = 101))
    info$MASKsec1 = MASKsec1
    info$fn_sec1list = fn_sec1list
  }
  if(makesec2){
    info$breaks_sec2 = fi(seq(ff(-max(maxlist2)), ff(max(maxlist2)), length.out = 101))
    info$MASKsec2 = MASKsec2
    info$fn_sec2list = fn_sec2list
  }
  if(makesurf){
    info$breaks_surf = fi(seq(ff(-max(maxlistsur)), ff(max(maxlistsur)), length.out = 101))
    info$MASKsurf = MASKsurf
    info$fn_surlist = fn_surlist
  }
  return(info)
}
