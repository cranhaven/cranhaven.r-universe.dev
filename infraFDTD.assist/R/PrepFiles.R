PrepFiles <-
function(X, sta, dh, fn_topo, fn_c, fn_rho, fn_sta, c = 335, rho = 1.2, xlim = range(X$x), ylim = range(X$y), c_function = NA, rho_function = NA, z_extent = NaN){
    # X: DEM. must include elements x (vector), y (vector), z (matrix: nrow = length(x), ncol = length(y))
    # sta: station info. must include elements x, y (vectors in meters, in same coordinate system as X). station names are optionally stored in element sta. z is optional; if missing, all stations are located on the surface. Otherwise, NaN values are located on the surface, and non-NaN values are preserved.
    # dh: grid spacing (m)
    # fn_topo: output file name for DEM
    # fn_c: output file name for sound speed
    # fn_rho: output file name for air density
    # fn_sta: output file name for stations
    # c: homogeneous sound speed (m/s)
    # rho: homogeneous air density (kg/m^3)
    # xlim, ylim: limits of output DEM--must be contained within limits of input DEM
  # c_function: function giving sound speed as a function of height
  # rho_function: function giving air density as a function of height
  # z_extent: vertical thickness of model (m); must be > total relief in model
  cf = function(x)c(ceiling(x[1]), floor(x[2]))
  ## define output grid
  xlim = cf(xlim/dh)*dh
  ylim = cf(ylim/dh)*dh
  if(xlim[1] < min(X$x) | xlim[2] > max(X$x) | xlim[1] >= xlim[2]){
    stop('Invalid xlim')
  }
  if(ylim[1] < min(X$y) | ylim[2] > max(X$y) | ylim[1] >= ylim[2]){
    stop('Invalid ylim')
  }
  x = seq(xlim[1], xlim[2], dh)
  y = seq(ylim[1], ylim[2], dh)
  
  nx = length(x)
  ny = length(y)
  
  ## interpolate grid for even spacing at dh
  z = fields::interp.surface.grid(X, list(x=x, y=y))$z
    
  ## round it off to the nearest node
  z = dh * round(z/dh)

  ## write to file
  write.table(as.vector(z), file = fn_topo, quote=FALSE, row.names=FALSE, col.names=FALSE)

  ## calculate vertical thickness of model
  if(is.na(z_extent)){
    z_extent = dh * round((max(z) - min(z))/dh)
  }else if(z_extent < dh * round(2 + (max(z) - min(z))/dh)){
    warning('z_extent insufficient for DEM; using default')
    z_extent = dh * round(1.5 * (max(z) - min(z))/dh)
  }    
          
  ## write station file
  sta$x = dh * round(sta$x/dh)
  sta$y = dh * round(sta$y/dh)
  nsta = length(sta$x)
  w = (sta$x < min(x) | sta$x > max(x) | sta$y < min(y) | sta$y > max(y))
  if(any(w)){
    stop('Station(s) ', paste(which(w), collapse = ','), ' are outside xlim or ylim')
  }
  if(length(sta$z) == 0){
    w = 1:nsta
  }else{
    w = which(!(1:nsta %in% which(!is.na(sta$z))))
  }
  ## set stations without given elevation on ground
  for(i in w){
    sta$z[i] = z[x == sta$x[i], y == sta$y[i]] + dh
  }
  if(length(sta$sta) == length(sta$x) && length(unique(sta$sta)) == length(sta$x)){
    sta$sta = substr(sta$sta, 1, 4)
  }else{
    sta$sta = formatC(1:nsta, width=3, flag=0)
    warning('Using default station names')
  }
  
  cat('', file=fn_sta, append=FALSE)
  for(i in 1:nsta){
    cat(sta$sta[i], sta$x[i] - min(x), sta$y[i] - min(y), sta$z[i], '\n', file = fn_sta, append = TRUE)
  }
     
     
  ## calculate vertical extent for sound speed, density files
##  nz = round((max(z) - min(z))/dh * 1.5)
  nz = round(z_extent/dh)
  zmin = min(z) - dh
  zmax = zmin + (nz - 1) * dh
  height = zmin + (0:(nz-1)) * dh

  if(is.na(c_function)){
    if(length(c) == 1){
      cvec = rep(c, nz)
    }else if(length(c) == nz){
      cvec = c
    }else{
      stop('invalid length of sound speed vector: must be either 1, nz, or provide c_function')
    }
  }else{
    cvec = c_function(height)
  }
  if(is.na(rho_function)){
    if(length(rho) == 1){
      rhovec = rep(rho, nz)
    }else if(length(rho) == nz){
      rhovec = rho
    }else{
       stop('invalid length of density vector: must be either 1, nz, or provide rho_function')
    }
  }else{
    rhovec = rho_function(height)
  }
     # write sound speed file
    cat('', file=fn_c, append=FALSE)
    for(i in 1:nz){
        cat(cvec[i], file = fn_c, sep = '\n', append = TRUE)
    }
    
     # write density file
    cat('', file=fn_rho, append=FALSE)
    for(i in 1:nz){
        cat(rhovec[i], file = fn_rho, sep = '\n', append = TRUE)
    }
    
     # print useful info for param.h
    print(paste('X extent (m):', max(x) - min(x)))
    print(paste('Y extent (m):', max(y) - min(y)))
    print(paste('Z extent (m):', zmax - zmin))
    print(paste('Minimum elevation (m):', zmin))
    print(paste('Grid interval:', dh))
    print(paste('(max recommended freq: ', signif(c/(15*dh),3), ' Hz)', sep = ''))
    print(paste('(max recommended dt: ', signif(dh/c/sqrt(3), 3), ' s)', sep = ''))
    print(paste('Origin in new coordinates:', -min(x), -min(y)))
    print(paste('Source elevation:', z[x==0, y==0]))
    print(paste('Elevation File:', fn_topo))
    print(paste('Sound Speed File:', fn_c))
    print(paste('Density File:', fn_rho))
    print(paste('Station File:', fn_sta))
  print(paste('Number of Stations:', nsta))
  print(paste('Total number of nodes:', nx*ny*nz))
#  print(paste('Estimated memory required (6004 MB available):', nx*ny*nz*4/2^20, 'MB'))
  print(paste('Estimated memory required:', nx*ny*nz*4/2^20, 'MB'))
}
