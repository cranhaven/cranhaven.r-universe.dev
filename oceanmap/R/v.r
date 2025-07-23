
# last update: 24.sept.2013

setClass('bathy')
setClass('gz')
setClass('nc')
setClass('ncdf4')

# if ( !isGeneric("v") ) {
#   setGeneric("v", function(x, ...)
#     standardGeneric("v"))
# }

v <- function(obj, ...) UseMethod("v")

setMethod('v', signature(obj='character'), 
          function(obj,folder,...){
            if(length(obj) == 0) stop(paste('No file corresponds to search string entered! Please revise or .check.folder!'))
            obj.opt <- 1
            for(s in obj){
              if(s == 'bathy'){
                class(obj.opt) <- 'bathy'
                v(obj=obj.opt,...)
              }else{
                ss <- s
                if(!missing(folder)){
                  folder <- paste0(folder, "/"); folder <- gsub('//','/',folder)
                  ss <- paste0(folder,"*",s)
                }
                files <- Sys.glob(ss)
                if(length(files) == 0) stop(paste('No file corresponds to search string entered! Please revise or .check.folder!'))
                for(files.sub in files){
                  if(grepl('.gz', files.sub, fixed=T)){ # fixed necessary since otherwise "point" is not recognized.
                    class(files.sub) <- 'gz'
                    v(obj=files.sub,...)
                  }
                  if(grepl('.nc', files.sub, fixed=T)){ # fixed necessary since otherwise "point" is not recognized.
                    class(obj) <- 'nc'
                    v(obj,...)
                    
                  }
                }
              }
            }
          }
)

# setMethod('v', signature(obj='.bathy'), function(obj, ...) v.bathy(visualize=T, ...))
setMethod('v', signature(obj='bathy'), 
          function(obj, v_area, lon, lat, resolution=4, keep=F, 
                   savename.bathy, folder.bathy=".", adaptive.vals=T, cb.title, show.colorbar=T,...) v.bathy(v_area=v_area,lon=lon,lat=lat,resolution=resolution, keep=keep,
                                                                                                             savename.bathy=savename.bathy, folder.bathy=folder.bathy,visualize=T,cb.title=cb.title,show.colorbar=show.colorbar,...))

setMethod('v', signature(obj='gz'), function(obj, v_area, adaptive.vals=F,show.colorbar=T, ...) v.gz(obj=unique(obj), v_area=v_area, adaptive.vals=adaptive.vals, show.colorbar=show.colorbar,...))


setMethod('v', signature(obj='nc'), 
          function(obj, varname, t=1, layer=t, adaptive.vals=T, dates, 
                   cb.xlab=varname,show.colorbar=T,...){
            obj2 <- nc2raster(obj,varname,layer=layer)
            if(missing(t)){
              if(!missing(layer)){
                t <- layer
              }else{
                t <- 1
                n <- raster::nlayers(obj2)
                warning("no (time) layer defined from a netcdf-file, selecting first of ", n, " layer(s)")
              }
            }
            v.raster(obj=obj2, layer=1:length(t),adaptive.vals=adaptive.vals,show.colorbar=show.colorbar,cb.xlab=cb.xlab,...)
          }
)

setMethod('v', signature(obj='ncdf4'), 
          function(obj, varname, t=1, layer=t, adaptive.vals=T, dates, 
                   cb.xlab=varname, show.colorbar=T,...){
            obj2 <- nc2raster(obj,varname)
            if(missing(t)){
              if(!missing(layer)){
                t <- layer
              }else{
                t <- 1
                n <- raster::nlayers(obj2)
                warning("no (time) layer defined from ncdf4-object, selecting first of ", n, " layer(s)")
              }
            }
            v.raster(obj=obj2, layer=t,adaptive.vals=adaptive.vals, show.colorbar=T, cb.xlab=cb.xlab, ...)
          }
)

setMethod('v', signature(obj='RasterLayer'), 
          function(obj, varname, t=1, layer=t, ...){
            obj2 <- obj
            if(missing(t)){
              if(!missing(layer)){
                t <- layer
              }else{
                t <- 1
                n <- raster::nlayers(obj2)
                warning("no (time) layer defined from RasterLayer-object, selecting first of ", n, " layer(s)")
              }
            }
            v.raster(obj=obj2, layer=t,...)
          }
)

setMethod('v', signature(obj='RasterStack'), 
          function(obj, varname, t=1, layer=t, ...){
            obj2 <- obj
            if(missing(t)){
              if(!missing(layer)){
                t <- layer
              }else{
                t <- 1
                n <- raster::nlayers(obj2)
                warning("no (time) layer defined from RasterStack-object, selecting first of ", n, " layer(s)")
              }
            }
            v.raster(obj=obj2, layer=t,...)
          }
)

setMethod('v', signature(obj='RasterBrick'), 
          function(obj, varname, t=1, layer=t, ...){
            obj2 <- obj
            if(missing(t)){
              if(!missing(layer)){
                t <- layer
              }else{
                t <- 1
                n <- raster::nlayers(obj2)
                warning("no (time) layer defined from RasterBrick-object, selecting first of ", n, " layer(s)")
              }
            }
            v.raster(obj=obj2, layer=t,...)
          }
)









