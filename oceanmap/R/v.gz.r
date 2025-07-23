v.gz <- function(obj, v_area=region, adaptive.vals=F, param, zlim, minv, maxv, replace.na=F, Log, main, cbpos,cbx, cby, cb.title, cb.xlab, cb.xlab.line=0, pal, nticks=5,
                 sidelabels=F, Ylab=F, axeslabels=T, ticklabels=T, show.colorbar, cex.lab=0.8, cex.ticks=0.8, cex.cb.title=0.9,cex.cb.xlab=0.8,cex.cb.ticks=0.7,
                 subplot=T, width, height, figdim, xpos=-1, Save=F, plotfolder, plotname, fileformat="png", suffix='', 
                 region, v_image=T, v_contour=F, levels, contour.labels=NULL, v_arrows=F, scale_arrow=1, 
                 fill.land=T, col.land="grey", col.bg=NA,border='black', grid=T, grid.res, cb.ticks.srt=90,las=0, bwd=2, verbose=T){
  if(missing(show.colorbar)) show.colorbar <- T    
  
  #   cat('\nrunning .v.gz')
  infile <- obj
  get.back <- F
  if(grepl('/',obj)){
    get.back <- T
    ii <- tail(which(strsplit(obj, "")[[1]]=="/"),1)
    folder <- substr(obj,1,ii)
    folder <- paste0(folder, "/"); folder <- gsub('//','/',folder)
    obj <- substr(obj,ii+1,nchar(obj))
  }
  
  if(!missing(width) & !missing(height)) figdim <- c(width, height)
  
  #     obj <- subset(obj, substr(obj, nchar(obj)-2, nchar(obj)) == ".gz") # select binary files of which area is known
  file_def <- name_split(obj)
  if(missing(param)) param <- file_def$parameter
  
  parameter_definitions <- NULL
  rm(parameter_definitions)
  data('parameter_definitions',envir=environment())
  
  if(!any(parameter_definitions$param %in% param)) stop('error in .v.gz.r: Parameter definition is missing or not matching parameter list. Please select valid parameter label\n',
                                                        paste(paste(parameter_definitions$param,"\t",parameter_definitions$name1),collapse='\n'))
  param_def <- parameter_definitions[as.character(parameter_definitions$param) == param,]
  if(!missing(Log)) param_def$log <- Log
  if(nrow(param_def) == 0) stop('parameter not found, please check')
  
  if(missing(v_area)){v_area <- file_def$area}else{v_area <- v_area}
  b <- readbin(infile,area=v_area,Raster=T) # read binary data file
  
  outfile.name <- obj
  file_def2 <- file_def; file_def2$area <- v_area; outfile <- name_join(file_def2)
  outfile.name <- paste0(substr(outfile,1,nchar(outfile)-3),suffix)
  
  if(get.back & missing(plotfolder)) plotfolder <- folder
  r <- regions(v_area)
  if(!missing(cbx)) r$cbx <- cbx
  if(!missing(cby)) r$cbx <- cby
  if(!missing(cbpos)){
    cb <- cust.colorbar(v_area,cbpos=cbpos,figdim=figdim,force.figdim.widget=F)
    r$cbx <- cb$cbx
    r$cby <- cb$cby
    r$align <- cb$align
  }
  if(missing(grid.res)) grid.res <- .get.grid.res(r)
  if(!missing(figdim)) r$figdim <- figdim
  
  .v.plot(b=b, minv=minv,maxv=maxv, adaptive.vals=adaptive.vals, zlim=zlim, replace.na=replace.na, main=main, cb.title=cb.title, cb.xlab=cb.xlab, pal=pal, cb.ticks.srt=cb.ticks.srt, nticks=nticks,
          sidelabels=sidelabels, Ylab=Ylab, axeslabels=axeslabels, ticklabels=ticklabels, show.colorbar=show.colorbar, cex.lab=cex.lab, cex.ticks=cex.ticks,
          cex.cb.title=cex.cb.title,cex.cb.xlab=cex.cb.xlab,cex.cb.ticks=cex.cb.ticks,cb.xlab.line=cb.xlab.line,
          subplot=subplot, xpos=xpos, Save=Save, plotfolder=plotfolder, plotname=plotname, fileformat=fileformat, # suffix no longer required
          param=param, param_def=param_def, file_def=file_def, r=r, outfile.name=outfile.name, # set within this procedure
          v_area=v_area, v_image=v_image, v_contour=v_contour, contour.labels=contour.labels, levels=levels, v_arrows=v_arrows, scale_arrow=scale_arrow,
          fill.land=fill.land, col.land=col.land, col.bg=col.bg, border=border, grid=grid, grid.res=grid.res, bwd=bwd,las=las, verbose=verbose) # further arguments passed to plotmap
}