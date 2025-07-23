
clim_plot <- function(obj, folder, plotfolder=".", plotname, question=T, sst.frontcolor='red', 
                      chla.frontcolor='blue',sidelabels=F, Ylab=F, axeslabels=T, v_area,...
                      #                       zlim, minv, maxv, adaptive.vals, replace.na=F, main,
                      #                       cb.title, cb.xlab,
                      #                       ticklabels=T, cex.lab=0.8, cex.ticks=0.8, 
                      #                       subplot=T, suffix='',
                      #                       v_area, v_image=T, v_contour=F, v_arrows=F, scale_arrow,
                      #                       fill=T, col="grey", border='black', grid=T, grid.res, bwd=1
){
  
  #   obj <- '*ncro*chla*'
  
  get.back <- F
  if(grepl('/',obj)[1]){
    get.back <- T
    ii <- tail(which(strsplit(obj, "")[[1]]=="/"),1)
    if(missing(folder)) {
      folder <- substr(obj,1,ii)
    }
    obj <- substr(obj,ii+1,nchar(obj))
  }
  if(missing(folder)) folder <- getwd()
  folder <- paste0(folder, "/"); folder <- gsub('//','/',folder)
  
  
  file_summary <- check_gzfiles(obj,folder = folder)
  if(nrow(file_summary) > 1){
    b <- capture.output(file_summary)
    c <- paste(b, "\n", sep="")
    stop("error in clim.plot: multiple variables, please type explicit search string:\n\n", c, "\n")
  }
  files <- Sys.glob(paste0(folder,obj))
  files.split <- name_split(gsub(folder[1],"",files))
  dates1 <- as.Date(files.split$date1,"%Y%m%d")
  years <- as.numeric(unique(format(dates1,"%Y")))
  years.levels <- years[is.finite(years)]
  months <- as.numeric(unique(format(dates1,"%m")))
  months.levels <- months[is.finite(months)]
  param <- unique(files.split$parameter)
  if(question) {
    enter <- readline(paste0("\nGoing to plot ",nrow(files.split)," figures.\n\nPress <Enter> to continue"))
    if(enter != "") stop("Operation stopped by user")
  }
  if(missing(v_area)){
    v_area <- unique(files.split$area)
  }else{
    files.split$area <- v_area
  }
  
  #   print(param)
  if(param == "p100"){
    if(any(grepl("sst",files.split$option)) & missing(pal)) pal <- sst.frontcolor
    if(any(grepl("chla",files.split$option)) & missing(pal)) pal <- chla.frontcolor
    #     if(any(grepl("chla.sst",files.split$option))) Colors <- chla.frontcolor
  }
  
  v(obj=obj, folder=folder,sidelabels=sidelabels, Ylab=Ylab, axeslabels=axeslabels, v_area=v_area, Save=T, ...)
  #     zlim=zlim, minv=minv, maxv=maxv, adaptive.vals=adaptive.vals, replace.na=replace.na, main=main,
  #     cb.title=cb.title, cb.xlab=cb.xlab, pal=pal, 
  #     ticklabels=ticklabels, cex.lab=cex.lab, cex.ticks=cex.ticks, 
  #     subplot=subplot, Save=T, plotfolder=".",suffix=suffix,
  #     v_area=v_area, v_image=v_image, v_contour=v_contour, v_arrows=v_arrows, scale_arrow=scale_arrow,
  #     fill=fill, col=col, border=border, grid=grid, grid.res=grid.res, bwd=bwd)
  
  #     v(obj,Save=T,plotfolder=".",sidelabels=sidelabels,v_area=v_area,bwd=bwd)
  pngfiles <- paste0(folder,name_join(files.split,"png"))
  
  for(i in 1:length(pngfiles)){
    cmd <- paste("convert -resize 25% ", pngfiles[i], " ", pngfiles[i], sep="") # reduce image size
    switch(grepl('windows',.Platform$OS.type)+1,system(cmd),shell(cmd)) 
  }
  
  if(file_summary$ts == "1m"){
    spngfiles<-c()
    for(y in years.levels){
      ids <- grep(paste0("_",y),pngfiles) # find files
      spngfile <- name_join(cbind(files.split[ids[1],1:5], # region label was reset above
                                  date1=paste(min(files.split$date1[ids])),
                                  date2=paste(max(files.split$date2[ids])),
                                  option=files.split$option[ids[1]]),"png")
      cmd <- paste0("convert ",paste(pngfiles[ids],collapse=" ")," +append ",spngfile)
      switch(grepl('windows',.Platform$OS.type)+1,system(cmd),shell(cmd)) 
      spngfiles <- c(spngfiles, spngfile)
    }
    
    if(missing(plotname)) plotname <- name_join(cbind(files.split[ids[1],1:5], # region label was reset above
                                                      date1=paste(min(files.split$date1)),
                                                      date2=paste(max(files.split$date2)),
                                                      option=files.split$option[ids[1]]),"png")
    cmd <- paste0("convert ",paste(spngfiles,collapse=" ")," -append ",plotname)
    switch(grepl('windows',.Platform$OS.type)+1,system(cmd),shell(cmd)) 
  }
  
  if(file_summary$ts == "1s"){
    spngfiles <- pngfiles
    if(missing(plotname)) plotname <- name_join(cbind(files.split[1,1:5], # region label was reset above
                                                      date1=paste(min(files.split$date1)),
                                                      date2=paste(max(files.split$date2)),
                                                      option=paste0(files.split$option[1],".summary")),"png")
    cmd <- paste0("convert ",paste(spngfiles,collapse=" ")," +append ",plotname)
    switch(grepl('windows',.Platform$OS.type)+1,system(cmd),shell(cmd)) 
  }
  system(paste0("mv ",plotname," ",plotfolder))
  system(paste("rm",paste(pngfiles,collapse=" ")))
  cat(paste0("\ncreated: ",plotname," ",plotfolder))
}



