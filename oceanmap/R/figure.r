figure <- function(filename,folder,type,save=F,do.save=save,
                   width=10,height=10,xpos=-1,do.overwrite=T,delete.old=do.overwrite,...){
  ## requires 
  # library(remotes)
  # remotes::install_version("Rttf2pt1", version = "1.3.8")
  
  
  if(do.save){
    if(missing(folder) & grepl('/',filename)) {
      ###' re-split filename
      folder <- substr(filename, 1,tail(gregexpr(pattern ='/',filename)[[1]],1))
      filename <- substr(filename, tail(gregexpr(pattern ='/',filename)[[1]],1)+1,nchar(filename))
    }
    
    if(missing(folder)) {
      warning(paste("plot folder is missing, selecting current working directory:",getwd()))
      folder <- getwd()
    }
    folder <- gsub('//','/',folder)
    if(missing(type)) type <- 'png'
    for(ftype in c('png','jpg','pdf','eps','jpg','jpeg','tiff')) {
      if(grepl(paste0('.',ftype),filename)) {
        type <- ftype
        filename <- gsub(paste0('.',ftype),'',filename)
      }
    }
    # if(ftype != type) stop("figure type specifications are not coherent. Please revise! (see filename and type)")

    if(type == 'png') .save.png(filename=filename,folder=folder,width=width,height=height,delete.old=delete.old,...)
    if(type %in% c('jpg','jpeg')) .save.jpg(filename=filename,folder=folder,width=width,height=height,delete.old=delete.old,...)  
    if(type == 'pdf') .save.pdf(filename=filename,folder=folder,width=width,height=height,delete.old=delete.old,...)  
    if(type == 'eps') .save.eps(filename=filename,folder=folder,width=width,height=height,delete.old=delete.old,...)  
    if(type == 'tiff') .save.tiff(filename=filename,folder=folder,width=width,height=height,delete.old=delete.old,...)  
    
  }else{
    dev.new(width=width, height=height,xpos=xpos)
  }
  if(!missing(filename)) cat(paste("showing:",filename,'\n'))
}



.save.pdf <- function(filename,folder=".",width=7,height=7,delete.old=T,units="in",finaly=F,family="Arial"){
  if(folder != '.') system(paste("mkdir -p",folder)) # create folder if needed
  
  if(!is.null(family)){
    f <- fonts()
    if(length(f) == 0) font_import()
    # loadfonts() ## for pdf()
    loadfonts(device = "postscript",quiet=TRUE) ## for postscript
  }
  if(units == "cm"){
    width <- 0.393701*width
    height <- 0.393701*height
  }
  d <- format(Sys.Date(),format='%Y%m%d')
  #     f <- paste0(.check.folder(folder),.check.point(filename),"_",d,".eps")
  f <- paste0(.check.folder(folder),.check.point(filename),".pdf")
  if(file.exists(f)) {
    if(!delete.old) stop("file with selected filename already exists!") 
    warning("file with selected filename already exists and will be overwritten")
  }
  setEPS();  
  cairo_pdf(f,width=width, height=height,family=family)
  cat(paste("generating",f,"\n"))
}

.save.eps <- function(filename,folder=".",width=7,height=7,delete.old=T,units="in",finaly=F,family="Arial"){
  if(folder != '.') system(paste("mkdir -p",folder)) # create folder if needed
  
  if(!is.null(family)){
    f <- fonts()
    if(length(f) == 0) font_import()
    # loadfonts() ## for pdf()
    loadfonts(device = "postscript",quiet = TRUE) ## for postscript
  }
  if(units == "cm"){
    width <- 0.393701*width
    height <- 0.393701*height
  }
  d <- format(Sys.Date(),format='%Y%m%d')
  #     f <- paste0(.check.folder(folder),.check.point(filename),"_",d,".eps")
  f <- paste0(.check.folder(folder),.check.point(filename),".eps")
  if(file.exists(f)) {
    if(!delete.old) stop("file with selected filename already exists!") 
    warning("file with selected filename already exists and will be overwritten")
  }
  setEPS();  
  cairo_ps(f,width=width, height=height,family=family)
  cat(paste("generating",f,"\n"))
}


.save.png <- function(filename,folder=".",width=7,height=7,units="in",res=300,delete.old=T,family='Arial'){
  if(folder != '.') system(paste("mkdir -p",folder)) # create folder if needed
  if(!is.null(family)){
    f <- fonts()
    if(length(f) == 0) font_import()
    # loadfonts() ## for cairo_pdf()
    loadfonts(quiet = T) ## for postscript
  }
  d <- format(Sys.Date(),format='%Y%m%d')
  #   f <- paste0(.check.folder(folder),.check.point(filename),"_",d,".png")
  f <- paste0(.check.folder(folder),.check.point(filename),".png")
  if(file.exists(f)) {
    if(!delete.old) stop("file with selected filename already exists!") 
    warning("file with selected filename already exists and will be overwritten")
  }
  png(f,width=width, height=height,res=res,units=units,family=family)
  cat(paste("generating",f,"\n"))
}


.save.tiff <- function(filename,folder=".",width=7,height=7,units="in",res=300,delete.old=T,family='Arial'){
  if(folder != '.') system(paste("mkdir -p",folder)) # create folder if needed
  if(!is.null(family)){
    f <- fonts()
    if(length(f) == 0) font_import()
    # loadfonts() ## for cairo_pdf()
    loadfonts(quiet = TRUE) ## for postscript
  }
  d <- format(Sys.Date(),format='%Y%m%d')
  #   f <- paste0(.check.folder(folder),.check.point(filename),"_",d,".tiff")
  f <- paste0(.check.folder(folder),.check.point(filename),".tiff")
  if(file.exists(f)) {
    if(!delete.old) stop("file with selected filename already exists!") 
    warning("file with selected filename already exists and will be overwritten")
  }
  tiff(f,width=width, height=height,res=res,units=units,family=family)
  cat(paste("generating",f,"\n"))
}


.save.jpg <- .save.jpeg <- function(filename,folder=".",width=7,height=7,units="in",res=300,delete.old=T, quality=100,family='Arial'){
  d <- format(Sys.Date(),format='%Y%m%d')
  #   f <- paste0(.check.folder(folder),.check.point(filename),"_",d,".jpeg")
  f <- paste0(.check.folder(folder),.check.point(filename),".jpeg")
  if(file.exists(f)) {
    if(!delete.old) stop("file with selected filename already exists!") 
    warning("file with selected filename already exists and will be overwritten")
  }
  jpeg(f,width=width, height=height,res=res,units=units,quality=quality)
  cat(paste("generating",f,"\n"))
}

.check.folder <- function(folder){
  if(nchar(folder > 1) & substr(folder,nchar(folder),nchar(folder)) != '/') folder <- paste0(folder,'/')
  return(folder)
}

.check.point <- function(filename){
  if(substr(filename,nchar(filename),nchar(filename)) == "."){
    filename <- substr(filename,1,nchar(filename)-1)
  }
  return(filename)
}

close_fig <- function(do.close=F,do.save=do.close) {
  if(do.close | do.save) dev.off()
}