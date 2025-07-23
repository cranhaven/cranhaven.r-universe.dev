### function used to check types of available filesuffix in a directory

# folder <- '~/med4_avhrr_sst2_4km'
check_gzfiles <- function(sstring="*",folder,filetype=".gz")
{
  
  if(!missing(folder)) {
    if(folder[1] !=F){
      folder <- paste0(folder, "/"); folder <- gsub('//','/',folder)
      sstring <- paste0(folder,"*",sstring)
    }
  }
  files <- Sys.glob(sstring)
  ii <- tail(which(strsplit(files, "")[[1]]=="/"),1)
  folder <- substr(files,1,ii)
  files <- substr(files,ii+1,nchar(files))
  files <- files[files != "dummy.gz"]
  
  if(length(files) == 0)stop(paste0('ERROR: there is no file of the type ',sstring))
  
  files <- subset(files, substr(files, nchar(files)+1-nchar(filetype), nchar(files)) == filetype)
  objs <- matrix(as.character(unlist(strsplit(files,"\\_"))),ncol=7,byrow=T)
  for (i in 1:length(objs[,7]))
  {
    objs[i,7] <- substr(objs[i,7], 9,nchar(objs[i,7]))
  }
  objs <- data.frame(objs)
  objs <- data.frame(objs[,1:5],objs[,7])
  colnames(objs) <- c("region", "sat", "param", "res", "ts", "option")
  head(objs)
  
  objs <- cbind(objs,files=1)  
  objs.agg <- aggregate(objs$files, by=as.list(objs[,1:6]), FUN=sum)
  colnames(objs.agg)[7] <- 'files'
  objs.agg$option <- gsub(filetype,"",objs.agg$option)
  objs.agg$filetype <- filetype
  return(objs.agg)
}