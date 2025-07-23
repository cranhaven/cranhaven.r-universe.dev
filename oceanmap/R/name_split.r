.name_split0 <- function(gz.files)
{
  if(grepl('/',gz.files[1])){
    ii <- tail(which(strsplit(gz.files, "")[[1]]=="/"),1)
    # folder <- substr(gz.files,1,ii)
    # folder <- paste0(folder, "/"); folder <- gsub('//','/',folder)
    gz.files <- substr(gz.files,ii+1,nchar(gz.files))
  }
  #+
  # Function that returns a structure of strings that composed an image name :
  #          of the form:
  #
  #   -   "area_source_parameter_resolution_time-step_date1_date2[.optional-part]"
  #       example: 'b3_modis_sst2_1km_1d_20090910_20090910.gz'
  #
  # Note: the end of the compulsory part is either the end of the date
  #         or the character before the first "." that begins the optional part
  #
  # Example : parts = name_split('w05d_seawifs_pp_50km_1m_19970901_19970930.gz')
  #
  # See also: name_join.pro (join into a name parts extracted by name_parts.pro)
  #
  # Author: H. Demarcq
  #
  # Date:   February 2010
  #
  # -----
  #  
  # translated and extended  R version by R. Bauer
  #
  # Date:   April 2013
  #-
  
  # Checking file_name validity
  # Determination of the type of list (text file OR array of names)
  
  dim <- length(gz.files)   #size() contains : number_of_dim, dimensions, type_code, nb_of_elements
  if(dim != 1){   # First argument is a string array of the file names
    aparts <- list()
    for (i in 1:dim)
    {
      aparts[[i]] <- .name_split_org(gz.files[i])    
    }
    parts <- matrix(as.character(unlist(aparts)),ncol=length(aparts[[i]]),byrow=T)
    colnames(parts) <- names(data.frame((aparts[1])))
  }else{
    parts <- .name_split_org(gz.files)
  }
  return(data.frame(lapply(as.data.frame(parts), as.character), stringsAsFactors=F))#as.data.frame(parts))
}

.name_split_org <- function(name)
{
  str <- unlist(strsplit(name,"\\_"))
  count <- n_parts <- length(str)
  if(n_parts != 7) stop("name_parts: the number of ''_'' separator in name must be only 6, please check)")
  
  if(n_parts == 3){  # old format (Usefull for AOOS_rename.pro !)
    parts <- list(area="", source="", parameter="", resolution="", timestep="", date1="", date2="", option="")#short=1)
    parts$area       = str[1]
    parts$parameter  = str[2]
    parts$date1      = str[3]
    
    # Extract the optional part of the name (after the date and after the first '.')
    pos <- which(strsplit(str[n_parts], '')[[1]]=='.')[1]
    if(is.finite(pos)){
      parts$option <-  substr(str[n_parts], pos, nchar(str[n_parts]))
      before <- substr(str[n_parts], 1, pos-1)
      parts$date1 <- substr(str[n_parts], 1, pos-1)
      parts$date2 <- parts$date1
    }
  }else{
    parts <- list(area="", source="", parameter="", resolution="", timestep="", date1="", date2="", option="")#, short=0)
    parts$area       <- str[1]
    parts$source     <- str[2]
    parts$parameter  <- str[3]
    parts$resolution <- str[4]
    parts$timestep   <- str[5]
    parts$date1      <- str[6]
    parts$date2      <- str[7]
    # Extract the optional part of the name (after the date and after the first '.')
    pos <- which(strsplit(str[n_parts], '')[[1]]=='.')[1]
    if(is.finite(pos)){
      parts$option <-  substr(str[n_parts], pos, nchar(str[n_parts]))
      before <- substr(str[n_parts], 1, pos-1)
      parts$date2 <- substr(str[n_parts], 1, pos-1)   
    }
    parts$filetype <- tail(unlist(strsplit(parts$option, "[.]")),1)
    parts$option <- unlist(strsplit(parts$option,paste0(".",parts$filetype)))
  }
  return(parts)
}

get.gz.info <- name_split <- function(gz.files){
  if(missing(gz.files)) gz.files <- '*.gz'
  if(length(gz.files) == 1) {
    if(grepl('\\*',gz.files[1])) gz.files <- Sys.glob(gz.files)
  }
  if(length(gz.files > 0)){
    k <- .name_split0(gz.files)
    head(k)
    k$date1 <- as.Date(k$date1,"%Y%m%d")
    k$date2 <- as.Date(k$date2,"%Y%m%d")
    
    return(k)
  }else{
    cat('no files found!\n')
  }
}
