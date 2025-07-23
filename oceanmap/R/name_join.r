name_join <- function(parts,filetype='gz')
{
  
  #+
  # Function that returns a valid file name from a structure of name parts
  # as extracted by name_parts.pro
  # The names must be of the form:
  #
  #   -   "area_source_parameter_resolution_time-step_date1_date2[.optional-part]"
  #       example: 'b3_modis_sst2_1km_1d_20090910_20090910.gz'
  #
  # Example : name = name_join(name_split(file_name))
  #
  # See also: name_parts.pro (split a name into its components)
  #
  # Author: H. Demarcq
  #
  # Date:   April 2010
  #
  # -----
  #  
  # translated and extended R version by R. Bauer
  #
  # Date:   April 2013
  #-
  
  # Checking structure validity
  parts$date1 <- format(parts$date1,"%Y%m%d")
  parts$date2 <- format(parts$date2,"%Y%m%d")
  if(length(parts) > 9 | length(parts) < 8) stop('name_join: invalid input structure, see name_parts.pro)')
  
  # convert data.fame columns to character values:
  parts <- data.frame(parts)
  parts[sapply(parts, is.factor)] <- lapply(parts[sapply(parts, is.factor)], as.character)

  for(i in 1:nrow(parts)){
    ending <- substr(parts$option[i],nchar(parts$option[i]),nchar(parts$option[i]))
    ending2 <- substr(filetype,1,1)
    fill <- ''
    if(ending == '.' & ending2 == '.') filetype <- substr(filetype,2,nchar(filetype))
    if(ending != '.' & ending2 != '.') fill <- '.'
    parts$date2[i] <- paste0(parts$date2[i],parts$option[i],fill,filetype)
  }
  
  parts$option <- c()
  parts$filetype <- c()
  
  name <- apply(parts,1,function(x)paste0(paste(x,collapse='_')))
  
  return(name)
}