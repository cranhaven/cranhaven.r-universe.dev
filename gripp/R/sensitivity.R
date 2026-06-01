# Function to calculate the sensitivity matrix
#
#' @export
sensitivity <- function(parm_s)
#' @importFrom utils write.table read.csv write.csv
#' @importFrom GenSA GenSA
#' @importFrom GA ga
#' @importFrom stats rnorm
{
# Read the file with information about the Inverse Problem.
 nn <- length(.GlobalEnv$file_name)
 #
 # reading each file_name information
 readRfiles(.GlobalEnv$folder_name,.GlobalEnv$file_name)
 #
 change_parm(.GlobalEnv$folder_name,.GlobalEnv$file_name,.GlobalEnv$parm_name,.GlobalEnv$line_number,.GlobalEnv$parm_type,.GlobalEnv$parm_vector,parm_s,.GlobalEnv$attrib_str,.GlobalEnv$isitR)
 #
 # running the command
 if(.GlobalEnv$command_folder!='wd')
 {
   if (.GlobalEnv$isitR==TRUE)
   {my<-eval(parse(text=paste(".GlobalEnv$my",.GlobalEnv$command,sep='')))
   eval(parse(text=my))
   resultado1 <- .GlobalEnv$results
   }else{system(paste(.GlobalEnv$command_folder,'/',.GlobalEnv$command,sep=''))}
 }else{
   if (.GlobalEnv$isitR==TRUE)
   {my<-eval(parse(text=paste(".GlobalEnv$my",.GlobalEnv$command,sep='')))
   eval(parse(text=my))
   resultado1 <- .GlobalEnv$results
   }else{system(.GlobalEnv$command)}
 }
 #
 # reading the results after running the command
 # only needed if the function is not a R routine
 if (.GlobalEnv$isitR==FALSE)
 {if(.GlobalEnv$result_folder!='wd')
  {resultado1 <- unlist(strsplit(readLines(paste(.GlobalEnv$result_folder,'/',.GlobalEnv$result,sep='')),' '))
  }else{resultado1 <- unlist(strsplit(readLines(.GlobalEnv$result),' '))}
 }
 mm <- length(resultado1)
 sensitivity <- matrix(ncol=nn,nrow=mm)
 parm_dif <- .GlobalEnv$parm_max - .GlobalEnv$parm_min
 parm_max1 <- parm_s + (.GlobalEnv$ppdif / 100) * parm_dif
 parm_min1 <- parm_s - (.GlobalEnv$ppdif / 100) * parm_dif
 i_sn <- 1
 while(i_sn<=nn)
 {
   auxi<-parm_s
   # change all parameters and auxi[i_sn] to parm_max1[i_sn]
   auxi[i_sn] <- parm_max1[i_sn]
   change_parm(.GlobalEnv$folder_name,.GlobalEnv$file_name,.GlobalEnv$parm_name,.GlobalEnv$line_number,.GlobalEnv$parm_type,.GlobalEnv$parm_vector,auxi,.GlobalEnv$attrib_str,.GlobalEnv$isitR)
   # running the command
   if(.GlobalEnv$command_folder!='wd')
   {
     if (.GlobalEnv$isitR==TRUE)
     {my<-eval(parse(text=paste(".GlobalEnv$my",.GlobalEnv$command,sep='')))
     eval(parse(text=my))
     resultado1 <- .GlobalEnv$results
     }else{system(paste(.GlobalEnv$command_folder,'/',.GlobalEnv$command,sep=''))}
   }else{
     if (.GlobalEnv$isitR==TRUE)
     {my<-eval(parse(text=paste(".GlobalEnv$my",.GlobalEnv$command,sep='')))
     eval(parse(text=my))
     resultado1 <- .GlobalEnv$results
     }else{system(.GlobalEnv$command)}
   }
   #
   # reading the results after running the command
   # only needed if the function is not a R routine
   if (.GlobalEnv$isitR==FALSE)
    {if(.GlobalEnv$result_folder!='wd')
     {resultado1 <- unlist(strsplit(readLines(paste(.GlobalEnv$result_folder,'/',.GlobalEnv$result,sep='')),' '))
     }else{resultado1 <- unlist(strsplit(readLines(.GlobalEnv$result),' '))}
    }
   # change all parameters and auxi[i_sn] to parm_min[i_sn]
   auxi[i_sn] <- parm_min1[i_sn]
   change_parm(.GlobalEnv$folder_name,.GlobalEnv$file_name,.GlobalEnv$parm_name,.GlobalEnv$line_number,.GlobalEnv$parm_type,.GlobalEnv$parm_vector,auxi,.GlobalEnv$attrib_str,.GlobalEnv$isitR)
   # running the command
   if(.GlobalEnv$command_folder!='wd')
   {
     if (.GlobalEnv$isitR==TRUE)
     {my<-eval(parse(text=paste(".GlobalEnv$my",.GlobalEnv$command,sep='')))
     eval(parse(text=my))
     resultado2 <- .GlobalEnv$results
     }else{system(paste(.GlobalEnv$command_folder,'/',.GlobalEnv$command,sep=''))}
   }else{
     if (.GlobalEnv$isitR==TRUE)
     {my<-eval(parse(text=paste(".GlobalEnv$my",.GlobalEnv$command,sep='')))
     eval(parse(text=my))
     resultado2 <- .GlobalEnv$results
     }else{system(.GlobalEnv$command)}
   }
   #
   # reading the results after running the command
   # only needed if the function is not a R routine
   if (.GlobalEnv$isitR==FALSE)
    {if(.GlobalEnv$result_folder!='wd')
     {resultado2 <- unlist(strsplit(readLines(paste(.GlobalEnv$result_folder,'/',.GlobalEnv$result,sep='')),' '))
     }else{resultado2 <- unlist(strsplit(readLines(.GlobalEnv$result),' '))}
    }
   #
   # change everything back to original value (parm[i_sn])
   change_parm(.GlobalEnv$folder_name,.GlobalEnv$file_name,.GlobalEnv$parm_name,.GlobalEnv$line_number,.GlobalEnv$parm_type,.GlobalEnv$parm_vector,parm_s,.GlobalEnv$attrib_str,.GlobalEnv$isitR)
   j_sn <- 1
   while(j_sn<=mm)
   {
     sensitivity[j_sn,i_sn] <- (as.numeric(resultado1[j_sn])-as.numeric(resultado2[j_sn]))/(.GlobalEnv$parm_max[i_sn]-.GlobalEnv$parm_min[i_sn])
     j_sn <- j_sn+1
   }
   i_sn <- i_sn+1
 }
 return(sensitivity)
}
