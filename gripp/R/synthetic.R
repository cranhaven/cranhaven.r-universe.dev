# Synthetic Experimental Data function
#
#' @export
synthetic <- function(parm,sigma)
#' @importFrom utils write.table read.csv write.csv
#' @importFrom GenSA GenSA
#' @importFrom GA ga
#' @importFrom stats rnorm
{
 n <- length(.GlobalEnv$file_name)
 #
 # reading each file_name information
 readRfiles(.GlobalEnv$folder_name,.GlobalEnv$file_name)
 #
 # changing the parameters
 change_parm(.GlobalEnv$folder_name,.GlobalEnv$file_name,.GlobalEnv$parm_name,.GlobalEnv$line_number,.GlobalEnv$parm_type,.GlobalEnv$parm_vector,parm,.GlobalEnv$attrib_str,.GlobalEnv$isitR)
 #
 # running the command
 if(.GlobalEnv$command_folder!='wd')
 {
   if (.GlobalEnv$isitR==TRUE)
   {my<-eval(parse(text=paste(".GlobalEnv$my",.GlobalEnv$command,sep='')))
   eval(parse(text=my))
   resultado <- .GlobalEnv$results
   }else{system(paste(.GlobalEnv$command_folder,'/',.GlobalEnv$command,sep=''))}
 }else{
   if (.GlobalEnv$isitR==TRUE)
   {my<-eval(parse(text=paste(".GlobalEnv$my",.GlobalEnv$command,sep='')))
   eval(parse(text=my))
   resultado <- .GlobalEnv$results
   }else{system(.GlobalEnv$command)}
 }
 #
 # reading the results after running the command
 # only needed if the function is not a R routine
 if (.GlobalEnv$isitR==FALSE)
  {if(.GlobalEnv$result_folder!='wd')
   {resultado <- unlist(strsplit(readLines(paste(.GlobalEnv$result_folder,'/',.GlobalEnv$result,sep='')),' '))
  }else{resultado <- unlist(strsplit(readLines(.GlobalEnv$result),' '))}
  }
 nn <- length(resultado)
 erro <- rnorm(nn,mean=0,sd=sigma)
 resultado<-as.numeric(resultado)*(1+erro)
 return(resultado)
}
