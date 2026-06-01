# Function to read and change parameters
#
change_parm <- function(folder_name1,file_name1,parm_name1,line_number1,parm_type1,parm_vector1,parm1,attrib_str1,isitR1)
{
# Read the file with information about the Inverse Problem.
 folder<-getwd()
 nn <- length(folder_name1)
 i_cp<-1
 while (i_cp<=nn)
 {
  if(folder_name1[i_cp]!='wd')
   {setwd(folder_name1[i_cp])}
  if(isitR1==TRUE)
   {my_data<-eval(parse(text=paste(".GlobalEnv$my",file_name1[i_cp],sep='')))
   }else{my_data <- readLines(file_name1[i_cp])}
  if (line_number1[i_cp]==0)
  {
   if (parm_type1[i_cp]==1)
    {busca <- paste(parm_name1[i_cp],'.*',attrib_str1,'.*',sep='')
     my_data <- sub(busca,paste(parm_name1[i_cp],attrib_str1,toString(parm1[i_cp]),sep=''),my_data)
    }else if (parm_type1[i_cp]==2)
    {cont<-length(my_data)
     k_cp<-1
     while(k_cp<=cont)
     {busca <- unlist(strsplit(my_data[k_cp],attrib_str1))
      if(isTRUE(gsub(" ","",busca[1])==parm_name1[i_cp]))
      {vetor<-unlist(strsplit(busca[2],' '))
      auxi<-paste(parm_name1[i_cp],attrib_str1,sep='')
      num<-length(vetor)-1
      p_cp<-1
      while(p_cp<=num)
      {if(isTRUE(p_cp!=parm_vector1[i_cp]))
       {auxi<-paste(auxi,vetor[p_cp+1],sep=' ')
       }else{
       auxi<-paste(auxi,parm1[i_cp],sep=' ')}
       p_cp<-p_cp+1
      }
      my_data[k_cp]<-auxi
      }
      k_cp<-k_cp+1
      }
    }
  }else{
   my_data[line_number1[i_cp]]<-toString(parm1[i_cp])
  }
  if(isitR1==TRUE)
   {eval(parse(text=paste(".GlobalEnv$my",file_name1[i_cp],"<-my_data",sep='')))
   }else{writeLines(my_data,file_name1[i_cp])}
  setwd(folder)
  i_cp<-i_cp+1
 }
 return()
}

# Function to read the R each "file_name" when (isitR==TRUE)
#
readRfiles <- function(folder_name1,file_name1)
{
 folder<-getwd()
 nn <- length(folder_name1)
 i_rf<-1
 while (i_rf<=nn)
 {
   if(folder_name1[i_rf]!='wd')
    {setwd(folder_name1[i_rf])}
   eval(parse(text=paste(".GlobalEnv$my",file_name1[i_rf],"<-readLines('",file_name1[i_rf],"')",sep='')))
   setwd(folder)
 i_rf<-i_rf+1
 }
}
