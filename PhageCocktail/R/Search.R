# Search
Search<-function(type,MaxPhage,MaxBacteria,new_matrix,phage_names){

  if (type==1){

    ResultS<-Search1(MaxPhage,MaxBacteria, new_matrix,phage_names)
    return(c(ResultS))
  }
  if (type==2){

    ResultS<-Search2(MaxPhage,MaxBacteria, new_matrix,phage_names)
    return(c(ResultS))
  }
  if (type==3){

    ResultS<-Search3(MaxPhage,MaxBacteria, new_matrix,phage_names)
    return(c(ResultS))
  }
  if (type==4){

    ResultS<-Search4(MaxPhage,MaxBacteria, new_matrix,phage_names)
    return(c(ResultS))
  }
  if (type==5){

    ResultS<-Search5(MaxPhage,MaxBacteria, new_matrix,phage_names)
    return(c(ResultS))
  }
  if (type==6){

    ResultS<-Search6(MaxPhage,MaxBacteria, new_matrix,phage_names)
    return(c(ResultS))
  }
  if (type==7){

    ResultS<-Search7(MaxPhage,MaxBacteria, new_matrix,phage_names)
    return(c(ResultS))
  }

}
