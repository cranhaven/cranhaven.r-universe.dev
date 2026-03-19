bacteria_lysed<-function(Size,bacteria_number,matrix_pb,list_pn,bacteria_names){
  list_bacteria_lysed=c()


  if(Size==1){
    for (j in 1:bacteria_number){
    if(matrix_pb[j,list_pn[1]] ){
      list_bacteria_lysed=c(list_bacteria_lysed,bacteria_names[j])
      }
    }
    return(list_bacteria_lysed)
  }

  if(Size==2){
    for (j in 1:bacteria_number){
      if(matrix_pb[j,list_pn[1]]| matrix_pb[j,list_pn[2]]){
        list_bacteria_lysed=c(list_bacteria_lysed,bacteria_names[j])
      }
    }
    return(list_bacteria_lysed)
  }

  if(Size==3){
    for (j in 1:bacteria_number){
      if(matrix_pb[j,list_pn[1]]| matrix_pb[j,list_pn[2]]| matrix_pb[j,list_pn[3]]){
        list_bacteria_lysed=c(list_bacteria_lysed,bacteria_names[j])
      }
    }
    return(list_bacteria_lysed)
  }

  if(Size==4){

    for (j in 1:bacteria_number){
      if(matrix_pb[j,list_pn[1]] | matrix_pb[j,list_pn[2]]| matrix_pb[j,list_pn[3]]| matrix_pb[j,list_pn[4]]){
        list_bacteria_lysed=c(list_bacteria_lysed,bacteria_names[j])
      }
    }
    return(list_bacteria_lysed)
  }

  if(Size==5){

    for (j in 1:bacteria_number){
      if(matrix_pb[j,list_pn[1]] | matrix_pb[j,list_pn[2]]| matrix_pb[j,list_pn[3]]| matrix_pb[j,list_pn[4]]| matrix_pb[j,list_pn[5]]){
        list_bacteria_lysed=c(list_bacteria_lysed,bacteria_names[j])
      }
    }
    return(list_bacteria_lysed)
  }

  if(Size==6){

    for (j in 1:bacteria_number){
      if(matrix_pb[j,list_pn[1]] | matrix_pb[j,list_pn[2]]| matrix_pb[j,list_pn[3]]| matrix_pb[j,list_pn[4]]| matrix_pb[j,list_pn[5]]| matrix_pb[j,list_pn[6]]){
        list_bacteria_lysed=c(list_bacteria_lysed,bacteria_names[j])
      }
    }
    return(list_bacteria_lysed)
  }

  if(Size==7){

    for (j in 1:bacteria_number){
      if(matrix_pb[j,list_pn[1]] | matrix_pb[j,list_pn[2]]| matrix_pb[j,list_pn[3]]| matrix_pb[j,list_pn[4]]| matrix_pb[j,list_pn[5]]| matrix_pb[j,list_pn[6]]| matrix_pb[j,list_pn[7]]){
        list_bacteria_lysed=c(list_bacteria_lysed,bacteria_names[j])
      }
    }
    return(list_bacteria_lysed)
  }

}
