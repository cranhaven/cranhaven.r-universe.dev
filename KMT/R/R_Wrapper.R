

GetGiMat = function(strDistr, Xhat){

  #Data_Folder = "C:/Users/ji_wo/Documents/R1/GofKMT2024/data/"
  #path = paste(Data_Folder, strDistr, "_table.rda", sep="")
  #load(path)

  # #SMat = Normal_table$SMat
  # tmpstr = paste0("SMat = ", strDistr, "_table$SMat")
  # eval(parse(text=tmpstr))
  # 
  # #VMat = Normal_table$VMat
  # tmpstr = paste0("VMat = ", strDistr, "_table$VMat")
  # eval(parse(text=tmpstr))
  # 
  # #ReVec = Normal_table$ReVec
  # tmpstr = paste0("ReVec = ", strDistr, "_table$ReVec")
  # eval(parse(text=tmpstr))
  # 
  # #xVec = Normal_table$xVec
  # tmpstr = paste0("xVec = ", strDistr, "_table$xVec")
  # eval(parse(text=tmpstr))

  
  
  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  

  G = Get_G_Mat(strDistr, Xhat,
            SMat, VMat,
            ReVec, xVec)
  return(G)
}







fl = function(x, strDistr){

  
  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  
  
  lst = Init_Distr(strDistr, SMat, VMat, ReVec, xVec, x)
  out = lst[[1]]
  return(out)

}



Fl = function(x, strDistr){

  
  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  
  
  lst = Init_Distr(strDistr, SMat, VMat, ReVec, xVec, x)
  out = lst[[2]]
  return(out)

}



re = function(x, strDistr){

  
  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  
  
  lst = Init_Distr(strDistr, SMat, VMat, ReVec, xVec, x)
  out = lst[[3]]
  return(out)

}


Re = function(x, strDistr){

  
  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  
  
  lst = Init_Distr(strDistr, SMat, VMat, ReVec, xVec, x)
  out = lst[[4]]
  return(out)

}


phix = function(x, strDistr){

  
  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  
  lst = Init_Distr(strDistr, SMat, VMat, ReVec, xVec, x)
  out = lst[[5]]
  return(out)

}


v0 = function(x, strDistr){

  
  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  
  lst = Init_Distr(strDistr, SMat, VMat, ReVec, xVec, x)
  out = lst[[6]]
  return(out)

}


v1 = function(x, strDistr){

  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  
  
  lst = Init_Distr(strDistr, SMat, VMat, ReVec, xVec, x)
  out = lst[[7]]
  return(out)

}


v2 = function(x, strDistr){

  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  
  lst = Init_Distr(strDistr, SMat, VMat, ReVec, xVec, x)
  out = lst[[8]]
  return(out)

}


c0 = function(x, strDistr){

  
  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  
  lst = Init_Distr(strDistr, SMat, VMat, ReVec, xVec, x)
  out = lst[[9]]
  return(out)

}


c1 = function(x, strDistr){

  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  
  
  lst = Init_Distr(strDistr, SMat, VMat, ReVec, xVec, x)
  out = lst[[10]]
  return(out)

}


c2 = function(x, strDistr){

  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  
  
  lst = Init_Distr(strDistr, SMat, VMat, ReVec, xVec, x)
  out = lst[[11]]
  return(out)

}


Gamma = function(x, strDistr){

  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  
  
  lst = Init_Distr(strDistr, SMat, VMat, ReVec, xVec, x)
  out = lst[[12]]
  return(out)

}


s1 = function(x, strDistr){

  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  
  lst = Init_Distr(strDistr, SMat, VMat, ReVec, xVec, x)
  out = lst[[13]]
  return(out)

}


s2 = function(x, strDistr){

  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  
  
  lst = Init_Distr(strDistr, SMat, VMat, ReVec, xVec, x)
  out = lst[[14]]
  return(out)

}


s3 = function(x, strDistr){

  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  
  
  lst = Init_Distr(strDistr, SMat, VMat, ReVec, xVec, x)
  out = lst[[15]]
  return(out)

}



S1 = function(x, strDistr){

  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  
  
  lst = Init_Distr(strDistr, SMat, VMat, ReVec, xVec, x)
  out = lst[[16]]
  return(out)

}


S2 = function(x, strDistr){

  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  
  
  lst = Init_Distr(strDistr, SMat, VMat, ReVec, xVec, x)
  out = lst[[17]]
  return(out)

}


S3 = function(x, strDistr){

  if(strDistr=="Normal"){
    tbl = Normal_table
  }else if(strDistr=="Logistic"){
    tbl = Logistic_table
  }else if(strDistr=="Cauchy"){
    tbl = Cauchy_table
  }
  
  SMat = tbl$SMat
  xVec = tbl$xVec
  VMat = tbl$VMat
  ReVec = tbl$ReVec
  
  
  
  lst = Init_Distr(strDistr, SMat, VMat, ReVec, xVec, x)
  out = lst[[18]]
  return(out)

}





