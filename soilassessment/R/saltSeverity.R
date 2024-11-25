saltSeverity=function(ec,ph,esp,criterion="FAO"){
  esp=esp*1
  ec=ec*1
  ph=round(ph*1,2)
  if(criterion=="FAO"){
    sal=ifelse(negData("ec",ec)<0.75,1,ifelse(negData("ec",ec)<2,2,ifelse(negData("ec",ec)<4,3,ifelse(negData("ec",ec)<8,4,ifelse(negData("ec",ec)<15,5,6)))))
    sod=ifelse(negData("esp",esp)<15,1,(ifelse(negData("esp",esp)<30,7,ifelse(negData("esp",esp)<50,8,ifelse(negData("esp",esp)<70,9,10)))))
    intenclass=ifelse(negData("ph",ph)<8.2,ifelse(esp<15,sal,ifelse(ec<4,13,12)),ifelse(esp<15,ifelse(ec<4,11,13),ifelse(ec<4,sod,13)))
   }
  if(criterion=="USDA"){
    sal=ifelse(negData("ec",ec)<2,1,ifelse(negData("ec",ec)<4,2,ifelse(negData("ec",ec)<8,3,ifelse(negData("ec",ec)<16,4,6))))
    sod=ifelse(negData("esp",esp)<15,1,(ifelse(negData("esp",esp)<30,7,ifelse(negData("esp",esp)<50,8,ifelse(negData("esp",esp)<70,9,10)))))
    intenclass=ifelse(negData("ph",ph)<8.5,ifelse(negData("esp",esp)<15,sal,ifelse(negData("ec",ec)<4,13,12)),ifelse(negData("esp",esp)<15,ifelse(negData("ec",ec)<4,11,13),ifelse(negData("ec",ec)<4,sod,13)))
  }
  else if(criterion=="Amrhein"){
    sal=ifelse(negData("ec",ec)<0.75,1,ifelse(negData("ec",ec)<2,2,ifelse(negData("ec",ec)<4,3,ifelse(negData("ec",ec)<8,4,ifelse(negData("ec",ec)<15,5,6)))))
    sod=ifelse(negData("esp",esp)<6,1,(ifelse(negData("esp",esp)<10,7,ifelse(negData("esp",esp)<15,8,10))))
    intenclass=ifelse(negData("ph",ph)<8.5,ifelse(negData("esp",esp)<15,sal,ifelse(negData("ec",ec)<4,13,12)),ifelse(negData("esp",esp)<15,ifelse(negData("ec",ec)<4,11,13),ifelse(negData("ec",ec)<4,sod,13)))
  }
  else if(criterion=="PSALT"){
    sal=ifelse(negData("ec",ec)<0.25,1,ifelse(negData("ec",ec)<0.5,2,ifelse(negData("ec",ec)<1,3,ifelse(negData("ec",ec)<2,4,6))))
    sod=ifelse(negData("esp",esp)<15,1,(ifelse(negData("esp",esp)<30,7,ifelse(negData("esp",esp)<50,8,ifelse(negData("esp",esp)<70,9,10)))))
    intenclass=ifelse(negData("ph",ph)<8.5,ifelse(negData("esp",esp)<15,sal,ifelse(negData("ec",ec)<0.25,13,12)),ifelse(negData("esp",esp)<15,ifelse(negData("ec",ec)<0.25,11,13),ifelse(negData("ec",ec)<0.25,sod,13)))
  }
  return(intenclass)
}

