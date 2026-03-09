ME_PHharm=function(ph,TEXCLASS, model="polynomial", phtype="cacl2"){
  TEXCLASS2=ifelse(TEXCLASS=="Cl",1,ifelse(TEXCLASS=="ClLo",7,ifelse(TEXCLASS=="Lo",11, ifelse(TEXCLASS=="LoSa",10,
                                                                                               ifelse(TEXCLASS=="Sa",12,ifelse(TEXCLASS=="SaCl",8,ifelse(TEXCLASS=="SaClLo",9,
                                                                                                                                                         ifelse(TEXCLASS=="SaLo",5,ifelse(TEXCLASS=="SiCl",2,ifelse(TEXCLASS=="SiClLo",3,
                                                                                                                                                                                                                    ifelse(TEXCLASS=="SiLo",4,ifelse(TEXCLASS=="Si",6,ifelse(TEXCLASS=="CS",13,
                                                                                                                                                                                                                                                                             ifelse(TEXCLASS=="MS",14,ifelse(TEXCLASS=="HCl",16,ifelse(TEXCLASS=="FS",15,0))))))))))))))))

  if(phtype=="cacl2"){
    dfm=data.frame(PHCA=c(ph),TEXCLASS1=c(TEXCLASS2))
    if(model=="polynomial"){PHharm=predict(ME_PHharmserve[[1]],dfm)}
    else if(model=="sigmoid"){PHharm=predict(ME_PHharmserve[[2]],dfm)}
    else if(model=="spherical"){PHharm=predict(ME_PHharmserve[[3]],dfm)}
    else if(model=="gaussian"){PHharm=predict(ME_PHharmserve[[4]],dfm)}
    else if(model=="exponential"){PHharm=predict(ME_PHharmserve[[5]],dfm)}
    else if(model=="power"){PHharm=predict(ME_PHharmserve[[6]],dfm)}
    else if(model=="linear"){PHharm=predict(ME_PHharmserve[[7]],dfm)}
  }
  else if(phtype=="kcl"){
    dfm=data.frame(PHKC=c(ph),TEXCLASS1=c(TEXCLASS2))
    if(model=="polynomial"){PHharm=predict(ME_PHharmserve[[8]],dfm)}
    else if(model=="sigmoid"){PHharm=predict(ME_PHharmserve[[9]],dfm)}
    else if(model=="spherical"){PHharm=predict(ME_PHharmserve[[10]],dfm)}
    else if(model=="gaussian"){PHharm=predict(ME_PHharmserve[[11]],dfm)}
    else if(model=="exponential"){PHharm=predict(ME_PHharmserve[[12]],dfm)}
    else if(model=="power"){PHharm=predict(ME_PHharmserve[[13]],dfm)}
    else if(model=="linear"){PHharm=predict(ME_PHharmserve[[14]],dfm)}
  }
  PHharm=ifelse(PHharm<1,"NA",ifelse(PHharm>14,"NA",PHharm))
  return(PHharm)
}
