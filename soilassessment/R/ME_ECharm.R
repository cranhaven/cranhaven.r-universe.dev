ME_ECharm=function(EC,TEXCLASS, model="polynomial", soilsolution="1:2"){
  #TEXCLASS,Cl=1, ClLo =7,Lo=11,LoSa=10,Sa=12,SaCl=8,SaClLo=9,SaLo=5,SiCl=2,SiClLo=3,SiLo=4,Si=6,CS=13,MS=14,HCL=16,FS=15
  TEXCLASS2=ifelse(TEXCLASS=="Cl",1,ifelse(TEXCLASS=="ClLo",7,ifelse(TEXCLASS=="Lo",11, ifelse(TEXCLASS=="LoSa",10,
                                                                                               ifelse(TEXCLASS=="Sa",12,ifelse(TEXCLASS=="SaCl",8,ifelse(TEXCLASS=="SaClLo",9,
                                                                                                                                                         ifelse(TEXCLASS=="SaLo",5,ifelse(TEXCLASS=="SiCl",2,ifelse(TEXCLASS=="SiClLo",3,
                                                                                                                                                                                                                    ifelse(TEXCLASS=="SiLo",4,ifelse(TEXCLASS=="Si",6,ifelse(TEXCLASS=="CS",13,
                                                                                                                                                                                                                                                                             ifelse(TEXCLASS=="MS",14,ifelse(TEXCLASS=="HCl",16,ifelse(TEXCLASS=="FS",15,0))))))))))))))))

  if (soilsolution=="1:2"){
    dfm=data.frame(EC2=c(EC),TEXCLASS1=c(TEXCLASS2))
    if(model=="polynomial"){ECharm=predict(ME_ECharmserve[[1]],dfm)}
    else if(model=="sigmoid"){ECharm=predict(ME_ECharmserve[[2]],dfm)}
    else if(model=="spherical"){ECharm=predict(ME_ECharmserve[[3]],dfm)}
    else if(model=="gaussian"){ECharm=predict(ME_ECharmserve[[4]],dfm)}
    else if(model=="linear"){ECharm=predict(ME_ECharmserve[[5]],dfm)}
    else if(model=="power"){ECharm=predict(ME_ECharmserve[[6]],dfm)}
    else if(model=="exponential"){ECharm=predict(ME_ECharmserve[[7]],dfm)}}
  else if(soilsolution=="1:2.5"){
    dfm=data.frame(EC25=c(EC),TEXCLASS1=c(TEXCLASS2))
    if(model=="polynomial"){ECharm=predict(ME_ECharmserve[[8]],dfm)}
    else if(model=="sigmoid"){ECharm=predict(ME_ECharmserve[[9]],dfm)}
    else if(model=="spherical"){ECharm=predict(ME_ECharmserve[[10]],dfm)}
    else if(model=="gaussian"){ECharm=predict(ME_ECharmserve[[11]],dfm)}
    else if(model=="linear"){ECharm=predict(ME_ECharmserve[[12]],dfm)}
    else if(model=="power"){ECharm=predict(ME_ECharmserve[[13]],dfm)}
    else if(model=="exponential"){ECharm=predict(ME_ECharmserve[[14]],dfm)}}
  else if(soilsolution=="1:5"){
    dfm=data.frame(EC50=c(EC),TEXCLASS1=c(TEXCLASS2))
    if(model=="polynomial"){ECharm=predict(ME_ECharmserve[[15]],dfm)}
    else if(model=="sigmoid"){ECharm=predict(ME_ECharmserve[[16]],dfm)}
    else if(model=="spherical"){ECharm=predict(ME_ECharmserve[[17]],dfm)}
    else if(model=="gaussian"){ECharm=predict(ME_ECharmserve[[18]],dfm)}
    else if(model=="linear"){ECharm=predict(ME_ECharmserve[[19]],dfm)}
    else if(model=="power"){ECharm=predict(ME_ECharmserve[[20]],dfm)}
    else if(model=="exponential"){ECharm=predict(ME_ECharmserve[[21]],dfm)}}

  ECharm=ifelse(ECharm<0,0,ECharm)
  return(ECharm)
}
