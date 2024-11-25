classnames=function(indicator="texture"){
  if(indicator=="salinity"){
    classcodes=matrix(c(1,"None",2,"Slight",7,"VerySlight",3,"Moderate",4,"Strong",5,"VeryStrong",6,"Extreme", 8,"Sodic", 9,"Saline-Sodic",10,"Salty"),nrow = 2,ncol = 10,byrow = FALSE)

  }
  if(indicator=="texture"){
    classcodes=matrix(c(1,"CL","Clay",2,"SiCl","SiltyCLay",3,"SiClLo","SiltyCLayLoam",4,"SiLo","SiltyLoam",5,"SaLo","SandyLoam",6,"Si","Silty",7,"ClLo","ClayLoam",8,"SaCl","SandyClay",9,"SaClLo","SandyClayLoam",10,"LoSa","LoamySand",11,"Lo","Loam",12,"Sa","Sandy",13,"CS","CoarseSand",14,"MS","MediumSand",15,"FS","FineSand",16,"HCL","HeavyClay"),nrow = 3,ncol = 16,byrow = FALSE)

  }
  if(indicator=="suitability"){
    classcodes=matrix(c(1,"High",2,"Moderate",3,"Marginal",4,"NotsuitableNow",5,"NotSuitable"),nrow = 2,ncol = 5,byrow = FALSE)

  }
  if(indicator=="drainage"){
    classcodes=matrix(c(1,"VPr","VeryPoorlyDrained",2,"PDr","PoorlyDrained",3,"Imp","ImperfectlyDrained",4,"Modr","ModeratelyDrained",5,"WDr","WellDrained",6,"SDr","SomewhatExcessivelyDrained", 7,"Exdr","ExcessivelyDrained"),nrow = 3,ncol = 7,byrow = FALSE)

  }
  if(indicator=="fertility"){
    classcodes=matrix(c(1,"High",2,"Moderate",3,"Low",4,"VeryLow"),nrow = 2,ncol = 4,byrow = FALSE)

  }
  if(indicator=="erodibility"){
    classcodes=matrix(c(1,"High",2,"Moderate",3,"Low",4,"VeryLow"),nrow = 2,ncol = 4,byrow = FALSE)
}
  if(indicator=="permeability"){
      classcodes=matrix(c(1,"VerySlow",2,"Slow", 3,"ModeratelySlow", 4,"Moderate", 5,"ModeratelyRapid",6,"Rapid",7,"VeryRapid"),nrow = 2,ncol = 7,byrow = FALSE)
    }
  if(indicator=="structure"){
      classcodes=matrix(c(1,"Granular",2,"Crumby", 3,"AngularBlocky", 4,"Columnar", 5,"SubAngularBlocky",6,"Platty",7,"SingleGrain",8,"Massive",9,"Prismatic",10,"Cloddy"),nrow = 2,ncol = 10,byrow = FALSE)
    }

return(classcodes)
}
