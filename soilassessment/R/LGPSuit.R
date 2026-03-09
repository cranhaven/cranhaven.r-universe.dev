LGPSuit=function(value,crop="wheat"){
  #lgp (length of growing season/period) suitability requirements for cereals
  if(crop=="wheat"){suitclass=ifelse(value>150,1,ifelse(value>120,2,ifelse(value>90,3,4)))}
  else if(crop=="rice"){suitclass=ifelse(value>110,1,ifelse(value>90,2,ifelse(value>80,3,4)))}
  else if(crop=="maize"){suitclass=ifelse(value>100,1,ifelse(value>80,2,ifelse(value>60,3,4)))}
  else if(crop=="sorghum"){suitclass=ifelse(value>150,1,ifelse(value>120,2,ifelse(value>90,3,4)))}
  else if(crop=="millet"){suitclass=ifelse(value>110,1,ifelse(value>90,2,ifelse(value>60,3,4)))}
  else if(crop=="barley"){suitclass=ifelse(value>150,1,ifelse(value>120,2,ifelse(value>90,3,4)))}
  else if(crop=="oat"){suitclass=ifelse(value>180,1,ifelse(value>120,2,ifelse(value>90,3,4)))}

  #lgp (length of growing season/period) suitability requirements for legumes
  else if(crop=="groundnut"){suitclass=ifelse(value>100,1,ifelse(value>90,2,ifelse(value>75,3,4)))}
  else if(crop=="groundnut"){suitclass=ifelse(value>120,1,ifelse(value>90,2,ifelse(value>70,3,4)))}
  else if(crop=="gram"){suitclass=ifelse(value>110,1,ifelse(value>90,2,ifelse(value>70,3,4)))}
  else if(crop=="pea"){suitclass=ifelse(value>120,1,ifelse(value>100,2,ifelse(value>80,3,4)))}
  else if(crop=="soybean"){suitclass=ifelse(value>120,1,ifelse(value>100,2,ifelse(value>85,3,4)))}
  else if(crop=="lentil"){suitclass=ifelse(value>120,1,ifelse(value>100,2,ifelse(value>80,3,4)))}

  #lgp (length of growing season/period) suitability requirements for forests
  else if(crop=="poplar"){suitclass=ifelse(value>100,1,ifelse(value>90,2,ifelse(value>75,3,4)))}
  else if(crop=="grevillea"){suitclass=ifelse(value>120,1,ifelse(value>90,2,ifelse(value>70,3,4)))}
  else if(crop=="sesbania"){suitclass=ifelse(value>110,1,ifelse(value>90,2,ifelse(value>70,3,4)))}
  else if(crop=="calliandra"){suitclass=ifelse(value>120,1,ifelse(value>100,2,ifelse(value>80,3,4)))}
  else if(crop=="leucaena"){suitclass=ifelse(value>120,1,ifelse(value>100,2,ifelse(value>85,3,4)))}
  else if(crop=="acacia"){suitclass=ifelse(value>120,1,ifelse(value>100,2,ifelse(value>80,3,4)))}
  else if(crop=="eucalyptus"){suitclass=ifelse(value>120,1,ifelse(value>100,2,ifelse(value>80,3,4)))}
  else if(crop=="teak"){suitclass=ifelse(value>120,1,ifelse(value>100,2,ifelse(value>80,3,4)))}
  else if(crop=="maple"){suitclass=ifelse(value>120,1,ifelse(value>100,2,ifelse(value>80,3,4)))}
  else if(crop=="ash"){suitclass=ifelse(value>120,1,ifelse(value>100,2,ifelse(value>80,3,4)))}
  
  #lgp (length of growing season/period) suitability requirements for oil crops
  else if(crop=="sesame"){suitclass=ifelse(value>90,1,ifelse(value>70,2,ifelse(value>60,3,4)))}
  else if(crop=="oilpalm"){suitclass=ifelse(value>120,4,ifelse(value>100,3,ifelse(value>90,2,1)))}
  else if(crop=="castor"){suitclass=ifelse(value>150,1,ifelse(value>120,2,ifelse(value>90,3,4)))}
  else if(crop=="mustard"){suitclass=ifelse(value>100,1,ifelse(value>90,2,ifelse(value>75,3,4)))}
  else if(crop=="safflower"){suitclass=ifelse(value>160,1,ifelse(value>120,2,ifelse(value>90,3,4)))}
  else if(crop=="rapeseed"){suitclass=ifelse(value>100,1,ifelse(value>85,2,ifelse(value>75,3,4)))}
  else if(crop=="olive"){stop("not included")}

  #lgp (length of growing season/period) suitability requirements for nuts
  else if(crop=="cashew"){suitclass=ifelse(value>210,1,ifelse(value>150,2,ifelse(value>90,3,4)))}
  else if(crop=="coconut"){stop("not included")}
  else if(crop=="pistachio"){stop("not included")}
  else if(crop=="almond"){stop("not included")}

  #lgp crop suitability requirements for industrial crops
  else if(crop=="cotton"){suitclass=ifelse(value>240,1,ifelse(value>180,2,ifelse(value>120,3,4)))}
  else if(crop=="sugarcane"){stop("not included")}
  else if(crop=="tea"){suitclass=ifelse(value>240,1,ifelse(value>180,2,ifelse(value>120,3,4)))}
  else if(crop=="coffee"){suitclass=ifelse(value>320,1,ifelse(value>240,2,ifelse(value>180,3,4)))}
  else if(crop=="saffron"){suitclass=ifelse(value>150,1,ifelse(value>110,2,ifelse(value>90,3,4)))}
  else if(crop=="jute"){suitclass=ifelse(value>120,1,ifelse(value>100,2,ifelse(value>90,3,4)))}
  else if(crop=="tobacco"){suitclass=ifelse(value>120,1,ifelse(value>100,2,ifelse(value>90,3,4)))}
  else if(crop=="pyrethrum"){suitclass=ifelse(value>120,1,ifelse(value>100,2,ifelse(value>80,3,4)))}
  else if(crop=="rubber"){stop("not included")}

  #lgp crop suitability requirements for tuber
  else if(crop=="potato"){suitclass=ifelse(value>120,1,ifelse(value>100,2,ifelse(value>80,3,4)))}
  else if(crop=="cassava"){suitclass=ifelse(value>120,1,ifelse(value>100,2,ifelse(value>80,3,4)))}
  else if(crop=="turnip"){suitclass=ifelse(value>60,1,ifelse(value>40,2,ifelse(value>30,3,4)))}
  else if(crop=="radish"){suitclass=ifelse(value>60,1,ifelse(value>40,2,ifelse(value>30,3,4)))}
  else if(crop=="sweetpotato"){suitclass=ifelse(value>120,1,ifelse(value>100,2,ifelse(value>80,3,4)))}
  else if(crop=="carrot"){suitclass=ifelse(value>60,1,ifelse(value>40,2,ifelse(value>30,3,4)))}

  #lgp crop suitability requirements for fruits
  else if(crop=="mango"){suitclass=ifelse(value>180,1,ifelse(value>150,2,ifelse(value>120,3,4)))}
  else if(crop=="grapes"){stop("not included")}
  else if(crop=="citrus"){suitclass=ifelse(value>240,1,ifelse(value>180,2,ifelse(value>150,3,4)))}
  else if(crop=="pomegranate"){suitclass=ifelse(value>150,1,ifelse(value>120,2,ifelse(value>90,3,4)))}
  else if(crop=="banana"){stop("not included")}
  else if(crop=="watermelon"){suitclass=ifelse(value>110,1,ifelse(value>90,2,ifelse(value>60,3,4)))}
  else if(crop=="melon"){suitclass=ifelse(value>90,1,ifelse(value>70,2,ifelse(value>60,3,4)))}
  else if(crop=="pineaple"){stop("not included")}
  else if(crop=="pawpaw"){stop("not included")}
  else if(crop=="avocado"){stop("not included")}

  #lgp crop suitability requirements for vegetables
  else if(crop=="cabbages"){suitclass=ifelse(value>180,1,ifelse(value>120,2,ifelse(value>90,3,4)))}
  else if(crop=="vegetable"){suitclass=ifelse(value>150,1,ifelse(value>120,2,ifelse(value>90,3,4)))}
  else if(crop=="tomato"){suitclass=ifelse(value>150,1,ifelse(value>120,2,ifelse(value>90,3,4)))}
  else if(crop=="broccoli"){suitclass=ifelse(value>150,1,ifelse(value>120,2,ifelse(value>90,3,4)))}
  else if(crop=="okra"){suitclass=ifelse(value>100,1,ifelse(value>70,2,ifelse(value>60,3,4)))}
  else if(crop=="cauliflower"){suitclass=ifelse(value>90,1,ifelse(value>70,2,ifelse(value>60,3,4)))}

  #lgp crop suitability requirements for spices
  else if(crop=="chilli"){suitclass=ifelse(value>180,1,ifelse(value>120,2,ifelse(value>90,3,4)))}
  else if(crop=="pepper"){suitclass=ifelse(value>180,1,ifelse(value>120,2,ifelse(value>90,3,4)))}
  else if(crop=="onion"){suitclass=ifelse(value>110,1,ifelse(value>90,2,ifelse(value>70,3,4)))}
  else if(crop=="ginger"){stop("not included")}
  else if(crop=="tumeric"){stop("not included")}
  else if(crop=="lemongrass"){suitclass=ifelse(value>210,1,ifelse(value>180,2,ifelse(value>150,3,4)))}
  else if(crop=="cardamom"){stop("not included")}
  else if(crop=="vanilla"){stop("not included")}

  #lgp crop suitability requirements for others
  else if(crop=="alfalfa"){stop("not included")}
  else if(crop=="rose"){stop("not included")}
  else if(crop=="jasmine"){stop("not included")}

  #lgp crop suitability requirements for fleshy
  else if(crop=="yam"){stop("not included")}
  else if(crop=="butternut"){suitclass=ifelse(value>120,1,ifelse(value>90,2,ifelse(value>80,3,4)))}
  else if(crop=="pumpkin"){suitclass=ifelse(value>150,1,ifelse(value>110,2,ifelse(value>90,3,4)))}
  else if(crop=="squash"){suitclass=ifelse(value>180,1,ifelse(value>110,2,ifelse(value>90,3,4)))}

  return(suitclass)
}
