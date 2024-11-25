depthSuit=function(value, crop="wheat"){
  #Soil depth (cm) suitability classes for cereals
  if(crop=="wheat"){suitclass=ifelse(value>65,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="maize"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="rice"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="sorghum"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>30,3,4)))}
  else if(crop=="barley"){suitclass=ifelse(value>90,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="millet"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>30,3,4)))}
  else if(crop=="oat"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>30,3,4)))}

  #Soil depth (cm) suitability classes for forests
  else if(crop=="poplar"){suitclass=ifelse(value>175,1,ifelse(value>150,2,ifelse(value>105,3,4)))}
  else if(crop=="grevillea"){suitclass=ifelse(value>175,1,ifelse(value>150,2,ifelse(value>75,3,4)))}
  else if(crop=="sesbania"){suitclass=ifelse(value>100,1,ifelse(value>85,2,ifelse(value>40,3,4)))}
  else if(crop=="calliandra"){suitclass=ifelse(value>175,1,ifelse(value>150,2,ifelse(value>125,3,4)))}
  else if(crop=="leucaena"){suitclass=ifelse(value>175,1,ifelse(value>100,2,ifelse(value>80,3,4)))}
  else if(crop=="eucalyptus"){suitclass=ifelse(value>275,1,ifelse(value>100,2,ifelse(value>80,3,4)))}
  else if(crop=="teak"){suitclass=ifelse(value>275,1,ifelse(value>100,2,ifelse(value>80,3,4)))}
  else if(crop=="maple"){suitclass=ifelse(value>275,1,ifelse(value>150,2,ifelse(value>100,3,4)))}
  else if(crop=="ash"){suitclass=ifelse(value>275,1,ifelse(value>150,2,ifelse(value>100,3,4)))}
  
  #Soil depth (cm) suitability classes for legumes
  else if(crop=="groundnut"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="bean"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="pea"){suitclass=ifelse(value>100,1,ifelse(value>85,2,ifelse(value>40,3,4)))}
  else if(crop=="gram"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="soybean"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>30,3,4)))}
  else if(crop=="lentil"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>30,3,4)))}
  
  #Soil depth (cm) suitability classes for oil crops
  else if(crop=="sesame"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="sunflower"){suitclass=ifelse(value>100,1,ifelse(value>75,2,ifelse(value>50,3,4)))}
  else if(crop=="oilpalm"){suitclass=ifelse(value>100,1,ifelse(value>75,2,ifelse(value>50,3,4)))}
  else if(crop=="castor"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="rapeseed"){suitclass=ifelse(value>100,1,ifelse(value>50,2,ifelse(value>30,3,4)))}
  else if(crop=="mustard"){suitclass=ifelse(value>100,1,ifelse(value>50,2,ifelse(value>30,3,4)))}
  else if(crop=="olive"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>30,3,4)))}
  else if(crop=="safflower"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>30,3,4)))}

  #Soil depth (cm) suitability classes for nuts
  else if(crop=="cashew"){suitclass=ifelse(value>150,1,ifelse(value>75,2,ifelse(value>50,3,4)))}
  else if(crop=="coconut"){suitclass=ifelse(value>100,1,ifelse(value>75,2,ifelse(value>50,3,4)))}
  else if(crop=="almond"){suitclass=ifelse(value>180,1,ifelse(value>75,2,ifelse(value>50,3,4)))}
  else if(crop=="pistachio"){suitclass=ifelse(value>180,1,ifelse(value>75,2,ifelse(value>50,3,4)))}

  #Soil depth (cm) suitability classes for industrial
  else if(crop=="cotton"){suitclass=ifelse(value>100,1,ifelse(value>60,2,ifelse(value>30,3,4)))}
  else if(crop=="sugarcane"){suitclass=ifelse(value>100,1,ifelse(value>75,2,ifelse(value>50,3,4)))}
  else if(crop=="tea"){suitclass=ifelse(value>150,1,ifelse(value>100,2,ifelse(value>500,3,4)))}
  else if(crop=="coffee"){suitclass=ifelse(value>100,1,ifelse(value>75,2,ifelse(value>50,3,4)))}
  else if(crop=="rubber"){stop("not included")}
  else if(crop=="jute"){suitclass=ifelse(value>150,1,ifelse(value>75,2,ifelse(value>50,3,4)))}
  else if(crop=="saffron"){suitclass=ifelse(value>100,1,ifelse(value>50,2,ifelse(value>30,3,4)))}
  else if(crop=="pyrethrum"){suitclass=ifelse(value>90,1,ifelse(value>40,2,ifelse(value>20,3,4)))}
  else if(crop=="tobacco"){suitclass=ifelse(value>100,1,ifelse(value>50,2,ifelse(value>30,3,4)))}

  #Soil depth (cm) suitability classes for tuber
  else if(crop=="sweetpotato"){suitclass=ifelse(value>100,1,ifelse(value>75,2,ifelse(value>20,3,4)))}
  else if(crop=="cassava"){suitclass=ifelse(value>125,1,ifelse(value>100,2,ifelse(value>50,3,4)))}
  else if(crop=="potato"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="carrot"){suitclass=ifelse(value>100,1,ifelse(value>75,2,ifelse(value>50,3,4)))}
  else if(crop=="turnip"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>30,3,4)))}
  else if(crop=="radish"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>30,3,4)))}

  #Soil depth (cm) suitability classes for fruits
  else if(crop=="mango"){suitclass=ifelse(value>200,1,ifelse(value>125,2,ifelse(value>75,3,4)))}
  else if(crop=="grape"){suitclass=ifelse(value>100,1,ifelse(value>75,2,ifelse(value>50,3,4)))}
  else if(crop=="citrus"){suitclass=ifelse(value>150,1,ifelse(value>100,2,ifelse(value>50,3,4)))}
  else if(crop=="pomegranate"){suitclass=ifelse(value>210,1,ifelse(value>75,2,ifelse(value>50,3,4)))}
  else if(crop=="banana"){suitclass=ifelse(value>125,1,ifelse(value>75,2,ifelse(value>50,3,4)))}
  else if(crop=="avocado"){stop("not included")}
  else if(crop=="watermelon"){suitclass=ifelse(value>150,1,ifelse(value>75,2,ifelse(value>50,3,4)))}
  else if(crop=="melon"){suitclass=ifelse(value>120,1,ifelse(value>50,2,ifelse(value>30,3,4)))}
  else if(crop=="pineaple"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>30,3,4)))}
  else if(crop=="pawpaw"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>30,3,4)))}


  #Soil depth (cm) suitability classes for vegetables
  else if(crop=="cabbage"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="tomato"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="vegetable"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="broccoli"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>30,3,4)))}
  else if(crop=="cauliflower"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>30,3,4)))}
  else if(crop=="okra"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>30,3,4)))}

  #Soil depth (cm) suitability classes for spices
  else if(crop=="onion"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="ginger"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="vanilla"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="lemongrass"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="chilli"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="pepper"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="tumeric"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="cardamom"){suitclass=ifelse(value>80,1,ifelse(value>60,2,ifelse(value>25,3,4)))}

  #Soil depth (cm) suitability classes for other
  else if(crop=="alfalfa"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="rose"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="jasmine"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}

  #Soil depth (cm) suitability classes for fleshy crops
  else if(crop=="yam"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="pumpkin"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="squash"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}
  else if(crop=="butternut"){suitclass=ifelse(value>75,1,ifelse(value>50,2,ifelse(value>25,3,4)))}

  return(suitclass)
}
