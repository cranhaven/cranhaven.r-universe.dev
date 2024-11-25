SOCSuit=function(value,crop="wheat"){
  #Soil organic carbon % suitability classes for cereals
  if(crop=="wheat"){suitclass=ifelse(value>0.6,1,ifelse(value>0.5,2,ifelse(value>0.3,3,4)))}
  else if(crop=="rice"){suitclass=ifelse(value>1.5,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="maize"){suitclass=ifelse(value>1.0,1,ifelse(value>0.8,2,ifelse(value>0.4,3,4)))}
  else if(crop=="sorghum"){suitclass=ifelse(value>0.75,1,ifelse(value>0.5,2,ifelse(value>0.2,3,4)))}
  else if(crop=="millet"){suitclass=ifelse(value>0.75,1,ifelse(value>0.5,2,ifelse(value>0.2,3,4)))}
  else if(crop=="barley"){suitclass=ifelse(value>2,1,ifelse(value>1.5,2,ifelse(value>0.5,3,4)))}
  else if(crop=="oat"){suitclass=ifelse(value>0.75,1,ifelse(value>0.5,2,ifelse(value>0.2,3,4)))}

  #Soil organic carbon % suitability classes for legumes
  else if(crop=="bean"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="groundnut"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="pea"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="soybean"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="lentil"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="gram"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}

  #Soil organic carbon % suitability classes for forest
  else if(crop=="poplar"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="grevillea"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="sesbania"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="calliandra"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="leucaena"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="acacia"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="eucalyptus"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="teak"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="maple"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="ash"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  
  #Soil organic carbon % suitability classes for oilcrops
  else if(crop=="sesame"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="sunflower"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="oilpalm"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="castor"){suitclass=ifelse(value>1.2,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="safflower"){suitclass=ifelse(value>2,1,ifelse(value>1.2,2,ifelse(value>0.8,3,4)))}
  else if(crop=="olive"){suitclass=ifelse(value>2,1,ifelse(value>1.2,2,ifelse(value>0.8,3,4)))}
  else if(crop=="mustard"){suitclass=ifelse(value>1.8,1,ifelse(value>0.98,2,ifelse(value>0.5,3,4)))}
  else if(crop=="rapeseed"){suitclass=ifelse(value>1.8,1,ifelse(value>0.98,2,ifelse(value>0.5,3,4)))}

  #Soil organic carbon % suitability classes for nuts
  else if(crop=="cashew"){suitclass=ifelse(value>1.5,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="coconut"){suitclass=ifelse(value>1.5,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="almond"){suitclass=ifelse(value>1.5,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="pistachio"){suitclass=ifelse(value>1.5,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}

  #Soil organic carbon % suitability classes for industrial crops
  else if(crop=="cotton"){suitclass=ifelse(value>1.0,1,ifelse(value>0.75,2,ifelse(value>0.5,3,4)))}
  else if(crop=="sugarcane"){suitclass=ifelse(value>1.0,1,ifelse(value>0.6,2,ifelse(value>0.4,3,4)))}
  else if(crop=="tea"){suitclass=ifelse(value>1.0,1,ifelse(value>0.6,2,ifelse(value>0.4,3,4)))}
  else if(crop=="coffee"){suitclass=ifelse(value>1.0,1,ifelse(value>0.6,2,ifelse(value>0.4,3,4)))}
  else if(crop=="rubber"){suitclass=ifelse(value>1.0,1,ifelse(value>0.6,2,ifelse(value>0.4,3,4)))}
  else if(crop=="jute"){suitclass=ifelse(value>1.0,1,ifelse(value>0.6,2,ifelse(value>0.4,3,4)))}
  else if(crop=="pyrethrum"){suitclass=ifelse(value>2.4,1,ifelse(value>1.6,2,ifelse(value>0.8,3,4)))}
  else if(crop=="saffron"){suitclass=ifelse(value>2.0,1,ifelse(value>1.6,2,ifelse(value>0.8,3,4)))}
  else if(crop=="tobacco"){suitclass=ifelse(value>2.0,1,ifelse(value>1.6,2,ifelse(value>0.8,3,4)))}

  #Soil organic carbon % suitability classes for tubers
  else if(crop=="sweetpotato"){suitclass=ifelse(value>3.0,1,ifelse(value>2,2,ifelse(value>1,3,4)))}
  else if(crop=="cassava"){suitclass=ifelse(value>1.5,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="potato"){suitclass=ifelse(value>1.0,1,ifelse(value>0.8,2,ifelse(value>0.4,3,4)))}
  else if(crop=="turnip"){suitclass=ifelse(value>1.0,1,ifelse(value>0.8,2,ifelse(value>0.4,3,4)))}
  else if(crop=="carrot"){suitclass=ifelse(value>1.0,1,ifelse(value>0.8,2,ifelse(value>0.4,3,4)))}
  else if(crop=="radish"){suitclass=ifelse(value>1.0,1,ifelse(value>0.8,2,ifelse(value>0.4,3,4)))}

  #Soil organic carbon % suitability classes for fruits
  else if(crop=="mango"){suitclass=ifelse(value>1.80,1,ifelse(value>1.2,2,ifelse(value>0.6,3,4)))}
  else if(crop=="grape"){suitclass=ifelse(value>1.0,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="citrus"){suitclass=ifelse(value>1.5,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="pomegranate"){suitclass=ifelse(value>1.5,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="banana"){suitclass=ifelse(value>1.5,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="watermelon"){suitclass=ifelse(value>1.5,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="melon"){suitclass=ifelse(value>1.5,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="avocado"){suitclass=ifelse(value>1.5,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="pineaple"){suitclass=ifelse(value>1.5,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}
  else if(crop=="pawpaw"){suitclass=ifelse(value>1.5,1,ifelse(value>0.8,2,ifelse(value>0.3,3,4)))}

  #Soil organic carbon % suitability classes for vegetables
  else if(crop=="cabbage"){suitclass=ifelse(value>1.50,1,ifelse(value>0.8,2,ifelse(value>0.6,3,4)))}
  else if(crop=="tomato"){suitclass=ifelse(value>2.0,1,ifelse(value>1.2,2,ifelse(value>0.8,3,4)))}
  else if(crop=="vegetable"){suitclass=ifelse(value>1.50,1,ifelse(value>0.8,2,ifelse(value>0.6,3,4)))}
  else if(crop=="cauliflower"){suitclass=ifelse(value>1.50,1,ifelse(value>0.8,2,ifelse(value>0.6,3,4)))}
  else if(crop=="broccoli"){suitclass=ifelse(value>1.50,1,ifelse(value>0.8,2,ifelse(value>0.6,3,4)))}
  else if(crop=="okra"){suitclass=ifelse(value>1.50,1,ifelse(value>0.8,2,ifelse(value>0.6,3,4)))}

  #Soil organic carbon % suitability classes for spices
  else if(crop=="chilli"){suitclass=ifelse(value>1.50,1,ifelse(value>0.8,2,ifelse(value>0.6,3,4)))}
  else if(crop=="pepper"){suitclass=ifelse(value>1.50,1,ifelse(value>0.8,2,ifelse(value>0.6,3,4)))}
  else if(crop=="vanilla"){suitclass=ifelse(value>2.0,1,ifelse(value>0.8,2,ifelse(value>0.6,3,4)))}
  else if(crop=="lemongrass"){suitclass=ifelse(value>1.50,1,ifelse(value>0.8,2,ifelse(value>0.6,3,4)))}
  else if(crop=="onion"){suitclass=ifelse(value>1.50,1,ifelse(value>0.8,2,ifelse(value>0.6,3,4)))}
  else if(crop=="cardamom"){suitclass=ifelse(value>2.0,1,ifelse(value>0.8,2,ifelse(value>0.6,3,4)))}
  else if(crop=="ginger"){suitclass=ifelse(value>1.80,1,ifelse(value>0.8,2,ifelse(value>0.6,3,4)))}
  else if(crop=="tumeric"){suitclass=ifelse(value>1.80,1,ifelse(value>0.8,2,ifelse(value>0.6,3,4)))}

  #Soil organic carbon % suitability classes for others
  else if(crop=="alfalfa"){suitclass=ifelse(value>1.80,1,ifelse(value>0.98,2,ifelse(value>0.6,3,4)))}
  else if(crop=="rose"){suitclass=ifelse(value>1.50,1,ifelse(value>0.8,2,ifelse(value>0.6,3,4)))}
  else if(crop=="jasmine"){suitclass=ifelse(value>1.50,1,ifelse(value>0.8,2,ifelse(value>0.6,3,4)))}

  #Soil organic carbon % suitability classes for fleshy crops
  else if(crop=="yam"){suitclass=ifelse(value>1.50,1,ifelse(value>0.8,2,ifelse(value>0.6,3,4)))}
  else if(crop=="butternut"){suitclass=ifelse(value>1.50,1,ifelse(value>0.8,2,ifelse(value>0.6,3,4)))}
  else if(crop=="squash"){suitclass=ifelse(value>1.50,1,ifelse(value>0.8,2,ifelse(value>0.6,3,4)))}
  else if(crop=="pumpkin"){suitclass=ifelse(value>1.50,1,ifelse(value>0.8,2,ifelse(value>0.6,3,4)))}

  return(suitclass)
}
