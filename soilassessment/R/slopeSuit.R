slopeSuit=function (value,crop="wheat"){
  if(!missing(value)){warning("slope must be in degrees")}
  value=tan(value*pi/180)*100 #if slope is in radians else the slope must first be converted into radians if in degrees
  #slope suitability rating for cereal
  if(crop=="wheat"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="maize"){suitclass=ifelse(value>8,4,ifelse(value>5,3,ifelse(value>3.5,2,1)))}
  else if(crop=="rice"){suitclass=ifelse(value>5,4,ifelse(value>3,3,ifelse(value>1,2,1)))}
  else if(crop=="sorghum"){suitclass=ifelse(value>15,4,ifelse(value>8,3,ifelse(value>3,2,1)))}
  else if(crop=="millet"){suitclass=ifelse(value>25,4,ifelse(value>15,3,ifelse(value>10,2,1)))}
  else if(crop=="barley"){suitclass=ifelse(value>18,4,ifelse(value>10,3,ifelse(value>6,2,1)))}
  else if(crop=="oat"){suitclass=ifelse(value>18,4,ifelse(value>8,3,ifelse(value>3,2,1)))}

  #slope suitability for legumes
  else if(crop=="groundnut"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="pea"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="gram"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="bean"){suitclass=ifelse(value>15,4,ifelse(value>10,3,ifelse(value>5,2,1)))}
  else if(crop=="soybean"){suitclass=ifelse(value>35,4,ifelse(value>25,3,ifelse(value>15,2,1)))}
  else if(crop=="lentil"){suitclass=ifelse(value>16,4,ifelse(value>8,3,ifelse(value>4,2,1)))}

  #slope suitability for forests
  else if(crop=="poplar"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="maple"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="ash"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="grevillea"){suitclass=ifelse(value>15,4,ifelse(value>10,3,ifelse(value>5,2,1)))}
  else if(crop=="sesbania"){suitclass=ifelse(value>35,4,ifelse(value>25,3,ifelse(value>15,2,1)))}
  else if(crop=="calliandra"){suitclass=ifelse(value>16,4,ifelse(value>8,3,ifelse(value>4,2,1)))}
  else if(crop=="leucaena"){suitclass=ifelse(value>16,4,ifelse(value>8,3,ifelse(value>4,2,1)))}
  else if(crop=="acacia"){suitclass=ifelse(value>16,4,ifelse(value>8,3,ifelse(value>4,2,1)))}
  else if(crop=="eucalyptus"){suitclass=ifelse(value>16,4,ifelse(value>8,3,ifelse(value>4,2,1)))}
  else if(crop=="teak"){suitclass=ifelse(value>16,4,ifelse(value>8,3,ifelse(value>4,2,1)))}
  
  #slope suitability rating for oilcrops
  else if(crop=="sesame"){suitclass=ifelse(value>8,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="sunflower"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="oilplam"){suitclass=ifelse(value>18,4,ifelse(value>10,3,ifelse(value>5,2,1)))}
  else if(crop=="castor"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="rapeseed"){suitclass=ifelse(value>8,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="safflower"){suitclass=ifelse(value>16,4,ifelse(value>8,3,ifelse(value>4,2,1)))}
  else if(crop=="olive"){suitclass=ifelse(value>15,4,ifelse(value>10,3,ifelse(value>2,2,1)))}
  else if(crop=="mustard"){suitclass=ifelse(value>15,4,ifelse(value>10,3,ifelse(value>2,2,1)))}

  #slope suitability rating for nuts
  else if(crop=="cashew"){suitclass=ifelse(value>30,4,ifelse(value>15,3,ifelse(value>5,2,1)))}
  else if(crop=="coconut"){suitclass=ifelse(value>30,4,ifelse(value>15,3,ifelse(value>8,2,1)))}
  else if(crop=="almond"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="pistachio"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}

  #slope suitability requirement for fibre crops
  else if(crop=="cotton"){suitclass=ifelse(value>5,4,ifelse(value>2,3,ifelse(value>1,2,1)))}
  else if(crop=="sugarcane"){suitclass=ifelse(value>8,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="tea"){suitclass=ifelse(value>50,4,ifelse(value>35,3,ifelse(value>15,2,1)))}
  else if(crop=="rubber"){suitclass=ifelse(value>30,4,ifelse(value>15,3,ifelse(value>10,2,1)))}
  else if(crop=="coffee"){suitclass=ifelse(value>50,4,ifelse(value>35,3,ifelse(value>15,2,1)))}
  else if(crop=="jute"){suitclass=ifelse(value>8,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="saffron"){suitclass=ifelse(value>26,4,ifelse(value>15,3,ifelse(value>5,2,1)))}
  else if(crop=="tobacco"){suitclass=ifelse(value>8,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="pyrethrum"){suitclass=ifelse(value>30,4,ifelse(value>25,3,ifelse(value>8,2,1)))}

  #slope suitability requirement for tuber crops
  else if(crop=="potato"){suitclass=ifelse(value>15,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="sweetpotato"){suitclass=ifelse(value>30,4,ifelse(value>8,3,ifelse(value>1,2,1)))}
  else if(crop=="cassava"){suitclass=ifelse(value>16,4,ifelse(value>8,3,ifelse(value>2,2,1)))}
  else if(crop=="carrot"){suitclass=ifelse(value>16,4,ifelse(value>8,3,ifelse(value>4,2,1)))}
  else if(crop=="turnip"){suitclass=ifelse(value>16,4,ifelse(value>8,3,ifelse(value>2,2,1)))}
  else if(crop=="radish"){suitclass=ifelse(value>16,4,ifelse(value>8,3,ifelse(value>2,2,1)))}

  #slope suitability rating for fruit crops
  else if(crop=="mango"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="grape"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="citrus"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="pomegranate"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="watermelon"){suitclass=ifelse(value>16,4,ifelse(value>8,3,ifelse(value>4,2,1)))}
  else if(crop=="melon"){suitclass=ifelse(value>30,4,ifelse(value>16,3,ifelse(value>8,2,1)))}
  else if(crop=="pineaple"){suitclass=ifelse(value>16,4,ifelse(value>8,3,ifelse(value>4,2,1)))}
  else if(crop=="pawpaw"){suitclass=ifelse(value>16,4,ifelse(value>8,3,ifelse(value>4,2,1)))}
  else if(crop=="avocado"){suitclass=ifelse(value>16,4,ifelse(value>8,3,ifelse(value>4,2,1)))}
  else if(crop=="banana"){suitclass=ifelse(value>15,4,ifelse(value>10,3,ifelse(value>5,2,1)))}

  #slope suitability rating for vegetables
  else if(crop=="tomato"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="cabbage"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="vegetable"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="broccoli"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="cauliflower"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="okra"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}

  #slope suitability rating for spices
  else if(crop=="ginger"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="onion"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="tumeric"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="lemongrass"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="vanilla"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="chilli"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="pepper"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="cardamom"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}


  #slope suitability rating for others
  else if(crop=="alfalfa"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="rose"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="jasmine"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}

  #slope suitability rating for fleshy
  else if(crop=="yam"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="pumpkin"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="squash"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}
  else if(crop=="butternut"){suitclass=ifelse(value>10,4,ifelse(value>5,3,ifelse(value>3,2,1)))}

  return(suitclass)
}
