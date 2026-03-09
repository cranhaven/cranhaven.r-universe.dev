rainSuit=function(value,crop="wheat"){
  #rainfed crop suitability requirements for cereals
  if(crop=="wheat"){suitclass=ifelse(value>1200,4,ifelse(value>1000,3,ifelse(value>950,2,ifelse(value>500,1,ifelse(value>350,2,ifelse(value>250,3,4))))))}#rainfed wheat
  else if(crop=="rice"){suitclass=ifelse(value>1250,1,ifelse(value>900,2,ifelse(value>750,3,4)))}
  else if(crop=="sorghum"){suitclass=ifelse(value>650,1,ifelse(value>550,2,ifelse(value>450,3,4)))}
  else if(crop=="maize"){suitclass=ifelse(value>1800,4,ifelse(value>1600,3,ifelse(value>1250,2,ifelse(value>850,1,ifelse(value>650,2,ifelse(value>500,3,4))))))}
  else if(crop=="barley"){suitclass=ifelse(value>1250,4,ifelse(value>1000,3,ifelse(value>750,2,ifelse(value>400,1,ifelse(value>300,2,ifelse(value>200,3,4))))))}
  else if(crop=="oat"){suitclass=ifelse(value>1250,4,ifelse(value>1000,3,ifelse(value>750,2,ifelse(value>450,1,ifelse(value>350,2,ifelse(value>280,3,4))))))}
  else if(crop=="millet"){suitclass=ifelse(value>750,1,ifelse(value>600,2,ifelse(value>450,3,4)))}

  #rainfed crop suitability requirements for legumes
  else if(crop=="groundnut"){suitclass=ifelse(value>1000,4,ifelse(value>700,1,ifelse(value>500,2,ifelse(value>350,3,4))))}
  else if(crop=="gram"){suitclass=ifelse(value>1250,3,ifelse(value>800,1,ifelse(value>600,2,ifelse(value>400,3,4))))}
  else if(crop=="pea"){suitclass=ifelse(value>1250,3,ifelse(value>800,1,ifelse(value>600,2,ifelse(value>400,3,4))))}
  else if(crop=="bean"){suitclass=ifelse(value>1000,4,ifelse(value>900,3,ifelse(value>750,1,ifelse(value>600,2,ifelse(value>500,3,4)))))}
  else if(crop=="soybean"){suitclass=ifelse(value>1900,4,ifelse(value>1600,3,ifelse(value>1100,2,ifelse(value>600,1,ifelse(value>350,2,ifelse(value>250,3,4))))))}
  else if(crop=="lentil"){suitclass=ifelse(value>1000,4,ifelse(value>800,3,ifelse(value>700,2,ifelse(value>450,1,ifelse(value>350,2,ifelse(value>300,3,4))))))}

  #rainfed crop suitability requirements for forests
  else if(crop=="poplar"){suitclass=ifelse(value>1500,1,ifelse(value>1000,2,ifelse(value>700,3,4)))}
  else if(crop=="grevillea"){suitclass=ifelse(value>1250,1,ifelse(value>800,2,ifelse(value>600,3,4)))}
  else if(crop=="sesbania"){suitclass=ifelse(value>1250,1,ifelse(value>800,2,ifelse(value>600,3,ifelse(value>400,3,4))))}
  else if(crop=="calliandra"){suitclass=ifelse(value>1300,1,ifelse(value>1000,2,ifelse(value>750,3,4)))}
  else if(crop=="leucaena"){suitclass=ifelse(value>1000,1,ifelse(value>900,2,ifelse(value>600,3,4)))}
  else if(crop=="acacia"){suitclass=ifelse(value>1200,1,ifelse(value>600,2,ifelse(value>400,3,4)))}
  else if(crop=="eucalyptus"){suitclass=ifelse(value>1500,1,ifelse(value>1200,2,ifelse(value>900,3,4)))}
  else if(crop=="teak"){suitclass=ifelse(value>1800,1,ifelse(value>1200,2,ifelse(value>1000,3,4)))}
  else if(crop=="maple"){suitclass=ifelse(value>1400,1,ifelse(value>1100,2,ifelse(value>900,3,4)))}
  else if(crop=="ash"){suitclass=ifelse(value>1400,1,ifelse(value>1100,2,ifelse(value>900,3,4)))}
  
  #rainfed crop suitability requirements for oil crops
  if(crop=="sesame"){suitclass=ifelse(value>600,3,ifelse(value>500,1,ifelse(value>400,2,ifelse(value>300,3,4))))}
  else if(crop=="sunflower"){suitclass=ifelse(value>700,3,ifelse(value>600,1,ifelse(value>500,2,ifelse(value>400,3,4))))}
  else if(crop=="oilpalm"){suitclass=ifelse(value>2000,1,ifelse(value>1500,2,ifelse(value>1000,3,4)))}
  else if(crop=="castor"){suitclass=ifelse(value>800,3,ifelse(value>600,1,ifelse(value>400,2,ifelse(value>250,3,4))))}
  else if(crop=="safflower"){suitclass=ifelse(value>1000,3,ifelse(value>600,1,ifelse(value>400,2,ifelse(value>300,3,4))))}
  else if(crop=="rapeseed"){suitclass=ifelse(value>1000,3,ifelse(value>650,1,ifelse(value>500,2,ifelse(value>400,3,4))))}
  else if(crop=="mustard"){suitclass=ifelse(value>1000,3,ifelse(value>650,1,ifelse(value>500,2,ifelse(value>350,3,4))))}
  else if(crop=="olive"){suitclass=ifelse(value>1000,3,ifelse(value>600,1,ifelse(value>450,2,ifelse(value>350,3,4))))}

  #rainfed crop suitability requirements for nuts
  else if(crop=="coconut"){suitclass=ifelse(value>800,3,ifelse(value>600,1,ifelse(value>400,2,ifelse(value>250,3,4))))}
  else if(crop=="cashew"){suitclass=ifelse(value>2000,1,ifelse(value>1000,2,ifelse(value>500,3,4)))}
  else if(crop=="almond"){suitclass=ifelse(value>900,1,ifelse(value>600,2,ifelse(value>250,3,4)))}
  else if(crop=="pistachio"){suitclass=ifelse(value>700,1,ifelse(value>400,2,ifelse(value>250,3,4)))}

  #rainfed crop suitability requirements for fibre/industrial
  else if(crop=="cotton"){suitclass=ifelse(value>1200,3,ifelse(value>1000,2,ifelse(value>700,1,ifelse(value>500,2,4))))}
  else if(crop=="sugarcane"){suitclass=ifelse(value>1600,1,ifelse(value>1200,2,ifelse(value>900,3,4)))}
  else if(crop=="saffron"){suitclass=ifelse(value>300,1,ifelse(value>250,2,ifelse(value>200,3,4)))}
  else if(crop=="tea"){suitclass=ifelse(value>1800,1,ifelse(value>1600,2,ifelse(value>1000,3,4)))}
  else if(crop=="coffee"){suitclass=ifelse(value>1150,1,ifelse(value>1000,2,ifelse(value>800,3,4)))}
  else if(crop=="pyrethrum"){suitclass=ifelse(value>1600,4,ifelse(value>1400,3,ifelse(value>1200,2,ifelse(value>1100,1,ifelse(value>1000,2,ifelse(value>950,3,4))))))}
  else if(crop=="rubber"){suitclass=ifelse(value>6000,4,ifelse(value>3000,3,ifelse(value>1750,1,ifelse(value>1500,2,ifelse(value>1250,3,4)))))}
  else if(crop=="jute"){suitclass=ifelse(value>1500,1,ifelse(value>1000,2,ifelse(value>850,3,4)))}
  else if(crop=="tobacco"){suitclass=ifelse(value>1400,3,ifelse(value>1200,2,ifelse(value>850,1,ifelse(value>600,2,ifelse(value>400,3,4)))))}

  #rainfed crop suitability requirements for tuber
  else if(crop=="sweetpotato"){suitclass=ifelse(value>1300,1,ifelse(value>800,1,ifelse(value>500,3,4)))}
  else if(crop=="cassava"){suitclass=ifelse(value>900,1,ifelse(value>700,2,ifelse(value>500,3,4)))}
  else if(crop=="potato"){suitclass=ifelse(value>1300,1,ifelse(value>800,2,ifelse(value>500,3,4)))}
  else if(crop=="carrot"){suitclass=ifelse(value>1200,4,ifelse(value>600,3,ifelse(value>400,3,ifelse(value>350,2,ifelse(value>250,3,4)))))}
  else if(crop=="turnip"){suitclass=ifelse(value>1000,1,ifelse(value>800,2,ifelse(value>650,3,4)))}
  else if(crop=="radish"){suitclass=ifelse(value>1200,1,ifelse(value>900,2,ifelse(value>800,3,4)))}

  #rainfed crop suitability requirements for fruits
  else if(crop=="mango"){suitclass=ifelse(value>1000,1,ifelse(value>500,2,ifelse(value>250,3,4)))}
  else if(crop=="grape"){suitclass=ifelse(value>900,3,ifelse(value>600,1,ifelse(value>400,2,ifelse(value>250,3,4))))}
  else if(crop=="citrus"){suitclass=ifelse(value>1200,1,ifelse(value>1000,2,ifelse(value>800,3,4)))}
  else if(crop=="pomegranate"){suitclass=ifelse(value>1000,1,ifelse(value>500,2,ifelse(value>250,3,4)))}
  else if(crop=="watermelon"){suitclass=ifelse(value>1000,1,ifelse(value>500,2,ifelse(value>250,3,4)))}
  else if(crop=="avocado"){suitclass=ifelse(value>2000,2,ifelse(value>1500,1,ifelse(value>1200,2,ifelse(value>770,3,4))))}
  else if(crop=="banana"){suitclass=ifelse(value>1500,1,ifelse(value>1250,2,ifelse(value>1000,3,4)))}
  else if(crop=="pineaple"){suitclass=ifelse(value>2000,3,ifelse(value>1600,2,ifelse(value>1000,1,ifelse(value>800,2,ifelse(value>600,3,4)))))}
  else if(crop=="pawpaw"){suitclass=ifelse(value>2000,2,ifelse(value>1000,1,ifelse(value>800,2,ifelse(value>600,3,4))))}
  else if(crop=="melon"){suitclass=ifelse(value>1000,3,ifelse(value>700,2,ifelse(value>400,1,ifelse(value>300,2,ifelse(value>200,3,4)))))}

  #rainfed crop suitability requirements for vegetables
  else if(crop=="tomato"){suitclass=ifelse(value>1200,4,ifelse(value>1000,3,ifelse(value>750,2,ifelse(value>600,1,ifelse(value>500,2,ifelse(value>400,3,4))))))}
  else if(crop=="vegetable"){suitclass=ifelse(value>1200,1,ifelse(value>1000,2,ifelse(value>850,3,4)))}
  else if(crop=="cabbage"){suitclass=ifelse(value>1000,3,ifelse(value>700,2,ifelse(value>550,1,ifelse(value>400,2,ifelse(value>200,3,4)))))}
  else if(crop=="okra"){suitclass=ifelse(value>1200,1,ifelse(value>800,2,ifelse(value>550,3,4)))}
  else if(crop=="cauliflower"){suitclass=ifelse(value>1200,1,ifelse(value>900,2,ifelse(value>550,3,4)))}
  else if(crop=="broccoli"){suitclass=ifelse(value>1200,1,ifelse(value>1000,2,ifelse(value>850,3,4)))}

  #rainfed crop suitability requirements for spices
  else if(crop=="chilli"){suitclass=ifelse(value>1200,3,ifelse(value>900,2,ifelse(value>750,1,ifelse(value>600,2,ifelse(value>500,3,4)))))}
  else if(crop=="pepper"){suitclass=ifelse(value>2000,1,ifelse(value>1500,2,ifelse(value>850,3,4)))}
  else if(crop=="onion"){suitclass=ifelse(value>1600,4,ifelse(value>800,3,ifelse(value>600,2,ifelse(value>400,1,ifelse(value>350,2,ifelse(value>250,3,4))))))}
  else if(crop=="ginger"){suitclass=ifelse(value>3000,1,ifelse(value>2500,2,ifelse(value>2000,3,4)))}
  else if(crop=="tumeric"){suitclass=ifelse(value>1500,1,ifelse(value>1000,2,ifelse(value>800,3,4)))}
  else if(crop=="cardamom"){suitclass=ifelse(value>3000,1,ifelse(value>2500,2,ifelse(value>2000,3,4)))}
  else if(crop=="lemongrass"){suitclass=ifelse(value>1750,1,ifelse(value>1000,2,ifelse(value>750,3,4)))}
  else if(crop=="vanilla"){suitclass=ifelse(value>1200,1,ifelse(value>800,2,ifelse(value>600,3,4)))}

  #rainfed crop suitability requirements for other crops
  else if(crop=="alfalfa"){suitclass=ifelse(value>1600,4,ifelse(value>1400,3,ifelse(value>1200,2,ifelse(value>800,1,ifelse(value>600,2,ifelse(value>400,3,4))))))}
  else if(crop=="rose"){suitclass=ifelse(value>1200,1,ifelse(value>1000,2,ifelse(value>850,3,4)))}
  else if(crop=="jasmine"){suitclass=ifelse(value>1200,1,ifelse(value>1000,2,ifelse(value>850,3,4)))}

  #rainfed crop suitability requirements for fleshy crops
  else if(crop=="yam"){suitclass=ifelse(value>1200,1,ifelse(value>1000,2,ifelse(value>850,3,4)))}
  else if(crop=="butternut"){suitclass=ifelse(value>1200,1,ifelse(value>1000,2,ifelse(value>850,3,4)))}
  else if(crop=="squash"){suitclass=ifelse(value>1000,1,ifelse(value>800,2,ifelse(value>450,3,4)))}
  else if(crop=="pumpkin"){suitclass=ifelse(value>1000,1,ifelse(value>800,2,ifelse(value>450,3,4)))}

  return(suitclass)
}
