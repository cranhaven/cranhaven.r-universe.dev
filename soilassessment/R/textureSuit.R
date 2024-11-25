textureSuit=function(value,crop="wheat"){
  #Soil texture suitability raing for cereals
  if(crop=="wheat"){suitclass=ifelse(value==11,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==9,1,ifelse(value==8,2,ifelse(value==2,2,ifelse(value==1,2,ifelse(value==10,2,ifelse(value==3,2,ifelse(value==5,2,ifelse(value==16,3,4)))))))))))}
  else if(crop=="maize"){suitclass=ifelse(value==11,1,ifelse(value==7,1,ifelse(value==9,1,ifelse(value==4,1,ifelse(value==5,2,ifelse(value==3,2,ifelse(value==2,2,ifelse(value==13,3,ifelse(value==10,3,4)))))))))}
  else if(crop=="rice"){suitclass=ifelse(value==1,1,ifelse(value==2,1,ifelse(value==7,1,ifelse(value==3,1,ifelse(value==8,1,ifelse(value==9,2,ifelse(value==4,2,ifelse(value==11,2,ifelse(value==5,3,ifelse(value==10,3,4))))))))))}
  else if(crop=="sorghum"){suitclass=ifelse(value==1,1,ifelse(value==7,1,ifelse(value==3,1,ifelse(value==8,1,ifelse(value==11,2,ifelse(value==4,2,ifelse(value==2,2,ifelse(value==5,3,ifelse(value==10,3,4)))))))))}
  else if(crop=="millet"){suitclass=ifelse(value==11,1,ifelse(value==4,1,ifelse(value==5,1,ifelse(value==7,1,ifelse(value==3,1,ifelse(value==9,1,ifelse(value==2,2,ifelse(value==1,2,ifelse(value==8,2,ifelse(value==10,3,ifelse(value==16,3,4)))))))))))}
  else if(crop=="barley"){suitclass=ifelse(value==1,1,ifelse(value==7,1,ifelse(value==3,1,ifelse(value==6,1,ifelse(value==8,2,ifelse(value==9,2,ifelse(value==16,2,ifelse(value==5,3,ifelse(value==10,3,4)))))))))}
  else if(crop=="oat"){suitclass=ifelse(value==1,1,ifelse(value==7,1,ifelse(value==3,1,ifelse(value==6,1,ifelse(value==8,2,ifelse(value==9,2,ifelse(value==16,2,ifelse(value==5,3,ifelse(value==10,3,4)))))))))}

  #soil texture suitability ratings for legumes
  else if (crop=="groundnut"){suitclass=ifelse(value==11,1,ifelse(value==5,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==8,1,ifelse(value==10,2,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==16,3,4))))))))))}
  else if (crop=="gram"){suitclass=ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==9,1,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==5,3,ifelse(value==16,3,4)))))))))}
  else if(crop=="pea"){suitclass=ifelse(value==5,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==9,1,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==10,3,4))))))))}
  else if(crop=="bean"){suitclass=ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==9,1,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==5,2,ifelse(value==16,3,ifelse(value==10,3,4))))))))))}
  else if(crop=="soybean"){suitclass=ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==9,1,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==5,2,ifelse(value==16,3,ifelse(value==10,3,4))))))))))}
  else if(crop=="lentil"){suitclass=ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==9,1,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==5,2,ifelse(value==16,3,ifelse(value==10,3,4))))))))))}

  #soil texture suitability ratings for forests
  else if (crop=="poplar"){suitclass=ifelse(value==11,1,ifelse(value==5,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==8,1,ifelse(value==10,2,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==16,3,4))))))))))}
  else if (crop=="grevillea"){suitclass=ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==9,1,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==5,3,ifelse(value==16,3,4)))))))))}
  else if(crop=="sesbania"){suitclass=ifelse(value==5,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==9,1,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==10,3,4))))))))}
  else if(crop=="calliandra"){suitclass=ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==9,1,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==5,2,ifelse(value==16,3,ifelse(value==10,3,4))))))))))}
  else if(crop=="leucaena"){suitclass=ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==9,1,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==5,2,ifelse(value==16,3,ifelse(value==10,3,4))))))))))}
  else if(crop=="acacia"){suitclass=ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==9,1,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==5,2,ifelse(value==16,3,ifelse(value==10,3,4))))))))))}
  else if(crop=="eucalyptus"){suitclass=ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==9,1,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==5,2,ifelse(value==16,3,ifelse(value==10,3,4))))))))))}
  else if(crop=="teak"){suitclass=ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==9,1,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==5,2,ifelse(value==16,3,ifelse(value==10,3,4))))))))))}
  else if(crop=="maple"){suitclass=ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==9,1,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==5,2,ifelse(value==16,3,ifelse(value==10,3,4))))))))))}
  else if(crop=="ash"){suitclass=ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==9,1,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==5,2,ifelse(value==16,3,ifelse(value==10,3,4))))))))))}
  
  #SOil texture suitability rating for oilcrops
  else if(crop=="sesame"){suitclass=ifelse(value==3,1,ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==5,1,ifelse(value==2,2,ifelse(value==8,2,ifelse(value==1,2,ifelse(value==10,3,ifelse(value==16,3,4))))))))))}
  else if(crop=="sunflower"){suitclass=ifelse(value==11,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==8,1,ifelse(value==9,2,ifelse(value==2,2,ifelse(value==1,2,ifelse(value==16,3,ifelse(value==5,3,4)))))))))}
  else if(crop=="oilpalm"){suitclass=ifelse(value==11,1,ifelse(value==9,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==5,2,ifelse(value==1,3,ifelse(value==10,3,ifelse(value==12,3,4))))))))))}
  else if(crop=="castor"){suitclass=ifelse(value==11,1,ifelse(value==9,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==5,1,ifelse(value==3,2,ifelse(value==2,2,ifelse(value==8,2,ifelse(value==10,3,ifelse(value==1,3,4))))))))))}
  else if(crop=="safflower"){suitclass=ifelse(value==11,1,ifelse(value==9,1,ifelse(value==4,2,ifelse(value==7,1,ifelse(value==5,2,ifelse(value==3,1,ifelse(value==2,2,ifelse(value==8,2,ifelse(value==10,3,ifelse(value==1,3,4))))))))))}
  else if(crop=="mustard"){suitclass=ifelse(value==11,1,ifelse(value==9,1,ifelse(value==4,1,ifelse(value==10,1,ifelse(value==5,1,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==8,2,ifelse(value==7,2,ifelse(value==12,3,ifelse(value==16,3,4)))))))))))}
  else if(crop=="rapeseed"){suitclass=ifelse(value==11,1,ifelse(value==9,1,ifelse(value==4,1,ifelse(value==10,1,ifelse(value==5,1,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==8,2,ifelse(value==7,2,ifelse(value==12,3,ifelse(value==16,3,4)))))))))))}
  else if(crop=="olive"){suitclass=ifelse(value==11,1,ifelse(value==9,1,ifelse(value==4,1,ifelse(value==10,1,ifelse(value==5,1,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==8,2,ifelse(value==7,2,ifelse(value==12,3,ifelse(value==16,3,4)))))))))))}

  #soil texture suitability rating for nut crops
  else if(crop=="cashew"){suitclass=ifelse(value==11,1,ifelse(value==5,1,ifelse(value==9,1,ifelse(value==7,2,ifelse(value==4,2,ifelse(value==10,2,ifelse(value==12,2,ifelse(value==2,3,ifelse(value==1,3,ifelse(value==16,3,4))))))))))}
  else if(crop=="almond"){suitclass=ifelse(value==11,1,ifelse(value==5,1,ifelse(value==9,1,ifelse(value==7,2,ifelse(value==4,2,ifelse(value==10,2,ifelse(value==12,2,ifelse(value==2,3,ifelse(value==1,3,ifelse(value==16,3,4))))))))))}
  else if(crop=="pistachio"){suitclass=ifelse(value==11,1,ifelse(value==5,1,ifelse(value==9,1,ifelse(value==7,2,ifelse(value==4,2,ifelse(value==10,2,ifelse(value==12,2,ifelse(value==2,3,ifelse(value==1,3,ifelse(value==16,3,4))))))))))}
  else if(crop=="coconut"){suitclass=ifelse(value==7,1,ifelse(value==9,1,ifelse(value==8,1,ifelse(value==3,1,ifelse(value==4,1,ifelse(value==5,2,ifelse(value==1,2,ifelse(value==2,2,ifelse(value==16,3,ifelse(value==10,3,ifelse(value==12,3,4)))))))))))}

  #soil texture suitability rating for industrial crops
  else if(crop=="cotton"){suitclass=ifelse(value==2,1,ifelse(value==1,1,ifelse(value==3,2,ifelse(value==7,2,ifelse(value==6,3,ifelse(value==4,3,ifelse(value==8,3,ifelse(value==9,3,ifelse(value==11,3,4)))))))))}
  else if(crop=="sugarcane"){suitclass=ifelse(value==11,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==3,1,ifelse(value==8,1,ifelse(value==9,1,ifelse(value==1,2,ifelse(value==5,2,ifelse(value==16,3,4)))))))))}
  else if(crop=="tea"){suitclass=ifelse(value==11,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==5,1,ifelse(value==3,2,ifelse(value==9,1,ifelse(value==1,2,ifelse(value==2,2,ifelse(value==16,3,ifelse(value==10,3,ifelse(value==12,3,4)))))))))))}
  else if(crop=="coffee"){suitclass=ifelse(value==11,1,ifelse(value==7,1,ifelse(value==8,1,ifelse(value==1,1,ifelse(value==3,1,ifelse(value==9,1,ifelse(value==5,3,ifelse(value==2,2,ifelse(value==16,3,4)))))))))}
  else if(crop=="rubber"){suitclass=ifelse(value==11,1,ifelse(value==9,1,ifelse(value==4,2,ifelse(value==3,2,ifelse(value==8,3,ifelse(value==1,3,4))))))}
  else if(crop=="jute"){suitclass=ifelse(value==11,1,ifelse(value==7,1,ifelse(value==5,1,ifelse(value==8,2,ifelse(value==3,2,ifelse(value==9,1,ifelse(value==1,3,ifelse(value==2,2,ifelse(value==10,3,4)))))))))}
  else if(crop=="saffron"){suitclass=ifelse(value==4,1,ifelse(value==5,1,ifelse(value==9,2,ifelse(value==8,2,ifelse(value==1,2,ifelse(value==16,3,4))))))}
  else if(crop=="tobacco"){suitclass=ifelse(value==11,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==5,1,ifelse(value==3,1,ifelse(value==9,1,ifelse(value==1,3,ifelse(value==2,2,ifelse(value==16,2,ifelse(value==10,1,ifelse(value==12,1,4)))))))))))}
  else if(crop=="pyrethrum"){suitclass=ifelse(value==1,1,ifelse(value==11,1,ifelse(value==9,2,ifelse(value==16,2,ifelse(value==5,3,ifelse(value==10,3,4))))))}

  #soil texture suitability rating for tuber crops
  else if(crop=="sweetpotato"){suitcalss=ifelse(value==2,1,ifelse(value==3,1,ifelse(value==6,1,ifelse(value==11,1,ifelse(value==9,1,ifelse(value==8,1,ifelse(value==1,2,ifelse(value==5,2,ifelse(value==10,3,ifelse(value==12,3,4))))))))))}
  else if(crop=="cassava"){suitclass=ifelse(value==5,1,ifelse(value==8,1,ifelse(value==1,1,ifelse(value==2,1,ifelse(value==3,1,ifelse(value==10,2,ifelse(value==15,2,ifelse(value==12,3,ifelse(value==13,3,4)))))))))}
  else if (crop=="potato"){suitclass=ifelse(value==5,1,ifelse(value==11,1,ifelse(value==10,1,ifelse(value==12,2,ifelse(value==3,2,ifelse(value==4,2,ifelse(value==7,3,4)))))))}
  else if(crop=="carrot"){suitcalss=ifelse(value==5,1,ifelse(value==11,1,ifelse(value==4,2,ifelse(value==7,3,ifelse(value==9,3,ifelse(value==10,3,4))))))}
  else if(crop=="turnip"){suitcalss=ifelse(value==5,1,ifelse(value==11,1,ifelse(value==4,2,ifelse(value==7,3,ifelse(value==9,3,ifelse(value==10,3,4))))))}
  else if(crop=="radish"){suitcalss=ifelse(value==5,1,ifelse(value==11,1,ifelse(value==4,2,ifelse(value==7,3,ifelse(value==9,3,ifelse(value==10,3,4))))))}

  #soil texture suitability rating for fruits
  else if(crop=="mango"){suitclass=ifelse(value==8,1,ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==5,2,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,3,4))))))))}
  else if(crop=="grape"){suitclass=ifelse(value==9,1,ifelse(value==11,1,ifelse(value==7,1,ifelse(value==5,2,ifelse(value==8,2,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,3,4))))))))}
  else if(crop=="citrus"){suitclass=ifelse(value==9,1,ifelse(value==11,1,ifelse(value==3,1,ifelse(value==7,1,ifelse(value==12,1,ifelse(value==8,2,ifelse(1,2,ifelse(value==16,3,4))))))))}
  else if(crop=="pomegranate"){suitclass=ifelse(value==8,1,ifelse(value==9,1,ifelse(value==11,1,ifelse(value==7,1,ifelse(value==1,2,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==12,3,ifelse(value==10,3,4)))))))))}
  else if(crop=="banana"){suitclass=ifelse(value==8,1,ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==5,3,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,3,4))))))))}
  else if(crop=="watermelon"){suitclass=ifelse(value==8,1,ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,2,ifelse(value==10,2,ifelse(value==2,2,ifelse(value==3,1,ifelse(value==1,3,ifelse(value==12,2,4)))))))))}
  else if(crop=="melon"){suitclass=ifelse(value==8,1,ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,2,ifelse(value==10,2,ifelse(value==2,2,ifelse(value==3,1,ifelse(value==1,3,ifelse(value==12,2,4)))))))))}
  else if(crop=="pineaple"){suitclass=ifelse(value==8,1,ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,2,ifelse(value==5,1,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==1,3,4))))))))}
  else if(crop=="avocado"){suitclass=ifelse(value==8,1,ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==5,1,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==12,2,ifelse(value==1,3,ifelse(value==16,3,4))))))))))}
  else if(crop=="pawpaw"){suitclass=ifelse(value==8,1,ifelse(value==11,1,ifelse(value==4,1,ifelse(value==7,1,ifelse(value==5,1,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==12,2,ifelse(value==1,3,ifelse(value==16,3,4))))))))))}

  #soil testure suitability rating for vegetables
  else if(crop=="tomato"){suitclass=ifelse(value==11,1,ifelse(value==5,1,ifelse(value==7,1,ifelse(value==9,1,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==8,2,ifelse(value==1,3,4))))))))}
  else if(crop=="cabbage"){suitclass=ifelse(value==5,1,ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==10,2,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==8,2,ifelse(value==1,3,4))))))))))}
  else if(crop=="vegetable"){suitclass=ifelse(value==5,1,ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==10,2,ifelse(value==2,3,ifelse(value==3,2,ifelse(value==8,2,ifelse(value==1,3,4))))))))))}
  else if(crop=="broccoli"){suitclass=ifelse(value==5,1,ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==10,2,ifelse(value==2,3,ifelse(value==3,2,ifelse(value==8,2,ifelse(value==1,3,4))))))))))}
  else if(crop=="cauliflower"){suitclass=ifelse(value==5,1,ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==10,2,ifelse(value==2,3,ifelse(value==3,2,ifelse(value==8,2,ifelse(value==1,3,4))))))))))}
  else if(crop=="okra"){suitclass=ifelse(value==5,1,ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==10,2,ifelse(value==2,3,ifelse(value==3,2,ifelse(value==8,2,ifelse(value==1,3,4))))))))))}

  #soil testure suitability rating for spices
  else if(crop=="chilli"){suitclass=ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==5,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==10,3,ifelse(value==12,3,ifelse(value==16,3,4))))))))))}
  else if(crop=="pepper"){suitclass=ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==8,1,ifelse(value==5,2,ifelse(value==3,2,ifelse(value==4,2,ifelse(value==10,3,ifelse(value==1,3,ifelse(value==16,3,4))))))))))}
  else if(crop=="ginger"){suitclass=ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==5,1,ifelse(value==8,2,ifelse(value==3,2,ifelse(value==1,3,ifelse(value==10,3,ifelse(value==12,3,ifelse(value==13,3,4))))))))))}
  else if(crop=="vanilla"){suitclass=ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==5,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==10,3,ifelse(value==12,3,ifelse(value==16,3,4))))))))))}
  else if(crop=="lemongrass"){suitclass=ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==5,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==10,3,ifelse(value==12,3,ifelse(value==16,3,4))))))))))}
  else if(crop=="onion"){suitclass=ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==5,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==10,3,ifelse(value==12,3,ifelse(value==16,3,4))))))))))}
  else if(crop=="cardamom"){suitclass=ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==5,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==10,3,ifelse(value==12,3,ifelse(value==16,3,4))))))))))}
  else if(crop=="tumeric"){suitclass=ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==5,2,ifelse(value==3,2,ifelse(value==1,2,ifelse(value==10,3,ifelse(value==12,3,ifelse(value==16,3,4))))))))))}

  #soil testure suitability rating for other crops
  else if(crop=="alfalfa"){suitclass=ifelse(value==5,1,ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==10,2,ifelse(value==2,2,ifelse(value==3,1,ifelse(value==8,1,ifelse(value==1,1,ifelse(value==16,2,ifelse(value==15,2,4))))))))))))}
  else if(crop=="rose"){suitclass=ifelse(value==5,1,ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==10,4,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==8,2,ifelse(value==1,3,4))))))))))}
  else if(crop=="jasmine"){suitclass=ifelse(value==5,1,ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==10,4,ifelse(value==2,2,ifelse(value==3,2,ifelse(value==8,2,ifelse(value==1,3,4))))))))))}

  #soil testure suitability rating for fleshy crops
  else if(crop=="pumpkin"){suitclass=ifelse(value==5,1,ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==10,2,ifelse(value==2,3,ifelse(value==3,2,ifelse(value==8,2,ifelse(value==1,3,4))))))))))}
  else if(crop=="squash"){suitclass=ifelse(value==5,1,ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==10,2,ifelse(value==2,3,ifelse(value==3,2,ifelse(value==8,2,ifelse(value==1,3,4))))))))))}
  else if(crop=="butternut"){suitclass=ifelse(value==5,1,ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==10,2,ifelse(value==2,3,ifelse(value==3,2,ifelse(value==8,2,ifelse(value==1,3,4))))))))))}
  else if(crop=="yam"){suitclass=ifelse(value==5,1,ifelse(value==11,1,ifelse(value==9,1,ifelse(value==7,1,ifelse(value==4,1,ifelse(value==10,2,ifelse(value==2,3,ifelse(value==3,2,ifelse(value==8,2,ifelse(value==1,3,4))))))))))}

  return(suitclass)
}
