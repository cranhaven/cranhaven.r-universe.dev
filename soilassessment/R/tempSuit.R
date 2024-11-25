tempSuit=function(value,crop="wheat"){
  #Temperature suitability requirements for cereals
  if(crop=="wheat"){suitclass=ifelse(value>34,4,ifelse(value>29,3,ifelse(value>26,2,ifelse(value>20,1,ifelse(value>18,2,ifelse(value>14,3,4))))))}
  else if(crop=="rice"){suitclass=ifelse(value>40,4,ifelse(value>38,3,ifelse(value>34,2,ifelse(value>30,1,ifelse(value>21,2,ifelse(value>15,3,4))))))}
  else if(crop=="maize"){suitclass=ifelse(value>40,4,ifelse(value>38,3,ifelse(value>33,2,ifelse(value>20,1,ifelse(value>15,2,ifelse(value>12,3,4))))))}
  else if(crop=="sorghum"){suitclass=ifelse(value>40,4,ifelse(value>35,3,ifelse(value>31,2,ifelse(value>26,1,ifelse(value>24,2,ifelse(value>20,3,4))))))}
  else if(crop=="barley"){suitclass=ifelse(value>40,4,ifelse(value>38,3,ifelse(value>33,2,ifelse(value>20,1,ifelse(value>15,2,ifelse(value>12,3,4))))))}
  else if(crop=="oat"){suitclass=ifelse(value>40,4,ifelse(value>38,3,ifelse(value>30,2,ifelse(value>25,1,ifelse(value>20,2,ifelse(value>16,3,4))))))}
  else if(crop=="millet"){suitclass=ifelse(value>40,4,ifelse(value>38,3,ifelse(value>34,2,ifelse(value>28,1,ifelse(value>24,2,ifelse(value>20,3,4))))))}

  #Temperature suitability requirements for legumes
  else if(crop=="groundnut"){suitclass=ifelse(value>40,4,ifelse(value>34,3,ifelse(value>31,2,ifelse(value>24,1,ifelse(value>22,2,ifelse(value>20,3,4))))))}
  else if(crop=="pea"){suitclass=ifelse(value>40,4,ifelse(value>38,3,ifelse(value>34,2,ifelse(value>28,1,ifelse(value>25,2,ifelse(value>20,3,4))))))}
  else if(crop=="gram"){suitclass=ifelse(value>40,4,ifelse(value>30,3,ifelse(value>26,2,ifelse(value>20,1,ifelse(value>15,2,ifelse(value>5,3,4))))))}
  else if(crop=="bean"){suitclass=ifelse(value>45,4,ifelse(value>41,3,ifelse(value>36,2,ifelse(value>20,1,ifelse(value>18,2,ifelse(value>15,3,4))))))}
  else if(crop=="soybean"){suitclass=ifelse(value>36,4,ifelse(value>33,3,ifelse(value>28,2,ifelse(value>25,1,ifelse(value>15,2,ifelse(value>10,3,4))))))}
  else if(crop=="lentil"){suitclass=ifelse(value>30,4,ifelse(value>27,3,ifelse(value>20,2,ifelse(value>18,1,ifelse(value>12,2,ifelse(value>10,3,4))))))}

  #Temperature suitability requirements for forests
  else if(crop=="poplar"){suitclass=ifelse(value>40,4,ifelse(value>34,3,ifelse(value>31,2,ifelse(value>24,1,ifelse(value>22,2,ifelse(value>20,3,4))))))}
  else if(crop=="grevillea"){suitclass=ifelse(value>40,4,ifelse(value>38,3,ifelse(value>34,2,ifelse(value>28,1,ifelse(value>25,2,ifelse(value>20,3,4))))))}
  else if(crop=="sesbania"){suitclass=ifelse(value>40,4,ifelse(value>30,3,ifelse(value>26,2,ifelse(value>20,1,ifelse(value>15,2,ifelse(value>5,3,4))))))}
  else if(crop=="calliandra"){suitclass=ifelse(value>45,4,ifelse(value>41,3,ifelse(value>36,2,ifelse(value>20,1,ifelse(value>18,2,ifelse(value>15,3,4))))))}
  else if(crop=="leucaena"){suitclass=ifelse(value>36,4,ifelse(value>33,3,ifelse(value>28,2,ifelse(value>25,1,ifelse(value>15,2,ifelse(value>10,3,4))))))}
  else if(crop=="acacia"){suitclass=ifelse(value>30,4,ifelse(value>27,3,ifelse(value>20,2,ifelse(value>18,1,ifelse(value>12,2,ifelse(value>10,3,4))))))}
  else if(crop=="eucalyptus"){suitclass=ifelse(value>30,4,ifelse(value>27,3,ifelse(value>20,2,ifelse(value>18,1,ifelse(value>12,2,ifelse(value>10,3,4))))))}
  else if(crop=="teak"){suitclass=ifelse(value>30,4,ifelse(value>27,3,ifelse(value>20,2,ifelse(value>18,1,ifelse(value>12,2,ifelse(value>10,3,4))))))}
  else if(crop=="maple"){suitclass=ifelse(value>30,4,ifelse(value>27,3,ifelse(value>20,2,ifelse(value>18,1,ifelse(value>12,2,ifelse(value>10,3,4))))))}
  else if(crop=="ash"){suitclass=ifelse(value>30,4,ifelse(value>27,3,ifelse(value>20,2,ifelse(value>18,1,ifelse(value>12,2,ifelse(value>10,3,4))))))}
  

  #Temperature suitability requirements for oil crops
  if(crop=="sesame"){suitclass=ifelse(value>40,4,ifelse(value>38,3,ifelse(value>34,2,ifelse(value>30,1,ifelse(value>24,2,ifelse(value>20,3,4))))))}
  else if(crop=="sunflower"){suitclass=ifelse(value>40,4,ifelse(value>35,3,ifelse(value>31,2,ifelse(value>24,1,ifelse(value>20,2,ifelse(value>16,3,4))))))}
  else if(crop=="oilpalm"){suitclass=ifelse(value>40,4,ifelse(value>35,3,ifelse(value>33,2,ifelse(value>28,1,ifelse(value>26,2,ifelse(value>20,3,4))))))}
  else if(crop=="castor"){suitclass=ifelse(value>40,4,ifelse(value>36,3,ifelse(value>32,2,ifelse(value>26,1,ifelse(value>24,2,ifelse(value>15,3,4))))))}
  else if(crop=="safflower"){suitclass=ifelse(value>40,4,ifelse(value>34,3,ifelse(value>30,2,ifelse(value>28,1,ifelse(value>18,2,ifelse(value>15,3,4))))))}
  else if(crop=="rapeseed"){suitclass=ifelse(value>35,4,ifelse(value>33,3,ifelse(value>27,2,ifelse(value>20,1,ifelse(value>15,2,ifelse(value>10,3,4))))))}
  else if(crop=="mustard"){suitclass=ifelse(value>35,4,ifelse(value>33,3,ifelse(value>27,2,ifelse(value>20,1,ifelse(value>15,2,ifelse(value>10,3,4))))))}
  else if(crop=="olive"){suitclass=ifelse(value>40,4,ifelse(value>34,3,ifelse(value>30,2,ifelse(value>20,1,ifelse(value>15,2,ifelse(value>5,3,4))))))}

  #Temperature suitability requirements for nuts
  else if(crop=="coconut"){suitclass=ifelse(value>40,4,ifelse(value>34,3,ifelse(value>30,2,ifelse(value>26,1,ifelse(value>23,2,ifelse(value>20,3,4))))))}
  else if(crop=="cashew"){suitclass=ifelse(value>40,4,ifelse(value>36,3,ifelse(value>30,2,ifelse(value>25,1,ifelse(value>15,2,ifelse(value>12,3,4))))))}
  else if(crop=="almond"){suitclass=ifelse(value>40,2,ifelse(value>34,1,ifelse(value>25,2,ifelse(value>10,3,4))))}
  else if(crop=="pistachio"){suitclass=ifelse(value>40,2,ifelse(value>34,1,ifelse(value>25,2,ifelse(value>10,3,4))))}

  #Temperature suitability requirements for fibre/industrial (cotton,sugarcane, saffron)
  else if(crop=="cotton"){suitclass=ifelse(value>40,4,ifelse(value>35,3,ifelse(value>30,2,ifelse(value>20,1,ifelse(value>15,2,ifelse(value>10,3,4))))))}
  else if(crop=="sugarcane"){suitclass=ifelse(value>40,4,ifelse(value>38,3,ifelse(value>34,2,ifelse(value>30,1,ifelse(value>25,2,ifelse(value>20,3,4))))))}
  else if(crop=="saffron"){suitclass=ifelse(value>35,4,ifelse(value>28,3,ifelse(value>22,2,ifelse(value>10,1,ifelse(value>6,2,ifelse(value>0,3,4))))))}
  else if(crop=="tea"){suitclass=ifelse(value>30,4,ifelse(value>28,3,ifelse(value>25,2,ifelse(value>18,1,ifelse(value>15,2,ifelse(value>12,3,4))))))}
  else if(crop=="coffee"){suitclass=ifelse(value>28,4,ifelse(value>26,3,ifelse(value>24,2,ifelse(value>20,1,ifelse(value>15,2,ifelse(value>12,3,4))))))}
  else if(crop=="pyrethrum"){suitclass=ifelse(value>21,4,ifelse(value>19,3,ifelse(value>17,2,ifelse(value>12,1,ifelse(value>10,2,ifelse(value>7,3,4))))))}
  else if(crop=="rubber"){suitclass=ifelse(value>34,4,ifelse(value>32,3,ifelse(value>30,2,ifelse(value>25,1,ifelse(value>18,2,ifelse(value>12,3,4))))))}
  else if(crop=="jute"){suitclass=ifelse(value>43,4,ifelse(value>39,3,ifelse(value>35,2,ifelse(value>25,1,ifelse(value>22,2,ifelse(value>15,3,4))))))}
  else if(crop=="tobacco"){suitclass=ifelse(value>40,4,ifelse(value>33,3,ifelse(value>30,2,ifelse(value>25,1,ifelse(value>21,2,ifelse(value>15,3,4))))))}

  #Temperature suitability requirements for tuber
  else if(crop=="sweetpotato"){suitclass=ifelse(value>40,4,ifelse(value>35,3,ifelse(value>32,2,ifelse(value>25,1,ifelse(value>21,2,ifelse(value>15,3,4))))))}
  else if(crop=="cassava"){suitclass=ifelse(value>40,4,ifelse(value>33,3,ifelse(value>30,2,ifelse(value>23,1,ifelse(value>18,2,ifelse(value>12,3,4))))))}
  else if(crop=="potato"){suitclass=ifelse(value>32,4,ifelse(value>30,3,ifelse(value>26,2,ifelse(value>15,1,ifelse(value>13,2,ifelse(value>10,3,4))))))}
  else if(crop=="carrot"){suitclass=ifelse(value>35,4,ifelse(value>28,3,ifelse(value>22,2,ifelse(value>18,1,ifelse(value>13,2,ifelse(value>4,3,4))))))}
  else if(crop=="turnip"){suitclass=ifelse(value>40,4,ifelse(value>33,3,ifelse(value>25,2,ifelse(value>20,1,ifelse(value>15,2,ifelse(value>10,3,4))))))}
  else if(crop=="radish"){suitclass=ifelse(value>40,4,ifelse(value>30,3,ifelse(value>24,2,ifelse(value>18,1,ifelse(value>15,2,ifelse(value>10,3,4))))))}

  #Temperature suitability requirements for fruits
  else if(crop=="mango"){suitclass=ifelse(value>40,4,ifelse(value>36,3,ifelse(value>33,2,ifelse(value>28,1,ifelse(value>24,2,ifelse(value>20,3,4))))))}
  else if(crop=="grape"){suitclass=ifelse(value>40,4,ifelse(value>36,3,ifelse(value>30,2,ifelse(value>25,1,ifelse(value>21,2,ifelse(value>15,3,4))))))}
  else if(crop=="citrus"){suitclass=ifelse(value>40,4,ifelse(value>36,3,ifelse(value>30,2,ifelse(value>28,1,ifelse(value>25,2,ifelse(value>20,3,4))))))}
  else if(crop=="pomegranate"){suitclass=ifelse(value>40,4,ifelse(value>38,3,ifelse(value>34,2,ifelse(value>30,1,ifelse(value>25,2,ifelse(value>15,3,4))))))}
  else if(crop=="watermelon"){suitclass=ifelse(value>35,4,ifelse(value>32,3,ifelse(value>30,2,ifelse(value>24,1,ifelse(value>20,2,ifelse(value>18,3,4))))))}
  else if(crop=="avocado"){suitclass=ifelse(value>35,4,ifelse(value>30,3,ifelse(value>26,2,ifelse(value>22,1,ifelse(value>15,2,ifelse(value>10,3,4))))))}
  else if(crop=="banana"){suitclass=ifelse(value>40,4,ifelse(value>33,3,ifelse(value>25,2,ifelse(value>18,1,ifelse(value>15,2,ifelse(value>10,3,4))))))}
  else if(crop=="pineaple"){suitclass=ifelse(value>35,4,ifelse(value>30,3,ifelse(value>26,2,ifelse(value>20,1,ifelse(value>18,2,ifelse(value>16,3,4))))))}
  else if(crop=="pawpaw"){suitclass=ifelse(value>38,4,ifelse(value>34,3,ifelse(value>28,2,ifelse(value>24,1,ifelse(value>15,2,ifelse(value>8,3,4))))))}
  else if(crop=="melon"){suitclass=ifelse(value>35,4,ifelse(value>32,3,ifelse(value>30,2,ifelse(value>22,1,ifelse(value>20,2,ifelse(value>18,3,4))))))}

  #Temperature suitability requirements for vegetables
  else if(crop=="tomato"){suitclass=ifelse(value>36,4,ifelse(value>33,3,ifelse(value>28,2,ifelse(value>25,1,ifelse(value>21,2,ifelse(value>15,3,4))))))}
  else if(crop=="vegetable"){suitclass=ifelse(value>35,4,ifelse(value>30,3,ifelse(value>25,2,ifelse(value>20,1,ifelse(value>15,2,ifelse(value>10,3,4))))))}
  else if(crop=="cabbage"){suitclass=ifelse(value>35,4,ifelse(value>30,3,ifelse(value>25,2,ifelse(value>20,1,ifelse(value>15,2,ifelse(value>10,3,4))))))}
  else if(crop=="okra"){suitclass=ifelse(value>35,4,ifelse(value>30,3,ifelse(value>25,2,ifelse(value>20,1,ifelse(value>20,2,ifelse(value>15,3,4))))))}
  else if(crop=="cauliflower"){suitclass=ifelse(value>35,4,ifelse(value>30,3,ifelse(value>25,2,ifelse(value>20,1,ifelse(value>15,2,ifelse(value>10,3,4))))))}
  else if(crop=="broccoli"){suitclass=ifelse(value>35,4,ifelse(value>30,3,ifelse(value>25,2,ifelse(value>20,1,ifelse(value>15,2,ifelse(value>10,3,4))))))}

  #Temperature suitability requirements for spices
  else if(crop=="chilli"){suitclass=ifelse(value>38,4,ifelse(value>36,3,ifelse(value>33,2,ifelse(value>25,1,ifelse(value>21,2,ifelse(value>15,3,4))))))}
  else if(crop=="pepper"){suitclass=ifelse(value>40,4,ifelse(value>38,3,ifelse(value>35,2,ifelse(value>28,1,ifelse(value>21,2,ifelse(value>15,3,4))))))}
  else if(crop=="onion"){suitclass=ifelse(value>25,4,ifelse(value>23,3,ifelse(value>19,2,ifelse(value>16,1,ifelse(value>13,2,ifelse(value>10,3,4))))))}
  else if(crop=="ginger"){suitclass=ifelse(value>40,4,ifelse(value>33,3,ifelse(value>30,2,ifelse(value>25,1,ifelse(value>21,2,ifelse(value>15,3,4))))))}
  else if(crop=="tumeric"){suitclass=ifelse(value>40,4,ifelse(value>38,3,ifelse(value>33,2,ifelse(value>28,1,ifelse(value>19,2,ifelse(value>10,3,4))))))}
  else if(crop=="cardamom"){suitclass=ifelse(value>40,4,ifelse(value>38,3,ifelse(value>30,2,ifelse(value>25,1,ifelse(value>21,2,ifelse(value>15,3,4))))))}
  else if(crop=="lemongrass"){suitclass=ifelse(value>40,4,ifelse(value>38,3,ifelse(value>30,2,ifelse(value>25,1,ifelse(value>21,2,ifelse(value>15,3,4))))))}
  else if(crop=="vanilla"){suitclass=ifelse(value>40,4,ifelse(value>36,3,ifelse(value>33,2,ifelse(value>21,1,ifelse(value>15,2,ifelse(value>10,3,4))))))}

  #Temperature suitability requirements for other crops
  else if(crop=="alfalfa"){suitclass=ifelse(value>40,4,ifelse(value>32,3,ifelse(value>26,2,ifelse(value>23,1,ifelse(value>20,2,ifelse(value>15,3,4))))))}
  else if(crop=="rose"){suitclass=ifelse(value>36,4,ifelse(value>32,3,ifelse(value>30,2,ifelse(value>25,1,ifelse(value>21,2,ifelse(value>15,3,4))))))}
  else if(crop=="jasmine"){suitclass=ifelse(value>40,4,ifelse(value>35,3,ifelse(value>24,2,ifelse(value>18,1,ifelse(value>14,2,ifelse(value>10,3,4))))))}

  #Temperature suitability requirements for fleshy crops
  else if(crop=="yam"){suitclass=ifelse(value>40,4,ifelse(value>33,3,ifelse(value>30,2,ifelse(value>25,1,ifelse(value>21,2,ifelse(value>15,3,4))))))}
  else if(crop=="butternut"){suitclass=ifelse(value>40,4,ifelse(value>33,3,ifelse(value>30,2,ifelse(value>25,1,ifelse(value>21,2,ifelse(value>15,3,4))))))}
  else if(crop=="squash"){suitclass=ifelse(value>40,4,ifelse(value>33,3,ifelse(value>30,2,ifelse(value>25,1,ifelse(value>21,2,ifelse(value>15,3,4))))))}
  else if(crop=="pumpkin"){suitclass=ifelse(value>40,4,ifelse(value>33,3,ifelse(value>30,2,ifelse(value>25,1,ifelse(value>21,2,ifelse(value>15,3,4))))))}

  return(suitclass)
}
