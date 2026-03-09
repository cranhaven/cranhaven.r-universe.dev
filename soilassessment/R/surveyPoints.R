surveyPoints=function(soilmap,scorpan,conditionclass,mapproportion){
  if(is(soilmap,"RasterLayer")){
  map=soilmap;
  w=scorpan;
  b=conditionclass;
  j=mapproportion
  tagt=which(map[]<=b)
  emerg=sort(sample(tagt,length(tagt)*(j/100)))
  map[emerg]=1
  area=length(subset(getValues(map), getValues(map) == 1)) * res(map)^2
  w=ifelse(w<1,1,ifelse(w>5,5,w))
  area=area[1]
  samplnum=area*(1/(4*w*res(map)))^2
  samplnum=round(samplnum[1],0)
  map$new=values(map)<=b
  ndat=sampleRandom(map$new,samplnum, xy=TRUE, sp=TRUE)
  ndat=subset(ndat,ndat$new>0)}
  else {
    map=raster(soilmap);
    w=scorpan;
    b=conditionclass;
    j=mapproportion
    tagt=which(map[]<=b)
    emerg=sort(sample(tagt,length(tagt)*(j/100)))
    map[emerg]=1
    area=length(subset(getValues(map), getValues(map) == 1)) * res(map)^2
    w=ifelse(w<1,1,ifelse(w>5,5,w))
    area=area[1]
    samplnum=area*(1/(4*w*res(map)))^2
    samplnum=round(samplnum[1],0)
    map$new=values(map)<=b
    ndat=sampleRandom(map$new,samplnum, xy=TRUE, sp=TRUE)
    ndat=subset(ndat,ndat$new>0)
    }
    return(ndat)
}

