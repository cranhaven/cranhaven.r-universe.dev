initializetest<-function(){
   utils::data("envData",package="test")
}

Getenvir<-function(envData){

if(!exists("envData")) initializetest()
return(get(envData,envir=envData))
}
