bitset<-function(A,bit,V){
#library(DescTools)

  Abit<-DecToBin(A)

  lengthA<-nchar(Abit)

if(lengthA<bit){
  extraLength<-bit-lengthA-1
  for (i in 1:extraLength) {
    Abit<-paste0("0",Abit)
  }

  if(V==0){
    out<-A
  }else{
    bit_return<-paste0(1,Abit)
    out<-BinToDec(bit_return)
  }


}else{
  if(V==0){
    bit_return<-paste0(substr(Abit,1,lengthA-bit),0,substr(Abit,lengthA-bit+2,lengthA))
    out<-BinToDec(bit_return)
  }else{
    bit_return<-paste0(substr(Abit,1,lengthA-bit),1,substr(Abit,lengthA-bit+2,lengthA))
    out<-BinToDec(bit_return)
  }
}


  return(out)
}
