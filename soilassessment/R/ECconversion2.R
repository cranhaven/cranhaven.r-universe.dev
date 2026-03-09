ECconversion2=function(ec, method="USDA", extract="1:1"){

  if(method=="landon"){
    if(extract=="1:1"){ElectConduct=harmonization(ec,2.2,0)}
    else if(extract=="1:5"){ElectConduct=harmonization(ec,6.4,0)}
    else if(extract=="1:3"){ElectConduct=harmonization(ec,3.81,0)}
  }
  else if(method=="kargas"){
    if(extract=="1:1"){ElectConduct=harmonization(ec,1.83,0.117)}
    else if(extract=="1:5"){ElectConduct=harmonization(ec,6.53,-0.108)}
  }
  else if(method=="ozkan"){
    if(extract=="1:1"){ElectConduct=harmonization(ec,1.93,-0.57)}
    else if(extract=="1:2.5"){ElectConduct=harmonization(ec,3.3,-0.2)}
    else if(extract=="1:5"){ElectConduct=harmonization(ec,5.97,-1.17)}
  }
  else if(method=="USDA") {
    if(extract=="1:1"){ElectConduct=harmonization(ec,3,0)}
    else if(extract=="1:5"){ElectConduct=harmonization(ec,4.5,0)}
    else if(extract=="1:2"){ElectConduct=harmonization(ec,5,0)}
  }
  else if(method=="hogg") {
    if(extract=="1:1"){ElectConduct=harmonization(ec,1.75,-0.37)}
    else if(extract=="1:2"){ElectConduct=harmonization(ec,1.38,-0.14)}
  }
  else if(method=="zhang") {
    if(extract=="1:1"){ElectConduct=harmonization(ec,1.79,1.46)}
  }
  else if(method=="chi") {
    if(extract=="1:5"){ElectConduct=harmonization(ec,11.68,-5.77)}
  }
  else if(method=="park") {
    if(extract=="1:5"){ElectConduct=harmonization(ec,8.7,0)}
  }
  else if(method=="viscounti") {
    if(extract=="1:5"){ElectConduct=harmonization(ec,6.53,-0.108)}
  }
  else if(method=="khorsandi") {
    if(extract=="1:5"){ElectConduct=harmonization(ec,5.4,-0.61)}
  }
  else if(method=="shahid") {
    if(extract=="1:2.5"){ElectConduct=harmonization(ec,4.77,0)}
  }
  else if(method=="klaustermeier") {
    if(extract=="1:5"){ElectConduct=10^(1.256*log10(ec) +0.766)}
  }
  else if(method=="he") {
    if(extract=="1:5"){ElectConduct=exp(0.7*log(ec)+1.78)}
  }
  else if(method=="aboukila") {
    if(extract=="1:5"){ElectConduct=harmonization(ec,11.74,-6.15)}
  }
  else if(method=="halder") {
    if(extract=="1:5"){ElectConduct=harmonization(ec,4.834,0.437)}
  }

  return(ElectConduct)
}
