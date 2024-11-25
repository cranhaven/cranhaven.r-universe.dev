erosivFUN = function(rain,A,B, model="linear") {
  total <- 20
  MFI=rain
  if(model=="linear"){erosivity=rain*B+A}
  else if(model=="Fourier"){erosivity=A*MFI^B}
  else if(model=="power"){erosivity=A*rain^B}
  else if(model=="logarithmic"){erosivity=B*log(rain)+A}
  else if(model=="exponential"){erosivity=A*exp(B*rain)}

  for(i in 1:20){
    Sys.sleep(0.1)
    setTxtProgressBar((txtProgressBar(min = 0, max = total, style = 3)), i)
  }
  return(erosivity)
}
