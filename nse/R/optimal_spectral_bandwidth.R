
f.optimal_h = function(x,type){
  k = kernel("daniell", 0)
  spectrumh0 = spectrum(x, kernel= k, taper=0.5, plot = FALSE)
  len = length(x)
  h = seq(from = 2,to = min(round(0.3*len),200), by = 1)
  error = h
  for(i in 1:length(h)){
    if(type == "daniell" || type == "modified.daniell"){
    k = f.kernel_addon(type, h[i], steep= FALSE, y = NULL)
    }else{
    k = f.kernel_addon(type = type, h[i], steep = FALSE, y = NULL)
    }
    k$coef[1] = 0
    spectrumsmoothed = spectrum(x,kernel= k, taper=0.5, plot = FALSE)
    error[i] = f.stuetzle(spectrumsmooth = spectrumsmoothed,spectrumh0 = spectrumh0)
  }
  h_opt = which(error == min(error))
  return(h_opt)
}