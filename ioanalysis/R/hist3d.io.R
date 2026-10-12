hist3d.io = function(obj, alpha = 1, phi = 65, theta = 45, limits, 
                   colors = ramp.col(c('yellow', 'violet', 'blue'))){
  
  if(missing(limits)){
    limits = c(min(obj), max(obj))
  } else if(length(limits) != 2){
    stop('Limits must have a minimum and a maximum. Cosnider either adding min(obj) or max(obj)')
  }
  
  hist3D(z = obj, alpha = alpha, phi = phi, theta = theta, border = 'black', bty = 'b2', 
         xlab = 'Sales', ylab = 'Purchases', zlab = 'Value', ticktype ='simple',
         d = 2, colkey = TRUE, zlim = limits, clim = limits,
         col = ramp.col(c('yellow', 'violet', 'blue')))
}