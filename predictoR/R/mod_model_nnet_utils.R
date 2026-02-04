nnet_plot <- function(modelo) {
  plot(modelo, arrow.length = 0.1, rep = 'best', intercept = T, 
       x.entry = 0.1, x.out = 0.9, information = F, intercept.factor = 0.8,
       col.entry.synapse = 'red', col.entry = 'red', col.out = 'green',
       col.out.synapse = 'green', dimension = 15, radius = 0.2, fontsize = 10)
}