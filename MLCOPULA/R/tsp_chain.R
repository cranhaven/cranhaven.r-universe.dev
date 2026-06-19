chain_tst = function(dist_mat){
# revisar y en su caso instalar y cargar el paquete TSP
#    if(!require('TSP')) {
#    install.packages('TSP')
#    library('TSP')
#    }
  # inicializar las variables para reportar el resultado
  result = list()
  maxim = which.max(dist_mat)
  # k es el indice del valor de IM mas alto en la matriz
  k <- arrayInd(maxim, dim(dist_mat))
  # om es la matriz reescalada en base al valor mas alto para evitar valores negativos
  om = matrix(rep((dist_mat[maxim]*1.1),length(dist_mat)), ncol = ncol(dist_mat),
              nrow=nrow(dist_mat))
  matriz = om-dist_mat
  diag(matriz) = 0
  # Se crea un objeto TSP
  tsp = suppressWarnings(TSP(matriz))
  # Se resuelve el TSP con el Nearest Neighbor Algorithm.
  tour_1 = suppressWarnings(solve_TSP(tsp, method = "nn", 
                   control = list(start=k[2], rep = 50, two_opt = FALSE)))
  # El tour generado en el paso anterior se trata dwe optimizar con el Two-Opt Algorithm
  tour = suppressWarnings(solve_TSP(tsp, method = "two_opt",
                   control = list(tour = tour_1)))
  distances = c()
  # Se crea la tabla de variables con el tour optimizado
  for (i in 1:(length(tour)-1)) {
    distances[i] = dist_mat[tour[i], tour[i+1]]
  }
  # El resultado consta de una lista con la tabla
  names <- colnames(dist_mat)
  tour <- names[tour]
  result$table = data.frame(from = tour[-length(tour)],
                              to = tour[-1],
                              MI = distances)
  # Y la suma de IM
  result$total_MI = sum(result$table$MI)
  return(result)
}
