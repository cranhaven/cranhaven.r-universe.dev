PlantCalc <- function(lengths, Ger=100, wr = 90, wh = 10, wg = 0.7, wu = 0.3, Unif = 1) {

  #data.frame dos resultados das funcoes
  result_trat_rep <- data.frame()
  result_mean_pa <- data.frame()
  result_mean_raiz <- data.frame()
  result_mean_total <- data.frame()
  result_mean_razao <- data.frame()
  result_unif_1 <- data.frame()
  result_unif_2 <- data.frame()
  result_growth <- data.frame()
  result_vigor <- data.frame()
  result_vigor_corr <- data.frame()

  #cria varios bancos de dados, para os niveis de trat e as rep, e rodas as funcoes para cada um
  niveis_1 <- levels(as.factor(lengths[[1]]))

  for (i in 1:length(niveis_1)) {

    lengths_calc <- subset(lengths, lengths[1]==niveis_1[i])

    niveis_2 <- levels(as.factor(lengths_calc[[2]]))

    for (j in 1:length(niveis_2)){

      lengths_calc_2 <- subset(lengths_calc, lengths_calc[2]==niveis_2[j])

      result_mean_pa <- rbind(result_mean_pa, mean_pa(lengths_calc_2))

      result_mean_raiz <- rbind(result_mean_raiz, mean_raiz(lengths_calc_2))

      result_mean_total <- rbind(result_mean_total, mean_total(lengths_calc_2))

      result_mean_razao <- rbind(result_mean_razao, mean_razao(lengths_calc_2))

      result_unif_1 <- rbind(result_unif_1, unif_1(lengths_calc_2))

      result_unif_2 <- rbind(result_unif_2, unif_2(lengths_calc_2))

      result_growth <- rbind(result_growth, growth(lengths_calc_2, wr, wh))

      result_vigor <- rbind(result_vigor, vigor(lengths_calc_2, wg, wu, Unif))

      if (is.data.frame(Ger)) {
        Ger_inf <- subset(Ger[2], Ger[1]==niveis_1[i])
        result_vigor_corr <- rbind(result_vigor_corr, vigor_corr(lengths_calc_2, Ger_inf, wg, wu, Unif))
      }
      else
      {
        result_vigor_corr <- rbind(result_vigor_corr, vigor_corr(lengths_calc_2, Ger, wg, wu, Unif))
      }

      result_trat_rep <- rbind(result_trat_rep, cbind(niveis_1[i], niveis_2[j]))
    }

  }

  result <- data.frame(result_trat_rep, result_mean_pa, result_mean_raiz, result_mean_total, result_mean_razao, result_unif_1, result_unif_2, result_growth, result_vigor, result_vigor_corr)


  colnames(result) <- c(names(lengths[1]), names(lengths[2]), 'mean_pa', 'mean_raiz', 'mean_total', 'mean_razao', 'Unif_1', 'Unif_2', 'Growth', 'Vigor', 'Vigor_corr')

  return(result)
}

