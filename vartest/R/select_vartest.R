select_vartest<- function(formula, data, nrep = 1000, alpha = 0.05, na.rm = TRUE, verbose = TRUE){
  
  data <- model.frame(formula, data)
  dp <- as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  METHOD <- "Fisher's Test"
  
  if (na.rm) {
    completeObs <- complete.cases(data)
    data <- data[completeObs, ]
  }
  
  
  if (any(colnames(data) == dp[[3L]]) == FALSE) 
    stop("The name of group variable does not match the variable names in the data. The group variable must be one factor.")
  
  
  if (any(colnames(data) == dp[[2L]]) == FALSE) 
    stop("The name of response variable does not match the variable names in the data.")
  
  y = data[[dp[[2L]]]]
  
  group = data[[dp[[3L]]]]
  
  if (!(is.factor(group) | is.character(group))) 
    stop("The group variable must be a factor or a character.")
  
  if (is.character(group)) 
    group <- as.factor(group)
  
  if (!is.numeric(y)) 
    stop("The response must be a numeric variable.")
  
  if (is.list(y)) {
    if (length(y) < dp[[2L]])
      stop("'y' must be a list with at least 2 elements") 
  }
  
  
  group_params <- do.call(
    rbind,
    lapply(split(y, group), function(x) {
      data.frame(
        n    = length(x),
        mean = mean(x),
        var  = var(x),
        skew = skewness(x),
        kurt = kurtosis(x)
      )
    })
  )
  
  group_params$group <- rownames(group_params)
  rownames(group_params) <- NULL
  group_params <- group_params[, c("group", "n", "mean", "var", "skew", "kurt")]
  
  for (h in 1:2){
    
    if (h==1) group_params <- group_params
    if (h == 2) {
      group_params$var  <- mean(group_params$var)
    } 
    
    out<-NULL
    for (k in 1:nrep){
      
      sim_list <- lapply(1:nrow(group_params), function(i) {
        
        p <- group_params[i, ]
        
        y <- rpearson(
          n = p$n,
          moments = list(
            mean     = p$mean,
            variance = p$var,
            skewness = p$skew,
            kurtosis = p$kurt
          )
        )
        
        data.frame(
          group = p$group,
          y = y
        )
      })
      
      sim_data <- do.call(rbind, sim_list)
      sim_data$group<-as.factor(sim_data$group)
      
      out_bartletts<-bartletts.test(y ~ group, data = sim_data,verbose=F)$p.value
      out_cochrans<-cochrans.test(y ~ group, data = sim_data,verbose=F)$p.value
      out_mzv<-mzv.test(y ~ group, data = sim_data,verbose=F)$p.value
      out_f<-f.test(y ~ group, data = sim_data,verbose=F)$p.value
      out_g<-g.test(y ~ group, data = sim_data,verbose=F)$p.value
      out_zv<-zv.test(y ~ group, data = sim_data,verbose=F)$p.value
      out_ansari<-ansari.test(y ~ group, data = sim_data,verbose=F)$p.value
      out_capon<-capon.test(y ~ group, data = sim_data,verbose=F)$p.value
      out_david.barton<-david.barton.test(y ~ group, data = sim_data,verbose=F)$p.value
      out_duran<-duran.test(y ~ group, data = sim_data,verbose=F)$p.value
      out_fk<-fk.test(y ~ group, data = sim_data,verbose=F)$p.value
      out_klotz<-klotz.test(y ~ group, data = sim_data,verbose=F)$p.value
      out_mood<-mood.test(y ~ group, data = sim_data,verbose=F)$p.value
      out_siegel.tukey<-siegel.tukey.test(y ~ group, data = sim_data,verbose=F)$p.value
      out_talwar.gentle<-talwar.gentle.test(y ~ group, data = sim_data,verbose=F)$p.value
      out_hartley_mean<-hartley.test(y ~ group, data = sim_data, size = "mean",verbose=F)$p.value
      out_hartley_harmonic<-hartley.test(y ~ group, data = sim_data, size = "harmonic",verbose=F)$p.value
      out_hartley_maxn<-hartley.test(y ~ group, data = sim_data, size = "maxn",verbose=F)$p.value
      out_hartley_minvar<-hartley.test(y ~ group, data = sim_data, size = "minvar",verbose=F)$p.value
      out_obrien_mean<-obrien.test(y ~ group, data = sim_data, center = "mean",verbose=F)$p.value
      out_obrien_median<-obrien.test(y ~ group, data = sim_data, center = "median",verbose=F)$p.value
      out_obrien_trim.mean<-obrien.test(y ~ group, data = sim_data, center = "trim.mean",verbose=F)$p.value
      out_levene_mean_absolute<-levene.test(y ~ group, data = sim_data, center = "mean", deviation = "absolute", verbose=F)$p.value
      out_levene_mean_squared<-levene.test(y ~ group, data = sim_data, center = "mean", deviation = "squared",verbose=F)$p.value
      out_levene_median_absolute<-levene.test(y ~ group, data = sim_data, center = "median", deviation = "absolute",verbose=F)$p.value
      out_levene_median_squared<-levene.test(y ~ group, data = sim_data, center = "median", deviation = "squared",verbose=F)$p.value
      out_levene_trim.mean_absolute<-levene.test(y ~ group, data = sim_data, center = "trim.mean", deviation = "absolute",verbose=F)$p.value
      out_levene_trim.mean_squared<-levene.test(y ~ group, data = sim_data, center = "trim.mean", deviation = "squared",verbose=F)$p.value
      
      out<-rbind(out,cbind(out_bartletts,out_cochrans,out_mzv,out_f,out_g,out_zv,out_ansari,out_capon,out_david.barton,out_duran,out_fk,
                           out_klotz,out_mood,out_siegel.tukey,out_talwar.gentle,out_hartley_mean,out_hartley_harmonic,out_hartley_maxn,out_hartley_minvar,out_obrien_mean,
                           out_obrien_median,out_obrien_trim.mean,out_levene_mean_absolute,out_levene_mean_squared,out_levene_median_absolute,out_levene_median_squared,out_levene_trim.mean_absolute,
                           out_levene_trim.mean_squared))
      
    }
    
    if (h==1) power <- colMeans(out<=alpha)
    if (h==2) typeIerror <- colMeans(out<=alpha)
    
  }
  
  adjpower <- pnorm(qnorm(power)-qnorm(typeIerror)+qnorm(alpha))
  
  df <- data.frame(
    power = as.numeric(power),
    typeIerror = as.numeric(typeIerror),
    adjpower = as.numeric(adjpower)
  )
  
  df$Method <- c(
    "Bartlett", "Cochran's C", "Modified Z Variance", "Fisher", "G", 
    "Z Variance", "Ansari-Bradley", "Capon", "David-Barton", "Duran", 
    "Fligner-Killeen", "Klotz", "Mood", "Siegel-Tukey", 
    "Talwar-Gentle", "Hartley (Mean)", "Hartley (Harmonic)", 
    "Hartley (Max n)", "Hartley (Min Var)", "O'Brien (Mean)", 
    "O'Brien (Median)", "O'Brien (Trimmed Mean)", "Levene (Mean, Abs)", 
    "Levene (Mean, Sq)", "Levene (Med, Abs)", "Levene (Med, Sq)", 
    "Levene (Trim, Abs)", "Levene (Trim, Sq)"
  )
  
  df[is.na(df)]<-0
  
  df_sorted <- df[order(-df$adjpower, df$typeIerror), ]
  
  max_adj <- max(df_sorted$adjpower)
  
  min_typeI_among_best <- min(round(df_sorted$typeIerror[df_sorted$adjpower == max_adj], 4))
  if (verbose == TRUE) {
    
    cat(rep("=", 90), "\n", sep="")
    cat(sprintf("%-28s | %-10s | %-12s | %-15s | %-15s\n", 
                "Test Method", "Power", "Type I Error", "Adj. Power", "Evaluation"))
    cat(rep("-", 90), "\n", sep="")
    
    for(i in 1:nrow(df_sorted)) {
      
      eval_str <- "-"
      adj_str <- sprintf("%.4f", df_sorted$adjpower[i]) 
      
      if (df_sorted$adjpower[i] == max_adj) {
        
        if (round(df_sorted$typeIerror[i], 4) == min_typeI_among_best) {
          eval_str <- "Suggested *"
        }
      }
      
      cat(sprintf("%-28s | %-10.4f | %-12.4f | %-15s | %-15s\n", 
                  df_sorted$Method[i], 
                  df_sorted$power[i], 
                  df_sorted$typeIerror[i], 
                  adj_str, 
                  eval_str))
    }
    cat(rep("=", 90), "\n", sep="")
    
    num_max_adj <- sum(df_sorted$adjpower == max_adj)
    
    
    if (num_max_adj == 1) {
      cat("* Suggested method yielding the highest adjusted power.\n")
    } else {
      cat("* Suggested method yielding the highest adjusted power with the lowest Type I error.\n")
    }
  }
  
  result <- df_sorted[, c("Method", setdiff(names(df_sorted), "Method"))]
  
  return(invisible(result))
}

