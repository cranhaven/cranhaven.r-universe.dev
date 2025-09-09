#'Accumulated Thermal Sum
#'@description
#'Calculates the daily and accumulated thermal sum, considering the subtraction
#' of the average air temperature by the lower cardinal temperature for each crop.
#'@param AAT The column with the average air temperature values.
#'@param crop Parameter to define the culture. Use 'maize' for maize, 'soybean'
#'for soybean, 'flax' for flaxseed, 'trit' for wheat or 'oat' for oat crop.
#'@param lbt Parameter to define the value of the lower basal temperature to be
#'used in the calculation. If not informed, the function will use the values of
#' 10, 5, 2, 2 and 0 \eqn{^{\circ}C} for maize, soybeans, flaxseed, wheat and
#'oats, respectively.
#'@param verbose Logical argument. Runs the code silently if FALSE.
#'@param plot Logical argument. Plot a graph of thermal accumulation if TRUE.
#' @return Returns the cumulative and total thermal sum considering the
#' cultivation cycle of the selected crop. Also presents the following parameters:\cr
#' \cr
#' * Total Cycle\cr
#'   The number of cycle days, for verification.\cr
#' \cr
#' * TS\cr
#'   The value of the total thermal sum, in daily degree days (GDD).\cr
#' \cr
#' * TBi\cr
#'   The value used for the lower base temperature.\cr
#' \cr
#' * General Parameters\cr
#'   Considering the reported average air temperature values, it returns
#'   the maximum, minimum, and coefficient of variation.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@examples
#'library(EstimateBreed)
#'
#'data("clima")
#'clima <- get("clima")[1:150, ]
#'
#'with(clima,atsum(TMED,crop="maize"))
#'
#'#Adjusting lower basal temperature manually
#'with(clima,atsum(TMED,crop="maize",lbt=12))
#'@export

atsum <- function(AAT,crop="maize",lbt=NULL,verbose=FALSE,plot=FALSE){

  if(crop=="maize"){
    if(!is.null(lbt)){
      TBi=lbt
    } else{TBi <- 10}
    ST <- AAT-TBi
    STot <- sum(ST)
    STAc <- cumsum(ST)
    CV <- (sd(AAT)/mean(AAT))*100
    VMax <- max(AAT)
    VMin <- min(AAT)
    acumulado <- data.frame(STAc)
    acumulado$Ciclo <- 1:nrow(acumulado)
    if(verbose==TRUE){
      print(acumulado)
      cat("\n------------------------------\n")
      cat("Thermal sum for the maize crop")
      cat("\n------------------------------\n")
      cat("Total Cycle =",tail(acumulado$Ciclo, n = 1),"Days\n")
      cat("TS =",paste(STot),"GDD\n")
      cat("TBi =",paste(TBi),"\u00B0C\n")
      cat("Max Value =",paste(VMax),"\u00B0C\n")
      cat("Min Value =",paste(VMin),"\u00B0C\n")
      cat("CV(%) =",paste(round(CV,digits = 2)),"\n")
    }
    if(plot==TRUE){
        grafico <- ggplot(acumulado, aes(x=Ciclo, y=STAc)) +
        geom_line(color="red", size=1, alpha=0.9, linetype=1) +
          ylab("Accumulated TS (\u00B0C)") + xlab("Maize Cycle") +
          theme_classic() +
        scale_x_continuous(breaks=seq(0,tail(acumulado$Ciclo, n = 1)+5,10))
      plot(grafico)
  }
  }
  else if(crop=="soybean"){
    if(!is.null(lbt)){
      TBi=lbt
    } else{TBi <- 10}
    ST <- AAT-TBi
    STot <- sum(ST)
    STAc <- cumsum(ST)
    CV <- (sd(AAT)/mean(AAT))*100
    VMax <- max(AAT)
    VMin <- min(AAT)
    acumulado <- data.frame(STAc)
    acumulado$Ciclo <- 1:nrow(acumulado)
    if(verbose==TRUE){
      print(acumulado)
      cat("\n----------------------------\n")
      cat("Thermal sum for the soybean crop")
      cat("\n----------------------------\n")
      cat("Total Cycle =",tail(acumulado$Ciclo, n = 1),"Days\n")
      cat("TS =",paste(STot),"GDD\n")
      cat("TBi =",paste(TBi),"\u00B0C\n")
      cat("Max Value =",paste(VMax),"\u00B0C\n")
      cat("Min Value =",paste(VMin),"\u00B0C\n")
      cat("CV(%) =",paste(round(CV,digits = 2)),"\n")
    }
    if(plot==TRUE){
      grafico <- ggplot(acumulado, aes(x=Ciclo, y=STAc)) +
        geom_line(color="red", size=1, alpha=0.9, linetype=1) +
        ylab("Accumulated TS (\u00B0C)") + xlab("Soybean Cycle") +
        theme_classic() +
        scale_x_continuous(breaks=seq(0,tail(acumulado$Ciclo, n = 1)+5,10))
      plot(grafico)
    }
  }
  else if (crop=="flax"){
    if(!is.null(lbt)){
      TBi=lbt
    } else{TBi <- 10}
    ST <- AAT-TBi
    STot <- sum(ST)
    STAc <- cumsum(ST)
    CV <- (sd(AAT)/mean(AAT))*100
    VMax <- max(AAT)
    VMin <- min(AAT)
    acumulado <- data.frame(STAc)
    acumulado$Ciclo <- 1:nrow(acumulado)
    if(verbose==TRUE){
      print(acumulado)
      cat("\n----------------------------\n")
      cat("Thermal sum form the flaxseed crop")
      cat("\n----------------------------\n")
      cat("Total Cycle =",tail(acumulado$Ciclo, n = 1),"Days\n")
      cat("TS =",paste(STot),"GDD\n")
      cat("TBi =",paste(TBi),"\u00B0C\n")
      cat("Max Value =",paste(VMax),"\u00B0C\n")
      cat("Min Value =",paste(VMin),"\u00B0C\n")
      cat("CV(%) =",paste(round(CV,digits = 2)),"\n")
    }
    if(plot==TRUE){
      grafico <- ggplot(acumulado, aes(x=Ciclo, y=STAc)) +
        geom_line(color="red", size=1, alpha=0.9, linetype=1) +
        ylab("Accumulated TS (\u00B0C)") + xlab("Flaxseed Cycle") +
        theme_classic() +
        scale_x_continuous(breaks=seq(0,tail(acumulado$Ciclo, n = 1)+5,10))
      plot(grafico)
    }
  }
  else if (crop=="trit"){
    if(!is.null(lbt)){
      TBi=lbt
    } else{TBi <- 10}
    ST <- AAT-TBi
    STot <- sum(ST)
    STAc <- cumsum(ST)
    CV <- (sd(AAT)/mean(AAT))*100
    VMax <- max(AAT)
    VMin <- min(AAT)
    acumulado <- data.frame(STAc)
    acumulado$Ciclo <- 1:nrow(acumulado)
    if(verbose==TRUE){
      print(acumulado)
      cat("\n----------------------------\n")
      cat("Thermal sum for the wheat crop")
      cat("\n----------------------------\n")
      cat("Total Cycle =",tail(acumulado$Ciclo, n = 1),"Days\n")
      cat("TS =",paste(STot),"GDD\n")
      cat("TBi =",paste(TBi),"\u00B0C\n")
      cat("Max Value =",paste(VMax),"\u00B0C\n")
      cat("Min Value =",paste(VMin),"\u00B0C\n")
      cat("CV(%) =",paste(round(CV,digits = 2)),"\n")
    }
    if(plot==TRUE){
      grafico <- ggplot(acumulado, aes(x=Ciclo, y=STAc)) +
        geom_line(color="red", size=1, alpha=0.9, linetype=1) +
        ylab("Accumulated TS (\u00B0C)") + xlab("Wheat Cycle") +
        theme_classic() +
        scale_x_continuous(breaks=seq(0,tail(acumulado$Ciclo, n = 1)+5,10))
      plot(grafico)
    }
    }
  else if (crop=="oat"){
    if(!is.null(lbt)){
      TBi=lbt
    } else{TBi <- 10}
    ST <- AAT-TBi
    STot <- sum(ST)
    STAc <- cumsum(ST)
    CV <- (sd(AAT)/mean(AAT))*100
    VMax <- max(AAT)
    VMin <- min(AAT)
    acumulado <- data.frame(STAc)
    acumulado$Ciclo <- 1:nrow(acumulado)
    if(verbose==TRUE){
      print(acumulado)
      cat("\n----------------------------\n")
      cat("Thermal sum for the oat crop")
      cat("\n----------------------------\n")
      cat("Total Cycle =",tail(acumulado$Ciclo, n = 1),"Days\n")
      cat("TS =",paste(STot),"GDD\n")
      cat("TBi =",paste(TBi),"\u00B0C\n")
      cat("Max Value =",paste(VMax),"\u00B0C\n")
      cat("Min Value =",paste(VMin),"\u00B0C\n")
      cat("CV(%) =",paste(round(CV,digits = 2)),"\n")
    }
    if(plot==TRUE){
      grafico <- ggplot(acumulado, aes(x=Ciclo, y=STAc)) +
        geom_line(color="red", size=1, alpha=0.9, linetype=1) +
        ylab("Accumulated TS (\u00B0C)") + xlab("Oat Cycle") +
        theme_classic() +
        scale_x_continuous(breaks=seq(0,tail(acumulado$Ciclo, n = 1)+5,10))
      plot(grafico)
    }
  }
}

#'Plotting the optimum and cardinal temperatures for crops
#'@description
#'Utility function for plotting graphs of thermal preferences for crops. It is
#'necessary to inform the  temperature values (minimum, average or maximum).
#'@param VAR The column with air temperature values (minimum, average or
#'maximum).
#'@param crop Parameter to define the culture. Use 'soybean' for soybean crop,
#''maize' for maize crop and 'trit' for wheat crop.
#'@param verbose Logical argument. Runs the code silently if FALSE.
#'@param ylab The name of the Y axis.
#'@param xlab The name of the X axis.
#'@param plot Logical argument. Plot a graph of optimal temperatures if TRUE.
#'@return Returns the parameters of lower basal and optimum temperature, upper
#'basal and optimum temperature, maximum temperature and average temperature.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@examples
#'library(EstimateBreed)
#'
#'data("clima")
#'clima <- get("clima")[1:150, ]
#'
#'with(clima,optemp(TMED,crop="soybean"))
#'@export

optemp <-function(VAR,crop = NULL,verbose=FALSE,plot=TRUE,
                  ylab = "Meteorological Atribute",
                  xlab = "Days After Sowing"){
  if(is.numeric(VAR)){
    VAR <- data.frame(VAR)
  }
  if(is.null(crop)){
    stop("Please enter the desired culture!", call. = FALSE)
  }
  nlines <- nrow(VAR)
  dados <- VAR %>%
    mutate(DAS = seq_len(nlines))
  dados <- dados[, c("DAS", setdiff(names(dados), "DAS"))]
  if (crop == "soybean") {
    TbInferior <- 10
    TbSuperior <- 35
    ToInferior <- 20
    ToSuperior <- 30
    TGeral <- mean(dados$VAR)
    Tmax <- max(VAR)
    Tmin <- min(VAR)
    if(plot==TRUE){
      grafico <- ggplot(dados, aes(x = DAS, y = VAR)) +
        geom_line(col = "red", size = 0.8, linetype = 2, group = 1) +
        ylab(ylab) + xlab(xlab) + theme_classic() +
        geom_segment(x = 0, y = TbInferior, xend = max(dados$DAS),
                     yend = TbInferior,linetype = 1, color = "blue") +
        annotate("label", x = 15, y = TbInferior, label = "Lower base temperature",
                 color = "blue") +
        geom_segment(x = 0, y = TbSuperior, xend = max(dados$DAS), yend = TbSuperior,
                     linetype = 1, color = "blue") +
        annotate("label", x = 15, y = TbSuperior, label = "Upper base temperature",
                 color = "blue") +
        geom_segment(x = 0, y = ToInferior, xend = max(dados$DAS), yend = ToInferior,
                     linetype = 2, color = "darkgreen") +
        annotate("label", x = 15, y = ToInferior, label = "Lower optimum temperature",
                 color = "darkgreen") +
        geom_segment(x = 0, y = ToSuperior, xend = max(dados$DAS), yend = ToSuperior,
                     linetype = 2, color = "darkgreen") +
        annotate("label", x = 15, y = ToSuperior, label = "Upper optimum temperature",
                 color = "darkgreen") +
        theme_classic()
      print(grafico)
    }

    if(verbose==TRUE){
      parameters<-list(
        InferiorBT=TbInferior,
        SuperiorBT=TbSuperior,
        InferiorOT=ToInferior,
        SuperiorOT=ToSuperior,
        AvTemp=TGeral,
        Tmax=Tmax,
        Tmin=Tmin
      )
      cat("\n-----------------------------------------------------------------\n")
      cat("General Parameters - Soybean")
      cat("\n-----------------------------------------------------------------\n")
      print(parameters)
    }

  }
  else if (crop=="maize"){
    TbInferior<-10
    TbSuperior<-34
    ToInferior<-18
    ToSuperior<-30
    TGeral<-mean(dados$VAR)
    Tmax<-max(VAR)
    Tmin<-min(VAR)
    if(plot==TRUE){
      grafico <- ggplot(dados, aes(x = DAS, y = VAR)) +
        geom_line(col = "red", size = 0.8, linetype = 2, group = 1) +
        ylab(ylab) + xlab(xlab) + theme_classic() +
        geom_segment(x = 0, y = TbInferior, xend = max(dados$DAS),
                     yend = TbInferior,linetype = 1, color = "blue") +
        annotate("label", x = 15, y = TbInferior, label = "Lower base temperature",
                 color = "blue") +
        geom_segment(x = 0, y = TbSuperior, xend = max(dados$DAS), yend = TbSuperior,
                     linetype = 1, color = "blue") +
        annotate("label", x = 15, y = TbSuperior, label = "Upper base temperature",
                 color = "blue") +
        geom_segment(x = 0, y = ToInferior, xend = max(dados$DAS), yend = ToInferior,
                     linetype = 2, color = "darkgreen") +
        annotate("label", x = 15, y = ToInferior, label = "Lower optimum temperature",
                 color = "darkgreen") +
        geom_segment(x = 0, y = ToSuperior, xend = max(dados$DAS), yend = ToSuperior,
                     linetype = 2, color = "darkgreen") +
        annotate("label", x = 15, y = ToSuperior, label = "Upper optimum temperature",
                 color = "darkgreen") +
        theme_classic()
      print(grafico)
    }
    if(verbose==TRUE){
      parameters<-list(
        InferiorBT=TbInferior,
        SuperiorBT=TbSuperior,
        InferiorOT=ToInferior,
        SuperiorOT=ToSuperior,
        AvTemp=TGeral,
        Tmax=Tmax,
        Tmin=Tmin
      )
      cat("\n-----------------------------------------------------------------\n")
      cat("General Parameters - Maize")
      cat("\n-----------------------------------------------------------------\n")
      print(parameters)
    }
  }
  else if (crop=="trit"){
    TbInferior<-1.5
    TbSuperior<-30
    ToInferior<-17.2
    ToSuperior<-26
    TGeral<-mean(dados$VAR)
    Tmax<-max(VAR)
    Tmin<-min(VAR)
    if(plot==TRUE){
      grafico <- ggplot(dados, aes(x = DAS, y = VAR)) +
        geom_line(col = "red", size = 0.8, linetype = 2, group = 1) +
        ylab(ylab) + xlab(xlab) + theme_classic() +
        geom_segment(x = 0, y = TbInferior, xend = max(dados$DAS),
                     yend = TbInferior,linetype = 1, color = "blue") +
        annotate("label", x = 15, y = TbInferior, label = "Lower base temperature",
                 color = "blue") +
        geom_segment(x = 0, y = TbSuperior, xend = max(dados$DAS), yend = TbSuperior,
                     linetype = 1, color = "blue") +
        annotate("label", x = 15, y = TbSuperior, label = "Upper base temperature",
                 color = "blue") +
        geom_segment(x = 0, y = ToInferior, xend = max(dados$DAS), yend = ToInferior,
                     linetype = 2, color = "darkgreen") +
        annotate("label", x = 15, y = ToInferior, label = "Lower optimum temperature",
                 color = "darkgreen") +
        geom_segment(x = 0, y = ToSuperior, xend = max(dados$DAS), yend = ToSuperior,
                     linetype = 2, color = "darkgreen") +
        annotate("label", x = 15, y = ToSuperior, label = "Upper optimum temperature",
                 color = "darkgreen") +
        theme_classic()
      print(grafico)
    }
    if(verbose==TRUE){
      parameters<-list(
        InferiorBT=TbInferior,
        SuperiorBT=TbSuperior,
        InferiorOT=ToInferior,
        SuperiorOT=ToSuperior,
        AvTemp=TGeral,
        Tmax=Tmax,
        Tmin=Tmin
      )
      cat("\n-----------------------------------------------------------------\n")
      cat("General Parameters - Wheat")
      cat("\n-----------------------------------------------------------------\n")
      print(parameters)
    }
  }
}

#'Soybean plastochron estimation
#'@description
#'Estimation of soybean plastochron using average air temperature and number of
#'nodes
#'@param GEN The column with the genotype name.
#'@param AAT The column with the average air temperature values.
#'@param STAD The column with the phenological stages of soybean, as described by
#' Fehr & Caviness (1977).
#'@param NN The column with the number of nodes measured in field.
#'@param habit Growth habit of the genotype (default = "ind"). Use "ind" for
#'indeterminate and "det" for determinate.
#'@param verbose Logical argument. Runs the code silently if FALSE.
#'@param plot Logical argument. Returns a graph with the linear models if TRUE.
#'@return If the growth habit is determined, the function returns a linear model
#'for the V1 to R1 stages (Early Pheno) and a linear model for the R1 to R5
#'stages (Late Pheno). If the growth habit is indeterminate, returns three linear
#'models: Early Pheno (V1 to R1), Intermediate Pheno (R1 to R3) and Late Pheno
#'(R3 to R5).
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@references Porta, F. S. D., Streck, N. A., Alberto, C. M., da Silva, M. R.,
#'& Tura, E. F. (2024). Improving understanding of the plastochron of
#'determinate and indeterminate soybean cultivars. Revista Brasileira de
#'Engenharia Agricola e Ambiental, 28(10), e278299.
#'\doi{10.1590/1807-1929/agriambi.v28n10e278299}
#'
#'Fehr, W. R., & Caviness, C. E. (1977). Stages of soybean development.
#'Iowa State University of Science and Technology Special Report, 80, 1-11.
#'@examples
#'library(EstimateBreed)
#'data("pheno")
#'
#'mod1 <- with(pheno, plast(GEN,TMED,EST,NN,habit="ind",plot=TRUE))
#'mod1
#'@export

plast <- function(GEN, AAT, STAD, NN, habit = "ind",verbose=FALSE, plot = FALSE){
  Tb <- 7.6
  Tot <- 31
  TB <- 40
  resultado <- data.frame(GEN, AAT, STAD, NN) %>%
    group_by(GEN) %>%
    mutate(
      TTd = case_when(
        AAT> Tb & AAT<= Tot ~ (Tot - Tb) * ((AAT- Tb) / (Tot - Tb)) * 1,
        AAT> Tot & AAT<= TB ~ (Tot - Tb) * ((AAT- TB) / (Tot - TB)) * 1,
        TRUE ~ 0
      ),
      ATT = cumsum(TTd)
    )

  total <- resultado %>%
    group_by(GEN) %>%
    summarize(TST = max(ATT, na.rm = TRUE))

  if (habit == "ind") {
    dadosf <- resultado %>%
      group_by(NN) %>%
      mutate(STA = max(ATT)) %>%
      ungroup() %>%
      filter(STAD %in% c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9",
                         "V10", "R1", "R2", "R3", "R4", "R5")) %>%
      mutate(Class = case_when(
        STAD %in% c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9",
                    "V10") ~ "Early",
        STAD %in% c("R1", "R2") ~ "Intermediate",
        STAD %in% c("R3", "R4", "R5") ~ "Late",
        TRUE ~ "Undefined"
      ))

    # LM by Class
    modc <- dadosf %>%
      group_by(Class) %>%
      summarise(
        modelo = list(lm(NN ~ STA, data = cur_data())),
        .groups = "drop"
      )

    res <- modc %>%
      mutate(resumo = lapply(modelo, summary))
    if(verbose==TRUE){
      cat("-------------------------------\n")
      cat("Early Soybean Pheno (V1 to R1)\n")
      print(res$resumo[[1]])
      cat("-------------------------------\n")
      cat("Intermediate Soybean Pheno (R1 to R3)\n")
      print(res$resumo[[2]])
      cat("-------------------------------\n")
      cat("Late Soybean Pheno (R3 to R5)\n")
      print(res$resumo[[3]])
    }
  }

  if (habit == "det") {
    dadosf <- resultado %>%
      group_by(NN) %>%
      mutate(STA = max(ATT)) %>%
      ungroup() %>%
      filter(STAD %in% c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9",
                         "V10", "R1", "R2", "R3")) %>%
      mutate(Class = case_when(
        STAD %in% c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10")
        ~ "Early",
        STAD %in% c("R1", "R2", "R3") ~ "Late",
        TRUE ~ "Undefined"
      ))

    # LM by Class
    modc <- dadosf %>%
      group_by(Class) %>%
      summarise(
        modelo = list(lm(NN ~ STA, data = cur_data())),
        .groups = "drop"
      )

    res <- modc %>%
      mutate(resumo = lapply(modelo, summary))
    if(verbose==TRUE){
      cat("-------------------------------\n")
      cat("Early Soybean Pheno (V1 to R1)\n")
      print(res$resumo[[1]])
      cat("-------------------------------\n")
      cat("Late Soybean Pheno (R1 to R3)\n")
      print(res$resumo[[2]])
    }
  }

  if (plot==TRUE) {
    modelos <- dadosf %>%
      group_by(Class) %>%
      summarise(model = list(lm(NN ~ STA, data = cur_data())), .groups = "drop")

    modelos_stats <- modelos %>%
      mutate(
        stats = map(model, ~ tidy(.x)),
        model_summary = map(model, ~ summary(.x)),
        rsq = map_dbl(model_summary, ~ .$r.squared),
        slope_pval = map_dbl(model_summary, ~ coef(.x)[2, "Pr(>|t|)"]),
        eq_text = map2(model, rsq, ~ paste(
          "y =", signif(coef(.x)[2], 6), "x +", signif(coef(.x)[1], 6), "\n",
          "R-squared =", signif(.y, 2), "\n",
          "Pr(>t) =", format.pval(coef(summary(.x))[2, "Pr(>|t|)"], digits = 5,
                                  eps = 1e-16)
        ))
      )

    dadosf <- dadosf %>%
      left_join(modelos, by = "Class") %>%
      mutate(pred = map2_dbl(model, STA,
                             ~ predict(.x, newdata = data.frame(STA = .y)))) %>%
      left_join(modelos_stats %>% select(Class, eq_text), by = "Class")

    x_limits <- c(min(dadosf$STA), max(dadosf$STA))
    y_limits <- c(min(dadosf$NN), max(dadosf$NN))

    num_classes <- nrow(modelos_stats)
    spacing <- (y_limits[2] - y_limits[1]) * 0.1

    p <- ggplot(dadosf, aes(x = STA, y = NN, color = Class, shape = Class)) +
      geom_point(size = 3) +
      geom_line(aes(y = pred), size = 1.2) +
      labs(title = "Soybean Plastochron",
           x = "Accumulated Thermal Sum (ATT, degrees Celsius Day)",
           y = "Number of Nodes (NN)",
           color = "Class",
           shape = "Class") +
      theme_classic() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        legend.position = "bottom"
      ) +
      scale_x_continuous(
        breaks = seq(floor(x_limits[1] / 200) * 200,
                     ceiling(x_limits[2] / 200) * 200, 200),
        expand = expansion(mult = 0.05)
      ) +
      scale_y_continuous(
        breaks = seq(floor(y_limits[1] / 2) * 2,
                     ceiling(y_limits[2] / 2) * 2, 2),
        expand = expansion(mult = 0.05)
      ) +
      coord_cartesian(xlim = x_limits, ylim = y_limits)

    for (i in 1:num_classes) {
      p <- p + annotation_custom(
        grob = textGrob(
          paste("Class:", modelos_stats$Class[i]),
          gp = gpar(fontsize = 10, fontface = "bold", col = "black")
        ),
        xmin = x_limits[1] + 0.05 * (x_limits[2] - x_limits[1]),
        xmax = x_limits[1] + 0.05 * (x_limits[2] - x_limits[1]),
        ymin = y_limits[2] - (i - 1) * spacing,
        ymax = y_limits[2] - (i - 1) * spacing
      )
      p <- p + annotation_custom(
        grob = textGrob(
          modelos_stats$eq_text[i],
          gp = gpar(fontsize = 10, fontface = "italic", col = "black")
        ),
        xmin = x_limits[1] + 0.05 * (x_limits[2] - x_limits[1]),
        xmax = x_limits[1] + 0.05 * (x_limits[2] - x_limits[1]),
        ymin = y_limits[2] - (i - 1) * spacing - 0.05 * (y_limits[2] - y_limits[1]),
        ymax = y_limits[2] - (i - 1) * spacing - 0.05 * (y_limits[2] - y_limits[1])
      )
    }
    print(p)
  }
}

#'Photothermal Index
#'@description
#'Calculation of the photothermal index based on average temperature and
#'radiation
#'@param DAY The column with the cycle days
#'@param AAT The column with the average air temperature values
#'@param RAD The column with the incident radiation values
#'@param PER The column with the period (use VEG for vegetative and REP for
#'reproductive)
#'@param verbose Logical argument. Runs the code silently if FALSE.
#'@return Retorna o ind fototermal
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@references
#'Zanon, A. J., & Tagliapietra, E. L. (2022). Ecofisiologia da soja:
#'Visando altas produtividades (2a ed.). Field Crops.
#'@examples
#'library(EstimateBreed)
#'data("termaldata")
#'
#'termal <- with(termaldata,ptermal(Day,Temperature,Radiation,Period))
#'termal
#'@export

ptermal <- function(DAY, AAT, RAD, PER,verbose=FALSE) {

  if (length(DAY) != length(AAT) || length(DAY) != length(RAD) ||
      length(DAY) != length(PER)) {
    stop("All input vectors must have the same length.")
  }
  if (!is.numeric(AAT) || any(AAT < 0)) {
    stop("Average Air Temperature values must be numeric and non-negative.")
  }
  if (!is.numeric(RAD) || any(RAD <= 0)) {
    stop("Radiation values must be numeric and positive.")
  }

  data <- data.frame(DAY, PER, AAT, RAD, stringsAsFactors = FALSE)
  data <- data[order(data$DAY), ]

  periodos <- unique(data$PER)
  T_base_dict <- setNames(rep(0, length(periodos)), periodos)

  data$Qac_final <- NA
  offset <- 0
  resultado <- data.frame()

  for (p in periodos) {
    dados_periodo <- subset(data, PER == p)
    T_base <- T_base_dict[p]

    dados_periodo$Tef <- dados_periodo$AAT - T_base
    dados_periodo$Q <- ifelse(dados_periodo$Tef > 0, dados_periodo$RAD /
                                dados_periodo$Tef, NA)
    dados_periodo$Qac_final <- cumsum(ifelse(is.na(dados_periodo$Q), 0,
                                             dados_periodo$Q)) + offset
    offset <- tail(dados_periodo$Qac_final, 1)
    resultado <- rbind(resultado, dados_periodo)
  }

  resultado <- resultado[order(resultado$DAY), ]
  if(verbose==TRUE){return(resultado)}
  return(resultado)
}

#'Optimum conditions for pesticide application
#'@description
#'Determining the ideal time for pesticide application using TDELTA
#'@param LON Longitude (in decimal)
#'@param LAT Latitude (in decimal)
#'@param type Type of analysis. Use 1 for forecast and 2 for temporal data.
#'@param days Number of days (only use this argument if type=1).
#'@param control Type of product to be applied. Use 'fung' for fungicide,
#''herb' for herbicide, 'ins' for insecticides, 'bio' for biological products.
#'@param details Returns the result in detail if TRUE.
#'@param verbose Logical argument. Runs the code silently if FALSE.
#'@param dates Only use this argument if type=2. Start and end date for obtaining
#'weather data for a crop cycle.
#'@param plot Logical argument. Plots a graphic if 'TRUE'.
#'@return Returns the ideal application times, considering each scenario.
#'Taking as a parameter a TDELTA between 2 and 8, wind speed between 3 and 8,
#'and no precipitation.
#'@author Willyan Junior Adorian Bandeira
#'@author Ivan Ricardo Carvalho
#'@author Murilo Vieira Loro
#'@author Leonardo Cesar Pradebon
#'@author Jose Antonio Gonzalez da Silva
#'@examples
#'\donttest{
#'library(EstimateBreed)
#'
#'# Forecasting application conditions
#'forecast <- tdelta(-53.6969,-28.0638,type=1,days=10,verbose=TRUE)
#'
#'# Retrospective analysis of application conditions
#'retrosp <- tdelta(-53.6969,-28.0638,type=2,days=10,
#'                  dates=c("2023-01-01","2023-05-01"),
#'                  verbose=TRUE)
#'}
#'@export

tdelta <- function(LON,LAT,type=2,days=7,control=NULL,details=FALSE,verbose=TRUE,
                   dates=NULL,plot=FALSE){

  if (type==1) {
    # Type 1 - Forecast
    url <- "https://api.open-meteo.com/v1/forecast"
    res <- GET(url, query = list(
      latitude = LAT,
      longitude = LON,
      hourly = "temperature_2m,relative_humidity_2m,windspeed_10m,precipitation",
      timezone = "auto",
      forecast_days = days
    ))
    if (status_code(res) != 200) {
      stop("Check the coordinates")
    }
    previsao <- fromJSON(content(res, "text"))
    hora <- previsao$hourly$time
    temp <- previsao$hourly$temperature_2m
    ur <- previsao$hourly$relative_humidity_2m
    wind <- previsao$hourly$windspeed_10m
    prec <- previsao$hourly$precipitation
    df1 <- data.frame(
      Hour = hora,
      Temp = temp,
      RH = ur,
      WindS = wind,
      Prec = prec
    )
    df1$Hour <- as.POSIXct(df1$Hour, format = "%Y-%m-%dT%H:%M", tz = "UTC")
    df1$Hour <- with_tz(df1$Hour, "America/Sao_Paulo")
    df1$Day <- as.Date(df1$Hour, tz = "America/Sao_Paulo")
    df1$HourF <- format(df1$Hour, "%H:%M")
    df1 <- df1[, c("Day", "HourF", "Temp", "RH", "WindS", "Prec")]
    colnames(df1)[2] <- "Hour"

    dt <- df1 %>%
      mutate(alpha = log(RH/100)+(17.27*Temp)/(237.7+Temp),
             Td = (237.7*alpha)/(17.27-alpha),
             DELTAT = Temp-Td)
    dt <- dt %>% select(-alpha,-Td)
    return(dt)
    assign("forecast", dt, envir = .estimatebreed_env)
    if(details==TRUE){
      print(previsao$hourly_units)
    }
    if(verbose==TRUE){
      if(is.null(control)){
        ideal <- dt %>%
          filter(DELTAT >= 2 & DELTAT <= 8,
                 WindS < 10,
                 Prec < 2)
        cat("Moments with ideal application conditions\n")
        cat("--------------------------------------------------------\n")
        print(ideal)
      } else if(control=="fung"){
        ideal <- dt %>%
          filter(DELTAT >= 2 & DELTAT <= 8,
                 WindS < 10,
                 Prec < 2,
                 Temp >= 15 & Temp <=25)
        cat("Optimum conditions for fungicide application\n")
        cat("--------------------------------------------------------\n")
        print(ideal)
      } else if(control=="ins"){
        ideal <- dt %>%
          filter(DELTAT >= 2 & DELTAT <= 8,
                 WindS < 10,
                 Prec < 2,
                 Temp >= 15 & Temp <=30)
        cat("Optimum conditions for insecticide application\n")
        cat("--------------------------------------------------------\n")
        print(ideal)
      } else if(control=="herb"){
        ideal <- dt %>%
          filter(DELTAT >= 2 & DELTAT <= 8,
                 WindS < 10,
                 Prec < 2,
                 Temp >= 15 & Temp <=30)
        cat("Optimum conditions for herbicide application\n")
        cat("--------------------------------------------------------\n")
        print(ideal)
      } else if(control=="bio"){
        ideal <- dt %>%
          filter(DELTAT >= 2 & DELTAT <= 8,
                 WindS < 10,
                 Prec < 2,
                 Temp >= 15 & Temp <=30)
        cat("Optimal conditions for applying biologicals\n")
        cat("--------------------------------------------------------\n")
        print(ideal)
      }
    }
  }

  if(type==2){
    if(is.null(dates) || length(dates) !=2){
      stop("The 'dates' parameter must be a vector with two dates in the format
          'YYYY-MM-DD'. Example: c('2023-01-01', '2023-05-01').")
    }
    clim <- get_power(
              community = "ag",
              pars = c("T2M", "RH2M", "PRECTOTCORR"),
              lonlat = c(LON, LAT),
              dates = dates,
              temporal_api = "hourly")
    assign("climate_data",clim,envir = .estimatebreed_env)

    clim <- clim %>%
      select(-LON,-LAT)
    dt <- clim %>%
      mutate(alpha = log(RH2M/100)+(17.27*T2M)/(237.7+T2M),
             Td = (237.7*alpha)/(17.27-alpha),
             DELTAT = T2M-Td)
    dt <- dt %>% select(-alpha,-Td)
    return(dt)
    ideal <- dt %>%
      filter(DELTAT >= 2 & DELTAT <= 8,
             PRECTOTCORR < 2)
    assign("retrospective",ideal,envir = .estimatebreed_env)
    if(verbose==TRUE){
      print(ideal)
    }
  }
}
