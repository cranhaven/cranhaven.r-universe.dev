# Leloup and Goldbeter model
#   Based on the discrete model, allow Gaussian stochastic noise terms
#   1998 DOI: https://doi.org/10.1177/074873098128999934
#   2000 DOI: https://doi.org/10.1002/(sici)1521-1878(200001)22:1%3C84::aid-bies13%3E3.0.co;2-i


# # Kinetic model R functions commented here.
# #   Used in explicitly full forms in model equations.
# HILL <- function(Vmax, K, STATE, n){
#   Vmax*STATE^n / (K^n + STATE^n)
# }
# 
# MM <- function(Vmax, K, STATE){
#   HILL(Vmax, K, STATE, 1)
# }
# 
# REPRESS <- function(Vmax, K, STATE, n){
#   # Repression is hill-like equation
#   # However, when STATE = 0, reaches maximum
#   Vmax*K^n / (K^n + STATE^n)
# }

# Count multiplier, nM -> count
#   assume (10um)^3 = 1000 fL volume of cell
#   NA*1E-9 [(nmol)^-1] * 1000 [fL] * X [nmol/L] = NA*1E-9*1E-12*X [count]
# WHENEVER CONVERTION CHANGES, MUST UPDATE `getOdinGen()`!!!
cellDimUM <- 10
mulNM <- 6.02214076E23 * 1E-9 * 1E-15 * cellDimUM^3
# Count multiplier, count -> nM
#   nM * mulNM = count, therefore nM = count/mulNM = count * (1/mulNM)
#mulCOUNT <- 1/mulNM

# Time step in hours
STEP_HOURS <- user(0.001)

# LD cycle parameters
LD_HOURS <- user(720)
VdT_ON_conc <- user(6.0)
VdT_OFF_conc <- user(3.0)
VdT_ON <- VdT_ON_conc * mulNM
VdT_OFF <- VdT_OFF_conc * mulNM

# Core equations - discrete step model

#   TIM protein degradation rate update
current_hour <- step * STEP_HOURS
V_dT <- 
  if (current_hour >= LD_HOURS || ((current_hour %/% 12) %% 2 == 1)) VdT_OFF else VdT_ON

#   Noise update (unit: 1/sqrt(hour))
#     Must use independent noise terms for each noise in the system.
SH <- sqrt(STEP_HOURS)
NS_M_T <- 
  if (NoiseVariance_M_T < 0) 
    sqrt(inc_hour1_1+inc_hour1_2+inc_hour1_3) else sqrt(NoiseVariance_M_T * mulNM)
NS_M_P <- 
  if (NoiseVariance_M_T < 0)
    sqrt(inc_hour2_1+inc_hour2_2+inc_hour2_3) else sqrt(NoiseVariance_M_P * mulNM)

NS_T_0 <- 
  if (NoiseVariance_T_0 < 0)
    sqrt(inc_hour3_1+inc_hour3_2+inc_hour3_3+inc_hour3_4) else sqrt(NoiseVariance_T_0 * mulNM)
NS_T_1 <- 
  if (NoiseVariance_T_1 < 0)
    sqrt(inc_hour4_1+inc_hour4_2+inc_hour4_3+inc_hour4_4+inc_hour4_5) else sqrt(NoiseVariance_T_1 * mulNM)
NS_T_2 <- 
  if (NoiseVariance_T_2 < 0)
    sqrt(inc_hour5_1+inc_hour5_2+inc_hour5_3+inc_hour5_4+inc_hour5_5+inc_hour5_6) else sqrt(NoiseVariance_T_2 * mulNM)

NS_P_0 <- 
  if (NoiseVariance_P_0 < 0)
    sqrt(inc_hour6_1+inc_hour6_2+inc_hour6_3+inc_hour6_4) else sqrt(NoiseVariance_P_0 * mulNM)
NS_P_1 <- 
  if (NoiseVariance_P_1 < 0)
    sqrt(inc_hour7_1+inc_hour7_2+inc_hour7_3+inc_hour7_4+inc_hour7_5) else sqrt(NoiseVariance_P_1 * mulNM)
NS_P_2 <- 
  if (NoiseVariance_P_2 < 0)
    sqrt(inc_hour8_1+inc_hour8_2+inc_hour8_3+inc_hour8_4+inc_hour8_5+inc_hour8_6) else sqrt(NoiseVariance_P_2 * mulNM)

NS_C <- 
  if (NoiseVariance_C < 0)
    sqrt(inc_hour9_1+inc_hour9_2+inc_hour9_3) else sqrt(NoiseVariance_C * mulNM)
NS_C_N <- 
  if (NoiseVariance_C_N < 0)
    sqrt(inc_hour10_1+inc_hour10_2+inc_hour10_3) else sqrt(NoiseVariance_C_N * mulNM)
#current_noise <- rnorm(0, 1)*SH

#   mRNA of TIM and PER
inc_hour1_1 <- V_sT*K_IT^n / (K_IT^n + C_N^n)
inc_hour1_2 <- k_d*M_T
inc_hour1_3 <- V_mT*M_T / (K_mT + M_T)
inc_hour1 <- 
  inc_hour1_1 - inc_hour1_2 - inc_hour1_3
update(M_T) <- M_T + inc_hour1*STEP_HOURS + NS_M_T*rnorm(0, 1)*SH

inc_hour2_1 <- V_sP*K_IP^n / (K_IP^n + C_N^n)
inc_hour2_2 <- k_d*M_P
inc_hour2_3 <- V_mP*M_P / (K_mP + M_P)
inc_hour2 <- 
  inc_hour2_1 - inc_hour2_2 - inc_hour2_3
update(M_P) <- M_P + inc_hour2*STEP_HOURS + NS_M_P*rnorm(0, 1)*SH

#   Protein of TIM
inc_hour3_1 <- k_sT*M_T
inc_hour3_2 <- V_2T*T_1 / (K_p + T_1)
inc_hour3_3 <- k_d*T_0
inc_hour3_4 <- V_1T*T_0 / (K_p + T_0)
inc_hour3 <- 
  inc_hour3_1 + inc_hour3_2 - inc_hour3_3 - inc_hour3_4
update(T_0) <- T_0 + inc_hour3*STEP_HOURS + NS_T_0*rnorm(0, 1)*SH

inc_hour4_1 <- V_1T*T_0 / (K_p + T_0)
inc_hour4_2 <- V_4T*T_2 / (K_p + T_2)
inc_hour4_3 <- k_d*T_1
inc_hour4_4 <- V_2T*T_1 / (K_p + T_1)
inc_hour4_5 <- V_3T*T_1 / (K_p + T_1)
inc_hour4 <- 
  inc_hour4_1 + inc_hour4_2 - inc_hour4_3 - inc_hour4_4 - inc_hour4_5
update(T_1) <- T_1 + inc_hour4*STEP_HOURS + NS_T_1*rnorm(0, 1)*SH

inc_hour5_1 <- V_3T*T_1 / (K_p + T_1)
inc_hour5_2 <- k_4*C
inc_hour5_3 <- k_d*T_2
inc_hour5_4 <- k_3*P_2*T_2
inc_hour5_5 <- V_4T*T_2 / (K_p + T_2)
inc_hour5_6 <- V_dT*T_2 / (K_dT + T_2)
inc_hour5 <- 
  inc_hour5_1 + inc_hour5_2 - inc_hour5_3 - inc_hour5_4 - inc_hour5_5 - inc_hour5_6
update(T_2) <- T_2 + inc_hour5*STEP_HOURS + NS_T_2*rnorm(0, 1)*SH

#   Protein of PER
inc_hour6_1 <- k_sP*M_P
inc_hour6_2 <- V_2P*P_1 / (K_p + P_1)
inc_hour6_3 <- k_d*P_0
inc_hour6_4 <- V_1P*P_0 / (K_p + P_0)
inc_hour6 <- 
  inc_hour6_1 + inc_hour6_2 - inc_hour6_3 - inc_hour6_4
update(P_0) <- P_0 + inc_hour6*STEP_HOURS + NS_P_0*rnorm(0, 1)*SH

inc_hour7_1 <- V_1P*P_0 / (K_p + P_0)
inc_hour7_2 <- V_4P*P_2 / (K_p + P_2)
inc_hour7_3 <- k_d*P_1
inc_hour7_4 <- V_2P*P_1 / (K_p + P_1)
inc_hour7_5 <- V_3P*P_1 / (K_p + P_1)
inc_hour7 <- 
  inc_hour7_1 + inc_hour7_2 - inc_hour7_3 - inc_hour7_4 - inc_hour7_5
update(P_1) <- P_1 + inc_hour7*STEP_HOURS + NS_P_1*rnorm(0, 1)*SH

inc_hour8_1 <- V_3P*P_1 / (K_p + P_1)
inc_hour8_2 <- k_4*C
inc_hour8_3 <- k_d*P_2
inc_hour8_4 <- k_3*P_2*T_2
inc_hour8_5 <- V_4P*P_2 / (K_p + P_2)
inc_hour8_6 <- V_dP*P_2 / (K_dP + P_2)
inc_hour8 <- 
  inc_hour8_1 + inc_hour8_2 - inc_hour8_3 - inc_hour8_4 - inc_hour8_5 - inc_hour8_6
update(P_2) <- P_2 + inc_hour8*STEP_HOURS + NS_P_2*rnorm(0, 1)*SH

#   PER-TIM Complex
inc_hour9_1 <- k_3*P_2*T_2
inc_hour9_2 <- k_2*C_N
inc_hour9_3 <- (k_4*C + k_1*C + k_d*C)
inc_hour9 <- 
  inc_hour9_1 + inc_hour9_2 - inc_hour9_3
update(C) <- C + inc_hour9*STEP_HOURS + NS_C*rnorm(0, 1)*SH

inc_hour10_1 <- k_1*C
inc_hour10_2 <- k_2*C_N
inc_hour10_3 <- k_d*C_N
inc_hour10 <- 
  inc_hour10_1 - inc_hour10_2 - inc_hour10_3
update(C_N) <- C_N + inc_hour10*STEP_HOURS + NS_C_N*rnorm(0, 1)*SH

# Noise terms
#   When used in difference equation ("NS_..."), unit is 1/sqrt(hour)
#   When defined by user ("NoiseVariance_..."), unit is nM/hour
#     User provided variables are converted into molecule count, then sqrt -> sd
#   When user provides -1, scaled noise is used (see vignette).
NoiseVariance_M_T <- user(-1)
NoiseVariance_M_P <- user(0)
NoiseVariance_T_0 <- user(0)
NoiseVariance_T_1 <- user(0)
NoiseVariance_T_2 <- user(0)
NoiseVariance_P_0 <- user(0)
NoiseVariance_P_1 <- user(0)
NoiseVariance_P_2 <- user(0)
NoiseVariance_C <- user(0)
NoiseVariance_C_N <- user(0)

# Initial states
#   All states could be user defined (as nM in concentration unit)
#   step is the internal counting variable enabled by default
initial(M_T) <- setUserInitial_M_T * mulNM
initial(M_P) <- setUserInitial_M_P * mulNM
initial(T_0) <- setUserInitial_T_0 * mulNM
initial(T_1) <- setUserInitial_T_1 * mulNM
initial(T_2) <- setUserInitial_T_2 * mulNM
initial(P_0) <- setUserInitial_P_0 * mulNM
initial(P_1) <- setUserInitial_P_1 * mulNM
initial(P_2) <- setUserInitial_P_2 * mulNM
initial(C) <- setUserInitial_C * mulNM
initial(C_N) <- setUserInitial_C_N * mulNM

setUserInitial_M_T <- user(0)
setUserInitial_M_P <- user(0)
setUserInitial_T_0 <- user(0)
setUserInitial_T_1 <- user(0)
setUserInitial_T_2 <- user(0)
setUserInitial_P_0 <- user(0)
setUserInitial_P_1 <- user(0)
setUserInitial_P_2 <- user(0)
setUserInitial_C <- user(0)
setUserInitial_C_N <- user(0)

# Parameters, following '00 paper (nM, h units)
#   For noise modeling, convert concentration to molecule count unit.

#   Transcription to produce RNA
V_sP_conc <- user(1.1)
V_sT_conc <- user(1.0)
V_mP_conc <- user(1.0)
V_mT_conc <- user(0.7)
K_mP_conc <- user(0.2)
K_mT_conc <- user(0.2)
V_sP <-  V_sP_conc * mulNM
V_sT <-  V_sT_conc * mulNM
V_mP <-  V_mP_conc * mulNM
V_mT <-  V_mT_conc * mulNM
K_mP <-  K_mP_conc * mulNM
K_mT <-  K_mT_conc * mulNM

#   Inhibition of transcription, Hill
K_IP_conc <- user(1.0)
K_IT_conc <- user(1.0)
n <- user(4.0)
K_IP <-  K_IP_conc * mulNM
K_IT <-  K_IT_conc * mulNM

#   Translation (unit already h-1)
k_sP <- user(0.9)
k_sT <- user(0.9)

#   Protein phosphorylation multi-state and degradation
#     TIM protein degradation is modeled as an "inherent parameter"
#     See update(VdT)
V_1P_conc <- user(8.0)
V_1T_conc <- user(8.0)
V_2P_conc <- user(1.0)
V_2T_conc <- user(1.0)
V_3P_conc <- user(8.0)
V_3T_conc <- user(8.0)
V_4P_conc <- user(1.0)
V_4T_conc <- user(1.0)
K_p_conc <- user(2.0)
V_dP_conc <- user(2.2)
K_dP_conc <- user(0.2)
K_dT_conc <- user(0.2)
V_1P <- V_1P_conc * mulNM
V_1T <- V_1T_conc * mulNM
V_2P <- V_2P_conc * mulNM
V_2T <- V_2T_conc * mulNM
V_3P <- V_3P_conc * mulNM
V_3T <- V_3T_conc * mulNM
V_4P <- V_4P_conc * mulNM
V_4T <- V_4T_conc * mulNM
K_p <- K_p_conc * mulNM
V_dP <- V_dP_conc * mulNM
K_dP <- K_dP_conc * mulNM
K_dT <- K_dT_conc * mulNM

#   PER-TIM complex formation (unit already h-1 except k3)
k_1 <- user(0.8)
k_2 <- user(0.2)
k_3_conc <- user(1.2)
k_3 <- k_3_conc / mulNM
k_4 <- user(0.6)
#   Universal degradation (unit already h-1)
k_d <- user(0.01)
