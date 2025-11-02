# Leloup and Goldbeter model
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

# Time step in hours
STEP_HOURS <- user(0.01)

# LD cycle parameters
LD_HOURS <- user(720)
VdT_ON <- user(6.0)
VdT_OFF <- user(3.0)

# Core equations - discrete step model

#   TIM protein degradation rate update
current_hour <- step * STEP_HOURS
V_dT <- 
  if (current_hour >= LD_HOURS || ((current_hour %/% 12) %% 2 == 1)) VdT_OFF else VdT_ON

#   mRNA of TIM and PER
inc_hour1 <- 
  V_sT*K_IT^n / (K_IT^n + C_N^n) - 
  k_d*M_T -
  V_mT*M_T / (K_mT + M_T)
update(M_T) <- M_T + inc_hour1*STEP_HOURS

inc_hour2 <- 
  V_sP*K_IP^n / (K_IP^n + C_N^n) - 
  k_d*M_P -
  V_mP*M_P / (K_mP + M_P)
update(M_P) <- M_P + inc_hour2*STEP_HOURS

#   Protein of TIM
inc_hour3 <- 
  k_sT*M_T + 
  V_2T*T_1 / (K_p + T_1) - 
  k_d*T_0 - 
  V_1T*T_0 / (K_p + T_0)
update(T_0) <- T_0 + inc_hour3*STEP_HOURS

inc_hour4 <- 
  V_1T*T_0 / (K_p + T_0) + 
  V_4T*T_2 / (K_p + T_2) -
  k_d*T_1 - 
  V_2T*T_1 / (K_p + T_1) - 
  V_3T*T_1 / (K_p + T_1)
update(T_1) <- T_1 + inc_hour4*STEP_HOURS

inc_hour5 <- 
  V_3T*T_1 / (K_p + T_1) + 
  k_4*C -
  k_d*T_2 - 
  k_3*P_2*T_2 - 
  V_4T*T_2 / (K_p + T_2) - 
  V_dT*T_2 / (K_dT + T_2)
update(T_2) <- T_2 + inc_hour5*STEP_HOURS

#   Protein of PER
inc_hour6 <- 
  k_sP*M_P + 
  V_2P*P_1 / (K_p + P_1) - 
  k_d*P_0 - 
  V_1P*P_0 / (K_p + P_0)
update(P_0) <- P_0 + inc_hour6*STEP_HOURS

inc_hour7 <- 
  V_1P*P_0 / (K_p + P_0) + 
  V_4P*P_2 / (K_p + P_2) -
  k_d*P_1 - 
  V_2P*P_1 / (K_p + P_1) - 
  V_3P*P_1 / (K_p + P_1)
update(P_1) <- P_1 + inc_hour7*STEP_HOURS

inc_hour8 <- 
  V_3P*P_1 / (K_p + P_1) + 
  k_4*C -
  k_d*P_2 - 
  k_3*P_2*T_2 - 
  V_4P*P_2 / (K_p + P_2) - 
  V_dP*P_2 / (K_dP + P_2)
update(P_2) <- P_2 + inc_hour8*STEP_HOURS

#   PER-TIM Complex
inc_hour9 <- 
  k_3*P_2*T_2 + 
  k_2*C_N -
  (k_4*C + k_1*C + k_d*C)
update(C) <- C + inc_hour9*STEP_HOURS

inc_hour10 <- 
  k_1*C - 
  k_2*C_N - 
  k_d*C_N
update(C_N) <- C_N + inc_hour10*STEP_HOURS

# Initial states
#   All states could be user defined.
#   step is the internal counting variable enabled by default
initial(M_T) <- setUserInitial_M_T
initial(M_P) <- setUserInitial_M_P
initial(T_0) <- setUserInitial_T_0
initial(T_1) <- setUserInitial_T_1
initial(T_2) <- setUserInitial_T_2
initial(P_0) <- setUserInitial_P_0
initial(P_1) <- setUserInitial_P_1
initial(P_2) <- setUserInitial_P_2
initial(C) <- setUserInitial_C
initial(C_N) <- setUserInitial_C_N

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

# Parameters, following '00 paper
V_sP <- user(1.1)
V_sT <- user(1.0)
V_mP <- user(1.0)
V_mT <- user(0.7)
K_mP <- user(0.2)
K_mT <- user(0.2)
#   Inhibition of transcription, Hill
K_IP <- user(1.0)
K_IT <- user(1.0)
n <- user(4.0)
#   Translation
k_sP <- user(0.9)
k_sT <- user(0.9)
#   Protein phosphorylation multi-state and degradation
#     TIM protein degradation is modeled as an "inherent parameter"
#     See update(VdT)
V_1P <- user(8.0)
V_1T <- user(8.0)
V_2P <- user(1.0)
V_2T <- user(1.0)
V_3P <- user(8.0)
V_3T <- user(8.0)
V_4P <- user(1.0)
V_4T <- user(1.0)
K_p <- user(2.0)
V_dP <- user(2.2)
K_dP <- user(0.2)
K_dT <- user(0.2)
#   PER-TIM complex formation
k_1 <- user(0.8)
k_2 <- user(0.2)
k_3 <- user(1.2)
k_4 <- user(0.6)
#   Universal degradation
k_d <- user(0.01)
