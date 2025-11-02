# Leloup and Goldbeter model
#   1998 DOI: https://doi.org/10.1177/074873098128999934
#   2000 DOI: https://doi.org/10.1002/(sici)1521-1878(200001)22:1%3C84::aid-bies13%3E3.0.co;2-i

# LD cycle parameters
LD_HOURS <- user(720)
VdT_ON <- user(6.0)
VdT_OFF <- user(3.0)

# Core equations - ODE derivative update model

#   TIM protein degradation rate update
current_hour <- t
V_dT <-
  if (current_hour >= LD_HOURS || ((current_hour %/% 12) %% 2 == 1)) VdT_OFF else VdT_ON

#   mRNA of TIM and PER
deriv(M_T) <- 
  V_sT*K_IT^n / (K_IT^n + C_N^n) - 
  k_d*M_T -
  V_mT*M_T / (K_mT + M_T)

deriv(M_P) <- 
  V_sP*K_IP^n / (K_IP^n + C_N^n) - 
  k_d*M_P -
  V_mP*M_P / (K_mP + M_P)

#   Protein of TIM
deriv(T_0) <- 
  k_sT*M_T + 
  V_2T*T_1 / (K_p + T_1) - 
  k_d*T_0 - 
  V_1T*T_0 / (K_p + T_0)

deriv(T_1) <- 
  V_1T*T_0 / (K_p + T_0) + 
  V_4T*T_2 / (K_p + T_2) -
  k_d*T_1 - 
  V_2T*T_1 / (K_p + T_1) - 
  V_3T*T_1 / (K_p + T_1)

deriv(T_2) <- 
  V_3T*T_1 / (K_p + T_1) + 
  k_4*C -
  k_d*T_2 - 
  k_3*P_2*T_2 - 
  V_4T*T_2 / (K_p + T_2) - 
  V_dT*T_2 / (K_dT + T_2)

#   Protein of PER
deriv(P_0) <- 
  k_sP*M_P + 
  V_2P*P_1 / (K_p + P_1) - 
  k_d*P_0 - 
  V_1P*P_0 / (K_p + P_0)

deriv(P_1) <- 
  V_1P*P_0 / (K_p + P_0) + 
  V_4P*P_2 / (K_p + P_2) -
  k_d*P_1 - 
  V_2P*P_1 / (K_p + P_1) - 
  V_3P*P_1 / (K_p + P_1)

deriv(P_2) <- 
  V_3P*P_1 / (K_p + P_1) + 
  k_4*C -
  k_d*P_2 - 
  k_3*P_2*T_2 - 
  V_4P*P_2 / (K_p + P_2) - 
  V_dP*P_2 / (K_dP + P_2)

#   PER-TIM Complex
deriv(C) <- 
  k_3*P_2*T_2 + 
  k_2*C_N -
  (k_4*C + k_1*C + k_d*C)

deriv(C_N) <- 
  k_1*C - 
  k_2*C_N - 
  k_d*C_N

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
