## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(tidyverse)
library(viridis)
library(lubridate)

## ---- fig.width=5-------------------------------------------------------------
# Set up the dates of change. 5 time windows
n_windows = 5
# Window intervals
start_dates = c(mdy("1-1-20"),  mdy("2-1-20"),  mdy("2-16-20"), mdy("3-11-20"),  mdy("3-22-20"))
end_dates =   c(mdy("1-31-20"), mdy("2-15-20"), mdy("3-10-20"), mdy("3-21-20"), mdy("5-1-20"))

# Date sequence
date_seq = seq.Date(start_dates[1], end_dates[n_windows], by = "1 day")

# Time-varying beta
changing_beta = c(0.3,            0.1,            0.1,            0.15,            0.15)

#beta sequence
beta_seq = NULL

beta_seq[1:(yday(end_dates[1]) - yday(start_dates[1]) + 1)] =
  changing_beta[1]

for(i in 2:n_windows){

  beta_temp_seq = NULL
  beta_temp = NULL

  if(changing_beta[i] != changing_beta[i-1]){

    beta_diff = changing_beta[i-1] - changing_beta[i]
    n_days = yday(end_dates[i]) - yday(start_dates[i]) + 1
    beta_slope = - beta_diff / n_days

    for(j in 1:n_days){
      beta_temp_seq[j] = changing_beta[i-1] + beta_slope*j
    }

  }else{
    n_days = yday(end_dates[i]) - yday(start_dates[i]) + 1
    beta_temp_seq = rep(changing_beta[i], times = n_days)
  }

  beta_seq = c(beta_seq, beta_temp_seq)

}

beta_seq_df = data.frame(beta_seq, date_seq)
date_breaks = seq(range(date_seq)[1],
                  range(date_seq)[2],
                  by = "1 month")


ggplot(beta_seq_df) +
  geom_path(aes(x = date_seq, y = beta_seq)) +
  scale_x_date(breaks = date_breaks, date_labels = "%b") +
  labs(x="", y=expression("Time-varying "*beta*", ("*beta[t]*")")) +
  # THEME
  theme_classic()+
  theme(
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  )



## ---- fig.width=5, echo=FALSE-------------------------------------------------

# Distance between populations:
dist_temp = seq(0, 300, length.out = 200)
dist_phi = c(50, 100, 200)

p_move_func = function(dist_phi, distance){
  1 / (exp( distance / dist_phi ))
}

p_move_mat = sapply(dist_phi,
                    p_move_func, distance = dist_temp)
p_move_df =
  data.frame(dist_ij = dist_temp, p_move_mat) %>%
  pivot_longer(X1:X3, values_to = "p_ij", names_to = "dp_val") %>%
  mutate(dp_val = case_when(
    dp_val == "X1" ~ "50",
    dp_val == "X2" ~ "100",
    dp_val == "X3" ~ "200"
  ))

ggplot(p_move_df) +
  geom_path(aes(x = dist_ij, y = p_ij,
                color = dp_val, group = dp_val)) +
  labs(x = "Distance between pops (km)",
       y = "Probability of migration") +
  scale_color_viridis_d(name = expression(phi),
                        breaks = c("50", "100", "200"),
                        direction = -1) +
  theme_classic() +
  theme(
    axis.title = element_text(color = "black", size = 12),
    axis.text = element_text(color = "black", size = 11),
    legend.position = c(0.7,0.7)
  )




## ---- fig.width=5, echo=FALSE-------------------------------------------------

# Distance between populations:
# Units hosts / km2
dens_temp = seq(0, 3000, length.out = 200)
monod_k = c(100, 500, 1000)
beta_max = 2.0

beta_dd_func = function(monod_k, dens_temp, beta_max){
  beta_max * dens_temp / (monod_k + dens_temp)
}

beta_dd_mat = sapply(monod_k,
                    beta_dd_func, dens_temp, beta_max)
beta_dd_df =
  data.frame(dens = dens_temp, beta_dd_mat) %>%
  pivot_longer(X1:X3, values_to = "beta_realz", names_to = "monod_K") %>%
  mutate(monod_K = case_when(
    monod_K == "X1" ~ "100",
    monod_K == "X2" ~ "500",
    monod_K == "X3" ~ "1000"
  ))

ggplot(beta_dd_df) +
  geom_path(aes(x = dens, y = beta_realz,
                color = monod_K, group = monod_K)) +
  labs(x = expression("Host density ("~km^-2~")"),
       y = expression("Transmission,"~beta["realized"])) +
  scale_color_viridis_d(name = "Monod_K",
                        breaks = c("100", "500", "1000"),
                        direction = -1) +
  theme_classic() +
  theme(
    axis.title = element_text(color = "black", size = 12),
    axis.text = element_text(color = "black", size = 11),
    legend.position = c(0.7,0.3)
  )





