## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message=FALSE-----------------------------------------------------------
library(SPARSEMODr)
library(future.apply)
library(tidyverse)
library(viridis)
library(lubridate)

# To run in parallel, use, e.g., plan("multisession"):
future::plan("sequential")

## ---- fig.show='hold'---------------------------------------------------------
# Set seed for reproducibility
set.seed(5)

# Number of focal populations:
n_pop = 100

# Population sizes + areas
## Draw from neg binom:
census_area = rnbinom(n_pop, mu = 50, size = 3)

# Identification variable for later
pop_ID = c(1:n_pop)

# Assign coordinates, plot for reference
lat_temp = runif(n_pop, 32, 37)
long_temp = runif(n_pop, -114, -109)

# Storage:
region = rep(NA, n_pop)
pop_N = rep(NA, n_pop)

# Assign region ID and population size
for(i in 1 : n_pop){
  if ((lat_temp[i] >= 34.5) & (long_temp[i] <= -111.5)){
    region[i] = "1"
    pop_N[i] = rnbinom(1, mu = 50000, size = 2)
  } else if((lat_temp[i] >= 34.5) & (long_temp[i] > -111.5)){
    region[i] = "2"
    pop_N[i] = rnbinom(1, mu = 10000, size = 3)
  } else if((lat_temp[i] < 34.5) & (long_temp[i] > -111.5)){
    region[i] = "4"
    pop_N[i] = rnbinom(1, mu = 50000, size = 2)
  } else if((lat_temp[i] < 34.5) & (long_temp[i] <= -111.5)){
    region[i] = "3"
    pop_N[i] = rnbinom(1, mu = 10000, size = 3)
  } 
}

pop_local_df =
  data.frame(pop_ID = pop_ID,
             pop_N = pop_N,
             census_area,
             lat = lat_temp,
             long = long_temp,
             region = region) 

# Plot the map:
pop_plot = ggplot(pop_local_df) +
  geom_point(aes(x = long, y = lat, 
                 fill = region, size = pop_N),
             shape = 21) +
  scale_size(name = "Pop. Size", range = c(1,5), 
             breaks = c(5000, 50000, 150000)) +
  scale_fill_manual(name = "Region", values = c("#00AFBB", "#D16103",
                                                "#E69F00", "#4E84C4")) +
  geom_hline(yintercept = 34.5, colour = "#999999", linetype = 2) +
  geom_vline(xintercept = -111.5, colour = "#999999", linetype = 2) +
  guides(size = guide_legend(order = 2), 
         fill = guide_legend(order = 1, 
                             override.aes = list(size = 3))) +
  # Map coord
  coord_quickmap() +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.title = element_blank(),
    plot.margin = unit(c(0, 0.1, 0, 0), "cm")
  )

pop_plot

# Calculate pairwise dist
## in meters so divide by 1000 for km
dist_mat = geosphere::distm(cbind(pop_local_df$long, pop_local_df$lat))/1000
hist(dist_mat, xlab = "Distance (km)", main = "")

# We need to determine how many Exposed individuals
# are present at the start in each population
E_pops = vector("numeric", length = n_pop)
# We'll assume a total number of exposed across the
# full meta-community, and then randomly distribute these hosts
n_initial_E = 20
# (more exposed in larger populations)
these_E <- sample.int(n_pop,
                      size = n_initial_E,
                      replace = TRUE,
                      prob = pop_N)
for(i in 1:n_initial_E){
  E_pops[these_E[i]] <- E_pops[these_E[i]] + 1
}

pop_local_df$E_pops = E_pops


## ---- , fig.width=7-----------------------------------------------------------
# Set up the dates of change:
# 10 years of day identifiers:
n_years = 10
day_ID = rep(c(1:365), times = n_years)
date_seq = seq.Date(mdy("1-1-90"),
                    mdy("1-1-90") + length(day_ID) - 1,
                    by = "1 day")
# \beta peaks once every how many days?
t_mode = 365
# Sinusoidal forcing in \beta:
beta_base = 0.14
beta_seq = beta_base * (1 + cos((2*pi*day_ID)/t_mode))

# Data frame for plotting:
beta_seq_df = data.frame(beta_seq, date_seq)
date_breaks = seq(date_seq[1],
                  date_seq[1] + years(n_years),
                  by = "5 years")

ggplot(beta_seq_df) +
  geom_path(aes(x = date_seq, y = beta_seq)) +
  scale_x_date(breaks = date_breaks, date_labels = "%Y") +
  labs(x="", y=expression("Time-varying "*beta*", ("*beta[t]*")")) +
  # THEME
  theme_classic()+
  theme(
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  )


## -----------------------------------------------------------------------------
# Set up the time_windows() function
n_days = length(date_seq)

# Time-varying beta
changing_beta = vector("list", length = n_pop)
for (this_pop in 1:n_pop) {
    changing_beta[[this_pop]] <- beta_seq
}


# Migration rate
changing_m = rep(1/10.0, times = n_days)
# Migration range
changing_dist_phi = rep(100, times = n_days)
# Immigration (none)
changing_imm_frac = rep(0, times = n_days)

# Create the time_window() object
tw = time_windows(
  beta = changing_beta,
  m = changing_m,
  dist_phi = changing_dist_phi,
  imm_frac = changing_imm_frac,
  daily = date_seq
)

# Create the seir_control() object
seir_control = seir_control(
    input_N_pops = pop_N,
    input_E_pops = E_pops,
    birth = 1/(2*365),
    incubate = 1/5.0,
    recov = 1/20.0
)



## ---- message=FALSE, warning=FALSE--------------------------------------------
# How many realizations of the model?
n_realz = 30

# Need to assign a distinct seed for each realization
## Allows for reproducibility
input_realz_seeds = c(1:n_realz)

# Run the model in parallel

model_output =
  model_parallel(
      # Necessary inputs
      input_dist_mat = dist_mat,
      input_census_area = pop_local_df$census_area,
      input_tw = tw,
      input_realz_seeds = input_realz_seeds,
      # OTHER MODEL PARAMS
      trans_type = 1, # freq-dependent trans
      stoch_sd = 0.85,  # stoch transmission sd,
      control = seir_control    # data structure with seir-specific params
  )

glimpse(model_output)


## -----------------------------------------------------------------------------
# Grab the new events variables
pops_out_df =
  model_output %>%
  select(pops.seed:pops.R_pop)

# Simplify/clarify colnames
colnames(pops_out_df) = c("iter","pop_ID","time",
                            "S", "E", "I", "R")

# Join the region
region_df = pop_local_df %>% select(pop_ID, region)
pops_out_df =
  left_join(pops_out_df, region_df, by = "pop_ID")

# Join with dates (instead of "time" integer)
date_df = data.frame(
  date = date_seq,
  time = c(1:length(date_seq))
)
pops_out_df =
  left_join(pops_out_df, date_df, by = "time")

# Aggregate outcomes by region:
## First, get the sum across regions,dates,iterations
pops_sum_df =
  pops_out_df %>%
  group_by(region, iter, date) %>%
  summarize_all(sum)

glimpse(pops_sum_df)


## ---- fig.height=5, fig.width=7, fig.align='center'---------------------------

#######################
# PLOT
#######################
# region labels for facets:
region_labs = paste0("Region ",
                     sort(unique(region_df$region)))
names(region_labs) = sort(unique(region_df$region))

# Create an element list for plotting theme:
plot_base =
  list(
      labs(x = "", y = "Number Infectious"),
      theme_classic(),
      theme(
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5)
      )
  )


plot_allyears =  
  ggplot(pops_sum_df) +
  geom_path(aes(x = date, y = I, group = iter, color = region),
            alpha = 0.25) +
  # Colors per region:
  scale_color_manual(values = c("#00AFBB", "#D16103", 
                                "#E69F00", "#4E84C4")) +
  guides(color="none") +
  facet_wrap(~region, scales = "fixed", ncol = 2,
             labeller = labeller(region = region_labs)) +
  plot_base

plot_allyears


