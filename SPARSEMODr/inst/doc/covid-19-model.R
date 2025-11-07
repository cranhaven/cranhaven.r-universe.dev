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


## ---- , fig.width=5, echo=FALSE-----------------------------------------------
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


## -----------------------------------------------------------------------------
# Set up the dates of change. 5 time windows
n_windows = 5
## Specify the start and end dates of the time intervals
start_dates = c(mdy("1-1-20"),  mdy("2-1-20"),  mdy("2-16-20"), mdy("3-11-20"), mdy("3-22-20"))
end_dates =   c(mdy("1-31-20"), mdy("2-15-20"), mdy("3-10-20"), mdy("3-21-20"), mdy("5-1-20"))

### TIME-VARYING PARAMETERS ###

# beta pattern per region
region_beta = list(
    "1"=c(0.30, 0.10, 0.10, 0.15, 0.15),
    "2"=c(0.30, 0.08, 0.08, 0.10, 0.10),
    "3"=c(0.30, 0.12, 0.12, 0.19, 0.19),
    "4"=c(0.30, 0.03, 0.03, 0.12, 0.12)
)

## Assign the appropriate, regional pattern of beta
## to each population
changing_beta = vector("list", length = n_pop)
for (this_pop in 1:n_pop) {
    this_region <- pop_local_df$region[this_pop]
    changing_beta[[this_pop]] <- region_beta[[this_region]]
}

# Migration rate
changing_m = rep(1/10.0, times = n_windows)
# Migration range
changing_dist_phi = rep(150, times = n_windows)
# Immigration (none)
changing_imm_frac = rep(0, times = n_windows)

# Create the time_window() object
tw = time_windows(
  beta = changing_beta,
  m = changing_m,
  dist_phi = changing_dist_phi,
  imm_frac = changing_imm_frac,

  start_dates = start_dates,
  end_dates = end_dates
)

# Create the covid19_control() object
covid19_control <- covid19_control(input_N_pops = pop_N,
                                   input_E_pops = E_pops)

# Date Sequence for later:
date_seq = seq.Date(start_dates[1], end_dates[n_windows], by = "1 day")


## -----------------------------------------------------------------------------
# How many realizations of the model?
n_realz = 75

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
      control = covid19_control,
      # OTHER MODEL PARAMS
      trans_type = 1, # freq-dependent trans
      stoch_sd = 2.0  # stoch transmission sd
  )

glimpse(model_output)


## -----------------------------------------------------------------------------
# Grab the new events variables
new_events_df =
  model_output %>%
  select(pops.seed:pops.time, events.pos:events.n_death)

# Simplify/clarify colnames
colnames(new_events_df) = c("iter","pop_ID","time",
                            "new_pos", "new_sym", "new_hosp",
                            "new_icu", "new_death")
# Join the region
region_df = pop_local_df %>% select(pop_ID, region)
new_events_df =
  left_join(new_events_df, region_df, by = "pop_ID")

# Join with dates (instead of "time" integer)
date_df = data.frame(
  date = date_seq,
  time = c(1:length(date_seq))
)
new_events_df =
  left_join(new_events_df, date_df, by = "time")

# Aggregate outcomes by region:
## First, get the sum across regions,dates,iterations
new_event_sum_df =
  new_events_df %>%
  group_by(region, iter, date) %>%
  summarize(new_pos = sum(new_pos),
            new_sym = sum(new_sym),
            new_hosp = sum(new_hosp),
            new_icu = sum(new_icu),
            new_death = sum(new_death))
glimpse(new_event_sum_df)

# Now calculate the median model trajectory across the realizations
new_event_median_df =
  new_event_sum_df %>%
  ungroup() %>%
  group_by(region, date) %>%
  summarize(med_new_pos = median(new_pos),
            med_new_sym = median(new_sym),
            med_new_hosp = median(new_hosp),
            med_new_icu = median(new_icu),
            med_new_death = median(new_death))
glimpse(new_event_median_df)


## ---- fig.height=3.7, fig.width=7, fig.align='center'-------------------------
# SET UP SOME THEMATIC ELEMENTS:
## Maximum value of the stoch trajectories, for y axis ranges
max_hosp = max(new_event_sum_df$new_hosp)
## Breaks for dates:
date_breaks = seq(range(date_seq)[1],
                  range(date_seq)[2],
                  by = "1 month")

#######################
# PLOT
#######################

# First we'll create an element list for plotting:
plot_new_hosp_base =
  list(
      # Date Range:
      scale_x_date(limits = range(date_seq),
                   breaks = date_breaks, date_labels = "%b"),
      # New Hosp Range:
      scale_y_continuous(limits = c(0, max_hosp*1.05)),
      # BOXES AND TEXT TO LABEL TIME WINDOWS
      annotate("rect", xmin = start_dates[1], xmax = end_dates[1],
               ymin = 0, ymax = max_hosp*1.05,
               fill = "gray", alpha = 0.2),
      annotate("rect", xmin = start_dates[3], xmax = end_dates[3],
               ymin = 0, ymax = max_hosp*1.05,
               fill = "gray", alpha = 0.2),
      annotate("rect", xmin = start_dates[5], xmax = end_dates[5],
               ymin = 0, ymax = max_hosp*1.05,
               fill = "gray", alpha = 0.2),
      # THEME ELEMENTS
      labs(x = "", y = "New Hospitalizations Per Day"),
      theme_classic(),
      theme(
        axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5)
      )
  )


ggplot() + plot_new_hosp_base



## ---- fig.height=5, fig.width=7, fig.align='center'---------------------------

# region labels for facets:
region_labs = paste0("Region ",
                     sort(unique(region_df$region)))
names(region_labs) = sort(unique(region_df$region))

# Regional beta labels
region_beta_df = data.frame(
  beta_lab = paste0("beta = ",format(unlist(lapply(region_beta, function(x){x[c(1,3,5)]})),nsmall = 1)),
  region = as.character(rep(c(1:4), each=3)),
  date = rep(start_dates[c(1,3,5)],times=4),
  new_hosp = max_hosp*1.05
)
  

# Create the plot:
plot_new_hosp =
  ggplot() +
  # Facet by Region
  facet_wrap(~region,
             scales = "free",
             labeller = labeller(region = region_labs)) +
  # Add our base thematics
  plot_new_hosp_base +
  # Add the stoch trajectories:
  geom_path(data = new_event_sum_df,
            aes(x = date, y = new_hosp, group = iter, color = region),
            alpha = 0.05) +
  # Add the median trajectory:
  geom_path(data = new_event_median_df,
            aes(x = date, y = med_new_hosp, color = region),
            size = 2) +
  # Add the beta labels:
  geom_text(data = region_beta_df,
            aes(x = date, y = new_hosp, label = beta_lab),
            color = "#39558CFF", hjust = 0, vjust = 1, size = 3.0) +
  # Colors per region:
  scale_color_manual(values = c("#00AFBB", "#D16103", 
                                "#E69F00", "#4E84C4")) +
  guides(color="none")


plot_new_hosp


