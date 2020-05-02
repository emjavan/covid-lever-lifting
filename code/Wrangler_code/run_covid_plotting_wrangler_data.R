##############################################################
### Code to call plotting functions for data on Wrangler
##############################################################

library(tidyverse)

# load file with get_save_path function
an.error.occured1 <- FALSE 
tryCatch({result <- file.exists("code/sim-covid-outbreaks-lever-lift.R"); print(result)},
          warning = function(w) { print("Can't find the file you're looking for") },
          error = function(e) {an.error.occured1 <<- TRUE})
stopifnot(an.error.occured1==FALSE)
source("code/sim-covid-outbreaks-lever-lift.R")

# load function with all the plotting functions
an.error.occured2 <- FALSE 
tryCatch({result <- file.exists("code/Wrangler_code/covid_plotting_wrangler_data.R"); print(result)},
          warning = function(w) { print("Can't find the file you're looking for") },
          error = function(e) {an.error.occured2 <<- TRUE})
stopifnot(an.error.occured2==FALSE)
source("code/Wrangler_code/covid_plotting_wrangler_data.R")

## Make new figures dir not in git.ignore to move images off wrangler to local desktop
if(!dir.exists("wrangler_figures/")){
  dir.create("wrangler_figures/")
} # end if fig dir

# Re-define all the variables wanted for heat map of new detections after lever lift
r_not <- c(0.5)
#init_num_infected = c(100, 1000, 10000)
increase_r_not = c(0, 0.2, 0.4, 0.6, 0.8, 1)
increase_r_not = c(0, 1)
inf_plot_params = expand_grid(r_not, init_num_infected, increase_r_not)
num_runs=10000

# Get the prob R0>1 by doing point comparison of new detections with day of lift new detections
list_of_lift_df_pt=inf_plot_params %>%
  pmap(.f = get_save_path, num_reps = num_runs) %>%
  map(get_mean_after_lift_stats_pt_compare )
all_lift_df_pt = bind_rows(list_of_lift_df_pt, .id = "column_label")
plot_prob_R0_above_1(all_lift_df_pt, init_num_infected, pt_or_mean = "pt")

plot_prob_R0_above_1_increase_by(all_lift_df_pt, input_init_inf=100, pt_or_mean = "pt")
plot_prob_R0_above_1_increase_by(all_lift_df_pt, input_init_inf=1000, pt_or_mean = "pt")
plot_prob_R0_above_1_increase_by(all_lift_df_pt, input_init_inf=10000, pt_or_mean = "pt")

# Get the prob R0>1 by doing mean of new detections on subsequent days in comparison with day of lift new detections
list_of_lift_df_mean=inf_plot_params %>%
  pmap(.f = get_save_path, num_reps = num_runs) %>%
  map(get_mean_after_lift_stats_mean_compare)
all_lift_df_mean = bind_rows(list_of_lift_df_mean, .id = "column_label")
plot_prob_R0_above_1(all_lift_df_mean, init_num_infected, pt_or_mean = "mean")

plot_prob_R0_above_1_increase_by(all_lift_df_mean, input_init_inf=100, pt_or_mean = "mean")
plot_prob_R0_above_1_increase_by(all_lift_df_mean, input_init_inf=1000, pt_or_mean = "mean")
plot_prob_R0_above_1_increase_by(all_lift_df_mean, input_init_inf=10000, pt_or_mean = "mean")










