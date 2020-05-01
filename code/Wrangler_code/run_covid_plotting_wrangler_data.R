##############################################################
### Code to call plotting functions for data on Wrangler
##############################################################

library(tidyverse)
#library(rtZIKVrisk)

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

## Plot the various figures
if(!dir.exists("figures/")){
  dir.create("figures/")
} # end if fig dir

# Re-define all the variables wanted for heat map of new detections after lever lift
r_not <- c(0.5)
init_num_infected = c(100, 1000, 10000)
increase_r_not = c(0, 0.2, 0.4, 0.6, 0.8, 1)
inf_plot_params = expand_grid(r_not, init_num_infected, increase_r_not)
num_runs=10000

list_of_lift_df=inf_plot_params %>%
  pmap(.f = get_save_path, num_reps = num_runs) %>%
  map(get_mean_after_lift_stats)
all_lift_df = bind_rows(list_of_lift_df, .id = "column_label")

plot_prob_R0_above_1(all_lift_df, init_num_infected)




