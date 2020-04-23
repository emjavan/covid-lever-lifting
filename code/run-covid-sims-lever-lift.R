##############################################################
## Code for running and saving all COVID simulations
##############################################################
library(tidyverse)
#library(rtZIKVrisk)

an.error.occured1 <- FALSE 
tryCatch( { result <- file.exists("code/sim-covid-outbreaks-lever-lift.R"); print(result) },
          warning = function(w) { print("Can't find the file you're looking for") },
          error = function(e) {an.error.occured1 <<- TRUE})
stopifnot(an.error.occured1==FALSE)
source("code/sim-covid-outbreaks-lever-lift.R")


an.error.occured2 <- FALSE 
tryCatch( { result <- file.exists("code/covid-plotting-fxns-lever-lift.R"); print(result) },
          warning = function(w) { print("Can't find the file you're looking for") },
          error = function(e) {an.error.occured2 <<- TRUE})
stopifnot(an.error.occured2==FALSE)
source("code/covid-plotting-fxns-lever-lift.R")


## Setup all of the parameters that need to be run
r_not <- c(0.5, 0.7, 0.9)
init_num_infected = c(100, 1000, 10000)
increase_r_not = c(0, 0.2, 0.5, 1)
num_runs=10000
run_df <- expand_grid(r_not, init_num_infected, increase_r_not)

## Run and save simulations across all parameter combinations
if(!dir.exists("processed_data/")){
  dir.create("processed_data/")
}
run_df %>% # This takes a long time, need to move to TACC batch jobs
  pmap(.f = save_covid_runs, num_reps = num_runs) %>% 
  unlist()


## Plot the various figures
if(!dir.exists("figures/")){
  dir.create("figures/")
}

## Heat map
end_day_list=run_df %>% # This does not take very long, but isn't instant
  pmap(.f = get_save_path, num_reps = num_runs) %>%
  map(get_cum_inf_lift )
df = bind_rows(end_day_list, .id = "column_label")
heat_map(df, init_num_infected)


# Re-define all the variables wanted for plots of infectect and detected through time
r_not <- c(0.5)
init_num_infected = c(100, 1000, 10000)
increase_r_not = c(0, 0.2, 0.3, 0.4)
inf_plot_params = expand_grid(r_not, init_num_infected, increase_r_not)
num_runs=10000

list_of_df=inf_plot_params %>%
  pmap(.f = get_save_path, num_reps = num_runs) %>%
  map(get_total_inf_R0_less_1)
all_df = bind_rows(list_of_df, .id = "column_label")

# full df is subset in function and plots both inf and det 
inf_end_day_vect100   = plot_through_time(df = all_df, R0 = 0.5, init_infected = 100,   increase = increase_r_not)
inf_end_day_vect1000  = plot_through_time(df = all_df, R0 = 0.5, init_infected = 1000,  increase = increase_r_not)
inf_end_day_vect10000 = plot_through_time(df = all_df, R0 = 0.5, init_infected = 10000, increase = increase_r_not)

# functions still need error checking for when the .rda doesn't exist, etc.


