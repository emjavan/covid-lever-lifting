##############################################################
## Code for running and saving all COVID simulations
##############################################################
library(tidyverse)
#library(rtZIKVrisk)

an.error.occured1 <- FALSE 
tryCatch( { result <- file.exists("code/sim-covid-outbreaks-through-time.R"); print(result) },
          warning = function(w) { print("Can't find the file you're looking for") },
          error = function(e) {an.error.occured1 <<- TRUE})
stopifnot(an.error.occured1==FALSE)
source("code/sim-covid-outbreaks-through-time.R")


an.error.occured2 <- FALSE 
tryCatch( { result <- file.exists("code/covid-plotting-fxns-time.R"); print(result) },
          warning = function(w) { print("Can't find the file you're looking for") },
          error = function(e) {an.error.occured2 <<- TRUE})
stopifnot(an.error.occured2==FALSE)
source("code/covid-plotting-fxns-time.R")


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
run_df %>% 
  pmap(.f = save_covid_runs, num_reps = num_runs) %>% 
  unlist()

# ## Get total infections through time into csv files
# run_df %>%
#   pmap(.f = get_save_path, num_reps = num_runs) %>%
#   map(get_total_inf_data)

## Plot the various figures
if(!dir.exists("figures/")){
  dir.create("figures/")
}

## Heat map
end_day_list=run_df %>%
  pmap(.f = get_save_path, num_reps = num_runs) %>%
  map(get_cum_inf_lift )
df <- data.frame(matrix(unlist(end_day_list), nrow=length(end_day_list), byrow=T)) 
names(df) = 
    c("Init_Infected",                   "Init_R0",                           "R0_increase",
    "Mean_Days_from_peak_to_lift",       "CI_lower_Days_from_peak_to_lift",   "CI_upper_Days_from_peak_to_lift",  
    "StdError_Days_from_peak_to_lift",   "Mean_Lift_day",                     "CI_lower_Lift_day",               
    "CI_upper_Lift_day",                 "StdError_Lift_day",                 "Mean_Cum_inf_lift_day",        
    "CI_lower_Cum_inf_lift_day",         "CI_upper_Cum_inf_lift_day",         "StdError_Cum_inf_lift_day",     
    "Mean_Cum_inf_7d_after_lift",        "CI_lower_Cum_inf_7d_after_lift",    "CI_upper_Cum_inf_7d_after_lift",   
    "StdError_Cum_inf_7d_after_lift",    "Mean_Cum_inf_14d_after_lift",       "CI_lower_Cum_inf_14d_after_lift",  
    "CI_upper_Cum_inf_14d_after_lift",   "StdError_Cum_inf_14d_after_lift",   "Mean_Cum_inf_30d_after_lift",  
    "CI_lower_Cum_inf_30d_after_lift",   "CI_upper_Cum_inf_30d_after_lift",   "StdError_Cum_inf_30d_after_lift")
heat_map(df, init_num_infected)











