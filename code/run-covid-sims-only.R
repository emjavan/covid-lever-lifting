##############################################################
## Code for running and saving all COVID simulations
##############################################################
library(tidyverse)
#library(rtZIKVrisk)
print("library found")

an.error.occured1 <- FALSE 
tryCatch( { result <- file.exists("code/sim-covid-outbreaks-lever-lift.R"); print(result) },
          warning = function(w) { print("Can't find the file you're looking for") },
          error = function(e) {an.error.occured1 <<- TRUE})
stopifnot(an.error.occured1==FALSE)
source("code/sim-covid-outbreaks-lever-lift.R")
print("sim file found/added")

an.error.occured2 <- FALSE 
tryCatch( { result <- file.exists("code/covid-plotting-fxns-lever-lift.R"); print(result) },
          warning = function(w) { print("Can't find the file you're looking for") },
          error = function(e) {an.error.occured2 <<- TRUE})
stopifnot(an.error.occured2==FALSE)
source("code/covid-plotting-fxns-lever-lift.R")
print("plot file found/added")


args<-commandArgs(TRUE)
# need to add error checking

## Setup all of the parameters that need to be run
r_not <- as.double(args[1]) #c(0.1)
init_num_infected = as.double(args[2]) #c(100)
increase_r_not = as.double(args[3]) #c(1)
num_runs= as.double(args[4]) #10
run_df <- expand_grid(r_not, init_num_infected, increase_r_not)

## Run and save simulations across all parameter combinations
if(!dir.exists("processed_data/")){
  dir.create("processed_data/")
}
run_df %>% # This takes a long time, need to move to TACC batch jobs
  pmap(.f = save_covid_runs, num_reps = num_runs) %>% 
  unlist()

