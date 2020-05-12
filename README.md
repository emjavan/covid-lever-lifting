# COVID-19 Lever Lifting

## Run model with different parameter inputs
In launcher_cmds.txt list the commands to run on new lines and change the number of tasks in launcher_single.sh to match number of commands. Submit job with sbatch. The args input order is r_not, init_num_infected, increase_r_not, and num_runs.

 launcher_cmds.txt
```
Rscript --no-save code/run-covid-sims-only.R 0.5 100 0.1 100
```

launcher_single.sh
```
#!/bin/bash

#SBATCH -J covid-test          # Job name
#SBATCH -o covid-test.%j.o     # Name of stdout output file (%j expands to jobId)
#SBATCH -e covid-test.%j.e     # Name of stdout output file (%j expands to jobId)
#SBATCH -p normal              # Queue name
#SBATCH -N 1                   # Total number of nodes requested (24 cores/node)
#SBATCH -n 1                   # Total number of tasks to run (update with number lines)
#SBATCH -t 00:10:00            # Run time (hh:mm:ss)

# Load launcher
module load launcher

# Configure launcher
EXECUTABLE=$TACC_LAUNCHER_DIR/init_launcher
PRUN=$TACC_LAUNCHER_DIR/paramrun
CONTROL_FILE=launcher_cmds.txt
export LAUNCHER_JOB_FILE=launcher_cmds.txt
export LAUNCHER_WORKDIR=`pwd`
export LAUNCHER_SCHED=interleaved

# Start launcher
$PRUN $EXECUTABLE $CONTROL_FILE
```

## County opening data variables
fips = unique indentifier for each state and county
CTYNAME = name of county, parish, borough, etc.
POPESTIMATE2019 = population estimate from census 
Reopening_date_true = day social distancing was/will be released in some way accoring to NYT and CNN
    https://www.nytimes.com/interactive/2020/us/states-reopen-map-coronavirus.html
    https://www.cnn.com/interactive/2020/us/states-reopen-coronavirus-trnd/
Reopening_date_10day = false re-opening date 10 days in past from Today_date
Reopening_date = the true reopening date or a 10-day window of time 
Reop_diff_from_true_date = is the true reopening date different from the one used, 1=yes, 0=no
Order_for_state_prior  = was a stay at home order in place prior to re-opening, 1=yes, 0=no
Order_for_state_date  = date of stay at home order if it exists, NA otherwise
Comment = any notes on if individual county reopening date differs from state
Today_date = date of last update with COVID-19 case data, usually yesterday's date    
Days_after_lift = days since re-opening or the 10-day window

