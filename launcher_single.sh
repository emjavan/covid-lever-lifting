#!/bin/bash

#SBATCH -J covid-test          # Job name
#SBATCH -o covid-test.%j.o     # Name of stdout output file (%j expands to jobId)
#SBATCH -e covid-test.%j.e     # Name of stdout output file (%j expands to jobId)
#SBATCH -p normal              # Queue name
#SBATCH -N 1                   # Total number of nodes requested (24 cores/node)
#SBATCH -n 33                  # Total number of tasks to run in total
#SBATCH -t 24:00:00            # Run time (hh:mm:ss)

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
