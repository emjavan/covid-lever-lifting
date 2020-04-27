# COVID-19 Lever Lifting

## Run model with different parameter inputs
In launcher_cmds.txt list the commands to run on new lines and change the number of tasks in sbatch launcher_single.sh to match number of commands. The args input order is r_not, init_num_infected, increase_r_not, and num_runs.

```
Rscript --no-save code/run-covid-sims-only.R 0.5 100 0.1 100
```

