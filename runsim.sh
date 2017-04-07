#!/bin/bash

#SBATCH --time 01:00:00

Rscript simulation.R $SLURM_ARRAY_TASK_ID
