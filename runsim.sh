#!/bin/bash

#SBATCH -n 100
#SBATCH --time 08:00:00

mpirun -np 1 Rscript simulation.R
