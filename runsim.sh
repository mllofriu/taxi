#!/bin/bash

#SBATCH -n 200
#SBATCH --time 01:00:00

mpirun -np 1 Rscript simulation.R
