#!/bin/bash

#SBATCH --job-name=senga-ops
#SBATCH --partition=gpu-l_paid
#SBATCH --output=logs/%A_%a.out
#SBATCH --time=12:05:00
#SBATCH --nodes=1
#SBATCH --cpus-per-task=4
#SBATCH --gres=gpu:H100:4
#SBATCH --exclusive

source setup_env_comet_gnu_gpu_ops.sh
module list
date
mpirun -np 4  ./senga2 -OPS_DIAGS=2 OPS_FORCE_DECOMP_X=4 OPS_FORCE_DECOMP_Y=1 OPS_FORCE_DECOMP_Z=1 2>&1 | tee log_4gpus_gnu_ops.txt
date
