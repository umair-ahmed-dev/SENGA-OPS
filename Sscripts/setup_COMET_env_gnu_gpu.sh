#!/bin/bash

# CRAY
export OPS_COMPILER=gnu
export OPS_INSTALL_PATH=$HOME/OPS/ops

module purge
module load CUDA/12.8.0 
module load OpenMPI/4.1.5-GCC-12.3.0
#module load GCC/11.3.0 

export NV_ARCH=Hopper

export CUDA_INSTALL_PATH=/opt/software/easybuild/software/CUDA/12.8.0/
export CUDA_MATH_LIBS=/opt/software/easybuild/software/CUDA/12.8.0/lib64/
export LD_LIBRARY_PATH=$CUDA_MATH_LIBS:$LD_LIBRARY_PATH

export MPI_INSTALL_PATH=/opt/software/easybuild/software/OpenMPI/4.1.5-GCC-12.3.0
export LD_LIBRARY_PATH=$MPI_INSTALL_PATH/lib:$LD_LIBRARY_PATH




export MPICC=mpicc
export MPICPP=mpicxx
export MPICXX=mpicxx
export MPIFC=mpif90
export MPIF90=mpif90

unset HDF5_INSTALL_PATH
export HDF5_INSTALL_PATH=/opt/software/manual/apps/hdf5/1.14.6
export LD_LIBRARY_PATH=$HDF5_INSTALL_PATH/lib:$LD_LIBRARY_PATH

source $OPS_INSTALL_PATH/../ops_translator/ops_venv/bin/activate

