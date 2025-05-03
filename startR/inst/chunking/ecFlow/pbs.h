#!/bin/bash

##SBATCH -n %CORES_PER_JOB%
#PBS -l walltime=%JOB_WALLCLOCK%
##SBATCH -J %ECF_NAME%
#PBS -e %REMOTE_ECF_HOME%/%ECF_NAME%.%ECF_TRYNO%.err
#PBS -o %REMOTE_ECF_HOME%/%ECF_NAME%.%ECF_TRYNO%.out
include_extra_queue_params

