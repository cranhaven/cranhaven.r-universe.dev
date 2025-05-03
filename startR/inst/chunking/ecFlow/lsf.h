#!/bin/bash

#BSUB -n %CORES_PER_JOB%
#BSUB -W %JOB_WALLCLOCK%
#BSUB -J %ECF_NAME%
#BSUB -eo %REMOTE_ECF_HOME%/%ECF_NAME%.%ECF_TRYNO%.err
#BSUB -oo %REMOTE_ECF_HOME%/%ECF_NAME%.%ECF_TRYNO%.out
include_extra_queue_params

