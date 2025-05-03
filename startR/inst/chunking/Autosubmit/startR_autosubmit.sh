#!/bin/bash

######## AUTOSUBMIT INPUTS #######
proj_dir=%PROJDIR%
task_path=%DEFAULT.EXPID% 
chunknum=%JOBNAME%  #e.g., a68h_CHUNK_0
chunknum="${chunknum:5}"  #e.g., CHUNK_0
##################################

# Modified by write_bash.R
# e.g., chunk_args=(%JOBS."${chunknum}".dat% %JOBS."${chunknum}".dat_N% %JOBS."${chunknum}".var% %JOBS."${chunknum}".var_N% %JOBS."${chunknum}".sdate% %JOBS."${chunknum}".sdate_N%)
chunk_args=

include_init_commands
include_module_load

##Should move to the path that has load_process_save_chunk_autosubmit.R
#cd ${proj_dir}

# move to run_dir
cd_run_dir

#e.g., Rscript load_process_save_chunk_autosubmit.R --args $task_path 1 1 1 1 2 2 1 1 1 2 1 2
Rscript ${proj_dir}/load_process_save_chunk_autosubmit.R --args ${task_path} ${chunk_args[@]}

