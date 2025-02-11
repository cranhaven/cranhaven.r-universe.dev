#!/bin/sh

docker run \
  --mount type=bind,src=`pwd`,dst=/home/rstudio/projects \
  -w /home/rstudio/projects \
  --rm \
  ghcr.io/hugheylab/socker \
  bash -c "Rscript run_seeker_array.R GSE25585.yml ." \
  &> GSE25585_progress.log
