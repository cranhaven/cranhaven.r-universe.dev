#!/bin/sh

docker run \
  --mount type=bind,src=`pwd`,dst=/home/rstudio/projects \
  -w /home/rstudio/projects \
  --rm \
  ghcr.io/hugheylab/socker \
  bash -c \
    "source /home/rstudio/miniconda3/etc/profile.d/conda.sh \
      && conda activate seeker \
      && refgenie pull mm10/salmon_partial_sa_index \
      && Rscript run_seeker.R PRJNA600892.yml ." \
  &> PRJNA600892_progress.log
