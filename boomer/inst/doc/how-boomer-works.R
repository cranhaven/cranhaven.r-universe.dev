## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  error = TRUE # TODO: fix failing chunks
)

## -----------------------------------------------------------------------------
rigged_file_ext <- boomer::rig(tools::file_ext)
tools::file_ext
rigged_file_ext

## -----------------------------------------------------------------------------
# the original environment
environment(tools::file_ext)

# our new environment
env <- environment(rigged_file_ext)
env

# its parent
parent.env(env)

# its content
ls(env)

## -----------------------------------------------------------------------------
flow::flow_view_deps(boomer:::rig_impl, show_imports = "packages")

