# Rexperigen

[![Build Status](https://travis-ci.org/aquincum/Rexperigen.svg?branch=master)](https://travis-ci.org/aquincum/Rexperigen)
[![codecov.io](https://codecov.io/github/aquincum/Rexperigen/coverage.svg?branch=master)](https://codecov.io/github/aquincum/Rexperigen?branch=master)

An R interface for downloading results from an Experigen server. Works with the "classic" server that is currently running on `db.experigen.org` as well, but its main advantage is that it helps a lot with the new functions of the newer version of the Experigen server. Most importantly, it helps with registration of experimenters, registration of experiments and accessing their data.


## Setup

Key setup functions are:

- `setExperigenServer`: sets the URL of the  server where the data is stored. The default currently is `db.phonologist.org`
- `setExperigenCredentials` (server version >=2 only): sets the experimenter username and password for access to registered data
- `registerExperiment`, `removeRegistration`, `getRegisteredExperiments` (server version >=2 only): functions helping with registration of experiments, ie. limiting access to the given experiment to yourself. They do what's on their label.

## Download

Key download functions are:

- `getUsers`: lists the userCodes and the number of records for each user
- `getDestinations` (server version >=2 only): lists the destination files available for each experiment
- `downloadExperiment`: downloads the results of a destination file for a given experiment into a data frame. This is the **main download function**.

## Others

For further info, see the R documentation!
