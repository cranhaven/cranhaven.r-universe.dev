---
title: "Global Time-Frequency Baseline Comparison"
author: "Zhengjia Wang"
date: "Date built: `r format(date())`"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Objective

We want to sanity-check the global baseline results for time-frequency analysis. Our goal is to make sure the C++ implementation of function `ravetools::baseline_array` result in no difference (within floating error) than the naive R implementation. 

## Methods & Results

This document uses the `DemoSubject`. To start, let's clear the existing cache to make sure we start with a clean environment.

```{r, eval=FALSE}
raveio::clear_cached_files(quiet = TRUE)
```

To load power data in `RAVE`, we use the pre-built function `raveio::prepare_subject_power` to load electrode channel `14`, with `1` second before epoch onset and `2` seconds after onset (using default epoch).

```{r}
repo <- raveio::prepare_subject_power(
  subject = 'demo/DemoSubject',
  electrodes = 14,
  time_windows = c(-1, 2),
  verbose = FALSE
)
```

### Global Baseline via RAVE

`raveio::power_baseline` uses `ravetools::baseline_array` internally, with user-friendly parameters. Please use `?raveio::power_baseline` to check the documentation.

* `baseline_windows=c(-1,0)` means the baseline window is from `-1` to `0` seconds (`[-1,0]`, including time-points at both sides)
* `units=c("Frequency","Electrode")` means calculating baseline per frequency per electrode and across trials
* `method="percentage"` means calculating baseline using power percentage change

The resulting baseline data can be accessed via `repo$power$baselined`

*(Below is power percentage change)*

```{r, results='hide'}
raveio::power_baseline(repo, baseline_windows = c(-1,0), 
                       units = c("Frequency", "Electrode"),
                       method = "percentage")
power_percentage_change <- repo$power$baselined[]
```

*(Below is amplitude percentage change)*

```{r, results='hide'}
raveio::power_baseline(repo, baseline_windows = c(-1,0), 
                       units = c("Frequency", "Electrode"),
                       method = "sqrt_percentage")
amplitude_percentage_change <- repo$power$baselined[]
```

*(Below is decibel-unit change)*

```{r, results='hide'}
raveio::power_baseline(repo, baseline_windows = c(-1,0), 
                       units = c("Frequency", "Electrode"), 
                       method = "decibel")
decibel_change <- repo$power$baselined[]
```

*(Below is z-scored power)*

```{r, results='hide'}
raveio::power_baseline(repo, baseline_windows = c(-1,0), units = c("Frequency", "Electrode"), method = "zscore")
power_zscore <- repo$power$baselined[]
```

*(Below is z-scored amplitude)*

```{r, results='hide'}
raveio::power_baseline(repo, baseline_windows = c(-1,0), units = c("Frequency", "Electrode"), method = "sqrt_zscore")
amplitude_zscore <- repo$power$baselined[]
```

### Native implementation

For native implementation, let's get power data

```{r}
power <- repo$power$data_list$e_14
cat(
  "Margin names are: ",
  paste(names(dimnames(power)), collapse = ", "),
  "\nMargin dimensions are: ",
  paste(dim(power), collapse = " x ")
)
```

The variable `power` is a `Frequency x Time x Trial x Electrode` (4-mode) tensor array. 

The following script obtains the baseline part of the power data

```{r}
baseline_data <- subset(power, Time ~ Time >= -1 & Time <= 0)
cat(
  "Data in the baseline has margin dimensions: ",
  paste(dim(baseline_data), collapse = " x ")
)
```

The second margin dimension of `baseline_data` differs with `power`. This is because power has `r dim(power)[[2]]` time-points while baseline window only has `r dim(baseline_data)[[2]]`

To calculate **power percentage change**:

```{r}
# baseline per frequency
power_expected <- (power[] / rowMeans(baseline_data) - 1) * 100
```

The maximum relative difference (`power_percentage_change / power_expected - 1`) is:

```{r}
range(power_percentage_change / power_expected - 1)
```


Let's compare the rest baseline results:

**Amplitude percentage change**:

```{r}
amplitude_expected <- (sqrt(power[]) / rowMeans(sqrt(baseline_data)) - 1) * 100
range(amplitude_percentage_change / amplitude_expected - 1)
```

**Decibel change**:

```{r}
decibel_expected <- 10*log10(power[]) - rowMeans(10*log10(baseline_data))
range(decibel_change / decibel_expected - 1)
```

**Z-scored power**:

```{r}
power_zscore_expected <- (power[] - rowMeans(baseline_data)) / apply(baseline_data, 1, sd)
range(power_zscore / power_zscore_expected - 1)
```

**Z-scored amplitude**:

```{r}
amp_zscore_expected <- (sqrt(power[]) - rowMeans(sqrt(baseline_data))) / apply(sqrt(baseline_data), 1, sd)
range(amplitude_zscore / amp_zscore_expected - 1)
```

## Benchmark

Let's test the speed of running baseline using the entire 5 channels.

```{r}
repo <- raveio::prepare_subject_power(
  subject = 'demo/DemoSubject',
  electrodes = "13-16,24",
  time_windows = c(-1, 2),
  verbose = FALSE
)

# RAVE baseline on 5 electrode channels
system.time({
  raveio::power_baseline(repo, baseline_windows = c(-1,0), 
                       units = c("Frequency", "Electrode"),
                       method = "percentage")
})

# Naive implementation
system.time({
  lapply(repo$power$data_list, function(power) {
    baseline_data <- subset(power, Time ~ Time >= -1 & Time <= 0)
    power_expected <- (power[] / apply(baseline_data, 1, mean) - 1) * 100
    filearray::as_filearray(power_expected)
  })
})
```
