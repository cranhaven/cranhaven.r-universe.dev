
<!-- README.md is generated from README.Rmd. Please edit that file -->

# E4tools

Tested with TravisCI: [![Build
Status](https://travis-ci.org/ekleiman1/E4tools.svg?branch=master)](https://travis-ci.org/ekleiman1/E4tools)  
E4tools is an early-release set of tools that you can use to automate
your workflow for analyzing EDA data that comes from the Empatica E4.
You can run these functions consecutively.

## Setting up E4tools

### Step 1: Organize your files

**How do I structure my data for E4 tools?**  
Your files should be structured such that ZIP files should be grouped by
participant – that is, each ZIP file (i.e., what you downloaded from
Empatica Connect) is in a folder whose name is the participant ID.

### Step 2: The output structure

**The File Helper function**  
E4tools now comes with the **E4.Step0.FileHelper** function. This
function will not only cause other functions to require fewer arguments,
but will also automatically create a folder structure that is consistent
across all steps. When you run this function, you will only need to
specify the location of your ZIP files (see step 1 above) and where you
want your output folder to go. Once you do this, for all other
functions, you will not need to specify any file locations. Instead, you
will just write the name of the argument
(e.g.,“ziplocaton=ziplocation”), which will tell E4tools to use the
file structure specified by the File Helper function.

**What does the file structure look like?**  
The file structure is organized into five output folders, each
representing the type of output data.

  - raw\_data, which contains raw data that are extracted from ZIP files
    (e.g., raw EDA)
  - metadata, which contains metadata (e.g., E4 sessions summaries)
  - matched\_data, which contains data that are matches to events (e.g.,
    button pressess)
  - binned\_data, which contains data that have been binned
  - plots, which contains plots generated from the data (e.g., output
    from the diagnostics functions)

## What can you do now with E4tools?

There are currently four tools, all of which are part of the EDA
workflow (acc. workflow coming soon):

### The EDA processing pipeline

**E4\_EDA\_Process.part1.ExtractRawEDA**, which allows you extract and
filter EDA data. It will output raw data, filtered data (using
user-specified high and low pass filters + a butterworth filter), and
filtered + feature-scaled (\[0,1\]) data. It will also provide summary
data at the participant and session level.

**E4\_EDA\_Process.part2.ExtractButtonPresses**, which allows you
extract button pressess and remove pressess that are within a certain
number of minutes before the end of a session or that are too close to
another button press.

**E4\_EDA\_Process.part3.MatchPressesToEDA**, which allows you to
extract the data that are within X minutes before and/or after a button
press.

**E4\_EDA\_Process.part4.BinMatchedEDA**, which allows you to bin the
data that has been matched to the button pressess (from step 3).

## Frequently Asked Questions

### How do I install E4tools?

`install.packages("E4tools")`

## Funding

This package was created to support data from projects R34MH113757-01A1
and R21MH115293 to Evan M. Kleiman.
